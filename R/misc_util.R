###################
# Misc utility functions
###################
#' @title Return a list of sf_POINTS or a data.frame from a sftraj object
#' @name traj_geom
#' @param traj a trajectory geometery from sf_traj
#' @param sfc TRUE/FALSE should the return by an sfc or a list of points. Defaults to FALSE
#' @examples
#'
#' print(racc_traj, 5, 10)
#'
#' # extract a list of points
#' pts_traj(racc_traj)[1:10]
#'
#' # or a data.frame of points
#' coord_traj(racc_traj)[1:10]
#' @export
pts_traj <- function(traj, sfc = FALSE) {
  pts <- st_geometry(traj)

  ret <- lapply(pts, function(x) {
    if (inherits(x, "LINESTRING")) {
      st_point(unclass(x)[1L, , drop = TRUE])
    } else {
      x
    }
  })
  if (!sfc) {
    return(ret)
  }

  st_sfc(ret,
    crs = st_crs(pts),
    precision = st_precision(pts)
  )
}

#' @rdname traj_geom
#' @export
coord_traj <- function(traj) {
  # traj = my_sftraj
  pts <- st_geometry(traj)

  if ("XY" %in% class(pts[[1]])) {
    dim <- 2
  } else {
    dim <- 3
  }
  this_seq <- seq(1, dim * 2, by = 2)
  ret <- lapply(pts, function(x) {
    #  x = pts[[499]]
    if (inherits(x, "POINT")) {
      st_coordinates(x)
      x[1:dim]
    } else {
      x[this_seq]
      # st_coordinates(x)[pos, dim]
    }
  })
  do.call(rbind, ret)
}

#' @export
st_coordinates.sftraj <- function(x, return = "all") {
  # x = my_sftraj
  pts <- sf::st_geometry(x)

  ret <- lapply(
    pts,
    function(x) {
      coords <- data.frame(st_coordinates(x))
      coords$Point <- seq_len(nrow(coords))
      coords[, c("X", "Y", "Point")]
    }
  )
  ret <- do.call(rbind, ret)
  choice <- switch(return, start = 1, end = 2, all = c(1, 2))
  ret[ret$Point %in% unlist(choice), ]
}


#' @title Is a trajectory geometry a linestring or a point
#' @description A step is a movement from one point to the next, with an sftraj object
#' this manifests as a linestring. If, however, one of these two points is missing, the sftraj
#' is created as a geometery collection of two points, the beginning and the end point, where one
#' of the steps is NA. This function checks a trajectory geometry if its a linestring and returns
#' a vector of T/F.
#' @export
#' @param x an sftraj object
is_linestring <- function(x) {
  if (inherits(x, "sftraj")) {
    pts <- x[[attr(x, "sf_column")]]
  }
  if (inherits(x, "sfc")) {
    pts <- x
  }
  # if ('XY' %in% class(pts[[1]])) {
  #   dim = c('X', 'Y')
  # } else{
  #   dim = c('X', 'Y', 'Z')
  # }
  vapply(pts, function(y) {
    inherits(y, "LINESTRING")
  }, NA)

  # the sf version might be faster?
  # st_is(x,'LINESTRING')
}

#' @title Summarize sftrack objects
#' @param x an \code{sftrack} or \code{sftraj} object
#' @export
group_summary <- function(x) {
  if (!inherits(x, c("sftrack", "sftraj")))
      stop("Object of class 'sftrack' or 'sftraj' expected")
  time_col <- attr(x, "time_col")
  error_col <- attr(x, "error_col")
  sf_col <- attr(x, "sf_column")
  group_col <- attr(x, "group_col")
  levelz <- group_labels(x[[group_col]])
  statz <-
    tapply(t1(x), levelz, function(x) {
      list(
        "begin" = min(x),
        "end" = max(x),
        "n_rec" = length(x)
      )
    })

  if (inherits(x, "sftrack")) {
      NAs <- tapply(x[[sf_col]], levelz, function(sf_grp) {
          sum(st_is_empty(sf_grp))
      })
  }
  if (inherits(x, "sftraj")) {
      NAs <- tapply(x[[sf_col]], levelz, function(sf_grp) {
          sum(!is_linestring(sf_grp))
      })
  }

  if (inherits(x, "sftrack")) {
    my_crs <- attr(x[[sf_col]], "crs")
    lenz <- tapply(x[[sf_col]], levelz, function(pts) {
      new_pts <- pts[!vapply(pts, sf::st_is_empty, NA)]
      my_sfc <-
        st_sfc(st_linestring(st_coordinates(new_pts)), crs = my_crs)
      st_length(my_sfc)
    }, simplify = FALSE)
    lenz_unit <- as.character(attr(lenz[[1]], "units"))
  }
  if (inherits(x, "sftraj")) {
    lenz <- tapply(x[[sf_col]], levelz, function(pts) {
      sum(st_length(pts))
    }, simplify = FALSE)
    lenz_unit <- as.character(attr(lenz[[1]], "units"))
  }

  area <- tapply(x[[sf_col]], levelz, function(sf_grp) {
      st_area(st_convex_hull(st_combine(sf_grp[!st_is_empty(sf_grp)])))
  }, simplify = FALSE)
  area_unit <- as.character(attr(area[[1]], "units"))

  n_rec <- vapply(
    statz,
    FUN = function(x) {
      x$n_rec
    },
    numeric(1)
  )
  begin_time <- lapply(statz, function(x) {
    x$begin
  })
  end_time <- lapply(statz, function(x) {
    x$end
  })

  class(begin_time) <- class(end_time) <- c("POSIXct", "POSIXt")
  attr(begin_time, "tzone") <- attr(x[[attr(x, "time_col")]], "tzone")
  attr(end_time, "tzone") <- attr(x[[attr(x, "time_col")]], "tzone")

  as.data.frame(
      list(levels(levelz), n_rec, NAs, begin_time, end_time, lenz, area),
      col.names = c("group", "n_records", "NAs", "begin_time", "end_time", paste("length", lenz_unit), paste("area", area_unit)),
      row.names = 1:length(n_rec)
  )

}

# recalculates empty geometries (take from sf as it is an internal as well)
sfg_is_empty <- function(x) {
  switch(
    class(x)[2],
    POINT = any(!is.finite(x)),
    MULTIPOINT = ,
    LINESTRING = ,
    CIRCULARSTRING = ,
    CURVE = nrow(x) == 0,
    length(x) == 0
  )
}


#' @title Which grouping/time stamp combos are duplicated.
#' @description This function returns a data.frame of which rows are duplicated and their time stamps.
#' @export
#' @param data a data.frame containing burst or time data (if necessary)
#' @param group a list where each entry is a vector of groupings where length == nrow(data)|nrow(time). Or a character vector describing the column name they are located in data
#' @param time a vector of as.POSIXct time, or a character of the column name where it can be found in data

which_duplicated <- function(data = data.frame(), group, time) {
  # coords = c('longitude','latitude')
  # burst = c(id = 'animal_id', month = 'month')
  # time = 'time'
  # data$time[1] <- data$time[2]
  # data$time[4] <- data$time[5]
  if (length(group) == 1) {
    names(group) <- "id"
  }
  if (all(sapply(group, length) == nrow(data))) {
    # check id in burst
    check_group_id(group)
    group_list <- group
  } else {
    # check names exist
    check_names_exist(data, group)
    # check id in burst
    # check id in burst
    check_group_id(group)
    # create burst list from names
    group_list <- lapply(data[, group, FALSE], function(x) {
      x
    })
    if (!is.null(names(group))) {
      names(group_list) <-
        names(group)
    } else {
      names(group_list) <- group
    }
  }

  if (length(time) == nrow(data)) {
    reloc_time <- time
  } else {
    check_names_exist(data, time)
    reloc_time <- data[[time]]
  }
  check_time(reloc_time)

  group <-
    make_c_grouping(x = group_list)
  gl <- group_labels(group)
  dup_results <-
    tapply(reloc_time, gl, function(x) {
      isDup <- duplicated(x)
      if (any(isDup)) {
        return(x[isDup])
      }
    })
  ans <- dup_results[!vapply(dup_results, is.null, logical(1))]
  rowz <- which(names(ans) == gl & ans == reloc_time)
  data.frame(group = gl[rowz], time = reloc_time[rowz], which_row = rowz)
}

# Get the position of x2, given the time
get_x2 <- function(time) {
  or <- order(time)
  seq_along(time)[or][or + 1][order(or)]
}

#' @title Merge connected lines and create an sf object
#' @description This function returns a sf object grouped by each burst with a geometry column of multilinestrings for each grouping
#' @export
#' @importFrom stats aggregate
#' @param x an sftraj object
merge_traj <- function(x) {
  group_col <- attr(x, "group_col")
  x <- x[order(t1(x)), ]
  crs <- st_crs(x)
  ret <- stats::aggregate(st_geometry(x), list(group = group_labels(x)), function(y) {
    # y = st_geometry(x)[burst_labels(x, factor = TRUE)=='TTP-041_s']
    geom <- y[st_is(y, "LINESTRING")]
    if (length(geom) > 1) {
      st_line_merge(st_combine(geom))
    } else {
      st_multilinestring(list(st_linestring()))
    }
  })
  ret$geometry <- st_sfc(ret$geometry, crs = crs)
  st_sf(ret, sf_column_name = "geometry")
}

get_point <- function(x, position = "x1") {
  # position = 'x2'
  x <- st_geometry(x)
  if (inherits(x[[1]], "XY")) {
    ret <- vapply(
      x, function(y) {
        # y = x[[2]]
        if (inherits(y, "POINT")) {
          # 3 just represents a non-position here, as NA would fail for empty points
          pos <- switch(position, x1 = 1, x2 = 3, y1 = 2, y2 = 3, xy1 = c(1, 2), xy2 = c(3, 3))
        } else {
          pos <- switch(position, x1 = 1, x2 = 2, y1 = 3, y2 = 4, xy1 = c(1, 3), xy2 = c(2, 4))
        }
        y[pos]
      },
      numeric(nchar(position) - 1)
    )
  } else {
    ret <- vapply(
      x, function(y) {
        if (inherits(y, "POINT")) {
          # 4 just represents a non-position here, as NA would fail for empty points
          pos <- switch(position, x1 = 1, x2 = 4, y1 = 2, y2 = 4, z1 = 3, z2 = 4, xy1 = c(1, 2), xy2 = c(4, 4), xyz1 = c(1, 2, 3), xyz2 = c(4, 4, 4))
        } else {
          pos <- switch(position, x1 = 1, x2 = 2, y1 = 3, y2 = 4, z1 = 5, z2 = 6, xy1 = c(1, 3), xy2 = c(2, 4), xyz1 = c(1, 3, 5), xyz2 = c(2, 4, 6))
        }
        y[pos]
      },
      nchar(position) - 1
    )
  }
  if (nchar(position) == 2) {
    ret
  } else {
    t(ret)
  }
}
