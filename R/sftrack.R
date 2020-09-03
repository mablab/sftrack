#' Convert objects into sftrack objects.
#' @name as_sftrack
#' @title Convert objects into sftrack objects.
#' @description
#' This function converts x,y,z data into an sftrack object with a sf_geometry column of sf_POINTS.
#' Creates a `burst` column to group movement data and sets dedicated time and error columns.
#'
#' Raw data inputted in two ways: vector or data.frame. 'Vector' inputs gives the argument as a vector where
#' length = nrow(data). 'Data.frame' inputs gives the arguments as the column name of `data` where the input can be found.
#' Either input is allowed on any given argument.
#'
#' Some options are global and required regardless
#' @param data a data.frame of the movement data, if supplied all data.frame inputs, than is optional
#' @param coords a character vector describing where the x,y,z coordinates are located in `data` or a list with x,y,z (optional) vectors
#' @param burst a list of named vectors describing multiple grouping variables or  a character vector naming the other grouping columns in `data`.
#' @param active_burst a character vector of the burst names to be 'active' to group data by for analysis
#' @param time a vector of time information, can be either POSIX or an integer or a character string naming the column in `data` where the time information is located
#' @param error (optional) a vector of error information for the movement dataa character string naming the column in `data` where the error information is located
#' @param crs a crs string from rgdal of the crs and projection information for the spatial data. Defaults to NA
#' @param zeroNA logical whether to convert 0s in spatial data into NAs. Defaults to FALSE.
#' @param ... extra information to be passed on to as_sftrack
#' @import sf
#' @export
#' @examples
#' #'
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
#' # Input is a data.frame
#' my_track <- as_sftrack(raccoon,
#'   burst = burstz, time = "timestamp",
#'   error = NA, coords = c("longitude", "latitude")
#' )
#'
#' # Input is a ltraj
#' library(adehabitatLT)
#' ltraj_df <- as.ltraj(
#'   xy = raccoon[, c("longitude", "latitude")],
#'   date = as.POSIXct(raccoon$timestamp),
#'   id = raccoon$animal_id, typeII = TRUE,
#'   infolocs = raccoon[, 1:6]
#' )
#'
#' my_sftrack <- as_sftrack(ltraj_df)
#' head(my_sftrack)
#'
#' # Input is a sf object
#' library(sf)
#' df1 <- raccoon[!is.na(raccoon$latitude), ]
#' sf_df <- st_as_sf(df1, coords = c("longitude", "latitude"))
#'
#' new_sftrack <- as_sftrack(sf_df, burst = c(id = "animal_id"), time = "timestamp")
#' head(new_sftrack)
#'
#' # Input is an sftraj object
#' my_traj <- as_sftraj(raccoon,
#'   time = "timestamp",
#'   error = NA, coords = c("longitude", "latitude"),
#'   burst = burstz
#' )
#'
#' new_track <- as_sftrack(my_traj)
#' head(new_track)
#' ######################
#' # Builder
as_sftrack <- function(data = data.frame(), ...) {
  UseMethod("as_sftrack", object = data)
}

#' @title Define an sftrack
#' @param data  data.frame with multi_burst column, geometry column, time_col (integer/POSIXct), and error column (optional)
#' @param burst_col column name of multi_burst in `data`
#' @param sf_col column name of geometry in `data`
#' @param time_col column name of time in `data`
#' @param error_col column name of error in `data`
#' @export
new_sftrack <-
  function(data,
           burst_col,
           sf_col,
           time_col,
           error_col = NA) {
    data_sf <- sf::st_sf(data, sf_column_name = sf_col)
    structure(
      data_sf,
      burst = burst_col,
      time = time_col,
      error = error_col,
      class = c("sftrack", "sf", "data.frame")
    )
  }

#' @rdname as_sftrack
#' @export
#' @method as_sftrack data.frame
as_sftrack.data.frame <- function(data = data.frame(),
                                  ...,
                                  coords = c("x", "y"),
                                  burst = "id",
                                  active_burst = NA,
                                  time = "time",
                                  error = NA,
                                  crs = NA,
                                  zeroNA = FALSE) {
  # data = data.frame()
  # coords = sub_gps[,c('longitude','latitude')]
  # burst = list(id=sub_gps$id, numSat = sub_gps$numSat)
  # time = sub_gps[,'timez']
  # crs='+init=epsg:4326'
  # error = NA
  # zeroNA = FALSE
  # active_burst = NA
  ######################
  # Check inputs
  # Id
  if (nrow(data) == 0) {
    data <- data.frame(sftrack_id = seq_along(time))
  }
  # bursts
  if (length(burst) == 1) {
    names(burst) <- "id"
  }
  if (all(sapply(burst, length) == nrow(data))) {
    # check id in burst
    check_burst_id(burst)
    burst_list <- burst
  } else {
    # check names exist
    if(inherits(burst,'list')){
      burst <- vapply(burst,c, character(1))
    }
    check_names_exist(data, burst)
    # check id in burst
    # check id in burst
    check_burst_id(burst)
    # create burst list from names
    burst_list <- lapply(data[, burst, FALSE], function(x) {
      x
    })
    if (!is.null(names(burst))) {
      names(burst_list) <-
        names(burst)
    } else {
      names(burst_list) <- burst
    }
  }

  # Coords
  if (is.null(nrow(coords))) {
    check_names_exist(data, coords)
    xyz <- data[, coords]
  } else {
    xyz <- as.data.frame(coords)
  }
  # fix zeros to NA
  if (zeroNA) {
    xyz <- fix_zero(xyz)
  }
  check_NA_coords(xyz)


  # Time
  if (length(time) == nrow(data)) {
    data$reloc_time <- time
    time_col <- "reloc_time"
  } else {
    check_names_exist(data, time)
    time_col <- time
  }
  check_time(data[[time_col]])

  # Error
  if (length(error) == nrow(data)) {
    data$sftrack_error <- error
    error_col <- "sftrack_error"
  } else {
    if (!is.na(error)) {
      check_names_exist(data, error)
      error_col <- error
    } else {
      error_col <- NA
    }
  }

  # pull out other relevant info
  if (any(is.na(active_burst))) {
    active_burst <- names(burst_list)
  }
  burst <-
    make_multi_burst(burst_list, active_burst = active_burst)

  # earliest reasonable time to check time stamps
  dup_timestamp(time = data[[time_col]], x = burst)

  geom <-
    sf::st_as_sf(xyz,
      coords = names(xyz),
      crs = crs,
      na.fail = FALSE
    )
  # Force calculation of empty geometries.
  attr(geom[, attr(geom, "sf_column")], "n_empty") <-
    sum(vapply(st_geometry(geom), sfg_is_empty, TRUE))

  data$burst <- burst
  data$geometry <-  st_geometry(geom)

  ret <- new_sftrack(
    data = data,
    burst_col = "burst",
    sf_col = "geometry",
    error_col = error_col,
    time_col = time_col
  )
  # Sanity checks
  ret <- ret[check_ordered(ret$burst, ret[[attr(ret, "time")]]), ]

  check_z_coords(ret)

  return(ret)
}

#' @rdname as_sftrack
#' @method as_sftrack sftraj
#' @export
as_sftrack.sftraj <- function(data, ...) {
  geometry <- st_geometry(data)

  # pull out first points from straj
  new_geom <- pts_traj(data)
  crs <- attr(geometry, "crs")

  geometry <- sf::st_sfc(new_geom, crs = crs)
  burst <- attr(data, "burst")
  error <- attr(data, "error")
  time <- attr(data, "time")
  sf_col <- attr(data, "sf_column")
  data[[sf_col]] <- geometry

  new_data <- as.data.frame(data)
  ret <- new_sftrack(
    data = new_data,
    burst_col = burst,
    sf_col = sf_col,
    error_col = error,
    time_col = time
  )

  return(ret)
}

### Ltraj
#' @rdname as_sftrack
#' @method as_sftrack ltraj
#' @export
as_sftrack.ltraj <- function(data, ...) {
  # This is done so we dont have to import adehabitat. (instead of ld())
  # But it could go either way depending
  new_data <- lapply(seq_along(data), function(x) {
    sub <- data[x, ]
    attributes(sub[[1]])
    id <- attr(sub[[1]], "id")
    burst <- attr(sub[[1]], "burst")
    infolocs <- infolocs(data)[x]
    reloc_time <- sub[[1]]$date
    coords <- c("x", "y")
    data.frame(sub[[1]][, coords], id, burst, reloc_time, infolocs)
  })
  df1 <- do.call(rbind, new_data)
  time <- "reloc_time"
  burst <- list(id = df1$id)
  crs <- attr(data, "proj4string")
  # pull out id and burst from ltraj object
  id_lt <- vapply(data, function(x) {
    attr(x, "id")
  }, NA_character_)
  burst_lt <-
    vapply(data, function(x) {
      attr(x, "burst")
    }, NA_character_)

  if (!all(burst_lt == id_lt)) {
    burst$group <- df1$burst
  }
  coords <- c("x", "y")
  geom <-
    sf::st_as_sf(df1[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  # pull out other relevant info
  df1$burst <- make_multi_burst(burst)
  error <- NA
  new_data <-
    cbind(df1[, !colnames(df1) %in% c("id")], geometry = st_geometry(geom))
  ret <- new_sftrack(
    data = new_data,
    burst_col = burst,
    error_col = error,
    time_col = time,
    sf_col = "geometry"
  )
  # Sanity check. Which are necessary?
  ret <-
    ret[check_ordered(ret$burst, ret[, attr(ret, "time"), drop = T]), ]
  #
  return(ret)
}
# sf
#' @rdname as_sftrack
#' @method as_sftrack sf
#' @export
as_sftrack.sf <- function(data,
                          ...,
                          coords,
                          burst,
                          active_burst = NA,
                          time,
                          error = NA) {
  geom <- st_geometry(data)

  data <- as.data.frame(data)
  # Check inputs
  # geom
  if (attributes(geom)$class[1] != "sfc_POINT") {
    stop("sf column must be an sfc_POINT")
  }
  # Id

  # bursts
  if (length(burst) == 1) {
    names(burst) <- "id"
  }
  if (all(sapply(burst, length) == nrow(data))) {
    # check id in burst
    check_burst_id(burst)
    burst_list <- burst
  } else {
    # check names exist
    check_names_exist(data, burst)
    # check id in burst
    # check id in burst
    check_burst_id(burst)
    # create burst list from names
    burst_list <- lapply(data[, burst, FALSE], function(x) {
      x
    })
    if (!is.null(names(burst))) {
      names(burst_list) <-
        names(burst)
    } else {
      names(burst_list) <- burst
    }
  }

  # Time
  if (length(time) == nrow(data)) {
    data$reloc_time <- time
    time_col <- "reloc_time"
  } else {
    check_names_exist(data, time)
    time_col <- time
  }
  check_time(data[[time_col]])

  # Error
  if (length(error) == nrow(data)) {
    data$sftrack_error <- error
    error_col <- "sftrack_error"
  } else {
    if (!is.na(error)) {
      check_names_exist(data, error)
      error_col <- error
    } else {
      error_col <- NA
    }
  }

  #
  if (any(is.na(active_burst))) {
    active_burst <- names(burst_list)
  }
  burst <-
    make_multi_burst(burst_list, active_burst = active_burst)

  # earliest reasonable time to check time stamps
  dup_timestamp(time = data[[time_col]], x = burst)

  data$burst <- burst

  ret <- new_sftrack(
    data = data,
    burst_col = "burst",
    sf_col = "geometry",
    error_col = error_col,
    time_col = time_col
  )
  # Sanity checks
  ret <- ret[check_ordered(ret$burst, ret[[attr(ret, "time")]]), ]

  check_z_coords(ret)

  return(ret)
}


# Methods for 'sftrack' class

#' @title Print methods for sftrack
#' @name Print_sftrack_objects
#' @param x sftraj object
#' @param n_row Integer of number of rows to display. Defaults to global option default if non supplied
#' @param n_col Integer of number of columns to display + required sftrack columns (burst, geometry, time, and error). Defaults to global option default if non supplied
#' @param ... other arguments passed onto print
#' @export
print.sftrack <- function(x, n_row, n_col, ...) {
  if (missing(n_col)) {
    n_col <- ncol(x)
  }
  if (missing(n_row)) {
    n_row <- nrow(x)
  }
  burst_col <- attr(x, "burst")
  sf_col <- attr(x, "sf_column")
  time_col <- attr(x, "time")
  sf_attr <- attributes(st_geometry(x))
  # time stuff

  tcl <- attributes(x[[time_col]])$class[1]
  if (tcl == "POSIXct") {
    tz <- attributes(x[[time_col]])$tzone
    if (is.null(tz) || tz == "") {
      tz <- paste("no timezone")
    }
    time_mes <- paste(tcl, "in", tz)
  } else {
    time_mes <- "integer"
  }

  # active burst
  ab <- attr(x[[burst_col]], "active_burst")
  bn <- names(x[[burst_col]][[1]])
  active_burst_names <- paste0("*", ab, "*")
  burst_mes <-
    paste0(active_burst_names, bn[!ab %in% bn], collapse = ", ")
  # print
  cat(
    paste0(
      "Sftrack with ",
      nrow(x),
      " features and ",
      ncol(x),
      " fields (",
      sf_attr$n_empty,
      " empty geometries) \n"
    )
  )
  cat(paste0(
    'Geometry : \"',
    sf_col,
    '\" (',
    class(x[[sf_col]][[1]])[1],
    ", crs: ",
    format(sf_attr$crs),
    ") \n"
  ))
  cat(paste0('Timestamp : \"', time_col, '\" (', time_mes, ") \n"))
  cat(paste0('Burst : \"', burst_col, '\" (', burst_mes, ") \n"))
  cat("-------------------------------\n")
  # Figure out the row and columns
  row_l <- ifelse(nrow(x) > n_row, n_row, nrow(x))
  sub_col_names <-
    colnames(x)[!colnames(x) %in% c(burst_col, sf_col, time_col)]
  col_l <- length(sub_col_names)

  x <- as.data.frame(x)

  if (n_col < col_l) {
    p <- ifelse(col_l > n_col, n_col, col_l)
    ret <- cbind(
      x[1:row_l, sub_col_names[1:p], drop = FALSE],
      data.frame("..." = rep("...", row_l)),
      x[1:row_l, c(burst_col, sf_col, time_col)]
    )
  } else {
    ret <- x
  }

  ret <- ret[1:row_l, ]
  print(ret, ...)
}
# print(my_track,10,10)

# Sumary
# summary.sftrack
#' @export
summary.sftrack <- function(object, ..., stats = FALSE) {
  if (stats) {
    summary_sftrack(object)
  } else {
    (NextMethod())
  }
}
# summary(my_sftrack,stats=TRUE)
#' @export
`[.sftrack` <- function(x, i, j, ..., drop = FALSE) {
  # x = my_sftrack
  # i = 1:10
  # j=c(1,2,3)
  # rm(j)
  sf_col <- attr(x, "sf_column")
  time_col <- attr(x, "time")
  error_col <- attr(x, "error")
  # if(is.na(error_col)){ error_col <- NULL}
  nargs <- nargs()
  # print(nargs)
  if (!missing(j) && missing(i)) {
    i <- seq_len(nrow(x))
  }
  if (!missing(i) && nargs > 2) {
    if (is.character(i)) {
      i <- burst_labels(x) %in% i
    }
  }
  if (!missing(j) && all(is.character(j))) {
    j <- which(colnames(x) %in% j)
  }
  # x = as.data.frame(x)
  class(x) <- setdiff(class(x), c("sftrack", "sf"))
  x <- if (missing(j)) {
    if (nargs == 2) {
      x[i]
    } else {
      x[i, , ]
    }
  } else {
    if (drop) {
      return(x[i, j, drop = drop])
    } else {
      error_col <- if (is.na(error_col)) {
        NULL
      } else {
        error_col
      }
      x[i, union(colnames(x)[j], c("burst", sf_col, time_col, error_col))]
    }
  }


  # # This piece is if we want drop = TRUE to be the default
  # if((any(!exist_name[1:3]) || !is.na(error_col) & !exist_name[4])){
  #
  #   message(paste0(paste0(c('burst','geometry','time','error')[!exist_name],collapse=', '),' subsetted out of sftrack object, reverting to ',class(x)[1],
  #     '\n Use drop = FALSE to retain class'))
  #
  #     }
  ret <- new_sftrack(
    x,
    burst_col = "burst",
    sf_col = sf_col,
    time_col = time_col,
    error_col = error_col
  )
  dup_timestamp(ret)
  ret
}

#' @export
rbind.sftrack <- function(...) {
  all <- list(...)
  # all = list(my_sftrack, my_sftrack2)

  same_att <-
    all(duplicated(lapply(all, function(x) {
      attributes(x)[c("sf_column", "time", "error")]
    }))[-1])
  if (!same_att) {
    stop("sf, time, and error columns must be the same")
  }
  att <- attributes(all[[1]])
  time_col <- att$time
  error_col <- att$error
  sf_col <- att$sf_column
  for (i in seq_along(all)) {
    class(all[[i]]) <- setdiff(class(all[[i]]), c("sftrack", "sf"))
  }
  new_df <- do.call(rbind, all)
  class(new_df) <- setdiff(class(new_df), c("sftrack", "sf"))
  ret <- new_sftrack(
    data = new_df,
    burst_col = "burst",
    time_col = time_col,
    error_col = error_col,
    sf_col = sf_col
  )

  # Sanity checks
  dup_timestamp(ret)
  return(ret)
}
#' @export
cbind.sftrack <- function(..., deparse.level = 1) {
  all <- list(...)
  # all <- list(my_sftrack,my_sftrack[,1:5,drop=T])
  sftrack_obj <- sapply(all, function(x) {
    inherits(x, "sftrack")
  })
  if (sum(sftrack_obj) > 1) {
    # return error
    stop("Cannot attempt to merge two sftrack objects together, use new_sftrack() instead")
  }
  # The rest below this is not useful as I'm pretty sure you cbind.sftrack only works when both objects are an sftrack object
  # I'll keep it till we can decide what we want cbind to do
  # check if theres two burst columns
  if (sum(unlist(lapply(all, function(x) {
    colnames(x) == "burst"
  }))) > 1) {
    stop('More than one column named "burst" found')
  }
}

#' @export
"[[<-.sftrack" <- function(x, i, value) {
  x <- structure(NextMethod(), class = c("sftrack", "sf", "data.frame"))
}
