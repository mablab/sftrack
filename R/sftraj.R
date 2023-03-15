#' Convert objects into sftrack objects.
#' @name as_sftraj
#' @title Convert objects into sftrack objects.
#' @description
#' This function converts x,y,z data into an sftrack object with a sf_geometry column of sf_POINTS.
#' Creates a `grouping` column to group movement data and sets dedicated time and error columns.
#'
#' Raw data inputted in two ways: vector or data.frame. 'Vector' inputs gives the argument as a vector where
#' length = nrow(data). 'Data.frame' inputs gives the arguments as the column name of `data` where the input can be found.
#' Either input is allowed on any given argument.
#'
#' Some options are global and required regardless
#' @param data a data.frame of the movement data, if supplied all data.frame inputs, than is optional
#' @param coords a character vector describing where the x,y,z coordinates are located in `data` or a list with x,y,z (optional) vectors
#' @param group a list of named vectors describing multiple grouping variables or  a character vector naming the other grouping columns in `data`.
#' @param active_group a character vector of the burst names to be 'active' to group data by for analysis
#' @param time a vector of time information, can be either POSIX or an integer or a character string naming the column in `data` where the time information is located
#' @param error (optional) a vector of error information for the movement dataa character string naming the column in `data` where the error information is located
#' @param crs Coordinate reference system to be assigned; object of class `crs`. Defaults to NA
#' @param zeroNA logical whether to convert 0s in spatial data into NAs. Defaults to FALSE.
#' @param group_name (optional) new column name for grouping data
#' @param timestamp_name (optional) new column name for time data
#' @param error_name (optional) new column name for error data
#' @param overwrite_names T/F Whether to overwrite data if a group/time/error column name is supplied but already in data
#' @param ... extra information to be passed on to as_sftrack
#' @import sf
#' @export
#' @examples
#' #'
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
#' # Input is a data.frame
#' my_track <- as_sftraj(raccoon,
#'   group = burstz, time = "timestamp",
#'   error = NA, coords = c("longitude", "latitude")
#' )
#'
#' # Input is a ltraj
#' library("adehabitatLT")
#' ltraj_df <- as.ltraj(
#'   xy = raccoon[, c("longitude", "latitude")],
#'   date = as.POSIXct(raccoon$timestamp),
#'   id = raccoon$animal_id, typeII = TRUE,
#'   infolocs = raccoon[, 1:6]
#' )
#'
#' my_sftrack <- as_sftraj(ltraj_df)
#' head(my_sftrack)
#'
#' # Input is a sf object
#' library("sf")
#' df1 <- raccoon[!is.na(raccoon$latitude), ]
#' sf_df <- st_as_sf(df1, coords = c("longitude", "latitude"))
#'
#' new_sftrack <- as_sftrack(sf_df, group = c(id = "animal_id"), time = "timestamp")
#' head(new_sftrack)
#'
#' # Input is an sftrack object
#' my_track <- as_sftrack(raccoon,
#'   time = "timestamp",
#'   error = NA, coords = c("longitude", "latitude"),
#'   group = burstz
#' )
#'
#' new_traj <- as_sftraj(my_track)
#' head(new_traj)
#' ######################
as_sftraj <- function(data = data.frame(), ...) {
  UseMethod("as_sftraj", object = data)
}

#' @title Define an sftraj
#' @param data  data.frame with multi_burst column, geometry column, time_col (integer/POSIXct), and error column (optional)
#' @param group_col column name of multi_burst in `data`
#' @param sf_col column name of geometry in `data`
#' @param time_col column name of time in `data`
#' @param error_col column name of error in `data`
#' @export
new_sftraj <-
  function(data,
           group_col,
           sf_col,
           time_col,
           error_col = NA) {
    data_sf <- sf::st_sf(data, sf_column_name = sf_col)
    structure(
      data_sf,
      group_col = group_col,
      time_col = time_col,
      error_col = error_col,
      class = c("sftraj", "sf", "data.frame")
    )
  }
#########################
# Methods
#' @rdname as_sftraj
#' @export
#' @method as_sftraj data.frame
as_sftraj.data.frame <- function(data = data.frame(),
                                 ...,
                                 coords = c("x", "y"),
                                 group = "id",
                                 active_group = NA,
                                 time = "time",
                                 error = NA,
                                 crs = NA,
                                 zeroNA = FALSE,
                                 group_name = "sft_group",
                                 timestamp_name = "sft_timestamp",
                                 error_name = "sft_error",
                                 overwrite_names = FALSE) {
  # data = data.frame()
  # coords = sub_gps[,c('longitude','latitude')]
  # burst = list(id=sub_gps$id, numSat = sub_gps$numSat)
  # time = sub_gps[,'timez']
  # crs=4326
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
  if (length(group) == 1) {
    names(group) <- "id"
  }
  if (all(sapply(group, length) == nrow(data))) {
    # check id in burst
    check_group_id(group)
    group_list <- group
  } else {
    # check names exist
    if (inherits(group, "list")) {
      group <- vapply(group, c, character(1))
    }
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
    if (timestamp_name %in% colnames(data) && !overwrite_names) {
      stop(paste0("column name: \"", timestamp_name, "\" already found in data.frame.
If youd like to overwrite column use overwrite_names = TRUE"))
    } else {
      data[[timestamp_name]] <- time
      time_col <- timestamp_name
    }
  } else {
    check_names_exist(data, time)
    time_col <- time
  }
  check_time(data[[time_col]])

  # Error
  if (length(error) == nrow(data)) {
    # Decide whether to overwrite names or not
    if (error_name %in% colnames(data) && !overwrite_names) {
      stop(paste0("column name: \"", error_name, "\" already found in data.frame.
If youd like to overwrite column use overwrite_names = TRUE"))
    } else {
      data[[error_name]] <- error
    }
    error_col <- error_name
  } else {
    if (!is.na(error)) {
      check_names_exist(data, error)
      error_col <- error
    } else {
      error_col <- NA
    }
  }

  # pull out other relevant info
  if (any(is.na(active_group))) {
    active_group <- names(group_list)
  }
  group <-
    make_c_grouping(group_list, active_group = active_group)

  # earliest reasonable time to check time stamps
  dup_timestamp(time = data[[time_col]], x = group)

  geom <-
    sf::st_as_sf(xyz,
      coords = names(xyz),
      crs = crs,
      na.fail = FALSE
    )
  # Force calculation of empty geometries.
  attr(geom[, attr(geom, "sf_column")], "n_empty") <-
    sum(vapply(st_geometry(geom), sfg_is_empty, TRUE))

  geom <-
    make_step_geom(
      group = group,
      geometry = st_geometry(geom),
      time_data = data[[time_col]]
    )
  # Decide whether to overwrite names or not
  if (group_name %in% colnames(data) && !overwrite_names) {
    stop(paste0("column name: \"", group_name, "\" already found in data.frame.
If youd like to overwrite column use overwrite_names = TRUE"))
  } else {
    data[[group_name]] <- group
  }
  data$geometry <- geom

  ret <- new_sftraj(
    data = data,
    group_col = group_name,
    sf_col = "geometry",
    error_col = error_col,
    time_col = time_col
  )
  # Sanity checks
  ret <- ret[check_ordered(ret[[attr(ret, "group_col")]], ret[[attr(ret, "time_col")]]), ]

  check_z_coords(ret)

  return(ret)
}

#' @rdname as_sftraj
#' @method as_sftraj sftrack
#' @export
as_sftraj.sftrack <- function(data, ...) {
  group_col <- attr(data, "group_col")
  error_col <- attr(data, "error_col")
  time_col <- attr(data, "time_col")
  sf_col <- attr(data, "sf_column")
  geometry <-
    make_step_geom(
      group = data[[group_col]],
      geometry = data[[sf_col]],
      time_data = data[[time_col]]
    )

  data[[sf_col]] <- st_geometry(geometry)

  new_data <- as.data.frame(data)
  ret <- new_sftraj(
    data = new_data,
    group_col = group_col,
    sf_col = sf_col,
    error_col = error_col,
    time_col = time_col
  )
  return(ret)
}

# sf
#' @rdname as_sftraj
#' @method as_sftraj sf
#' @export
as_sftraj.sf <- function(data,
                         ...,
                         coords,
                         group,
                         active_group = NA,
                         time,
                         error = NA,
                         group_name = "sft_group",
                         timestamp_name = "sft_timestamp",
                         error_name = "sft_error",
                         overwrite_names = FALSE) {
  geom <- st_geometry(data)

  data <- as.data.frame(data)
  # Check inputs
  # geom
  if (attributes(geom)$class[1] != "sfc_POINT") {
    stop("sf column must be an sfc_POINT")
  }
  # Id
  if (nrow(data) == 0) {
    data <- data.frame(sftrack_id = seq_along(time))
  }
  # bursts
  if (length(group) == 1) {
    names(group) <- "id"
  }
  if (all(sapply(group, length) == nrow(data))) {
    # check id in burst
    check_group_id(group)
    group_list <- group
  } else {
    # check names exist
    if (inherits(group, "list")) {
      group <- vapply(group, c, character(1))
    }
    check_names_exist(data, group)

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

  # Time
  if (length(time) == nrow(data)) {
    if (timestamp_name %in% colnames(data) && !overwrite_names) {
      stop(paste0("column name: \"", timestamp_name, "\" already found in data.frame.
If youd like to overwrite column use overwrite_names = TRUE"))
    } else {
      data[[timestamp_name]] <- time
      time_col <- timestamp_name
    }
  } else {
    check_names_exist(data, time)
    time_col <- time
  }
  check_time(data[[time_col]])

  # Error
  if (length(error) == nrow(data)) {
    # Decide whether to overwrite names or not
    if (error_name %in% colnames(data) && !overwrite_names) {
      stop(paste0("column name: \"", error_name, "\" already found in data.frame.
If youd like to overwrite column use overwrite_names = TRUE"))
    } else {
      data[[error_name]] <- error
    }
    error_col <- error_name
  } else {
    if (!is.na(error)) {
      check_names_exist(data, error)
      error_col <- error
    } else {
      error_col <- NA
    }
  }
  #
  if (any(is.na(active_group))) {
    active_group <- names(group_list)
  }
  group <-
    make_c_grouping(group_list, active_group = active_group)

  # earliest reasonable time to check time stamps
  dup_timestamp(time = data[[time_col]], x = group)

  # Decide whether to overwrite names or not
  if (group_name %in% colnames(data) && !overwrite_names) {
    stop(paste0("column name: \"", group_name, "\" already found in data.frame.
If youd like to overwrite column use overwrite_names = TRUE"))
  } else {
    data[[group_name]] <- group
  }

  geom <-
    make_step_geom(
      group = group,
      geometry = geom,
      time_data = data[[time_col]]
    )
  data$geometry <- st_geometry(geom)
  ret <- new_sftrack(
    data = data,
    group_col = group_name,
    sf_col = "geometry",
    error_col = error_col,
    time_col = time_col
  )
  # Sanity checks
  ret <- ret[check_ordered(ret[[attr(ret, "group_col")]], ret[[attr(ret, "time_col")]]), ]

  check_z_coords(ret)

  return(ret)
}


#' @rdname as_sftraj
#' @method as_sftraj ltraj
#' @export
as_sftraj.ltraj <- function(data, ...) {
  # This is done so we dont have to import adehabitat. (instead of ld())
  # But it could go either way depending
  new_data <- lapply(seq_along(data), function(x) {
    sub <- data[x, ]
    attributes(sub[[1]])
    id <- attr(sub[[1]], "id")
    burst <- attr(sub[[1]], "burst")
    infolocs <- infolocs(data)[x]
    sft_timestamp <- sub[[1]]$date
    coords <- c("x", "y")
    data.frame(sub[[1]][, coords], id, burst, sft_timestamp, infolocs)
  })
  df1 <- do.call(rbind, new_data)
  time_col <- "sft_timestamp"
  group <- list(id = df1$id)
  crs <- attr(data, "proj4string")
  # pull out id and burst from ltraj object
  id_lt <- vapply(data, function(x) {
    attr(x, "id")
  }, NA_character_)
  group_lt <-
    vapply(data, function(x) {
      attr(x, "burst")
    }, NA_character_)
  if (!all(group_lt == id_lt)) {
    group$group <- df1$burst
  }
  coords <- c("x", "y")
  geom <-
    sf::st_as_sf(df1[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  #
  group <- make_c_grouping(group)

  step_geometry <-
    make_step_geom(
      group = group,
      geometry = st_geometry(geom),
      time_data = df1[[time_col]]
    )
  df1$sft_group <- make_c_grouping(group)
  error <- NA
  new_data <-
    cbind(df1[, !colnames(df1) %in% c("id")], geometry = st_geometry(geom))
  ret <- new_sftraj(
    data = new_data,
    group_col = "sft_group",
    error_col = error,
    time_col = time_col,
    sf_col = "geometry"
  )
  # Sanity check. Which are necessary?
  ret <-
    ret[check_ordered(ret[[attr(ret, "group_col")]], ret[[attr(ret, "time_col")]]), ]
  #
  return(ret)
}

#' @title Print methods for sftraj
#' @name Print_sftraj_objects
#' @param x sftraj object
#' @param n_row Integer of number of rows to display. Defaults to global option default if non supplied
#' @param n_col Integer of number of columns to display + required sftrack columns (burst, geometry, time, and error). Defaults to global option default if non supplied
#' @param ... other arguments passed onto print
#' @export
print.sftraj <- function(x, n_row, n_col, ...) {
  if (missing(n_col)) {
    n_col <- ncol(x)
  }
  if (missing(n_row)) {
    n_row <- nrow(x)
  }
  group_col <- attr(x, "group_col")
  sf_col <- attr(x, "sf_column")
  time_col <- attr(x, "time_col")
  sf_attr <- attributes(st_geometry(x))
  # time stuff

  tcl <- class(x[[time_col]])[1]
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
  ab <- attr(x[[group_col]], "active_group")
  bn <- attr(x[[group_col]], "group_names")
  if (nrow(x) > 0) {
    geomxy <- class(x[[sf_col]][[1]])[1]
  } else {
    geomxy <- NA
  }
  active_group_names <- paste0("*", ab, "*")
  group_mes <-
    paste0(active_group_names, bn[!ab %in% bn], collapse = ", ")
  # print
  cat(
    paste0(
      "Sftraj with ",
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
    geomxy,
    ", crs: ",
    format(sf_attr$crs),
    ") \n"
  ))
  cat(paste0('Timestamp : \"', time_col, '\" (', time_mes, ") \n"))
  cat(paste0('Grouping : \"', group_col, '\" (', group_mes, ") \n"))
  cat("-------------------------------\n")
  # Figure out the row and columns
  row_l <- ifelse(nrow(x) > n_row, n_row, nrow(x))
  sub_col_names <-
    colnames(x)[!colnames(x) %in% c(group_col, sf_col, time_col)]
  col_l <- length(sub_col_names)

  x <- as.data.frame(x)

  if (n_col < col_l) {
    p <- ifelse(col_l > n_col, n_col, col_l)
    ret <- cbind(
      x[1:row_l, sub_col_names[1:p], drop = FALSE],
      data.frame("..." = rep("...", row_l)),
      x[1:row_l, c(group_col, sf_col, time_col)]
    )
  } else {
    ret <- x
  }

  ret <- ret[1:row_l, ]
  print(ret, ...)
}

#' @export
summary.sftraj <- function(object, ..., stats = FALSE) {
  if (stats) {
    summary_sftrack(object)
  } else {
    (NextMethod())
  }
}


#' @export
rbind.sftraj <- function(...) {
  all <- list(...)
  # all = list(my_sftraj, my_sftraj2)
  same_att <-
    all(duplicated(lapply(all, function(x) {
      attributes(x)[c("sf_column", "time", "error")]
    }))[-1])
  if (!same_att) {
    stop("sf, time, and error columns must be the same")
  }

  df1 <- do.call(rbind.data.frame, all)
  att <- attributes(df1)
  time_col <- att$time_col
  error_col <- att$error_col
  group_col <- att$group_col
  sf_col <- att$sf_column

  geom <- pts_traj(df1[[sf_col]], sfc = T)
  step_geometry <-
    make_step_geom(
      group = df1[[group_col]],
      geometry = geom,
      time_data = df1[[time_col]]
    )
  class(df1) <- setdiff(class(df1), c("sftraj", "sf"))
  ret <- new_sftraj(
    data = df1,
    group_col = group_col,
    time_col = time_col,
    error_col = error_col,
    sf_col = sf_col
  )
  # Sanity checks
  dup_timestamp(ret)
  return(ret)
}

#' @export
"[.sftraj" <- function(x, i, j, ..., drop = FALSE) {
  # x = my_sftraj
  # i = 1:10
  # j=c(1,2,3)
  # rm(j)
  sf_col <- attr(x, "sf_column")
  time_col <- attr(x, "time_col")
  error_col <- attr(x, "error_col")
  group_col <- attr(x, "group_col")
  # if(is.na(error_col)){ error_col <- NULL}
  nargs <- nargs()
  if (!missing(j) && missing(i)) {
    i <- seq_len(nrow(x))
  }
  if (!missing(i) && nargs > 2) {
    if (is.character(i)) {
      i <- group_labels(x) %in% i
    }
  }
  if (!missing(j) && all(is.character(j))) {
    j <- which(colnames(x) %in% j)
  }
  # x = as.data.frame(x)
  class(x) <- setdiff(class(x), c("sftraj", "sf"))
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
      x[i, union(colnames(x)[j], c(group_col, sf_col, time_col, error_col))]
    }
  }


  # # This piece is if we want drop = TRUE to be the default
  # if((any(!exist_name[1:3]) || !is.na(error_col) & !exist_name[4])){
  #
  #   message(paste0(paste0(c('burst','geometry','time','error')[!exist_name],collapse=', '),' subsetted out of sftrack object, reverting to ',class(x)[1],
  #     '\n Use drop = FALSE to retain class'))
  #
  #     }
  ret <- new_sftraj(
    x,
    group_col = group_col,
    sf_col = sf_col,
    time_col = time_col,
    error_col = error_col
  )
  dup_timestamp(ret)
  ret
}

#' @export
"[[<-.sftraj" <- function(x, i, value) {
  x <- structure(NextMethod(), class = c("sftraj", "sf", "data.frame"))
}
