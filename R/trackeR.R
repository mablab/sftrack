#' @title convert to and from trackeR
#' @rdname coerce-trackeR
#' @param data the data either sftrack,sftraj,or trackeRdata class to convert
#' @param sport (for amt conversions) The sport (cycling, running, swimming) to data is taken from
#' @param include_units whether to add the units() to the ends of the colnames so they may be retained when converting to sftrack
#' @param ... More arguments to be passed to `trackerData()`, please see its help file for more information.
#' @method as_sftrack trackeRdata
#' @export
#' @examples
#' #'
#'
#' library(trackeR)
#' filepath <- system.file("extdata/tcx/", "2013-06-01-183220.TCX.gz", package = "trackeR")
#' runDF <- readTCX(file = filepath, timezone = "GMT")
#' runTr0 <- trackeRdata(runDF)
#'
#' # convert to sftrack
#' my_sftrack <- as_sftrack(runTr0)
#' head(my_sftrack)
#' head(as_sftraj(runTr0))
#'
#' # convert back
#' sftrack_to_trackeR(my_sftrack, sport = "running")
as_sftrack.trackeRdata <- function(data, ..., include_units = FALSE) {
  att <- attributes(data)
  nrow_len <- vapply(data, nrow, numeric(1))
  df1 <- as.data.frame(data)

  # df1$sport <- rep(att$sport,each = nrow_len)
  # df1$file <- rep(att$file,each = nrow_len)
  time_col <- "time"
  coords <- c("latitude", "longitude")
  crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  if (include_units) {
    colnames(df1)
    sub_units <- att$unit[att$units$sport %in% att$sport, ]
    single_unit <- tapply(sub_units$unit, sub_units$variable, function(x) length(unique(x)) == 1)
    if (any(!single_unit)) {
      print(
        paste(
          "mismatch in units for variables: \n",
          paste0(names(single_unit)[single_unit], collapse = ", "),
          "Not adding unit names to these columns", "\n"
        )
      )
    }
    final_names <- names(single_unit)[single_unit]
    for (i in final_names) {
      colnames(df1)[colnames(df1) == i] <- paste(colnames(df1)[colnames(df1) == i], sub_units$unit[sub_units$variable == i], sep = "_")
    }
    coords <- c(
      paste0("latitude_", sub_units$unit[sub_units$variable == "latitude"]),
      paste0("longitude_", sub_units$unit[sub_units$variable == "longitude"])
    )
  }

  group <- list(id = rep("id1", times = nrow(df1)), session = df1$session)
  geom <-
    sf::st_as_sf(df1[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  # group
  df1$sft_group <- make_c_grouping(group)

  # time
  df1[[time_col]] <- sft_time(df1[[time_col]])

  error <- NA
  new_data <-
    cbind(df1[, !colnames(df1) %in% c("id")], geometry = st_geometry(geom))
  ret <- new_sftrack(
    data = new_data,
    group_col = "sft_group",
    error_col = error,
    time_col = time_col,
    sf_col = "geometry"
  )
  # Sanity check. Which are necessary?
  ret <-
    ret[check_ordered(ret[[attr(ret, "group_col")]], t1(ret)), ]
  #
  return(ret)
}

#' @rdname coerce-trackeR
#' @method as_sftraj trackeRdata
#' @export
as_sftraj.trackeRdata <- function(data, ..., include_units = FALSE) {
  att <- attributes(data)
  nrow_len <- vapply(data, nrow, numeric(1))
  df1 <- as.data.frame(data)

  # df1$sport <- rep(att$sport,each = nrow_len)
  # df1$file <- rep(att$file,each = nrow_len)
  time_col <- "time"
  coords <- c("latitude", "longitude")
  crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  if (include_units) {
    colnames(df1)
    sub_units <- att$unit[att$units$sport %in% att$sport, ]
    single_unit <- tapply(sub_units$unit, sub_units$variable, function(x) length(unique(x)) == 1)
    if (any(!single_unit)) {
      print(
        paste(
          "mismatch in units for variables: \n",
          paste0(names(single_unit)[single_unit], collapse = ", "),
          "Not adding unit names to these columns", "\n"
        )
      )
    }
    final_names <- names(single_unit)[single_unit]
    for (i in final_names) {
      colnames(df1)[colnames(df1) == i] <- paste(colnames(df1)[colnames(df1) == i], sub_units$unit[sub_units$variable == i], sep = "_")
    }
    coords <- c(
      paste0("latitude_", sub_units$unit[sub_units$variable == "latitude"]),
      paste0("longitude_", sub_units$unit[sub_units$variable == "longitude"])
    )
  }

  group <- list(id = rep("id1", times = nrow(df1)), session = df1$session)

  # group
  df1$sft_group <- make_c_grouping(group)

  # time
  df1[[time_col]] <- make_timestamp(df1[[time_col]], df1$sft_group)

  geom <-
    sf::st_as_sf(df1[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  step_geometry <-
    make_step_geom(
      group = df1$sft_group,
      geometry = st_geometry(geom),
      time = df1[[time_col]]
    )

  error <- NA
  new_data <-
    cbind(df1[, !colnames(df1) %in% c("id")], geometry = st_geometry(step_geometry))
  ret <- new_sftraj(
    data = new_data,
    group_col = "sft_group",
    error_col = error,
    time_col = time_col,
    sf_col = "geometry"
  )
  # Sanity check. Which are necessary?
  ret <-
    ret[check_ordered(ret[[attr(ret, "group_col")]], t1(ret)), ]
  #
  return(ret)
}

#' #' @title Coerce methods
#' #' @name as
#' #' @rdname coerce-methods
#' #' @aliases coerce,sftrack
#' #' # setAs("sftrack", "trackeRdata",function(from){#   sftrack_to_trackeR(from,...)}

#' @rdname coerce-trackeR
#' @export
sftrack_to_trackeR <- function(data, sport, ...) {
  if (!requireNamespace("trackeR", quietly = TRUE)) {
    stop("package trackeR required: install first?")
  }
  if (!"time" %in% colnames(data)) {
    time_col <- attr(data, "time_col")
    data$time <- t1(data)
  } else {
    class(data$time) <- setdiff(class(data$time), "sft_timestamp")
    data$time <- as.POSIXct(data$time)
    time_col <- NULL
  }
  if (!"latitude" %in% colnames(data)) {
    data$latitude <- get_point(data, "x1")
  }
  if (!"longitude" %in% colnames(data)) {
    data$longitude <- get_point(data, "x1")
  }
  if (class(st_geometry(data)[[1]])[1] == "XYZ" & !"altitude" %in% colnames(data)) {
    data$altitude <- get_point(data, "z1")
  }

  info_col <- setdiff(colnames(data), c(
    time_col,
    attr(data, "group_col"),
    attr(data, "sf_column")
  ))

  trackeR::trackeRdata(data[, info_col, drop = T], sport = sport)
}

#' @rdname coerce-trackeR
#' @export
sftraj_to_trackeR <- function(data, sport, ...) {
  if (!requireNamespace("trackeR", quietly = TRUE)) {
    stop("package trackeR required: install first?")
  }
  data <- as_sftrack(data)
  if (!"time" %in% colnames(data)) {
    time_col <- attr(data, "time_col")
    data$time <- t1(data)
  } else {
    class(data$time) <- setdiff(class(data$time), "sft_timestamp")
    data$time <- as.POSIXct(data$time)
    time_col <- NULL
  }
  if (!"latitude" %in% colnames(data)) {
    data$latitude <- get_point(data, "y1")
  }
  if (!"longitude" %in% colnames(data)) {
    data$longitude <- get_point(data, "x1")
  }
  if (class(st_geometry(data)[[1]])[1] == "XYZ" & !"altitude" %in% colnames(data)) {
    data$altitude <- get_point(data, "z1")
  }

  info_col <- setdiff(colnames(data), c(
    time_col,
    attr(data, "group_col"),
    attr(data, "sf_column"),
    "pace",
    "cumulative_elevation_gain",
    "session"
  ))

  trackeR::trackeRdata(data[, info_col, drop = T], sport = sport)
}
