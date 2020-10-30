#' @title convert to and from amt class 'track_xy'
#' @rdname coerce-amt
#' @param data the data either sftrack,sftraj,or track_xy class to convert
#' @param crs a crs string from rgdal of the crs and projection information for the spatial data. Defaults to the one provided by \code{data}.
#' @param ... ignored
#' @method as_sftrack track_xy
#' @export
#' @examples
#'
#' library("amt")
#' data(sh)
#'
#' ## Add timestamp column, remove duplicated timestamps, and add ID
#' #column:
#' sh$timestamp <- as.POSIXct(paste(sh$day,sh$time))
#' sh <- sh[!duplicated(sh$timestamp), ]
#' sh$id <- "Animal 1"
#'
#' ## Create the 'track_xyt' object:
#' tr1 <- make_track(sh, x_epsg31467, y_epsg31467, timestamp,
#'                   id = id
#' )
#'
#' head(as_sftrack(tr1, crs = "+init=epsg:31467"))
as_sftrack.track_xy <- function(data, crs = attr(data, "crs_"), ...) {
  if (!inherits(data, "track_xyt")) {
    stop("object needs to be of class track_xyt")
  }

  coords <- c("x_", "y_")
  if (is.null(crs))
      stop("Data has no associated CRS.")
  data <- as.data.frame(data)
  extra_col <- setdiff(colnames(data), c("x_", "y_", "t_", "id"))
  # group
  group_name <- "sft_group"
  group <-
    make_c_grouping(list(id = data$id))
  data[[group_name]] <- group
  # time
  check_time(data$t_)
  dup_timestamp(time = data$t_, x = group)
  time_col <- "sft_timestamp"
  data[[time_col]] <- make_timestamp(data$t_)
  # geom
  geom <-
    sf::st_as_sf(data[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  # Force calculation of empty geometries.
  attr(geom[, attr(geom, "sf_column")], "n_empty") <-
    sum(vapply(st_geometry(geom), sfg_is_empty, TRUE))


  data$geometry <- st_geometry(geom)

  ret <- new_sftrack(
    data = data[, c(extra_col, time_col, group_name, "geometry")],
    group_col = group_name,
    sf_col = "geometry",
    error_col = NA,
    time_col = time_col
  )
  # Sanity checks
  ret <- ret[check_ordered(ret[[attr(ret, "group_col")]], t1(ret)), ]

  check_z_coords(ret)
  ret
}

#' @rdname coerce-amt
#' @export
as_sftraj.track_xy <- function(data, ...) {
  if (!inherits(data, "track_xyt")) {
    stop("object needs to be of class track_xyt")
  }

  coords <- c("x_", "y_")
  crs <- attr(data, "crs_")

  data <- as.data.frame(data)
  extra_col <- setdiff(colnames(data), c("x_", "y_", "t_", "id"))
  # group
  group_name <- "sft_group"
  group <-
    make_c_grouping(list(id = data$id))
  data[[group_name]] <- group
  # time
  check_time(data$t_)
  dup_timestamp(time = data$t_, x = group)
  time_col <- "sft_timestamp"
  data[[time_col]] <- make_timestamp(data$t_, group)

  geom <-
    sf::st_as_sf(data[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  #
  step_geometry <-
    make_step_geom(
      group = group,
      geometry = st_geometry(geom),
      time = data[[time_col]]
    )

  data$geometry <- st_geometry(step_geometry)

  ret <- new_sftrack(
    data = data[, c(extra_col, time_col, group_name, "geometry")],
    group_col = group_name,
    sf_col = "geometry",
    error_col = NA,
    time_col = time_col
  )
  # Sanity checks
  ret <- ret[check_ordered(ret[[attr(ret, "group_col")]], t1(ret)), ]

  check_z_coords(ret)
  ret
}
#' @rdname coerce-amt
#' @param data the sftrack object
#' @param ... any arguments to be passed on to `amt::make_track()`. See the help file in `amt` for more details
#' @export
sftrack_to_track_xy <- function(data, ...) {
  if (!requireNamespace("amt", quietly = TRUE)) {
    stop("package amt required: install first?")
  }

  data$x_ <- t(get_point(data, "x1"))
  data$y_ <- t(get_point(data, "y1"))
  data$t_ <- t1(data)
  data$id_ <- group_labels(data)
  info_col <- setdiff(colnames(data), c(
    attr(data, "time_col"),
    attr(data, "group_col"),
    attr(data, "sf_column")
  ))
  amt::make_track(data[, info_col, drop = T], x_, y_, t_, ...)
}
#' #' @title Coerce methods
#' #' @name as
#' #' @rdname coerce-methods
#' #' @aliases coerce,sftrack
#'
#' setAs("sftrack", "track_xy",function(from){
#'   sftrack_to_track_xy(from)})
