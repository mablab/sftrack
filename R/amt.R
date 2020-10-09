#' @title convert to and from amt class 'track_xy'
#' @rdname coerce-amt
#' @param data the data either sftrack,sftraj,or track_xy class to convert
#' @param ... ignored
#' @method as_sftrack track_xy
#' @export
as_sftrack.track_xy <- function(data, ...) {
  if (inherits(data, "track_xyt")) {
    coords <- c("x_", "y_")
  } else {
    coords <- c("x_", "y_", "t_")
  }
  crs <- attr(data, "crs_")

  data <- as.data.frame(data)
  extra_col <- setdiff(colnames(data), c("x_", "y_", "t_", "id"))

  group_name <- "sft_group"
  group <-
    make_c_grouping(list(id = data$id))
  data[[group_name]] <- group
  check_time(data$t_)
  dup_timestamp(time = data$t_, x = group)
  time_col <- "sft_timestamp"
  data[[time_col]] <- make_timestamp(data$t_, group)

  geom <-
    sf::st_as_sf(data[],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  # Force calculation of empty geometries.
  attr(geom[, attr(geom, "sf_column")], "n_empty") <-
    sum(vapply(st_geometry(geom), sfg_is_empty, TRUE))


  data$geometry <- geom

  ret <- new_sftrack(
    data = data,
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
sftrack_to_track_xy <- function(data) {
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
  amt::make_track(data[, info_col, drop = T], x_, y_, t_)
}
#' #' @title Coerce methods
#' #' @name as
#' #' @rdname coerce-methods
#' #' @aliases coerce,sftrack
#' #setClass('sftrack',slots = c(time_col='character',group_col='character',sf_column='character', error_col='character'))
#'
#' setAs("sftrack", "track_xy",function(from){
#'   sftrack_to_track_xy(from)})
