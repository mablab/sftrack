#' @title convert to and from ltraj
#' @rdname coerce-ltraj
#' @param data the data either sftrack,sftraj,or ltraj class to convert
#' @param ... ignored
#' @method as_sftrack ltraj
#' @export
#' @examples
#' #'
#' ### Ltraj Methods
#' # Input is a ltraj
#' library(adehabitatLT)
#' ltraj_df <- as.ltraj(
#'   xy = raccoon[, c("longitude", "latitude")],
#'   date = as.POSIXct(raccoon$timestamp),
#'   id = raccoon$animal_id, typeII = TRUE,
#'   infolocs = raccoon[, 1:6]
#' )
#' my_sftrack <- as_sftrack(ltraj_df)
#' head(my_sftrack)
#'
#' my_sftraj <- as_sftraj(ltraj_df)
#' head(my_sftraj)
#'
#' # convert back
#' sftrack_to_ltraj(my_sftrack)
#' sftraj_to_ltraj(my_sftraj)
as_sftrack.ltraj <- function(data, ...) {
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


#' @rdname coerce-ltraj
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
  group <- make_c_grouping(group)
  # time

  df1[[time_col]] <- make_timestamp(df1[[time_col]], group)

  geom <-
    sf::st_as_sf(df1[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE
    )
  #
  step_geometry <-
    make_step_geom(
      group = group,
      geometry = st_geometry(geom),
      time = df1[[time_col]]
    )
  df1$sft_group <- make_c_grouping(group)
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
  # Sanity checks? Which ones are necessary?
  ret <-
    ret[check_ordered(ret[[attr(ret, "group_col")]], t1(ret)), ]
  #
  return(ret)
}

#' #' @name as
#' #' @rdname coerce-methods
#' #' @aliases coerce,sftrack
#' #' @export
#' setOldClass('ltraj')
#' setAs("sftrack", "ltraj",function(from){
#'   sftrack_to_ltraj(from)})


#' @rdname coerce-ltraj
#' @export
sftrack_to_ltraj <- function(data) {
  # data=my_sftrack
  if (!requireNamespace("adehabitatLT", quietly = TRUE)) {
    stop("package adehabitatLT required: install first?")
  }
  info_col <- setdiff(colnames(data), c(
    attr(data, "time_col"),
    attr(data, "group_col"),
    attr(data, "sf_column")
  ))
  adehabitatLT::as.ltraj(
    xy = get_point(data, "xy1"), date = t1(data), id = group_labels(data),
    infolocs = data[, info_col, drop = T]
  )
}

#' #' @name as
#' #' @rdname coerce-methods
#' #' @aliases coerce,sftracj
#' #' @export
#' setAs("sftraj", "ltraj",function(from){
#'   sftraj_to_ltraj(from)})

#' @rdname coerce-ltraj
#' @export
sftraj_to_ltraj <- function(data) {
  # data=my_sftrack
  if (!requireNamespace("adehabitatLT", quietly = TRUE)) {
    stop("package adehabitatLT required: install first?")
  }
  # the fastest way is just to use as_sftrack
  data <- as_sftrack(data)
  info_col <- setdiff(colnames(data), c(
    attr(data, "time_col"),
    attr(data, "group_col"),
    attr(data, "sf_column")
  ))
  adehabitatLT::as.ltraj(
    xy = get_point(data, "xy1"), date = t1(data), id = group_labels(data),
    infolocs = data[, info_col, drop = T]
  )
}
