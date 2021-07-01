#' @title Methods for plotting sftrack/sftraj
#' @name plot_sftrack
#' @export
#' @param x 'sftrack' or 'sftraj' object
#' @param y ignored
#' @param key.pos Integer; side to plot a color key: 1 bottom, 2 left,
#'     3 top, 4 right; set to NULL to omit key, or -1 to select
#'     automatically (defaults to 4; see \code{\link[sf]{plot_sf}} for
#'     more details).
#' @param key.width Amount of space reserved for the key, including
#'     labels (see \code{\link[sf]{plot_sf}} for more details).)
#' @param step_mode Logical; whether to plot in step mode, see
#'     details, defaults to TRUE, unless there are more than 10,000
#'     steps.
#' @param ... Further arguments passed to 'plot.sf'. Among others,
#'     arguments for the key are set differently in 'sftrack' to allow
#'     for longer labels by default (but can be nevertheless
#'     adjusted).
#' @details Step mode refers to considering the trajectory as
#'     individual 'steps', in the case of plot this means it will plot
#'     each line & point individually. This approach is much slower to
#'     plot with large objects, and is thus turned off when
#'     n(steps)>10,000.  The alternative, much faster method is to
#'     merge the steps into a multilinestring as continuous lines.
#' @method plot sftrack
#' @importFrom graphics plot lcm
#' @examples
#'
#' ## Prepare an 'sftrack' object:
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- c(id = "animal_id")
#' my_sftrack <- as_sftrack(raccoon,
#'   time = "timestamp",
#'   coords = c("longitude", "latitude"),
#'   group = burstz
#' )
#'
#' ## Plotting with sftrack is just like sf. `...` will accept most
#' ## arguments as 'plot.sf':
#' plot(my_sftrack, axes = TRUE, lwd = 5, cex = 5, bgc = "gray50")
#'
#' ## sftraj will as well for the most part; however it is a more complex
#' ## structure that combines points and steps (in step mode):
#' my_sftraj <- as_sftraj(raccoon,
#'   time = "timestamp",
#'   coords = c("longitude", "latitude"),
#'   group = burstz
#' )
#' plot(my_sftraj, lwd = 5, cex = 5, bgc = "gray50", graticule = TRUE)
#'
plot.sftrack <- function(x, y, key.pos, key.width, ...) {
  if (missing(key.pos))
    key.pos <- 4
  if (missing(key.width))
  {
    if (key.pos %in% c(2, 4))
      key.width <- lcm(4)
    else key.width <- lcm(1.8)
  }
  group_col <- attr(x, "group_col")
  x[[group_col]] <- group_labels(x)
  class(x) <- setdiff(class(x), c("sftrack"))
  x <- x[group_col]
  plot(x, reset = FALSE, key.pos = key.pos, key.width = key.width, ...)
}

#' @title Methods for plotting sftrack/sftraj
#' @export
#' @rdname plot_sftrack
#' @method plot sftraj
#' @importFrom graphics plot
plot.sftraj <- function(x, y, key.pos, key.width, ..., step_mode) {
  if (missing(key.pos))
    key.pos <- 4
  if (missing(key.width))
  {
    if (key.pos %in% c(2, 4))
      key.width <- lcm(4)
    else key.width <- lcm(1.8)
  }
  if (missing(step_mode)) {
    step_mode <- if (nrow(x) < 10000) {
      TRUE
    } else {
      FALSE
    }
  }
  group_col <- attr(x, "group_col")
  if (step_mode)
  {
    x[[group_col]] <- group_labels(x)
    class(x) <- setdiff(class(x), c("sftraj"))
    x <- x[group_col]
  } else
  {
    warning("Step mode disabled. See details in the help of 'plot.sftraj'.")
    x <- merge_traj(x)
  }
  plot(x, reset = FALSE, key.pos = key.pos, key.width = key.width, ...)
}

#' @title Function to plot sftrack objects in ggplot
#' @name geom_sftrack
#' @description
#' This function can be added to ggplot() to plot an sftrack and sftraj
#' Function does not yet work with ggplot grammer so you must but data= in this function
#' @details step mode refers to considering the trajectory as individual 'steps', in the case of plot this means it will
#' plot each line & point individually. This approach is much slower to plot when n(steps)>10,000.
#' The alternative method is to merge the steps into a multilinestring of continuous lines. This is much faster to plot.
#' @param mapping mapping aesthetics for ggplot.
#' @param data the sftraj or sftrack object.
#' @param ... arguments to passed to ggplot
#' @param step_mode TRUE/FALSE, whether to plot in step_mode, See details
#' @examples
#' #'
#' require("ggplot2")
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- c(id = "animal_id")
#'
#' # sftraj will as well for the most part, however as its a more complex
#' # structure to speed up plotting.
#' my_sftraj <- as_sftraj(raccoon,
#'   time = "timestamp",
#'   coords = c("longitude", "latitude"),
#'   group = burstz
#' )
#'
#' ggplot() +
#'   geom_sftrack(data = my_sftraj)
#' @export
geom_sftrack <- function(mapping, data, ...) {
  UseMethod("geom_sftrack")
}

#' @rdname geom_sftrack
#' @export
geom_sftrack.sftrack <-
  function(mapping = ggplot2::aes(),
           data = NULL,
           ...) {
    group <- group_labels(data)
    class(data) <- class(data)[2:3]
    list(
      ggplot2::geom_sf(data = data, ggplot2::aes(color = group), ...)
    )
  }

#' @name geom_sftrack
#' @export
geom_sftrack.sftraj <-
  function(mapping = ggplot2::aes(),
           data = NULL,
           ..., step_mode = FALSE) {
    # data = my_sftraj
    group_col <- attr(data, "group_col")
    if (step_mode) {
      data[[group_col]] <- group_labels(data)
      class(data) <- setdiff(class(data), c("sftraj"))
      data <- data[group_col]
    } else {
      data <- merge_traj(data)
    }
    group <- data[[group_col]]
    list(
      ggplot2::geom_sf(data = data, ggplot2::aes(color = group), ...)
    )
  }

# ggplot() + geom_sf(data = data, aes(color = .data[[group_col]]))
