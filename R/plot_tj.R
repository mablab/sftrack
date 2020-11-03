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
#' @param mode Character; either of \code{"steps+points"},
#'     \code{"steps"}, or \code{"trajectories"}, defaults to
#'     \code{"steps+points"}, and switch to \code{"steps"} if there
#'     are more than 10,000 steps.
#' @param ... Further arguments passed to 'plot.sf'. Among others,
#'     arguments for the key are set differently in 'sftrack' to allow
#'     for longer labels by default (but can be nevertheless
#'     adjusted).
#' @details Plotting mode refers to considering the trajectory as
#'     connected elements. For \code{"steps+points"}, this means it
#'     will plot each step & point individually. The alternative,
#'     faster \code{"steps"} mode merges connected steps into
#'     multilinestrings, which are plotted as continuous lines.  This
#'     approach is much faster to plot with large objects, and is thus
#'     turned automatically on when n(steps)>10,000. Finally,
#'     \code{"trajectories"} merges the full trajectory as a continuous
#'     line (i.e. ignores gaps).
#' @method plot sftrack
#' @importFrom graphics plot lcm
#' @examples
#'
#' ## Plotting with sftrack is just like sf. `...` will accept most
#' ## arguments as 'plot.sf':
#' plot(racc_track, axes = TRUE, lwd = 5, cex = 5, bgc = "gray50")
#'
#' ## sftraj will as well for the most part; however it is a more complex
#' ## structure that combines points and steps (in step mode):
#' plot(racc_traj, lwd = 5, cex = 5, bgc = "gray50", graticule = TRUE)
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
plot.sftraj <- function(x, y, key.pos, key.width, ..., mode) {
  if (missing(key.pos))
    key.pos <- 4
  if (missing(key.width))
  {
    if (key.pos %in% c(2, 4))
      key.width <- lcm(4)
    else key.width <- lcm(1.8)
  }
  if (missing(mode)) {
    mode <- if (nrow(x) < 10000) {
      "steps+points"
    } else {
      "steps"
    }
  } else {
    if (!mode %in% c("steps+points", "steps", "trajectories"))
      stop("Invalid 'mode'.")
  }

  group_col <- attr(x, "group_col")
  if (mode == "steps+points")
  {
    x[[group_col]] <- group_labels(x)
    class(x) <- setdiff(class(x), c("sftraj"))
    x <- x[group_col]
  }
  if (mode == "steps")
  {
    warning("Plotting in 'steps' mode. See details in the help of 'plot.sftraj'.")
    x <- merge_traj(x, mode = "steps")
  }
  if (mode == "trajectories")
  {
    warning("Plotting in 'trajectories' mode. See details in the help of 'plot.sftraj'.")
    x <- merge_traj(x, mode = "trajectories")
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
#'
#' require("ggplot2")
#' ggplot() +
#'   geom_sftrack(data = racc_traj)
#'
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
