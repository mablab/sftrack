#' @title methods for plot sftrack/sftraj
#' @name plot_sftrack
#' @export
#' @param x sftrack/sftraj object
#' @param y ignored
#' @param step_mode TRUE/FALSE, whether to plot in step_mode, See details
#' @param ... arguments to passed to plot
#' @details step mode refers to considering the trajectory as individual 'steps', in the case of plot this means it will
#' plot each line & point individually. This approach is much slower to plot when n(steps)>10,000.
#' The alternative method is to merge the steps into a multilinestring of continuous lines. This is much faster to plot.
#' @method plot sftrack
#' @examples
#' #'
#' library(sftrack)
#'
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- c(id = "animal_id")
#' my_sftrack <- as_sftrack(raccoon,
#'   time = "timestamp",
#'   coords = c("longitude", "latitude"),
#'   group = burstz
#' )
#'
#' # Plotting with sftrack is just like sf. `...` will accept most arguments as plot.sf
#'
#' plot(my_sftrack, axes = TRUE, lwd = 5, cex = 5, bgc = "gray80")
#'
#' # sftraj will as well for the most part, however as its a more
#' # complex structure to speed up plotting.
#' my_sftraj <- as_sftraj(raccoon,
#'   time = "timestamp",
#'   coords = c("longitude", "latitude"),
#'   group = burstz
#' )
#'
#' plot(my_sftraj, axes = TRUE, lwd = 5, cex = 5, bgc = "gray80", graticule = TRUE, step_mode = TRUE)
#' #'
plot.sftrack <- function(x, y, ...) {
  # x <- my_sftrack
  graphics::par(oma = c(1, 1, 1, 4))
  group_col <- attr(x, "group_col")
  x[['group']] <- group_labels(x)
  class(x) <- setdiff(class(x), c("sftrack"))
  x <- x['group']
  plot(x, reset = FALSE, ...)
}


#' @title methods for plot sftrack/sftraj
#' @export
#' @rdname plot_sftrack
#' @method plot sftraj
plot.sftraj <- function(x, y, ..., step_mode = FALSE) {
  # x <- my_sftraj
  graphics::par(oma = c(1, 1, 1, 4))
  group_col <- attr(x, "group_col")
  if (step_mode) {
    x[[group_col]] <- group_labels(x)
    class(x) <- setdiff(class(x), c("sftraj"))
    x <- x[group_col]
  } else {
    x <- merge_traj(x)
  }

  plot(x, reset = FALSE, ...)
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
#' library(ggplot2)
#' library(sftrack)
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
