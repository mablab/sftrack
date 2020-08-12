#' @title methods for plot sftrack/sftraj
#' @name plot_sftrack
#' @export
#' @param x sftrack/sftraj object
#' @param ... arguments to passed to plot
#' @method plot sftrack
#' @examples
#' library(sftrack)
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- c(id = "animal_id")
#' my_sftrack <- as_sftrack(raccoon,
#'   time = "timestamp",
#'   coords = c("longitude", "latitude"),
#'   burst = burstz
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
#'   burst = burstz
#' )
#' plot(my_sftraj, axes = TRUE, lwd = 5, cex = 5, bgc = "gray80", graticule = TRUE)
plot.sftrack <- function(x, ...) {
  # x <- my_sftrack
  graphics::par(oma = c(1, 1, 1, 4))
  x$burst <- burst_labels(x)
  class(x) <- setdiff(class(x), c("sftrack"))
  x <- x["burst"]
  plot(x, ...)
}
# plot(my_sftrack)
#' @title methods for plot sftrack/sftraj
#' @export
#' @rdname plot_sftrack
#' @method plot sftraj
plot.sftraj <- function(x, y, ..., step_mode = FALSE) {
  # x <- my_sftraj
  graphics::par(oma = c(1, 1, 1, 4))
  if (step_mode) {
    x$burst <- burst_labels(x)
    class(x) <- setdiff(class(x), c("sftraj"))
    x <- x["burst"]
  } else {
    x <- merge_traj(x)
  }

  plot(x, reset = FALSE, ...)
}

# plot(my_sftraj, lwd=5, axes = T)
# plot(my_step)
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
#'   burst = burstz
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
    bursts <- burst_labels(data, factor = T)
    list(
      ggplot2::geom_sf(data = data, ggplot2::aes(color = bursts), ...)
    )
  }

#' @name geom_sftrack
#' @export
geom_sftrack.sftraj <-
  function(mapping = ggplot2::aes(),
           data = NULL,
           ..., step_mode = FALSE) {
    # x = my_sftraj
    if (step_mode) {
      data$burst <- burst_labels(data)
      class(data) <- setdiff(class(data), c("sftraj"))
      data <- data["burst"]
    } else {
      data <- merge_traj(data)
    }
    list(
      ggplot2::geom_sf(data = data, ggplot2::aes(color = burst), ...)
    )
  }


# ggplot() + geom_sftrack(data = my_sftraj)
