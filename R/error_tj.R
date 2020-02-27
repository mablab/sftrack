#' Error Class
#' new_error_tj()
#'
#' This class exists as a placeholder. We do not
#' Currently know the details of what the error class
#' will look like.
#'
#' @param error error
#' @export new_error_tj
#' @examples
#' er <- new_error_tj()
new_error_tj <- function(error = NA,...) {
  structure(error,
    class = c("error_tj")
  )
}

#' @export
as.data.frame.error_tj <- function(x,...){
  ret = data.frame(row.names = seq_along(x))
  ret$error = x
  ret
}
