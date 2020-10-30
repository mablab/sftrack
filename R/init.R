#' @importFrom methods setClass
methods::setClass("sftrack", slots = c(time_col = "character", group_col = "character", sf_column = "character"))
methods::setClass("sftraj", slots = c(time_col = "character", group_col = "character", sf_column = "character"))
