#' @importFrom methods setClass
setClass("sftrack", slots = c(time_col = "character", group_col = "character", sf_column = "character"))
setClass("sftraj", slots = c(time_col = "character", group_col = "character", sf_column = "character"))
