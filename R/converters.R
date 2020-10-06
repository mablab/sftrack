#' #' @name as
#' #' @rdname coerce-methods
#' #' @aliases coerce,sftrack
#' # setOldClass(c('sftrack','ltraj'))
#' setAs("sftrack", "ltraj",function(from){
#'   # x=my_sftrack
#'   if (!requireNamespace("adehabitatLT", quietly = TRUE))
#'     stop("package adehabitatLT required: install first?")
#'   info_col <- setdiff(colnames(x),c(attr(x,'time_col'),
#'                                     attr(x,'group_col'),
#'                                     attr(x,'sf_column'))
#'   )
#'   adehabitatLT::as.ltraj(xy= get_point(x,'xy1'), date = t1(x), id = group_labels(my_sftrack),
#'                          infolocs = x[,  info_col, drop =T])
#' })
#'
#' #as.ltraj(my_sf)
#'
#' #' @name as
#' #' @rdname coerce-methods
#' #' @aliases coerce,sftrack
#' #'
#' setOldClass('sftraj')
#' setAs('sftrack','sftraj', function(from){
#'   sftrack::as_sftraj(from)
#' })
#' as(my_sf,'sftraj')
#'
#' as.sftraj(my_sf)
#'
#' adehabitatLT::as.ltraj.sftrack(my_sf)
#'
#' 'as.ltraj.sftrack' <- function(x,...){
#'     # x=my_sftrack
#'     if (!requireNamespace("adehabitatLT", quietly = TRUE))
#'       stop("package adehabitatLT required: install first?")
#'     info_col <- setdiff(colnames(x),c(attr(x,'time_col'),
#'                                       attr(x,'group_col'),
#'                                       attr(x,'sf_column'))
#'     )
#'     adehabitatLT::as.ltraj(xy= sftrack::get_point(x,'xy1'), date = sftrack::t1(x), id = sftrack::group_labels(x),
#'                            infolocs = x[,  info_col, drop =T])
#' }
#'
#' as.ltraj <- function(x,...){
#'   UseMethod('as.ltraj')
#' }
#' as.ltraj(my_sf)
