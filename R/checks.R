#' Sanity checks
#' @export
time_exists <- function(data, time){
  if(!time %in% colnames(data)) { stop('Time column not found in data') }
}
#' @export
error_exists <- function(data, error){
  if(!error %in% colnames(data)) { stop('Error column not found in data') }
}

#' Checks if burst is ordered by time and then outputs the correct order
#' @export
ordered <- function(burst, time_data, return = TRUE){

  idz <- factor(paste0(burst))

  # may not be as fast as something involving order(time_data, idz)
  isOrdered <- all(tapply(time_data, idz, function(x) identical(order(x), seq_len(length(x)))))
  if(isOrdered & return){ return(seq_along(time_data)) }
  if(!isOrdered){ message(paste0('time was not ordered for each burst'))
    if(return) { return(order(idz,time_data))}
    }
}

#' @export
check_names_exist <- function(data, names){
  # check burst
  col_names <- colnames(data)
  test <- !(names %in% colnames(data))
  if(any(test)){  stop(paste0(paste0(names[test],collapse= ' & '), ' not found in data frame', collapse = ' '))}

}

#check_names_exist(data=data, coords = 'what')
