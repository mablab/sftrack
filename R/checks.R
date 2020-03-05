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

check_names_exist <- function(data, burst_col, coords, error_col, time_col){
  # check burst
  col_names <- colnames(data)
  if(!missing('burst_col') && !burst_col %in% col_names){
     stop('Burst column names not found in data.frame')
  }

#' Checks if inputted column names are in data.frame
#' @export
  if(!missing('coords') && !coords %in% col_names){
    stop('Coordinate column names not found in data.frame')
  }

  if(!missing('error_col') && !error_col %in% col_names){
    stop('Error column not found in data.frame')
  }

  if(!missing('time_col') && !time_col %in% col_names){
    stop('Time column not found in data.frame')
  }
}

#check_names_exist(data=data, coords = 'what')
