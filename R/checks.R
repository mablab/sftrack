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

#' Check if a set of column names are found in a data frame and return an error if not
#' @export
check_names_exist <- function(data, names){
  # check burst
  col_names <- colnames(data)
  test <- !(names %in% colnames(data))
  if(any(test)){  stop(paste0(paste0(names[test],collapse= ' & '), ' not found in data frame', collapse = ' '))}

}

#' check that time is unique
#' @param x An sftrack/sftraj object
dup_timestamp <- function(x){
  test <- tapply(x[, attr(x, 'time'), drop=T]   ,attr(x$burst, 'sort_index'), function(y) any(duplicated(y)))
  if(any(test)){stop(paste0( paste0(names(test)[test],collapse=', '), ' have duplicated time stamps'))}
}

##### Burst related checks
# no NAs in burst
#' @export
NAburst <- function(burst){
  if(any(is.na(unlist(burst)))){stop('NAs not allowed in burst')}
}
#' @export
# more than one relocation for a burst
check_two_bursts <- function(burst){
  count <- table(attr(burst,'sort_index'))
if( any(count==1) ){warning(paste0(paste0(names(count)[count==1], collapse=' & '), ' has only one relocation'))}
}

# fix 0's to NAs in latitude and longitude
# xyz <- data[,coords]
# xyz[1:10,1:2] <- 0
# xyz
# returns data.frame with 0s replaced with NAs
#' @export
fix_zero <- function(xyz){
  zero_row <- apply(!is.na(xyz[,1:2]) & xyz[,1:2]==0, 1,any)
  xyz[zero_row,] <- NA
  return(xyz)
}
