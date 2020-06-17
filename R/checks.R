# Burst related checks
#' @title Checks if burst is ordered by time and then outputs the correct order
#' @export
#' @param burst a multi_burst
#' @param time_data a vector of time
#' @param return the new order or not?
check_ordered <- function(burst, time_data, return = TRUE) {
  idz <- burst_labels(burst, factor = TRUE)

  # may not be as fast as something involving order(time_data, idz)
  isOrdered <-
    all(tapply(time_data, idz, function(x)
      identical(order(x), seq_len(length(
        x
      )))))
  if (isOrdered & return) {
    return(seq_along(time_data))
  }
  if (!isOrdered) {
    message(paste0('time was not ordered for each burst, reordering output...'))
    if (return) {
      return(order(idz, time_data))
    }
  }
}

#' @title Check if a set of column names are found in a data frame and return an error if not
#' @export
#' @param data a data.frame to check names against
#' @param names the inputted column names
check_names_exist <- function(data, names) {
  # check burst
  if(!is.character(names)){stop('Column names must be a character')}
  col_names <- colnames(data)
  test <- !(names %in% colnames(data))
  if (any(test)) {
    stop(paste0(
      paste0(names[test], collapse = ' & '),
      ' not found in data frame',
      collapse = ' '
    ))
  }

}

##### Burst related checks
#' @title Check there are no NAs in burst
#' @export
#' @param burst a multi_burst
check_NA_burst <- function(burst) {
  if(inherits(burst,c('sftrack','sftraj'))){burst <- burst$burst}
  if (any(is.na(unlist(burst)))) {
    stop('NAs not allowed in burst')
  }
}

#' @title Check there is aburst id present
#' @export
#' @param burst a multi_burst
check_burst_id <- function(burst){
  if(!('id'%in%burst| 'id'%in%names(burst))){stop('There is no `id` column in burst names')}

}

# more than one relocation for a burst
check_two_bursts <- function(burst,..., active_burst) {

  if(inherits(burst,'multi_burst')){
    lvlz <- burst_labels(burst,factor=T)
    active_burst <- attr(burst, 'active_burst')
  } else {
  lvlz <- vapply(burst, function(y) {paste0(y[active_burst],collapse='_')}, NA_character_)
  }
  count <- table(lvlz)
  if (any(count == 1)) {
    warning(paste0(paste0(names(count)[count == 1], collapse = ' & '), ' has only one relocation'))
  }
}
#' @title Are burst names equivalent for each ind_burst?
#' @export
#' @param burst a multi_burst
check_burst_names <- function(burst) {
  if(inherits(burst,c('sftrack','sftraj'))){burst <- burst$burst}
  if (length(unique(vapply(burst, function(y)
    paste(names(y), collapse = ''), NA_character_))) != 1) {
    stop('Burst names do not match')
  }

  if (any(unlist(lapply(burst, function(x)
    duplicated(names(
      x
    )))))) {
    stop('burst names can not be duplicated')
  }
}

# How many bursts are there when combining two different multi_bursts
check_active_burst <- function(burst, active_burst = NULL, check_all = T){
  if(is.null(active_burst)){active_burst <- active_burst(burst)}
  if(!check_all){
    check <- all(active_burst %in% names(burst[[1]]))
  } else {
  check <- all(vapply(burst, function(x) all(active_burst %in% names(x)), NA))
  }
  if(!check){stop('not all active bursts found in burst names')}
}


###################
# coordinate related checks

#' @title fix 0's to NAs in latitude and longitude
#' @param xyz a data.frame of xy or xyz coordinates
#' @return returns a data.frame with 0s replaced with NAs
#' @export
fix_zero <- function(xyz) {
  zero_row <- apply(!is.na(xyz[, 1:2]) & xyz[, 1:2] == 0, 1, any)
  xyz[zero_row, ] <- NA
  return(xyz)
}


#' @title Check if coordinates contain NAs in some columns but not others
#' @param xyz a data.frame of xy or xyz coordinates
#' @export
check_NA_coords <- function(xyz) {
  check_row <- lapply(xyz, function(x)
    which(is.na(x)))
  NAs <-
    vapply(check_row, function(x)
      any(!unique(unlist(check_row)) %in% x), NA)
  if (any(NAs)) {
    stop(paste0(
      paste0(c('x', 'y', 'z')[!NAs], collapse = ' '),
      ' column has NAs that are not found in other coordinate columns'
    ))
  }
}

# Checks if z coordinates and returns a message
check_z_coords <- function(x) {
  if ('XYZ' %in% class(st_geometry(x)[[1]])) {
    message(
      'Z coordinates found. Note that the vast majority of rgdal calculations are done using 2D geometry'
    )
  }
}
#check_z_coords(my_track)

################
# Time
#' @title Check if time is integer or posix
#' @param time a vector of time
#' @export
check_time <- function(time) {
  # This function was originally envisioned to contain all time checks
  # Currently its not but can expand if we feel its necessary to have
  # is integer or posixct
  if(inherits(time, c('sftrack','sftraj'))){time <- time[[attr(time, 'time')]]}
  if (!(inherits(time, 'integer') | inherits(time, 'POSIXct'))) {
    stop('Time needs to be an integer or POSIXct')
  }
}

#' @title Check if time is regular for each burst and returns logical for each burst
#' @param sftrack an sftrack/sftraj object
#' @export
check_t_regular <- function(sftrack) {
  # is complete
  time_col = attr(sftrack, 'time')
  idz <- burst_labels(sftrack$burst,factor=T)
  sftrack <-
    sftrack[check_ordered(idz, sftrack[[time_col]]),]
  ans <-
    tapply(sftrack[[time_col]], idz, function(date) {
      x1 <- unclass(date[-1])
      x2 <- unclass(date[-length(date)])
      abs(mean(c(x1 - x2)) - (x1[1] - x2[1])) <= 1e-07
    })
  return(ans)
}

#' @title check that time is unique
#' @param x An sftrack/sftraj object or a multi_burst
#' @param time vector of time, not required if given a sftrack object.
#' @export
dup_timestamp <- function(x, time) {
  if(inherits(x,c('sftrack','sftraj'))){
    burst <- x$burst
    time <- x[[attr(x, 'time')]]
  } else{burst = x}

  test <-
    tapply(time, burst_labels(burst, TRUE), function(y)
      any(duplicated(y)))
  if (any(test)) {
    stop(paste0(
      'bursts: ',
      paste0(names(test)[test], collapse = ', '),
      ' have duplicated time stamps'
    ))
  }
}

unique_active_bursts <-
  function(burst){
    #burst = list(burst1,burst2)
    #burst = list(x[[1]],value[[1]])
    if(length(unique(vapply(burst, function(x) paste(attr(x,'active_burst'),collapse=''),NA_character_)))!=1){
      stop('There are more than one possible active bursts')
    }
  }
