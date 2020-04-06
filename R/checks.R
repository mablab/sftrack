# Burst related checks
#' @title Checks if burst is ordered by time and then outputs the correct order
#' @export
#' @param burst a multi_burst
#' @param time_data a vector of time
#' @param return the new order or not?
check_ordered <- function(burst, time_data, return = TRUE) {
  idz <- factor(paste0(burst))

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
check_NAburst <- function(burst) {
  if (any(is.na(unlist(burst)))) {
    stop('NAs not allowed in burst')
  }
}

# more than one relocation for a burst
check_two_bursts <- function(burst) {
  count <- table(attr(burst, 'sort_index'))
  if (any(count == 1)) {
    warning(paste0(paste0(names(count)[count == 1], collapse = ' & '), ' has only one relocation'))
  }
}
#' @title Are burst names equivalent for each ind_burst?
#' @export
#' @param burst a multi_burst
check_burst_names <- function(burst) {
  if (length(unique(vapply(burst, function(x)
    paste(names(x), collapse = ''), NA_character_))) != 1) {
    stop('Burst names do not match')
  }

  if (any(unlist(lapply(burst, function(x)
    duplicated(names(
      x
    )))))) {
    stop('burst names can not be duplicated')
  }
}

#' @title Checks if sort_index needs to be recalculated then recalculates them
#' @param burst a multi_burst
#' @export
check_sort <- function(burst) {
  bl <- burst_labels(burst)
  eq <- all.equal(as.character(burst_sort(burst)), bl)
  if (!eq) {
    attr(burst, 'sort_index') <- factor(bl)
    burst
  }
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
check_z_coords <- function(sftrack_obj) {
  if ('XYZ' %in% class(sftrack_obj$geometry[[1]])) {
    message(
      'Z coordinates found. Note that the vast majority of rgdal calculations are done using planar geometry'
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
  sftrack <-
    sftrack[check_ordered(burst_select(sftrack$burst), sftrack[, time_col]),]
  ans <-
    tapply(sftrack[, time_col, drop = T], paste(burst_select(sftrack$burst)), function(date) {
      x1 <- unclass(date[-1])
      x2 <- unclass(date[-length(date)])
      abs(mean(c(x1 - x2)) - (x1[1] - x2[1])) <= 1e-07
    })
  return(ans)
}

#' @title check that time is unique
#' @param x An sftrack/sftraj object
#' @export
dup_timestamp <- function(x) {
  test <-
    tapply(x[, attr(x, 'time'), drop = T], burst_labels(x$burst, F), function(y)
      any(duplicated(y)))
  if (any(test)) {
    stop(paste0(
      'bursts: ',
      paste0(names(test)[test], collapse = ', '),
      ' have duplicated time stamps'
    ))
  }
}
