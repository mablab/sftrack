#' @title Checks if grouping is ordered by time and then outputs the correct order
#' @export
#' @param group a c_grouping
#' @param time_data a vector of time
#' @param return T/F return the new order or just run check?
check_ordered <- function(group, time_data, return = TRUE) {
  idz <- group_labels(group)

  # may not be as fast as something involving order(time_data, idz)
  isOrdered <-
    all(tapply(time_data, idz, function(x) {
      identical(order(x), seq_len(length(
        x
      )))
    }))
  if (isOrdered & return) {
    return(seq_along(time_data))
  }
  if (!isOrdered) {
    message(paste0("time was not ordered for each burst, reordering output..."))
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
  if (!is.character(names)) {
    stop("Column names must be a character")
  }
  col_names <- colnames(data)
  test <- !(names %in% colnames(data))
  if (any(test)) {
    stop(paste0(
      paste0(names[test], collapse = " & "),
      " not found in data frame",
      collapse = " "
    ))
  }
}

##### Grouping related checks
#' @title Check there are no NAs in burst
#' @export
#' @param x a c_grouping
check_NA_group <- function(x) {
  group <- x
  if (inherits(x, c("sftrack", "sftraj"))) {
    group <- x[[attr(x, "group_col")]]
  }

  if (any(vapply(group, function(x) any(is.na(x)), logical(1)))) {
    stop("NAs not allowed in groups")
  }
}

#' @title Check there is a grouping id present
#' @export
#' @param x a c_grouping
check_group_id <- function(x) {
  if (!("id" %in% x |
    "id" %in% names(x))) {
    stop("There is no `id` column in group names")
  }
}

# more than one relocation for a burst
check_two_groups <- function(x, ..., active_group) {
  group <- x
  if (inherits(group, "c_grouping")) {
    lvlz <- group_labels(group)
    active_group <- attr(group, "active_group")
  } else {
    lvlz <-
      vapply(group, function(y) {
        paste0(y[active_group], collapse = "_")
      }, NA_character_)
  }
  count <- table(lvlz)
  if (any(count == 1)) {
    message(paste0(paste0(names(count)[count == 1], collapse = " & "), " has only one relocation"))
  }
}
#' @title Are group names equivalent for each s_group?
#' @export
#' @param x a c_grouping
check_group_names <- function(x) {
  group <- x
  if (inherits(x, c("sftrack", "sftraj"))) {
    group <- x[[attr(x, "group_col")]]
  }
  if (length(unique(vapply(group, function(y) {
    paste(names(y), collapse = "")
  }, NA_character_))) != 1) {
    stop("Group names do not match")
  }

  if (any(unlist(lapply(group, function(x) {
    duplicated(names(
      x
    ))
  })))) {
    stop("group names can not be duplicated")
  }
}

# How many bursts are there when combining two different c_groupings
check_active_group <-
  function(x,
           active_group = NULL,
           check_all = T) {
    if (is.null(active_group)) {
      active_group <- active_group(x)
    }
    if (!check_all) {
      check <- all(active_group %in% names(x[[1]]))
    } else {
      check <-
        all(vapply(x, function(y) {
          all(active_group %in% names(y))
        }, NA))
    }
    if (!check) {
      stop("not all new active group names found in current group names")
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
  check_row <- lapply(xyz, function(x) {
    which(is.na(x))
  })
  NAs <-
    vapply(check_row, function(x) {
      any(!unique(unlist(check_row)) %in% x)
    }, NA)
  if (any(NAs)) {
    stop(paste0(
      paste0(c("x", "y", "z")[!NAs], collapse = " "),
      " column has NAs that are not found in other coordinate columns"
    ))
  }
}

# Checks if z coordinates and returns a message
check_z_coords <- function(x) {
  if ("XYZ" %in% class(st_geometry(x)[[1]])) {
    message(
      "Z coordinates found. Note that the vast majority of rgdal calculations are done using 2D geometry"
    )
  }
}
# check_z_coords(my_track)

################
# Time
#' @title Check if time is integer or posix
#' @param time a vector of time
#' @export
check_time <- function(time) {
  # This function was originally envisioned to contain all time checks
  # Currently its not but can expand if we feel its necessary to have
  # is integer or posixct
  if (inherits(time, c("sftrack", "sftraj"))) {
    time <- time[[attr(time, "time_col")]]
  }
  if (!(inherits(time, "integer") | inherits(time, "POSIXct") | inherits(time, "numeric"))) {
    stop("Time needs to be an integer or POSIXct")
  }
}

#' @title Check if time is regular for each burst and returns logical for each burst
#' @param x an sftrack/sftraj object
#' @export
check_t_regular <- function(x) {
  # is complete
  time_col <- attr(x, "time_col")
  idz <- group_labels(x[[attr(x, "group_col")]])
  sftrack <-
    x[check_ordered(idz, x[[time_col]]), ]
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
  if (inherits(x, c("sftrack", "sftraj"))) {
    group <- x[[attr(x, "group_col")]]
    time <- x[[attr(x, "time_col")]]
  } else {
    group <- x
  }

  test <-
    tapply(time, group_labels(group), function(y) {
      any(duplicated(y))
    })
  if (any(test)) {
    stop(paste0(
      "groups: ",
      paste0(names(test)[test], collapse = ", "),
      " have duplicated time stamps"
    ))
  }
}

unique_active_group <-
  function(x) {
    # burst = list(burst1,burst2)
    # burst = list(x[[1]],value[[1]])
    if (length(unique(vapply(x, function(y) {
      paste(attr(y, "active_group"), collapse = "")
    }, NA_character_))) != 1) {
      stop("There are more than one possible active groups")
    }
  }
