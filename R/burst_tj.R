#' @name grouping-class
#' @title A class to group movement data
#' @description This class describes grouping variables for movement data.
#' The grouping object is composed of a list with named vectors.
#' One of which must be 'id', this is the id of subject being monitored (commonly animal id in movement data)
#' Can be any number of groups after that.
#'
#' @details A grouping is a list of possible categories to group the data. The 'active group'
#' of these is the current grouping variables to be considered for analysis. The 'active group' can be any combination of the categories in a burst,
#' and can change with the use of `active_group()`.
#'
#' An `s_group` is a single row group. It is a 1xn dimensional list with any length(n) > 1.
#' Atleast one of the groups must be named 'id' which is the subjects id.
#'
#' A `c_grouping` is a collection of `s_groups`s, it is a data.frame with dimensions of 1xnrow(data).
#' One c_grouping has one 'active group' which describes the set of names in each s_group to group the data. When you change the 'active group', calculations and plots change accordingly
#' to the new grouping levels.
#'
#' You can create bursts with make_s_group and make_c_grouping.
#' @param x a list containing named grouping variables, one item must be named 'id'. ex: list(id = 1, month = 'may'). For a c_grouping: A list of s_groups or
#' a list of equal length named vectors which will be combined to create a c_grouping. ex: list(x = 1st_vector, y = 2nd_vector)
#' @param active_group a vector of the names of the groups to be considered 'active'.
#' @export make_c_grouping
#' @export make_s_group
#' @examples
#' # Make a single group
#' #'
#' make_s_group(x = list(id = "CJ11", month = 3, height = 10))
#'
#' # Make a c_grouping
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
#' mb1 <- make_c_grouping(x = burstz, active_group = c("id", "month"))
#' str(mb1)
#'
#' # Make a multi_burst from many ind_bursts
#' a <- make_s_group(list(id = 1, year = 2020))
#' b <- make_s_group(list(id = 1, year = 2020))
#' c <- make_s_group(list(id = 2, year = 2020))
#'
#' c(a, b, c)
#' @rdname grouping-class
make_s_group <- function(x) {
  # check duplicated
  if (any(duplicated(names(x)))) {
    stop("group names are duplicated")
  }

  # check id in burst
  check_group_id(x)

  new_group <- lapply(x, as.character)
  s_group(new_group)
}


s_group <- function(x = list(), active_group = NULL) {
  structure(
    x,
    class = c("s_group")
  )
}


c_grouping <- function(x = list(), active_group = NA) {
  if (is.na(active_group[1]) & length(x) > 0) {
    active_group <- names(x[[1]])
  }

  sort_index <- calc_sort_index(x, active_group)
  structure(x,
    active_group = active_group,
    sort_index = sort_index,
    class = c("c_grouping")
  )
}

#' @rdname grouping-class
make_c_grouping <-
  function(x = NULL,
           active_group = NULL) {
    if (inherits(x[[1]], c("s_group", "c_grouping"))) {
      group <- x
    } else {
      if (!inherits(x, "list")) {
        stop("grouping must be a list")
      }

      group <-
        do.call(function(...) {
          mapply(list, ..., SIMPLIFY = FALSE)
        }, x)
    }

    # to stop a problem where vector mode names are created via the first item in the list
    names(group) <- NULL
    if (is.null(active_group)) {
      active_group <- names(group[[1]])
    }
    check_NA_group(group)
    check_group_names(group)
    check_active_group(group, active_group, FALSE)
    check_two_groups(group, active_group = active_group)
    ret <- lapply(
      group,
      function(x, ...) {
        make_s_group(x)
      }
    )
    # ret
    cg <- c_grouping(ret, active_group = active_group)

    return(cg)
  }

#' @rdname grouping-class
#' @method c s_group
#' @export
#' @param ... objects to be pasted together into a c_grouping
c.s_group <- function(...) {
  ret <- list(...)
  # ret = list(ind_burst(list(id=1)), ind_burst(list(id=2)))
  unique_active_group(ret)
  make_c_grouping(ret)
}

#' @rdname grouping-class
#' @export
#' @method c c_grouping
#' @param recursive ignored
c.c_grouping <- function(..., recursive = FALSE) {
  x <- list(...)
  x <- unlist(x, recursive = FALSE)
  unique_active_group(x)
  active_group <- attr(x[[1]], "active_group")
  make_c_grouping(x, active_group = active_group)
}


#' @export
as.data.frame.c_grouping <- function(x, ...) {
  ret <- data.frame(row.names = seq_along(x))
  ret[[attr(x, "active_group")]] <- x
  ret
}

#' @export
print.s_group <- function(x, ...) {
  print(lapply(x, as.character))
}

#' @export
print.c_grouping <- function(x, ...) {
  print(lapply(x, function(x) {
    x
  }))
}

#' @export
#' @importFrom utils str
str.c_grouping <- function(object, ...) {
  n <- length(object)
  cat(paste0(class(object)[1], " object \n"))
  utils::str(unclass(object), list.len = 1)
}


#' @export
format.s_group <- function(x, ...) {
  message(paste0("(", paste0(
    names(x), ": ", as.character(unlist(x)),
    collapse = ", "
  ), ")"))
}

#' @export
format.c_grouping <- function(x, ...) {
  paste0("(", vapply(x, function(y) {
    paste(
      paste0(names(y), ": ", as.character(unlist(y))),
      collapse = ", "
    )
  }, NA_character_), ")")
}

#' @export
"[.c_grouping" <- function(x, i, j, ...) {
  nargs <- nargs()
  if (!missing(i) && is.character(i)) {
    i <- which(attr(x, "sort_index") %in% i)
  }
  active_group <- attr(x, "active_group")
  c_grouping(NextMethod(), active_group = active_group)
}

#' @export
"[<-.c_grouping" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "s_group")) {
    value <- list(value)
  }
  # attributes(x[[1]])
  unique_active_group(list(x[[1]], value[[1]]))
  x <- unclass(x) # becomes a list, but keeps attributes
  ret <- c_grouping(NextMethod())
  check_group_names(ret)
  check_two_groups(ret)
  check_NA_group(ret)
  ret
}

#' @export
"[<-.s_group" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "s_group")) {
    value <- list(value)
  }
  x <- unclass(x) # becomes a list, but keeps attributes

  ret <- make_s_group(NextMethod())
  structure(ret)
}

#' @export
"$<-.s_group" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "s_group")) {
    value <- list(value)
  }
  x <- unclass(x) # becomes a list, but keeps attributes
  ret <- make_s_group(NextMethod())
  structure(ret)
}

#' @export
summary.c_grouping <- function(object, ...) {
  ab <- paste0(attr(object, "active_group"), collapse = ", ")
  object <- group_labels(object)
  levels(object) <- c(levels(object), paste("active_group:", ab))
  NextMethod()
}

#' @title Shows grouping labels created from the s_group and the c_grouping
#' @name group_labels
#' @param x a sftrack or grouping object
#' @export
group_labels <- function(x) {
  UseMethod("group_labels", object = x)
}


#' @export
#' @rdname group_labels
group_labels.sftrack <- function(x) {
  attr(x[[attr(x, "group_col")]], "sort_index")
}

#' @export
#' @rdname group_labels
group_labels.sftraj <- function(x) {
  attr(x[[attr(x, "group_col")]], "sort_index")
}


#' @export
#' @rdname group_labels
group_labels.c_grouping <- function(x) {
  attr(x, "sort_index")
}

#' @title Access the active_group value
#' @rdname active_group
#' @param x a c_grouping
#' @export
#' @description The active group is the combination of group names to group the data sets.
#' The active_group acts essentially like a paste(names_of_groups, sep = '_') grouping variable.
#' @examples
#' #'
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
#' mb1 <- make_c_grouping(x = burstz, active_group = c("id", "month"))
#'
#' # see the current active burst
#' active_group(mb1)
#'
#' # change the active burst
#' active_group(mb1) <- "id"
#'
#' # Using a full data set
#' my_track <- as_sftrack(raccoon,
#'   time = "timestamp",
#'   error = NA, coords = c("longitude", "latitude"),
#'   group = burstz
#' )
#'
#' summary(my_track)
#'
#' # change active group
#' active_group(my_track$sft_group) <- "id"
#'
#' summary(my_track)
active_group <- function(x) {
  if (inherits(x, c("sftrack", "sftraj"))) {
    return(attr(x[[attr(x, "group_col")]], "active_group"))
  }
  # check_burst_names(x)
  attr(x, "active_group")
}

#' @title Set new active group
#' @export
#' @rdname active_group_replace
#' @param x sftrack/sftraj/c_grouping/s_group object
#' @param value character vector of the grouping names to make active
"active_group<-" <- function(x, value) {
  UseMethod("active_group<-", object = x)
}

#' @export
#' @name active_group_replace
"active_group<-.sftrack" <- function(x, value) {
  # x = my_sftrack
  # value = 'id'
  group <- x[[attr(x, "group_col")]]

  if (!all(value %in% names(group[[1]]))) {
    stop("not all new active group found in current grouping names")
  }

  attr(x[[attr(x, "group_col")]], "active_group") <- value
  attr(x[[attr(x, "group_col")]], "sort_index") <- calc_sort_index(x[[attr(x, "group_col")]])
  structure(x)
}

#' @export
#' @name active_group_replace
"active_group<-.sftraj" <- function(x, value) {
  group <- x[[attr(x, "group_col")]]

  if (!all(value %in% names(group[[1]]))) {
    stop("not all new active group found in current grouping names")
  }

  attr(x[[attr(x, "group_col")]], "active_group") <- value
  attr(x[[attr(x, "group_col")]], "sort_index") <- calc_sort_index(x[[attr(x, "group_col")]])
  step_recalc(x)
}

#' @export
#' @name active_group_replace
"active_group<-.c_grouping" <- function(x, value) {
  # x = my_sftrack
  # value = 'id'
  group <- x

  if (!all(value %in% names(group[[1]]))) {
    stop("not all new group names found in grouping burst")
  }

  attr(x, "active_group") <- value
  attr(x, "sort_index") <- calc_sort_index(x)
  structure(x)
}

#' @title Calculate a new sort index for groups
#' @param x group or sftrack object
#' @param active_group (optional), a new active group. If not included, defaults  to the active group (if a c_grouping) or the group names
#' @export
calc_sort_index <- function(x, active_group = NA) {
  if (inherits(x, "c_grouping")) {
    active_group <- attr(x, "active_group")
  }
  if (any(is.na(active_group))) {
    active_group <- names(x[[1]])
  }

  factor(vapply(x, function(y) {
    paste0(y[active_group], collapse = "_")
  }, character(1)))
}

#' @title Display the levels of the sort index
#' @export
#' @rdname group_name
#' @param x sftrack/sftraj/c_grouping/s_group object
group_names <- function(x) {
  UseMethod("group_names", object = x)
}

#' @export
#' @name group_name
group_names.c_grouping <- function(x) {
  levels(attr(x, "sort_index"))
}

#' @export
#' @name group_name
group_names.sftraj <- function(x) {
  levels(attr(x[[attr(x, "group_col")]], "sort_index"))
}

#' @export
#' @name group_name
group_names.sftrack <- function(x) {
  levels(attr(x[[attr(x, "group_col")]], "sort_index"))
}
