
# df1 <- data.frame(
#   id = rep(1,10),
#   time = 1:10,
#   weather = c('clear','windy','clear','windy','clear','clear','rainy','clear','cloudy','cloudy'),
#   x = c(0,1,1,1,NA,2,3,1,2,2),
#   y = c(0,0,1,1,NA,0,1,1,2,2)
# )
#
#' @export
sft_time <- function(x = list()) {
  class(x) <- setdiff(class(x), "sft_timestamp")
  # check for null
  if (length(x) == 0) {
    return(structure(list(),
      tzone = NA,
      type = "numeric",
      class = c("sft_timestamp", class(x))
    ))
  }

  type <- if (inherits(x[[1]][[1]], "POSIXct")) {
    "POSIX"
  } else {
    "numeric"
  }
  tz <- if (type == "POSIX") {
    attr(x[[1]][[1]], "tz")
  } else {
    ""
  }
  if (is.null(tz)) tz <- ""
  ret <- structure(
    I(x),
    tzone = tz,
    type = type,
    class = c("sft_timestamp", class(x))
  )
  ret
}


#' @export
as.data.frame.sft_timestamp <- function(x, ...) {
  ret <- data.frame(row.names = seq_along(x))
  ret[["sft_timestamp"]] <- I(x)
  ret
}

#' @export
c.sft_timestamp <- function(..., recursive = FALSE) {
  lst <- list(...)
  ret <- unlist(lapply(lst, unclass), recursive = FALSE)
  sft_time(ret)
}

#' @export
"[.sft_timestamp" <- function(x, i, j, ...) {
  sft_time(NextMethod())
}

#' @export
"$<-.sft_timestamp" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "sft_timestamp")) {
    value <- I(value)
  }
  NextMethod()
}
# df1$timestamp <- timez
# df1$timestamp

#' @export
t1 <- function(x) {
  UseMethod("t1")
}

#' @export
#' @method t1 sftrack
t1.sftrack <- function(x) {
  timez <- x[[attr(x, "time_col")]]
  ret <- vapply(x[[attr(x, "time_col")]], function(y) y[[1]], numeric(1))
  if (inherits(timez[[1]], "POSIXct")) {
    tzone <- attr(timez, "tzone")
    ret <- as.POSIXct(ret, origin = "1970-01-01 00:00.00 UTC", tz = tzone)
  }
  ret
}

#' @export
#' @method t1 sftraj
t1.sftraj <- function(x) {
  timez <- x[[attr(x, "time_col")]]
  ret <- vapply(x[[attr(x, "time_col")]], function(y) y[[1]], numeric(1))
  if (attr(timez, "type") == "POSIX") {
    tzone <- attr(timez, "tzone")
    ret <- as.POSIXct(ret, origin = "1970-01-01 00:00.00 UTC", tz = tzone)
  }
  ret
}

#' @export
#' @method t1 sft_timestamp
t1.sft_timestamp <- function(x) {
  ret <- vapply(x, function(y) y[[1]], numeric(1))
  if (attr(x, "type") == "POSIX") {
    tzone <- attr(x, "tzone")
    ret <- as.POSIXct(ret, origin = "1970-01-01 00:00.00 UTC", tz = tzone)
  }
  ret
}
#' @export
#' @method t1 POSIXct
t1.POSIXct <- function(x) {
  ret <- vapply(x, function(y) y[[1]], numeric(1))
  tzone <- attr(x, "tzone")
  ret <- as.POSIXct(ret, origin = "1970-01-01 00:00.00 UTC", tz = tzone)
  ret
}

#' @export
#' @method t1 numeric
t1.numeric <- function(x) {
  x
}
#' @export
t2 <- function(x) {
  UseMethod("t2")
}

#' @export
#' @method t2 sftrack
t2.sftrack <- function(x) {
  t1 <- t1(x)
  t2 <- t2_by_group(t1, x[[attr(x, "group_col")]])
  if (attr(x[[attr(x, "time_col")]], "type") == "POSIX") {
    tzone <- attr(t1, "tzone")
    ret <- as.POSIXct(t2, origin = "1970-01-01 00:00:00 UTC")
    attr(ret, "tzone") <- tzone
    return(ret)
  }
  t2
}

#' @export
#' @method t2 sftraj
t2.sftraj <- function(x) {
  timez <- x[[attr(x, "time_col")]]
  ret <- vapply(x[[attr(x, "time_col")]], function(y) y[[2]], numeric(1))
  if (attr(timez, "type") == "POSIX") {
    tzone <- attr(timez, "tzone")
    ret <- as.POSIXct(ret, origin = "1970-01-01 00:00.00 UTC", tz = tzone)
  }
  ret
}

#' @export
#' @method t2 sft_timestamp
t2.sft_timestamp <- function(x) {
  if (length(x[[1]]) == 2) {
    ret <- vapply(x, function(y) y[[2]], numeric(1))
    ordert <- seq_along(x)
  } else {
    ordert <- order(x)
    x <- x[ordert]
    ret <- c(unclass(x[-1]), NA)
  }
  tzone <- attr(x, "tzone")
  if (attr(x, "type") == "POSIX") {
    ret <- as.POSIXct(ret, origin = "1970-01-01 00:00:00 UTC")
    attr(ret, "tzone") <- tzone
  }
  ret[order(ordert)]
}

#' @export
#' @method t2 POSIXct
t2.POSIXct <- function(x) {
  ordert <- order(x)
  x <- x[ordert]
  ret <- c(unclass(x[-1]), NA)
  tzone <- attr(x, "tzone")
  ret <- as.POSIXct(ret, origin = "1970-01-01 00:00:00 UTC")
  attr(ret, "tzone") <- tzone
  ret[order(ordert)]
}

#' @export
#' @method t2 numeric
t2.numeric <- function(x) {
  ordert <- order(x)
  x <- x[ordert]
  ret <- c(x[-1], NA)
  ret[order(ordert)]
}

print.sft_timestamp <- function(x, ...) {
  NextMethod(tz = attr(x, "tzone"))
}

#' @export
c.sft_timestamp <- function(..., recursive = FALSE) {
  # x <- list(my_sftraj$timez, my_sftraj2$timez)
  x <- list(...)
  tz_test <- lapply(x[!is.na(x)], function(y) attr(y, "tzone"))
  if (length(unique(tz_test[!is.na(tz_test)])) > 1) {
    stop("times are not in the same timezone \n Please convert them to the same timezone and try again")
  }
  x <- unlist(x, recursive = FALSE)
  sft_time(x)
}

#' @export
format.sft_timestamp <- function(x, ...) {
  if (length(x[[1]]) == 1) {
    NextMethod()
  } else {
    paste0("(", vapply(x, function(y) {
      paste(
        paste0(y[[1]], " --> ", y[[2]]),
        collapse = " --> "
      )
    }, NA_character_), ")")
  }
}

#' @export
"[<-.sft_timestamp" <- function(x, i, value) {
  ret <- sft_time(NextMethod())
  check_time(ret, TRUE)
}

#' @export
"[[<-.sft_timestamp" <- function(x, i, value) {
  ret <- sft_time(NextMethod())
  check_time(ret, TRUE)
}

missing_next_pt <- function(x) {
  #  x <- my_sftraj
  gl <- group_labels(x)
  ret <- which(t1(x)[-1] != t2(x)[-nrow(x)] & gl[-1] == gl[-length(gl)])
  if (length(ret) == 0) {
    ret <- NA
  }
  ret
}

make_timestamp <- function(t1, group) {
  # t1 = data[[time_col]]
  idz <- group_labels(group)
  ordert <- order(idz, t1)
  t2 <- unlist(tapply(t1[ordert], idz[ordert], t2))
  t2 <- t2[order(ordert)]
  if (inherits(t1, "POSIXct")) {
    tzone <- attr(t1, "tzone")
    t2 <- as.POSIXct(t2, origin = "1970-01-01 00:00.00 UTC")
    ret <- mapply(function(x, y) {
      tv <- c(I(x), I(y))
      attr(tv, "tzone") <- tzone
      tv
    }, t1, t2, SIMPLIFY = FALSE)
  } else {
    ret <- mapply(function(x, y) {
      c(I(x), I(y))
    }, t1, t2, SIMPLIFY = FALSE)
  }
  sft_time(ret)
}

time_recalc <- function(x) {
  if (!inherits(x, c("sftraj"))) {
    stop("x must be an sftraj object, recalc not necessary otherwise")
  }
  x[[attr(x, "time_col")]] <- make_timestamp(t1(x), x[[attr(x, "group_col")]])
  x
}

t2_by_group <- function(time, group) {
  idz <- group_labels(group)
  t1 <- t1(time)
  ordert <- order(idz, t1)
  t2 <- unlist(tapply(t1[ordert], idz[ordert], t2), use.names = F)
  t2 <- t2[order(ordert)]
  t2
}
