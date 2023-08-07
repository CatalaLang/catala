#' @import methods
#' @import gmp
#' @import lubridate

################ Integers #################

#' @export
setClass(
  "catala_integer",
  representation(v = "bigz")
)
#' @export
setMethod("Arith", "catala_integer", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_integer", v = v)
})
#' @export
setMethod("-", c("catala_integer", "missing"), function(e1) {
  new("catala_integer", v = -e1@v)
})
#' @export
setMethod("Compare", "catala_integer", function(e1, e2) {
  callGeneric(e1@v, e2@v)
})

################ Decimals #################

#' @export
setClass(
  "catala_decimal",
  representation(v = "bigq")
)
#' @export
setMethod("Arith", "catala_decimal", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_decimal", v = v)
})
#' @export
setMethod("-", c("catala_decimal", "missing"), function(e1) {
  new("catala_decimal", v = -e1@v)
})
#' @export
setMethod("Compare", "catala_decimal", function(e1, e2) {
  callGeneric(e1@v, e2@v)
})

################ Money #################

#' @export
setClass(
  "catala_money",
  representation(v = "bigz")
)
#' @export
setMethod("+", c("catala_money", "catala_money"), function(e1, e2) {
  new("catala_money", v = e1@v + e2@v)
})
#' @export
setMethod("-", c("catala_money", "catala_money"), function(e1, e2) {
  new("catala_money", v = e1@v - e2@v)
})
#' @export
setMethod("-", c("catala_money", "missing"), function(e1) {
  new("catala_money", v = -e1@v)
})
#' @export
setMethod("*", c("catala_money", "catala_decimal"), function(e1, e2) {
  new("catala_money", v = as.bigz(as.bigq(e1@v) * e2@v))
})
#' @export
setMethod("/", c("catala_money", "catala_money"), function(e1, e2) {
  new("catala_decimal", v = as.bigq(e1@v / e2@v))
})
#' @export
setMethod("Compare", "catala_money", function(e1, e2) {
  callGeneric(e1@v, e2@v)
})

################ Duration #################

#' @export
setClass(
  "catala_duration",
  representation(v = "Period")
)
#' @export
setMethod("+", c("catala_duration", "catala_duration"), function(e1, e2) {
  new("catala_duration", v = e1@v + e2@v)
})
#' @export
setMethod("-", c("catala_duration", "catala_duration"), function(e1, e2) {
  new("catala_duration", v = e1@v - e2@v)
})
#' @export
setMethod("-", c("catala_duration", "missing"), function(e1) {
  new("catala_duration", v = -e1@v)
})
#' @export
setMethod("/", c("catala_duration", "catala_duration"), function(e1, e2) {
  new("catala_duration", v = e1@v / e2@v)
})
#' @export
setMethod("Compare", "catala_duration", function(e1, e2) {
  callGeneric(e1@v, e2@v)
})


# TODO: port the dates_calc library to R to make date computations
# more robust.

################ Dates #################

#' @export
setClass(
  "catala_date",
  representation(v = "Date")
)
#' @export
setMethod("+", c("catala_date", "catala_duration"), function(e1, e2) {
  new("catala_date", v = e1@v + e2@v)
})
#' @export
setMethod("-", c("catala_date", "catala_date"), function(e1, e2) {
  new("catala_date", v = e1@v - e2@v)
})
#' @export
setMethod("Compare", "catala_date", function(e1, e2) {
  callGeneric(e1@v, e2@v)
})

################ Unit #################

#' @export
setClass("catala_unit", representation(v = "numeric"))

################ Constructors and conversions #################

# Money

#' @export
catala_money_from_units <- function(x) {
  new("catala_money", v = as.bigz(x) * as.bigz(100))
}
#' @export
catala_money_from_cents <- function(x) {
  new("catala_money", v = as.bigz(x))
}
#' @export
catala_money_from_decimal <- function(d) {
  num_cents_q <- abs(d@v * as.bigq(100))
  unit_part_num_cents_z <- as.bigz(num_cents_q)
  remainder_q <- num_cents_q - as.bigq(unit_part_num_cents_z)
  if (remainder_q < as.bigq(0.5)) {
    new("catala_money", v = as.bigz(sign(d@v)) * unit_part_num_cents_z)
  } else {
    new("catala_money", v = as.bigz(sign(d@v)) * (unit_part_num_cents_z + as.bigz(1)))
  }
}
#' @export
catala_money_to_numeric <- function(m) {
  as.numeric(as.bigq(m@v) / as.bigq(100))
}
#' @export
catala_money_to_string <- function(m) {
  paste0("$", catala_money_to_numeric(m))
}
#' @export
catala_money_round <- function(m) {
  q <- abs(m@v) %/% as.bigz(100)
  r <- abs(m@v) %% as.bigz(100)
  if (abs(r) < 50) {
    new("catala_money", v = sign(m@v) * q * as.bigz(100))
  } else {
    new("catala_money", v = sign(m@v) * (q + 1) * as.bigz(100))
  }
}


# Decimals

#' @export
catala_decimal_from_numeric <- function(x) {
  new("catala_decimal", v = as.bigq(x))
}
#' @export
catala_decimal_from_fraction <- function(x, y) {
  new("catala_decimal", v = as.bigq(n = x, d = y))
}
#' @export
catala_decimal_from_integer <- function(x) {
  new("catala_decimal", v = as.bigq(x@v))
}
#' @export
catala_decimal_to_numeric <- function(x) {
  as.numeric(x@v)
}
#' @export
catala_decimal_round <- function(d) {
  q <- abs(as.bigq(as.bigz(d@v)))
  r <- abs(d@v) - as.bigq(q)
  if (r < as.bigq(0.5)) {
    new("catala_decimal", v = sign(d@v) * q)
  } else {
    new("catala_decimal", v = sign(d@v) * (q + as.bigq(1)))
  }
}
#' @export
catala_decimal_from_money <- function(m) {
  new("catala_decimal", v = as.bigq(as.bigq(m@v) / as.bigq(100)))
}

# Integers

#' @export
catala_integer_from_numeric <- function(x) {
  new("catala_integer", v = as.bigz(x))
}
#' @export
catala_integer_from_string <- function(x) {
  new("catala_integer", v = as.bigz(x))
}
#' @export
catala_integer_to_numeric <- function(x) {
  as.numeric(x@v)
}

# Dates

#' @export
catala_date_from_ymd <- function(y, m, d) {
  new("catala_date", v = make_date(year = y, month = m, day = d))
}
#' @export
catala_day_of_month_of_date <- function(d) {
  mday(d@v)
}
#' @export
catala_month_number_of_date <- function(d) {
  month(d@v)
}
#' @export
catala_year_of_date <- function(d) {
  year(d@v)
}
#' @export
catala_date_to_string <- function(d) {
  paste0(d@v)
}
#' @export
catala_date_first_day_of_month <- function(d) {
  new("catala_date", v = make_date(year = year(d@v), month = month(d@v), day = 1))
}
#' @export
catala_date_last_day_of_month <- function(d) {
  new("catala_date", v = make_date(
    year = year(d@v),
    month = month(d@v),
    day = days_in_month(d@v)
  ))
}

# Durations

#' @export
catala_duration_from_ymd <- function(y, m, d) {
  new("catala_duration", v = years(y) + months(m) + days(d))
}
#' @export
catala_duration_to_ymd <- function(d) {
  c(d@v@year, d@v@month, d@v@day)
}

# List

#' @export
catala_list_fold_left <- function(f, init, l) {
  Reduce(f, l, init)
}
#' @export
catala_list_filter <- function(f, l) {
  Filter(f, l)
}
#' @export
catala_list_map <- function(f, l) {
  Map(f, l)
}
#' @export
catala_list_reduce <- function(f, default, l) {
  if (length(l) == 0) {
    default
  } else {
    Reduce(f, l[-(1:1)], l[[1]])
  }
}
#' @export
catala_list_length <- function(l) {
  catala_integer_from_numeric(length(l))
}

################ Exceptions #################

#' @export
setClass(
  "catala_position",
  representation(
    filename = "character",
    start_line = "numeric",
    end_line = "numeric",
    start_column = "numeric",
    end_column = "numeric",
    law_headings = "character"
  )
)

#' @export
catala_position_to_string <- function(pos) {
  headings <- paste(pos@law_headings, collapse = ", ")
  paste0(
    pos@filename, ":",
    pos@start_line, ".",
    pos@start_column, "-",
    pos@end_line, ".",
    pos@end_column, " (",
    headings, ")"
  )
}

# Source: http://adv-r.had.co.nz/beyond-exception-handling.html
# We redefine conditions to add our own conditions

#' @export
catala_empty_error <- function() {
  structure(
    class = c("catala_empty_error", "error", "condition"),
    list(message = "EmptyError", call = sys.call(-1))
  )
}
#' @export
catala_conflict_error <- function(pos) {
  structure(
    class = c("catala_conflict_error", "error", "condition"),
    list(message = catala_position_to_string(pos), call = sys.call(-1))
  )
}
#' @export
catala_no_value_provided_error <- function(pos) {
  structure(
    class = c("catala_no_value_provided_error", "error", "condition"),
    list(message = catala_position_to_string(pos), call = sys.call(-1))
  )
}
#' @export
catala_assertion_failure <- function(pos) {
  structure(
    class = c("catala_assertion_failure", "error", "condition"),
    list(message = catala_position_to_string(pos), call = sys.call(-1))
  )
}

################ Defaults #################

#' @export
catala_handle_default <- function(pos, exceptions, just, cons) {
  acc <- Reduce(function(acc, exception) {
    new_val <- tryCatch(
      exception(new("catala_unit", v = 0)),
      catala_empty_error = function(e) {
        NULL
      }
    )
    if (is.null(acc)) {
      new_val
    } else {
      if (is.null(new_val)) {
        acc
      } else {
        stop(catala_conflict_error(pos))
      }
    }
  }, exceptions, NULL)
  if (is.null(acc)) {
    if (just(new("catala_unit", v = 0))) {
      cons(new("catala_unit", v = 0))
    } else {
      stop(catala_empty_error())
    }
  } else {
    acc
  }
}

# This value is used for the R code generation to trump R and forcing
# it to accept dead code. Indeed, when raising an exception during a variable
# definition, R could complains that the later dead code will not know what
# this variable was. So we give this variable a dead value.

#' @export
dead_value <- 0
