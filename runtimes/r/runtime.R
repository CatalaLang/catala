library(methods)
suppressMessages(library(gmp))
suppressMessages(library(lubridate))

################ Integers #################
catala_integer <- setClass(
  "catala_integer",
  representation(v = "bigz"),
)
setMethod("Ops", "catala_integer", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_integer", v = v)
})

################ Decimals #################

catala_decimal <- setClass(
  "catala_decimal",
  representation(v = "bigq"),
)
setMethod("Ops", "catala_decimal", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_decimal", v = v)
})

################ Money #################
catala_money <- setClass(
  "catala_money",
  representation(v = "bigz"),
)
setMethod("+", c("catala_money", "catala_money"), function(e1, e2) {
  catala_money(v = e1@v + e2@v)
})
setMethod("-", c("catala_money", "catala_money"), function(e1, e2) {
  catala_money(v = e1@v - e2@v)
})
setMethod("*", c("catala_money", "catala_decimal"), function(e1, e2) {
  catala_money(v = as.bigz(as.bigq(e1@v) * e2@v))
})
setMethod("/", c("catala_money", "catala_money"), function(e1, e2) {
  catala_decimal(v = as.bigq(e1@v / e2@v))
})
setMethod("Compare", "catala_money", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_money", v = v)
})

################ Duration #################
catala_duration <- suppressWarnings(setClass(
  "catala_duration",
  representation(v = "Period")
))
setMethod("+", c("catala_duration", "catala_duration"), function(e1, e2) {
  catala_duration(v = e1@v + e2@v)
})
setMethod("-", c("catala_duration", "catala_duration"), function(e1, e2) {
  catala_duration(v = e1@v - e2@v)
})
setMethod("/", c("catala_duration", "catala_duration"), function(e1, e2) {
  catala_duration(v = e1@v / e2@v)
})
setMethod("Compare", "catala_duration", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_duration", v = v)
})


# TODO: port the dates_calc library to R to make date computations
# more robust.

################ Dates #################
catala_date <- setClass(
  "catala_date",
  representation(v = "Date"),
)
setMethod("+", c("catala_date", "catala_duration"), function(e1, e2) {
  catala_date(v = e1@v + e2@v)
})
setMethod("-", c("catala_date", "catala_date"), function(e1, e2) {
  catala_date(v = e1@v - e2@v)
})
setMethod("Compare", "catala_date", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_date", v = v)
})

################ Unit #################

catala_unit <- setClass("catala_unit")

################ Constructors and conversions #################

# Money

catala_money_from_units <- function(x) {
  catala_money(v = as.bigz(x) * as.bigz(100))
}
catala_money_from_cents <- function(x) {
  catala_money(v = as.bigz(x))
}
catala_money_from_decimal <- function(d) {
  num_cents_q <- abs(d@v * as.bigq(100))
  unit_part_num_cents_z <- as.bigz(num_cents_q)
  remainder_q <- num_cents_q - as.bigq(unit_part_num_cents_z)
  if (remainder_q < as.bigq(0.5)) {
    catala_money(v = as.bigz(sign(d@v)) * unit_part_num_cents_z)
  } else {
    catala_money(v = as.bigz(sign(d@v)) * (unit_part_num_cents_z + as.bigz(1)))
  }
}
catala_money_to_numeric <- function(m) {
  as.numeric(as.bigq(m@v) / as.bigq(100))
}
catala_money_to_string <- function(m) {
  paste0("$", catala_money_to_numeric(m))
}
catala_money_round <- function(m) {
  q <- abs(m@v) %/% as.bigz(100)
  r <- abs(m@v) %% as.bigz(100)
  if (abs(r) < 50) {
    catala_money(v = sign(m@v) * q * as.bigz(100))
  } else {
    catala_money(v = sign(m@v) * (q + 1) * as.bigz(100))
  }
}


# Decimals

catala_decimal_from_numeric <- function(x) {
  catala_decimal(v = as.bigq(x))
}

catala_decimal_from_string <- function(x) {
  catala_decimal(v = as.bigq(x))
}

catala_decimal_from_integer <- function(x) {
  catala_decimal(v = as.bigq(x@v))
}

catala_decimal_to_numeric <- function(x) {
  as.numeric(x@v)
}
catala_decimal_round <- function(d) {
  q <- abs(as.bigq(as.bigz(d@v)))
  r <- abs(d@v) - as.bigq(q)
  if (r < as.bigq(0.5)) {
    catala_decimal(v = sign(d@v) * q)
  } else {
    catala_decimal(v = sign(d@v) * (q + as.bigq(1)))
  }
}
catala_decimal_from_money <- function(m) {
  catala_decimal(v = as.bigq(as.bigq(m@v) / as.bigq(100)))
}

# Integers

catala_integer_from_numeric <- function(x) {
  catala_integer(v = as.bigz(x))
}
catala_integer_from_string <- function(x) {
  catala_integer(v = as.bigz(x))
}
catala_integer_to_numeric <- function(x) {
  as.numeric(x@v)
}

# Dates

catala_date_from_ymd <- function(y, m, d) {
  catala_date(v = make_date(year = y, month = m, day = d))
}

catala_day_of_month_of_date <- function(d) {
  mday(d@v)
}

catala_month_number_of_date <- function(d) {
  month(d@v)
}

catala_year_of_date <- function(d) {
  year(d@v)
}

catala_date_to_string <- function(d) {
  paste0(d@v)
}

catala_date_first_day_of_month <- function(d) {
  catala_date(v = make_date(year = year(d@v), month = month(d@v), day = 1))
}

catala_date_last_day_of_month <- function(d) {
  catala_date(v = make_date(
    year = year(d@v),
    month = month(d@v),
    day = days_in_month(d@v)
  ))
}


# Durations

catala_duration_from_ymd <- function(y, m, d) {
  catala_duration(v = years(y) + months(m) + days(d))
}

catala_duration_to_ymd <- function(d) {
  c(d@v@year, d@v@month, d@v@day)
}

# List

catala_list_fold_left <- function(f, init, l) {
  Reduce(f, l, init)
}

catala_list_filter <- function(f, l) {
  Filter(f, l)
}

catala_list_map <- function(f, l) {
  Map(f, l)
}

catala_list_reduce <- function(f, default, l) {
  if (length(l) == 0) {
    default
  } else {
    Reduce(f, l[-(1:1)], l[[1]])
  }
}

catala_list_length <- function(l) {
  catala_integer_from_numeric(length(l))
}

################ Exceptions #################

catala_position <- setClass(
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

catala_empty_error <- function() {
  structure(
    class = c("catala_empty_error", "error", "condition"),
    list(message = "EmptyError", call = sys.call(-1))
  )
}

catala_conflict_error <- function(pos) {
  structure(
    class = c("catala_conflict_error", "error", "condition"),
    list(message = catala_position_to_string(pos), call = sys.call(-1))
  )
}

catala_no_value_provided_error <- function(pos) {
  structure(
    class = c("catala_no_value_provided_error", "error", "condition"),
    list(message = catala_position_to_string(pos), call = sys.call(-1))
  )
}

catala_assertion_failure <- function(pos) {
  structure(
    class = c("catala_assertion_failure", "error", "condition"),
    list(message = catala_position_to_string(pos), call = sys.call(-1))
  )
}

################ Defaults #################

catala_handle_default <- function(pos, exceptions, just, cons) {
  acc <- Reduce(function(acc, exception) {
    new_val <- tryCatch(
      exception(catala_unit()),
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
    if (just(catala_unit())) {
      cons(catala_unit())
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
dead_value <- 0
