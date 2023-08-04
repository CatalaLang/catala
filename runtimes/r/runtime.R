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

as.catala_integer <- function(x) {
  catala_integer(v = as.bigz(x))
}

################ Decimals #################

catala_decimal <- setClass(
  "catala_decimal",
  representation(v = "bigq"),
)
setMethod("Ops", "catala_decimal", function(e1, e2) {
  v <- callGeneric(e1@v, e2@v)
  new("catala_decimal", v = v)
})
as.catala_decimal <- function(x) {
  catala_decimal(v = as.bigq(x))
}




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
as.catala_money_units <- function(x) {
  catala_money(v = as.bigz(x) * as.bigz(100))
}
as.catala_money_cents <- function(x) {
  catala_money(v = as.bigz(x))
}

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
as.catala_duration_ymd <- function(y, m, d) {
  catala_duration(v = years(y) + months(m) + days(d))
}

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
as.catala_date_ymd <- function(y, m, d) {
  catala_date(v = make_date(year = y, month = m, day = d))
}

################ Lists #################

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

################ Defaults #################

catala_position <- setClass(
  "catala_position",
  representation(
    filename = "character",
    start_line = "numeric",
    end_line = "numeric",
    start_col = "numeric",
    end_col = "numeric"
  )
)

catala_position_to_string <- function(pos) {
  paste0(
    pos@filename, ":",
    pos@start_line, ".",
    pos@start_col, "-",
    pos@end_line, ".",
    pos@end_col
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

catala_unit <- setClass("catala_unit")

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
