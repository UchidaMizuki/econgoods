#' @export
goods_by <- function(.data, ...) {
  .data |>
    timbr::forest_by(...) |>
    dplyr::select(price, amount)
}

#' @export
goods_compose <- function(data, utility,
                          node = NULL) {
  data |>
    dplyr::summarise(prices = list(price |>
                                     set_names(timbr::node_value())),
                     amounts = list(amount |>
                                      set_names(timbr::node_value())),
                     utility = list(.env$utility),
                     .node = node) |>
    dplyr::rowwise() |>
    dplyr::mutate(utility = list(util_calibrate(utility, prices, amounts)),
                  amount = utility(amounts),
                  price = sum(prices * amounts) / amount) |>
    dplyr::ungroup() |>
    dplyr::select(!c(prices, amounts))
}

#' @export
goods_reprice <- function(.data, ...) {
  prices <- list2(...)

  for (price in prices) {
    .data <- .data |>
      dplyr::rows_update(price,
                         by = setdiff(names(price), "price"))
  }

  .data |>
    purrr::modify(function(x, y) {
      prices <- y$price
      amounts <- util_demand(x$utility[[1L]], prices,
                             utility = 1)
      x$price <- sum(prices * amounts)
      x
    })
}

#' @export
goods_produce <- function(.data, ...) {
  amounts <- list2(...)

  for (amount in amounts) {
    .data <- .data |>
      dplyr::rows_update(amount,
                         by = setdiff(names(amount), "amount"))
  }

  .data |>
    purrr::modify(function(x, y) {
      amounts <- util_demand(y$utility[[1L]], x$price,
                             utility = 1)
      x$amount <- y$amount * amounts
      x
    },
    .climb = TRUE)
}

#' @export
goods_consume <- function(.data, ...) {
  incomes <- list2(...)
  .data <- .data |>
    dplyr::mutate(income = NA_real_)

  for (income in incomes) {
    .data <- .data |>
      dplyr::rows_update(income,
                         by = setdiff(names(income), "income"))
  }

  .data |>
    dplyr::mutate(amount = dplyr::if_else(is.na(income),
                                          amount,
                                          income / price)) |>
    dplyr::select(!income) |>
    purrr::modify(function(x, y) {
      amounts <- util_demand(y$utility[[1L]], x$price,
                             utility = 1)
      x$amount <- y$amount * amounts
      x
    },
    .climb = TRUE)
}

#' @export
goods_reprice_recursive <- function(data, f,
                                    tolerance = 1e-12) {
  price <- function(data) {
    out <- data |>
      tibble::as_tibble()
    out$price
  }

  price_old <- price(data)

  repeat {
    data <- data |>
      goods_reprice(f(data))

    price_new <- price(data)

    error <- sum(abs(price_new - price_old))

    if (error < tolerance) {
      break
    } else {
      price_old <- price_new
    }
  }

  data
}

#' @export
goods_produce_recursive <- function(data, f,
                                    tolerance = 1e-12) {
  amount <- function(data) {
    out <- data |>
      tibble::as_tibble()
    out$amount
  }

  amount_old <- amount(data)

  repeat {
    data <- data |>
      goods_produce(f(data))

    amount_new <- amount(data)

    error <- sum(abs(amount_new - amount_old))

    if (error < tolerance) {
      break
    } else {
      amount_old <- amount_new
    }
  }

  data
}

#' @export
goods_consume_recursive <- function(data, f,
                                    tolerance = 1e-12) {
  income <- function(data) {
    out <- data |>
      tibble::as_tibble()
    out$price * out$amount
  }

  income_old <- income(data)

  repeat {
    data <- data |>
      goods_consume(f(data))

    income_new <- income(data)

    error <- sum(abs(income_new - income_old))

    if (error < tolerance) {
      break
    } else {
      income_old <- income_new
    }
  }

  data
}
