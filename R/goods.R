#' @export
goods_by <- function(.data, ...) {
  .data |>
    timbr::forest_by(...) |>
    dplyr::select("price", "quantity") |>
    add_goods_class()
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.econgoods <- function(x, ...) {
  out <- NextMethod()
  names(out)[[1L]] <- "Goods"
  out
}

#' @export
goods_compose <- function(data, utility,
                          node = NULL) {
  data |>
    dplyr::summarise(prices = list(.data$price |>
                                     set_names(timbr::node_value())),
                     quantities = list(.data$quantity |>
                                         set_names(timbr::node_value())),
                     .node = node) |>
    dplyr::mutate(utility = ifelse(is_utility(.env$utility),
                                   list(.env$utility),
                                   unname(.env$utility[timbr::node_value()]))) |>
    dplyr::rowwise() |>
    dplyr::mutate(utility = list(util_calibrate(.data$utility, .data$prices, .data$quantities)),
                  quantity = .data$utility(.data$quantities),
                  price = sum(.data$prices * .data$quantities) / .data$quantity) |>
    dplyr::ungroup() |>
    dplyr::select(!c("prices", "quantities")) |>
    add_goods_class()
}

#' @export
goods_reprice <- function(data,
                          prices = NULL) {
  if (is.data.frame(prices)) {
    prices <- list(prices)
  }

  for (price in prices) {
    data <- data |>
      dplyr::rows_update(price,
                         by = setdiff(names(price), "price"))
  }

  data |>
    timbr::traverse(function(x, y) {
      prices <- y$price
      quantities <- util_demand(x$utility[[1L]], prices,
                                utility = 1)
      x$price <- sum(prices * quantities)
      x
    })
}

#' @export
goods_produce <- function(data,
                          quantities = NULL) {
  if (is.data.frame(quantities)) {
    quantities <- list(quantities)
  }

  for (quantity in quantities) {
    data <- data |>
      dplyr::rows_update(quantity,
                         by = setdiff(names(quantity), "quantity"))
  }

  data |>
    timbr::traverse(function(x, y) {
      quantities <- util_demand(y$utility[[1L]], x$price,
                                utility = 1)
      x$quantity <- y$quantity * quantities
      x
    },
    .climb = TRUE)
}

#' @export
goods_consume <- function(data,
                          incomes = NULL) {
  if (is.data.frame(incomes)) {
    incomes <- list(incomes)
  }

  data <- data |>
    dplyr::mutate(income = NA_real_)

  for (income in incomes) {
    data <- data |>
      dplyr::rows_update(income,
                         by = setdiff(names(income), "income"))
  }

  data |>
    dplyr::mutate(quantity = dplyr::if_else(is.na(.data$income),
                                            .data$quantity,
                                            .data$income / .data$price)) |>
    dplyr::select(!income) |>
    timbr::traverse(function(x, y) {
      quantities <- util_demand(y$utility[[1L]], x$price,
                                utility = 1)
      x$quantity <- y$quantity * quantities
      x
    },
    .climb = TRUE)
}

#' @export
goods_reprice_recursive <- function(data, f,
                                    tolerance = 1e-12) {
  price <- function(data) {
    out <- tibble::as_tibble(data)
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
  quantity <- function(data) {
    out <- tibble::as_tibble(data)
    out$quantity
  }

  quantity_old <- quantity(data)

  repeat {
    data <- data |>
      goods_produce(f(data))

    quantity_new <- quantity(data)

    error <- sum(abs(quantity_new - quantity_old))

    if (error < tolerance) {
      break
    } else {
      quantity_old <- quantity_new
    }
  }
  data
}

#' @export
goods_consume_recursive <- function(data, f,
                                    tolerance = 1e-12) {
  income <- function(data) {
    out <- tibble::as_tibble(data)
    out$price * out$quantity
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
