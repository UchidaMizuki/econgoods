add_goods_class <- function(x) {
  structure(x,
            class = c("econgoods", class(x)))
}

remove_goods_class <- function(x) {
  structure(x,
            class = setdiff(class(x), "econgoods"))
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}
