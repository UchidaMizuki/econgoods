#' @importFrom tidygraph as_tbl_graph
#' @export
as_tbl_graph.econgoods <- function(x, ...) {
  x$nodes <- x$nodes |>
    dplyr::rowwise() |>
    dplyr::mutate(utility_type = ifelse(is.null(.data$utility),
                                        NA_character_,
                                        pillar::type_sum(.data$utility)),
                  utility_substitution = .data$utility$substitution %||% NA_real_,
                  utility_efficiency = .data$utility$efficiency %||% NA_real_) |>
    dplyr::ungroup()
  out <- x |>
    dplyr::mutate(utility_weight = NA_real_) |>
    timbr::traverse(\(x, y) {
      x |>
        dplyr::mutate(utility_weight = unname(y$utility[[1L]]$weights))
    },
    .climb = TRUE) |>
    dplyr::select(!"utility") |>
    remove_goods_class() |>
    as_tbl_graph()

  utility_weight <- out |>
    tidygraph::activate("nodes") |>
    tibble::as_tibble() |>
    tibble::rowid_to_column("to") |>
    dplyr::select("to", "utility_weight")
  out |>
    tidygraph::activate("edges") |>
    dplyr::left_join(utility_weight,
                     by = dplyr::join_by("to")) |>
    tidygraph::activate("nodes") |>
    dplyr::select(!"utility_weight")
}

#' @importFrom ggraph autograph
#' @export
autograph.econgoods <- function(graph,
                                edge_colour = "gray",
                                label_size = 3, ...) {
  as_tbl_graph(graph) |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(label_goods = paste0(.data$.node_value, ": ", big_mark(.data$price), " x ", big_mark(.data$quantity)) ,
                  label_utility = dplyr::case_when(is.na(.data$utility_type) ~ "",
                                                   is.na(.data$utility_substitution) ~ paste0(.data$utility_type, ": ", big_mark(.data$utility_efficiency)),
                                                   TRUE ~ paste0(.data$utility_type, "(", big_mark(.data$utility_substitution), "): ", big_mark(.data$utility_efficiency)))) |>
    # FIXME: Use `layout = "auto"` because the behavior changes when the `layout = "tree"`.
    purrr::quietly(ggraph::ggraph)(layout = "auto") |>
    purrr::chuck("result") +
    ggraph::geom_edge_diagonal(ggplot2::aes(label = big_mark(.data$utility_weight)),
                               edge_colour = edge_colour,
                               angle_calc = "along",
                               vjust = -0.25,
                               label_size = label_size,
                               force_flip = FALSE) +
    ggraph::geom_node_label(ggplot2::aes(label = dplyr::if_else(.data$label_utility == "",
                                                                label_goods,
                                                                paste0(label_goods, "\n", .data$label_utility))),
                            size = label_size,
                            hjust = "inward") +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_flip()
}
