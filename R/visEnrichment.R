#' Title
#'
#' @param enriched Output dataframe of calculateEnrichment() (if visualisation = F)
#' @param col Fill colour
#'
#' @importFrom ggplot2 ggplot aes geom_col theme_bw coord_flip ylab theme geom_hline
#' @importFrom dplyr arrange desc row_number
#' @importFrom stats reorder
#' @importFrom rlang .data

#' @importFrom magrittr `%>%`
#' @return ggplot object
#' @export
visEnrichment <- function(enriched, col = "#d2d2d2"){
  gg <- enriched %>%
    dplyr::arrange(desc(.data$Adjusted.P.value)) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    ggplot2::ggplot(ggplot2::aes(x= stats::reorder(.data$Term, .data$order),
                                 y = -log(.data$Adjusted.P.value))) +
    ggplot2::geom_col(fill = col) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() +
    ggplot2::ylab("-log(FDR)") +
    ggplot2::theme(plot.title.position = "plot") +
    ggplot2::geom_hline(yintercept = 2.995732, color = "red", linetype = "dotted")

  return(gg)
}
