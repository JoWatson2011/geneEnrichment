#' Save Enrichment Visualization
#'
#' @param gg ggplot object
#' @param path image file save path
#' @importFrom ggplot2 ggsave theme element_text
#' @return Image saved to path
#' @export

saveEnrichmentVis <- function(gg, path){
  if(length(unique(gg$data$Term)) < 25){
    ggplot2::ggsave(path, gg)
  } else if(length(unique(gg$data$Term)) < 100){
    ggplot2::ggsave(path, gg + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7)),
           height = 6, width = 8.3, units ="in")
  } else{
    ggplot2::ggsave(path, gg + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7)),
           height = 11.7, width = 8.3, units ="in")
  }
}
