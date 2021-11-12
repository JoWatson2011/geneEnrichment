#' Calculate Enrichment using EnrichR
#'
#' @param genes vector of HGNC gene identifiers
#' @param enrichr_db Enrichr database to use, as in \url{https://maayanlab.cloud/Enrichr/#stats}
#' @param simplify logical; perform simplifcation calculations/filtering on GOBP and Reactome?
#' @param visualise logical; if T returns a ggplot object
#' @param col fill colour of columns in ggplot
#'
#' @importFrom rlang .data
#' @importFrom enrichR enrichr
#' @importFrom dplyr filter mutate
#' @importFrom tidyr separate
#' @importFrom magrittr `%>%`
#' @importFrom ggplot2 xlab
#' @return Either a dataframe  or ggplot visualisation of enriched terms
#' @export
calculateEnrichment <- function(genes, enrichr_db, simplify = T, visualise = F, col = "#d2d2d2"){

  df <- enrichr(genes, databases = enrichr_db)
  df_flt <- filter(df[[1]], .data$Adjusted.P.value < 0.05)

  if(grepl("GO", enrichr_db)){
    df_flt <-  df_flt %>%
      separate(.data$Term, c("Term", "GOID"), " \\(GO:") %>%
      mutate(GOID = paste0("GO:", gsub(")", "", .data$GOID)))

    if(nrow(df_flt) > 1 &
       simplify == T &
       grepl("GO_Biological_Process", enrichr_db)){
      colnames(df_flt)[colnames(df_flt) == "Term"] <- "TERM"
      df_flt <- simplifyGO(df_flt, adjpcol = "Adjusted.P.value")
      colnames(df_flt)[colnames(df_flt) == "TERM"] <- "Term"
    }
  }

  if(grepl("Reactome", enrichr_db)){
    df_flt <- df_flt %>%
      mutate(ID = paste0("R-HSA", gsub(".* R-HSA", "", .data$Term)),
             Term = gsub(" Homo sapiens R-HSA.*", "", .data$Term))
    if(simplify == T & nrow(df_flt) > 1){
      df_flt <- df_flt %>%
        filter(.data$ID %in% c(reactHier_Signl$from,
                               reactHier_Signl$to))
    }
  }

  if(visualise == T){
    gg <- visEnrichment(df_flt, col = col) + ggplot2::xlab(enrichr_db)

    return(gg)
  }

  return(df_flt)
}
