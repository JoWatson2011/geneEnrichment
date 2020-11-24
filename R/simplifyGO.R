#' REmove semantically similar GOBP terms
#'
#' @param GORes Output of calculateEnrichment of enrichr::enrichr functions
#' @param simThresh Maximum allowed semantic similarity.
#' @param adjpcol Column in GORes with enrichment p-value
#'
#' @importFrom GOSemSim mgoSim
#' @importFrom tidyr gather
#' @importFrom stats na.omit
#' @return Filtered dataframe containing distinct GOBP terms
#' @export
#'
simplifyGO <-  function (GORes, simThresh = 0.4, adjpcol)
{
  if (nrow(GORes) < 1) {
    NA
  }
  else {
    if(!("GOID" %in% colnames(GORes))){
      GORes <- merge(GORes, simplGO$GOterms, by = "TERM") %>%
        unique()
    }
    sim <- GOSemSim::mgoSim(GORes$GOID, GORes$GOID, semData = simplGO$semData,
                            measure = "Rel", combine = NULL)
    sim[is.na(sim)] <- 0
    go1 <- go2 <- similarity <- NULL
    sim.df <- as.data.frame(sim)
    sim.df$go1 <- row.names(sim.df)
    sim.df <- tidyr::gather(sim.df, go2, similarity, -go1)
    sim.df <- sim.df[!is.na(sim.df$similarity), ]
    sim.df <- sim.df[order(sim.df$similarity, decreasing = T),
    ]
    sim.df <- sim.df[sim.df$go1 != sim.df$go2, ]
    sim.df <- sim.df[sim.df$similarity > simThresh, ]
    sim.df$remove <- apply(sim.df, 1, function(x) {
      if (x[1] %in% simplGO$highFreqTerms) {
        return(x[1])
      }
      if (x[2] %in% simplGO$highFreqTerms) {
        return(x[2])
      }
      else {
        return(NA)
      }
    })
    remove <- na.omit(sim.df$remove)
    sim.df <- sim.df[is.na(sim.df$remove), ]
    if (nrow(sim.df) == 0) {
      GORes.filt <- data.frame()
    }
    else {
      sim.df$go1.pval <- GORes[,adjpcol][match(sim.df$go1,
                                               GORes$GOID)]
      sim.df$go2.pval <- GORes[,adjpcol][match(sim.df$go2,
                                               GORes$GOID)]
      for (i in 1:nrow(sim.df)) {
        if (sim.df[i, "go1"] %in% remove) {
          next
        }
        if (sim.df[i, "go2"] %in% remove) {
          next
        }
        go1.pval <- sim.df[i, "go1.pval"]
        go2.pval <- sim.df[i, "go2.pval"]
        if (go1.pval == go2.pval) {
          go1 <- sim.df[i, "go1"]
          go2 <- sim.df[i, "go2"]
          if (go2 %in% simplGO$childTerms[[go1]]) {
            remove <- c(remove, go2)
            next
          }
          else if (go1 %in% simplGO$childTerms[[go2]])
            remove <- c(remove, go1)
          next
        }
        remove <- c(remove, sim.df[i, which.max(c(go1.pval,
                                                  go2.pval))])
      }
      GORes.filt <- GORes[!GORes$GOID %in% remove, ]
    }
    return(GORes.filt)
  }
}
