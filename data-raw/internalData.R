## code to prepare `DATASET` dataset goes here

#simplGO
library(GO.db)
library(org.Hs.eg.db)
library(GOSemSim)
library(dplyr)
gene2GOi<-AnnotationDbi::select(org.Hs.eg.db,keys(org.Hs.egGO2EG),c("ENTREZID", "SYMBOL"),  "GOALL") %>% filter(ONTOLOGYALL == "BP")
gene2GO<-unstack(gene2GOi[,c(1,5)])

GOterms <- AnnotationDbi::select(GO.db, keys=gene2GOi$GOALL, columns=c("TERM"), keytype = "GOID")

GO2Gene<-unstack(stack(gene2GO)[2:1])

freq <- sapply(GO2Gene,length)
freqCutOff <- length(gene2GO)*0.05
highFreqTerms <- names(freq[freq>freqCutOff])

semData <- godata("org.Hs.eg.db","SYMBOL","BP")

childTerms <- as.list(GOBPCHILDREN)

simplGO <- list(gene2GO = gene2GO,
                GOterms=GOterms,
                GO2Gene=GO2Gene,
                highFreqTerms=highFreqTerms,
                semData = semData,
                childTerms=childTerms)
#reactHier_Signl

reactHier <- readr::read_tsv("data-raw/ReactomePathwaysRelation.txt", col_names = c("from", "to"))

reactHier_Signl <- filter(reactHier, from %in% c("R-HSA-162582", # Signal Transduction
                                                 "R-HSA-5653656", # Vesicle Mediated transport
                                                 "R-HSA-9609507", # Localisation of Proteins
                                                 "R-HSA-1474244", # Extracellular matrix organisation
                                                 "R-HSA-8953897", # Cellular responses to external stimuli
                                                 "R-HSA-1640170", # Cell Cycle
                                                 "R-HSA-1500931", # Cell-Cell communication
                                                 "R-HSA-5357801") # Programmed Cell Death
) %>%
  mutate(new = T, iter = 1)

lowerLevels <- T
iter <- 1
while(lowerLevels == T){
  new <- filter(reactHier, from %in% reactHier_Signl[reactHier_Signl$new == T,]$to) %>%
    mutate(new = T, iter = iter + 1)

  if(nrow(new) > 0){
    reactHier_Signl$new = F

    reactHier_Signl <- rbind(reactHier_Signl, new)
  }else{
    lowerLevels <-  F
  }
}

usethis::use_data(simplGO, reactHier_Signl, internal = T, overwrite = T)
