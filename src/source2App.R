getHeatmapTargetByGL <- function(liste, cutRow){ #non reproductible besoin de mes fichiers exacts
    tmpvector <- strsplit(liste, ";")
    geneSelect <- as.vector(tmpvector[[1]])

    redgreen <- c("blue","black","yellow") 
    paletteLength <-256
    pal <- colorRampPalette(redgreen)(paletteLength)
    VSTselect <- Git.tVSD.center.scaled[,geneSelect]
    myOrder <- rownames(orderedColdataLight)
    VSTselect <- VSTselect [myOrder,]
    orderedColdataLight$Temps <- factor(orderedColdataLight$Temps,
        levels = c("PRE","GrB","ColB","V4","V8","V12","V16","V20","V24","V28","V32","V36","V40","V44","V48","V52","V56","V60"))
    myBreaks <- c(seq(min(VSTselect), 0, length.out = ceiling(paletteLength/2) + 1), 
                  seq(max(VSTselect)/paletteLength, max(VSTselect), length.out = floor(paletteLength/2)))
    output <- pheatmap(t(VSTselect), show_rownames = TRUE,
             show_colnames = FALSE, cluster_cols = FALSE,
             col = pal, GitAnnotation_col = orderedColdataLight, breaks = myBreaks, cutree_rows = cutRow)  
    return(output)
}


CutHeatmapTreeHclust <- function (tree, Kcluster) { # to get cutTree from Hclust
    print("calling the function to Cut tree")
    objectTmp <- as.data.frame(cutree(tree, Kcluster))
    objectFinal <- data.frame(objectTmp[,1], rownames(objectTmp))
    colnames(objectFinal) <- c("ClusterCutrow", "GeneID")
    return(objectFinal)
}


launch_SGS <- function (si, geneID, conditionPickerInput, alpha, min_n_sample){
  subSI <- si[which(si$condition %in% conditionPickerInput),1:6]
  subSI
  txdb <- importTranscripts("data/Vitis_vinifera.12X.49.gff3")
  colCHR <- "TXCHROM"
  txdb <- makeTxDbFromGFF("data/Vitis_vinifera.12X.49.gff3", format = "gff3")
  myGene <- geneID
  CHROMO <- AnnotationDbi::select(txdb, keys = myGene, columns = colCHR, keytype = "GENEID")[1,2]

  print(paste0("gene is on chromosome", CHROMO ))
  txdb <- keepSeqlevels(txdb, CHROMO)
  filter <- list(gene_id = myGene)
  grVIT <- transcripts(txdb, filter = filter)
  print("OK till here2")
  txf_ucsc <- convertToTxFeatures(txdb)
  txf_ucsc <- txf_ucsc[txf_ucsc %over% grVIT]

  sgf_ucsc <- convertToSGFeatures(txf_ucsc)
  print("OK till here3")
  head(sgf_ucsc)
  #Splice graph analysis based on annotated transcripts
  sgfc_ucsc <- analyzeFeatures(subSI, features = txf_ucsc)
  #Splice graph analysis based on de novo prediction
  sgfc_pred <- analyzeFeatures(subSI, which = grVIT, alpha = alpha, min_n_sample = min_n_sample)
  sgfc_pred <- annotate(sgfc_pred, txf_ucsc)
  toReturn <- list(sgfc_ucsc, sgfc_pred, txdb, subSI)
  return(toReturn)
}


getVariantsFrequencies <- function (sgfc_pred, txObject, subSI){

    sgvc_pred <- analyzeVariants(sgfc_pred)
    frequencies <- variantFreq(sgvc_pred)
    seqlevelsStyle(Vvinifera) <- "NCBI"
    print(rowRanges(sgfc_pred))
    vep <- predictVariantEffects(rowRanges(sgvc_pred), txObject, Vvinifera, output = "full")
    print("frequencies and Vep are done ! ")
    eventList <- eventID(sgvc_pred)
    variantIDToPaste <- variantID(sgvc_pred)
    sgv <- rowRanges(sgvc_pred)
    sgvc <- getSGVariantCounts(sgv, sample_info = subSI)
    variantTypes <- as.character(as.vector(variantType(sgvc)))
    frequencies_annotToprint <- data.frame(eventList, variantIDToPaste, variantTypes, frequencies)
    colnames(frequencies_annotToprint) <- c("eventID", "variantID", "EventType", colnames(frequencies))
    frequencies_annot <- frequencies_annotToprint[,-3]
    
  

    listToReturn <- list(frequencies_annot, vep, sgvc_pred, frequencies_annotToprint)
    return(listToReturn)

}

plotTheFrequencies <- function(frequenciesTable, EventToPlot) {
    myTable <- frequenciesTable[which(frequenciesTable$eventID == EventToPlot),-1] #choose only the event part and remove the line
    mytTable <- as.data.frame(t(myTable))
    colnames(mytTable) <- paste0("variant_",mytTable[1,]) # putVariantNames into colnames
    mytTable <- mytTable[-1,]
    mytTable$sample <- rownames(mytTable)
    myFinalTable <- mytTable %>% separate(sample, c("stress", "temps", "rep"), sep = "_") %>% tidyr::gather("variantName","frequencies", 1:2)
    myFinalTable$stress <- as.factor(myFinalTable$stress)
    myFinalTable$temps <- as.factor(myFinalTable$temps)
    myFinalTable$temps <- factor(myFinalTable$temps, level= c("PRE", "GrB", "ColB", "V4", "V8", "V12", "V16", "V20", "V24", "V28", "V32", "V36", "V40", "V44", "V48", "V52", "V56", "V60"))
    myFinalTable$rep <- as.factor(myFinalTable$rep)
    myFinalTable$variantName <- as.factor(myFinalTable$variantName)
   
    myPlot <- ggplot(data = myFinalTable, aes(x = temps, y = frequencies, fill = variantName))+
                geom_boxplot()+
                facet_grid(.~stress)+
                ggtitle("Plots of frequencies of each variant")
    return(myPlot)
}

