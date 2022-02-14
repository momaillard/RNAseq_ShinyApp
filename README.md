# Shiny App for RNAseq Data Visualization

The purpose of this git is to present a part of my work and the power of shiny R tool.
The shiny App code presented here is a part of the original one. Data used here for presentation are either simulated or randomly chosen. gene list available for this exemple goes from gene_1 to gene_16


The original App was built for Parasol ANR, in Kaliphruit team at B&PMP (new IPSIM), Montpellier, France.
 
I developped this shiny App to allow RNAseq visualization for **non R-users** from a grapevine project with an hundred samples RNAseq. 

### Gene Expression pattern

This tab allows user to easily plot gene expression pattern. two different types of normalization are plot here : DEseq2 **[1]**  and TPM **[2]**

This tab also give user several annotation informations : 

- Arabidopsis ortholgue (best blast hit)
- functionnal annotation + pathway
- gene ontology annotation
- plant ontology of the best arabidopsis blast hit

Just enter a gene ID and click on the button !

**img1** : Visualization of gene expression patterns
<img src ="/img/ScreenShot_tab1.png" width="750" > 


### TPM normalisation

TPM (transcript per million) is a "within sample" normalization method. Some studies tends to consider that this units shouldn't be use for "between samples" comparaison. **[2:4]**
This Tab allows user to compare different transcripts abundance in a single sample. 

**img2** : exemple of TPM comparaison between 3 genes in chosen sample
<img src ="/img/ScreenShot_tab2.png" width="750" > 

### Heatmap ###

This tab produce heatmap expression of a list of given genes. Heatmap is a commonly used plot for RNAseq as it allows the visualization of cluster of genes with common expression patterns

**img3** : heatmap expression of a given gene list. We can easily distinguish early response genes versus late response genes
<img src ="/img/ScreenShot_tabHMmap.png" width="750" > 

### Alternative splicing analysis tab

This tab isn't functionnal in this exemple as it needs bam files as inputs. 
It represents an implementation of SGSeq R packages **[5]** in a shiny App : Detection and quantification of alternative splicing.

Screen shots presented here show outputs of this tool on an exemple.

**img3** : detection and quantification of alternative splicing in our samples. 
<img src="/img/Screen1_SGSTab.png" width="750">

**img4** : frequencies of variants detected in our samples + informations concerning reference and new variants detected (size, splicing event type, predicted protein ...) + plot of the variants frequencies
<img src="/img/Screen2_SGSTab.png" width="750">


### packages needed 


```
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(pheatmap)
library(tidyr)
library(dplyr)
library(shinyBS)
library(SGSeq) \# not needed for exemple
library(GenomicFeatures) \# not needed for exemple
```

### Bibliography
> 1. Love, M.I.; Huber, W.; Anders, S. Moderated Estimation of Fold Change and Dispersion for RNA-Seq Data with DESeq2. Genome Biol **2014**, 15, 550, doi:10.1186/s13059-014-0550-8.
> 2. Wagner, G. P., Kin, K., & Lynch, V. J. Measurement of mRNA abundance using RNA-seq data: RPKM measure is inconsistent among samples. Theory in biosciences, **2012**, 131(4), 281-285.
> 3. Zhao, Y.; Li, M.-C.; Konaté, M.M.; Chen, L.; Das, B.; Karlovich, C.; Williams, P.M.; Evrard, Y.A.; Doroshow, J.H.; McShane, L.M. TPM, FPKM, or Normalized Counts? A Comparative Study of Quantification Measures for the Analysis of RNA-Seq Data from the NCI Patient-Derived Models Repository. J Transl Med **2021**, 19, 269, doi:10.1186/s12967-021-02936-w.
> 4. Zhao, S.; Ye, Z.; Stanton, R. Misuse of RPKM or TPM Normalization When Comparing across Samples and Sequencing Protocols. RNA **2020**, 26, 903–909, doi:10.1261/rna.074922.120.
> 5. Goldstein, L.D.; Cao, Y.; Pau, G.; Lawrence, M.; Wu, T.D.; Seshagiri, S.; Gentleman, R. Prediction and Quantification of Splice Events from RNA-Seq Data. PLoS ONE **2016**, 11, e0156132, doi:10.1371/journal.pone.0156132.






contact : morganmaillard92@gmail.com

