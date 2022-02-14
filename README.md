# Shiny App for RNAseq Data Visualization

Purpose of this git is to present a part of my work, and the power of shiny R tool.
The shiny App code presented here is a parto of the original one. Data used here for presentation are either simulated or randomly chosen.


The Original App was built for Parasol ANR, in Kaliphruit team at B&PMP (new IPSIM), Montpellier, France.
 
I developped this shiny App to allow RNAseq visualization for **non R-users** from a project with an hundred samples RNAseq. 

### Gene Expression pattern ###

This tab allows user to easily plot gene expression pattern. two different types of normalization are plot here : DEseq2 [1]  and TPM 

This tab also give user several annotation informations : 

- arabidopsis ortholgue (best blast hit)
- functionnal annotation + pathway
- gene ontology annotation
- plant ontology of the best arabidopsis blast hit

Just enter a gene ID and click on the button !

**img1** : Visualization of gene expression pattern 

<img src ="/img/ScreenShot_tab1.png", width="500" > 

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


### Alternative splicing analysis tab

This tab isn't functionnal in this exemple as it needs bam files as inputs. 
It represents an implementation of SGSeq R packages [2] in a shiny App : Detection and quantification of alternative splicing.

Screen shots presented here show outputs of this tool on an exemple.

**img1** : detection and quantification of alternative splicing in our samples. 
<img src="/img/Screen1_SGSTab.png" width="500">

**img2** : frequencies of variants detected in our samples + informations concerning reference and new variants detected (size, splicing event type, predicted protein ...) + plot of the variants frequencies

<img src="/img/Screen2_SGSTab.png" width="500">


> 1. Love, M.I.; Huber, W.; Anders, S. Moderated Estimation of Fold Change and Dispersion for RNA-Seq Data with DESeq2. Genome Biol **2014**, 15, 550, doi:10.1186/s13059-014-0550-8.
> 2. Goldstein, L.D.; Cao, Y.; Pau, G.; Lawrence, M.; Wu, T.D.; Seshagiri, S.; Gentleman, R. Prediction and Quantification of Splice Events from RNA-Seq Data. PLoS ONE **2016**, 11, e0156132, doi:10.1371/journal.pone.0156132.






contact : morganmaillard92@gmail.com

