# Shiny App for RNAseq Data Visualization

App built for Parasol ANR, in Kaliphruit team at B&PMP (new IPSIM), Montpellier, France 
I developped this shiny App to allow RNAseq visualization for non R-users of an hundred samples RNAseq project. 
Data used in git exemple are simulated.

### packages needed 


```
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(pheatmap)
library(shinycssloaders)
library(tidyr)
library(dplyr)
library(SGSeq) \# not needed for exemple
library(GenomicFeatures) \# not needed for exemple
```


### Alternative splicing analysis tab

This subItem in this exemple isn't functionnal as he need bam files as inputs. 
It represents an implementation of SGSeq packages in a shiny App : Detection and quantification of alternative splicing.


<img src="/img/Screen1_SGSTab.png" width="500">
<img src="/img/Screen2_SGSTab.png" width="500">



> Goldstein *et al*. Prediction and Quantification of Splice Events from RNA-Seq Data. PLoS ONE 2016, 11, e0156132, doi:10.1371/journal.pone.0156132.
contact : morganmaillard92@gmail.com

