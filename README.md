# Shiny App for RNAseq Data Visualization

Purpose of this git is to present a part of my work, and the power of shiny R tool.
The shiny App code presented here are a sample of the original on Data used here a simulated or randomly chosen


the Original App was built for Parasol ANR, in Kaliphruit team at B&PMP (new IPSIM), Montpellier, France 
I developped this shiny App to allow RNAseq visualization for **non R-users** of an hundred samples RNAseq project. 



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

This tab in this exemple isn't functionnal in this exemple as it needs bam files as inputs. 
It represents an implementation of SGSeq packages in a shiny App : Detection and quantification of alternative splicing.

Screen shots presented here present outputs of this tool.

**img1** : detection and quantification of alternative splicing in our samples. 
<img src="/img/Screen1_SGSTab.png" width="500">

**img2** : frequencies of variant detected in all sample + informations concerning the reference and new variants detected (size, splicing event type, predicted protein ...) + plot of the variants frequencies
<img src="/img/Screen2_SGSTab.png" width="500">


> Goldstein *et al*. Prediction and Quantification of Splice Events from RNA-Seq Data. PLoS ONE 2016, 11, e0156132, doi:10.1371/journal.pone.0156132.


contact : morganmaillard92@gmail.com

