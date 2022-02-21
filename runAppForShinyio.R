#installation of needed package
if (!require("shiny")) install.packages('shiny')
if (!require("ggplot2")) install.packages('ggplot2')
if (!require("shinyWidgets")) install.packages('shinyWidgets')
if (!require("shinydashboard")) install.packages('shinydashboard')
if (!require("DT")) install.packages('DT')
if (!require("pheatmap")) install.packages('pheatmap')
if (!require("tidyr")) install.packages('tidyr')
if (!require("dplyr")) install.packages('dplyr')
#if (!require("SGSeq")) BiocManager::install("SGSeq")
#if (!require("GenomicFeatures")) BiocManager::install('GenomicFeatures')
if (!require("shinyBS")) install.packages('shinyBS')



# library 
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(pheatmap)
library(tidyr)
library(dplyr)
#library(SGSeq)
#library(GenomicFeatures)
#library(BSgenome.Vvinifera.URGI.IGGP12Xv0) # genome de la vigne format BS
library(shinyBS) # allow popup windows
imgsize <- "auto 40%"
img <- 'http://northerntechmap.com/assets/img/loading-dog.gif'

###############################
#####   Resume of data    #####
###############################
myURL <- "https://github.com/momaillard/RNAseq_ShinyApp/raw/master/data/dataForApp.RData"
load(url(myURL))

source(file = "src/source2App.R")
#load(file = "./data/dataForApp.RData")
#si <- read.csv2("data/siFromGetBamInfo.csv", header = TRUE, row.names = 1) # si file for alterntive splicing


ui <- dashboardPage(skin = "red",
    dashboardHeader(title = "SweetK"),
    dashboardSidebar(
        sidebarMenu(id = "tabs",
            menuItem("Expression Plot", tabName = "expressionPlot", icon = icon("chart-line")),
            menuItem("TPM normalisation", tabName = "TPM_expression", icon = icon("balance-scale")),
            menuItem("Download Expression Matrix", tabName = "downloadTab", icon = icon("file-download")),
            menuItem("SOM View", tabName = "SomClustering", icon = icon("layer-group")),
            menuItem("Expression heatmap", tabName = "heatmapExpression", icon = icon("buromobelexperte")),
            menuItem("Gene pattern Comparaison", tabName = "geneComp", icon = icon("list-ol"),
                menuSubItem("Gene comparaison Deseq2", tabName = "expressionBiplot", icon = icon("list-ol")),
                menuSubItem("Gene comparaison TPM", tabName = "expressionBiplotTPM", icon = icon("list-ol"))
            ),
            menuItem("Alternative_Splicing", tabName = "SGS_tab", icon = icon("cut"))
        )
    ),
    dashboardBody(
        singleton(tags$head(HTML("
            <script type='text/javascript'>

            /* When recalculating starts, show loading screen */
            $(document).on('shiny:recalculating', function(event) {
            $('div#divLoading').addClass('show');
            });

            /* When new value or error comes in, hide loading screen */
            $(document).on('shiny:value shiny:error', function(event) {
            $('div#divLoading').removeClass('show');
            });

            </script>"))),

            # CSS Code
            singleton(tags$head(HTML(paste0("
            <style type='text/css'>
            #divLoading
            {
            display : none;
            }
            #divLoading.show
            {
            display : block;
            position : fixed;
            z-index: 100;
            background-image : url('",img,"');
            background-size:", imgsize, ";
            background-repeat : no-repeat;
            background-position : center;
            left : 0;
            bottom : 0;
            right : 0;
            top : 0;
            }
            #loadinggif.show
            {
            left : 50%;
            top : 50%;
            position : absolute;
            z-index : 101;
            -webkit-transform: translateY(-50%);
            transform: translateY(-50%);
            width: 100%;
            margin-left : -16px;
            margin-top : -16px;
            }
            div.content {
            width : 1000px;
            height : 1000px;
            }
            </style>")))),
        tabItems(
            tabItem(tabName = "expressionPlot",
                    box(status = "danger", width = 4, title = "Ploting gene expression", solidHeader = TRUE,
                        textInput(input = "geneName", label = "Enter Gene ID", value = "Gene_16"),
                        h5(strong("Arabidopsis orthologue : ")),
                        textOutput("annot1"),
                        h5(strong("Annotation :")),
                        textOutput("annot2"),
                        textOutput("annot3"),
                        br(),br(),br(),
                        h5(strong("Gene ontology")),
                        textOutput("annot4"),
                        h5(strong("plant ontology At orthologue")),
                        textOutput("annot5")
                    ),
                    box(width = 8,
                        plotOutput( "plotBoxplot", height = "450px"),
                        plotOutput("plotTPMBoxplot", height = "450px")
                    )
            ),
            tabItem(tabName = "TPM_expression",
                    box(width = 4, status = "danger", solidHeader = TRUE, title = "See TPM expression of a gene in a samples",
                        textInput(input = "geneNamesTPM", label = "Enter Gene ID", value = "Gene_14;Gene_2;Gene_3"),
                        pickerInput("SampleChoice", "Pick your Sample", choices = rownames(GittNormCount), 
                                    options = list(`actions-box` = TRUE, "max-options" = 1), 
                                    multiple = F),
                        actionButton("GO_button01", "go_button", class = "btn-success")
                    ),
                    box(width = 8,
                        plotOutput("QPCR", height = "1000px")
                    )
            ),
            tabItem(tabName = "downloadTab",
                box(width = 12,
                    title = "This tool is made to easily download the gene expression matrices", status = "primary", solidHeader = TRUE,
                    h4("choose you gene IDs, click on 'subset_The_Mat' and then on 'get_Matrix' to download it in csv format"),
                    p("choose the couple of button regardind which matrix you want to download. ", code("TPM matrix"), " is used for ", strong('IN SAMPLE comparaison'), " while ", code("DEseq2 normalized matrix"), " is used for ", 
                        strong('BETWEEN SAMPLES comparaison')),
                    textInput(input = "geneNamesToDL", label = "Enter Gene ID", value = "Gene_14;Gene_2;Gene_3"),
                    tags$style(type="text/css", "#actionB_subsetMat {width: 200px;background-color: #85C1E9}"),
                    actionButton("actionB_subsetMat", "Subset_The_TPM_Matrix", class = "btn-success", width = "200px", icon = icon("file")),
                    tags$style(type="text/css", "#downloadTPMMatrix {width: 200px;background-color: #5DADE2}"), # include some CSS to modify the download button (remplace le class = "btn-warning")
                    downloadButton("downloadTPMMatrix", "get_TPM_Matrix", class = "btn-warning"),
                    br(),
                    tags$style(type="text/css", "#myClickactionB_Deseq {width: 200px;background-color: #85C1E9}"),
                    actionButton("myClickactionB_Deseq", "Subset_The_Deseq_Mat", class = "btn-success", width = "200px", icon = icon("file")),
                    tags$style(type="text/css", "#downloadDeseqMatrix {width: 200px;background-color: #5DADE2}"), # include some CSS to modify the download button (remplace le class = "btn-warning") exemple que l'on peut ajouter dans le style "color: black;font-family: Courier New;background-color:white;
                    downloadButton("downloadDeseqMatrix", "get_Deseq_Matrix", class = "btn-warning")
                ),
                box(title = "TPM matrix", status = "info", DT::dataTableOutput("matriceToDL")),
                box(title = "Deseq Norm matrix", status = "info", DT::dataTableOutput("matriceDeseqToDL"))
            ),
            tabItem(tabName = "SomClustering",
                box(width = 4, status = "danger", solidHeader = TRUE, title = "Explore and visualize SOM gene cluster expression",
                    numericInput(inputId = "clusterName", label = "EnterClusterNumber", value = 1, min = 1, max = 122)   
                ),
                box(width = 8,
                    plotOutput( "SOMBoxplot", height = "450px")
                )
            ),
            tabItem(tabName = "heatmapExpression",    
                        box(width = 6, status = "danger", solidHeader = TRUE, title = "Plot heatmap of gene expresssion", 
                            h5("Enter a gene list to get the expression heatmap (gene separated by ';' )"),
                            fluidRow(
                                column(8, textInput(inputId = "geneList2Plot", label = "GeneID", value = "Gene_1;Gene_2;Gene_3;Gene_4;Gene_5;Gene_6;Gene_7;Gene_8;Gene_9;Gene_10;Gene_11;Gene_12;Gene_13;Gene_14;Gene_15;Gene_16")),
                                column(4, numericInput(inputId = "CutRowGL", "howManyRowCluter", value = 1, min = 1 , max = 10))
                            ),
                        actionButton("button_GL", "GO_GO", class = "btn-success"),
                        downloadButton("getClusterGene", "getCluster", class = "btn-warning"),
                        ),                           
                plotOutput("heatmap_selected_By_GL", height = "600px")
            ),
            tabItem(tabName = "expressionBiplot",
                box(width = 6, status = "danger", solidHeader = TRUE, title = "Visualize several genes expression in a single plot",
                    textInput(input = "biGene", label = "Enter Gene ID", value = "Gene_14;Gene_2;Gene_3"),
                    actionButton("buttonBiplot", "GO_GO", class = "btn-success")
                ),
                box(width = 12,
                    plotOutput("plotBiGeneExpr"),
                    plotOutput("plotBiGeneExprVSD"),
                    plotOutput("plotBiGeneExprCentered")
                   )
            ),
            tabItem(tabName = "expressionBiplotTPM",
                box(width = 6, status = "danger", solidHeader = TRUE, title = "Visualize several genes expression in a single plot",
                    textInput(input = "biGeneTPM", label = "Enter Gene ID", value = "Gene_14;Gene_2;Gene_3"),
                    actionButton("buttonBiplotTPM", "GO_GO", class = "btn-success")
                ),
                box(width = 12,
                    plotOutput("plotBiGeneExprTPM")
                )
            ),
            tabItem(tags$body(HTML("<div id='divLoading'> </div>")),
                tabName ="SGS_tab",
                fluidRow(
                    box(width = 6, status = "danger", title = "Alternative splicing by SGSseq", solidHeader = TRUE,
                        h5("Select a or several growth conditions and a gene ID (ex : 'Gene_3')"),
                        pickerInput("SGS_condition", "Pick your Sample conditions", multiple = TRUE, choices = c("Sample1","Sample2"), 
                                    options = list(`actions-box` = TRUE, "max-options" = 89)                           
                        ),
                        numericInput("minFPKM", "Minimum FPKM required for a splice junction to be included", value = 2, min = 0.5, max = 10, step = 0.5),
                        numericInput("minSample", "Minimum number of samples a feature must be observed in to be included", value = 2, min = 1, max = 89, step = 1),
                        textInput(input = "geneSGS", label = "Enter Gene ID", value = "Gene_3"),
                        actionButton("buttonSGS", "Launch_analysis",  icon("paper-plane"), class = "btn-success"),
                        actionButton("buttonFrequencies", "getfrequencies", icon("paper-plane"), class = "btn-success"),
                        downloadButton("tableFrequencies", "getTableFrequencies", class = "btn-warning"),
                        downloadButton("tableSplicing", "getTableSplicing", class = "btn-warning") 
                    ),
                    box(width = 6 , status = "info", title = "Once frequencies are calculated you can view Event plot", solidHeader = TRUE, 
                        textInput("eventToView", "chooseEventToPlot --See Table of frequencies--"),
                        actionButton("popupPlotEvent", label = "viewEvent", class = "info", icon("eye")),
                        numericInput(inputId = "plotFrequencies", label = "choose event ID to plot", value = 1, step = 1),
                        actionButton("plotFrequenciesButton", label = "Plot the frequencies", class = "info", icon ("stopwatch"))
                    )
                ),
                box(
                    h5("plot of the predicted annotation"),
                    plotOutput("grapheSplicingVanilla"),
                    width = 12
                ),
                box(width = 6, status = "danger", title = "plot of the de novo splicing in our experiment", solidHeader = TRUE,
                    fluidRow(
                        column(6, numericInput(inputId = "plotDeNovoGeneID", label = "if De novo plot seems wrong, set this value to 2 and click again on 'Launch analysis'", value = 1, step = 1)),
                        column(6, pickerInput("addTXView", label = HTML("set Tx view parametres <br/> Pick True to unstack the graph"), multiple = FALSE, choices = c(TRUE, FALSE), selected = FALSE))
                    )
                ),
                box(width = 12, plotOutput("grapheSplicingDeNovo", height = "1200px")),
                box(width = 12, DT::dataTableOutput("variant_fraquencies")),
                box(width = 12, DT::dataTableOutput("variant_recap")),
                box(width = 12, plotOutput("boxplotFreq")),
                bsModal("myModal", "Event_ID_frequencies", "popupPlotEvent", size = "Big", plotOutput("plotEvent"))
            )  
        )
    )
)

######  Define server  #####A
server <- function(input, output, session) {


    #############################################################
    ################        EXPRESSION PROFIL       #############
    #############################################################

    valeurs <- reactive({
        which(colnames(GittNormCount) == input$geneName)
    })

    valeursTPM <- reactive({
        which(colnames(GitTPMV1toAPP) == input$geneName)
    })

    output$plotBoxplot <- renderPlot({
        validate(
            need(input$geneName %in% colnames(GittNormCount), "Enter a valid Gene name")
        )
        qplot(data = GittNormCount, y = GittNormCount[,valeurs()], x = Temps, fill = Stress,
              geom = c("boxplot","point"), xlab = "Stade", ylab = "Deseq2 normalized Count",
              main = paste0("boxplot of ", input$geneName, " Deseq2 expression"), colour = Stress) +
              scale_fill_manual(values=c("royalblue1", "firebrick2" ))+
              scale_color_manual(values=c("royalblue4", "firebrick4" ))
              
    })

    output$plotTPMBoxplot <- renderPlot({
        validate(
            need(input$geneName %in% colnames(GitTPMV1toAPP), "Enter a valid Gene name")
        )
        qplot(data = GitTPMV1toAPP, y = GitTPMV1toAPP[,valeursTPM()], x = Temps, fill = Stress,
              geom = c("boxplot","point"), xlab = "Stade", ylab = "TPM",
              main = paste0("boxplot of ", input$geneName, " TPM expression"), colour = Stress) +
              scale_fill_manual(values=c("royalblue1", "firebrick2" ))+
              scale_color_manual(values=c("royalblue4", "firebrick4" ))
              
    })

    output$annot1 <-renderText({
        as.character(GitAnnotation[input$geneName,1])
        
    })  

    output$annot2 <- renderText({
        as.character(GitAnnotation[input$geneName,2])
    })

    output$annot3 <- renderText({
        as.character(GitAnnotation[input$geneName,3])
    })

     output$annot4 <- renderText({
        as.character(GitAnnotation[input$geneName,4])
    })

     output$annot5 <- renderText({
        as.character(GitAnnotation[input$geneName,5])
    })
    ######################################################################
    ###################         SOM PLOT        ##########################
    ######################################################################

    output$SOMBoxplot <- renderPlot({
        clusterNameValue <- paste("V",input$clusterName, sep = "")
        qplot(data = codesBooks, y = as.numeric(codesBooks[,clusterNameValue]), x = Temps, fill = Stress,
              geom = c("boxplot","point"), xlab = "Stress Condition", ylab = "Gene Expression",
              main = paste0("boxplot of ", clusterNameValue, " expression"), colour = Stress) +
              scale_fill_manual(values=c("royalblue1", "firebrick2" )) +
              scale_color_manual(values=c("royalblue4", "firebrick4" ))
    })

    #################################################
    #######        TPM PART START      ##############
    #################################################

    myClickGO_button01 <- reactiveValues()
    
    observeEvent(input$GO_button01, {
        print("parsing des arguments")
        tmpvector <- strsplit(input$geneNamesTPM, ";")
        tmpGeneSelect <- as.vector(tmpvector[[1]])
        myClickGO_button01$GeneList <- tmpGeneSelect
        myClickGO_button01$Sample <- input$SampleChoice

    })

    output$QPCR <- renderPlot({
        if (is.null(myClickGO_button01$Sample)) {
            return()
        }
        else {
            validate( 
                need(myClickGO_button01$GeneList %in% rownames(GittpmFinal), "Enter a valid Gene name")
        )
            Tdata <- t(GittpmFinal)
            dataSelect <- data.frame(Tdata [myClickGO_button01$Sample,myClickGO_button01$GeneList], myClickGO_button01$GeneList)
            print(dataSelect)
            colnames(dataSelect) <- c("reads", "geneID")
            ggplot(data = dataSelect, aes(x=geneID, y=reads)) +
            geom_bar(stat="identity") +
            geom_text(aes(label = reads), vjust = 1.6, color = "white", size = 10)+
            ggtitle(paste(c("TPM expression of ", myClickGO_button01$GeneList, " in", myClickGO_button01$Sample), collapse =" "))+
            labs(y = "TPM", x = "GeneID")
        }
    })

    #################################################
    #################  TPM Profils  #################
    #################################################

    myClickGO_button02 <- reactiveValues()
    
    observeEvent(input$GO_button02, {
        print("parsing des arguments")
        tmpvectorProfil <- strsplit(input$geneNamesTPMprofil, ";")
        tmpGeneSelectProfil <- as.vector(tmpvectorProfil[[1]])
        myClickGO_button02$GeneList <- tmpGeneSelectProfil
        myClickGO_button02$Sample <- input$SampleChoiceProfil
    })

    ########################################################
    ########        Downloading matrix part         ########
    ########################################################

    myClickactionB_subsetMat <- reactiveValues()
    
    observeEvent(input$actionB_subsetMat, {
        print("parsing des arguments")
        tmpvectorProfil <- strsplit(input$geneNamesToDL, ";")
        tmpGeneSelectProfil <- as.vector(tmpvectorProfil[[1]])
        myClickactionB_subsetMat$GeneList <- tmpGeneSelectProfil
        
        print(myClickactionB_subsetMat$GeneList)
        
    })

    toDownloadTPM <- reactive({
        if (is.null(myClickactionB_subsetMat$GeneList)) return()
        TPMV2toUse <- data.frame(GitTPMV1toAPP[,myClickactionB_subsetMat$GeneList],GitTPMV1toAPP$RepBio, rownames(GitTPMV1toAPP))
        colnames(TPMV2toUse) <- c(myClickactionB_subsetMat$GeneList, "Conditions", "SampleName")
        return(TPMV2toUse)
    })
    
    output$matriceToDL <- DT::renderDataTable({
        validate(
                need(myClickactionB_subsetMat$GeneList %in% colnames(GitTPMV1toAPP), "Enter a valid Gene name")
        )
        DT::datatable(toDownloadTPM())
    })

    output$downloadTPMMatrix <- downloadHandler(  
        filename <- function () {
            paste0("TPM_Matrix", as.numeric(Sys.time()), "_SubsetGenes.txt")
        },
        content <- function (file) {
            write.csv2(toDownloadTPM(), file, row.names = FALSE) 
        }
    )
    #### download DEseq Matrix ####
    
    myClickactionB_Deseq <- reactiveValues()
    
    observeEvent(input$myClickactionB_Deseq, {
        print("parsing des arguments")
        tmpvectorProfil <- strsplit(input$geneNamesToDL, ";")
        tmpGeneSelectProfil <- as.vector(tmpvectorProfil[[1]])
        myClickactionB_Deseq$GeneList <- tmpGeneSelectProfil
        
        print(myClickactionB_Deseq$GeneList)
        
    })

    toDownloadDeseqMat <- reactive({
        if (is.null(myClickactionB_Deseq$GeneList)) return()
        NormDeseq <- data.frame(GittNormCount[,myClickactionB_Deseq$GeneList],GittNormCount$RepBio, rownames(GittNormCount))
        colnames(NormDeseq) <- c(myClickactionB_Deseq$GeneList, "Conditions", "SampleName")
        return(NormDeseq)
    })

    output$matriceDeseqToDL <- DT::renderDataTable({
        validate(
                need(myClickactionB_Deseq$GeneList %in% colnames(GittNormCount), "EEnter a valid Gene name ")
        )
        DT::datatable(toDownloadDeseqMat())
    })

    output$downloadDeseqMatrix <- downloadHandler(  
        filename <- function () {
            paste0("DeseqNorm_Matrix", as.numeric(Sys.time()), "_SubsetGenes.txt")  #
        },
        content <- function (file) {
            write.csv2(toDownloadDeseqMat(), file, row.names = FALSE) 
        }
    )

    #################################################
    #############   HEATMAP PART    #################
    #################################################

    myClicks3 <- reactiveValues()
    
    observeEvent(input$button_GL, {
        print("changement de gene list")
        myClicks3$GeneList <- input$geneList2Plot
        myClicks3$CutRowGL <- input$CutRowGL
        print(myClicks3$GeneList)
    })
    
    getTargetedHeatmapGLf <- reactive({
        if (is.null(myClicks3$GeneList)) return()
        getHeatmapTargetByGL(myClicks3$GeneList, myClicks3$CutRowGL,Git.tVSD.center.scaled,orderedColdataLight)
    })
    
    output$heatmap_selected_By_GL <- renderPlot ({
        getTargetedHeatmapGLf()
    })

    CutTheTreeGLh <- reactive({
        if (is.null(myClicks3$GeneList)) return()
        print("cuting the tree")
        CutHeatmapTreeHclust(getTargetedHeatmapGLf()[[1]], myClicks3$CutRowGL)
    })
    
    output$getClusterGene <- downloadHandler( 
        filename <- function () {
            paste0("cutreeOf_","GeneFunction.txt")  
        },
        content <- function (file) {
            write.csv2(CutTheTreeGLh(), file, row.names = FALSE) 
        }
    )

    ################################################################
    ############          COMPARAISON GENES START       ############
    ################################################################

    ######## comparaison gène avec trois polot (norm From Deseq2, VSD from deseq2 et VSD centrées) ##########
    myClickGO_buttonBiplot <- reactiveValues()
    
    observeEvent(input$buttonBiplot, {
        tmpvectorBi <- strsplit(input$biGene, ";")
        tmpGeneSelectBi <- as.vector(tmpvectorBi[[1]])
        myClickGO_buttonBiplot$GeneListBi <- tmpGeneSelectBi
    })


    output$plotBiGeneExpr <- renderPlot({
        if (is.null(myClickGO_buttonBiplot$GeneListBi)) {
            return()
        }
       else {
            validate( # permet de checker si les conditions sont remplies (ici si le "gene en input" fait partie des gènes étudiés
                need(myClickGO_buttonBiplot$GeneListBi %in% colnames(GittNormCount), "Enter valid Gene names")
            )
            selectedCol <- c(myClickGO_buttonBiplot$GeneListBi,"Stress","Temps","RepBio")
            selectedGene <- myClickGO_buttonBiplot$GeneListBi
            myDataSelected <- GittNormCount[,selectedCol]
            myDataGathered <- gather( data = myDataSelected, "gene", "expression", -Stress, -Temps, -RepBio)

            myDataGathered$Gene_RepBio<-paste0(myDataGathered$gene, "_", myDataGathered$RepBio)
            Means <- myDataGathered %>% group_by(Gene_RepBio) %>% 
                summarize(Avg = mean(expression))

            myDataGatheredBis <- merge(x = myDataGathered, y = Means, by = "Gene_RepBio")

            myPlot<-ggplot(myDataGatheredBis, aes(x = Temps, y = expression, colour = gene, group = gene))+
            geom_point(aes(fill = gene))+
            geom_line(mapping = aes (Temps, Avg))+
            #theme(legend.position = "none")+ to remove legend or not
            facet_grid(.~Stress)+
            ggtitle(paste(c("Deseq2 normalised count Gene expression profils of ",selectedGene), collapse =" "))+
            labs(y = "Deseq2 normalised count", x = "Stade")
            
            myPlot
      }
   })

   output$plotBiGeneExprVSD <- renderPlot({
        if (is.null(myClickGO_buttonBiplot$GeneListBi)) {
            return()
        }
       else {
            validate( 
                need(myClickGO_buttonBiplot$GeneListBi %in% colnames(GittVSD2App), "Enter valid Gene names")
            )
            selectedCol <- c(myClickGO_buttonBiplot$GeneListBi,"Stress","Temps","RepBio")
            selectedGene <- myClickGO_buttonBiplot$GeneListBi
            myDataSelected <- GittVSD2App[,selectedCol]
            myDataGathered <- gather( data = myDataSelected, "gene", "expression", -Stress, -Temps, -RepBio)

            myDataGathered$Gene_RepBio<-paste0(myDataGathered$gene, "_", myDataGathered$RepBio)
            Means <- myDataGathered %>% group_by(Gene_RepBio) %>% 
                summarize(Avg = mean(expression))
            myDataGatheredBis <- merge(x = myDataGathered, y = Means, by = "Gene_RepBio")
            myPlot<-ggplot(myDataGatheredBis, aes(x = Temps, y = expression, colour = gene, group = gene))+
            geom_point(aes(fill = gene))+
            geom_line(mapping = aes (Temps, Avg))+
            facet_grid(.~Stress)+
            ggtitle(paste(c("VSD Gene expression profils of ",selectedGene), collapse =" "))+
            theme(legend.position = "none")+
            labs(y = "VSD normalised count", x = "Stade")
            myPlot
      }
   })
   output$plotBiGeneExprCentered <- renderPlot({
        if (is.null(myClickGO_buttonBiplot$GeneListBi)) {
            return()
        }
       else {
            validate( 
                need(myClickGO_buttonBiplot$GeneListBi %in% colnames(Git.vsd.center.scale2App), "Enter valid genes names")
            )
            selectedCol <- c(myClickGO_buttonBiplot$GeneListBi,"Stress","Temps","RepBio")
            selectedGene <- myClickGO_buttonBiplot$GeneListBi
            myDataSelected <- Git.vsd.center.scale2App[,selectedCol]
            myDataGathered <- gather( data = myDataSelected, "gene", "expression", -Stress, -Temps, -RepBio)

            myDataGathered$Gene_RepBio<-paste0(myDataGathered$gene, "_", myDataGathered$RepBio)
            Means <- myDataGathered %>% group_by(Gene_RepBio) %>% 
                summarize(Avg = mean(expression))
            myDataGatheredBis <- merge(x = myDataGathered, y = Means, by = "Gene_RepBio")
            myPlot<-ggplot(myDataGatheredBis, aes(x = Temps, y = expression, colour = gene, group = gene))+
            geom_point(aes(fill = gene))+
            geom_line(mapping = aes (Temps, Avg))+
            facet_grid(.~Stress)+
            ggtitle(paste(c("VSD.centered Gene expression profils of ",selectedGene), collapse =" "))+
            theme(legend.position = "none")+
            labs(y = "VSD centered normalised count", x = "Stade")
            myPlot
      }
   })

   #####################################################################
   ##########         PLOT TPM , GENE COMPARAISON           ############
   #####################################################################
    myClickGO_buttonBiplotTPM <- reactiveValues()
    
        observeEvent(input$buttonBiplotTPM, {
            tmpvectorBi <- strsplit(input$biGeneTPM, ";")
            tmpGeneSelectBi <- as.vector(tmpvectorBi[[1]])
            myClickGO_buttonBiplotTPM$GeneListBi <- tmpGeneSelectBi
        })

   output$plotBiGeneExprTPM <- renderPlot({
        if (is.null(myClickGO_buttonBiplotTPM$GeneListBi)) {
            return()
        }
       else {
            validate(
                need(myClickGO_buttonBiplotTPM$GeneListBi %in% colnames(GitTPMV1toAPP), "Enter valid Gene names")
            )
            selectedCol <- c(myClickGO_buttonBiplotTPM$GeneListBi,"Stress","Temps","RepBio")
            selectedGene <- myClickGO_buttonBiplotTPM$GeneListBi
            myDataSelected <- GitTPMV1toAPP[,selectedCol]
            myDataGathered <- gather( data = myDataSelected, "gene", "expression", -Stress, -Temps, -RepBio)

            myDataGathered$Gene_RepBio<-paste0(myDataGathered$gene, "_", myDataGathered$RepBio)
            Means <- myDataGathered %>% group_by(Gene_RepBio) %>% 
                summarize(Avg = mean(expression))

            myDataGatheredBis <- merge(x = myDataGathered, y = Means, by = "Gene_RepBio")

            myPlot<-ggplot(myDataGatheredBis, aes(x = Temps, y = expression, colour = gene, group = gene),)+
            geom_point(aes(fill = gene))+
            geom_line(mapping = aes (Temps, Avg))+
            facet_grid(.~Stress)+
            ggtitle(paste(c("norm Gene expression profils of ",selectedGene), collapse =" "))+
            labs(y = "TPM expression", x = "Stade")
            
            myPlot
      }
   })

    ###################################################################################
    ##########              ALTERNATIVE SPLICING PART / SGSESQ               ##########
    ###################################################################################

    myClickGO_buttonSGS <- reactiveValues()
    
    observeEvent(input$buttonSGS, {
        myClickGO_buttonSGS$conditions <- input$SGS_condition
        myClickGO_buttonSGS$gene <- input$geneSGS
        myClickGO_buttonSGS$IDplotDeNovo <- input$plotDeNovoGeneID
        myClickGO_buttonSGS$TXview <- input$addTXView
        myClickGO_buttonSGS$alpha <- input$minFPKM
        myClickGO_buttonSGS$min_n_sample <- input$minSample
        print(myClickGO_buttonSGS$conditions)
        str(myClickGO_buttonSGS$conditions)
        print(myClickGO_buttonSGS$gene)
        str(myClickGO_buttonSGS$gene)
        print(myClickGO_buttonSGS$alpha)
        print(myClickGO_buttonSGS$min_n_sample)
    })


    getResultsSGS <- reactive({
        if (is.null(myClickGO_buttonSGS$conditions)) return()
        launch_SGS(si, myClickGO_buttonSGS$gene, myClickGO_buttonSGS$conditions, myClickGO_buttonSGS$alpha, myClickGO_buttonSGS$min_n_sample)
    })
    
    output$grapheSplicingVanilla <- renderPlot ({
        if (is.null(myClickGO_buttonSGS$conditions)) return()
        dfVanilla <- plotFeatures(getResultsSGS()[[1]], geneID = 1)
    })

     output$grapheSplicingDeNovo <- renderPlot ({
        if (is.null(myClickGO_buttonSGS$conditions)) return()
        dfDeNovo <- plotFeatures(getResultsSGS()[[2]], geneID = myClickGO_buttonSGS$IDplotDeNovo, color_novel = "red", tx_view = myClickGO_buttonSGS$TXview)
    })

    myClick_buttonFrequencies <- reactiveValues()

    observeEvent(input$buttonFrequencies,{
        print(myClick_buttonFrequencies$signal)
        myClick_buttonFrequencies$signal <- 1
        print(myClick_buttonFrequencies$signal)
    })

    getFrequencieAndMore <- reactive ({
        if (is.null(myClick_buttonFrequencies$signal)) return()
        getVariantsFrequencies(getResultsSGS()[[2]], getResultsSGS()[[3]], getResultsSGS()[[4]])
    })

    output$variant_fraquencies <- DT::renderDataTable({
            DT::datatable(getFrequencieAndMore()[[4]], 
                options = list(scrollX =TRUE)
            )
    })
    output$variant_recap <- DT::renderDataTable({
            DT::datatable(getFrequencieAndMore()[[2]],
                options = list(scrollX =TRUE)
            )
    })

    output$tableSplicing <- downloadHandler( 
        filename <- function () {
            paste0("Table",input$geneSGS, "Splicing.txt")  
        },
        content <- function (file) {
            write.csv2(getFrequencieAndMore()[[2]], file, row.names = FALSE) 
        }
    )
    output$tableFrequencies <- downloadHandler( 
        filename <- function () {
            paste0("Table","Frequencies.txt")  
        },
        content <- function (file) {
            write.csv2(getFrequencieAndMore()[[4]], file, row.names = FALSE)
        }
    )
    myClickGO_plotFrequenciesButton <- reactiveValues()

    observeEvent(input$plotFrequenciesButton, {
        myClickGO_plotFrequenciesButton$eventToPlot <- input$plotFrequencies
        print(myClickGO_plotFrequenciesButton$eventToPlot)
        myClickGO_plotFrequenciesButton$frequenciesTable <- getFrequencieAndMore()[[1]]
        print(myClickGO_plotFrequenciesButton$frequenciesTable)
    })

    output$boxplotFreq <- renderPlot({
        print("je rentre dans le render plot")
        if (is.null(myClickGO_plotFrequenciesButton$frequenciesTable)) return()
        plotFreq <- plotTheFrequencies(myClickGO_plotFrequenciesButton$frequenciesTable, myClickGO_plotFrequenciesButton$eventToPlot)
        print("LE plot est produit ! ")
        plotFreq
    })

    ####### Modal popup Event ID #######


    myClickGO_popupPlotEvent <- reactiveValues()
    
    observeEvent(input$popupPlotEvent, {
        myClickGO_popupPlotEvent$eventToView <- input$eventToView
        myClickGO_popupPlotEvent$sgvc_pred <- getFrequencieAndMore()[[3]]
    })

    output$plotEvent <- renderPlot ({
        if (is.null(myClickGO_popupPlotEvent$eventToView)) return()
        eventPlot <- plotVariants(myClickGO_popupPlotEvent$sgvc_pred, eventID = as.numeric(myClickGO_popupPlotEvent$eventToView) , color_novel = "red", heightPanels = c(1,1), ypos = c(0.15, 0.1), main = paste0("Event = ", myClickGO_popupPlotEvent$eventToView))
    })

}

shinyApp(ui = ui, server = server)
