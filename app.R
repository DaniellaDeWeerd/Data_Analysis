###### TO INSTAL PACKAGES UNCOMMENT BELLOW ######
# install.packages(c("shiny","readxl","dplyr","ggplot2","highcharter","viridis","ggforce","bslib","devtools"))
# install_github("vqv/ggbiplot")
#################################################

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(highcharter)
library(viridis)
library(ggforce)
library(bslib)
library(devtools)
library(ggbiplot)

options(shiny.maxRequestSize = 30 * 1024^2)

theme = bs_theme(
  bootswatch = "darkly"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "lumen"
  ),
  titlePanel("Data Analysis Tool"),
  tags$script('$(document).on("keydown", function (e) { Shiny.onInputChange("lastkeypresscode", e.keyCode); });'),
  tabsetPanel(
    tabPanel(
      "Information and Help Page",
      mainPanel(h1("Data Format Requirements"),
                p("Please make sure that your data is in xlsx file format, your first column is row names, and your first row is column names."),
                p("Your Column names and Row names must not contain irrelevant metadata and only contain what you actually want to compare. Metadata is data about data. For example, if you have a bunch of cells on plates and some plates are NeuN only and the rest are labeled pSyn, the metadata might be plate_neun_mouse1_region. We don't want a column or row where each column name is all that metadata as the app is not able to pull out exactly what you want. As such, if you are looking to compare mice, you would only want your column/rownames to have the mouse number, or if you want to compare NeuN to pSynyou would have your column/row names be NeuN or pSyn."),
                h1("PCA requirements"),
                p("For PCA to work, you must do that first before any other analyses. As such, the option is given wether you would like to do PCA. If you select yes and then don't do PCA first, it will crash."),
                radioButtons("pcaOrNot","Select wether you will be doing pca or not.",c("No","Yes")),
                p(),h1("Notes on the functionality"),
                p("Everything with the checkboxes will require two to be clicked before it functions. An exception is seen in the Anova and t-test section where none can be clicked in one of the columns. More instructions are provided on the page."),
                p("The transpose button can be glitchy, so if you are hoping to keep flipping it back and forth, you may break it. I would reccomend to only use it once and download the resulting data for use. That said, most of the graphs have where it shows it row wise and column wise so it tranposes it for most of the visualizations anyways."),
                p("When you download a file. Make sure to input a name. It will be downloaded into the same folder as the app."),
                p(),p("If you run into any issues or want a functionality added, please contact me. My email is: danielladeweerd@gmail.com."))
    ),
    tabPanel(
      "Input Data",
      sidebarLayout(
        sidebarPanel(
          fileInput("inputData","File Here (please make sure it is xlsx):",accept = c(".xlsx")),
          textInput("colSearch","Search Column Names",value = ""),
          checkboxGroupInput("rCols", "Removed Cols",c("Don't Click This")),
          checkboxGroupInput("sCols","Select your Columns to Remove",c("A","B","C")),
          textInput("rowSearch","Search Row Names",value = ""),
          checkboxGroupInput("rRows", "Removed Rows",c("Don't Click This")),
          checkboxGroupInput("sRows","Select your Rows to Remove",c("A","B","C")),
          actionButton("transpButton","Transpose Your Data"),
          textInput("fileName1","Enter file name (don't include file type)",value = ""),
          actionButton("downloadButton1","Download Data")
        ),
        mainPanel(p("The following calculated values are not including NAs."),
                  textInput("colDataSearch","Search for Columns",value = ""),p(),
                  verbatimTextOutput("colStats"),
                  textInput("rowDataSearch","Search for Rows",value = ""),p(),
                  verbatimTextOutput("rowStats"))
      )
    ),
    tabPanel(
      "Bar Graph",
      sidebarLayout(
        sidebarPanel(
          radioButtons("colorInputBar","Select Your Color Palette",c("magma","inferno","plasma","viridis","cividis")),
          textInput("colSearchBar","Search Column Names",value = ""),
          checkboxGroupInput("sColsBar", "Selected Cols",c("Don't Click This")),
          checkboxGroupInput("cColsBar","Select your Columns",c("A","B","C")),
          textInput("rowSearchBar","Search Row Names",value = ""),
          checkboxGroupInput("sRowsBar", "Selected Rows",c("Don't Click This")),
          checkboxGroupInput("cRowsBar","Select your Rows",c("A","B","C"))
        ),
        mainPanel(plotOutput("colBarGraph"),
                  textInput("fileName2","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton2","Download Data"),
                  plotOutput("rowBarGraph"),
                  textInput("fileName3","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton3","Download Data"))
      )
    ),
    tabPanel(
      "Line Graph",
      sidebarLayout(
        sidebarPanel(
          radioButtons("colorInputLine","Select Your Color Palette",c("magma","inferno","plasma","viridis","cividis")),
          textInput("colSearchLine","Search Column Names",value = ""),
          checkboxGroupInput("sColsLine", "Selected Cols",c("Don't Click This")),
          checkboxGroupInput("cColsLine","Select your Columns",c("A","B","C")),
          textInput("rowSearchLine","Search Row Names",value = ""),
          checkboxGroupInput("sRowsLine", "Selected Rows",c("Don't Click This")),
          checkboxGroupInput("cRowsLine","Select your Rows",c("A","B","C"))
        ),
        mainPanel(plotOutput("colLineGraph"),
                  textInput("fileName4","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton4","Download Data"),
                  plotOutput("rowLineGraph"),
                  textInput("fileName5","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton5","Download Data"))
      )
    ),
    tabPanel(
      "Violin Plot",
      sidebarLayout(
        sidebarPanel(
          radioButtons("colorInputViolin","Select Your Color Palette",c("magma","inferno","plasma","viridis","cividis")),
          textInput("colSearchViolin","Search Column Names",value = ""),
          checkboxGroupInput("sColsViolin", "Selected Cols",c("Don't Click This")),
          checkboxGroupInput("cColsViolin","Select your Columns",c("A","B","C")),
          textInput("rowSearchViolin","Search Row Names",value = ""),
          checkboxGroupInput("sRowsViolin", "Selected Rows",c("Don't Click This")),
          checkboxGroupInput("cRowsViolin","Select your Rows",c("A","B","C"))
        ),
        mainPanel(p("This function will only work properlly if the groups you select have more than two points in common. For example, you select column group a and row group b, there must be more than two points that fit those two groups overlap."),
                  plotOutput("colViolinGraph"),
                  textInput("fileName6","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton6","Download Data"),
                  plotOutput("rowViolinGraph"),
                  textInput("fileName7","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton7","Download Data"))
      )
    ),
    tabPanel(
      "Histogram",
      sidebarLayout(
        sidebarPanel(
          radioButtons("colorInputHistogram","Select Your Color Palette",c("magma","inferno","plasma","viridis","cividis")),
          textInput("oneColSearchHistogram", "Search Column Name", value = ""),
          radioButtons("oneColHistogram", "Select your Col for your Base", c("A","B","C"), selected = character(0)),
          textInput("multiRowSearchHistogram", "Search Row Names", value = ""),
          checkboxGroupInput("sRowsHistogram", "Selected Rows",c("Don't Click This")),
          checkboxGroupInput("multiRowsHistogram", "Select your Rows to Include", c("A","B","C")),
          textInput("oneRowSearchHistogram", "Search Row Name", value = ""),
          radioButtons("oneRowHistogram", "Select your Row for your Base", c("A","B","C"), selected = character(0)),
          textInput("multiColSearchHistogram", "Search Col Names", value = ""),
          checkboxGroupInput("sColsHistogram", "Selected Cols",c("Don't Click This")),
          checkboxGroupInput("multiColsHistogram", "Select your Rows to Include", c("A","B","C"))
          
        ),
        mainPanel(plotOutput("colHistogramGraph"),
                  textInput("fileName8","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton8","Download Data"),
                  plotOutput("rowHistogramGraph"),
                  textInput("fileName9","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton9","Download Data"))
      )
    ),
    tabPanel(
      "PCA",
      sidebarLayout(
        sidebarPanel(
          radioButtons("colorInputPca","Select Your Color Palette",c("magma","inferno","plasma","viridis","cividis")),
          textInput("colSearchPca","Search Column Names",value = ""),
          checkboxGroupInput("sColsPca", "Selected Cols",c("Don't Click This")),
          checkboxGroupInput("cColsPca","Select your Columns",c("A","B","C")),
          textInput("rowSearchPca","Search Row Names",value = ""),
          checkboxGroupInput("sRowsPca", "Selected Rows",c("Don't Click This")),
          checkboxGroupInput("cRowsPca","Select your Rows",c("A","B","C"))
        ),
        mainPanel(verbatimTextOutput("colPca"),plotOutput("colPcaGraph"),
                  textInput("fileName10","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton10","Download Data"),
                  verbatimTextOutput("rowPca"),plotOutput("rowPcaGraph"),
                  textInput("fileName11","Enter file name (don't include file type)",value = ""),
                  actionButton("downloadButton11","Download Data"))
      )
    ),
    tabPanel(
      "T-Test and Anova",
      sidebarLayout(
        sidebarPanel(
          radioButtons("colorInputTAnova","Select Your Color Palette",c("magma","inferno","plasma","viridis","cividis")),
          textInput("colSearchTAnova","Search Column Names",value = ""),
          checkboxGroupInput("sColsTAnova", "Selected Cols",c("Don't Click This")),
          checkboxGroupInput("cColsTAnova","Select your Columns",c("A","B","C")),
          textInput("rowSearchTAnova","Search Row Names",value = ""),
          checkboxGroupInput("sRowsTAnova", "Selected Rows",c("Don't Click This")),
          checkboxGroupInput("cRowsTAnova","Select your Rows",c("A","B","C"))
        ),
        mainPanel(verbatimTextOutput("tTest"),p(),
                  verbatimTextOutput("anova"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  v <- reactiveValues(TQ = F, oData="original", mData = c("working normal"), Rows = c(), Cols = c(), mRows = c(), mCols = c(),
                      tOData = "transposed original", tMData = "transposed working", tRows = c(), tCols = c(),tMRows = c(), tMCols = c(),
                      rCols = c(), rRows = c(), sCols = c(), sRows = c(),
                      tRCols = c(), tRRows = c(), tSRows = c(), tSCols = c())
  
  
  ###### Basic Functions ######
  
  updateVariables <- function(){
    print("Updating variables...")
    v$mData <- v$oData[!v$Rows %in% v$rRows,!v$Cols %in% v$rCols]
    v$tMData <- v$tOData[!v$tRows %in% v$tRRows, !v$tCols %in% v$tRCols]
    v$mCols <- v$Cols[!v$Cols %in% v$rCols]
    v$mRows <- v$Rows[!v$Rows %in% v$rRows]
    v$tMCols <- v$tCols[!v$tCols %in% v$tRCols]
    v$tMRows <- v$tRows[!v$tRows %in% v$tRRows]
    v$sCols <- unique(v$mCols)
    if (length(v$sCols) > 20) {v$sCols <- v$sCols[1:20]}
    v$sRows <- unique(v$mRows)
    if (length(v$sRows) > 20) {v$sRows <- v$sRows[1:20]}
    v$tSCols <- unique(v$tMCols)
    if (length(v$tSCols) > 20) {v$tSCols <- v$tSCols[1:20]}
    v$tSRows <- unique(v$tMRows)
    if (length(v$tSRows) > 20) {v$tSRows <- v$tSRows[1:20]}
    
    print("Done updating variables")
  }
  
  updateInputChoices <- function() {
    print("Updating Input Choices...")
    colsList <- c("cColsBar","cColsLine","cColsViolin","multiColsHistogram","cColsPca","cColsTAnova")
    rowsList <- c("cRowsBar","cRowsLine","cRowsViolin","multiRowsHistogram","cRowsPca","cRowsTAnova")
    sColsList <- c("sColsBar","sColsLine","sColsViolin","sColsHistogram","sColsPca","sColsTAnova")
    sRowsList <- c("sRowsBar","sRowsLine","sRowsViolin","sRowsHistogram","sRowsPca","sRowsTAnova")
    for (i in 1:length(colsList)) {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), sColsList[i],
                               label = "Selected Columns",
                               choices = c("Don't Click This"))
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), sRowsList[i],
                               label = "Selected Rows",
                               choices = c("Don't Click This"))
    }
    if (v$TQ == F) {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "rCols",
                               label = "Removed Cols",
                               choices = v$rCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "rRows",
                               label = "Removed Rows",
                               choices = v$rRows)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sCols",
                               label = "Select your Columns to Remove",
                               choices = v$sCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRows",
                               label = "Select your Rows to Remove",
                               choices = v$sRows)
      updateRadioButtons(session = getDefaultReactiveDomain(), "oneRowHistogram",
                         label = "Select your Row for your Base",
                         choices = v$sRows)
      updateRadioButtons(session = getDefaultReactiveDomain(), "oneColHistogram",
                         label = "Select your Col for your Base",
                         choices = v$sCols)
      for (i in 1:length(colsList)) {
        updateCheckboxGroupInput(session = getDefaultReactiveDomain(), colsList[i],
                                 label = "Select your Columns",
                                 choices = v$sCols)
        updateCheckboxGroupInput(session = getDefaultReactiveDomain(), rowsList[i],
                                 label = "Select your Rows",
                                 choices = v$sRows)
      }
    }
    else {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "rCols",
                               label = "Removed Cols",
                               choices = v$tRCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "rRows",
                               label = "Removed Rows",
                               choices = v$tRRows)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sCols",
                               label = "Select your Columns to Remove",
                               choices = v$tSCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRows",
                               label = "Select your Rows to Remove",
                               choices = v$tSRows)
      updateRadioButtons(session = getDefaultReactiveDomain(), "oneRowHistogram",
                         label = "Select your Row for your Base",
                         choices = v$tSRows)
      updateRadioButtons(session = getDefaultReactiveDomain(), "oneColHistogram",
                         label = "Select your Col for your Base",
                         choices = v$tSCols)
      for (i in 1:length(colsList)) {
        updateCheckboxGroupInput(session = getDefaultReactiveDomain(), colsList[i],
                                 label = "Select your Columns",
                                 choices = v$tSCols)
        updateCheckboxGroupInput(session = getDefaultReactiveDomain(), rowsList[i],
                                 label = "Select your Rows",
                                 choices = v$tSRows)
      }
    }
    print("Done updating inputs")
  }
  
  updateOutputs <- function() {
    print("Updating Outputs...")
    if (v$TQ == F) {
      output$colStats <- renderPrint({getSummary(v$mData,v$mCols)})
      output$rowStats <- renderPrint({getSummary(v$tMData,v$tMCols)})
    }
    else {
      output$colStats <- renderPrint({getSummary(v$tMData,v$tMCols)})
      output$rowStats <- renderPrint({getSummary(v$mData,v$mCols)})
    }
    print("Done updating outputs")
  }

  getSummary <- function (data,colNames){
    #1 for row-wise, 2 for column-wise, naR == T or F
    true <- T
    name <- colNames
    cols <- unique(name)
    maxLevel <- max(table(name))
    combinedData <- data.frame(matrix(ncol = length(cols), nrow = length(data[,1])* maxLevel))
    for (i in 1:length(cols)) {
      col <- cols[i]
      column <- unlist(data[name %in% col], use.names = F)
      combinedData[i] <- column
    }

    mins <- data.frame(apply(combinedData,2,min,na.rm = T))
    mins <- t(mins)[1,]
    maxs <- data.frame(apply(combinedData,2,max,na.rm = T))
    maxs <- t(maxs)[1,]
    sums <- data.frame(apply(combinedData,2,sum,na.rm = T))
    sums <- t(sums)[1,]
    means <- data.frame(apply(combinedData,2,mean,na.rm = T))
    means <- t(means)[1,]
    stds <- data.frame(apply(combinedData,2,sd,na.rm = T))
    stds <- t(stds)[1,]
    quartile <- data.frame(apply(combinedData,2,quantile,na.rm = T))
    first <- quartile[2,]
    third <- quartile[4,]
    NAs <- sapply(combinedData, function(x) sum(is.na(x)))

    toPrint <- rbind(mins,maxs,sums,first,means,third,stds,NAs)
    colnames(toPrint) <- cols
    rownames(toPrint) <- c("Min","Max","Sum","1st Quartile","Mean","2rd Quartile","Standard Deviation", "Number of NAs")
    print(toPrint)
  }
  
  observeEvent(input$inputData,{
    print("START HERE")
    #Load The File Here
    file <- input$inputData
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file."))
    data <- read_excel(file$datapath,col_names = F)
    print("file read")
    v$Rows <- data %>% pull(1)
    v$Rows <- v$Rows[-1]
    v$mRows <- v$Rows
    data <- data[-1]
    data <- as.data.frame(data)
    v$Cols <- as.character(data[1,])
    v$mCols <- v$Cols
    data <- data[-1,]
    print("rownames added")
    data <- lapply(data,as.numeric)
    data <- as.data.frame(data)
    v$oData <- data
    v$mData <- data
    print("Data uploaded")
    transposed <- t(data)
    transposed <- as.data.frame(transposed)
    v$tOData <- transposed
    v$tMData <- transposed
    print("Transposing Done")
    v$tRows <- v$Cols
    v$tMRows <- v$tRows
    v$tCols <- v$Rows
    v$tMCols <- v$tCols
    
    updateVariables()
    updateInputChoices()
    updateOutputs()
  })
  
  ##### Unique the columns when removing and remove all that match #####
  
  observe({
    if(!is.null(input$lastkeypresscode)) {
      if(input$lastkeypresscode == 13){
        colsList <- c("cColsBar","cColsLine","cColsViolin","multiColsHistogram","cColsPca","cColsTAnova")
        rowsList <- c("cRowsBar","cRowsLine","cRowsViolin","multiRowsHistogram","cRowsPca","cRowsTAnova")
        if (v$TQ == F) {
          searchResults <- v$mCols
          searchResults <- searchResults[grepl(input$colSearch,searchResults)]
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sCols",
                                   label = "Select your Columns to Remove",
                                   choices = searchResults)
          print("Search results for Cols updated")
          
          searchResults <- v$mRows
          searchResults <- searchResults[grepl(input$rowSearch,searchResults)]
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRows",
                                   label = "Select your Rows to Remove",
                                   choices = searchResults)
          print("Search results for Rows updated")
          
          searchResults <- v$mRows
          searchResults <- unique(searchResults[grepl(input$oneRowSearchHistogram,searchResults)])
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateRadioButtons(session = getDefaultReactiveDomain(), "oneRowHistogram",
                             label = "Select your Row for your Base",
                             choices = c(input$oneRowHistogram,searchResults),selected = input$oneRowHistogram)
          print("Search results for row updated")
          searchResults <- v$mCols
          searchResults <- unique(searchResults[grepl(input$oneColSearchHistogram,searchResults)])
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateRadioButtons(session = getDefaultReactiveDomain(), "oneColHistogram",
                             label = "Select your Col for your Base",
                             choices = c(input$oneColHistogram,searchResults),selected = input$oneColHistogram)
          print("Search results for col updated")
          
          for (i in 1:length(colsList)) {
            searchResults <- v$mCols
            searchResults <- searchResults[grepl(input$colSearch,searchResults)]
            if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
            updateCheckboxGroupInput(session = getDefaultReactiveDomain(), colsList[i],
                                     label = "Select your Columns",
                                     choices = searchResults)
            print("Search results for Cols updated")
            
            searchResults <- v$mRows
            searchResults <- searchResults[grepl(input$rowSearch,searchResults)]
            if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
            updateCheckboxGroupInput(session = getDefaultReactiveDomain(), rowsList[i],
                                     label = "Select your Rows",
                                     choices = searchResults)
            print("Search results for Rows updated")
          }
        }
        else {
          searchResults <- v$tMCols
          searchResults <- searchResults[grepl(input$colSearch,searchResults)]
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sCols",
                                   label = "Select your Columns to Remove",
                                   choices = searchResults)
          print("Search results for Cols updated")
          
          searchResults <- v$tMRows
          searchResults <- searchResults[grepl(input$rowSearch,searchResults)]
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRows",
                                   label = "Select your Rows to Remove",
                                   choices = searchResults)
          print("Search results for Rows updated")
          searchResults <- v$tMRows
          searchResults <- unique(searchResults[grepl(input$oneRowSearchHistogram,searchResults)])
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateRadioButtons(session = getDefaultReactiveDomain(), "oneRowHistogram",
                             label = "Select your Row for your Base",
                             choices = c(input$oneRowHistogram,searchResults),selected = input$oneRowHistogram)
          print("Search results for row updated")
          searchResults <- v$tMCols
          searchResults <- unique(searchResults[grepl(input$oneColSearchHistogram,searchResults)])
          if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
          updateRadioButtons(session = getDefaultReactiveDomain(), "oneColHistogram",
                             label = "Select your Col for your Base",
                             choices = c(input$oneColHistogram,searchResults),selected = input$oneColHistogram)
          print("Search results for col updated")
          
          for (i in 1:length(colsList)) {
            searchResults <- v$tMCols
            searchResults <- searchResults[grepl(input$colSearch,searchResults)]
            if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
            updateCheckboxGroupInput(session = getDefaultReactiveDomain(), colsList[i],
                                     label = "Select your Columns",
                                     choices = searchResults)
            print("Search results for Cols updated")
            
            searchResults <- v$tMRows
            searchResults <- searchResults[grepl(input$rowSearch,searchResults)]
            if (length(searchResults) > 20 ) {searchResults <- searchResults[1:20]}
            updateCheckboxGroupInput(session = getDefaultReactiveDomain(), rowsList[i],
                                     label = "Select your Rows",
                                     choices = searchResults)
            print("Search results for Rows updated")
          }
        }
      }
    }
  })
  
  observeEvent(input$transpButton,{
    if (v$TQ == F) {
      v$TQ <- T
    }
    else {
      v$TQ <- F
    }
    updateVariables()
    updateInputChoices()
    updateOutputs()
    updateAllPlots()
  })
  
  observeEvent(input$downloadButton1,{
    filename <- paste(input$fileName1,".csv")
    if (v$TQ == F) {
      data <- v$mData
      data <- rbind(v$mCols,data)
      rows <- c("",v$mRows)
      data <- cbind(rows,data)
      write.csv(data,filename,row.names = F,col.names = F)
    }
    else {
      data <- v$tMData
      data <- rbind(v$tMCols,data)
      rows <- c("",v$tMRows)
      data <- cbind(rows,data)
      write.csv(data,filename,row.names = F,col.names = F)
    }
    print("Done downloading")
  })
  
  observeEvent(input$sCols,{
    if (!is.null(input$sCols)) {
      print("Col choice clicked")
      if (v$TQ == F) {
        toRemove <- input$sCols[!input$sCols %in% v$rCols]
        v$rCols <- c(v$rCols,toRemove)
        v$tRRows <- v$rCols
      }
      else {
        toRemove <- input$sCols[!input$sCols %in% v$tRCols]
        v$tRCols <- c(v$tRCols,toRemove)
        v$rRows <- v$tRCols
      }
      updateVariables()
      updateInputChoices()
      updateOutputs()
    }
  })
  
  observeEvent(input$sRows,{
    if (!is.null(input$sRows)) {
      print("Row choice clicked")
      if (v$TQ == F) {
        toRemove <- input$sRows[!input$sRows %in% v$rRows]
        v$rRows <- c(v$rRows,toRemove)
        v$tRCols <- v$rRows
      }
      else {
        toRemove <- input$sRows[!input$sRows %in% v$tRRows]
        v$tRRows <- c(v$tRRows,toRemove)
        v$rCols <- v$tRRows
      }
      updateVariables()
      updateInputChoices()
      updateOutputs()
    }
  })
  
  observeEvent(input$rCols,{
    if (!is.null(input$rCols)) {
      print("Col choice clicked")
      if (v$TQ == F) {
        toRemove <- input$rCols[input$rCols %in% v$rCols]
        v$rCols <- v$rCols[!v$rCols %in% toRemove]
        v$tRRows <- v$rCols
      }
      else {
        toRemove <- input$rCols[input$rCols %in% v$tRCols]
        v$tRCols <- v$tRCols[!v$tRCols %in% toRemove]
        v$rRows <- v$tRCols
      }
      updateVariables()
      updateInputChoices()
      updateOutputs()
    }
  })
  
  observeEvent(input$rRows,{
    if (!is.null(input$rRows)) {
      print("Row choice clicked")
      if (v$TQ == F) {
        toRemove <- input$rRows[input$rRows %in% v$rRows]
        v$rRows <- v$rRows[!v$rRows %in% toRemove]
        v$tRCols <- v$rRows
      }
      else {
        toRemove <- input$rRows[input$rRows %in% v$tRRows]
        v$tRRows <- v$tRRows[!v$tRRows %in% toRemove]
        v$rCols <- v$tRRows
      }
      updateVariables()
      updateInputChoices()
      updateOutputs()
    }
  })
  
  ###### BAR STUFFS ######
  
  updateAllPlots <- function() {
    print("Bar")
    updateGraphSelections()
    print("Line")
    updateGraphSelections1()
    print("Violin")
    updateGraphSelections2()
    print("Histogram")
    updateGraphSelections3()
    if (input$pcaOrNot == "Yes") {
      print("Pca")
      updateGraphSelections4()
    }
    print("TAnova")
    updateGraphSelections5()
  }
  
  createAllData <- function (Cols,Rows) {
    if (v$TQ == F) {
      print("Here")
      print(Rows)
      print(Cols)
      modifiedData <- v$mData[v$mRows %in% Rows, v$mCols %in% Cols]
      rows <- v$mRows[v$mRows %in% Rows]
      cols <- v$mCols[v$mCols %in% Cols]
      runningNumList <- c()
      runningColList <- c()
      runningRowList <- c()
      if (length(cols) >1 & length(rows) >1) {
        print("both are greater than 1")
        for (i in 1:length(rows)) {
          runningNumList <- c(runningNumList,unlist(modifiedData[i,]))
          runningColList <- c(runningColList,cols)
          runningRowList <- c(runningRowList, replicate(length(cols),rows[i]))
        }
        theData <- cbind(runningColList,runningRowList,runningNumList)
        colnames(theData) <- c("colNames","rowNames","values")
        theData <- data.frame(theData)
        theData$values<-as.numeric(theData$values)
        if (length(Rows) >1 & length(Cols) >1 ) {
          g$allData <- theData
        }
        else if (length(Rows) == 1) {
          g$rowData <- theData
        }
        else if (length(Cols) == 1) {
          g$colData <- theData
        }
      }
      else if (length(cols) == 1) {
        print("only one col")
        theData <- cbind(replicate(length(rows),cols), rows, modifiedData)
        colnames(theData) <- c("colNames","rowNames","values")
        theData <- data.frame(theData)
        theData$values<-as.numeric(theData$values)
        g$colData <- theData
      }
      else if (length(rows) == 1) {
        print("only one row")
        print(modifiedData)
        theData <- cbind(cols, replicate(length(cols),rows), modifiedData)
        colnames(theData) <- c("colNames","rowNames","values")
        theData <- data.frame(theData)
        theData$values<-as.numeric(theData$values)
        g$rowData <- theData
      }
    }
    else {
      modifiedData <- v$tMData[v$tMRows %in% Rows, v$tMCols %in% Cols]
      rows <- v$tMRows[v$tMRows %in% Rows]
      cols <- v$tMCols[v$tMCols %in% Cols]
      runningNumList <- c()
      runningColList <- c()
      runningRowList <- c()
      if (length(cols) >1 & length(rows) >1) {
        for (i in 1:length(rows)) {
          runningNumList <- c(runningNumList,unlist(modifiedData[i,]))
          runningColList <- c(runningColList,cols)
          runningRowList <- c(runningRowList, replicate(length(cols),rows[i]))
        }
        theData <- cbind(runningColList,runningRowList,runningNumList)
        colnames(theData) <- c("colNames","rowNames","values")
        theData <- data.frame(theData)
        theData$values<-as.numeric(theData$values)
        g$allData <- theData
      }
      else if (length(cols) == 1) {
        print("only one col")
        theData <- cbind(replicate(length(rows),cols), rows, modifiedData)
        colnames(theData) <- c("colNames","rowNames","values")
        theData <- data.frame(theData)
        theData$values<-as.numeric(theData$values)
        g$colData <- theData
      }
      else if (length(rows) == 1) {
        print("only one row")
        print(modifiedData)
        theData <- cbind(cols, replicate(length(cols),rows), modifiedData)
        colnames(theData) <- c("colNames","rowNames","values")
        theData <- data.frame(theData)
        theData$values<-as.numeric(theData$values)
        g$rowData <- theData
      }
    }
    print("All Data Created")
  }
  
  g <- reactiveValues(sRows = c(), sCols = c(),sTRows = c(), sTCols = c(), allData = c(), histogram = "text", colData = c(), rowData = c(), 
                      bar1 = c(), bar2 = c(), line1 = c(), line2 = c(), violin1 = c(), violin2 = c(), histogram1 = c(), histogram2 = c(), pca1 = c(), pca2 = c())
  
  updateGraphSelections <- function() {
    if (v$TQ == F) {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsBar",
                               label = "Selected Columns",
                               choices = g$sCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsBar",
                               label = "Selected Rows",
                               choices = g$sRows)
    
      displayedColChoices <- v$mCols[!v$mCols %in% g$sCols]
      displayedRowChoices <- v$mRows[!v$mRows %in% g$sRows]
      if(length(g$sCols) >= 2 & length(g$sRows) >= 2) {
        print("enough to graph")
        createAllData(g$sCols,g$sRows)
        output$colBarGraph <- renderPlot({barPlot(T)})
        output$rowBarGraph <- renderPlot({barPlot(F)})
      }
    }
    else {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsBar",
                               label = "Selected Columns",
                               choices = g$sTCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsBar",
                               label = "Selected Rows",
                               choices = g$sTRows)
      displayedColChoices <- v$tMCols[!v$tMCols %in% g$sTCols]
      displayedRowChoices <- v$tMRows[!v$tMRows %in% g$sTRows]
      if(length(g$sTCols) >= 2 & length(g$sTRows) >= 2) {
        print("enough to graph")
        createAllData(g$sTCols,g$sTRows)
        output$colBarGraph <- renderPlot({barPlot(T)})
        output$rowBarGraph <- renderPlot({barPlot(F)})
      }
    }
    displayedColChoices <- unique(displayedColChoices)
    displayedRowChoices <- unique(displayedRowChoices)
    if (length(displayedColChoices) > 20) {displayedColChoices <- displayedColChoices[1:20]}
    if (length(displayedRowChoices) > 20) {displayedRowChoices <- displayedRowChoices[1:20]}
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cColsBar",
                             label = "Select your Columns",
                             choices = displayedColChoices)
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cRowsBar",
                             label = "Select your Rows",
                             choices = displayedRowChoices)
    print("finished updating")
  }
  
  #if any of the columns changed
  observeEvent(input$cColsBar,{
    print("col was clicked")
    if (!is.null(input$cColsBar)) {
      if (v$TQ == F) {
        clicked <- input$cColsBar[!input$cColsBar %in% g$sCols]
        g$sCols <- c(g$sCols, clicked)
      }
      else {
        clicked <- input$cColsBar[!input$cColsBar %in% g$sTCols]
        g$sTCols <- c(g$sTCols, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$cRowsBar,{
    print("One of the inputs was clicked")
    if (!is.null(input$cRowsBar)) {
      if (v$TQ == F) {
        clicked <- input$cRowsBar[!input$cRowsBar %in% g$sRows]
        g$sRows <- c(g$sRows, clicked)
      }
      else {
        clicked <- input$cRowsBar[!input$cRowsBar %in% g$sTRows]
        g$sTRows <- c(g$sTRows, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$sColsBar,{
    if (!is.null(input$sColsBar)) {
      g$sCols <- g$sCols[!g$sCols %in% input$sColsBar]
      updateAllPlots()
    }
  })
  
  observeEvent(input$sRowsBar,{
    if (!is.null(input$sRowsBar)) {
      g$sRows <- g$sRows[!g$sRows %in% input$sRowsBar]
      updateAllPlots()
    }
  })
  
  observeEvent(input$downloadButton2,{
    fileName <- paste(input$fileName2,".pdf")
    pdf(fileName)
    plot(g$bar1)
    dev.off()
  })
  
  observeEvent(input$downloadButton3,{
    fileName <- paste(input$fileName3,".pdf")
    pdf(fileName)
    plot(g$bar2)
    dev.off()
  })
  
  barPlot <- function(colQ) {
    color <- input$colorInputBar
    if (colQ == T) {
      theData <- g$allData
      p<-ggplot(data=theData, aes(fill=colNames, x=rowNames, y=values)) +
        geom_bar(position='dodge', stat='identity') +
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_viridis_d(option = color)
      g$bar1 <- p
    }
    else {
      theData <- g$allData
      p<-ggplot(data=theData, aes(fill=rowNames, x=colNames, y=values)) +
        geom_bar(position='dodge', stat='identity') +
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_viridis_d(option = color)
      g$bar2 <- p
    }
    plot(p)
    print("Plotted")
  }
  
  ###### LINE STUFFS ######
  
  updateGraphSelections1 <- function() {
    if (v$TQ == F) {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsLine",
                               label = "Selected Columns",
                               choices = g$sCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsLine",
                               label = "Selected Rows",
                               choices = g$sRows)
      
      displayedColChoices <- v$mCols[!v$mCols %in% g$sCols]
      displayedRowChoices <- v$mRows[!v$mRows %in% g$sRows]
      if(length(g$sCols) >= 2 & length(g$sRows) >= 2) {
        print("enough to graph")
        createAllData(g$sCols,g$sRows)
        output$colLineGraph <- renderPlot({LinePlot(T)})
        output$rowLineGraph <- renderPlot({LinePlot(F)})
      }
    }
    else {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsLine",
                               label = "Selected Columns",
                               choices = g$sTCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsLine",
                               label = "Selected Rows",
                               choices = g$sTRows)
      displayedColChoices <- v$tMCols[!v$tMCols %in% g$sTCols]
      displayedRowChoices <- v$tMRows[!v$tMRows %in% g$sTRows]
      if(length(g$sTCols) >= 2 & length(g$sTRows) >= 2) {
        print("enough to graph")
        createAllData(g$sTCols,g$sTRows)
        output$colLineGraph <- renderPlot({LinePlot(T)})
        output$rowLineGraph <- renderPlot({LinePlot(F)})
      }
    }
    displayedColChoices <- unique(displayedColChoices)
    displayedRowChoices <- unique(displayedRowChoices)
    if (length(displayedColChoices) > 20) {displayedColChoices <- displayedColChoices[1:20]}
    if (length(displayedRowChoices) > 20) {displayedRowChoices <- displayedRowChoices[1:20]}
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cColsLine",
                             label = "Select your Columns",
                             choices = displayedColChoices)
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cRowsLine",
                             label = "Select your Rows",
                             choices = displayedRowChoices)
    print("finished updating")
  }
  
  #if any of the columns changed
  observeEvent(input$cColsLine,{
    print("col was clicked")
    if (!is.null(input$cColsLine)) {
      if (v$TQ == F) {
        clicked <- input$cColsLine[!input$cColsLine %in% g$sCols]
        g$sCols <- c(g$sCols, clicked)
      }
      else {
        clicked <- input$cColsLine[!input$cColsLine %in% g$sTCols]
        g$sTCols <- c(g$sTCols, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$cRowsLine,{
    print("One of the inputs was clicked")
    if (!is.null(input$cRowsLine)) {
      if (v$TQ == F) {
        clicked <- input$cRowsLine[!input$cRowsLine %in% g$sRows]
        g$sRows <- c(g$sRows, clicked)
      }
      else {
        clicked <- input$cRowsLine[!input$cRowsLine %in% g$sTRows]
        g$sTRows <- c(g$sTRows, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$sColsLine,{
    if (!is.null(input$sColsLine)) {
      g$sCols <- g$sCols[!g$sCols %in% input$sColsLine]
      updateAllPlots()
    }
  })
  
  observeEvent(input$sRowsLine,{
    if (!is.null(input$sRowsLine)) {
      g$sRows <- g$sRows[!g$sRows %in% input$sRowsLine]
      updateAllPlots()
    }
  })
  
  observeEvent(input$downloadButton4,{
    fileName <- paste(input$fileName4,".pdf")
    pdf(fileName)
    plot(g$line1)
    dev.off()
  })
  
  observeEvent(input$downloadButton5,{
    fileName <- paste(input$fileName5,".pdf")
    pdf(fileName)
    plot(g$line2)
    dev.off()
  })
  
  LinePlot <- function(colQ) {
    color <- input$colorInputLine
    if (colQ == T) {
      theData <- g$allData
      p<-ggplot(data=theData, aes(fill=colNames, x=rowNames, y=values)) +
        geom_line(aes(color=colNames,group = colNames))+
        geom_point(aes(color=colNames)) +
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_viridis_d(option = color)+
        scale_color_viridis_d(option = color)
      g$line1 <- p
    }
    else {
      theData <- g$allData
      p<-ggplot(data=theData, aes(fill=rowNames, x=colNames, y=values)) +
        geom_line(aes(color=rowNames,group = rowNames))+
        geom_point(aes(color=rowNames)) +
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_viridis_d(option = color) +
        scale_color_viridis_d(option = color)
      g$line2 <- p
    }
    plot(p)
    print("Plotted")
  }
  
  ###### VIOLIN STUFFS ######
  
  updateGraphSelections2 <- function() {
    if (v$TQ == F) {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsViolin",
                               label = "Selected Columns",
                               choices = g$sCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsViolin",
                               label = "Selected Rows",
                               choices = g$sRows)
      
      displayedColChoices <- v$mCols[!v$mCols %in% g$sCols]
      displayedRowChoices <- v$mRows[!v$mRows %in% g$sRows]
      if(length(g$sCols) >= 2 & length(g$sRows) >= 2) {
        print("enough to graph")
        createAllData(g$sCols,g$sRows)
        output$colViolinGraph <- renderPlot({ViolinPlot(T)})
        output$rowViolinGraph <- renderPlot({ViolinPlot(F)})
      }
    }
    else {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsViolin",
                               label = "Selected Columns",
                               choices = g$sTCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsViolin",
                               label = "Selected Rows",
                               choices = g$sTRows)
      displayedColChoices <- v$tMCols[!v$tMCols %in% g$sTCols]
      displayedRowChoices <- v$tMRows[!v$tMRows %in% g$sTRows]
      if(length(g$sTCols) >= 2 & length(g$sTRows) >= 2) {
        print("enough to graph")
        createAllData(g$sTCols,g$sTRows)
        output$colViolinGraph <- renderPlot({ViolinPlot(T)})
        output$rowViolinGraph <- renderPlot({ViolinPlot(F)})
      }
    }
    displayedColChoices <- unique(displayedColChoices)
    displayedRowChoices <- unique(displayedRowChoices)
    if (length(displayedColChoices) > 20) {displayedColChoices <- displayedColChoices[1:20]}
    if (length(displayedRowChoices) > 20) {displayedRowChoices <- displayedRowChoices[1:20]}
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cColsViolin",
                             label = "Select your Columns",
                             choices = displayedColChoices)
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cRowsViolin",
                             label = "Select your Rows",
                             choices = displayedRowChoices)
    print("finished updating")
  }
  
  #if any of the columns changed
  observeEvent(input$cColsViolin,{
    print("col was clicked")
    if (!is.null(input$cColsViolin)) {
      if (v$TQ == F) {
        clicked <- input$cColsViolin[!input$cColsViolin %in% g$sCols]
        g$sCols <- c(g$sCols, clicked)
      }
      else {
        clicked <- input$cColsViolin[!input$cColsViolin %in% g$sTCols]
        g$sTCols <- c(g$sTCols, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$cRowsViolin,{
    print("One of the inputs was clicked")
    if (!is.null(input$cRowsViolin)) {
      if (v$TQ == F) {
        clicked <- input$cRowsViolin[!input$cRowsViolin %in% g$sRows]
        g$sRows <- c(g$sRows, clicked)
      }
      else {
        clicked <- input$cRowsViolin[!input$cRowsViolin %in% g$sTRows]
        g$sTRows <- c(g$sTRows, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$sColsViolin,{
    if (!is.null(input$sColsViolin)) {
      g$sCols <- g$sCols[!g$sCols %in% input$sColsViolin]
      updateAllPlots()
    }
  })
  
  observeEvent(input$sRowsViolin,{
    if (!is.null(input$sRowsViolin)) {
      g$sRows <- g$sRows[!g$sRows %in% input$sRowsViolin]
      updateAllPlots()
    }
  })
  
  observeEvent(input$downloadButton6,{
    fileName <- paste(input$fileName6,".pdf")
    pdf(fileName)
    plot(g$violin1)
    dev.off()
  })
  
  observeEvent(input$downloadButton7,{
    fileName <- paste(input$fileName7,".pdf")
    pdf(fileName)
    plot(g$violin2)
    dev.off()
  })
  
  ViolinPlot <- function(colQ) {
    color <- input$colorInputViolin
    theData <- g$allData
    if (colQ == T) {
      p<-ggplot(data=theData, aes(x=factor(rowNames), y=values)) +
        geom_violin(aes(fill=factor(colNames)),alpha = 0.5)+
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        geom_sina(aes(fill = factor(colNames)), alpha = 0.5) +
        geom_boxplot(width=0.1, color="grey", alpha=1, aes(fill=factor(colNames)),position = position_dodge(width = 0.9)) +
        scale_fill_viridis_d(option = color)
      g$violin1 <- p
    }
    else {
      p<-ggplot(data=theData, aes( x=factor(colNames), y=values)) +
        geom_violin(aes(fill=factor(rowNames)),alpha = 0.5)+
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        geom_sina(aes(fill = factor(rowNames)), alpha = 0.5) +
        geom_boxplot(width=0.1, color="grey", alpha=1, aes(fill=factor(rowNames)),position = position_dodge(width = 0.9)) +
        scale_fill_viridis_d(option = color)
      g$violin2 <- p
    }
    plot(p)
    print("Plotted")
  }
  
  ###### HISTOGRAM STUFFS ######
  
  updateGraphSelections3 <- function() {
    if (v$TQ == F) {
      print("Updating Histogram")
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsHistogram",
                               label = "Selected Columns",
                               choices = g$sCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsHistogram",
                               label = "Selected Rows",
                               choices = g$sRows)
      displayedColChoices <- v$mCols[!v$mCols %in% g$sCols]
      displayedRowChoices <- v$mRows[!v$mRows %in% g$sRows]
      if (g$histogram == F) {
        #Row Graph
        if(!is.null(input$oneColHistogram) & length(g$sRows)>=2) {
          print("enough to graph Hist col")
          createAllData(input$oneColHistogram,g$sRows)
          print("onto writing hist col")
          output$colHistogramGraph <- renderPlot({HistogramPlot(T, g$colData)})
        }
      }
      if (g$histogram == T) {
        #Col Graph
        if (!is.null(input$oneRowHistogram) & length(g$sCols)>=2) {
          print("enough to graph Hist row")
          createAllData(g$sCols,input$oneRowHistogram)
          print("onto writing hist row")
          output$rowHistogramGraph <- renderPlot({HistogramPlot(F, g$rowData)})
        }
      }
    }
    else {
      print("Updating Histogram")
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsHistogram",
                               label = "Selected Columns",
                               choices = g$sTCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsHistogram",
                               label = "Selected Rows",
                               choices = g$sTRows)
      displayedColChoices <- v$tMCols[!v$tMCols %in% g$sTCols]
      displayedRowChoices <- v$tMRows[!v$tMRows %in% g$sTRows]
      if (g$histogram == F) {
        #Row Graph
        if(!is.null(input$oneColHistogram) & length(g$sTRows)>=2) {
          print("enough to graph Hist col")
          createAllData(input$oneColHistogram,g$sTRows)
          print("onto writing hist col")
          output$colHistogramGraph <- renderPlot({HistogramPlot(T, g$rowData)})
        }
      }
      if (g$histogram == T) {
        #Col Graph
        if (!is.null(input$oneRowHistogram) & length(g$sTCols)>=2) {
          print("enough to graph Hist row")
          createAllData(g$sTCols,input$oneRowHistogram)
          print("onto writing hist row")
          output$rowHistogramGraph <- renderPlot({HistogramPlot(F, g$colData)})
        }
      }
    }
    displayedCol <- unique(v$mCols)
    displayedRow <- unique(v$mRows)
    if (length(displayedCol) > 20) {displayedCol <- displayedCol[1:20]}
    if (length(displayedRow) > 20) {displayedRow <- displayedRowChoices[1:20]}
    if (!is.null(input$oneColHistogram)){selectedcol <- input$oneColHistogram}else{selectedcol <- character(0)}
    if (!is.null(input$oneRowHistogram)){selectedrow <- input$oneRowHistogram}else{selectedrow <- character(0)}
    updateRadioButtons(session = getDefaultReactiveDomain(), "oneColHistogram", 
                             label = "Select your Col for your Base",
                             choices = displayedCol, selected = selectedcol)
    updateRadioButtons(session = getDefaultReactiveDomain(), "oneRowHistogram", 
                             label = "Select your Row for your Base",
                             choices = displayedRow, selected = selectedrow)
    displayedColChoices <- unique(displayedColChoices)
    displayedRowChoices <- unique(displayedRowChoices)
    if (length(displayedColChoices) > 20) {displayedColChoices <- displayedColChoices[1:20]}
    if (length(displayedRowChoices) > 20) {displayedRowChoices <- displayedRowChoices[1:20]}
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "multiColsHistogram", 
                       label = "Select your Cols",
                       choices = displayedColChoices)
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "multiRowsHistogram", 
                       label = "Select your Rows",
                       choices = displayedRowChoices)
    print("finished updating")
  }
  
  #if any of the columns changed
  observeEvent(input$oneColHistogram,{
    g$histogram <- F
    updateAllPlots()
  })
  
  observeEvent(input$oneRowHistogram,{
    g$histogram <- T
    updateAllPlots()
  })
  
  observeEvent(input$multiColsHistogram,{
    print("col was clicked")
    g$histogram <- T
    if (!is.null(input$multiColsHistogram)) {
      if (v$TQ == F) {
        clicked <- input$multiColsHistogram[!input$multiColsHistogram %in% g$sCols]
        g$sCols <- c(g$sCols, clicked)
      }
      else {
        clicked <- input$multiColsHistogram[!input$multiColsHistogram %in% g$sTCols]
        g$sTCols <- c(g$sTCols, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$multiRowsHistogram,{
    g$histogram <- F
    print("One of the inputs was clicked")
    if (!is.null(input$multiRowsHistogram)) {
      if (v$TQ == F) {
        clicked <- input$multiRowsHistogram[!input$multiRowsHistogram %in% g$sRows]
        g$sRows <- c(g$sRows, clicked)
      }
      else {
        clicked <- input$multiRowsHistogram[!input$multiRowsHistogram %in% g$sTRows]
        g$sTRows <- c(g$sTRows, clicked)
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$sColsHistogram,{
    g$histogram <- F
    if (!is.null(input$sColsHistogram)) {
      if (v$TQ == F) {
        g$sCols <- g$sCols[!g$sCols %in% input$sColsHistogram]
        updateAllPlots()
      }
      else {
        g$sTCols <- g$sTCols[!g$sTCols %in% input$sColsHistogram]
        updateAllPlots()
      }
    }
  })
  
  observeEvent(input$sRowsHistogram,{
    g$histogram <- T
    if (!is.null(input$sRowsHistogram)) {
      if (v$TQ == F) {
        g$sRows <- g$sRows[!g$sRows %in% input$sRowsHistogram]
        updateAllPlots()
      }
      else {
        g$sTRows <- g$sTRows[!g$sTRows %in% input$sRowsHistogram]
        updateAllPlots()
      }
    }
  })
  
  observeEvent(input$downloadButton8,{
    fileName <- paste(input$fileName8,".pdf")
    pdf(fileName)
    plot(g$histogram1)
    dev.off()
  })
  
  observeEvent(input$downloadButton9,{
    fileName <- paste(input$fileName9,".pdf")
    pdf(fileName)
    plot(g$histogram2)
    dev.off()
  })
  
  HistogramPlot <- function(colQ, theData) {
    color <- input$colorInputHistogram
    print(theData)
    if (colQ == T) {
      p<-ggplot(data=theData, aes(x=values, fill=factor(rowNames))) +
        geom_histogram(position="identity", alpha=0.5)+
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_viridis_d(option = color)
      g$histogram1 <- p
    }
    else {
      p<-ggplot(data=theData, aes(x=values, fill=factor(colNames))) +
        geom_histogram(position="identity", alpha=0.5)+
        xlab("Rows") + ylab("Column Value") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_viridis_d(option = color)
      g$histogram2 <- p
    }
    plot(p)
    print("Plotted")
  }
  
  ###### PCA STUFFS ######
  
  updateGraphSelections4 <- function() {
    if (v$TQ == F) {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsPca",
                               label = "Selected Columns",
                               choices = g$sCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsPca",
                               label = "Selected Rows",
                               choices = g$sRows)
      
      displayedColChoices <- v$mCols[!v$mCols %in% g$sCols]
      displayedRowChoices <- v$mRows[!v$mRows %in% g$sRows]
      if(length(g$sCols) >= 2 & length(g$sRows) >= 2) {
        print("enough to pca")
        modifiedData <- v$mData[v$mRows %in% g$sRows, v$mCols %in% g$sCols]
        theCols <- v$mCols[v$mCols %in% g$sCols]
        if (length(unique(theCols)) == length(theCols)) {
          colnames(modifiedData) <- theCols
          pc <- PcaPrint(modifiedData)
          print("print")
          output$colPca <- renderPrint({PcaPrint(modifiedData)})
          print("plot")
          PcaPlot(pc,theCols, F)
          output$colPcaGraph <- renderPlot({plot(g$pca1)})
        }
        
        tModifiedData <- v$tMData[v$tMRows %in% g$sTRows, v$tMCols %in% g$sTCols]
        theCols <- v$tMCols[v$tMCols %in% g$sTCols]
        if (length(unique(theCols)) == length(theCols)) {
          colnames(tModifiedData) <- theCols
          pc <- PcaPrint(tModifiedData)
          print("print")
          output$rowPca <- renderPrint({PcaPrint(tModifiedData)})
          print("plot")
          output$rowPcaGraph <- renderPlot({PcaPlot(pc,theCols,T)})
        }
      }
    }
    else {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsPca",
                               label = "Selected Columns",
                               choices = g$sTCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsPca",
                               label = "Selected Rows",
                               choices = g$sTRows)
      displayedColChoices <- v$tMCols[!v$tMCols %in% g$sTCols]
      displayedRowChoices <- v$tMRows[!v$tMRows %in% g$sTRows]
      if(length(g$sTCols) >= 2 & length(g$sTRows) >= 2) {
        print("enough to pca")
        tModifiedData <- v$mData[v$mRows %in% g$sRows, v$mCols %in% g$sCols]
        theCols <- v$mCols[v$mCols %in% g$sCols]
        if (length(unique(theCols)) == length(theCols)) {
          colnames(tModifiedData) <- theCols
          pc <- PcaPrint(tModifiedData)
          output$rowPca <- renderPrint({PcaPrint(tModifiedData)})
          output$rowPcaGraph <- renderPlot({PcaPlot(pc,theCols,F)})
        }
        modifiedData <- v$tMData[v$tMRows %in% g$sTRows, v$tMCols %in% g$sTCols]
        theCols <- v$tMCols[v$tMCols %in% g$sTCols]
        if (length(unique(theCols)) == length(theCols)) {
          colnames(modifiedData) <- theCols
          pc <- PcaPrint(modifiedData)
          output$colPca <- renderPrint({PcaPrint(modifiedData)})
          output$colPcaGraph <- renderPlot({PcaPlot(pc,theCols,T)})
        }
      }
    }
    print("Here")
    displayedColChoices <- unique(displayedColChoices)
    displayedRowChoices <- unique(displayedRowChoices)
    if (length(displayedColChoices) > 20) {displayedColChoices <- displayedColChoices[1:20]}
    if (length(displayedRowChoices) > 20) {displayedRowChoices <- displayedRowChoices[1:20]}
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cColsPca",
                             label = "Select your Columns",
                             choices = displayedColChoices)
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cRowsPca",
                             label = "Select your Rows",
                             choices = displayedRowChoices)
    print("finished updating")
  }
  
  #if any of the columns changed
  observeEvent(input$cColsPca,{
    print("col was clicked")
    if (!is.null(input$cColsPca)) {
      if (v$TQ == F) {
        clicked <- input$cColsPca[!input$cColsPca %in% g$sCols]
        g$sCols <- c(g$sCols, clicked)
        g$sTRows <- g$sCols
      }
      else {
        clicked <- input$cColsPca[!input$cColsPca %in% g$sTCols]
        g$sTCols <- c(g$sTCols, clicked)
        g$sRows <- g$sTCols
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$cRowsPca,{
    print("One of the inputs was clicked")
    if (!is.null(input$cRowsPca)) {
      if (v$TQ == F) {
        clicked <- input$cRowsPca[!input$cRowsPca %in% g$sRows]
        g$sRows <- c(g$sRows, clicked)
        g$sTCols <- g$sRows
      }
      else {
        clicked <- input$cRowsPca[!input$cRowsPca %in% g$sTRows]
        g$sTRows <- c(g$sTRows, clicked)
        g$sCols <- g$sTRows
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$sColsPca,{
    if (!is.null(input$sColsPca)) {
      g$sCols <- g$sCols[!g$sCols %in% input$sColsPca]
      g$sTRows <- g$sCols 
      updateAllPlots()
    }
  })
  
  observeEvent(input$sRowsPca,{
    if (!is.null(input$sRowsPca)) {
      g$sRows <- g$sRows[!g$sRows %in% input$sRowsPca]
      g$sTCols <- g$sRows
      updateAllPlots()
    }
  })
  
  observeEvent(input$downloadButton10,{
    fileName <- paste(input$fileName10,".pdf")
    pdf(fileName)
    plot(g$pca1)
    dev.off()
  })
  
  observeEvent(input$downloadButton11,{
    fileName <- paste(input$fileName11,".pdf")
    pdf(fileName)
    plot(g$pca2)
    dev.off()
  })
  
  PcaPrint <- function(theData) {
    pc <- prcomp(theData)
    summary(pc)
    return(pc)
  }
  
  PcaPlot <- function (pc,rownames,TQ) {
    color <- input$colorInputPca
    p <- ggbiplot(pc,
                  obs.scale = 1,
                  var.scale = 1,
                  groups = rownames,
                  ellipse = TRUE,
                  circle = TRUE,
                  ellipse.prob = 0.68) +
      scale_fill_viridis_d(option = color) +
      theme(axis.text.x = element_text(angle = 90), legend.direction = "horizontal", legend.position = "top")
    if (TQ == F) {
      g$pca1 <- p
      print("saved")
    }
    else {
      g$pca2 <- p
      print("saved")
    }
    plot(p)
    print("plotted")
  }
  
  updateGraphSelections5 <- function() {
    if (v$TQ == F) {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsTAnova",
                               label = "Selected Columns",
                               choices = g$sCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsTAnova",
                               label = "Selected Rows",
                               choices = g$sRows)
      
      displayedColChoices <- v$mCols[!v$mCols %in% g$sCols]
      displayedRowChoices <- v$mRows[!v$mRows %in% g$sRows]
      
      if(length(g$sCols) == 2 & length(g$sRows) < 1) {
        print("Two Cols")
        runningCols <- c()
        runningValues <- c()
        for (col in g$sCols) {
          values <- v$mData[,v$mCols %in% col]
          values <- as.vector(t(values))
          runningValues <- c(runningValues,values)
          runningCols <- c(runningCols,replicate(length(values),col))
        }
        data <- cbind(runningValues,runningCols)
        colnames(data) <- c("values","colNames")
        data <- as.data.frame(data)
        data$colNames <- as.factor(data$colNames)
        data$values <- as.numeric(data$values)
        output$tTest <- renderPrint({tTestPrint(data)})
        output$anova <- renderPrint({anovaPrint(data)})
      }
      else if(length(g$sRows) == 2 & length(g$sCols) < 1) {
        print("Two Rows")
        runningCols <- c()
        runningValues <- c()
        for (row in g$sRows) {
          values <- v$mData[v$mRows %in% row,]
          values <- as.vector(t(values))
          runningValues <- c(runningValues,values)
          runningCols <- c(runningCols,replicate(length(values),row))
        }
        data <- cbind(runningValues,runningCols)
        colnames(data) <- c("values","colNames")
        data <- as.data.frame(data)
        data$colNames <- as.factor(data$colNames)
        data$values <- as.numeric(data$values)
        print("Printing started")
        output$tTest <- renderPrint({tTestPrint(data)})
        output$anova <- renderPrint({anovaPrint(data)})
        print("Two rows finished")
      }
      else if(length(g$sCols) > 2 & length(g$sRows) < 1) {
        print("multiple Cols")
        runningCols <- c()
        runningValues <- c()
        for (col in g$sCols) {
          values <- v$mData[,v$mCols %in% col]
          values <- as.vector(t(test))
          print(values)
          runningValues <- c(runningValues,values)
          runningCols <- c(runningCols,replicate(length(values),col))
        }
        data <- cbind(runningValues,runningCols)
        colnames(data) <- c("values","colNames")
        data <- as.data.frame(data)
        data$colNames <- as.factor(data$colNames)
        data$values <- as.numeric(data$values)
        output$tTest <- renderPrint({print("You have selected more than two variables, so a t-test cannot be run.")})
        output$anova <- renderPrint({anovaPrint(data)})
      }
      else if(length(g$sRows) > 2 & length(g$sCols) < 1) {
        print("multiple Rows")
        runningCols <- c()
        runningValues <- c()
        for (row in g$sRows) {
          values <- v$mData[v$mRows %in% row,]
          values <- as.vector(t(values))
          runningValues <- c(runningValues,values)
          runningCols <- c(runningCols,replicate(length(values),row))
        }
        data <- cbind(runningValues,runningCols)
        colnames(data) <- c("values","colNames")
        data <- as.data.frame(data)
        data$colNames <- as.factor(data$colNames)
        data$values <- as.numeric(data$values)
        output$tTest <- renderPrint({print("You have selected more than two variables, so a t-test cannot be run.")})
        output$anova <- renderPrint({anovaPrint(data)})
      }
      else {
        output$tTest <- renderPrint({print("You must have two or more of only columns or only rows selected.")})
        output$anova <- renderPrint({})
      }
    }
    else {
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sColsTAnova",
                               label = "Selected Columns",
                               choices = g$sTCols)
      updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "sRowsTAnova",
                               label = "Selected Rows",
                               choices = g$sTRows)
      displayedColChoices <- v$tMCols[!v$tMCols %in% g$sTCols]
      displayedRowChoices <- v$tMRows[!v$tMRows %in% g$sTRows]
      if(length(g$sTCols) >= 2 & length(g$sTRows) >= 2) {
        print("enough to TAnova")
        tModifiedData <- v$mData[v$mRows %in% g$sRows, v$mCols %in% g$sCols]
        theCols <- v$mCols[v$mCols %in% g$sCols]
        if (length(unique(theCols)) == length(theCols)) {
          colnames(tModifiedData) <- theCols
          pc <- TAnovaPrint(tModifiedData)
          output$rowTAnova <- renderPrint({TAnovaPrint(tModifiedData)})
        }
        modifiedData <- v$tMData[v$tMRows %in% g$sTRows, v$tMCols %in% g$sTCols]
        theCols <- v$tMCols[v$tMCols %in% g$sTCols]
        if (length(unique(theCols)) == length(theCols)) {
          colnames(modifiedData) <- theCols
          pc <- TAnovaPrint(modifiedData)
          output$colTAnova <- renderPrint({TAnovaPrint(modifiedData)})
        }
      }
    }
    displayedColChoices <- unique(displayedColChoices)
    displayedRowChoices <- unique(displayedRowChoices)
    if (length(displayedColChoices) > 20) {displayedColChoices <- displayedColChoices[1:20]}
    if (length(displayedRowChoices) > 20) {displayedRowChoices <- displayedRowChoices[1:20]}
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cColsTAnova",
                             label = "Select your Columns",
                             choices = displayedColChoices)
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "cRowsTAnova",
                             label = "Select your Rows",
                             choices = displayedRowChoices)
    print("finished updating")
  }
  
  #if any of the columns changed
  observeEvent(input$cColsTAnova,{
    print("col was clicked")
    if (!is.null(input$cColsTAnova)) {
      if (v$TQ == F) {
        clicked <- input$cColsTAnova[!input$cColsTAnova %in% g$sCols]
        g$sCols <- c(g$sCols, clicked)
        g$sTRows <- g$sCols
      }
      else {
        clicked <- input$cColsTAnova[!input$cColsTAnova %in% g$sTCols]
        g$sTCols <- c(g$sTCols, clicked)
        g$sRows <- g$sTCols
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$cRowsTAnova,{
    print("One of the inputs was clicked")
    if (!is.null(input$cRowsTAnova)) {
      if (v$TQ == F) {
        clicked <- input$cRowsTAnova[!input$cRowsTAnova %in% g$sRows]
        g$sRows <- c(g$sRows, clicked)
        g$sTCols <- g$sRows
      }
      else {
        clicked <- input$cRowsTAnova[!input$cRowsTAnova %in% g$sTRows]
        g$sTRows <- c(g$sTRows, clicked)
        g$sCols <- g$sTRows
      }
      updateAllPlots()
    }
  })
  
  observeEvent(input$sColsTAnova,{
    if (!is.null(input$sColsTAnova)) {
      g$sCols <- g$sCols[!g$sCols %in% input$sColsTAnova]
      g$sTRows <- g$sCols 
      updateAllPlots()
    }
  })
  
  observeEvent(input$sRowsTAnova,{
    if (!is.null(input$sRowsTAnova)) {
      g$sRows <- g$sRows[!g$sRows %in% input$sRowsTAnova]
      g$sTCols <- g$sRows
      updateAllPlots()
    }
  })
  
  tTestPrint <- function(theData) {
    tTest <- t.test(values ~ colNames, data = theData)
    print(tTest)
  }
  
  anovaPrint <- function(theData) {
    anova <- aov(values ~ colNames, data = theData)
    summary(anova)
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)
