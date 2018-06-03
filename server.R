# 
# # This is the server logic for a Shiny web application.
# Literature readerv0.2 BIODIVERSITY OBSERVATIONS MINER
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
library(shiny)
library(DT)
library(stringi)
library(taxize)
library(fulltext)
library(tm)
library(tidyverse)
library(tidytext)
library(tibble)
library(widyr)
#library(RWeka)


# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output, session) {
  
  # Call functions script
  source("functions.R")
  
  ####### ----- ######
  ## UI order 
  ####### ----- ###### 
  
# Conditional UI's and base reactive events
  
  # Get the name of files uploaded and display it as a dropping list
  output$names <- renderUI({
    dat <- input$file1
    selectInput('articlePath', 'Select an article to read', dat$name)
  })

  
  ## Settings tab
  
  # Render the conditional input for user choices of taxonomic resolution
  output$conditionalInput <- renderUI({
    if(input$SnamesOnly == F){
      checkboxGroupInput("columns","or provide also:",
                         choices  = c("family", "class"),
                         selected = c("family", "class"))
    }
  })
  
  # Render conditional input for user choices of taxonomic database to query
  output$conditionalInput2 <- renderUI({
    if(input$SnamesOnly == F){
      radioButtons("database", "Choose taxonomic database", 
                         choices  = c("ncbi", "wikispecies (not yet implemented)"),
                         selected = c("ncbi"))
    }
  })

  
  # Reactive expression triggered after pressing the "GetTaxa" button
  read <- eventReactive(input$GoButton,{
    path <- input$file1
    getSnames(path, jSciNames = input$SnamesOnly,
              taxLevel = input$columns, 
              database = input$database)
  })
  
  

  Index = eventReactive(input$indexButton, { 

    dictio = read.csv(input$dictionary,header = TRUE, stringsAsFactors = F)
    corpusIndexText(read(), dictio)})

  
  # Reactive event triggered by the "Find Word Associations" button to find SkipGrams in the corpus text 
  
  skipGram =  eventReactive(input$SkGram,{
    i = which(Index()$file == input$articlePath2)
    findSkipGram(Index()[[i]]$text, filter = input$checkbox)})
 
  ### Mine Biodiversity observations tab
  
  ## Mine based on Scientific names subtab
  
  # Render the datatable with scientific names + taxonomic id for the "Select an species" box
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(read()$namew,rownames = F,filter = "top",
                  style = "bootstrap", selection = "single")
  })
  
  
  ## Reactive event to give context with selected rows of scientific names
  renderContext <- eventReactive(input$data_table_rows_selected,{
    rows <- input$data_table_rows_selected
    names <- read()$namew
    list <- names[,1][rows]
    cont <- lapply(read()$content, function(x) giveContext(x,list, input$up,input$down))
    reshape2::melt(cont)
    
  })
  
  # Render the datatable with context for the text snippets box
  output$context <-  DT::renderDataTable({
    DT::datatable(renderContext(), rownames = F, filter = "top",style = "bootstrap",
                  selection = "single",extensions = "Buttons",
                  options = list(dom = "Bfrtip", buttons = c( 'csv', 'pdf')))
  })
  
  ## Mine based on biodiversity events subtab: 
  
  # Get the name of files uploaded and display it as a dropping list in the SkipGram Matches box
  
  output$names2 <- renderUI({
    dat2 <- input$file1
    selectInput('articlePath2', 'Select an article to explore',dat2$name)
  })
  # Datatable with word associations in the SkipGram Matches box 
  
  output$skipGram = DT::renderDataTable({
   DT::datatable(data.frame(skipGram()[,c(1,2,7)]), rownames = F, filter = "top",
                 style = "bootstrap", selection = "single")
  })
  
  
  ## Reactive event to give context with selected rows of word associations
  renderContext2 <- eventReactive(input$skipGram_rows_selected,{
    rows <- input$skipGram_rows_selected
    list <- skipGram()[rows,]
    cont <- lapply(read()$content, function(x) giveContext2(x,list, input$up2,input$down2))
    reshape2::melt(cont)
    
  })
  
  # Render the datatable with context for the text snippets box
  output$context2 <-  DT::renderDataTable({
    DT::datatable(renderContext2(), rownames = F, filter = "top",style = "bootstrap", selection = "single", extensions = "Buttons",
                  options = list(dom = "Bfrtip", buttons = c( 'csv', 'pdf')))
  })
  
  
  
  #--------------------------------------------------------------------------
  # Index selected articles 
  
  index <- eventReactive(input$GoButton2,{
    
    #wh <- match(input$articlePath,input$file1$name)
    
    text <- read()$content#[wh]
    
    verb <- read()$verbatim#[wh]
    print("here")
    
    dictio <- read.csv(input$dictionary,header = TRUE, stringsAsFactors = F)

    readLoop <- for (i in 1:length(text)){readtext(text[[i]], dictio, verb[[i]], input$up2, input$down2) }
    print(readLoop)
  })
  
  
  
  # Wordclouds  
  
  
  output$dictionary <- renderPlot({
    # Read the dictionary
    c <- wordcloudChunk(index()$chunks)
    wordcloud::wordcloud(c,scale = c(2,.8),
                         max.words = 20, colors = brewer.pal(5,"YlOrRd"))
    
  })
  
  
  output$dictionary2 <-  DT::renderDataTable({
    # Read the dictionary
    
    aa <- index()$dic
    print(aa)
    chun<- index()$chunks
    print(chun)
   c <- termcount(aa, chun)
    DT::datatable(c,rownames = F,style = "bootstrap")
  })
  
  

# Tab to show the indexed results
  
  
  output$Indexed.version <- DT::renderDataTable({
    dat <- data.frame(index()$chunks)
    print(index()$chunks)
    DT::datatable(dat, rownames = F,style = "bootstrap", buttons = c( 'csv', 'pdf')) })
  
  
  
  # Plot showing the index positions in the article 
  
  output$plot <- renderPlot({
    #e = input$Indexed.version_rows_selected
    wh <- match(input$articlePath,input$file1$name)
    print(wh)
    text <- read()$content[wh]
    
    plot(index()$where,
         xlab = "String match rank", ylab = "Article lenght",
         ylim = c(0,nchar(text)),
         col = "#5ba966",pch = 16,
         main = "position on the text")
    legend("topright", "Position of record \n along the text",pch = 16, 
           col = "#5ba966", bty = "y" )
    #if(length(e)) points(read()$where[e , drop = FALSE], pch = 19, cex = 2)
  })
  
  
  
  ## Render article text and tables
  
  output$article <- renderText({ 
    articleRender()
  })
  
  
  
})





