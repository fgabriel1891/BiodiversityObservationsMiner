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
library(tibble)
library(udpipe)
#library(RWeka)


# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)

# The server side of this web application is composed of eight outputs and five reactive events.

# Reactive Events: 

# read(): this event gets triggered after a user presses the "Get Taxa" button. This event will read the path to the files uploaded with the "Upload" tab and pass it to the getSnames() function. 
# Index(): this event is triggered by the "Index" button. This event will read the selected biodiversity dictionary (as .csv file) and pass the object to the corpusIndexText() function 
# skipGram(): this event occurs after pressing the "Find word associations" button. It looks for a match between the selected article from the drop-down menu with the results of the Index() event. This is passed to the findSkipGram() function. 
# renderContext(): This reactive event is dependent on the user row selection to the data.table rendered as a partial result in the "Mine by Scientific Names" tab. The scientific name of the selected row is passed to the giveContext() function.
# renderContext2(): This does of the same as renderContext(), however, this event is triggered by the row selection of the data.table with the word associations rendered in the "Match with observation events" tab.

# Outputs: 

# names: This output will read the filenames of the uploaded articles and display them as a dropping list with the selectInput() widget.
# conditionalInput: Given that the result from input$SnamesOnly is FALSE, this conditional input will render additional user choices as checkboxes for the taxonomic classification of scientific names found.
# conditionalInput2: Given that the result from input$SnamesOnly is FALSE, this conditional input will render additional user choices as checkboxes for databases to query
# data_table: This will render a data.table with the scientific names found (plus additional taxonomic classifications) inside the "Select a species" box.
# context: This will render a data.table with the indexed text snippets by scientific names that match the row selection of the "Text snippets" box from the "Mine by scientific names" tab.
# names2: Similarly as names, this will display the filenames of articles uploaded as a dropping list.
# skipGram: This output will render a data.table with the results of the word associations found resulting of the skipGram() reactive event. The data.table will show the pairs of words found to be associated and the normalized probability of finding those pairs of words together in the corpus of indexed snippets result of the Index() event.
# context2: This, as context, will render a data.table with the indexed snippets of text that matches words from the row selection of the skipGram data.table.
# 
# 

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
  
  annotateEN = reactive({
    withProgress( value = 0.1 , message = "Annotating Text", { 
    english_model = udpipe_load_model("www/english-ud-2.0-170801.udpipe")
    x = reshape2::melt(read()$content)
    x = udpipe_annotate(english_model,
                          x = x$value,
                        doc_id = x$L1)
    incProgress(amount = 0.5, message = "Done!")
     as.data.frame(x) })
    
  })

  Index = eventReactive(input$indexButton, { 

    dictio = read.csv(input$dictionary,
                      header = TRUE, 
                      stringsAsFactors = F)
    corpusIndexText(read(), dictio)})


  
  
  ## Reactive event to give context with selected rows of scientific names
  renderContext <- eventReactive(input$data_table_rows_selected,{
    rows <- input$data_table_rows_selected
    names <- read()$namew
    list <- names[,1][rows]
    which = grep(list, annotateEN()$sentence)
    annotateEN()[which,]
  })
  
  
  

  
  # Reactive event triggered by the "Find Word Associations" button 
  
  skipGram =  eventReactive(input$SkGram,{
   
    getCoOcu(annotateEN(), input$dictionary, filter = input$byDic)
    })
  
  # reactive event triggered by the "infer context" button i
  
  skipGram2 =  eventReactive(input$SkGram2,{
    getCoOcu2(renderContext())
  })
  
  
 
  ### Mine Biodiversity observations tab
  
  ## Mine based on Scientific names subtab
  
  # Render the datatable with scientific names + taxonomic id for the "Select an species" box
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(read()$namew,
                  rownames = F,
                  filter = "top",
                  style = "bootstrap",
                  selection = "single")
  })
  

  
  # Render the datatable with context for the text snippets box
  output$context <-  DT::renderDataTable({
    DT::datatable(unique(renderContext()[c("doc_id","sentence")]), 
                  rownames = F,
                  filter = "top",
                  style = "bootstrap",
                  selection = "single",
                  extensions = "Buttons",
                  options = list(dom = "Bfrtip", 
                                 buttons = c( 'csv', 'pdf')))
  })
  
  ## Mine based on biodiversity events subtab: 
  
  # Get the name of files uploaded and display it as a dropping list in the SkipGram Matches box
  
  output$names2 <- renderUI({
    dat2 <- input$file1
    selectInput('articlePath2', 
                'Select an article to explore',
                dat2$name)
  })
  
  # Render ui if checkbox to filter by dictionary terms is selected 
  
  output$byDic2 = renderUI({
    if(input$byDic == F){ }
    else{
    selectInput(inputId = "dictionary",
                label = "Select a dictionary",
                path2)
      }
  })
  
  # button to filter
  
  output$filterBut = renderUI({
    if(input$byDic == F){ }
    else{
  actionButton("Filter", 
               label = tags$b("Filter terms"),
               icon("mouse-pointer"), 
               style="color: #fff; 
               background-color: #337ab7;
               border-color: #2e6da4")}
    })
  # Datatable with word associations in the SkipGram Matches box 
  
  output$skipGram = DT::renderDataTable({
   DT::datatable(data.frame(skipGram()), 
                 rownames = F, 
                 filter = "top",
                 style = "bootstrap", 
                 selection = "single")
  })
  
  
  ## Reactive event to give context with selected rows of word associations
  renderContext2 <- eventReactive(input$skipGram_rows_selected,{
    rows <- input$skipGram_rows_selected
    list <- skipGram()[rows,]
    list = paste0(list$term1, ".*", list$term2)
    which = grep(list, annotateEN()$sentence)
    annotateEN()[which,]
  })
  
  # Render the datatable with context for the text snippets box
  output$context2 <-  DT::renderDataTable({
    DT::datatable(unique(renderContext2()[c("doc_id","sentence")]), 
                  rownames = F,
                  filter = "top",
                  style = "bootstrap", 
                  selection = "single",
                  extensions = "Buttons",
                  options = list(dom = "Bfrtip", 
                                 buttons = c( 'csv')))
  })
  

  output$contextTableSp <-  DT::renderDataTable({
    DT::datatable(data.frame(skipGram2()), 
                  rownames = F,
                  filter = "top",
                  style = "bootstrap", 
                  selection = "single")
  })
  
  
  
  #--------------------------------------------------------------------------
  # Index selected articles 
  
  index <- eventReactive(input$GoButton2,{
    
    #wh <- match(input$articlePath,input$file1$name)
    
    text <- read()$content#[wh]
    
    verb <- read()$verbatim#[wh]
    print("here")
    
    dictio <- read.csv(input$dictionary,header = TRUE, stringsAsFactors = F)

    readLoop <- for (i in 1:length(text)){readtext(text[[i]],
                                                   dictio, verb[[i]],
                                                   input$up2, input$down2) }
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
    DT::datatable(dat, rownames = F,
                  style = "bootstrap",
                  buttons = c( 'csv', 'pdf')) })
  
  
  
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
  
  
  ## render attention call 
  
  output$attention <- renderText({
    read()$mess
  })
  
  
  output$attention2 <- renderText({
    read()$mess
  })
  
  ## Render article text and tables
  
  output$article <- renderText({ 
    articleRender()
  })
  
  
  
})





