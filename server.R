# 
# # This is the server logic for a Shiny web application.
# Literature readerv0.2 BIODIVERSITY OBSERVATIONS MINER
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
library(shiny)
library(monkeylearn)
library(shinythemes)
library(DT)
library(stringi)
library(wordcloud)
library(taxize)
library(fulltext)
library(tm)
#library(RWeka)


# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output, session) {
  
  
  
  # Define functions: 
  #
  # Custom function to retrieve dictionary matches
  #
  termcount <- function(dictionary, text){
    
    count <-c()
    for (i in 1:length(unlist(dictionary))){
      count$term[i] <- dictionary[i,]
      count$count[i] <- length(grep(dictionary[i,], text))
    }
    
    b <- data.frame(count)
    return(b[order(b$count, decreasing = T),]
    )
  }
  
  # Function to create a cropus from the indexed snippets
  
  wordcloudChunk <- function( chunks){ 
    
    
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) # from https://rstudio-pubs-static.s3.amazonaws.com/118348_a00ba585d2314b3c937d6acd4f4698b0.html
    corpus <- tm::Corpus(tm::VectorSource(chunks))
    corpus <- tm::tm_map(corpus, removeWords, stopwords('english'))
    
    #dtm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
    #freq <- sort(rowSums(as.matrix(dtm)),decreasing = TRUE)
    #freq <- data.frame("terms"=names(freq), "freq"=freq)
    return(corpus)
  }
  
  
  #
  # Custom function to index
  #
  
  #return( print(IndexText[which(matches[[i]] == TRUE)]))
  # 
  # test <- scrapenames(file = "www/Frugivory and seed dispersal by tapirs: an insight on their ecological role .pdf", return_content = T)
  # txt <- test$meta$content
  # vr <- test$data
  # Index(txt,vr, dictionary)
  
  Index <- function(read,verbatim, dictionary) {
    
    IndexText <- list()
    for(i in 1:length(verbatim$offsetend)){
      IndexText[i] <- stringr::str_sub(read,
                                       verbatim$offsetstart[i]-100,
                                       verbatim$offsetend[i]+150)
      
    print("here2")
    }
    matches <- c()
    ToMatch <- c()
    for ( i in 1:length(IndexText)){
      matches[[i]] <- grepl(paste(dictionary[,1], collapse = "|"),IndexText[i])
    }
    print("here3")
    df <- c()
    df$text <- unlist(IndexText[matches==TRUE])
    df$where <- verbatim$offsetstart[which(matches == TRUE)]
    
    
    return(df)
  }
  
  # Function to index the ocr text and give context. 
  
  giveContext <- function(text,terms, up, down) {
    indx <- unlist(gregexpr(terms, text))
    cont = sapply(indx, function(x) stringr::str_sub(text, x-up,x+down))
    names(cont) <- names(text) 
    return(cont)}
  
  
  
  # Function to split text based on positions                    
  splitAt <- function(x, pos){ unname(split(x, cumsum(seq_along(x) %in% pos)))} # from:https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
  
  # Get Scientific and family names function
  
  getSnames <- function(path, jSciNames = F, taxLevel, database){
    
    print("initiated")
    # Initiate progress bar
    withProgress( value = 0.1 , message = "Scrapping Scientific Names", { 
      # Set an apply function to extract the scientific names of the articles uploaded. 
      scrapeRes <- lapply(path, function(x) tryCatch({scrapenames(file = x, return_content = T)}, error = function(e) NULL ))
      file <- input$file1
      content <- lapply(scrapeRes, function(x) x$meta$content) # return the text content
      names(content) <- file$name
      
      verbatim <- lapply(scrapeRes, function(x) x$data) # return the scientific names found 
      names(verbatim) <- file$name # give proper names
      namew <- lapply(verbatim, function(x) unique(x$scientificname)) # list scientific names 
      namew <- reshape2::melt(namew) # arrage dataset with file
      names(namew) <- c("species", "file") # give proper names
      ret <- list()
      ret$content <- content
      ret$verbatim <- verbatim
      ret$file <- file$name
      ret$namew <- namew
      
      if (jSciNames == T) { 
        incProgress(amount = 0.9, message = "Done!")}
      
      else  {  
        # Create unique pool of names to identify (Save computation time)
        spfound <- unique(namew$species)
      #incProgress(amount = 0.4, message = "Done!" )
      incProgress(amount = 0.2,
                  message = "Finding taxonomic 
                  ontologies of scientific names found" ) # update progress bar
     
      # Identify family and class names
      families <- taxize::tax_name(spfound, get = taxLevel, 
                                   db = database, verbose = F,
                                   ask = F) # Look for families
      families <- cbind(spfound, families)
      out <- c("query","db")
      families2 <- families[,!(names(families) %in% out)]
      ret$namew <-NULL
      ret$namew <- families2
      incProgress(amount = 0.5, message = "Done!")
      
      ret$families <- families}
      
      
      return(ret)
    }) 
  } 
  
  #getSnames(path = "www/eiserhardt2011.pdf")
  #sasa <- getSnames(c("02 David, Manakadan & Ganesh.pdf","1-s2.0-S037663571500056X-main.pdf"))
  
  # Custom function to scrapenames, OCR the pdf and return a list of taxonomic entities found (including its identification at family and class) 
  # along with a list of "snippets" of the text of ~600 long that matches both the dictionary terms and the target species of the search. 
  # Requires: a dictionary, a filter and the path where the article (pdf) is stored. This function calls the "Index" function and passes the arguments
  # dictionary and verbatim = list of scientific names found with the taxize::scrapenames function.  
  
  readtext <- function(text, dictionary,verbatim, up, down){
    readed <- Index(text, verbatim, dictionary)
    # Create chunks to summarize text output
    nam <- readed$where
    ww <- lapply(data.frame(nam), diff) # Calculate differences between matches
    bp <- as.numeric(which(unlist(ww) > 250)) # Select breakpoints based on a difference threshold (large of snippets)
    # create chunks 
    lmis <- splitAt(nam,bp+1)
    # Set limits
    lims <- lapply(lmis, function(x) c(min(x)-down, max(x)+up))
    # retrieve text
    chunks <- lapply(lims, function(x) stringr::str_sub(text, x[1],x[2]))
    # create a return object 
    retorne <- list()
    retorne$dic <- dictionary
    retorne$where <- nam
    retorne$chunks <- unlist(chunks)
    return(retorne) 
  }
  
  
  
  
  # Function to get frequency counts of scientific names found 
  
  getFrecuencyNames <-  function(namelist){ 
    sciname <- namelist
    splist <- table(sciname$species)[order(table(sciname$species), decreasing = T)]
    splist <- data.frame(splist)
    names(splist) <- c("species", "count")
    splist$family <- namelist$family[match(splist$species, namelist$species)]
    splist$class <- namelist$class[match(splist$species, namelist$species)]
    return(splist)
  }
  
  # Scrape locations with monkeylearn 
  
  locScrap <- function(article, key){
    print("starting")
    withProgress( value = 0.5 , message = "Scrapping Locations", { 
      scrap <- list()        
      scrap <- monkeylearn::monkeylearn_extract(article,
                                                extractor_id = "ex_isnnZRbS", 
                                                key=key, verbose = T)
      print(scrap)
      locs <- scrap$found[scrap$found$tag == "LOCATION"]
      return(scrap)})
   
  }
  
  # Get the Key imput 
  
  key <- eventReactive(input$submit,{ 
      df <- input$ApiButton
      df
    })

  
  ## Function to read locations
  
  
  readLocs <- function(txt, locs, len){
    rre <- lapply(locs,function(x) unlist(gregexpr(x, txt)))
    len <- len
    lapply(rre, function(x) ifelse( x > len, stringr::str_sub(txt,x-len,x+len),
                                    stringr::str_sub(txt,x,x+len )))
    
  }
  
  
  ####### ----- ######
  ## UI order 
  ####### ----- ###### 
  
  
  ## FIRST TAB 
  
  # Get the name of files uploaded and display it as a dropping list
  output$names <- renderUI({
    dat <- input$file1
    selectInput('articlePath', 'Select an article to read', dat$name)
  })
  
  # Get the name of files uploaded and display it as a dropping list in the third tab
  output$names2 <- renderUI({
    dat2 <- input$file1
    selectInput('articlePath2', 'Select an article to read',dat2$name)
  })
  
  # Reactive expression after pressing "go" button
  read <- eventReactive(input$GoButton,{
    path <- input$file1$datapath
    getSnames(path, jSciNames = input$SnamesOnly,
              taxLevel = input$columns, 
              database = input$database)
    
    
  })
  
  # Geocode scrapped locations 
  
  geocoder <-  function(out1){ 
    withProgress( value = 0.5 , message = "Geocoding Locations", { 
      
      fram <- data.frame(out1())
      fram <- fram[fram$tag == "LOCATION",] # Get only location names
      fram <- fram[,c(1,3)]
      names(fram) <- c("Count","Location")
      
      toGeoCode <- unique(fram$Location)
      print(toGeoCode)
      tagText <- readLocs(read()$article,toGeoCode, 100)
      names(tagText) <- toGeoCode
      dfram <- reshape2::melt(tagText)
      
      
      geocode <- ggmap::geocode(toGeoCode,messaging = F)
      geocode$Location <- toGeoCode
      
      dfram$lat <- geocode$lat[match(dfram$L1,geocode$Location)]
      dfram$lon <- geocode$lon[match(dfram$L1,geocode$Location)]
      
      return(dfram)})
    
  }
  ### SECOND TAB 
  
  ## Taxonomic overview: 
  
  # Tab with Scientific Names found and count
  # 
  # 
  # dictio <- read.csv(input$dictionary, header = TRUE, stringsAsFactors = F)
  # path <- input$file1$datapath[match( input$articlePath,input$file1$name)]
  
  output$data_table <- DT::renderDataTable({
    
    DT::datatable(read()$namew,rownames = F,filter = "top")
    
  })
  
  ## Reactive event to give context with selected rows of names
  renderContext <- eventReactive(input$data_table_rows_selected,{
    rows <- input$data_table_rows_selected
    names <- read()$namew
    list <- names[,1][rows]
    cont <- lapply(read()$content, function(x) giveContext(x,list, input$up,input$down))
    reshape2::melt(cont)
    
  })
  
  output$context <-  DT::renderDataTable({
    DT::datatable(renderContext(), rownames = F, filter = "top")
  })
  
  #######
  
  # THIRD TAB
  # Ecological Events Overview
  
  # Index selected articles 
  
  index <- eventReactive(input$GoButton2,{
    
    wh <- match(input$articlePath,input$file1$name)
    
    text <- read()$content[wh]
    
    verb <- read()$verbatim[wh]
    
    dictio <- read.csv(input$dictionary,header = TRUE, stringsAsFactors = F)

    readtext(text[[1]], dictio, verb[[1]], input$up2, input$down2)  
    
  })
  
  output$conditionalInput <- renderUI({
    if(input$SnamesOnly == F){
      checkboxGroupInput("columns", "Choose taxonomic level:", 
                         choices  = c("family", "class"),
                         selected = c("family", "class"))
    }
  })
  
  output$conditionalInput2 <- renderUI({
    if(input$SnamesOnly == F){
      checkboxGroupInput("database", "Choose taxonomic database", 
                         choices  = c("ncbi", "wikipedia"),
                         selected = c("ncbi"))
    }
  })
  
  # Render Article (uselful to copy tables)
  
  articleRender <- eventReactive(input$renderArticle, { 
    path <- input$file1$datapath[match(input$articlePath2,input$file1$name)]
    print(path)
                                       
    pdftools::pdf_text(path)
  })
  
  # Wordclouds  
  
  
  output$dictionary <- renderPlot({
    # Read the dictionary
    c <- wordcloudChunk(index()$chunks)
    wordcloud::wordcloud(c,scale = c(2,.8),
                         max.words = 20, colors = brewer.pal(5,"YlOrRd"))
    
  })
  
  
  output$dictionary2 <- renderPlot({
    # Read the dictionary
    c <- termcount(index()$dic,read()$chunks)
    c <- c[c$count != 0,]
    print(c)
    wordcloud::wordcloud(c$term, c$count,scale=c(2,.8), min.freq = 1,
                         max.words = 10, colors = brewer.pal(5,"YlOrRd"))
    
  })
  
  
  # Tab to show the indexed results
  
  
  output$Indexed.version <- DT::renderDataTable({
    dat <- data.frame(index()$chunks)
    print(index()$chunks)
    DT::datatable(dat, rownames = F) })
  
  
  
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
  
  
  # Reactive to "Get Locations" button 
  
  out1 <-  eventReactive(input$GoButton3,{
    print("here locs")
    locs <- locScrap(read()$article, key())
    geocoder(locs)})
  
  
  
  
  output$LocationsText <- DT::renderDataTable({
    DT::datatable(out1(), rownames = F)
  })
  
 
  
  output$map <- renderLeaflet({
    na.omit(out1()) %>%
    leaflet() %>% 
   addProviderTiles("OpenStreetMap.Mapnik") %>%
      addCircleMarkers(~lon,
                       ~lat,
                       popup = ~value, 
                       fillOpacity=0.8,
                       clusterOptions = markerClusterOptions()) #addTiles() %>% 
       
  })
  
})


# key = "cf0b9da7695ba68256cd61ee7fe04cbf84ae4ede"
# 
# monkeylearn_extract("Brazil es la fuente de la mayor 
#                         conatidad de estudios en agronomía,
#                         biologia y matemacticas en america latina",
#                     extractor_id = "ex_isnnZRbS", 
#                     verbose = T)

