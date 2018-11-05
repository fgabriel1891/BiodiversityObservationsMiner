

# Define functions: 
#

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
    
    
  }
  matches <- c()
  ToMatch <- c()
  for ( i in 1:length(IndexText)){
    matches[[i]] <- grepl(paste(dictionary[,1], collapse = "|"),IndexText[i])
  }
  
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



# Get Scientific and family names function

getSnames <- function(path, jSciNames = F, taxLevel, database ){
  
  print("initiated")
  # Initiate progress bar
  withProgress( value = 0.1 ,
                message = "Finding Taxonomic Entities", { 
    # Set an apply function to extract the scientific names of the articles uploaded. 
    names = path$name
    path = path$datapath
    scrapeRes <- lapply(path, function(x)
      tryCatch({scrapenames(file = x,
                            return_content = T)}, 
               error = function(e) NULL ))
    content <- lapply(scrapeRes, function(x) 
      x$meta$content) # return the text content
    names(content) <- names
    verbatim <- lapply(scrapeRes, function(x) x$data) # return the scientific names found 
    names(verbatim) <- names # give proper names
    namew <- lapply(verbatim, function(x) unique(x$scientificname)) # list scientific names 
    goodArt =sapply(1:length(namew), function(x) length(namew[[x]]))
    goodArt2 = which(goodArt > 0)
    namew <- reshape2::melt(namew[goodArt2]) # arrage dataset with file
    names(namew) <- c("species", "file") # give proper names
    ret <- list()
    ret$content <- content
    ret$verbatim <- verbatim
    ret$file <- names
    ret$namew <- namew
    
    if (jSciNames == T) { 
      incProgress(amount = 0.9,
                  message = "Done!")}
    
    else  {  
      # Create unique pool of names to identify (Save computation time)
      taxaFound <- unique(namew$species)
      #incProgress(amount = 0.4, message = "Done!" )
      incProgress(amount = 0.2,
                  message = "Finding taxonomic 
                  ontologies recognized taxonomic entities" ) # update progress bar
      print(database)
      # Identify family and class names
      families <- taxize::tax_name(taxaFound, get = taxLevel,
                                   db = database, verbose = F,messages = F,
                                   ask = F) # Look for families
      families <- cbind(taxaFound, families)
      out <- c("query","db")
      families2 <- families[,!(names(families) %in% out)]
      ret$namew <-NULL
      ret$namew <- families2
      incProgress(amount = 0.5, 
                  
                  message = "Done!"
      )
      
     
      ret$families <- families}
    
    ret$mess = if(length(which(goodArt <= 0)!= 1)){
      paste("Attention, no taxa found in", 
            length(which(goodArt <= 0)), "file(s):",
            "\n",
            reshape2::melt(names[which(goodArt <= 0)]), 
            "Please manually revise such file(s)")
    } else { "All files were correctly mined"}
    
    return(ret)
  }) 
} 



## FindSkipGrams and normalized probability between pair of words (option to filter with dictionary matches)

findSkipGram = function(indexText, filter = T){
  test2 = as.tibble(indexText)
  print(test2)
  names(test2) = "text"
  
  probsT2 = unigramProbs = test2 %>% 
    unnest_tokens(word, text) %>% 
    count(word, sort = TRUE) %>%
    mutate(p = n / sum(n)) 
  # Remove stopwords
  probsT2 = probsT2 %>%
    anti_join(stop_words, by = "word")
  
  # Calculate skipgrams with moving window
  
  tidy_skipgrams <- test2 %>%
    unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
    mutate(ngramID = row_number()) %>% 
    unite(skipgramID, ngramID) %>%
    unnest_tokens(word, ngram) %>%
    anti_join(stop_words, by = "word")
  # Calculate probabilities that pair of words occur together
  skipgram_probs <- tidy_skipgrams %>%
    pairwise_count(word, skipgramID, 
                   diag = TRUE, 
                   sort = TRUE) %>%
    mutate(p = n / sum(n))
  
  
  ## Normailized probability 
  
  normalized_prob <- skipgram_probs %>%
    filter(n > 20) %>%
    rename(word1 = item1, word2 = item2) %>%
    left_join(probsT2 %>%
                select(word1 = word, p1 = p),
              by = "word1") %>%
    left_join(probsT2 %>%
                select(word2 = word, p2 = p),
              by = "word2") %>%
    mutate(p_together = p / p1 / p2)
  
  print(normalized_prob)
  print(which(normalized_prob$word1 == normalized_prob$word2))
  resWord = normalized_prob[-which(normalized_prob$word1 == normalized_prob$word2),]
  resWord = resWord[order(resWord$p_together, decreasing = T),]
  if (filter == T) {resWord = resWord = resWord[grep(regex,resWord$word1 ),]}
  else{resWord = resWord}
  
  return(resWord)
}


# function to create coorcurrences 

getCoOcu = function(x, dictio, filter = F){
  

  
  stats <- cooccurrence(x = x$lemma, 
                        relevant = x$upos %in% c("NOUN", "VERB","ADJ"), 
                        skipgram = 8)

if (filter == T) {
  dictio = read.csv(dictio,
                    header = TRUE, 
                    stringsAsFactors = F)
  
  match1  = unlist(sapply(dictio$dictionary, 
                          function(x)grep(x,stats$term1)))
  match2 = unlist(sapply(dictio$dictionary, 
                         function(x)grep(x,stats$term2)))
  match = unique(c(match1, match2))
  print(match2)
  print(match)
  stats = stats[match,]
}
else{stats = stats}

  stats[order(stats$cooc,stats$term1, decreasing = T),]
}

getCoOcu2 = function(x){
  stats <- cooccurrence(x = x$lemma, 
                        relevant = x$upos %in% c("NOUN", "VERB","ADJ"),
                        skipgram = 3)

  stats[order(stats$cooc,stats$term1, decreasing = T),]
}

# Function to filter datatables based on a dictionary of terms
 




# Funtion to index text content in a loop and create corpus 

corpusIndexText = function(names, regex){
  
  # create corpus 
  TextCorpus = sapply(names$content, function(x) "text" = x)
  Corp = as_tibble(TextCorpus)
  names(Corp) = "text" ## Important! 
  verbatim =  sapply(1:length(names$verbatim), function(x) "verbatim" = names$verbatim[x])
  print(verbatim)
  ## Loop to index scientific names and dictionary matches and find snippets
  indexText = c()
  for (i in 1: length(Corp)){
    print(i)
    indexText[[i]] = tryCatch({Index(read = Corp[i], verbatim = verbatim[[i]], regex )}, 
                              error = function(e) print(paste("There was an Error at" ,i)))
  }
  indexText$file = names$file
  return(indexText)
  
}
# create corpus 
# TextCorpus = sapply(re$content, function(x) "text" = x)
# Corp = as_tibble(TextCorpus)
# verbatim =  sapply(1:length(re$verbatim), function(x) "verbatim" = re$verbatim[x])
# str(verbatim)
# Index(read = Corp[i], verbatim = verbatim, dic )
# verbatim
# re$verbatim
# rrr = taxize::scrapenames(file = "www/frugivory/OFarrill_et_al_Tapirs.pdf", return_content = T)
# sdas = Index(read = Corp[i], verbatim = verbatim, dic )
# sdas$file = "tapirs"
# findSkipGram(sdas)
# re = rrr$meta$content
# re$verbatim = rrr$data
# names(re) = c("content", "verbatim")
# Function to retrieve the snippets of corpus text matching the desired terms 

giveContext2 <- function(text,skipGram, up, down) {
  word2 = skipGram$word2
  indx <- unlist(gregexpr(word2, text))
  cont = sapply(indx, function(x) stringr::str_sub(text, x-up,x+down))
  word1 = skipGram$word1
  names(cont) <- names(text) 
  return(cont[grep(word1, cont)])}


makeTextNetwork = function(words){
  # Create labels from dataframe
  labels = c(unique(as.character(words$w1)),
             unique(as.character(words$w2)))
  # Create nodes from dataframe
  nodes = data.frame("label" = labels , 
                     "id" = 1:length(labels))
  
  # Create edges (interactions) by matching the dataframe with the nodelist
  edges = data.frame("from" = match(words$w1, labels),
                     "to" = match(words$w2, labels),
                     "width" = words$fre)
  # Make network object
  netDat = list("nodes" = nodes,
                "edges" = edges)
  return(netDat)
  
}




## End of functions 


getBackText = function(rows, text, skipGram){
  rows = as.numeric(rows)
  word2 = grep(skipGram$word2[rows], text)
  word1 = grep(skipGram$word1[rows], text)
  return(Corpus[word2[na.omit(match(word1, word2))],])
  
}
# Custom function to retrieve dictionary matches
#
termcount <- function(dictionary, text){
  
  count <-c()
  for (i in 1:length(unlist(dictionary))){
    print(dictionary[i,])
    count$term[i] <- dictionary[i,]
    count$count[i] <- length(grep(dictionary[i,], text))
  }
  
  b <- data.frame(count)
  b <-  b[b$count != 0,]
  return(b)
  
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


#getSnames(path = "www/eiserhardt2011.pdf")
#sasa <- getSnames(c("02 David, Manakadan & Ganesh.pdf","1-s2.0-S037663571500056X-main.pdf"))



# Function to split text based on positions                    
splitAt <- function(x, pos){ unname(split(x, cumsum(seq_along(x) %in% pos)))} # from:https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position

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


sasa = c("asvssffggerasmss")

gregexpr("ms",sasa[])
unlist(gregexpr("as", sasa))


# 
# 
# # Geocode scrapped locations 
# 
# geocoder <-  function(out1){ 
#   withProgress( value = 0.5 , message = "Geocoding Locations", { 
#     
#     fram <- data.frame(out1())
#     fram <- fram[fram$tag == "LOCATION",] # Get only location names
#     fram <- fram[,c(1,3)]
#     names(fram) <- c("Count","Location")
#     
#     toGeoCode <- unique(fram$Location)
#     print(toGeoCode)
#     tagText <- readLocs(read()$article,toGeoCode, 100)
#     names(tagText) <- toGeoCode
#     dfram <- reshape2::melt(tagText)
#     
#     
#     geocode <- ggmap::geocode(toGeoCode,messaging = F)
#     geocode$Location <- toGeoCode
#     
#     dfram$lat <- geocode$lat[match(dfram$L1,geocode$Location)]
#     dfram$lon <- geocode$lon[match(dfram$L1,geocode$Location)]
#     
#     return(dfram)})
#   
# }