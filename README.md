[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1036826.svg)](https://doi.org/10.5281/zenodo.1036826)



# BiodiversityObservationsMiner

This shiny app allows mining biodiversity data from Literature. I have developed Biodiversity Observations Miner with guidance and input from [W. Daniel Kissling](https://www.danielkissling.de/) and [Emiel van Loon](https://staff.fnwi.uva.nl/e.e.vanloon/) of the [Institute of Biodiversity and Ecosystems Dynamics (IBED)](http://ibed.uva.nl/) from the University of Amsterdam (UvA)

### To use it from the shiny server: 

https://fgabriel1891.shinyapps.io/biodiversityobservationsminer/  (free)

### To run locally in your computer:  
 
 1.- Clone or download the repository to a directory on your computer
 
 2.- Open the R Project container file
 
 3.- Make sure you have all R packages installed and updated to run the app (see About) 
 
 4.- Run the app through RStudio with the RunApp button. 
 
 Alternatively, you can run the `shiny::runGitHub()` function. Function arguments: `repo= "BiodiversityObservationsMiner", username= "fgabriel1891"`.
 

#### Consider this before using the app in the server or locally

For the moment I have a Starter Shiny server account. This limits the app to a couple of running hours / users per month. Until (if) I get financing to run the app in a [pro.server](http://www.shinyapps.io/)  please only use the server as a test of the app functionalities. If you want to use/mine larger portions of literature please run the app locally at your computer. 

I am open to get in touch for development / financing ideas for this app. 

--------
For comments, suggestions and bugs please open an [issue](https://github.com/fgabriel1891/BiodiversityObservationsMiner/issues/new)


### Required packages to fuction (if you run locally) 

shinythemes
DT
stringi
stringr
wordcloud
taxize
fulltext
