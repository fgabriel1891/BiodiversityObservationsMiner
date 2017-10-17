
# Literature readerv0.2 BIODIVERSITY OBSERVATIONS MINER
# Shiny app development 
# Gabriel Muñoz 
# University of Amsterdam
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(shinythemes)
library(shinydashboard)

### Set color and similar paramenters

skin = "green" # Skin of dashboard 
title = "Biodiversity Observations Miner" # Title of APP
twidth = 400 # width of dashboard title 
width = 400 # width of dashboard

# Set paths of files
dictionary.path <- "dic/"
filenames.dic<-list.files(dictionary.path)
path2 <- paste("dic","/", filenames.dic, sep = "")
names(path2) <- filenames.dic

## Build the UI
shinyUI(
  dashboardPage(skin = skin, 
                dashboardHeader( title = title, 
                                tags$li(a(href = 'https://github.com/fgabriel1891/Ecological_Events_Miner',
                                          img(
                                              title = "Biodiversity Observations Miner", height = "100px"),
                                          style = "padding-top:2px; padding-bottom:0px;"),
                                        class = "dropdown"),
                
                                 titleWidth = 0
                                 ),
                dashboardSidebar(width = width,
                                 sidebarMenu(
                                   p(),
                                   menuItem( text = "Home",
                                            tabName = "Instr",
                                            icon = icon("home")),
                                   menuItem( text = "Upload Literature to Mine",
                                             icon = icon("floppy-open", lib = "glyphicon"),
                                             fluidPage(
                                               h4("Important!"),
                                             h5("Files must be in PDF format"),
                                            h5("Scanned versions of articles are not yet accepted "),
                                             fileInput('file1', 'Upload your article(s) here',
                                                       accept=c('.pdf'), multiple = T,
                                                       placeholder = "No file(s) selected"))),
                                   menuItem( text = "Mine Biodiversity Observations",
                                             tabName = "Mine",
                                             icon = icon("tags"),
                                   menuSubItem( text = "Mine by Scientific Names",
                                             tabName = "ScienNames",
                                             icon = icon("briefcase")),
                                   menuSubItem( text = "Mine by an Observation event",
                                             tabName = "Events",
                                             icon = icon("briefcase")),
                                   menuItem( text = "Mine by Geographical Locations",
                                             tabName = "Geog",
                                             icon = icon("briefcase"),
                                             badgeLabel = "In construction", badgeColor = "yellow")),
                                   menuItem( text = "About",
                                             tabName = "About",
                                             icon = icon("puzzle-piece"))
                                 )),
                dashboardBody( 
                  
                             
                             tabItems(
                                tabItem("Instr", 
                                        h1("Welcome to Biodiversity Observations Miner!"),
                                        h4("Biodiversity Observations Miner has been built to optimize 
                                           the discoverability of primary biodiversity data in scientific articles/gray literature."),
                                        h4("This Shiny web application provides
                                           a different experience on reading through lots of literature
                                           to retrieve primary biodiversity data"),
                                        p(),
                                        includeMarkdown("www/01-Instructions.Rmd"),
                                        p("This shiny app is currently on development and under the CreativeCommons CC BY-NC-SA License."),
                                        p("Contact Person: Gabriel Muñoz"),
                                        p(a(shiny::icon('github fa-2x'),href='https://github.com/fgabriel1891/Ecological_event_miner',target='_blank'))
                                        
                                        
                                       ),
                                
                              
                               
                               tabItem("ScienNames",
                                       h1("Mine by Scientific Names"),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       fluidRow(
                                         box(title = "Get Taxonomic Information",collapsible = T,width = 4,collapsed = T,
                                             checkboxInput("SnamesOnly", "Scientific Names Only", value = FALSE, width = NULL),
                                             uiOutput("conditionalInput"),
                                             uiOutput("conditionalInput2"),
                                             actionButton(inputId = "GoButton", 
                                                          label = "Get Taxa!")),
                                         box( title = "Set context limits:",collapsible = T, width = 4,collapsed = T,
                                              p("Lenght in characters left and right from the indexed positions"),
                                              sliderInput("up", "Left:", min = 0, max = 500,
                                                          value = 100, step= 10),
                                              sliderInput("down", "Right:", min = 0, max = 500, 
                                                          value = 100, step= 10)
                                         )),
                                      
                                       fluidRow(
                                         box( title = "Select an species",collapsible = T,
                                              DT::dataTableOutput("data_table", width = "90%")
                                         ),
                                       box(title = "Read the output",collapsible = T,
                                         DT::dataTableOutput("context"))),
                                       fluidRow(
                                       box(title = "Unformatted text",width = 12,
                                         uiOutput("names2"),
                                         actionButton("renderArticle","Render Article"),
                                         verbatimTextOutput("article"))
                                       )),
                               tabItem("Events",
                                       h1("Mine by an Observation Event"),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       fluidRow(
                                         box( title = "Biodiversity events to Index", collapsible = T,collapsed = T,
                                              uiOutput("names"),
                                              p("Explicative text"),
                                              selectInput(inputId = "dictionary",
                                                          label = "From the dropdown list",
                                                          path2),
                                              h5("or"),
                                              fileInput('file2', 'Upload your own dictionary as .csv or .txt',
                                                        accept=c('.csv', ".txt"), multiple = F,
                                                        placeholder = "No file selected"),
                                              actionButton(inputId = "GoButton2",
                                                           label = "Index!")),
                                         box( title = "Set context limits:",collapsible = T, width = 4,collapsed = T,
                                              p("Lenght in characters left and right from the indexed positions"),
                                              sliderInput("up2", "Left:", min = 0, max = 500,
                                                          value = 100, step= 10),
                                              sliderInput("down2", "Right:", min = 0, max = 500, 
                                                          value = 100, step= 10)
                                         )),
                                       
                                       fluidRow(
                                         box(title = "Text snippets",
                                             DT::dataTableOutput("Indexed.version", width = "90%")
                                         ),
                                         tabBox(title = "Plots",
                                                # The id lets us use input$tabset1 on the server to find the current tab
                                                id = "tabset1",
                                                tabPanel("Wordcloud",
                                                         plotOutput("dictionary")),
                                                tabPanel("Plot", 
                                                         plotOutput("plot")),
                                                tabPanel("Dictionary Matches",
                                                         plotOutput("dictionary2")))
                                       )
                                       ),
                               tabItem("Geog",
                                       h1("Mine by geographical locations"),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       p(),
                                       fluidPage(
                                         fluidRow( 
                                           box( title = "Index geographically",collapsible = T, width = 4,collapsed = T,
                                                textInput("ApiButton", " Type your monkeylearn API key here", ""),
                                                actionButton("submit","Submit"),
                                                actionButton(inputId = "GoButton3",
                                                             label = "Scrape Locations"))),
                                         fluidRow(
                                                  tabsetPanel(
                                                    tabPanel("Map",
                                                             leafletOutput("map")),
                                                    tabPanel("Locations Mined",
                                                             DT::dataTableOutput("LocationsText")))
                                         )
                                       )
                                       )
                             
                             ))
))
  

