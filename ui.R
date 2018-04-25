
# Literature readerv0.2 BIODIVERSITY OBSERVATIONS MINER
# Shiny app development 
# Gabriel Muñoz 
# University of Amsterdam
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(shinydashboard)

### Set color and similar paramenters

skin = "black" # Skin of dashboard 
title = "Biodiversity Observations Miner" # Title of APP
twidth = 400 # width of dashboard title 
width = 300 # width of dashboard

# Set paths of files
dictionary.path <- "dic/"
filenames.dic<-list.files(dictionary.path)
path2 <- paste("dic","/", filenames.dic, sep = "")
names(path2) <- filenames.dic

## Build the UI
shinyUI(
  dashboardPage(skin = skin, 
                dashboardHeader( title = title, 
                                 titleWidth = 300
                                 ),
                dashboardSidebar(width = width,
                                 sidebarMenu(
                                   p(),
                                   menuItem( text = "Home",
                                            tabName = "Instr",
                                            icon = icon("home")),
                                   menuItem( text = "Upload Files",
                                             tabName = "Set",
                                             icon = icon("floppy-open", lib = "glyphicon")
                                             ),
                                   menuItem( text = "Mine Biodiversity Observations",
                                             tabName = "Mine",
                                             icon = icon("binoculars"),
                                   menuSubItem( text = "Mine Scientific names",
                                             tabName = "ScienNames",
                                             icon = icon("envira")),
                                   menuSubItem( text = "Match with biodiversity dictionaries",
                                             tabName = "Events",
                                             icon = icon("puzzle-piece"))),
                                   # menuItem( text = "Mine by Geographical Locations",
                                   #           tabName = "Geog",
                                   #           icon = icon("briefcase"),
                                   #           badgeLabel = "In construction", badgeColor = "yellow")),
                                   menuItem( text = "About",
                                             tabName = "AboutBranch",
                                             icon = icon("folder-open"),
                                             menuSubItem( text = "How does it work?",
                                                          tabName = "About",
                                                          icon = icon("cogs")),
                                             menuSubItem( text = "Report a bug",
                                                          tabName = "bug",
                                                          icon = icon("bug")),
                                             menuSubItem(text = "Source Code",
                                                         tabName = "code",
                                                         icon = icon("code"))),
                                   menuItem( text = "Contact",
                                             tabName = "contact",
                                             icon = icon("id-card"))
                                 )),
                dashboardBody( 
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                  
                             
                             tabItems(
                               tabItem("Set",
                                       fluidRow(
                                         box(title = "Upload you article(s) to mine",
                                             width = 12 ,status = "success", solidHeader = TRUE,
                                              fileInput('file1', 'Upload your article(s) here',
                                                   accept=c('.pdf'), multiple = T,
                                                   placeholder = "No file(s) selected"),
                                             fluidPage(
                                               h4("Important!"),
                                               h5("Files must be in PDF format"), 
                                               p( "It is recomended to name your files appropiately"),
                                               p( "(e.g. SomeAuthor_et_al_2018.pdf)"),
                                               h5("Scanned versions of articles are not yet accepted "))))
                                      ),
                                            
                                tabItem("Instr", 
                                        h1("Welcome to Biodiversity Observations Miner!"),
                                        p(),
                                        includeMarkdown("www/01-Instructions.Rmd"),
                                        p("This shiny app is currently on development and under the CreativeCommons CC BY-NC-SA License.")
                                        ),
                               tabItem("ScienNames",
                                       HTML("<br><br><br>"),
                                       fluidPage(
                                       column(4,
                                              fluidRow(
                                         box(title = "Select taxonomic resolution", status = "warning", 
                                             collapsible = T,width = 12,collapsed = T,
                                             p("Selecting many options to resolve taxonomically will result in longer computational times, please be patient. 
                                               (specially when your corpus contain a large and diverse collection of scientific names "),
                                             h5(tags$b("Identify species until:")),
                                             checkboxInput("SnamesOnly", "Scientific names Only", value = FALSE, width = NULL),
                                             uiOutput("conditionalInput"),
                                             uiOutput("conditionalInput2"),
                                             actionButton(inputId = "GoButton", label = tags$b("Get Taxa"),icon("mouse-pointer"), 
                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                         fluidRow(
                                         box( title = "Select an species",collapsible = T,
                                              width = 12,status = "warning",
                                              p("Set context limits:"),
                                              p("Lenght in characters left and right from the indexed positions"),
                                              column(5,
                                                     sliderInput("up", "Left:", min = 0, max = 500,
                                                                 value = 100, step= 10)),
                                              column(5,
                                                     sliderInput("down", "Right:", min = 0, max = 500, 
                                                                 value = 100, step= 10)),
                                              DT::dataTableOutput("data_table", width = "90%")))),
                                       column(8,
                                       box(title = "Text snippets",collapsible = T, width = 12,solidHeader = T, 
                                           status = "danger",
                                          
                                         DT::dataTableOutput("context"))))),
                               tabItem("Events",
                                       HTML("<br><br>"),
                                       fluidPage(
                                       column(5,
                                         box(title = "Select a biodiversity dictionary", width = 12, collapsible = T,collapsed = F,
                                             status = "warning",
                                           selectInput(inputId = "dictionary",
                                                       label = "From the dropdown list",
                                                       path2),
                                           actionButton("indexButton", label = tags$b("Index"),icon("mouse-pointer"), 
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                           h3("SkipGram matches"),
                                          uiOutput("names2"),
                                        checkboxInput("checkbox", label = "Filter by dictionary terms?", value = TRUE),
                                        actionButton("SkGram", label = tags$b("Find Word Associations"),icon("mouse-pointer"), 
                                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        
                                         h5(tags$b("Set context limit")),
                                         p("Lenght in characters left and right from the indexed positions"),
                                         column(5,
                                                sliderInput("up2", "Left:", min = 0, max = 500,
                                                            value = 100, step= 10)),
                                         column(5,
                                                sliderInput("down2", "Right:", min = 0, max = 500, 
                                                            value = 100, step= 10)),
                                         DT::dataTableOutput("skipGram",width = "90%"))
                                       ),
                                       column(7,
                                         box(title = "Text snippets", collapsible = T, width = 12, collapsed = F,status = "danger",solidHeader = T,
                                             DT::dataTableOutput("context2", width = "90%")
                                         )))
                                       # fluidRow(
                                       #   tabBox(title = "Plots",
                                       #          # The id lets us use input$tabset1 on the server to find the current tab
                                       #          id = "tabset1",
                                       #          tabPanel("Wordcloud",
                                       #                   plotOutput("dictionary")),
                                       #          tabPanel("Plot", 
                                       #                   plotOutput("plot")),
                                       #          tabPanel("Dictionary Matches",
                                       #                   DT::dataTableOutput("dictionary2"))))
                                       ),
                               tabItem("About",
                                       includeMarkdown("www/02-About.Rmd"),
                                       p(h3("Workflow")),
                                       img(src = "APPWORKFLOW.png", height = "900px"),
                                       includeMarkdown("www/03-About.Rmd")),
                               tabItem("bug",
                                       fluidPage(
                                         h3("Bugs and suggestions can be reported with the link below"),
                                         helpText(a("Report a bug", href = "https://github.com/fgabriel1891/BiodiversityObservationsMiner/issues/new", target = "_blank")),
                                         h3("Do you have new biodiversity dictionaries to share?"),
                                         helpText(a("Commit to this folder", href = "https://github.com/fgabriel1891/BiodiversityObservationsMiner/tree/master/dic", target = "_blank"))
                                                  
                                       )),
                               tabItem("code",
                                       fluidPage(
                                         h3("Biodiversity Observations Miner is written in R"),
                                         h4("Interested on details?, check the link below for the Source Code (Hosted as a GitHub repo)"),
                                         helpText(a("Check the Source Code", href = "https://github.com/fgabriel1891/BiodiversityObservationsMiner", target = "_blank")
                                         ))),
                               tabItem("contact",
                                       fluidPage(
                                         h3("Developed by: Gabriel Muñoz"),
                                         h5("GitHub"),
                                         a(shiny::icon('github fa-2x'),href='https://github.com/fgabriel1891/',target='_blank'),
                                         br(),
                                         h5("Mail"),
                                         a(shiny::icon('envelope-open fa-2x'),href='mailto:fgabriel1891@gmail.com',target='_blank'),
                                         br(),
                                         h5("Personal Page"),
                                         a(shiny::icon('user fa-2x'),href='https://sites.google.com/view/fgabriel1891',target='_blank')
                                         
                                         ))
                             
                             ))
))
  

                               #       
                               # tabItem("Geog",
                               #         h1("Mine by geographical locations"),
                               #         HTML("<br><br><br><br><br><br>"),
                               #         fluidPage(
                               #           fluidRow( 
                               #             box( title = "Index geographically",collapsible = T, width = 4,collapsed = T,
                               #                  textInput("ApiButton", " Type your monkeylearn API key here", ""),
                               #                  actionButton("submit","Submit"),
                               #                  actionButton(inputId = "GoButton3",
                               #                               label = "Scrape Locations"))),
                               #           fluidRow(
                               #                    tabsetPanel(
                               #                      tabPanel("Map",
                               #                               leafletOutput("map")),
                               #                      tabPanel("Locations Mined",
                               #                               DT::dataTableOutput("LocationsText")))
                               #                    ))
                               #         ),
