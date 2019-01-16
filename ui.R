
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

## 

# Inputs: 
#   file1: fileInput widget, accepts one or more files with a .pdf extension
# SnamesOnly: checkboxInput widget, logical operator. 
# If its value equals to FALSE, it will render a conditional input to give more options to the user (conditionalInput, conditionalInput2).
# GoButton: actionButton widget, render as the "Get Taxa" button.
# up: sliderInput widget, ranges from 0-500. Sets the value for the number of characters up the matching position
# down: sliderInput widget ranges from 0-500. Sets the value for the number of characters down the matching position
# dictionary: selectInput widget, display a dropdown list of biodiversity dictionaries available
# indexButton: actionButton widget, renders the "Index" button. Triggers Index events based on the biodiversity dictionaries
# skGram: actionButton: actionButton widget, renders the "Find Word Associations" button. Triggers skipGram associations related functions.
# up2: same as up.
# down2: same as down.
# checkbox: checkboxInput widget, logical argument. 
# This controls whether the output of the skipGram matches will show only matches with the dictionary terms (TRUE/FALSE).

### Set color and similar paramenters

skin = "black" # Skin of dashboard 
title = "Biodiversity Observations Miner v.1.0" # Title of APP
twidth = 400 # width of dashboard title 
width = 300 # width of dashboard

# Set paths for dictionary files
dictionary.path <- "dic/"
filenames.dic<-list.files(dictionary.path)
path2 <- paste("dic","/", 
               filenames.dic, 
               sep = "")
names(path2) <- filenames.dic



## Build the UI
shinyUI(
  dashboardPage(skin = skin, 
                dashboardHeader( title = title, 
                                 titleWidth = 300,
                                 dropdownMenu(type = "notifications",
                                              notificationItem(
                                                text = textOutput("attention"),
                                                icon("warning"))
                                 )),
                dashboardSidebar(width = width,
                                 sidebarMenu(
                                   p(),
                                   menuItem( text = "Home",
                                            tabName = "Instr",
                                            icon = icon("home")),
                                   menuItem( text = "Upload",
                                             tabName = "Set",
                                             icon = icon("floppy-open", lib = "glyphicon")
                                             ),
                                   menuItem( text = "Mine Biodiversity Observations",
                                             tabName = "Mine",
                                             icon = icon("binoculars"),
                                   menuSubItem( text = "by taxa",
                                             tabName = "ScienNames",
                                             icon = icon("envira")),
                                   menuSubItem( text = "by keywords",
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
                  tags$link(rel = "stylesheet", 
                            type = "text/css",
                            href = "custom.css"),
                  
                             
                             tabItems(
                                            
                                tabItem("Instr", 
                                       includeMarkdown("www/01-Instructions.Rmd")),
                                    
                               tabItem("Set",
                                       fluidPage(
                                         includeMarkdown("www/settings.Rmd"),
                                        br(),br(),
                                         box(title = "Upload you article(s) to mine",
                                             width = 12 ,status = "success", 
                                             solidHeader = TRUE,
                                              fileInput('file1', 'Upload your article(s) here',
                                                   accept=c('.pdf'),
                                                   multiple = T,
                                                   placeholder = "No file(s) selected"))
                                         )),
                               tabItem("ScienNames",
                                       fluidPage(
                                         includeMarkdown("www/biTaxa.Rmd"),
                                         HTML("<br><br><br>"),
                                         column(5,
                                         fluidRow(
                                           box(title = "1: Select taxonomic resolution", 
                                               status = "warning", 
                                               collapsible = T,
                                               width = 12, 
                                               collapsed = F,
                                               p("Selecting many options to resolve taxonomically will result in longer computational times, please be patient. 
                                                 (specially when your corpus contain a large and diverse collection of scientific names "),
                                               p("When you are done, click on the Button at the bottom of the box"),
                                               h5(tags$b("Identify species until:")),
                                               checkboxInput("SnamesOnly", "Scientific names Only",
                                                             value = FALSE, 
                                                             width = NULL),
                                               uiOutput("conditionalInput"),
                                               uiOutput("conditionalInput2"),
                                               actionButton(inputId = "GoButton",
                                                            label = tags$b("Get Taxa"),
                                                            icon("mouse-pointer"), 
                                                            style="color: #fff; background-color: #337ab7; 
                                                            border-color: #2e6da4")),
                                         box( title = "2: Select a taxa",
                                              collapsible = T,
                                              width = 12, 
                                              status = "warning",
                                              p("The following table contains the taxonomic entities recognize in the corpus text.
                                                Taxonomic entities are found with the Global Names Recognition and Discovery tool"),
                                              p("Once the annotation is finished, click on individual species names to reveal species-specific content"),
                                              DT::dataTableOutput("data_table", width = "90%")),
                                         box(width = 12,
                                             textOutput("attention2"))
                                         )),
                                       column(7,
                                              box( title = "3: Render context",
                                                   collapsible = T,
                                                   width = 12, 
                                                   status = "warning",
                                                   actionButton(inputId = "SkGram2",
                                                                label = tags$b("Infer context"),
                                                                icon("mouse-pointer"), 
                                                                style="color: #fff;
                                                                background-color: #337ab7; 
                                                                border-color: #2e6da4"),
                                                   DT::dataTableOutput("contextTableSp",
                                                                       width = "90%")),
                                       box(title = "Text snippets",
                                           collapsible = T, 
                                           width = 12,
                                           solidHeader = T, 
                                           status = "danger",
                                           DT::dataTableOutput("context")))
                                       
                                       )),
                               tabItem("Events",
                                       fluidPage(
                                        includeMarkdown("www/byBioDic.Rmd"),
                                         HTML("<br><br><br>"),
                                        column(5,
                                        box( title = "Content discovery", 
                                             collapsible = T,
                                             width = 12, status = "warning",
                                             checkboxInput("byDic", 
                                                           "Use a biodiversity dictionary to filter results (optional)", 
                                                           value = F),
                                            uiOutput("byDic2"),
                                             actionButton("SkGram", 
                                                          label = tags$b("Find Word Associations"),
                                                          icon("mouse-pointer"), 
                                                          style="color: #fff; 
                                                          background-color: #337ab7;
                                                          border-color: #2e6da4"),
                                             br(),
                                            br(),
                                            #uiOutput("filterBut"),
                                             DT::dataTableOutput("skipGram"))),
                                       column(7,
                                              box(title = "Text snippets", 
                                                  collapsible = T, 
                                                  width = 12, 
                                                  collapsed = F,
                                                  status = "danger",
                                                  solidHeader = T,
                                                  DT::dataTableOutput("context2", width = "90%")
                                              ))
                                       
                                       )),
                               
                               # box(title = "3: Select a biodiversity dictionary", width = 12, collapsible = T,collapsed = T,
                               #     status = "warning",
                               #     selectInput(inputId = "dictionary",
                               #                 label = "From the dropdown list",
                               #                 path2),
                               #     p("When you are done, click on the Button at the bottom of the box"),
                               #     actionButton("indexButton", label = tags$b("Index"),icon("mouse-pointer"), 
                               #                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                               
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
                                         h2("Here there is available contact information for all authors of Biodiversity Observations Miner."),
                                         h4("BOM interface was written  by: Gabriel Muñoz"),
                                         h5("GitHub"),
                                         a(shiny::icon('github fa-2x'),href='https://github.com/fgabriel1891/',target='_blank'),
                                         br(),
                                         h5("Mail"),
                                         a(shiny::icon('envelope-open fa-2x'),href='mailto:fgabriel1891@gmail.com',target='_blank'),
                                         br(),
                                         h5("Personal Page"),
                                         a(shiny::icon('user fa-2x'),href='https://sites.google.com/view/fgabriel1891',target='_blank'),
                                         HTML("<br><br/>"),
                                         h3("Guidance, comments and input by:"),
                                         h4(a("W.Daniel Kissling"), href = "https://www.danielkissling.de/", target = "_blank"),
                                         h4(a("Emiel van Loon"), href = "https://staff.fnwi.uva.nl/e.e.vanloon/", target = "_blank")
                                         ))
                             
                             )))
)