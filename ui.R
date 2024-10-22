# Load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(shinyalert)
  library(shinyWidgets)
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(svglite)
  library(shinydashboard)
  library(plotly)
  library(shinycssloaders)
  library(lubridate)
  library(GGally)
  library(network)
  library(sna)
  library(scales)
  library(readxl)
  library(plotrix)
  library(shinyhelper)
  library(visNetwork)
  library(RColorBrewer)
  library(stringr)
  library(IRanges)
  library(shinymanager)
  library(shinyjs)
  library(rmarkdown)
  library(webshot)
  library(tidyr)
  library(here)
  library(shinythemes)
  source("functions.R")
})

#### Settings parameters #######
# Loading animation
options(spinner.type = 6)

secure_app(language = "fr",
           theme = shinytheme("cerulean"),
           tags_top = tags$div(tags$img(src = "Epiplot_logo.png", width = 400)),
           dashboardPage(skin = "blue",
                title = "Epiplot",
                
                # Header
                dashboardHeader(title = img(src = "Epiplot_logo.png", height = 50),
                                dropdownMenu(type="notifications", 
                                             badgeStatus = NULL,
                                             icon = icon("info"),
                                             headerText = paste0("Epiplot version ", epiplot_version),
                                             notificationItem("florian.mauffrey@chuv.ch",
                                                              icon = icon("envelope"),
                                                              status = "info"))),
  
                # Side bar menu
                dashboardSidebar(
                  sidebarMenu(
                    id = "SideBar",
                    
                    # Files loading
                    menuItem("Fichiers", icon = icon("file"), startExpanded = TRUE,
                             fileInput("Data_mouvements", "Table des mouvements", accept=c(".xls", ".xlsx"),
                                       buttonLabel ="Parcourir...", placeholder = "Aucun fichier"),
                             fileInput("Data_sampling", "Table des prélèvements", accept=c(".xls", ".xlsx"),
                                       buttonLabel = "Parcourir...", placeholder = "Aucun fichier")
                             ),
                    
                    # Parameters settings
                    menuItem("Paramètres", icon = icon("gears"),
                             dateRangeInput("DateRange", "Dates", language = "fr-CH", weekstart = 1, separator = "à", format = "dd-mm-yyyy"),
                             fluidRow(column(6, actionBttn("bttnDateFilter365", "Dernière année", style = "simple", color = "default", size = "xs"),
                                             actionBttn("bttnDateFilterReset", "Reset", style = "simple", color = "primary", size = "xs"))),
                             pickerInput("genotypePicker", "Génotype", choices = "",
                                         options = pickerOptions(title = "Aucune sélection", size = 10, liveSearch = T)),
                             pickerInput("patientPicker", "Patients", choices = "",
                                         multiple = TRUE, options = pickerOptions(title = "Aucune sélection", size = 10, actionsBox = T)),
                             selectInput("selectedUnit", "Niveau", list("Unité de soins" = "Unité_de_soins",
                                                                        "Unité fonctionelle" = "Unité_fonctionelle",
                                                                        "Service" = "Service",
                                                                        "Département" = "Département")),
                             hr(),
                             awesomeCheckbox("shortMovesCheckbox", "Filtrer mouvements courts", value = F),
                             numericInput("shortMovesThreshold", label = "Jours", value = 0.5, 
                                          min = 0, max = 10, step=0.1)
                             ),
                    
                    # Report generation
                    menuItem("Rapport", icon=icon("file-arrow-down"),
                             pickerInput("speciesPicker", "Organisme", choices=c("Staphyloccocus aureus résistant à la méticiline",
                                                                                 "Enteroccocus faecium résistant à la vancomycine",
                                                                                 "Pseudomonas aeruginosa",
                                                                                 "Clostridium difficile")),
                             pickerInput("typing_method", "Méthode", choices = c("DLST",
                                                                                 "wgMLST",
                                                                                 "cgMLST")),
                             textAreaInput("report_comments", "Commentaires", 
                                           width = "100%", 
                                           height = "30vh", 
                                           resize = "none"),
                             radioGroupButtons("report_type", "Format", status="info", 
                                               checkIcon = list(yes = icon("check", class="fa-solid fa-check fa-2xs")),
                                               choiceNames=list(icon("file-pdf", class="fa-regular fa-file-pdf fa-xl"),
                                                                icon("file-code", class="fa-regular fa-file-code fa-xl"),
                                                                icon("file-word", class="fa-regular fa-file-word fa-xl")),
                                               choiceValues= list("report_pdf.Rmd",
                                                                  "report_html.Rmd",
                                                                  "report_word.Rmd"),
                                               justified = T,
                                               size="lg"),
                             fluidPage(column(10,downloadBttn("generate_report_button", label = "Générer", style = "material-flat",
                                          size="sm", icon = icon("file-export"), block=T, color="default")))
                    ),
                    menuItem("Rapport final", icon=icon("file-signature"),
                             fluidPage(
                               fluidRow(fileInput("Data_reports", "Résultats à inclure", accept=c(".pdf", ".html"),
                                                  buttonLabel ="Parcourir...", placeholder = "Aucun fichier", multiple = T)),
                               fluidRow(column(10, downloadBttn("generate_final_report_button", label = "Générer", style = "material-flat",
                                                                size="sm", icon = icon("file-export"), block=T, color="default"))))
                    )
                  )
                ),
              
                # Body
                dashboardBody(
                  
                  ########## Logo for the browser tab
                  tags$head(tags$link(rel = "shortcut icon", href = "Epiplot_logo_mini.png")),
                  
                  tabsetPanel(type = "tabs",
                              selected = "Table",
                              
                              ########## Table tab
                              tabPanel("Table", icon = icon("table"),
                                       uiOutput("table")
                              ),
                              
                              ########## Summary tab
                              tabPanel("Résumé", icon = icon("table-list"),
                                       uiOutput("summary_table"),
                              ),
                              
                              ############# Gantt tab
                              tabPanel("Mouvements", icon = icon("chart-gantt"),
                                       fluidRow(
                                         # Box with plot
                                         box(width = 12, height = "60vh",
                                             withSpinner(plotlyOutput("timeline", height="57vh"))),
                                         
                                         # Box with controls
                                         box(title = "Axes du graphique", width = 2, height = "25vh",
                                             selectInput("ganttOrder", "Ordre", list("Admission" = "Début_mouvement", "IPP" = "IPP"),
                                                         selected = "Début_mouvement"),
                                             selectInput("scaleType", "Echelle", selected = "year", list("Jours" = "day",
                                                                                                         "Semaines" = "week",
                                                                                                         "Mois" = "month",
                                                                                                         "Semestre" = "semester",
                                                                                                         "Années" = "year"))),
                                         box(title = "Taille", width = 2, height = "25vh",
                                             sliderInput(inputId = 'DotSize', label = 'Prélèvements', value = 3, min = 1, max = 10, ticks = F),
                                             sliderInput(inputId = 'segmentSize', label = 'Mouvements', value = 4, min = 1, max = 20, ticks = F)),
                                         box(title = "Patients",width = 2, height = "25vh",
                                             pickerInput("highlightPicker", label = "Accentuer", choices = "",
                                                         multiple = TRUE, options = pickerOptions(title = "Aucune sélection", 
                                                                                                  size = 10,
                                                                                                  liveSearch = T))),
                                         actionBttn("download_moves_button", label=NULL, size="lg", style="gradient",
                                                    icon=icon("download", class="sharp", lib = "font-awesome"))
                                       )),
                              
                              ########### Network tab
                              tabPanel("Réseau", icon = icon("circle-nodes"),
                                       fluidRow(
                                         # Activate shinyjs for disabling button
                                         useShinyjs(),
                                         # Box with plot stats units
                                         box(width = 12, height = "60vh",
                                             withSpinner(visNetworkOutput("network", height="57vh"))),
                                         
                                         box(title = "Liens indirects", width = 2,  height = "25vh",
                                             sliderInput(inputId = 'IndirectLinkTime',
                                                         label = "Jours d'écart",
                                                         value = 14, min = 1, max = 28, ticks = F),
                                             radioGroupButtons(inputId = "update_edges_button",
                                                               choices = c("Avec", "Sans"),
                                                               selected = "Sans",
                                                               status = "primary",
                                                               justified = T,
                                                               individual = T,
                                                               size = "sm")),
                                         box(title = "Options", width = 2, height = "25vh",
                                             sliderInput(inputId = 'SizeNodes',
                                                         label = "Taille des noeuds",
                                                         value = 15, min = 1, max = 30, ticks = F),
                                             column(width = 6,
                                                    prettySwitch("network_focus_trigger", label = "Focus",
                                                                 value = TRUE, status = "success")),
                                             column(width = 6,
                                                    prettySwitch("network_gravity_trigger", label = "Gravité",
                                                                 value = TRUE, status = "success"))),
                                         box(title = "IPP", width = 2, height = "25vh",
                                             pickerInput("findPatient_network", choices = "",
                                                         options = pickerOptions(title = "Localiser un patient",
                                                                                 size = 10,
                                                                                 liveSearch = TRUE)),
                                                    actionBttn("getNodes", label = "Afficher/Filtrer les IPP sélectionnées",
                                                               size = "md", style = "simple", color = "primary"))
                                         )))
                  )))