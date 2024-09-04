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
  source("functions.R")
})

#### Settings parameters #######
# Loading animation
options(spinner.type = 6)

secure_app(language = "fr",
  dashboardPage(skin = "green",
                title = "Epiplot",
                
                # Header
                dashboardHeader(title = span(img(src = "CHUV.png", height = 40), "Epiplot"),
                                dropdownMenu(type="notifications", 
                                             badgeStatus = NULL,
                                             icon = icon("info"),
                                             headerText = "Epiplot version 0.9.6",
                                             notificationItem("florian.mauffrey@chuv.ch",
                                                              icon = icon("envelope"),
                                                              status = "info"))),
  
                # Side bar menu
                dashboardSidebar(
                  sidebarMenu(
                    id = "SideBar",
                    menuItem("Fichiers", icon = icon("file"),
                             fileInput("Data_mouvements", "Table des mouvements", accept=c(".xls", ".xlsx"),
                                       buttonLabel ="Parcourir...", placeholder = "Aucun fichier"),
                             fileInput("Data_sampling", "Table des prélèvements", accept=c(".xls", ".xlsx"),
                                       buttonLabel = "Parcourir...", placeholder = "Aucun fichier")
                             ),
                    menuItem("Paramètres", icon = icon("gears"),
                             dateRangeInput("DateRange", "Dates", language = "fr-CH", weekstart = 1, separator = "à", format = "dd-mm-yyyy"),
                             fluidRow(column(6, actionBttn("bttnDateFilter365", "Dernière année", style = "simple", color = "success", size = "xs"),
                                             actionBttn("bttnDateFilterReset", "Reset", style = "simple", color = "success", size = "xs"))),
                             pickerInput("genotypePicker", "Génotype", choices = "",
                                         options = pickerOptions(title = "Aucune sélection", size = 10, liveSearch = T)),
                             pickerInput("patientPicker", "Patients", choices = "",
                                         multiple = TRUE, options = pickerOptions(title = "Aucune sélection", size = 10, actionsBox = T)),
                             selectInput("selectedUnit", "Niveau", list("Unité de soins" = "Unité_de_soins",
                                                                        "Unité fonctionelle" = "Unité_fonctionelle",
                                                                        "Service" = "Service",
                                                                        "Département" = "Département"))
                             )
                    )),
              
                # Body
                dashboardBody(
                  
                  ########## Logo for the browser tab
                  tags$head(tags$link(rel = "shortcut icon", href = "logo.png")),
                  
                  tabsetPanel(type = "tabs",
                              selected = "Table",
                              
                              ########## Table loading tab
                              tabPanel("Table", icon = icon("table"),
                                       uiOutput("table")
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
                                             selectInput("scaleType", "Echelle", selected = "month", list("Jours" = "day",
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