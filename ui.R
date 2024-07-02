# Load required packages
suppressPackageStartupMessages({
  library(shiny)
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
  library(shinyalert)
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
                                         box(width = 12, height = "65vh",
                                             withSpinner(plotlyOutput("timeline", height="62vh"))),
                                         
                                         
                                         
                                         # Box with controls
                                         box(width = 12,
                                             column(width = 2,
                                                    selectInput("ganttOrder", "Ordre", list("Date d'admission" = "Début_mouvement", "IPP" = "IPP"), selected = "Début_mouvement")),
                                             column(width = 2,
                                                    selectInput("scaleType", "Echelle", selected = "month", list("Jours" = "day",
                                                                                                                            "Semaines" = "week",
                                                                                                                            "Mois" = "month",
                                                                                                                            "Semestre" = "semester",
                                                                                                                            "Années" = "year"))),
                                             column(width = 2,
                                                    sliderInput(inputId = 'DotSize', label = 'Taille prélèvements', value = 3, min = 1, max = 10, ticks = F)
                                             ),
                                             column(width = 2,
                                                    sliderInput(inputId = 'segmentSize', label = 'Taille mouvements', value = 4, min = 1, max = 20, ticks = F)
                                             ),
                                             column(width=2,
                                                    pickerInput("highlightPicker", "Mettre en avant", choices = "",
                                                                multiple = TRUE, options = pickerOptions(title = "Aucune sélection", size = 10, actionsBox = T, liveSearch = T))),
                                             column(width=2, div(style = "display: flex; justify-content: center;",
                                                    actionBttn("download_moves_button", label=NULL, size="lg", style="gradient",
                                                               icon=icon("download", class="sharp", lib = "font-awesome"))))
                                       ))),
                              
                              ########### Network tab
                              tabPanel("Réseau", icon = icon("circle-nodes"),
                                       fluidRow(
                                         # Activate shinyjs for disabling button
                                         useShinyjs(),
                                         # Box with plot stats units
                                         box(width = 12, height = "65vh",
                                             withSpinner(visNetworkOutput("network", height="600px"))),
                                         
                                         box(width = 12, 
                                             column(width = 2,
                                                    sliderInput(inputId = 'IndirectLinkTime', 
                                                                label = "Jours d'écart pour un lien indirect", 
                                                                value = 14, min = 1, max = 28, ticks = F)),
                                             column(width = 1,
                                                    div(
                                                    actionBttn("update_edges_button", label = "Ajouter", size = "sm"),
                                                    actionBttn("remove_edges_button", label= "Enlever", size = "sm"),
                                                    style = "display: flex; flex-direction: column; gap: 10px;")),
                                             column(width = 2,
                                                    actionBttn("getNodes", label = "Afficher les IPP sélectionnées",
                                                               size = "md", style = "minimal", color = "success")),
                                             column(width = 2,
                                                    prettySwitch("network_focus_trigger", label = "Focus",
                                                                 value = TRUE, status = "success", slim = TRUE)
                                                    )
                                             )
                                         )
                                       )
                              ))))