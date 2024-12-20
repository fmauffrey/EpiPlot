# First block to execute
list.of.packages <- c(
  "shiny", "shinyalert", "shinyWidgets", "dplyr", "ggplot2", "forcats",
  "svglite", "shinydashboard", "plotly", "shinycssloaders", "lubridate",
  "GGally", "network", "sna", "scales", "readxl", "plotrix", "visNetwork",
  "RColorBrewer", "stringr", "shinymanager", "shinyjs", 
  "rmarkdown", "webshot2", "tidyr", "here", "shinythemes", "BiocManager", "digest",
  "reticulate"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

BiocManager::install("IRanges")

# Second block to execute
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')
tinytex::install_tinytex(force = T)

