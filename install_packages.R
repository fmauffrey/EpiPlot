list.of.packages <- c(
  "shiny", "shinyalert", "shinyWidgets", "dplyr", "ggplot2", "forcats",
  "svglite", "shinydashboard", "plotly", "shinycssloaders", "lubridate",
  "GGally", "network", "sna", "scales", "readxl", "plotrix", "visNetwork",
  "RColorBrewer", "stringr", "IRanges", "shinymanager", "shinyjs", 
  "rmarkdown", "webshot", "tidyr", "here", "shinythemes", "BiocManager", "digest"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

BiocManager::install("IRanges")

install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')