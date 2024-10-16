# Version of the software
epiplot_version <- "0.9.8"

# Define colors when less than 15 units
colors_vector <- c("#FF0000", "#0000FF", "#00FF00", "#FFFF00", "#FFA500", 
                   "#800080", "#FFC0CB", "#A52A2A", "#808080", "#40E0D0", 
                   "#b3b300", "#000000", "#FA8072", "#00751f", "#800000")

# define some basic credentials (on data.frame) for login
credentials <- data.frame(
  user = c("hpci"), # mandatory
  password = c("hpci"), # mandatory
  admin = c(FALSE),
  stringsAsFactors = FALSE
)

# Define some variable for reports generation
report_title = "Analyse des mouvements des patients"
final_report_title = "Rapport de surveillance moléculaire"
report_author1 = "Laboratoire d'épidémiologie"
report_author2 = "Institut de microbiologie - CHUV"
report_date = paste0("Date du rapport : ", format(Sys.time(), "%d.%m.%Y"))
