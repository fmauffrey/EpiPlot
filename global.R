# Version of the software
epiplot_version <- "1.0"

# Define colors when less than 15 units
predefined_colors <- c("#FF0000", "#0000FF", "#00FF00", "#FFFF00", "#FFA500", 
                       "#800080", "#FFC0CB", "#A52A2A", "#808080", "#40E0D0", 
                       "#b3b300", "#000000", "#FA8072", "#00751f", "#800000")

# Define bacteria showed in the list
bacteria_list <- c(
  "Staphyloccocus aureus",
  "Enteroccocus faecium",
  "Pseudomonas aeruginosa",
  "Clostridium difficile"
)

# Define genotyping methods showed in the list
genotyping_list <- c("DLST", "wgMLST", "cgMLST")

# Define some variable for reports generation
report_title = "Analyse des mouvements des patients"
final_report_title = "Rapport de surveillance moléculaire"
report_author1 = "Laboratoire d'épidémiologie"
report_author2 = "Institut de microbiologie - CHUV"
report_date = paste0("Date du rapport : ", format(Sys.time(), "%d.%m.%Y"))
