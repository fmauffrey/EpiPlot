# Version of the software
epiplot_version <- "1.0"

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
