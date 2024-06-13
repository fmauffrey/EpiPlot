# Define colors when less than 15 units
colors_vector <- c("#FF0000", "#0000FF", "#00FF00", "#FFFF00", "#FFA500", 
                   "#800080", "#FFC0CB", "#A52A2A", "#808080", "#40E0D0", 
                   "#b3b300", "#000000", "#FA8072", "#00751f", "#800000")

# define some basic credentials (on data.frame) for login
credentials <- data.frame(
  user = c("HPCI"), # mandatory
  password = c("HPCI"), # mandatory
  admin = c(FALSE),
  stringsAsFactors = FALSE
)
