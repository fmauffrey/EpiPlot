tables_per_genotype <- function(session, moves_table, samplings_table){
  # Create a list of tables for each genotype
  
  # Add genotype column and create the final table
  complete_table <- moves_table %>%
    mutate(Génotype = "Aucun")
  
  # Create list for storing table and genotype picker
  tables_list <- list()
  genotype_id <- list()
  IPP_list <- list()
  
  # Build the different lists
  all_genotypes <- unlist(str_split(levels(factor(samplings_table$CLUSTER)), ", "))
  for (gen in all_genotypes){
    filt_IPP <- samplings_table[grep(gen,samplings_table$CLUSTER), "IPP"]
    filt_moves <- moves_table %>%
      filter(IPP %in% filt_IPP) %>%
      mutate(Génotype = gen)
    tables_list[[paste(gen)]] <- filt_moves
    
    genotype_id[[paste0(gen, " (", length(unique(filt_IPP)) ,")")]] <- gen
    
    IPP_list[[paste(gen)]] <- unique(filt_IPP)
  }
  
  # Update genotype picker widget
  updatePickerInput(session, "genotypePicker", 
                    choices = genotype_id,
                    selected = genotype_id[[1]],
                    choicesOpt = list(style = rep("color:black;", length(genotype_id))))
  
  return(list(tables_list, IPP_list))
}

check_moves_table <- function(input_file){
  # Load and check the formatting of the table
  
  # Check extension
  ext <- tools::file_ext(input_file$name)
  if (!ext %in% c("xlsx", "xls")) {
    show_alert(title="Erreur de chargement",
               type="error",
               closeOnClickOutside = T,
               text="Mauvaise extension")
    return(NULL)
  }
  
  # Check data
  moves_table <- read_excel(input_file$datapath, skip = 7)
  rows_number <- nrow(moves_table) - 2 # 2 useless rows at the end
  moves_table <- moves_table[!is.na(moves_table$IPP),]
  
  if (ncol(moves_table) != 11){
    show_alert(title="Erreur de chargement",
               type="error",
               closeOnClickOutside = T,
               text=paste0("Fichier incorrect. Nombre de colonnes = ", ncol(moves_table), " (Requis 11)"))
    return(NULL)
  }
  
  if (nrow(moves_table) == 0){
    show_alert(title="Erreur de chargement",
               type="error",
               closeOnClickOutside = T,
               text="Fichier incorrect. Aucune donnée.")
    return(NULL)
  }
  
  return("OK")
}

check_samplings_table <- function(input_file){
  # Load and check the formatting of the samplings table
  
  # Check extension
  ext <- tools::file_ext(input_file$name)
  if (!ext %in% c("xlsx", "xls")) {
    message <- paste0("Le type de fichier est incorrect (", 
                      ext,
                      "). Fichiers acceptés : .xlsx, .xls.")
    return(message)
  }
  
  # Load table
  samplings_table <- as.data.frame(read_excel(input_file$datapath))
  
  # Check number of columns
  if (ncol(samplings_table) != 4){
    message <- paste0("Le nombre de colonnes est incorrect ("
                      , ncol(samplings_table), 
                      "). Le nombre de colonnes attendu est 4.")
    return(message)
  }
  
  # Check number of rows
  if (nrow(samplings_table) == 0){
    message <- "Aucune donnée présente dans le fichier."
    return(message)
  }
  
  # Check sample sampling type
  sampling_types <- levels(factor(samplings_table[,2]))
  incorrect_values <- setdiff(sampling_types, c("POSITIVE", "NEGATIVE"))
  if (length(incorrect_values) > 0){
    message <- paste0("Valeurs incorrectes pour les données de la colonne ", 
                      colnames(samplings_table)[2],
                      " : ", 
                      paste0(incorrect_values, collapse = ", "),
                      ". Valeurs acceptées : POSITIVE ou NEGATIVE.")
    return(message)
  }
  
  # Check missing values
  col_with_na <- c()
  for (n in colnames(samplings_table)[1:3]){
    if (NA %in% samplings_table[,n]){
      col_with_na <- c(col_with_na, n)
    }
  }
  if (length(col_with_na) > 0){
    message <- paste0("Des valeurs sont manquantes pour les données suivantes : ",
                      paste0(col_with_na, collapse = ", "),
                      ".")
    return(message)
  }

  # Check samplings dates format
  date_test <- try(as.Date(samplings_table$DATE), silent = T)
  if (length(grep("Error", date_test)) > 0){
    message <- paste0("Valeurs incorrectes pour les données de la colonne ", 
                      colnames(samplings_table)[3],
                      ". Valeurs acceptées : date au format JJ/MM/AAAA.")
    return(message)
  }

  return("ok")
}

filter_by_date <- function(table, start, end){
  # Filter the table by date and truncate moves if needed
  moves_to_remove <- c()
  for (i in 1:nrow(table)){
    if (table[i,"Fin_mouvement"] < start | table[i,"Début_mouvement"] > end){
      moves_to_remove <- c(moves_to_remove, i)
    } else if (table[i,"Fin_mouvement"] > start & table[i,"Début_mouvement"] < start){
      table[i,"Début_mouvement"] <- start
    } else if (table[i,"Fin_mouvement"] > end & table[i,"Début_mouvement"] < end){
      table[i,"Fin_mouvement"] <- end
    }
  }
  
  # Remove the filtered rows if needed
  if (is.null(moves_to_remove)){
    return(table)
  } else {
    return(table[-moves_to_remove, ])
  }
}

format_moves_table <- function(input_table_path){
  # Format the moves table
  
  # First read of the table to find the number of rows to lead
  moves_table <- read_excel(input_table_path, skip = 7)
  rows_number <- nrow(moves_table) - 2 # 2 useless rows at the end
  
  # Load the table
  moves_table <- read_excel(input_table_path, skip = 8,
                            col_names = c("IPP", "Séjour", "Début_séjour",
                                          "Fin_séjour", "Début_mouvement",
                                          "Fin_mouvement", "Département",
                                          "Service", "Unité_fonctionelle",
                                          "Unité_de_soins", "Durée_mouvement"),
                            n_max = rows_number)
  
  # Convert into appropriate type
  moves_table <- moves_table %>% mutate(IPP = as.character(IPP),
                                        Séjour = as.factor(Séjour),
                                        Début_mouvement = as.POSIXct(Début_mouvement),
                                        Fin_mouvement = as.POSIXct(Fin_mouvement),
                                        Fin_séjour = as.POSIXct(Fin_séjour),
                                        Département = as.factor(Département),
                                        Service = as.factor(Service),
                                        Unité_fonctionelle = as.factor(Unité_fonctionelle),
                                        Unité_de_soins = as.factor(Unité_de_soins),
                                        Durée_mouvement = as.numeric(Durée_mouvement))
  
  return(moves_table)
}

format_samplings_table <- function(input_table_path){
  
  # Load the table
  table <- read_excel(input_table_path, skip =1,
                      col_names = c("IPP", "PRELEVEMENT",
                                    "DATE_PRELEVEMENT", "CLUSTER"))
  
  # Convert into appropriate type
  table <- as.data.frame(table %>% 
                           mutate(IPP = as.character(IPP),
                                  PRELEVEMENT = as.character(PRELEVEMENT),
                                  DATE_PRELEVEMENT = as.POSIXct(DATE_PRELEVEMENT),
                                  CLUSTER = as.character(CLUSTER)) %>%
                           filter(!is.na(DATE_PRELEVEMENT)))
  
  # Set sampling time at 12:00:00 if no time given
  table <- table %>%
    mutate(DATE_PRELEVEMENT = if_else(format(DATE_PRELEVEMENT, "%H:%M:%S") == "00:00:00",
                                      as.POSIXct(format(DATE_PRELEVEMENT, "%Y-%m-%d 12:00:00"), tz = attr(DATE_PRELEVEMENT, "tzone")),
                                      as.POSIXct(DATE_PRELEVEMENT, tz = attr(DATE_PRELEVEMENT, "tzone"))))
  
  return(table)
}

generate_network_data <- function(time_unit, detailed_button, table, 
                                  network_unit, colors_vector, 
                                  indirect_time=14, length_edges=150,
                                  size_font_edges=20){
  # Generate network data for plotting or extracting label info on selected
  
  # Import filtered data
  data <- as.data.frame(table)
  data$IPP <- as.character(data$IPP)
  
  # Drop rows with NA in departements (no moves)
  data <- data[!is.na(data$Département),]
  
  # Update the different factors levels with the currently selected data
  data <- droplevels(data)
  
  # Convert into edge list using IRanges
  connections <- data.frame()
  
  for (unit in levels(factor(data[,network_unit]))){
    subtable <- data[data[,network_unit]==unit,]
    
    # Setup the IRanges object
    ir = IRanges(as.numeric(subtable$Début_mouvement), as.numeric(subtable$Fin_mouvement), names = subtable$IPP)
    ovrlp = findOverlaps(ir, drop.self = TRUE, drop.redundant = TRUE) 
    
    if (length(ovrlp)!=0){
      # Store id indices for further use    
      hit1 = queryHits(ovrlp)
      hit2 = subjectHits(ovrlp)
      
      # Extract the overlaps duration, convert into days (rounded), names for visNetwork
      widths = width(pintersect(ir[hit1], ir[hit2])) - 1
      unit_connections <- data.frame(from = names(ir)[hit1], to = names(ir)[hit2], label=round(widths/86400,1))
      unit_connections <- aggregate(label ~ from + to, data = unit_connections, FUN=sum)
      unit_connections <- unit_connections[unit_connections$label!=0,]
      unit_connections$color <- rep(unit, nrow(unit_connections))
      connections <- rbind.data.frame(connections, unit_connections)
    }
  }
  
  # Create indirect links table
  data$Fin_mouvement <- data$Fin_mouvement + days(indirect_time)
  
  for (unit in levels(factor(data[,network_unit]))){
    subtable <- data[data[,network_unit]==unit,]
    
    # Setup the IRanges object
    ir = IRanges(as.numeric(subtable$Début_mouvement), as.numeric(subtable$Fin_mouvement), names = subtable$IPP)
    ovrlp = findOverlaps(ir, drop.self = TRUE, drop.redundant = TRUE) 
    
    if (length(ovrlp)!=0){
      # Store id indices for further use    
      hit1 = queryHits(ovrlp)
      hit2 = subjectHits(ovrlp)
      
      # Extract the overlaps duration, convert into days (rounded), names for visNetwork
      unit_connections <- data.frame(from = names(ir)[hit1], to = names(ir)[hit2])
      
      for (row in 1:nrow(unit_connections)){
        if (unit_connections[row,1] != unit_connections[row,2]){
          # if no direct link, just add all indirect links
          if (nrow(connections) < 1){ 
            connections <- rbind.data.frame(connections, c(unit_connections[row,], label=0, color=unit))
            # if there are direct links, check if there is a direct link for the same unit
          } else if (!(paste0(unit_connections[row,1], unit_connections[row,2], unit) %in% 
                       paste0(connections[,1], connections[,2], connections[,4]))){
            connections <- rbind.data.frame(connections, c(unit_connections[row,], label=0, color=unit))
          }
        }
      }
    }
  }
  
  # Creation of the nodes data
  nodes <- unique(data$IPP) # All IPP are considered
  net_nodes <- data_frame(id=1:length(nodes), label=as.character(nodes))
  net_nodes$shape <- rep("circle", nrow(net_nodes))
  
  # Creation of the edges and color data if there is connections
  if (nrow(connections) > 0){
    net_edges <- connections
    
    # Prepare data for visNetwork
    net_edges <- net_edges %>% mutate(from=as.character(from),
                                      to=as.character(to),
                                      label=as.character(label))
    
    net_edges$from <- match(net_edges$from, net_nodes$label, nomatch = 0)
    net_edges$to <- match(net_edges$to, net_nodes$label, nomatch = 0)
    
    # Create dataframe for the legend
    edges_colors <- data.frame(color=colors_vector[net_edges$color],
                               label=net_edges$color)
    edges_colors <- edges_colors[!duplicated(edges_colors$color),]
    edges_colors$width <- rep(10, nrow(edges_colors))
    edges_colors$font.background <- rep("#ffffff", nrow(edges_colors))
    edges_colors$arrows <- rep("NULL", nrow(edges_colors))

    # Add custom characteristics to edges
    net_edges$width <- rescale(as.numeric(net_edges$label), c(2, 20))
    net_edges$length <- rep(length_edges, nrow(net_edges))
    net_edges$color.color <- rep("#818281", nrow(net_edges))
    net_edges$font.size <- rep(size_font_edges, nrow(net_edges))
    net_edges$font.background <- rep("#ffffff", nrow(net_edges))
    net_edges$color <- colors_vector[net_edges$color]
    net_edges$dashes <- rep(FALSE, nrow(net_edges))
    
    if (length(net_edges$label=="0") > 0){
      # Adapt indirect links if present
      net_edges[net_edges$label=="0","dashes"] = TRUE
      net_edges[net_edges$label=="0","width"] = 4
      net_edges[net_edges$label=="0","label"] = ""
    }
    
    # Split the direct links and indirect links into 2 tables
    net_edges_indirect <- net_edges[net_edges$label=="",]
    net_edges <- net_edges[net_edges$label!="",]
    
  } else {
    net_edges <- data.frame()
    edges_colors <- data.frame()
    net_edges_indirect <- data.frame()
  }
  
  # Return data
  return(list(net_nodes, net_edges, edges_colors, net_edges_indirect))
}

nested_list_to_df <- function(nested_list) {
  # Define a function to convert a nested list to a data frame
  
  # Initialize an empty list to store data frame rows
  rows <- list()
  
  # Iterate over each element in the nested list
  for (id in names(nested_list)) {
    item <- nested_list[[id]]
    # Flatten the nested structure into a single-level named list
    row <- list(
      from = item$from,
      to = item$to,
      label = item$label,
      color = item$color,
      width = item$width,
      length = item$length,
      font_size = item$font$size,
      font_background = item$font$background,
      dashes = item$dashes,
      id = item$id
    )
    # Add the flattened row to the list of rows
    rows <- append(rows, list(row))
  }
  
  # Combine all rows into a single data frame
  df <- do.call(rbind, lapply(rows, as.data.frame))
  return(df)
}

order_plot_data <- function(plot_data, samplings_data, user_choice){
  # Change IPP order for Gantt plot
  
  # Define IPP order based on user choice
  new_order <- NA
  
  # Order by IPP
  if (user_choice == "IPP"){
    new_order <- unique(sort(plot_data$IPP))
    
    # Order by older move
  } else if (user_choice == "Début_mouvement") {
    new_order <-  unique(plot_data$IPP[order(plot_data$Début_mouvement)])
    
    # Order by first positive sample
  } else if (user_choice == "Prelevements") {
    sample_table_first_pos <- samplings_data %>%
      dplyr::group_by(IPP) %>%
      dplyr::filter(PRELEVEMENT == "Positif") %>%
      dplyr::filter(DATE_PRELEVEMENT == min(DATE_PRELEVEMENT)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
    
    # Check if patients have moves but no positive sample
    no_pos <- setdiff(levels(as.factor(plot_data$IPP)), levels(as.factor(sample_table_first_pos$IPP)))
    
    # Create the new order (adding patients with no positive sample)
    new_order <- c(sample_table_first_pos$IPP[order(sample_table_first_pos$DATE_PRELEVEMENT)], no_pos)
  }
  
  # Apply new order to the dataframe
  plot_data$IPP <- factor(plot_data$IPP, levels = new_order)
  
  return(plot_data)
}

replace_no_end <- function(input_table){
  # Check if moves have no end, extend to today date and display popup
  
  # Check if NA are present in date and set replacement popup state
  popup_replace <- if_else(NA %in% input_table$Fin_séjour, T, F)
  replacement_number <- nrow(unique(input_table[is.na(input_table$Fin_séjour),"IPP"]))
  
  # Pop-up if replacements occurred
  if (popup_replace){
    show_alert(title="Date(s) manquante(s)",
               type="info",
               closeOnClickOutside = T,
               text=paste0(replacement_number, " séjour(s) sans date de sortie. La date du jour sera utilisée."))
  }
  
  # Identify rows where "Fin_séjour" is NA and replace "Fin_mouvement" if applicable
  new_table <- input_table %>%
    mutate(Fin_mouvement = if_else(
      is.na(Fin_séjour) & (IPP != lead(IPP, default = "")), # Condition: Fin_séjour is NA and IPP is not equal to the next row
      as.POSIXct(Sys.Date()),                               # Action: Replace Fin_mouvement with today's date as POSIXct
      Fin_mouvement                                         # Otherwise, keep the existing value
    ))
  
  # Replace "Fin_mouvement" for the last row if "Fin_séjour" is NA
  new_table <- new_table %>%
    mutate(Fin_mouvement = if_else(
      is.na(Fin_séjour) & row_number() == nrow(new_table),      # Condition: Fin_séjour is NA and it's the last row
      as.POSIXct(Sys.Date()),                               # Action: Replace Fin_mouvement with today's date as POSIXct
      Fin_mouvement                                         # Otherwise, keep the existing value
    ))
  
  return(new_table)
}

replace_short_moves <- function(table, threshold){
  # Replace moves shorter than the threshold by extending the previous or next move
  new_table <- table %>%
    group_by(IPP, Séjour) %>%  # Group by IPP and Séjour
    mutate(
      # Convert columns to character (necessary for next operations)
      Département = as.character(Département),
      Service = as.character(Service),
      Unité_fonctionelle = as.character(Unité_fonctionelle),
      Unité_de_soins = as.character(Unité_de_soins),
      
      # Set Département, Service, Unité_fonctionelle, Unité_de_soins to NA if Durée_mouvement is below threshold
      Département = ifelse(Durée_mouvement < threshold, NA, Département),
      Service = ifelse(Durée_mouvement < threshold, NA, Service),
      Unité_fonctionelle = ifelse(Durée_mouvement < threshold, NA, Unité_fonctionelle),
      Unité_de_soins = ifelse(Durée_mouvement < threshold, NA, Unité_de_soins),
      
      # Convert columns to factor
      Département = as.factor(Département),
      Service = as.factor(Service),
      Unité_fonctionelle = as.factor(Unité_fonctionelle),
      Unité_de_soins = as.factor(Unité_de_soins)
    ) %>%
    
    # Use fill() to propagate the next available non-NA value downward
    fill(Département, Service, Unité_fonctionelle, Unité_de_soins, .direction = "downup") %>%
    
    # Remove rows with any NA values in the relevant columns
    drop_na(Département, Service, Unité_fonctionelle, Unité_de_soins) %>%
    
    ungroup()  # Ungroup after operations
  
  return(as.data.frame(new_table))
}

string_to_color <- function(strings) {
  # Generate unique hashes for each string
  hashes <- sapply(strings, function(x) digest(x, algo = "md5", serialize = FALSE))
  
  # Convert hash to numeric values
  nums <- sapply(hashes, function(h) {
    # Take the first 6 characters (hex) and convert to decimal
    as.numeric(strtoi(substr(h, 1, 6), 16L))
  })
  
  # Normalize to a 0-1 range and generate RGB colors
  colors <- rgb(
    (nums %% 255) / 255,                  # Red channel
    ((nums %/% 255) %% 255) / 255,        # Green channel
    ((nums %/% (255 * 255)) %% 255) / 255 # Blue channel
  )
  
  names(colors) <- strings
  
  return(colors)
}

summary_table <- function(moves_table, sampling_table){
  # Generate the statistics table display in the statistics tab
  
  # Basic information 
  summary_table <- moves_table %>%
    dplyr::group_by(IPP) %>%
    dplyr::summarize(Nombre_de_séjours = n_distinct(Séjour),
                     Services_visités = n_distinct(Service),
                     Unités_de_soins_visités = n_distinct(Unité_de_soins),
                     Temps_total = round(sum(Durée_mouvement, na.rm = T),1))
  
  # Add genotype information if the table is present
  if (!is.null(sampling_table)){
    
    # Add IPP with no moves if present
    IPP_no_moves <- setdiff(unique(sampling_table$IPP), unique(moves_table$IPP))
    if (length(IPP_no_moves) > 0){
      IPP_no_moves_table <- cbind.data.frame(IPP = IPP_no_moves,
                                             Nombre_de_séjours = rep(NA, length(IPP_no_moves)),
                                             Services_visités = rep(NA, length(IPP_no_moves)),
                                             Unités_de_soins_visités = rep(NA, length(IPP_no_moves)),
                                             Temps_total = rep(NA, length(IPP_no_moves)))
      
      summary_table <- rbind.data.frame(summary_table, IPP_no_moves_table)
    }
    
    # Count tables for positive and negative samples
    positive_samples <- sampling_table %>%
      dplyr::group_by(IPP, PRELEVEMENT) %>%
      dplyr::summarize(Freq=n()) %>%
      dplyr::filter(PRELEVEMENT=="Positif") %>%
      dplyr::select(-PRELEVEMENT) %>%    
      dplyr::rename(Echantillons_positifs = Freq)
    
    negative_samples <- sampling_table %>%
      dplyr::group_by(IPP, PRELEVEMENT) %>%
      dplyr::summarize(Freq=n()) %>%
      dplyr::filter(PRELEVEMENT=="Négatif") %>%
      dplyr::select(-PRELEVEMENT) %>%    
      dplyr::rename(Echantillons_négatifs = Freq)
    
    summary_table <- summary_table %>%
      dplyr::left_join(positive_samples, by="IPP") %>%
      dplyr::left_join(negative_samples, by="IPP") %>%
      dplyr::mutate(Echantillons_négatifs = replace_na(Echantillons_négatifs, 0))
  }
  
  # Rename column with unit
  colnames(summary_table) <- gsub("Temps_total", "Temps_total_(jours)", colnames(summary_table))
  
  return(summary_table)
}

update_date <- function(session, table, samplings){
  # Update the date range widget considering both moves and samplings dates
  
  # Gather all dates
  all_dates <- c(table$Début_mouvement, table$Fin_mouvement, samplings$DATE_PRELEVEMENT)
  
  # Update the widget
  updateDateRangeInput(session, "DateRange", start=min(all_dates)-150000, 
                       end=max(all_dates)+150000)
}

update_patients_widgets <- function(session, IPP_list){
  # Update patient picker and selected patients widgets
  
  updatePickerInput(session, "patientPicker", choices = IPP_list,
                    selected = IPP_list,
                    choicesOpt = list(style = rep("color:black;", length(IPP_list))))
  updatePickerInput(session, "highlightPicker", choices = IPP_list,
                    selected = NA,
                    choicesOpt = list(style = rep("color:black;", length(IPP_list))))
  updatePickerInput(session, "findPatient_network", choices = IPP_list,
                    selected = NA,
                    choicesOpt = list(style = rep("color:black;", length(IPP_list))))
}