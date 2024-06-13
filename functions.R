generate_network_data <- function(time_unit, detailed_button, table, 
                                  network_unit, colors_vector){
  # Generate network data for plotting or extracting label info on selected
  
  # Import filtered data
  data <- as.data.frame(table)
  data$IPP <- as.character(data$IPP)
  
  # Update the different factors levels with the currently selected data
  data <- droplevels(data)
  
  # If there is only one patient, return nothing
  if (length(levels(factor(data$IPP)))<2){
    shinyalert(text=HTML("<p style='font-weight:bold; font-size:2vh'>Un seul patient sélectionné</p>"), 
               type = "info", html = T, closeOnEsc = T, 
               closeOnClickOutside = T, showConfirmButton = F)
    return(NULL)
  }
  
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
  
  # If there is no connections, return nothing
  if (nrow(connections)<1){
    shinyalert(text=HTML("<p style='font-weight:bold; font-size:2vh'>Aucun lien entre ces patients</p>"), 
               type = "error", html = T, closeOnEsc = T, 
               closeOnClickOutside = T, showConfirmButton = F)
    return(NULL)
  }
  
  # Aggregate connections and count the occurrence (detailed)
  net_edges <- connections
  
  # Prepare data for visNetwork
  net_edges <- net_edges %>% mutate(from=as.character(from),
                                    to=as.character(to),
                                    label=as.character(label))
  
  nodes <- unique(data$IPP) # All IPP are considered
  net_nodes <- data_frame(id=1:length(nodes), label=as.character(nodes))
  
  net_edges$from <- match(net_edges$from, net_nodes$label, nomatch = 0)
  net_edges$to <- match(net_edges$to, net_nodes$label, nomatch = 0)
  
  # Create dataframe for the legend
  all_colors_levels <- levels(data[,network_unit])
  edge_colors_code <- match(net_edges$color, all_colors_levels)
  edges_colors <- data.frame(color=colors_vector[edge_colors_code],
                             label=net_edges$color)
  edges_colors <- edges_colors[!duplicated(edges_colors$color),]
  edges_colors$width <- rep(10, nrow(edges_colors))
  edges_colors$font.background <- rep("#ffffff", nrow(edges_colors))
  edges_colors$arrows <- rep("NULL", nrow(edges_colors))
  
  # Add custom characteristics to nodes
  net_nodes$shape <- rep("circle", nrow(net_nodes))
  
  # Add custom characteristics to edges
  net_edges$width <- rescale(as.numeric(net_edges$label), c(2, 20))
  net_edges$length <- rep(150, nrow(net_edges))
  net_edges$color.color <- rep("#818281", nrow(net_edges))
  net_edges$font.size <- rep(20, nrow(net_edges))
  net_edges$font.background <- rep("#ffffff", nrow(net_edges))
  net_edges$color <- colors_vector[edge_colors_code]
  net_edges$dashes <- rep(FALSE, nrow(net_edges))

  # Return data
  return(list(net_nodes, net_edges, edges_colors))
}

generate_network_data2 <- function(time_unit, detailed_button, table, 
                                  network_unit, colors_vector, indirect_button=T,
                                  indirect_time=14){
  # Generate network data for plotting or extracting label info on selected
  
  # Import filtered data
  data <- as.data.frame(table)
  data$IPP <- as.character(data$IPP)
  
  # Update the different factors levels with the currently selected data
  data <- droplevels(data)
  
  # If there is only one patient, return nothing
  if (length(levels(factor(data$IPP)))<2){
    shinyalert(text=HTML("<p style='font-weight:bold; font-size:2vh'>Un seul patient sélectionné</p>"), 
               type = "info", html = T, closeOnEsc = T, 
               closeOnClickOutside = T, showConfirmButton = F)
    return(NULL)
  }
  
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
  
  # Add indirect links if activated
  if (indirect_button == T){
    
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
  }
  
  # If there is no connections, return nothing
  if (nrow(connections)<1){
    shinyalert(text=HTML("<p style='font-weight:bold; font-size:2vh'>Aucun lien entre ces patients</p>"), 
               type = "error", html = T, closeOnEsc = T, 
               closeOnClickOutside = T, showConfirmButton = F)
    return(NULL)
  }
  
  # Aggregate connections and count the occurrence (detailed)
  net_edges <- connections
  
  # Prepare data for visNetwork
  net_edges <- net_edges %>% mutate(from=as.character(from),
                                    to=as.character(to),
                                    label=as.character(label))
  
  nodes <- unique(data$IPP) # All IPP are considered
  net_nodes <- data_frame(id=1:length(nodes), label=as.character(nodes))
  
  net_edges$from <- match(net_edges$from, net_nodes$label, nomatch = 0)
  net_edges$to <- match(net_edges$to, net_nodes$label, nomatch = 0)
  
  # Create dataframe for the legend
  all_colors_levels <- levels(data[,network_unit])
  edge_colors_code <- match(net_edges$color, all_colors_levels)
  edges_colors <- data.frame(color=colors_vector[edge_colors_code],
                             label=net_edges$color)
  edges_colors <- edges_colors[!duplicated(edges_colors$color),]
  edges_colors$width <- rep(10, nrow(edges_colors))
  edges_colors$font.background <- rep("#ffffff", nrow(edges_colors))
  edges_colors$arrows <- rep("NULL", nrow(edges_colors))
  
  # Add custom characteristics to nodes
  net_nodes$shape <- rep("circle", nrow(net_nodes))
  
  # Add custom characteristics to edges
  net_edges$width <- rescale(as.numeric(net_edges$label), c(2, 20))
  net_edges$length <- rep(150, nrow(net_edges))
  net_edges$color.color <- rep("#818281", nrow(net_edges))
  net_edges$font.size <- rep(20, nrow(net_edges))
  net_edges$font.background <- rep("#ffffff", nrow(net_edges))
  net_edges$color <- colors_vector[edge_colors_code]
  net_edges$dashes <- rep(FALSE, nrow(net_edges))
  
  if (indirect_button==T){
    # Adapt indirect links
    net_edges[net_edges$label=="0","dashes"] = TRUE
    net_edges[net_edges$label=="0","width"] = 4
    net_edges[net_edges$label=="0","label"] = ""
    
    return(list(net_nodes, net_edges, edges_colors))
  }
  
  # Return data
  return(list(net_nodes, net_edges, edges_colors))
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

wards_plot_data <- function(table, unit_selected){
  # Create the plot data table with stats for each ward
  ward_data <- data.frame()
  for (ward in levels(factor(table[,unit_selected]))){
    subtable <- table[table[,unit_selected]==ward,]
    patients <- length(levels(factor(subtable$IPP)))
    time <- round(as.numeric(sum(subtable$Fin_mouvement - subtable$Début_mouvement))/patients,1)
    ward_data <- rbind.data.frame(ward_data, c(ward, time, patients))
  }
  colnames(ward_data) <- c("Ward", "Days", "Patients")
  ward_data$Days <- as.numeric(ward_data$Days)
  
  # Reorder and add label position relative to the plot dimension
  ward_data$Ward <- factor(ward_data$Ward, levels = ward_data$Ward[order(ward_data$Days, decreasing = T)])
  ward_data$label_pos <- ward_data$Days + (max(ward_data$Days)*0.02)
  
  return(ward_data)
}

genotype_count_table <- function(table){
  # Return a table with the count of each DLST
  
  table <- dplyr::distinct(.data = table, IPP, .keep_all = T)
  ST_count <- table(unlist(strsplit(table$Génotype, ",")))
  ST_count <- as.data.frame(ST_count)
  colnames(ST_count) <- c("DLST", "Count")
  
  return(ST_count)
}