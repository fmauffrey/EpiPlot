generate_network_data <- function(time_unit, detailed_button, table, 
                                  network_unit, colors_vector, 
                                  indirect_time=14, length_edges=150,
                                  size_font_edges=20){
  # Generate network data for plotting or extracting label info on selected
  
  # Import filtered data
  data <- as.data.frame(table)
  data$IPP <- as.character(data$IPP)
  
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
    all_colors_levels <- levels(data[,network_unit])
    edge_colors_code <- match(net_edges$color, all_colors_levels)
    edges_colors <- data.frame(color=colors_vector[edge_colors_code],
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
    net_edges$color <- colors_vector[edge_colors_code]
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

genotype_count_table <- function(table){
  # Return a table with the count of each DLST
  
  table <- dplyr::distinct(.data = table, IPP, .keep_all = T)
  ST_count <- table(gsub(" ", "", unlist(strsplit(table$Génotype, ","))))
  ST_count <- as.data.frame(ST_count)
  colnames(ST_count) <- c("DLST", "Count")
  
  return(ST_count)
}

# Define a function to convert a nested list to a data frame
nested_list_to_df <- function(nested_list) {
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