# Server ######################################################################
server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Global variables and events ###############################################
  
  # Manual date range update buttons - Minus 365 days
  observeEvent(input$bttnDateFilter365, {
    if (!is.null(input$Data_mouvements)){
      updateDateRangeInput(session, "DateRange", start=Sys.Date()-365)
    }
  })
  
  # Manual date range update buttons - Reset
  observeEvent(input$bttnDateFilterReset, {
    if (!is.null(input$Data_mouvements)){
      min_date <- min(raw_data()$Début_mouvement)
      max_date <- max(raw_data()$Fin_mouvement)
      # The table loaded changes if sampling data loaded or not
      if (is.null(input$Data_sampling)){
        updateDateRangeInput(session, "DateRange", start=min_date-150000, end=max_date+150000)
      } else {
        updateDateRangeInput(session, "DateRange", start=min_date, end=max_date+150000)
      }
    }
  })
  
  # Observe events for getting nodes selected on network and display them
  # + Reactive variable to store selected nodes
  observeEvent(input$getNodes, {
    visNetworkProxy("network") %>%
      visGetSelectedNodes()
  })
  
  selectedNodes <- reactiveVal(NULL)
  observeEvent(input$network_selectedNodes, {
    selectedNodes(input$network_selectedNodes)
  })
  
  observeEvent(selectedNodes(), {
    if (!is.null(input$network_selectedNodes)){
      selected_nodes <- input$network_selectedNodes
      nodes <- as.data.frame(generate_network_data(time_unit = "days", 
                                                   detailed_button = input$NetworkDetailed,
                                                   table = filtered_data(),
                                                   network_unit = input$selectedUnit,
                                                   colors_vector = colors_vector)[[1]])
      selection <- nodes[selected_nodes,"label"]
      shinyalert(title="IPP", type="info", html = T, closeOnClickOutside=T, 
                 showConfirmButton=T, confirmButtonText="Filtrer ces IPP",
                 text=HTML(paste(selection, collapse = "<br/>")),
                 callbackR = function(x){ if(x != F) network_update(selection)})
    }
  })
  
  # Update selected patients from network alert
  network_update <- function(patients){
    updatePickerInput(session, "patientPicker", selected = patients)
  }
  
  # Load patient table ########################################################
  raw_data <- reactive({
    
    # First read of the table to find the number of rows to lead
    table <- read_excel(input$Data_mouvements$datapath, skip = 7)
    rows_number <- nrow(table) - 2 # 2 useless rows at the end
    
    # Load the table
    table <- read_excel(input$Data_mouvements$datapath, skip = 8,
                        col_names = c("IPP", "Séjour", "Début_séjour", 
                                      "Fin_séjour", "Début_mouvement", 
                                      "Fin_mouvement", "Département", 
                                      "Service", "Unité_fonctionelle", 
                                      "Unité_de_soins", "Durée_mouvement"),
                        n_max = rows_number)
    
    # Convert into appropriate type
    table <- table %>% mutate(IPP = as.character(IPP),
                              Séjour = as.factor(Séjour),
                              Début_mouvement = as.POSIXct(Début_mouvement), 
                              Fin_mouvement = as.POSIXct(Fin_mouvement),
                              Fin_séjour = as.POSIXct(Fin_séjour),
                              Département = as.factor(Département),
                              Service = as.factor(Service),
                              Unité_fonctionelle = as.factor(Unité_fonctionelle),
                              Unité_de_soins = as.factor(Unité_de_soins))
    
    # For patients still at the hospital, replace last movement date with current date
    replacement <- 0 # Changes count
    for (line in 1:nrow(table)){
      if (is.na(table[line,"Fin_séjour"])){ # If no end date
        if (line!=nrow(table)){ # If not the last line, check for following line
          if (!(table[line,"IPP"]==table[line+1,"IPP"])){
            table[line, "Fin_mouvement"] <- Sys.Date()
            replacement <- replacement +1
          }
        } else {
          table[line, "Fin_mouvement"] <- Sys.Date()
          replacement <- replacement +1
        }
      }
    }
    
    # Pop-up if replacements occurred
    if (replacement > 0){
      shinyalert(title="Date(s) manquante(s)",
                 type="info",
                 closeOnClickOutside = T,
                 text=paste0(replacement, " séjour(s) sans date de sortie. La date du jour sera utilisée."))
    }
    
    # Remove useless columns
    table <- subset(table, select = -c(Séjour, Début_séjour, Fin_séjour, Durée_mouvement))
    
    # Update date range widget
    updateDateRangeInput(session, "DateRange", start=min(table$Début_mouvement)-150000, 
                         end=max(table$Fin_mouvement)+150000)
    
    # Update patient picker widgets
    patients_list <- levels(factor(table$IPP))
    updatePickerInput(session, "patientPicker", choices = patients_list,
                      selected = patients_list,
                      choicesOpt = list(style = rep("color:black;", length(patients_list))))
    
    return(table)
  })
  
  # Add sampling information to the main table ################################
  sampling_data <- reactive({
    
    # Import raw data table
    table <- raw_data()
    
    # Load the table
    table_samp <- read_excel(input$Data_sampling$datapath, skip =1,
                             col_names = c("IPP", "PRELEVEMENT",
                                           "DATE_PRELEVEMENT", "CLUSTER"))
    
    # Add the AMBULATOIRE level to the relevant categories
    levels(table$Département) <- c(levels(table$Département), "AMB")
    levels(table$Service) <- c(levels(table$Service), "AMB")
    levels(table$Unité_fonctionelle) <- c(levels(table$Unité_fonctionelle), "AMB")
    levels(table$Unité_de_soins) <- c(levels(table$Unité_de_soins), "AMB")

    # Add IPP in main table if no moves for this IPP
    for (IPP in levels(factor(table_samp$IPP))){
      if (!(IPP %in% table$IPP)){
        date = c(table_samp[table_samp$IPP==IPP,"DATE_PRELEVEMENT"])
        new_row <- data.frame(IPP=IPP,
                              Début_mouvement=date,
                              fin_mouvement=date,
                              Département="AMB",
                              Service="AMB",
                              Unité_fonctionelle="AMB",
                              Unité_de_soins="AMB")
        colnames(new_row) <- colnames(table)
        table <- rbind.data.frame(table, new_row)
      }
    }

    # Convert into appropriate type
    table_samp <- as.data.frame(table_samp %>% mutate(IPP = as.character(IPP),
                                                      PRELEVEMENT = as.factor(PRELEVEMENT),
                                                      DATE_PRELEVEMENT = as.POSIXct(DATE_PRELEVEMENT),
                                                      CLUSTER = as.character(CLUSTER)))

    # Add sampling column
    table$Génotype <- rep("Aucun", nrow(table))

    # Add the genotype to each IPP when present.
    # Regroups genotypes when multiple. Useless for MRSA as it is already 
    # presented this way but necessary for pseudomonas
    for (IPP in levels(factor(table_samp$IPP))){
      all_genotypes <- c()
      IPP_table <- table_samp[table_samp$IPP==IPP,]
      for (row in 1:nrow(IPP_table)){
        IPP_list <- str_split_1(IPP_table[row,"CLUSTER"], ", ")
        for (genotype in IPP_list){
          if (!genotype %in% all_genotypes){
            all_genotypes <- c(all_genotypes, genotype)
          }
        }
      }
      table[table$IPP==IPP,"Génotype"] <- paste(sort(all_genotypes), collapse = ", ")
    }

    # Import genotype count table and create variables for the picker
    genotype_count_table <- genotype_count_table(table)
    genotype_id <- genotype_count_table$DLST
    names(genotype_id) <- paste0(genotype_count_table$DLST, " (", genotype_count_table$Count ,")")
    
    # Update genotype picker widget
    updatePickerInput(session, "genotypePicker", 
                      choices = genotype_id,
                      selected = genotype_id[1],
                      choicesOpt = list(style = rep("color:black;", length(genotype_id))))
    
    return(table)
  })
  
  # Genotype filtered data ####################################################
  genotype_filtered_data <- reactive({
    
    # Import the table with the genotype information added
    table <- sampling_data()
    
    # Filter the table with the selected genotype
    table <- table[grep(input$genotypePicker, table$Génotype),]
    
    # Update date range widget
    updateDateRangeInput(session, "DateRange", start=min(table$Début_mouvement)-150000, 
                         end=max(table$Fin_mouvement)+150000)
    
    # Update patient picker widgets
    patients_list <- levels(factor(table$IPP))
    updatePickerInput(session, "patientPicker", choices = patients_list,
                      selected = patients_list,
                      choicesOpt = list(style = rep("color:black;", length(patients_list))))

    return(table)
  })
  
  # Date, patient, unit filter data ###########################################
  filtered_data <- reactive({
    
    # Load table depending if sampling data was given or not
    if (is.null(input$Data_sampling)){
      data_table <- as.data.frame(raw_data())
    } else {
      data_table <- as.data.frame(genotype_filtered_data())
    }
    
    # Filter by date
    filt_data <- filter_by_date(table = data_table,
                                start = as.POSIXct(input$DateRange[1]), 
                                end = as.POSIXct(input$DateRange[2]))
    
    # Filter by patient 
    filt_data <- filt_data[which(filt_data$IPP %in% input$patientPicker),]
    
    # Changing samples order for plot
    order_var <- input$ganttOrder
    if (order_var == "IPP"){
      filt_data$IPP <- factor(filt_data$IPP, levels = unique(sort(filt_data$IPP)))
    } else if (order_var == "Début_mouvement") {
      filt_data$IPP <- factor(filt_data$IPP, levels = unique(filt_data$IPP[order(filt_data$Début_mouvement)]))
    }

    return(filt_data)
  })
  
  # Return the filtered sampling data for Gantt plot ##########################
  sampling_data_plot <- reactive({
    
    # Load the table
    table_samp <- read_excel(input$Data_sampling$datapath, skip =1,
                             col_names = c("IPP", "Prélèvements",
                                           "DATE_PRELEVEMENT", "CLUSTER"))
    
    # Convert into appropriate type
    table_samp <- as.data.frame(table_samp %>% mutate(IPP = as.character(IPP),
                                                      Prélèvements = as.character(Prélèvements),
                                                      DATE_PRELEVEMENT = as.POSIXct(DATE_PRELEVEMENT),
                                                      CLUSTER = as.character(CLUSTER)))
    
    # Change text for sampling
    table_samp[table_samp$Prélèvements=="POSITIVE","Prélèvements"] <- "Positif"
    table_samp[table_samp$Prélèvements=="NEGATIVE","Prélèvements"] <- "Négatif"
    
    # Filter by genotype
    table_samp <- table_samp[grep(input$genotypePicker, table_samp$CLUSTER),]
    
    # Filter by date
    table_samp <- table_samp[(which(table_samp$DATE_PRELEVEMENT>=as.POSIXct(input$DateRange[1]))),]
    table_samp <- table_samp[(which(table_samp$DATE_PRELEVEMENT<=as.POSIXct(input$DateRange[2]))),]
    
    # Filter by patient
    table_samp <- table_samp[which(table_samp$IPP %in% input$patientPicker),]

    return(table_samp)
  })
  
  # Moves plot ################################################################
  moves_plot <- reactive({
    
    # Return nothing if no data loaded
    if (is.null(input$Data_mouvements))
      return(NULL)
    
    # Import filtered data 
    plot_data <- as.data.frame(filtered_data())
    
    # Main plot
    plot <- ggplot(plot_data, 
                   aes(x=Début_mouvement, xend=Fin_mouvement, y=IPP, yend=IPP, 
                       color=plot_data[,input$selectedUnit],
                       text = paste("IPP: ",IPP,
                                    "\nDébut mouvement:", format(Début_mouvement, "%d-%m-%Y"),
                                    "\nFin mouvement:", format(Fin_mouvement, "%d-%m-%Y"),
                                    "\nUnité:", plot_data[,input$selectedUnit])))
    
    # Parameters
    plot <- plot + geom_segment(linewidth=input$segmentSize) +
      theme_bw()+ 
      theme(axis.title = element_blank(), 
            legend.position = "right",
            axis.text = element_text(size=15),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            panel.grid.minor.x = element_blank()) +
      ggtitle(paste0(input$genotypePicker, " | ", 
                     format(input$DateRange[1], "%d-%m-%Y"), " à ", 
                     format(input$DateRange[2], "%d-%m-%Y"))) + 
      guides(color=guide_legend(title=gsub("_", " ", input$selectedUnit)))
    
    # Change colors if 15 categories or less
    selected_level <- factor(plot_data[,input$selectedUnit])
    if (length(levels(selected_level)) <= 15){
      plot <- plot + scale_color_manual(values = colors_vector[1:length(levels(selected_level))])
    }
    
    # Add sampling points if set
    if (input$ganttSampling == TRUE & !is.null(input$Data_sampling)){
      # Import sampling data
      sampling_data <- sampling_data_plot()
      
      # Add columns for shape and colors
      plot <- plot + geom_point(data = sampling_data, 
                                aes(x=DATE_PRELEVEMENT, y=IPP, 
                                    shape=Prélèvements, fill=Prélèvements,
                                    text = paste0("IPP: ",IPP,
                                                  "\nDate de prélèvement: ",DATE_PRELEVEMENT,
                                                  "\n", Prélèvements)), 
                                size=input$DotSize, inherit.aes = F) + 
        scale_shape_manual(values=c(23, 21), breaks = c("Positif", "Négatif")) + 
        scale_fill_manual(values=c("#d4171e", "#17d417"), breaks = c("Positif", "Négatif"))
    }
    
    # Selection of the axis scale
    if (input$scaleType == "semester"){
      scale_axis <- "3 months"
    } else {
      scale_axis <- paste("1", input$scaleType)
    }
    plot <- plot + scale_x_datetime(date_breaks = scale_axis)
    
    # Legend
    plot <- plot + labs(color=input$selectedUnit)
    
    plot
  })
  
  # Moves plot plotly display #################################################
  # Modification of the plotly object for the display in the window
  moves_plot_tab <- reactive({
    plot_data <- as.data.frame(filtered_data())
    tab_plot <- ggplotly(moves_plot(),
                         tooltip = "text")
    
    # Change legend title
    tab_plot$x$layout$legend$title$text <- gsub("Prélèvements", "", tab_plot$x$layout$legend$title$text)
    tab_plot$x$layout$legend$title$text <- gsub("<br />", "", tab_plot$x$layout$legend$title$text)
    
    # Change legend groups and modify names
    for (n in 1:length(tab_plot$x$data)){
      name <- str_extract(tab_plot$x$data[[n]]$name, "[a-zA-Z0-9à-ü]{1,}")
      tab_plot$x$data[[n]]$name <- name
      if (name %in% c("Positif", "Négatif")){
        tab_plot$x$data[[n]]$legendgroup <- name
      } else {
        tab_plot$x$data[[n]]$legendgroup <- "Mouvement"
      }
    }
    
    tab_plot
  })
  
  # Network plot ##############################################################
  network_plot <- reactive({
    
    # Return nothing if no data loaded
    if (is.null(input$Data_mouvements))
      return(NULL)
    
    # Import table
    table <- filtered_data()
    number_units <- length(levels(factor(table[,input$selectedUnit])))
    
    # Define colors depending on the number of different units
    if (number_units <= 15){
      network_colors <- colors_vector
    } else {
      network_colors <- hue_pal()(number_units)
    }
    
    network_data <- generate_network_data(time_unit = "hour",
                                          table = table,
                                          network_unit = input$selectedUnit,
                                          colors_vector = network_colors,
                                          indirect_button = input$IndirectLinks,
                                          indirect_time = input$IndirectLinkTime)

    # Return nothing if no link
    if (is.null(network_data))
      return(NULL)
    
    # Create network
    visNetwork(network_data[[1]], network_data[[2]]) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visLegend(addEdges = network_data[[3]]) %>%
      visInteraction(multiselect = T) %>% 
      visNodes(color = list(background = "lightblue", 
                            border = "darkblue")) %>%
      visLayout(randomSeed = 32)
  })
  
  # Wards plot ################################################################
  wards_plot <- reactive({
    
    # Load the filtered table
    table <- as.data.frame(filtered_data())
    
    # Unit selected
    unit_selected <- input$selectedUnit
    
    # Generate plot data
    ward_data <- wards_plot_data(table, unit_selected)
    
    # Base plot
    plot <- ggplot(ward_data, aes(x=Ward, y=Days, fill=Ward)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      geom_text(aes(y = label_pos,
                    label=paste(Patients, "patient(s)"))) +
      theme(legend.position = "none") +
      labs(x=NULL, y="Nombre de jours (moyenne)") +
      ggtitle("Temps d'occupation moyen par unité")
    
    # Change colors depending on the number of wards
    base_color <- hue_pal()(length(levels(as.factor(ward_data$Ward))))
    other_color <- colors_vector[1:nrow(ward_data)]
    
    if (length(levels(ward_data$Ward)) <= 15){
      new_colors <- other_color[order(ward_data$Days, decreasing = T)]
      plot <- plot + scale_fill_manual(values=new_colors) 
    } else {
      new_colors <- base_color[order(ward_data$Days, decreasing = T)]
      plot <- plot + scale_fill_manual(values=new_colors) 
    }
    
    plot
  })
  
  # Display moves if table loaded
  output$timeline <- renderPlotly({
    if (is.null(input$Data_mouvements))
      return(NULL)
    
    moves_plot_tab() %>%
      
    # Modify plotly parameters to display
      config(moves_plot_tab(),
             toImageButtonOptions= list(filename = paste0(as.character(input$genotypePicker),
                                                          "_",
                                                          format(Sys.time(), "%e-%m-%y"))),
             edits=list(legendPosition = T),
             displaylogo = F)
  })
  
  # Display Network
  output$network <- renderVisNetwork({network_plot()})
  
  # Display table
  output$table <- renderUI({
    if (is.null(input$Data_mouvements))
      return(NULL)
    
    table <- filtered_data()
    box(width = NULL,
        DT::renderDT(table,
                        options = list(pageLength = 18,
                                       lengthChange = F,
                                       searching = F
                        )))
  })
  
  # Display wards barplot
  output$wards <- renderPlotly({
    
    # Return nothing if no data loaded
    if (is.null(input$Data_mouvements))
      return(NULL)
    
    wards_plot()
  })
  
  # Saving buttons Gantt plot
  # For filename, a function is needed to reevaluate the genotype selected each time the button is pressed
  output$ganttDlSVG <- downloadHandler(filename = function(){
    if (input$genotypePicker=="") "mouvement.svg" else paste0(as.character(input$genotypePicker),
                                                              "_",
                                                              format(Sys.time(), "%e-%m-%y"),
                                                              ".svg")
  },
  content = function(file){
    ggsave(file, moves_plot(), device = "svg",
           width = 4096,
           height = 2048,
           units = "px")
  })
}