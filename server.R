# Server ######################################################################
server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    timeout = 120,
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
      min_date <- min(moves_data()$Début_mouvement)
      max_date <- max(moves_data()$Fin_mouvement)
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
  network_nodes <- reactiveVal()
  selectedNodes <- reactiveVal()
  
  observe({
    if (!is.null(input$network_selected)){
      selectedNodes(network_nodes()[input$network_selected,"label"])
    } else {
      selectedNodes(NULL)
    }
  })
  
  observeEvent(input$getNodes, {
    if (!is.null(selectedNodes())){
      shinyalert(title="IPP", type="info", html = T, closeOnClickOutside=T, 
                 showConfirmButton=T, confirmButtonText="Filtrer ces IPP",
                 text=HTML(paste(selectedNodes(), collapse = "<br/>")),
                 callbackR = function(x){ if(x != F) network_update()})
    }
  })
  
  network_update <- function(patients){
    updatePickerInput(session, "patientPicker", selected = selectedNodes())
    updatePickerInput(session, "findPatient_network", choices = selectedNodes(), selected = NA)
  }
  
  observeEvent(input$findPatient_network, {
    if (!is.null(network_nodes())){
      focus_id <- network_nodes()[network_nodes()$label==input$findPatient_network,"id"]
      if (length(focus_id) == 1){
        visNetworkProxy("network") %>%
          visFocus(id = focus_id)
      }
    }
  })
  
  # Event for changing patient selected 
  observeEvent(input$patientPicker, {
    updatePickerInput(session, "findPatient_network", choices = input$patientPicker,
                      selected = NULL)
  })
  
  # Modal for choosing plot size before saving
  observeEvent(input$download_moves_button, {
    show_alert(
      title = "Exportation du graphique",
      html = TRUE,
      width = "20%",
      btn_labels = NA,
      closeOnClickOutside=T,
      text = tags$div(
        sliderTextInput(
          inputId = "plot_width",
          label = "Largeur",
          hide_min_max=T,
          selected="1024",
          choices = c("256", "512", "768", "1024", "2048")
        ),
        sliderTextInput(
          inputId = "plot_height",
          label = "Hauteur", 
          hide_min_max=T,
          selected="512",
          choices = c("256", "512", "768", "1024", "2048")
        ),
        radioGroupButtons("moves_plot_format",
                          choiceNames = c("SVG", "PNG"),
                          choiceValues = c(".svg", ".png"),
                          status = "primary",
                          individual = T,
                          size = "lg"),
        downloadBttn("ganttDl", label = "Exporter", style = "material-flat",
                     ,size="lg")
      ))})
  
  # Add indirect links on network
  observeEvent(input$update_edges_button, {

    if (!is.null(input$Data_mouvements)){
      if (input$update_edges_button=="Avec"){
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
                                            indirect_time = input$IndirectLinkTime)
      
      visNetworkProxy("network") %>%
        visUpdateEdges(edges = network_data[[4]])
      
      disable("IndirectLinkTime")
      
      } else if ((input$update_edges_button=="Sans")){
        visNetworkProxy("network") %>%
          visGetEdges(input = "network_edges_remove")
        
        enable("IndirectLinkTime")
      }}
    })
  
  observeEvent(input$network_edges_remove, {
    current_edges <- nested_list_to_df(input$network_edges_remove)
    edges_to_remove <- current_edges[which(current_edges$dashes==TRUE),"id"]
    visNetworkProxy("network") %>%
      visRemoveEdges(id = edges_to_remove)
    })
  
  # Enable/disable selection focus on network
  observeEvent(input$network_focus_trigger, {
    if (input$network_focus_trigger == TRUE){
      visNetworkProxy("network") %>%
        visOptions(highlightNearest = list(enabled = TRUE))
    } else {
      visNetworkProxy("network") %>%
        visOptions(highlightNearest = list(enabled = FALSE))
    }
  })
  
  # Enable/disable gravity on network
  observeEvent(input$network_gravity_trigger, {
    if (input$network_gravity_trigger == TRUE){
      visNetworkProxy("network") %>%
        visPhysics(enabled = T)
    } else {
      visNetworkProxy("network") %>%
        visPhysics(enabled = F)
    }
  })
  
  # Options for network
  observe({
    visNetworkProxy("network") %>%
      visNodes(font = list(size = input$SizeNodes)) %>%
      visSelectNodes(id = input$highlightPicker_network)
  })
  
  # Load moves table and format ###############################################
  moves_data <- reactive({
    
    # Format moves table
    table <- format_moves_table(input$Data_mouvements$datapath) 
  
    # Replace end date for stays without end date
    table <- replace_no_end(table)
    
    # Update date range widget
    updateDateRangeInput(session, "DateRange", start=min(table$Début_mouvement)-150000, 
                         end=max(table$Fin_mouvement)+150000)
    
    # Update all widgets based on patients list
    update_patients_widgets(session, table)
    
    return(table)
  })
  
  # Add sampling information to the main table ################################
  sampling_data <- reactive({
    
    # Import raw data table
    table <- moves_data()
    
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
                              Département=NA,
                              Service=NA,
                              Unité_fonctionelle=NA,
                              Unité_de_soins=NA)
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
    
    # Add the sampling ordering choice in sorting widget
    updateSelectInput(session, "ganttOrder",
                      choices = list("Prélèvements" = "Prelevements",
                                     "Admission" = "Début_mouvement", 
                                     "IPP" = "IPP"),
                      selected = "Prelevements")
    
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
    
    # Update patient picker and highlighted patients widgets
    update_patients_widgets(session, table)

    return(table)
  })
  
  # Date, patient, unit filter data ###########################################
  filtered_data <- reactive({
    
    # Load table depending if sampling data was given or not
    if (is.null(input$Data_sampling)){
      data_table <- as.data.frame(moves_data())
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
    new_order <- NA
    if (order_var == "IPP"){
      new_order <- unique(sort(filt_data$IPP))
    } else if (order_var == "Début_mouvement") {
      new_order <-  unique(filt_data$IPP[order(filt_data$Début_mouvement)])
    } else if (order_var == "Prelevements") {
      sample_table <- sampling_data_plot()
      sample_table_first_pos <- sample_table %>% 
        dplyr::group_by(IPP) %>%
        dplyr::filter(Prélèvements == "Positif") %>%
        dplyr::filter(DATE_PRELEVEMENT == min(DATE_PRELEVEMENT)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()
      no_pos <- setdiff(sample_table$IPP, sample_table_first_pos$IPP)
      new_order <- c(sample_table_first_pos$IPP[order(sample_table_first_pos$DATE_PRELEVEMENT)], no_pos)
    }
    filt_data$IPP <- factor(filt_data$IPP, levels = new_order)

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
    
    # Filter by genotype (spurtious spaces removed before grep CORRECT ERROR SOURCE)
    selected_gentotype <- gsub(" ", "", input$genotypePicker)
    table_samp <- table_samp[grep(selected_gentotype, table_samp$CLUSTER),]
    
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
            panel.grid.minor.x = element_blank(),
            panel.grid.major = element_line(color="grey")) +
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
    if (!is.null(input$Data_sampling)){
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
    
    # Adapt legend title
    plot <- plot + labs(color=input$selectedUnit)
    
    # Change font for selected patients
    # Original IPP order is saved because gsub change factors for character and
    # looses the order chosen before
    initial_order <- levels(plot$data$IPP)
    selected_patients <- input$highlightPicker
    if (length(selected_patients)>0){
      for (p in selected_patients){
        plot$data$IPP <- gsub(p, paste0("<b>", p, "</b>"), plot$data$IPP)
        initial_order <- gsub(p, paste0("<b>", p, "</b>"), initial_order)
        if (!is.null(input$Data_sampling)) plot$layers[[2]]$data$IPP <- gsub(p, paste0("<b>", p, "</b>"), plot$layers[[2]]$data$IPP)
      }
    }
    plot$data$IPP <- factor(plot$data$IPP, levels=initial_order)
    
    # Convert to plotly object for displaying in the tab
    tab_plot <- ggplotly(plot, tooltip = "text")
    
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
                                          colors_vector = network_colors)

    # Save the nodes information in the reactive value
    network_nodes(as.data.frame(network_data[[1]]))
    
    # Set the number of column for the legend depending of the number of edges
    legend_data <- network_data[[3]]
    legend_data$font.size <- rep(40,nrow(legend_data))
    legend_data$width <- rep(30,nrow(legend_data))
    
    # Create network
    visNetwork(network_data[[1]], network_data[[2]]) %>%
      visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50)) %>%
      visLegend(addEdges = legend_data) %>% 
      visNodes(color = list(background = "lightblue", 
                            border = "darkblue",
                            highlight = c(background = "#f57f7f",
                                          border = "darkred")),
               font = list(size = 15)) %>%
      visLayout(randomSeed = 32) %>%
      visOptions(highlightNearest = list(enabled = TRUE)) %>%
      visEvents(selectNode = "function(nodes) {
        Shiny.onInputChange('network_selected', nodes.nodes);
      }",
                deselectNode = "function(nodes) {
          Shiny.onInputChange('network_selected', null);
        }")  %>%
      visInteraction(multiselect = T)
  })
  
  # Display moves if table loaded
  output$timeline <- renderPlotly({
    if (is.null(input$Data_mouvements))
      return(NULL)
    
    moves_plot() %>%
      
    # Modify plotly parameters to display
      config(moves_plot(),
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
    
    # Formatting table before displaying
    table <- table %>% mutate(Début_mouvement = format(Début_mouvement, "%Y-%m-%d %H:%M:%S"),
                              Fin_mouvement = format(Fin_mouvement, "%Y-%m-%d %H:%M:%S"))
    colnames(table) <- gsub("_", " ", colnames(table))
    
    box(width = NULL,
        DT::renderDT(table,
                     options = list(pageLength = 18,
                                    lengthChange = F,
                                    searching = F
                                    )
                     )
        )
  })
  
  # Saving buttons Gantt plot
  # For filename, a function is needed to reevaluate the genotype selected each time the button is pressed
  output$ganttDl <- downloadHandler(filename = function(){
    if (input$genotypePicker=="") {
      paste0("mouvement", input$moves_plot_format) 
      } else {
        paste0(as.character(input$genotypePicker), "_", format(Sys.time(), "%y%m%e"), input$moves_plot_format)}
  },
  content = function(file){
    temp_file <- tempfile(fileext = input$moves_plot_format)
    
    # Save the plot as an image to the temporary file (necessary for writing on server)
    save_image(moves_plot(), temp_file,
               width = input$plot_width,
               height = input$plot_height)
    
    # Copy the temporary file to the final destination
    file.copy(temp_file, file)
  })
}