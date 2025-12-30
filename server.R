


# ///O>\\\ \\\O>//// ///O>\\\ \\\O>//// 
#       ...  THIS IS SCAN V2 ...                          
# ///O>\\\ \\\O>//// ///O>\\\ \\\O>////

library(shiny)
library(shinydashboard)
library(dplyr)
library(igraph)
library(tidygraph)
library(tidyr)
library(ggraph)
library(readr)
library(sf)
library(ggplot2)
library(leaflet)
library(units)
library(lwgeom)      
library(RColorBrewer)
library(DT)          
library(stringr)
library(shinyjs)

# ---- 🚀 GLOBAL SETTINGS ---
options(shiny.maxRequestSize = 500 * 1024^2) 
sf::sf_use_s2(FALSE)

# ---- GLOBAL FUNCTION: CHUNK CALCULUS ----

calculate_chunk_cs_engine <- function(species_chunk, all_shapes, areas_df) {
    # 1. Filtra o Chunk
    shapes_chunk <- all_shapes |> dplyr::filter(sp %in% species_chunk)
    
    # 2. Intersecção
    intersections <- sf::st_intersection(shapes_chunk, all_shapes)
    
    # 3. Limpeza e Cálculo
    cs_chunk <- intersections |>
        dplyr::filter(sp != sp.1) |>
        dplyr::mutate(area_overlap = sf::st_area(geometry)) |>
        sf::st_drop_geometry() |>
        dplyr::select(sp1 = sp, sp2 = sp.1, area_overlap) |>
        # Juntar áreas
        dplyr::left_join(areas_df, by = c("sp1" = "sp")) |>
        dplyr::rename(area_sp1 = area_sp) |>
        dplyr::left_join(areas_df, by = c("sp2" = "sp")) |>
        dplyr::rename(area_sp2 = area_sp) |>
        # Fórmula
        dplyr::mutate(Cs = (as.numeric(area_overlap) / as.numeric(area_sp1)) * (as.numeric(area_overlap) / as.numeric(area_sp2))) |>
        dplyr::select(sp1, sp2, Cs) |>
        dplyr::as_tibble()
    
    return(cs_chunk)
}


#    ---- SERVER START ----

server <- function(input, output, session) {
    
    # --- 1. REACTIVE STATE VALUES ---
    map_data       <- reactiveVal(NULL) # Stores the Master Shapefile (Projected)
    cs_matrix_data <- reactiveVal(NULL) # Stores the Cs Matrix (sp1, sp2, Cs)
    spp_choices    <- reactiveVal(NULL) # Stores the unique species list (Memory)
    
    
    # --- 2. MAP MANAGEMENT (Upload & Transform) ----
    
    # A. Initial Load
    observeEvent(input$filemap, {
        req(input$filemap)
        shpdf <- input$filemap
        tempdirname <- tempdir()
        
        # Copy files to temp
        for (i in 1:nrow(shpdf)) {
            file.copy(shpdf$datapath[i], file.path(tempdirname, shpdf$name[i]), overwrite = TRUE)
        }
        
        # Read Shapefile
        shp_file <- shpdf$name[grep("\\.shp$", shpdf$name)]
        req(length(shp_file) > 0)
        
        raw_shp <- st_read(file.path(tempdirname, shp_file), quiet = TRUE) %>% 
            st_transform(4326) %>% 
            st_make_valid()
        
        map_data(raw_shp)
        
        # Populate Memory
        target_col <- if("sp" %in% names(raw_shp)) "sp" else names(raw_shp)[1]
        new_list <- sort(unique(raw_shp[[target_col]]))
        spp_choices(new_list)
        
        # Update UI
        updateSelectizeInput(session, "map_spp_select", choices = new_list, server = TRUE)
        showNotification(paste("Map Uploaded. Found", length(new_list), "species."), type = "message")
    })
    
    # B. The Master Modifier (Project + Buffer)
    observeEvent(input$apply_mods, {
        req(map_data())
        showNotification("Applying Map Transformations...", type = "warning", duration = 4)
        
        wrk_shp <- map_data()
        
        # Step 1: Projection
        if (input$modify_crs == TRUE) {
            req(input$map_projection)
            wrk_shp <- tryCatch({
                wrk_shp %>% st_transform(input$map_projection)
            }, error = function(e) {
                showNotification("Invalid Projection!", type = "error")
                return(wrk_shp)
            })
        } else {
            if (st_crs(wrk_shp)$epsg != 4326) wrk_shp <- wrk_shp %>% st_transform(4326)
        }
        
        # Step 2: Buffering
        if (input$use_buffer_map == TRUE) {
            req(input$buffer_dist)
            
            # Area classification logic
            areas <- as.numeric(st_area(wrk_shp))
            qs <- quantile(areas, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)
            wrk_shp$area_class <- cut(areas, breaks = qs, labels = FALSE, include.lowest = TRUE)
            
            selected_qs <- as.numeric(input$quantiles_to_buffer)
            is_target <- wrk_shp$area_class %in% selected_qs
            
            if (any(is_target)) {
                target_shp <- wrk_shp[is_target, ]
                keep_shp   <- wrk_shp[!is_target, ]
                buffered_target <- st_buffer(target_shp, dist = input$buffer_dist)
                wrk_shp <- bind_rows(keep_shp, buffered_target)
            }
        }
        
        # Step 3: Validation & Save
        wrk_shp <- st_make_valid(wrk_shp)
        map_data(wrk_shp)
        
        # Update Memory
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        if (col_name %in% names(wrk_shp)) {
            new_spp <- sort(unique(wrk_shp[[col_name]]))
            spp_choices(new_spp)
            updateSelectizeInput(session, "map_spp_select", choices = new_spp, server = TRUE)
        }
        
        showNotification("Settings Applied & Species List Updated!", type = "message")
    })
    
    # C. Filtered Data () - Selected Species
    filtered_data <- reactive({
        req(map_data())
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        
        if (is.null(input$map_spp_select) || length(input$map_spp_select) == 0) {
            return(map_data())
        } else {
            return(map_data() %>% filter(.data[[col_name]] %in% input$map_spp_select))
        }
    })
    
    
    # --- 3. MAP DISPLAY (Cleaned - Single Observer) ----

    # A. Initialize Leaflet
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -45, lat = -15, zoom = 4)
    })
    
    # B. Map Updater
    observe({
        req(filtered_data())
        
        # Prepare WGS84 copy for display
        display_shp <- st_transform(filtered_data(), 4326)
        
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        if (!col_name %in% names(display_shp)) col_name <- names(display_shp)[1] 
        
        bb <- st_bbox(display_shp)
        
        leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(data = display_shp, 
                        color = "#18bc9c", 
                        weight = 2, 
                        fillOpacity = 0.4,
                        label = as.character(display_shp[[col_name]])) %>%
            flyToBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
    })
    
    # C. Reset Button
    observeEvent(input$btn_map_reset, {
        updateSelectizeInput(session, "map_spp_select", selected = character(0))
        showNotification("Map View Reset", type = "message")
    })
    
    # D. Info Outputs
    output$map_shp_names <- renderText({
        if(is.null(input$filemap)) return("Waiting for map upload...")
        paste("Columns:", paste(names(map_data()), collapse = ", "))
    })
    output$map_crs <- renderText({
        req(map_data())
        paste("Original CRS:", st_crs(map_data())$input)
    })
    
    
    # --- 4. CS MATRIX LOGIC ----
    
    # A. Upload Handler
    observeEvent(input$upload_cs_matrix, {
        req(input$upload_cs_matrix)
        df <- read.csv(input$upload_cs_matrix$datapath)
        
        required_cols <- c("sp1", "sp2", "Cs")
        if (!all(required_cols %in% names(df))) {
            showNotification("Error: CSV must have 'sp1', 'sp2', and 'Cs' columns.", type = "error")
            return()
        }
        cs_matrix_data(df)
        showNotification(paste("Matrix Uploaded:", nrow(df), "pairs."), type = "message")
    })
    
    # B. Calculation Engine (SF Serial)
    observeEvent(input$calculate_Cs, {
        req(map_data()) 
<<<<<<< HEAD
        
        # 1. PRE-PROCESSING
=======
>>>>>>> 7e7d9cec9725429806b2a5bcc5ff0b52a45d8da2
        showNotification("Preparing Data...", type = "message")
        shinyjs::runjs("$('#right_panel_container').show();") 
        
        shapes <- map_data() 
        areas_df <- shapes |> 
            mutate(area_sp = st_area(geometry)) |> 
            st_drop_geometry() |> 
            select(sp, area_sp)
        
        species_list <- unique(shapes$sp)
        final_cs <- NULL
        
        # --- Engine: SF / Serial / Chunked ---
        if (input$calc_engine == "engine_sf" && input$calc_mode == "mode_serial") {
            
            if (input$memory_strategy == "mem_chunk") {
                showNotification("Running: SF | Serial | Chunked", type = "message")
                chunk_size <- input$chunk_size
                chunks <- split(species_list, ceiling(seq_along(species_list) / chunk_size))
                results_list <- list()
                
                withProgress(message = 'Calculating Cs...', value = 0, {
                    for (i in seq_along(chunks)) {
                        results_list[[i]] <- calculate_chunk_cs_engine(chunks[[i]], shapes, areas_df)
                        incProgress(1/length(chunks), detail = paste("Batch", i, "of", length(chunks)))
                    }
                })
                final_cs <- bind_rows(results_list)
                
            } else { 
                # Load All
                showNotification("Running: SF | Serial | Full Load", type = "message")
                withProgress(message = 'Calculating Cs (Full)...', value = 0.5, {
                    final_cs <- calculate_chunk_cs_engine(species_list, shapes, areas_df)
                })
            }
        } else {
            showNotification("Other engines/modes under development", type = "warning")
            return()
        }
        
        # Post-Processing
        if (!is.null(final_cs)) {
            showNotification("Filtering & Cleaning Table...", type = "message")
            final_cs_clean <- final_cs |>
                filter(Cs >= input$filter_Cs) |>
                rowwise() |> 
                mutate(key = paste(sort(c(sp1, sp2)), collapse = "_")) |>
                ungroup() |>
                distinct(key, .keep_all = TRUE) |>
                select(-key) |>
                arrange(desc(Cs))
            
            cs_matrix_data(final_cs_clean)
            showNotification("Calculation Finished! Check Right Panel.", type = "message", duration = 5)
        }
    })
    
    # --- 5. SCAN ENGINE (V1 LOGIC) ----
    
    scan_graph <- eventReactive(input$run_scan, {
        req(cs_matrix_data())
        
        # 1. Inputs
        df_cs <- cs_matrix_data() 
        thresholds <- seq(input$threshold_min, input$threshold_max, by = input$resolution)
        
        # 2. Initial Graph
        cs_filtered <- df_cs %>% filter(Cs >= input$threshold_min)
        g_full <- as_tbl_graph(cs_filtered, directed = FALSE)
        chorotypes_df <- data.frame()
        
        # 3. Loop
        withProgress(message = 'Running SCAN Analysis...', value = 0, {
            for(ct in thresholds) {
                incProgress(1/length(thresholds), detail = paste("Ct:", ct))
                
                # Filter Edges & Nodes
                g_temp <- g_full %>%
                    activate(edges) %>% filter(Cs >= ct) %>%
                    activate(nodes) %>% mutate(degree = centrality_degree()) %>% filter(degree > 0)
                
                # Find Components
                comps <- g_temp %>% activate(nodes) %>% mutate(component_id = group_components()) %>% as_tibble()
                if(nrow(comps) == 0) next
                
                comp_list <- split(comps$name, comps$component_id)
                
                # Check Validity
                for(cid in names(comp_list)) {
                    spp_in_group <- comp_list[[cid]]
                    if(length(spp_in_group) < 2) next
                    
                    is_valid <- TRUE
                    
                    # Clique Check
                    if(isTRUE(input$overlap)) {
                        g_sub <- g_temp %>% filter(name %in% spp_in_group)
                        if(igraph::edge_density(g_sub) < 1) is_valid <- FALSE
                    }
                    
                    # Diameter Check
                    if(is_valid && isTRUE(input$filter_diameter)) {
                        g_sub <- g_temp %>% filter(name %in% spp_in_group)
                        if(igraph::diameter(g_sub) > input$max_diameter) is_valid <- FALSE
                    }
                    
                    # Store Result
                    if(is_valid) {
                        chorotypes_df <- rbind(chorotypes_df, data.frame(
                            Threshold = ct,
                            Chorotype_ID = paste0("Ct", ct, "_G", cid),
                            Species = I(list(spp_in_group)), 
                            N_Species = length(spp_in_group)
                        ))
                    }
                }
            }
        })
        
        # 4. Packaging
        results <- list()
        
        if(nrow(chorotypes_df) > 0) {
            chorotypes_long <- chorotypes_df %>% 
                tidyr::unnest(Species) %>%
                mutate(Species = as.character(Species))
        } else {
            chorotypes_long <- data.frame(Threshold=numeric(), Chorotype_ID=character(), Species=character(), N_Species=integer())
        }
        
        results[['chorotypes']] <- chorotypes_long
        results[['parameters']] <- data.frame(Min_Ct = input$threshold_min, Max_Ct = input$threshold_max, Resolution = input$resolution)
        results[['graph']] <- g_full
        results[['graph_nodes']] <- g_full %>% activate(nodes) %>% as_tibble()
        results[['graph_edges']] <- g_full %>% activate(edges) %>% as_tibble()
        
        return(results)
    })
    
    # --- 6. UI OUTPUTS & RENDERERS ----
     
    # A. SCAN Preview Table (Main Window)
    output$table_download_preview <- DT::renderDT({
        req(scan_graph())
        df <- scan_graph()[['chorotypes']]
        DT::datatable(df, options = list(pageLength = 35, scrollX = TRUE))
    })
    
    # B. SCAN Summary Text
    output$scan_summary_text <- renderUI({
        req(scan_graph())
        df <- scan_graph()[['chorotypes']]
        n_groups <- length(unique(df$Chorotype_ID))
        HTML(paste0("<b>", n_groups, " Chorotypes</b> found"))
    })
    
    # C. SCAN Results sidebar
    output$scan_chorotype_list <- renderTable({
        req(scan_graph())
        df <- scan_graph()[['chorotypes']]
        
        # Clean Table for Sidebar Display
        df %>% 
            dplyr::select(Group = Chorotype_ID, Species) %>% 
            # Optional: Shorten Group Name if needed (e.g. Ct0.5_G1 -> G1)
            dplyr::mutate(Group = sub(".*_G", "G", Group)) %>%
            head(100) # Safety limit for display speed
        
    }, striped = TRUE, hover = TRUE, width = "100%", spacing = "xs")
    
    # D. Helper Flags
    output$cs_data_available <- reactive({ !is.null(cs_matrix_data()) && nrow(cs_matrix_data()) > 0 })
    outputOptions(output, "cs_data_available", suspendWhenHidden = FALSE)
    
    output$scan_results_ready <- reactive({ !is.null(scan_graph()) })
    outputOptions(output, "scan_results_ready", suspendWhenHidden = FALSE)
    
    # --- 7. RIGHT PANEL (CONTEXT AWARE - WITH DOWNLOADS) ----
    
    output$right_panel_container <- renderUI({
        top_lvl <- input$top_nav
        sub_lvl <- input$analysis_subtabs
        alpha <- if(is.null(input$panel_opacity)) 0.85 else input$panel_opacity
        panel_content <- NULL
        panel_title <- ""
        
        if (!is.null(top_lvl) && top_lvl == "SCAN Analysis") {
            
            # --- CASE A: MAP TAB ---
            if (!is.null(sub_lvl) && sub_lvl == "Map") {
                panel_title <- "Map Filters"
                panel_content <- tagList(
                    p(class="text-muted", "Select species to highlight."),
                    selectizeInput("map_spp_select", NULL, choices = spp_choices(), multiple = TRUE, options = list(placeholder = "Select species...")),
                    actionButton("btn_map_reset", "Reset View", icon = icon("refresh"), size = "xs", style = "width: 100%; margin-top: 5px;")
                )
                
                # --- CASE B: CS TAB ----
            } else if (!is.null(sub_lvl) && sub_lvl == "Cs") {
                has_data <- !is.null(cs_matrix_data())
                panel_title <- "Matrix Inspector"
                
                panel_content <- tagList(
                    if(has_data) {
                        tagList(
                            div(style="margin-bottom: 10px;", 
                                p(strong("Dimensions:"), paste(nrow(cs_matrix_data()), "rows")),
                                
                                # --- NEW: DOWNLOAD BUTTON BEFORE TABLE ---
                                downloadButton("dl_cs_sidebar", "Download Matrix", 
                                               class = "btn-success btn-xs", 
                                               style = "width: 100%; margin-bottom: 15px;")
                            ),
                            hr(),
                            p(strong("Top Connected Pairs:")),
                            tableOutput("mini_nodes_table") 
                        )
                    } else {
                        p(class="text-warning", icon("exclamation-circle"), " No Matrix Calculated yet.")
                    }
                )
                
                # --- CASE C: SCAN TAB ----
            } else if (!is.null(sub_lvl) && sub_lvl == "SCAN") {
                has_matrix <- !is.null(cs_matrix_data())
                has_results <- FALSE
                try({ if(exists("scan_graph") && !is.null(scan_graph())) has_results <- TRUE }, silent=TRUE)
                
                panel_title <- "SCAN Status"
                
                if (!has_matrix) {
                    panel_content <- tagList(div(style="text-align: center; color: #e74c3c;", icon("exclamation-triangle", "fa-3x"), h4("Matrix Missing")))
                } else {
                    panel_content <- tagList(
                        p(class="text-muted", icon("info-circle"), " Configure parameters in the main window."),
                        
                        if(has_results) {
                            tagList(
                                hr(),
                                # --- NEW: DOWNLOAD BUTTON BEFORE TABLE ---
                                downloadButton("dl_chorotypes_sidebar", "Download Groups", 
                                               class = "btn-success btn-xs", 
                                               style = "width: 100%; margin-bottom: 15px;"),
                                
                                h4(style="color: #2c3e50;", icon("list-ol"), " Results Summary"),
                                tableOutput("mini_scan_summary"),
                                
                                hr(),
                                p(strong("Chorotypes List:")),
                                div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd;",
                                    tableOutput("scan_chorotype_list")
                                )
                            )
                        } else {
                            div(style="text-align: center;", p("Waiting for Analysis..."))
                        }
                    )
                }
                # --- CASE D: SCAN VIEWER (Visual Exploration) ---
            } else if (!is.null(top_lvl) && top_lvl == "SCAN Viewer") {
                
                # 1. Check if we have results to view
                has_results <- FALSE
                try({ if(exists("scan_graph") && !is.null(scan_graph())) has_results <- TRUE }, silent=TRUE)
                
                panel_title <- "Viewer Controls"
                
                if(!has_results) {
                    # 🔴 STOP: No Analysis
                    panel_content <- tagList(
                        div(style="text-align: center; color: #e74c3c; padding: 20px;",
                            icon("ban", "fa-3x"),
                            h4("No Analysis Found"),
                            p("Please run the SCAN Analysis first.")
                        )
                    )
                } else {
                    # 🟢 READY: The Controls
                    
                    # <<<<<<<<<<< HERE WE SHOULD TAKE THE PARAMETERS FROM CS TABLE?
                    
                    # Get Min/Max/Step from the parameters used in calculation
                    params <- scan_graph()[['parameters']]
                    
                    panel_content <- tagList(
                        
                        # 1. The Threshold Slider (Hierarchy Level 1)
                        div(style="background: #ecf0f1; padding: 10px; border-radius: 5px;",
                            h5(icon("sliders-h"), " 1. Select Threshold (Ct)"),
                            sliderInput("viewer_threshold", NULL, 
                                        min = params$Min_Ct, 
                                        max = params$Max_Ct, 
                                        value = params$Min_Ct, 
                                        step = params$Resolution)
                        ),
                        
                        # 2. The Group Selector (Hierarchy Level 2)
                        # This is dynamic: it changes based on the slider above
                        h5(icon("layer-group"), " 2. Select Chorotypes"),
                        p(class="text-muted", style="font-size: 0.9em;", "Chorotypes at this Ct:"),
                        
                        # The UI Output from Server Part B
                        uiOutput("viewer_group_selector"),
                        
                        # hr(),
                        
                        # 3. Visual Toggles (Optional polish)
                        checkboxInput("viewer_show_labels", "Show Species Labels", value = TRUE),
                        sliderInput("viewer_alpha", "Map Opacity", 0, 1, 0.5, 0.1)
                    )
                }
            }
        }
        
        
        
        
        
        
        # Render Sidebar HTML
        if (!is.null(panel_content)) {
            sidebar_style <- paste0("position: fixed; top: 50px; right: 0; bottom: 0; width: 280px; background-color: rgba(255, 255, 255, ", alpha, "); z-index: 1050; display: flex; flex-direction: column;")
            div(style = sidebar_style,
                div(style = "padding: 15px; background: rgba(44, 62, 80, 1.0); color: white;", h4(style="margin: 0;", icon("cogs"), " ", panel_title), tags$i(class="fa fa-times pull-right", style="cursor: pointer;", onclick = "$('#right_panel_container').hide()")),
                div(style = "padding: 15px; overflow-y: auto; flex-grow: 1;", panel_content)
            )
        } else { return(NULL) }
    })
    
    # --- 8. SMALL TABLES (Used in Right Panel) ----
    
    # E. Mini Table for Cs (Top 25)
    output$mini_nodes_table <- renderTable({
        req(cs_matrix_data())
        df <- cs_matrix_data()
        if (nrow(df) == 0) return(data.frame(Status = "No data"))
        
        df %>% ungroup() %>% head(25) %>%
            dplyr::select(Sp1 = sp1, Sp2 = sp2, Cs) %>%
            dplyr::mutate(Cs = sprintf("%.3f", as.numeric(Cs)))
    }, width = "100%", hover = TRUE, bordered = TRUE, striped = TRUE)
    
    # F. Mini Table for SCAN Results (Count of groups)
    output$mini_scan_summary <- renderTable({
        req(scan_graph())
        df <- scan_graph()[['chorotypes']]
        data.frame(Metric = c("Total Groups", "Rows"), Value = c(length(unique(df$Chorotype_ID)), nrow(df)))
    }, colnames = FALSE, width = "100%", bordered = TRUE)
    
    
    # --- 9. DOWNLOADS ----
    
    output$dl_map <- downloadHandler(
        filename = function() { paste0("SCAN_Map_Export_", Sys.Date(), ".zip") },
        content = function(file) {
            req(map_data())
            temp_dir <- tempdir()
            file_base <- "scan_map_export"
            sf::st_write(map_data(), file.path(temp_dir, paste0(file_base, ".shp")), delete_dsn = TRUE, quiet = TRUE)
            zip_files <- list.files(temp_dir, pattern = file_base, full.names = TRUE)
            utils::zip(zipfile = file, files = zip_files, flags = "-j")
        }, contentType = "application/zip"
    )
    
    # 2. Cs Matrix Download Handlers
    cs_h <- downloadHandler(
        filename = function() { paste0("SCAN_Cs_", Sys.Date(), ".csv") },
        content = function(file) { write.csv(cs_matrix_data(), file, row.names = FALSE) }
    )
    output$dl_cs <- cs_h         # Main window button
    output$dl_cs_sidebar <- cs_h # <--- NEW SIDEBAR BUTTON
    # output$dl_cs_float <- cs_h # (Deleted, no longer needed)
    
    # 3. Chorotypes Download Handlers
    choro_h <- downloadHandler(
        filename = function() { paste0("SCAN_Groups_", Sys.Date(), ".csv") },
        content = function(file) { 
            req(scan_graph())
            # Save the list format or the summary format
            df <- scan_graph()[['chorotypes']]
            write.csv(df, file, row.names = FALSE) 
        }
    )
    
    ###################### in obras  >>>>
    
    # --- VIEWER ----
    
    # A. UI DO SELETOR DE CHORÓTIPOS (NO PAINEL FLUTUANTE)
    output$chorotype_selector_global <- renderUI({
        req(scan_graph())
        df <- scan_graph()[['chorotypes']]
        
        # Filtra grupos disponíveis baseado no slider GLOBAL
        disp_ct <- input$threshold_global
        available_groups <- df |> 
            filter(abs(Threshold - disp_ct) < 0.001) |> 
            pull(Chorotype_ID) |> 
            unique()
        
        if(length(available_groups) == 0) return(helpText("No chorotypes at this threshold."))
        
        # Formata nomes para ficar bonito (Ct0.5_G1 -> Group 1)
        group_nums <- gsub(".*_G", "", available_groups)
        names(available_groups) <- paste("Group", group_nums)
        
        checkboxGroupInput("selected_chorotypes_global", NULL, # Label vazio pois já tem título no UI
                           choices = available_groups,
                           selected = available_groups[1:min(3, length(available_groups))], 
                           inline = TRUE)
    })
    
    # B. REACTIVE UNIFICADO (DADOS FILTRADOS)
    
    # V1 Este objeto alimenta o Mapa, o GGplot, o Grafo e a Tabela simultaneamente
    g_sub <- reactive({
        req(scan_graph(), input$threshold_global, input$selected_chorotypes_global)
        
        # 1. Identificar espécies dos grupos selecionados
        df <- scan_graph()[['chorotypes']]
        selected_spp <- df |> 
            filter(Chorotype_ID %in% input$selected_chorotypes_global) |>
            pull(Species) |>
            unique()
        
        req(length(selected_spp) > 0)
        
        # 2. Filtrar o grafo completo
        g_full <- scan_graph()[['graph']]
        
        g_view <- g_full |>
            activate(edges) |>
            filter(Cs >= input$threshold_global) |> # Usa Threshold Global
            activate(nodes) |>
            filter(name %in% selected_spp) |>        # Usa Espécies selecionadas
            mutate(comps = group_components())        # Recalcula componentes locais para cor
        
        return(g_view)
    })
    
    
    # GGPLOT MAP - ggplot_map()
    output$ggplot_map <- renderPlot({
        req(map_data(), chorotype_pal())
        
        ggplot(g_map()) +
            geom_sf(aes(fill = as.factor(comps)), color = "black", size = 0.2, alpha = input$alpha_global) +
            scale_fill_manual(values = chorotype_pal(), name = "Group") +
            theme_minimal() +
            labs(title = paste("Spatial Distribution (Ct =", input$threshold_global, ")"))
    })
    
    # GRAPH PLOT - graph_plot()
    output$graph_plot <- renderPlot({
        req(g_sub(), chorotype_pal())
        
        lay <- create_layout(g_sub(), layout = input$layout_graph)
        
        ggraph(lay) +
            geom_edge_link(aes(alpha = Cs), width = 1, show.legend = FALSE) +
            geom_node_point(aes(fill = as.factor(comps)), size = 5, shape = 21, color = "black") +
            scale_fill_manual(values = chorotype_pal()) +
            geom_node_text(aes(label = name), repel = TRUE, size = 3) +
            theme_graph() +
            theme(legend.position = "none")
    })
    
    # PALETA DE CORES UNIFICADA
    chorotype_pal <- reactive({
        req(g_sub())
        comps_presentes <- g_sub() |> activate(nodes) |> as_tibble() |> pull(comps) |> unique() |> sort()
        
        n_cores <- length(comps_presentes)
        if(n_cores < 3) n_cores <- 3
        
        pal_name <- input$palette_global
        cores <- suppressWarnings(RColorBrewer::brewer.pal(n = n_cores, name = pal_name))
        
        if(length(comps_presentes) > length(cores)) {
            cores <- colorRampPalette(cores)(length(comps_presentes))
        } else {
            cores <- cores[1:length(comps_presentes)]
        }
        
        names(cores) <- comps_presentes
        return(cores)
    })
    
    # 4. TABELA DE ESPÉCIES (ABA 2)
    output$view_species_table <- renderDT({
        req(scan_graph(), input$threshold_global, input$selected_chorotypes_global)
        
        # Pegamos o dataframe original dos resultados (muito mais seguro)
        df_completo <- scan_graph()[['chorotypes']]
        
        # Filtramos usando os inputs do Painel Global
        df_filtrado <- df_completo |> 
            filter(abs(Threshold - input$threshold_global) < 0.001) |> # Filtra Ct
            filter(Chorotype_ID %in% input$selected_chorotypes_global) |> # Filtra Grupos
            select(Ct = Threshold, Chorotype = Chorotype_ID, Species, N_Species) |>
            arrange(Chorotype, Species)
        
        datatable(df_filtrado, 
                  options = list(pageLength = 10, scrollX = TRUE), 
                  rownames = FALSE)
    })
    
    
    # --- 10. SCAN VIEWER LOGIC (The Visual Engine) ----
   
    # A. Dynamic Checkbox Generator (Feeds Right Panel)
    output$viewer_group_selector <- renderUI({
        req(scan_graph())
        
        # 1. Get Data
        df <- scan_graph()[['chorotypes']]
        current_ct <- input$viewer_threshold
        
        # 2. Filter groups that exist at this specific threshold
        # We use a small epsilon for float comparison safety
        available_groups <- df %>% 
            dplyr::filter(abs(Threshold - current_ct) < 0.001) %>%
            dplyr::pull(Chorotype_ID) %>%
            unique()
        
        if(length(available_groups) == 0) {
            return(helpText("No chorotypes formed exactly at this Threshold."))
        }
        
        # 3. Beautify Names (Ct0.5_G1 -> Group 1)
        display_names <- gsub(".*_G", "Chor. ", available_groups)
        names(available_groups) <- display_names
        
        # 4. Create Checkboxes
        checkboxGroupInput("viewer_selected_groups", NULL,
                           choices = available_groups,
                           # Select the first one by default so the screen isn't empty
                           selected = available_groups[1], 
                           inline = TRUE)
    })
    
    # B. Helper: The Sub-Graph (Filtered Network)
    viewer_sub_graph <- reactive({
        req(scan_graph(), input$viewer_threshold, input$viewer_selected_groups)
        
        # 1. Identify species in the selected groups
        df <- scan_graph()[['chorotypes']]
        
        selected_spp <- df %>% 
            filter(Chorotype_ID %in% input$viewer_selected_groups) %>% 
            pull(Species) %>% 
            unique()
        
        req(length(selected_spp) > 0)
        
        # 2. Filter the Master Graph
        g_full <- scan_graph()[['graph']]
        
        g_view <- g_full %>%
            activate(edges) %>%
            filter(Cs >= input$viewer_threshold) %>% # Filter weak links
            activate(nodes) %>%
            filter(name %in% selected_spp) %>%       # Keep only selected species
            mutate(comps = group_components())       # Recalculate IDs for coloring
        
        return(g_view)
    })
    
    # C. Helper: The Map Data (Joined with Shapes)
    viewer_map_data <- reactive({
        req(viewer_sub_graph(), map_data())
        
        # Get species names from the filtered graph
        spp_names <- viewer_sub_graph() %>% activate(nodes) %>% pull(name)
        
        # Filter the Master Shapefile
        # Note: We must join with the graph data to know which "Group" each shape belongs to
        node_data <- viewer_sub_graph() %>% activate(nodes) %>% as_tibble() %>% select(name, comps)
        
        map_final <- map_data() %>% 
            filter(sp %in% spp_names) %>%
            left_join(node_data, by = c("sp" = "name"))
        
        return(map_final)
    })
    
    # D. Helper: Consistent Palette
    viewer_palette <- reactive({
        req(viewer_sub_graph())
        
        # Find unique groups in the current view
        grps <- viewer_sub_graph() %>% activate(nodes) %>% as_tibble() %>% pull(comps) %>% unique() %>% sort()
        
        # Generate distinct colors
        n_colors <- length(grps)
        if(n_colors < 3) n_colors <- 3
        
        # Use Set1 or Paired for distinct groups
        cols <- suppressWarnings(RColorBrewer::brewer.pal(n = n_colors, name = "Set1"))
        
        # Interpolate if we have too many groups
        if(length(grps) > length(cols)) {
            cols <- colorRampPalette(cols)(length(grps))
        } else {
            cols <- cols[1:length(grps)]
        }
        
        names(cols) <- grps
        return(cols)
    })
    
    # --- OUTPUTS (For the Main Window) ---
    
    # 1. The Map (Left Column)
    output$ggplot_map <- renderPlot({
        req(viewer_map_data(), viewer_palette())
        
        ggplot(viewer_map_data()) +
            geom_sf(aes(fill = as.factor(comps)), color = "black", size = 0.2, alpha = input$viewer_alpha) +
            scale_fill_manual(values = viewer_palette(), name = "Group") +
            theme_minimal() +
            theme(legend.position = "bottom") +
            labs(title = paste("Distribution (Ct =", input$viewer_threshold, ")"))
    })
    
    # 2. The Network (Right Column)
    output$graph_plot <- renderPlot({
        req(viewer_sub_graph(), viewer_palette())
        
        # Layout: Nicely or Kamada-Kawai
        lay <- create_layout(viewer_sub_graph(), layout = "nicely")
        
        p <- ggraph(lay) +
            geom_edge_link(aes(alpha = Cs), width = 1, show.legend = FALSE) +
            geom_node_point(aes(fill = as.factor(comps)), size = 5, shape = 21, color = "black") +
            scale_fill_manual(values = viewer_palette()) +
            theme_graph() +
            theme(legend.position = "none")
        
        # Add labels if checked
        if(isTRUE(input$viewer_show_labels)) {
            p <- p + geom_node_text(aes(label = name), repel = TRUE, size = 4, fontface="bold")
        }
        
        return(p)
    })
    
    # 3. The Table (Bottom Row)
    output$view_species_table <- DT::renderDT({
        req(viewer_map_data())
        
        viewer_map_data() %>%
            sf::st_drop_geometry() %>%
            select(Species = sp, Group_ID = comps) %>%
            arrange(Group_ID, Species)
        
    }, options = list(pageLength = 5, scrollX = TRUE))
    
    
    
    
    
} # End Server



# THRASH ----
# # --- 7. RIGHT PANEL (CONTEXT AWARE) ---
# 
# output$right_panel_container <- renderUI({
#     top_lvl <- input$top_nav
#     sub_lvl <- input$analysis_subtabs
#     alpha <- if(is.null(input$panel_opacity)) 0.85 else input$panel_opacity
#     panel_content <- NULL
#     panel_title <- ""
#     
#     if (!is.null(top_lvl) && top_lvl == "SCAN Analysis") {
#         
#         # --- MAP TAB ---
#         if (!is.null(sub_lvl) && sub_lvl == "Map") {
#             panel_title <- "Map Filters"
#             panel_content <- tagList(
#                 p(class="text-muted", "Select species to highlight."),
#                 selectizeInput("map_spp_select", NULL, choices = spp_choices(), multiple = TRUE, options = list(placeholder = "Select species...")),
#                 actionButton("btn_map_reset", "Reset View", icon = icon("refresh"), size = "xs", style = "width: 100%; margin-top: 5px;")
#             )
#             
#             # --- CS TAB ---
#         } else if (!is.null(sub_lvl) && sub_lvl == "Cs") {
#             has_data <- !is.null(cs_matrix_data())
#             panel_title <- "Matrix Inspector"
#             
#             panel_content <- tagList(
#                 if(has_data) p(strong("Dimensions:"), paste(nrow(cs_matrix_data()), "rows")) 
#                 else p(class="text-warning", icon("exclamation-circle"), " No Matrix Calculated yet."),
#                 hr(),
#                 p(strong("Top Connected Pairs:")),
#                 tableOutput("mini_nodes_table") # This matches Renderer E below
#             )
#             
#             # --- SCAN TAB ---
#             # --- SCAN TAB (Status & Results) ---
#         } else if (!is.null(sub_lvl) && sub_lvl == "SCAN") {
#             
#             # Check Dependencies
#             has_matrix <- !is.null(cs_matrix_data())
#             has_results <- FALSE
#             try({ if(exists("scan_graph") && !is.null(scan_graph())) has_results <- TRUE }, silent=TRUE)
#             
#             panel_title <- "SCAN Status"
#             
#             if (!has_matrix) {
#                 # 🔴 STOP: No Matrix
#                 panel_content <- tagList(
#                     div(style="text-align: center; color: #e74c3c; padding: 20px;", 
#                         icon("exclamation-triangle", "fa-3x"), 
#                         h4("Matrix Missing"),
#                         p("Please calculate the Cs Index first.")
#                     )
#                 )
#             } else {
#                 # 🟡 READY
#                 panel_content <- tagList(
#                     p(class="text-muted", icon("info-circle"), " Configure parameters in the main window."),
#                     hr(),
#                     
#                     # 🟢 SUCCESS: Results
#                     if(has_results) {
#                         tagList(
#                             # 1. Summary Metrics
#                             h4(style="color: #2c3e50; margin-bottom: 5px;", icon("chart-pie"), " Summary"),
#                             tableOutput("mini_scan_summary"),
#                             
#                             hr(style="margin: 10px 0;"),
#                             
#                             # 2. Detailed List (Species | Group)
#                             h4(style="color: #2c3e50; margin-bottom: 5px;", icon("list"), " Chorotypes List"),
#                             p(class="text-muted", style="font-size: 0.85em;", "Scroll to view all assignments:"),
#                             
#                             # Scrollable Container for the list
#                             div(style = "max-height: 400px; overflow-y: auto; border: 1px solid #ecf0f1; border-radius: 4px;",
#                                 tableOutput("scan_chorotype_list")
#                             )
#                         )
#                     } else {
#                         div(style="text-align: center; color: #7f8c8d; padding-top: 20px;",
#                             icon("hourglass-half", "fa-2x"),
#                             p("Waiting for Analysis...")
#                         )
#                     }
#                 )
#             }
#         }
#     }
#     
#     # Render Sidebar HTML
#     if (!is.null(panel_content)) {
#         sidebar_style <- paste0("position: fixed; top: 50px; right: 0; bottom: 0; width: 280px; background-color: rgba(255, 255, 255, ", alpha, "); z-index: 1050; display: flex; flex-direction: column;")
#         div(style = sidebar_style,
#             div(style = "padding: 15px; background: rgba(44, 62, 80, 1.0); color: white;", h4(style="margin: 0;", icon("cogs"), " ", panel_title), tags$i(class="fa fa-times pull-right", style="cursor: pointer;", onclick = "$('#right_panel_container').hide()")),
#             div(style = "padding: 15px; overflow-y: auto; flex-grow: 1;", panel_content)
#         )
#     } else { return(NULL) }
# })


