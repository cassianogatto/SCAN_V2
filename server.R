
######################################################
#           THIS IS SCAN V2                          #
######################################################

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
library(lwgeom)      # Para corrigir geometrias (st_make_valid)
library(RColorBrewer)# Para a paleta de cores
library(DT)          # Para as tabelas modernas
library(stringr)

# --- 🚀 FIX: INCREASE UPLOAD LIMIT ---
# Set limit to 500 MB (Default is only 5MB)
options(shiny.maxRequestSize = 500 * 1024^2) 

# Disable spherical geometry for planar calculations
sf::sf_use_s2(FALSE)


# --- GLOBAL CALCULUS FUNCTION - CHUNKS ----
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



server <- function(input, output, session) {
    
    # =========================================================================
    #  MASTER DATA MANAGEMENT ---
    # =========================================================================
    
    map_data <- reactiveVal(NULL)
    
    cs_matrix_data <- reactiveVal(NULL)
    
    # 🧠 THE MEMORY BANK
    spp_choices <- reactiveVal(NULL)
    
    # Initial Load (Default WGS84)
    observeEvent(input$filemap, {
        req(input$filemap)
        shpdf <- input$filemap
        tempdirname <- tempdir()
        
        for (i in 1:nrow(shpdf)) {
            file.copy(shpdf$datapath[i], file.path(tempdirname, shpdf$name[i]), overwrite = TRUE)
        }
        
        shp_file <- shpdf$name[grep("\\.shp$", shpdf$name)]
        req(length(shp_file) > 0)
        
        # Load, Project to 4326, and Fix Topology
        raw_shp <- st_read(file.path(tempdirname, shp_file), quiet = TRUE) %>% 
            st_transform(4326) %>% 
            st_make_valid()
        
        map_data(raw_shp)
        
        # --- POPULATE THE MEMORY ---
        # 1. Try to find 'sp', otherwise grab the first column
        target_col <- if("sp" %in% names(raw_shp)) "sp" else names(raw_shp)[1]
        
        # 2. Extract and Sort
        new_list <- sort(unique(raw_shp[[target_col]]))
        
        # 3. Save to Memory (Reactive Value)
        spp_choices(new_list)
        
        showNotification(paste("Map Uploaded. Found", length(new_list), "species."), type = "message")
        
        # --- POPULATE THE RIGHT PANEL DROPDOWN ---
        spp_list <- unique(raw_shp$sp) %>% sort()
        updateSelectizeInput(session, "map_spp_select", 
                             choices = spp_list, 
                             server = TRUE)
        
        showNotification("Map Uploaded and Species List Ready", type = "message")
        
    })
    
    # --- 🚀 THE MISSING REACTIVE: filtered_data ---
    # This takes the master pool and filters it based on the right panel selection
    filtered_data <- reactive({
        req(map_data())
        
        # Determine which column to use (sp or custom)
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        
        # If the user hasn't selected any species, show the whole community
        if (is.null(input$map_spp_select) || length(input$map_spp_select) == 0) {
            return(map_data())
        } else {
            # Filter the master data
            return(map_data() %>% filter(.data[[col_name]] %in% input$map_spp_select))
        }
    })
    
    # THE MASTER MODIFIER (Project + Buffer)
    observeEvent(input$apply_mods, {
        req(map_data())
        
        showNotification("Applying Map Transformations...", type = "warning", duration = 4)
        
        # Start with the current data (or revert to raw if you prefer non-destructive)
        # For this workflow, we process the current state.
        wrk_shp <- map_data()
        
        # --- STEP 1: PROJECTION ---
        if (input$modify_crs == TRUE) {
            req(input$map_projection)
            wrk_shp <- tryCatch({
                wrk_shp %>% st_transform(input$map_projection)
            }, error = function(e) {
                showNotification("Invalid Projection! Keeping previous.", type = "error")
                return(wrk_shp)
            })
        } else {
            # Revert to WGS84 if unchecked
            if (st_crs(wrk_shp)$epsg != 4326) {
                wrk_shp <- wrk_shp %>% st_transform(4326)
            }
        }
        
        # --- STEP 2: BUFFERING (New Location) ---
        if (input$use_buffer_map == TRUE) {
            req(input$buffer_dist)
            
            showNotification("Applying Buffers...", type = "message")
            
            # A. Calculate Quartiles based on Area
            # Note: st_area returns units. as.numeric strips them for quantile calc.
            areas <- as.numeric(st_area(wrk_shp))
            qs <- quantile(areas, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)
            
            # B. Classify Polygons
            # We use 'cut' to assign 1-4. 
            # We add a tiny epsilon to the lowest break to include min values.
            wrk_shp$area_class <- cut(areas, breaks = qs, labels = FALSE, include.lowest = TRUE)
            
            # C. Apply Buffer Conditional on Class
            # We split, buffer selected, and bind back
            selected_qs <- as.numeric(input$quantiles_to_buffer)
            
            # Logic: If row is in selected quantile, buffer it. Else, keep geometry.
            # Using dplyr rowwise or simple if_else logic with st_buffer is tricky on vectors.
            # Safest is to modify the geometry column directly for the subset.
            
            is_target <- wrk_shp$area_class %in% selected_qs
            
            if (any(is_target)) {
                # Separate target and non-target
                target_shp <- wrk_shp[is_target, ]
                keep_shp   <- wrk_shp[!is_target, ]
                
                # Buffer the target
                buffered_target <- st_buffer(target_shp, dist = input$buffer_dist)
                
                # Recombine
                wrk_shp <- bind_rows(keep_shp, buffered_target)
            }
        }
        
        # --- STEP 3: FINAL VALIDATION ---
        wrk_shp <- st_make_valid(wrk_shp)
        
        # 1. Update the Master Reactive (Crucial: This saves your work!)
        map_data(wrk_shp)
        
        # 1. Identify the correct column name
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        
        # 2. Update Memory if column exists
        if (col_name %in% names(wrk_shp)) {
            new_list <- sort(unique(wrk_shp[[col_name]]))
            spp_choices(new_list) # <--- Save it!
        }
        
        # 2. Identify the correct column name
        col_name <- if(input$ID_column) input$colum_sp_map else "sp"
        
        # 3. Refresh the dropdown in case species names changed or polygons were merged
        new_spp <- unique(wrk_shp[[col_name]]) %>% sort()
        updateSelectizeInput(session, "map_spp_select", 
                             choices = new_spp, 
                             server = TRUE)
        
        showNotification("Settings Applied & Species List Updated!", type = "message")
    })
    
    # =========================================================================
    #  MAP DISPLAY (Leaflet Adapter) ----
    # =========================================================================
    
    # --- 1. INITIAL MAP SETUP (Must be active!) ---
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -45, lat = -15, zoom = 4)
    })
    
    
    # --- UNIFIED MAP DISPLAY ---
    observe({
        req(filtered_data())
        
        # 1. Create a display copy in WGS84
        display_shp <- st_transform(filtered_data(), 4326)
        
        # 2. Get the current label column
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        
        # Safety Check: Ensure the column actually exists before asking Leaflet to use it
        if (!col_name %in% names(display_shp)) {
            # Fallback to the first column if "sp" or custom name is missing
            col_name <- names(display_shp)[1] 
        }
        
        # 3. Calculate bounds
        bb <- st_bbox(display_shp)
        
        # 4. Update the map
        leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(data = display_shp, 
                        color = "#18bc9c", 
                        weight = 2, 
                        fillOpacity = 0.4,
                        # FIX IS HERE: Use standard R access [[ ]] without the tilde (~)
                        label = as.character(display_shp[[col_name]])) %>%
            flyToBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
    })
    
    
    # 5. Reset Button Logic (Inside the Right Panel)
    observeEvent(input$btn_map_reset, {
        updateSelectizeInput(session, "map_spp_select", selected = character(0))
        showNotification("Map View Reset", type = "message")
    })
    
    # # Initial Map Setup
    # output$map <- renderLeaflet({
    #     leaflet() %>%
    #         addProviderTiles(providers$CartoDB.Positron) %>%
    #         setView(lng = -45, lat = -15, zoom = 4)
    # })
    
    # --- Live Map Update (Leaflet Adapter) ----
    observe({
        req(map_data())
        
        # Prepare for Display (Must be WGS84)
        display_shp <- map_data()
        if (st_crs(display_shp)$epsg != 4326) {
            display_shp <- st_transform(display_shp, 4326)
        }
        
        # Fix for "lat2 missing" error: Calculate Box OUTSIDE the pipe
        bb <- st_bbox(display_shp)
        
        leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(data = display_shp, color = "#18bc9c", weight = 2, fillOpacity = 0.2) %>%
            flyToBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
    })
    
    # --- Whenever map_raw changes, it updates the background map
    observe({
        req(map_data())
        
        leafletProxy("map") %>%
            clearShapes() %>%
            addPolygons(data = map_data(), 
                        color = "#18bc9c", 
                        weight = 2, 
                        fillOpacity = 0.2,
                        label = ~"Raw Input Shape")  %>%
            
            flyToBounds(
                st_bbox(map_data())[1], st_bbox(map_data())[2],
                st_bbox(map_data())[3], st_bbox(map_data())[4]
            )
    })
    
    # --- If species are selected the maps change 
    
    
    
    
    # --- Workshop Outputs (Skeletons) ----
    output$map_shp_names <- renderText({
        if(is.null(input$filemap)) return("Waiting for map upload...")
        paste("Columns:", paste(names(map_data()), collapse = ", "))
    })
    
    output$map_crs <- renderText({
        req(map_data())
        paste("Original CRS:", st_crs(map_data())$input)
    })
    
    # ==============================================================================
    #  CONTEXT-AWARE RIGHT PANEL LOGIC
    # ==============================================================================
    
    output$right_panel_container <- renderUI({
        
        # 1. Get Navigation State (MATCH THE UI NAME!)
        top_lvl <- input$top_nav
        sub_lvl <- input$analysis_subtabs
        
        # Initialize content as NULL (hidden)
        panel_content <- tagList(
            p(class="text-muted", "Select species to highlight on the workshop map."),
            
            # 🚀 FIX: READ FROM MEMORY
            selectizeInput("map_spp_select", NULL, 
                           choices = spp_choices(), # <--- Directly reading the reactive value
                           multiple = TRUE, 
                           options = list(placeholder = "Select species...")),
            
            actionButton("btn_map_reset", "Reset View", icon = icon("refresh"), size = "xs")
        )
        panel_title <- ""
        
        # --- LOGIC TREE ---
        
        # CASE A: Top Level = "SCAN Analysis"
        if (!is.null(top_lvl) && top_lvl == "SCAN Analysis") {
            
            # Check Sub-Tabs
            if (!is.null(sub_lvl) && sub_lvl == "Map") {
                # --- MAP TOOLS ---
                panel_title <- "Map Filters"
                panel_content <- tagList(
                    p(class="text-muted", "Select species to highlight on the workshop map."),
                    selectizeInput("map_spp_select", NULL, choices = NULL, multiple = TRUE, 
                                   options = list(placeholder = "Select species...")),
                    actionButton("btn_map_reset", "Reset View", icon = icon("refresh"), size = "xs")
                )
                
            } else if (sub_lvl == "Cs") {
                # --- MATRIX TOOLS ---
                # Only show if matrix exists
                validate(need(cs_matrix_data(), "No Matrix Calculated"))
                
                panel_title <- "Matrix Inspector"
                panel_content <- tagList(
                    p(strong("Dimensions:"), paste(nrow(cs_matrix_data()), "x", ncol(cs_matrix_data()))),
                    hr(),
                    p("Top Connected Nodes:"),
                    tableOutput("mini_nodes_table") # You need to define this output below
                )
                
            } else if (sub_lvl == "SCAN") {
                # --- CHOROTYPE TOOLS ----
                panel_title <- "Chorotype Settings"
                panel_content <- tagList(
                    sliderInput("scan_ct_slider", "Ct Threshold:", min=0, max=1, value=0.5),
                    selectInput("scan_focus_group", "Highlight Group:", choices = c("All", "A", "B"))
                )
            }
            
            # CASE B: Top Level = "SCAN Viewer"
        } else if (!is.null(top_lvl) && top_lvl == "SCAN Viewer") {
            # --- VIEWER TOOLS ----
            panel_title <- "Viewer Controls"
            panel_content <- tagList(
                h5("Visual Layers"),
                checkboxInput("show_network", "Show Network", TRUE),
                checkboxInput("show_map", "Show Map Overlay", TRUE),
                hr(),
                selectizeInput("viewer_spp_search", "Find Species:", choices = NULL)
            )
        }
        
        # --- RENDER THE BOX (If content exists) ----
        if (!is.null(panel_content)) {
            div(class = "box box-solid box-primary", # Uses AdminLTE styling
                style = "background: rgba(255,255,255,0.9); box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
                
                # Header with Collapse Button
                div(class = "box-header with-border",
                    h3(class = "box-title", icon("cogs"), " ", panel_title),
                    div(class = "box-tools pull-right",
                        tags$button(type="button", class="btn btn-box-tool", 
                                    onclick = "$('#right_panel_body').slideToggle()",
                                    icon("minus"))
                    )
                ),
                
                # Body
                div(id = "right_panel_body", class = "box-body",
                    panel_content
                )
            )
        } else {
            return(NULL) # Hide panel completely on "About" or "Settings"
        }
    })
    
    output$mini_nodes_table <- renderTable({
        # Logic to find top connected nodes
    })
    
    # =========================================================================
    # --- THE NEW CS CALCULUS SKELETON ----
    # =========================================================================
    
    observeEvent(input$calculate_Cs, {
        req(map_data()) 
        
        # 1. PRE-PROCESSING
        showNotification("Preparing Data...", type = "message")
        
        # A. Get Data (Uses the Projected Master Data)
        shapes <- map_data() 
        
        # B. Pre-Calculate Areas 
        # If user applied Albers/Metric, st_area() now returns accurate METERS^2
        areas_df <- shapes |> 
            mutate(area_sp = st_area(geometry)) |> 
            st_drop_geometry() |> 
            select(sp, area_sp)
        
        # ... rest of your code remains exactly the same ...
        
        species_list <- unique(shapes$sp)
        n_species <- length(species_list)
        
        
        # 2. ROUTING LOGIC (The Switchboard)
        # ------------------------------------------------
        
        # >>> BRANCH 1: SF ENGINE (Standard Vector) <<<
        if (input$calc_engine == "engine_sf") {
            
            # --- Sub-Branch: SERIAL Mode ---
            if (input$calc_mode == "mode_serial") {
                
                # ... Strategy: CHUNKED (The "Old" Standard)
                if (input$memory_strategy == "mem_chunk") {
                    
                    showNotification("Running: SF | Serial | Chunked", type = "message")
                    
                    # 1. Define Chunks
                    chunk_size <- input$chunk_size
                    chunks <- split(species_list, ceiling(seq_along(species_list) / chunk_size))
                    
                    # 2. Initialize Storage
                    results_list <- list()
                    
                    # 3. Loop (Progress Bar)
                    withProgress(message = 'Calculating Cs (SF Serial)...', value = 0, {
                        for (i in seq_along(chunks)) {
                            
                            # CALL HELPER FUNCTION
                            chunk_res <- calculate_chunk_cs_engine(chunks[[i]], shapes, areas_df)
                            results_list[[i]] <- chunk_res
                            
                            incProgress(1/length(chunks), detail = paste("Batch", i, "of", length(chunks)))
                        }
                    })
                    
                    # 4. Bind Results # gem 21 dez 25
                    
                    final_cs <- bind_rows(results_list)
                    
                    # --- 5. POST-PROCESSING (The missing steps) ---
                    showNotification("Filtering & Cleaning Table...", type = "message")
                    
                    final_cs_clean <- final_cs |>
                        # A. Filter Min Cs (Your Step 5)
                        filter(Cs >= input$filter_Cs) |>
                        
                        # B. Remove Repetitions (Your Step 6)
                        # This keeps only one version of the pair (e.g. sp1 < sp2)
                        rowwise() |> 
                        mutate(
                            key = paste(sort(c(sp1, sp2)), collapse = "_")
                        ) |>
                        ungroup() |>
                        distinct(key, .keep_all = TRUE) |>
                        select(-key) |>
                        arrange(desc(Cs))
                    
                    print("Calculation & Cleaning Finished!")
                    
                    # Output to global reactive or table
                    # output$view_species_table <- renderDT(final_cs_clean)
                    
                } 
                # ... Strategy: ALL (Direct)
                else {
                    showNotification("Running: SF | Serial | Full Load", type = "warning")
                    # Placeholder: Logic to run st_intersection on the whole object at once
                    # final_cs <- calculate_chunk_cs_engine(species_list, shapes, areas_df) 
                }
                
            } 
            # --- Sub-Branch: PARALLEL Mode ---
            else { # mode_parallel
                showNotification("SF Parallel Mode: Under Development 🚧", type = "warning")
                # Placeholder for future/apply or foreach logic
            }
            
        } 
        
        # >>> BRANCH 2: TERRA ENGINE (High Performance) <<<
        else { # engine_terra
            showNotification("Terra Engine: Under Development 🚧", type = "warning")
            # Placeholder: Convert sf -> vect -> terra intersect logic
        }
        
    }) # End ObserverEvent

    
    # =========================================================================
    # --- E. DATA EXPORT HUB ---
    # =========================================================================
    
    # 1. Dynamic Button Renderer
    # Only shows the download button if map_data() actually exists
    output$download_map_ui <- renderUI({
        if (is.null(map_data())) {
            return(actionButton("no_map_dl", "No Map Loaded", icon = icon("exclamation"), disabled = TRUE))
        } else {
            downloadButton("download_processed_map", "Download .ZIP", class = "btn-success", style = "width: 100%;")
        }
    })

    # 2. Map Download Handler
    
    # 2. Map Download Handler (Base R Version)
    output$download_processed_map <- downloadHandler(
        
        filename = function() {
            # Naming convention: SCAN_Map_Processed_YYYY-MM-DD.zip
            paste0("SCAN_Map_Processed_", Sys.Date(), ".zip")
        },
        
        content = function(file) {
            # A. Setup Temporary Directory (Base R way)
            # tempfile() creates a unique path string like "C:\...\Rtmp...\file1a2b3c"
            # We use that random string to make a directory.
            temp_dir <- tempdir()
            unique_folder <- basename(tempfile(pattern = "scan_export_"))
            target_dir <- file.path(temp_dir, unique_folder)
            dir.create(target_dir)
            
            # B. Get the Data
            data_to_save <- map_data()
            
            # C. Write Shapefile Parts
            # We save it inside our unique folder
            layer_name <- "SCAN_Processed_Map"
            sf::st_write(data_to_save, dsn = file.path(target_dir, paste0(layer_name, ".shp")), delete_dsn = TRUE, quiet = TRUE)
            
            # D. Zip It Up
            # list.files() grabs the .shp, .shx, .dbf, .prj, etc.
            zip_files <- list.files(target_dir, full.names = TRUE)
            
            # utils::zip is Base R
            # flag '-j' (junk paths) flattens the structure so the user just gets files, not folders.
            utils::zip(zipfile = file, files = zip_files, flags = "-j") 
            
            # E. Cleanup
            unlink(target_dir, recursive = TRUE)
        },
        contentType = "application/zip"
    )

    
    # uuid pkg
    # output$download_processed_map <- downloadHandler(
    #     
    #     filename = function() {
    #         # Naming convention: SCAN_Map_[Date].zip
    #         paste0("SCAN_Map_Processed_", Sys.Date(), ".zip")
    #     },
    #     
    #     content = function(file) {
    #         # A. Setup Temporary Directory
    #         # We need a clean folder to write the shapefile components
    #         temp_dir <- tempdir()
    #         uuid <- uuid::UUIDgenerate() # Unique ID to avoid file conflicts
    #         target_dir <- file.path(temp_dir, uuid)
    #         dir.create(target_dir)
    #         
    #         # B. Get the Data
    #         # This grabs the CURRENT state (Projected, Buffered, Validated)
    #         data_to_save <- map_data()
    #         
    #         # C. Write Shapefile Parts
    #         # Note: delete_dsn=TRUE overwrites if exists
    #         layer_name <- "SCAN_Processed_Map"
    #         sf::st_write(data_to_save, dsn = file.path(target_dir, paste0(layer_name, ".shp")), delete_dsn = TRUE, quiet = TRUE)
    #         
    #         # D. Zip It Up
    #         # We must zip the files inside the target_dir
    #         zip_files <- list.files(target_dir, full.names = TRUE)
    #         utils::zip(zipfile = file, files = zip_files, flags = "-j") # -j flattens paths (junk paths)
    #         
    #         # E. Cleanup (Optional, but polite)
    #         unlink(target_dir, recursive = TRUE)
    #     },
    #     contentType = "application/zip"
    # )
    
    
    
    
    
    
    
}