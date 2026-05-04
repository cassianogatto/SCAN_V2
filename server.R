


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
library(shinyjqui)

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
    
    # --- 1. REACTIVE STATE VALUES updt 2may2026 ----
    map_data       <- reactiveVal(NULL) # Stores the Master Shapefile (Projected)
    cs_matrix_data <- reactiveVal(NULL) # Stores the Cs Matrix (sp1, sp2, Cs)
    spp_choices    <- reactiveVal(NULL) # Stores the unique species list (Memory)
    scan_results   <- reactiveVal(NULL) # NEW: Stores the SCAN Output for saving/loading
    
    # --- 1.1 VIEWER MEMORY BANK ----
    # Stores UI state so it doesn't reset when changing tabs
    viewer_state <- reactiveValues(
        threshold = 0.5,
        groups = NULL,
        alpha = 0.3,
        palette = "Set2",
        show_labels = TRUE
    )
    
    
    # 1.3 LISTENERS TO UPDATES ----
    # Listeners: Update the memory whenever the user touches a control
    observeEvent(input$viewer_threshold, { viewer_state$threshold <- input$viewer_threshold })
    observeEvent(input$viewer_groups_check, { viewer_state$groups <- input$viewer_groups_check }, ignoreNULL = FALSE)
    observeEvent(input$viewer_groups_radio, { viewer_state$groups <- input$viewer_groups_radio }, ignoreNULL = FALSE)
    observeEvent(input$alpha_global, { viewer_state$alpha <- input$alpha_global })
    observeEvent(input$palette_global, { viewer_state$palette <- input$palette_global })
    observeEvent(input$viewer_show_labels, { viewer_state$show_labels <- input$viewer_show_labels })
    
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
    
    # B. Map Updater (Smart Context-Aware Version) - 1 may 26
    observe({
        req(filtered_data()) # Always require the base map to exist
        
        # 1. Start by assuming we will show the standard Workshop map
        display_shp <- filtered_data()
        use_chorotypes <- FALSE
        
        # 2. Check if we are in the Viewer tab AND have analysis results ready
        if (!is.null(input$top_nav) && input$top_nav == "SCAN Viewer") {
            # We use tryCatch to safely attempt grabbing the Viewer Data without crashing
            tryCatch({
                if (!is.null(viewer_map_data()) && nrow(viewer_map_data()) > 0) {
                    display_shp <- viewer_map_data()
                    use_chorotypes <- TRUE
                    # Force re-evaluation when visual defaults change
                    req(input$alpha_global)
                }
            }, error = function(e) { NULL }) # Fail silently and keep the standard map
        }
        
        # 3. Prepare WGS84 copy for display
        if (st_crs(display_shp)$epsg != 4326) {
            display_shp <- st_transform(display_shp, 4326)
        }
        
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        if (!col_name %in% names(display_shp)) col_name <- names(display_shp)[1] 
        
        bb <- st_bbox(display_shp)
        map_proxy <- leafletProxy("map") %>% clearShapes()
        
        # 4. Render the appropriate polygons
        # --- MAP UPDATER: Lineage-Aware Version ---
        if (use_chorotypes && !is.null(viewer_palette())) {
            
            # Safety check: Do we actually have polygons to draw?
            req(nrow(display_shp) > 0)
            
            # Explicitly set the color domain to avoid 'Factor not found' errors
            pal_fun <- colorFactor(
                palette = viewer_palette(), 
                domain = display_shp$Chorotype 
            )
            
            alpha_val <- if(!is.null(input$alpha_global)) input$alpha_global else 0.3
            
            map_proxy %>% addPolygons(
                data = display_shp,
                fillColor = ~pal_fun(Chorotype), 
                fillOpacity = alpha_val,
                color = "white", weight = 1, opacity = 1,
                label = ~as.character(display_shp[[col_name]]),
                popup = ~paste("<b>Species:</b>", display_shp[[col_name]], 
                               "<br><b>Chorotype:</b>", Chorotype)
            )
        } else {
            
            # --- STANDARD MODE: Solid Green ---
            map_proxy %>% addPolygons(
                data = display_shp,
                color = "#18bc9c",
                weight = 2,
                fillOpacity = 0.4,
                label = ~as.character(display_shp[[col_name]])
            )
        }
        
        # Fly to bounds
        map_proxy %>% flyToBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
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

        
        # 1. PRE-PROCESSING

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
            
            # --- AUTO-SYNC VIEWER ---
            # Updates the viewer slider so its minimum and starting value match the calculated Cs
            updateSliderInput(session, "viewer_threshold", 
                              min = input$filter_Cs, 
                              value = input$filter_Cs)
            viewer_state$threshold <- input$filter_Cs # Update the memory bank too
            
            showNotification("Calculation Finished! Check Right Panel.", type = "message", duration = 5)
        }
    })
    
    # --- 5. HIERARCHICAL SCAN ENGINE (1.1.1 Nomenclature) 3may2026 ----
    observeEvent(input$run_scan, {
        req(cs_matrix_data())
        
        # 1. Setup
        df_cs <- cs_matrix_data() 
        thresholds <- seq(input$threshold_min, input$threshold_max, by = input$resolution)
        cs_filtered <- df_cs %>% filter(Cs >= input$threshold_min)
        g_full <- as_tbl_graph(cs_filtered, directed = FALSE)
        
        chorotypes_df <- data.frame()
        lineage_history <- list() # Stores {species_set = ID} for tracking
        next_base_id <- 1 
        
        # 2. The Hierarchical Loop
        withProgress(message = 'Running Lineage Analysis...', value = 0, {
            for(ct in thresholds) {
                incProgress(1/length(thresholds), detail = paste("Ct:", ct))
                
                # Filter network for current threshold
                g_temp <- g_full %>%
                    activate(edges) %>% filter(Cs >= ct) %>%
                    activate(nodes) %>% mutate(degree = centrality_degree()) %>% filter(degree > 0)
                
                Chorotype_ID <- g_temp %>% activate(nodes) %>% mutate(cid = group_components()) %>% as_tibble()
                if(nrow(Chorotype_ID) == 0) next
                
                comp_list <- split(Chorotype_ID$name, Chorotype_ID$cid)
                current_step_names <- list()
                
                for(cid in names(comp_list)) {
                    spp_in_group <- sort(comp_list[[cid]])
                    if(length(spp_in_group) < 2) next
                    
                    # --- VALIDATION (Diameter/Overlap) ---
                    is_valid <- TRUE
                    g_sub <- g_temp %>% filter(name %in% spp_in_group)
                    if(isTRUE(input$overlap) && igraph::edge_density(g_sub) < 1) is_valid <- FALSE
                    if(is_valid && isTRUE(input$filter_diameter) && igraph::diameter(g_sub) > input$max_diameter) is_valid <- FALSE
                    
                    if(is_valid) {
                        # --- 3. NOMENCLATURE LOGIC (1.1.1) ---
                        final_id <- ""
                        parent_found <- FALSE
                        
                        # Check previous step for a Parent or Identity
                        if(length(lineage_history) > 0) {
                            prev_step <- lineage_history[[length(lineage_history)]]
                            for(p_id in names(prev_step)) {
                                p_spp <- prev_step[[p_id]]
                                
                                # A. IDENTITY: Same species = Same ID
                                if(identical(spp_in_group, p_spp)) {
                                    final_id <- p_id
                                    parent_found <- TRUE; break
                                }
                                # B. DIFFERENTIATION: Subset = Child ID (e.g. 1.1)
                                if(all(spp_in_group %in% p_spp)) {
                                    siblings <- grep(paste0("^", p_id, "\\."), names(current_step_names))
                                    final_id <- paste0(p_id, ".", length(siblings) + 1)
                                    parent_found <- TRUE; break
                                }
                            }
                        }
                        
                        # C. NEW ORIGIN: No parent found[cite: 7]
                        if(!parent_found) {
                            final_id <- as.character(next_base_id)
                            next_base_id <- next_base_id + 1
                        }
                        
                        # Record for this step's history
                        current_step_names[[final_id]] <- spp_in_group
                        
                        # Store Result with Metrics[cite: 7]
                        chorotypes_df <- rbind(chorotypes_df, data.frame(
                            Threshold = ct,
                            Chorotype_ID = final_id,
                            Species = I(list(spp_in_group)), 
                            N_Species = length(spp_in_group),
                            Diameter = igraph::diameter(g_sub),
                            Density = round(igraph::edge_density(g_sub), 3)
                        ))
                    }
                }
                # Advance history tracker[cite: 7]
                lineage_history[[as.character(ct)]] <- current_step_names
            }
        })
        
        # 4. Final Packaging[cite: 6, 7]
        res_chorotypes <- chorotypes_df %>% tidyr::unnest(Species)
        
        scan_results(list(
            chorotypes = res_chorotypes,
            graph = g_full,
            graph_nodes = g_full %>% activate(nodes) %>% as_tibble(),
            graph_edges = g_full %>% activate(edges) %>% as_tibble()
        ))
        
        # --- SYNC SLIDER STEP ---
        updateSliderInput(session, "viewer_threshold", step = input$resolution)
        showNotification("Hierarchical SCAN Complete!", type = "message")
    })
    
    
    # --- 6. UI OUTPUTS & RENDERERS ----
     
    # A. SCAN Preview Table (Main Window)
    output$table_download_preview <- DT::renderDT({
        req(scan_results())
        df <- scan_results()[['chorotypes']] %>% 
            rename(Chorotype = Chorotype_ID) # Final touch for consistency
        DT::datatable(df, options = list(pageLength = 35, scrollX = TRUE))
    })
    
    # B. SCAN Summary Text
    output$scan_summary_text <- renderUI({
        req(scan_results())
        df <- scan_results()[['chorotypes']]
        n_groups <- length(unique(df$Chorotype_ID))
        HTML(paste0("<b>", n_groups, " Chorotypes</b> found"))
    })
    
    # C. SCAN Results sidebar
    
    # --- 6.C INTERACTIVE SIDEBAR SPECIES LIST ---
    output$scan_chorotype_list <- DT::renderDT({
        req(scan_results())
        
        # We use the full species list as you requested
        df <- scan_results()[['chorotypes']] %>%
            select(Chorotype = Chorotype_ID, Species, Threshold) %>%
            arrange(Threshold, Chorotype)
        
        DT::datatable(df, 
                      selection = 'single', # This prepares us for your 'Actions' idea!
                      options = list(
                          pageLength = 10,     # Small chunks prevent lag
                          dom = 'tp',          # Shows only table and pagination (keeps it tidy)
                          scrollX = TRUE,
                          fontSize = '8pt'
                      ),
                      rownames = FALSE
        )
    })
    
    # replace the output below
    # output$scan_chorotype_list <- renderTable({
    #     req(scan_results())
    #     df <- scan_results()[['chorotypes']]
    #     
    #     # Clean Table for Sidebar Display
    #     df %>% 
    #         dplyr::select(Group = Chorotype_ID, Species) %>% 
    #         # Optional: Shorten Group Name if needed (e.g. Ct0.5_G1 -> G1)
    #         dplyr::mutate(Group = sub(".*_G", "G", Group)) %>%
    #         head(100) # Safety limit for display speed
    #     
    # }, striped = TRUE, hover = TRUE, width = "100%", spacing = "xs")
    
    # D. Helper Flags
    output$cs_data_available <- reactive({ !is.null(cs_matrix_data()) && nrow(cs_matrix_data()) > 0 })
    
    outputOptions(output, "cs_data_available", suspendWhenHidden = FALSE)
    
    output$scan_results_ready <- reactive({ !is.null(scan_results()) })
    
    outputOptions(output, "scan_results_ready", suspendWhenHidden = FALSE)
    
    
    # --- 7. RIGHT PANEL (CONTEXT AWARE - WITH DOWNLOADS) ----
    output$right_panel_container <- renderUI({
        # 1. Capture Inputs
        top_lvl <- input$top_nav
        sub_lvl <- input$analysis_subtabs
        
        # 2. Defaults
        # Initialize variable HERE so it exists in all paths
        panel_content <- NULL 
        panel_title <- ""
        
        # 3. Logic Flow
        if (!is.null(top_lvl)) {
            
            # --- CASE A: SCAN ANALYSIS ----
            if (top_lvl == "SCAN Analysis") {
                
                if (!is.null(sub_lvl) && sub_lvl == "Map") {
                    panel_title <- "Map Filters"
                    panel_content <- tagList(
                        p(class="text-muted", "Select species to highlight."),
                        selectizeInput("map_spp_select", NULL, choices = spp_choices(), multiple = TRUE, options = list(placeholder = "Select species...")),
                        actionButton("btn_map_reset", "Reset View", icon = icon("refresh"), size = "xs", style = "width: 100%; margin-top: 5px;")
                    )
                    
                } else if (!is.null(sub_lvl) && sub_lvl == "Cs") {
                    has_data <- !is.null(cs_matrix_data())
                    panel_title <- "Matrix Inspector"
                    panel_content <- tagList(
                        if(has_data) {
                            tagList(
                                div(style="margin-bottom: 10px;", p(strong("Dimensions:"), paste(nrow(cs_matrix_data()), "rows")), 
                                    downloadButton("dl_cs_sidebar", "Download Matrix", class = "btn-success btn-xs", style = "width: 100%; margin-bottom: 15px;")),
                                hr(),
                                p(strong("Top Connected Pairs:")),
                                tableOutput("mini_nodes_table")
                            )
                        } else {
                            p(class="text-warning", icon("exclamation-circle"), " No Matrix Calculated yet.")
                        }
                    )
                    
                } else if (!is.null(sub_lvl) && sub_lvl == "SCAN") {
                    panel_title <- "SCAN Results"
                    # Check if results exist safely
                    res_ready <- FALSE
                    try({
                        if(exists("scan_graph") && !is.null(scan_results())) res_ready <- TRUE
                    }, silent=TRUE)
                    
                    if(res_ready) {
                        panel_content <- tagList(
                            htmlOutput("scan_summary_text"),
                            hr(style="margin: 5px 0;"),
                            p(class = "text-muted", style="font-size: 12px;", "Species & Group Assignment:"),
                            div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd;",
                                tableOutput("scan_chorotype_list")
                            )
                        )
                    } else {
                        panel_content <- p(class="text-muted", "Run analysis to see results.")
                    }
                }
                
                # --- CASE B: SCAN VIEWER ----
            } else if (top_lvl == "SCAN Viewer") {
                panel_title <- "Chorotype Selection"
                
                # Check if we have results to show
                
                # Default starting resolution
                default_res <- 0.05
                
                res_ready <- FALSE
                
                if (!is.null(scan_results())) res_ready <- TRUE
                
                if(res_ready) {
                    panel_content <- tagList(
                        
                        uiOutput("viewer_res_warning"),
                        
                        # The Master Controller
                        numericInput("viewer_res_manual", "Resolution (Step):", 
                                     value = default_res, step = 0.01, min = 0.001),
                        
                        # The Slider (Starts at default_res, but controlled by the observer below)
                        sliderInput("viewer_threshold", "1. Threshold (Ct):", 
                                    min = 0, max = 1, 
                                    value = isolate(viewer_state$threshold), step = default_res),
                        
                        # --- NEW: SELECTION CONTROLS ---
                        div(style = "margin-bottom: 5px; margin-top: -10px;",
                            actionButton("btn_select_all", "Select All", class="btn-xs btn-primary", style="margin-right: 5px;"),
                            actionButton("btn_select_none", "Clear", class="btn-xs btn-default")
                        ),
                        checkboxInput("single_select_mode", "Single Group Mode", value = FALSE),
                        
                        uiOutput("viewer_group_selector"),
                        
                        hr(style = "margin-top: 10px; margin-bottom: 10px;"),
                        tags$h5(icon("paint-brush"), "Visual Tweaks", style="margin-top: 0; color: #777;"),
                        # --- NEW: TOGGLE MAP LEGEND TWEAK ---
                        div(style = "display: flex; gap: 10px;",
                            checkboxInput("viewer_show_labels", "Show Network Labels", value = isolate(viewer_state$show_labels)),
                            checkboxInput("viewer_show_map_legend", "Show Map Legend", value = TRUE) # Tweak
                        ),
                        sliderInput("alpha_global", "Map Transparency:", min=0, max=1, value = isolate(viewer_state$alpha), step=0.1),
                        selectInput("palette_global", "Color Palette:", choices = c("Set2", "Set1", "Paired", "Dark2", "RdYlBu"), selected = isolate(viewer_state$palette))
                    )
                } else {
                    panel_content <- p(class="text-danger", icon("exclamation-triangle"), " Run SCAN Analysis first.")
                }
            }
        }
        
        # 4. Final Return (in renderUI)
        # If panel_content is still NULL (no condition met), we return NULL (hidden panel)
        if (is.null(panel_content)) return(NULL)
        
        # Construct the Floating Panel
        absolutePanel(
            id = "right_context_panel",
            class = "panel panel-default",
            top = 130, right = 20, width = 320,
            draggable = TRUE, fixed = TRUE,
            style = "z-index: 2000; opacity: 0.95;",
            
            div(class = "panel-heading", style="cursor: move;", strong(panel_title)),
            div(class = "panel-body", panel_content)
        )
    })     # end renderUI
    
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
        req(scan_results())
        df <- scan_results()[['chorotypes']]
        data.frame(Metric = c("Total Groups", "Rows"), Value = c(length(unique(df$Chorotype_ID)), nrow(df)))
    }, colnames = FALSE, width = "100%", bordered = TRUE)
    
    
    # --- 9. DOWNLOADS ----
    
    # 1. Map Export
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
    
    # 2. Cs Matrix (Supports both UI buttons)
    output$dl_cs_sidebar <- downloadHandler(
        filename = function() { paste0("SCAN_Cs_", Sys.Date(), ".csv") },
        content = function(file) { write.csv(cs_matrix_data(), file, row.names = FALSE) }
    )
    
    output$dl_cs <- downloadHandler(
        filename = function() { paste0("SCAN_Cs_", Sys.Date(), ".csv") },
        content = function(file) { write.csv(cs_matrix_data(), file, row.names = FALSE) }
    )
    
    # 3. SCAN Results 
    output$dl_chorotypes <- downloadHandler(
        filename = function() { paste0("SCAN_Groups_", Sys.Date(), ".csv") },
        content = function(file) { req(scan_results()); write.csv(scan_results()[['chorotypes']], file, row.names = FALSE) }
    )
    
    output$dl_edges <- downloadHandler(
        filename = function() { paste0("SCAN_Edges_", Sys.Date(), ".csv") },
        content = function(file) { req(scan_results()); write.csv(scan_results()[['graph_edges']], file, row.names = FALSE) }
    )
    
    output$dl_nodes <- downloadHandler(
        filename = function() { paste0("SCAN_Nodes_", Sys.Date(), ".csv") },
        content = function(file) { req(scan_results()); write.csv(scan_results()[['graph_nodes']], file, row.names = FALSE) }
    )
    
    # --- 10. SCAN VIEWER LOGIC ----
    
    # A. Dynamic Checkbox Generator (Sorted by Lineage)
    output$viewer_group_selector <- renderUI({
        req(scan_results()) 
        df <- scan_results()[['chorotypes']]
        current_ct <- input$viewer_threshold
        
        available_groups <- df %>% 
            dplyr::filter(abs(Threshold - current_ct) < 0.001) %>%
            dplyr::pull(Chorotype_ID) %>%
            unique()
        
        if(length(available_groups) == 0) return(helpText("No chorotypes formed exactly at this Threshold."))
        
        # --- PADDED SORTING FOR HIERARCHICAL IDs ---
        groups_raw <- unique(available_groups)
        split_parts <- stringr::str_split(groups_raw, "\\.")
        padded_keys <- lapply(split_parts, function(x) {
            padded_x <- stringr::str_pad(x, width = 3, side = "left", pad = "0")
            paste(padded_x, collapse = ".")
        })
        
        sorted_df <- data.frame(original = groups_raw, key = unlist(padded_keys)) %>%
            dplyr::arrange(key)
        final_sorted_groups <- sorted_df$original
        # ---------------------------------------------
        
        display_labels <- final_sorted_groups
        names(final_sorted_groups) <- display_labels
        
        mem_groups <- isolate(viewer_state$groups)
        safe_selection <- if (!is.null(mem_groups) && any(mem_groups %in% final_sorted_groups)) {
            mem_groups[mem_groups %in% final_sorted_groups]
        } else {
            final_sorted_groups[1]
        }
        
        if (isTRUE(input$single_select_mode)) {
            radioButtons("viewer_groups_radio", NULL, choices = final_sorted_groups, selected = safe_selection[1], inline = TRUE)
        } else {
            checkboxGroupInput("viewer_groups_check", NULL, choices = final_sorted_groups, selected = safe_selection, inline = TRUE)
        }
    })
    
    # B. BUTTON ACTIONS (Select All / None)
    
    # SELECT ALL
    observeEvent(input$btn_select_all, {
        req(scan_results(), input$viewer_threshold) 
        if (isTRUE(input$single_select_mode)) {
            showNotification("Cannot 'Select All' in Single Group Mode.", type = "warning")
            return()
        }
        df <- scan_results()[['chorotypes']]
        available_groups <- df %>% 
            dplyr::filter(abs(Threshold - input$viewer_threshold) < 0.001) %>% 
            dplyr::pull(Chorotype_ID) %>% unique()
        updateCheckboxGroupInput(session, "viewer_groups_check", selected = available_groups)
    })
    
    # SELECT NONE
    observeEvent(input$btn_select_none, {
        if (isTRUE(input$single_select_mode)) return()
        
        updateCheckboxGroupInput(session, "viewer_groups_check", 
                                 selected = character(0))
        
        viewer_state$groups <- NULL # Clears the memory bank too
        showNotification("Selections cleared.", type = "message", duration = 2)
    })
    
    # C. LINEAGE-AWARE FILTERS
    
    # Helper: The Sub-Graph Filter
    viewer_sub_graph <- reactive({
        req(scan_results(), input$viewer_threshold)
        
        current_groups <- if(isTRUE(input$single_select_mode)) {
            input$viewer_groups_radio 
        } else {
            input$viewer_groups_check
        }
        
        req(length(current_groups) > 0) 
        
        df <- scan_results()[['chorotypes']]
        
        target_df <- df %>% 
            filter(abs(Threshold - input$viewer_threshold) < 0.0001) %>%
            filter(Chorotype_ID %in% current_groups)
        
        selected_spp <- target_df %>% pull(Species) %>% unique()
        req(length(selected_spp) > 0)
        
        g_full <- scan_results()[['graph']]
        
        g_view <- g_full %>%
            activate(nodes) %>% 
            filter(name %in% selected_spp) %>% 
            left_join(target_df %>% select(name = Species, Chorotype = Chorotype_ID) %>% distinct(), by = "name") %>%
            activate(edges) %>% 
            filter(Cs >= input$viewer_threshold)
        
        return(g_view)
    })
    
    # D. Helper: The Map Data Join (Final Version)
    viewer_map_data <- reactive({
        req(viewer_sub_graph(), map_data())
        
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        if (!col_name %in% names(map_data())) col_name <- names(map_data())[1] 
        
        # Get species and their Chorotype IDs from the graph
        node_data <- viewer_sub_graph() %>% activate(nodes) %>% as_tibble() %>% 
            select(name, Chorotype)
        
        join_by_vec <- setNames("name", col_name)
        
        # We use inner_join to ensure only selected monkeys with geometry are kept
        map_final <- map_data() %>% 
            inner_join(node_data, by = join_by_vec)
        
        return(map_final)
    })
    
    # E. Helper: Lineage Palette
    viewer_palette <- reactive({
        req(viewer_sub_graph(), input$palette_global)
        grps <- viewer_sub_graph() %>% activate(nodes) %>% as_tibble() %>% 
            pull(Chorotype) %>% unique() %>% sort()
        
        n_colors <- length(grps)
        if(n_colors < 3) n_colors <- 3
        
        cols <- suppressWarnings(RColorBrewer::brewer.pal(n = n_colors, name = input$palette_global))
        if(length(grps) > length(cols)) { cols <- colorRampPalette(cols)(length(grps)) } else { cols <- cols[1:length(grps)] }
        names(cols) <- grps
        return(cols)
    })
    
    # --- PLOT RENDERERS ---
    
    output$ggplot_map <- renderPlot({
        req(viewer_map_data(), viewer_palette(), input$alpha_global)
        
        leg_pos <- if(isTRUE(input$viewer_show_map_legend)) "bottom" else "none"
        
        ggplot(viewer_map_data()) +
            geom_sf(aes(fill = as.factor(Chorotype)), color = "black", size = 0.2, alpha = input$alpha_global) +
            scale_fill_manual(values = viewer_palette(), name = "Chorotype") +
            theme_minimal() + 
            theme(legend.position = leg_pos) + 
            labs(title = paste("Distribution (Ct =", input$viewer_threshold, ")"))
    })
    
    output$graph_plot <- renderPlot({
        req(viewer_sub_graph(), viewer_palette())
        lay <- create_layout(viewer_sub_graph(), layout = "nicely")
        p <- ggraph(lay) +
            geom_edge_link(aes(alpha = Cs), width = 1, show.legend = FALSE) +
            geom_node_point(aes(fill = as.factor(Chorotype)), size = 5, shape = 21, color = "black") +
            scale_fill_manual(values = viewer_palette()) +
            theme_graph() + theme(legend.position = "none")
        if(isTRUE(input$viewer_show_labels)) p <- p + geom_node_text(aes(label = name), repel = TRUE, size = 4, fontface="bold")
        return(p)
    })
    
    output$view_species_table <- DT::renderDT({
        req(viewer_map_data())
        
        col_name <- if(isTRUE(input$ID_column)) input$colum_sp_map else "sp"
        if (!col_name %in% names(viewer_map_data())) col_name <- names(viewer_map_data())[1]
        
        table_df <- viewer_map_data() %>% 
            sf::st_drop_geometry() %>% 
            select(Species = all_of(col_name), Chorotype) %>% 
            arrange(Chorotype, Species)
        
        DT::datatable(table_df,
                      extensions = 'Buttons',
                      options = list(
                          pageLength = 20, 
                          scrollX = TRUE,
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel')
                      ),
                      rownames = FALSE
        )
    }, server = FALSE)
    
    
    # --- 11. UNIVERSAL SCAN UPLOADER (2 or 3 Files) ----
    observeEvent(input$upload_scan_csvs, {
        req(input$upload_scan_csvs)
        files <- input$upload_scan_csvs
        up_nodes <- NULL; up_edges <- NULL; up_chorotypes <- NULL
        
        showNotification("Unpacking SCAN Data...", type = "message")
        
        # 1. Identify Files by Content
        for (i in 1:nrow(files)) {
            df <- read.csv(files$datapath[i])
            if ("from" %in% names(df)) { up_edges <- df }
            else if ("Chorotype_ID" %in% names(df)) { up_chorotypes <- df }
            else if ("name" %in% names(df) || "Species" %in% names(df)) { 
                if("Species" %in% names(df)) names(df)[names(df) == "Species"] <- "name"
                up_nodes <- df 
            }
        }
        
        # 2. Reconstruct if mandatory Edges/Nodes exist
        if (!is.null(up_nodes) && !is.null(up_edges)) {
            tryCatch({
                # Bridge Integers to Names if needed
                if(is.numeric(up_edges$from)) {
                    up_edges$from <- up_nodes$name[up_edges$from]
                    up_edges$to   <- up_nodes$name[up_edges$to]
                }
                
                # Build the tidygraph[cite: 6]
                g_full <- as_tbl_graph(up_edges, directed = FALSE) %>%
                    activate(nodes) %>% left_join(up_nodes, by = "name")
                
                # 3. IF GROUPS FILE IS MISSING -> RECONSTRUCT HIERARCHY
                if (is.null(up_chorotypes)) {
                    showNotification("Chorotype table missing. Reconstructing hierarchy...", type = "warning")
                    thresholds <- seq(input$threshold_min, input$threshold_max, by = input$resolution)
                    chorotypes_list <- list()
                    
                    for(ct in thresholds) {
                        g_temp <- g_full %>% activate(edges) %>% filter(Cs >= ct) %>%
                            activate(nodes) %>% mutate(degree = centrality_degree()) %>% filter(degree > 0)
                        
                        Chorotype_ID <- g_temp %>% activate(nodes) %>% mutate(cid = group_components()) %>% as_tibble()
                        if(nrow(Chorotype_ID) == 0) next
                        
                        comp_groups <- split(Chorotype_ID$name, Chorotype_ID$cid)
                        for(id in names(comp_groups)) {
                            if(length(comp_groups[[id]]) >= 2) {
                                chorotypes_list[[length(chorotypes_list)+1]] <- data.frame(
                                    Threshold = ct, Chorotype_ID = paste0("Ct", ct, "_G", id),
                                    Species = comp_groups[[id]], N_Species = length(comp_groups[[id]])
                                )
                            }
                        }
                    }
                    up_chorotypes <- bind_rows(chorotypes_list)
                }
                
                # 4. FINAL SYNC
                scan_results(list(
                    chorotypes = up_chorotypes,
                    graph = g_full,
                    graph_nodes = up_nodes,
                    graph_edges = up_edges
                ))
                
                showNotification("Success: SCAN session fully restored!", type = "message")
            }, error = function(e) {
                showNotification(paste("Error during restoration:", e$message), type = "error")
            })
        } else {
            showNotification("Select at least Edges and Nodes CSVs.", type = "warning")
        }
        
        # --- SYNC SLIDER TO UPLOADED DATA RESOLUTION ---
        if (!is.null(up_chorotypes)) {
            # Find the distance between the first two unique thresholds
            u_thresholds <- sort(unique(up_chorotypes$Threshold))
            if(length(u_thresholds) > 1) {
                detected_res <- u_thresholds[2] - u_thresholds[1]
                updateSliderInput(session, "viewer_threshold", step = detected_res)
            }
        }
    })

    # ---- 12. PROJECT MANAGER (SAVE / LOAD) 2may26 ----
    
    # A. SAVE PROJECT
    output$save_project <- downloadHandler(
        filename = function() { paste0("SCAN_Project_", Sys.Date(), ".rds") },
        content = function(file) {
            showNotification("Bundling Project Data...", type = "message")
            
            project_bundle <- list(
                map_data = map_data(),
                cs_matrix = cs_matrix_data(),
                scan_res = scan_results(),
                spp_list = spp_choices()
            )
            saveRDS(project_bundle, file)
        }
    )
    
    # B. LOAD PROJECT
    observeEvent(input$load_project, {
        req(input$load_project)
        showNotification("Loading Project Data...", type = "message")
        
        tryCatch({
            project_bundle <- readRDS(input$load_project$datapath)
            
            # Restore the memory
            if (!is.null(project_bundle$map_data)) map_data(project_bundle$map_data)
            if (!is.null(project_bundle$cs_matrix)) cs_matrix_data(project_bundle$cs_matrix)
            if (!is.null(project_bundle$scan_res)) scan_results(project_bundle$scan_res)
            if (!is.null(project_bundle$spp_list)) spp_choices(project_bundle$spp_list)
            
            # Update the UI Dropdowns
            if (!is.null(project_bundle$spp_list)) {
                updateSelectizeInput(session, "map_spp_select", choices = project_bundle$spp_list, server = TRUE)
            }
            
            showNotification("Project Loaded Successfully! You can now navigate to the Viewer.", type = "message", duration = 8)
            
        }, error = function(e) {
            showNotification("Error loading project. Invalid file format.", type = "error")
        })
    })    
    
    # --- MANUAL RESOLUTION CONTROLLER ----
    observeEvent(input$viewer_res_manual, {
        # This function 're-writes' the slider settings in the user's browser
        updateSliderInput(session, "viewer_threshold", step = input$viewer_res_manual)
        
        showNotification(paste("Slider resolution updated to:", input$viewer_res_manual), 
                         type = "message", duration = 2)
    })
    
    # --- DYNAMIC MAP DIAGNOSIS 2may2026 ----
    # Provides feedback directly inside the Analysis sidebar Box 1
    output$map_diagnosis_ui <- renderUI({
        # If no map is loaded, show a bright warning
        if (is.null(map_data())) {
            return(wellPanel(style = "border: 1px solid red; background: rgba(255, 245, 245, 0.8); margin-top: 10px;",
                             p(style = "color: red; margin: 0; font-weight: bold;", 
                               icon("exclamation-triangle"), " Waiting for geometry upload...")))
        }
        
        # If map is loaded, show a diagnosis summary
        wellPanel(style = "background: rgba(248, 249, 250, 0.7); padding: 10px; margin-top: 10px; border-radius: 8px;",
                  tags$strong("Current Map Office:"),
                  tags$ul(style = "margin-bottom: 0;",
                          tags$li("Species found:", tags$span(style="color: #18bc9c;", length(spp_choices()))),
                          tags$li("CRS (input):", tags$span(style="color: #3498db;", st_crs(map_data())$input)),
                          tags$li("Status: Ready")
                  )
        )
    })
    
    
    # --- SYNC VIEWER STEP WITH SCAN RESOLUTION ----
    observeEvent(input$resolution, {
        # Ensure the resolution is a valid number before updating
        req(input$resolution)
        
        updateSliderInput(session, "viewer_threshold", 
                          step = input$resolution)
        
        # Optional: Log it so you know it's working
        # print(paste("Viewer Step updated to:", input$resolution))
    })
    
    
    
} # End Server ----




