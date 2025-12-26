######################################################
#           THIS IS SCAN V2                          #
######################################################

library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyjs)

ui <- fillPage(
    # --- 1. CSS & Header ----
    tags$style(type = "text/css", "
    html, body {width:100%; height:100%; margin:0; padding:0; overflow: hidden;}
    .top-nav-bar {
      position: absolute; top: 0; left: 0; width: 100%; height: 50px;
      background-color: rgba(44, 62, 80, 0.95); z-index: 2000; 
      display: flex; align-items: center; padding: 0 20px; color: white;
    }
    .nav-item { margin-right: 25px; cursor: pointer; font-weight: 500; }
    .nav-item:hover { color: #18bc9c; }
    .left-sidebar {
      background-color: rgba(255, 255, 255, 0.9);
      height: calc(100vh - 70px);
      overflow-y: auto; padding: 15px; border-radius: 0 15px 15px 0;
      box-shadow: 5px 0 15px rgba(0,0,0,0.1); z-index: 1000;
    }
    .scroll-panel {
      max-height: 85vh; overflow-y: auto; background-color: rgba(255,255,255,0.9);
      padding: 30px; border-radius: 10px; z-index: 1500;
    }
    /* --- GLASS EFFECT FOR FLOATING PANELS --- */
    
    /* 1. Remove a cor sólida padrão e aplica transparência no container */
    #cs_floating_box, #scan_floating_box {
        background-color: rgba(255, 255, 255, 0.85) !important; /* Vidro Branco */
        backdrop-filter: blur(5px); /* Desfoque chique (opcional, mas bonito) */
        border: 1px solid rgba(0,0,0,0.1);
        box-shadow: 0 4px 15px rgba(0,0,0,0.2) !important;
    }

    /* 2. Garante que o corpo do painel seja transparente para herdar o vidro */
    #cs_floating_box .panel-body, #scan_floating_box .panel-body {
        background-color: transparent !important;
    }

    /* 3. Ajusta os cabeçalhos para serem semi-transparentes também */
    /* Azul (Info) */
    #cs_floating_box .panel-heading {
        background-color: rgba(58, 135, 173, 0.15) !important; 
        color: #31708f;
        border-bottom: 1px solid rgba(58, 135, 173, 0.2);
    }
    /* Verde (Success) */
    #scan_floating_box .panel-heading {
        background-color: rgba(60, 118, 61, 0.15) !important;
        color: #3c763d;
        border-bottom: 1px solid rgba(60, 118, 61, 0.2);
    }
  "),
    

  # ---  Top Navigation Bar (Updated) ----
  tags$div(class = "top-nav-bar",
           
           # ABOUT
           tags$div(class = "nav-item", 
                    onclick = "Shiny.setInputValue('top_nav', 'About SCAN');", 
                    "About SCAN"),
           
           # ANALYSIS
           tags$div(class = "nav-item", 
                    onclick = "Shiny.setInputValue('top_nav', 'SCAN Analysis');", 
                    "SCAN Analysis"),
           
           # VIEWER
           tags$div(class = "nav-item", 
                    onclick = "Shiny.setInputValue('top_nav', 'SCAN Viewer');", 
                    "SCAN Viewer"),
           
           # SETTINGS
           tags$div(class = "nav-item", 
                    onclick = "Shiny.setInputValue('top_nav', 'Settings&Files');", 
                    "Settings&Files")
  ),
    
    # --- 3. The Leaflet Map Background ----
    leafletOutput("map", width = "100%", height = "100%"),
    
    
    # --- 4. Merged Content: Info & Documentation ----
    conditionalPanel(
        condition = "input.top_nav == 'About SCAN' || input.top_nav_selectio == 'Directions'",
        absolutePanel(
            top = 70, left = "15%", right = "15%",
            div(class = "scroll-panel",
                tabsetPanel(
                    id = "info_tabs",
                    # Link the tab selection to the main navigation
                    selected = "About SCAN", 
                    
                    tabPanel("About SCAN",
                             br(),
                             h2("About SCAN"),
                             p("Spatial Congruence Analysis Network (SCAN) version 2.0."),
                             tags$hr(),
                             div(style = "padding: 20px;",
                                 h4("Project Goals"),
                                 p("This tool is designed for high-performance biogeographical analysis...")
                             )
                    ),
                    
                    tabPanel("Directions",
                             br(),
                             h2("Tutorial & Directions"),
                             tags$hr(),
                             wellPanel(
                                 h4(icon("map"), "Step 1: Upload"),
                                 p("Go to 'Workshop' and upload your .shp files."),
                                 h4(icon("cogs"), "Step 2: Cs Calculus"),
                                 p("Navigate to the 'Cs' tab to configure your engine (sf/terra).")
                             )
                    )
                )
            )
        )
    ),
    
    # --- 5. Sidebar: SCAN Analysis Flow ----
    conditionalPanel(
        condition = "input.top_nav == 'SCAN Analysis'",
        absolutePanel(top = 60, left = 0, width = "25%",
            div(class = "left-sidebar",
                tabsetPanel(id = "analysis_subtabs", type = "pills",
                    # MAP input ----
                    tabPanel("Map",
                           br(),
                           box(title = "1. Input Map", status = "primary", width = NULL, solidHeader = T,
                               tags$p("Upload .shp + .shx + .dbf + .prj simultaneously:"),
                               fileInput("filemap", "Choose Shapefile components", 
                                         multiple = TRUE, 
                                         accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj")),
                             
                               
                           ),
                           
                           # Map Workshop ---
                           box(title = "2. Map Workshop", status = "primary", width = NULL, solidHeader = T,
                               textOutput("map_shp_names"),
                               # check here if column names are ok if ok we dont need the following queue
                               checkboxInput("ID_column", "Custom ID Column?", F),
                               conditionalPanel("input.ID_column == true",
                                                textInput("colum_sp_map", "Column name:", "sp")),
                               hr(),
                               
                               # check here if there are invalid geometries
                               checkboxInput("fix_invalid", "Fix Geometries (st_make_valid)?", F),
                               hr(),
                               
                               # --- CRS / PROJECTION SETTINGS ---
                               tags$hr(),
                               tags$h4("Map Projection Settings"),
                               
                               # 3. Dynamic Text Output to confirm current CRS
                               tags$br(),
                               tags$strong("Current CRS:"),
                               textOutput("map_crs_info", inline = TRUE),
                               
                               # --- MAP CONFIGURATION (Sidebar) ---
                               
                               tags$hr(),
                               
                               # A. PROJECTION SETTINGS
                               checkboxInput("modify_crs", "Enable Custom Projection", value = FALSE),
                               conditionalPanel(
                                   condition = "input.modify_crs == true",
                                   textInput("map_projection", "EPSG or Proj4 String:", value = "102033"),
                                   helpText("Ex: 102033 (Albers SA), 31982 (SIRGAS 2000), 3857 (Mercator Projected - metric version of WGS84).")
                               ),
                               
                               tags$hr(style="border-top: 1px dashed #777;"),
                               
                               # B. BUFFERING SETTINGS (Moved from Cs Tab)
                               checkboxInput("use_buffer_map", "Enable Geometry Buffering?", value = FALSE),
                               conditionalPanel(
                                   condition = "input.use_buffer_map == true",
                                   wellPanel(
                                       style = "background: #f5f5f5; color: black; padding: 10px;",
                                       
                                       # Buffer Distance
                                       numericInput("buffer_dist", "Buffer Dist (metric/deg):", value = 0),
                                       helpText("Positive = Expand, Negative = Shrink"),
                                       
                                       # Quartile Selection
                                       tags$label("Apply to Size Class (Quartiles):"),
                                       checkboxGroupInput("quantiles_to_buffer", NULL, 
                                                          choices = c("Q1 (Smallest)"=1, "Q2"=2, "Q3"=3, "Q4 (Largest)"=4), 
                                                          selected = c(1), inline = TRUE),
                                       
                                       tags$small(icon("exclamation-triangle"), "Warning: Alters polygon areas! Check your projection: if metric use meters, if geographic use degrees.")
                                   )
                               ),
                               
                               # C. THE MASTER TRIGGER (Controls A & B)
                               actionButton("apply_mods", "APPLY MAP SETTINGS", 
                                            class = "btn-primary", 
                                            icon = icon("cogs"), 
                                            style = "width: 100%; margin-top: 15px; font-weight: bold; font-size: 1.1em;"),
                               
                               
                           ),
                    ),
                          
                    # ... inside tabsetPanel ...
                    
                    # Cs Index Tab ----
                    tabPanel("Cs",
                             
                             # --- COLUMN 1: The "What" (Formulas & Thresholds) ---
                             fluidPage(
                                    box(title = "1. Index Configuration", status = "primary", width = NULL, solidHeader = TRUE,
                                        tags$h4("Spatial Congruence Settings"),
                                        
                                        # 1.1 Min CS
                                        numericInput("filter_Cs", "Minimum Cs Threshold (0 - 1)", value = 0.1, min = 0, max = 1, step = 0.05),
                                        
                                        # Warning Box
                                        div(style = "font-size: 0.9em; color: #856404; background-color: #fff3cd; border: 1px solid #ffeeba; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                                            icon("exclamation-triangle"), 
                                            "Note: Cutting the tail of lower values optimizes computation. However, low Cs values can still be biogeographically informative depending on the spatial structure."
                                        ),
                                        
                                        # 1.2 Formulas
                                        checkboxInput("use_alternative_index", "Use Custom Formula?", value = FALSE),
                                        conditionalPanel(
                                            condition = "input.use_alternative_index == true",
                                            wellPanel(
                                                style = "background: #fcfcfc; padding: 10px;",
                                                tags$small("Available variables: area_overlap, area_sp1, area_sp2"),
                                                textInput("cs_similarity_index", "Formula:", value = '(area_overlap / area_sp1) * (area_overlap / area_sp2)'),
                                                tags$small(em("Example Jaccard: area_overlap / (area_sp1 + area_sp2 - area_overlap)"))
                                            )
                                        ),
                                        
                                        box(title = "2. Engine & Performance", status = "warning", width = NULL, solidHeader = TRUE,
                                            
                                            # 2.1 Processing Engine (Library)
                                            tags$label("1. Calculation Engine:"),
                                            radioButtons("calc_engine", NULL,
                                                         choices = c("sf (Standard Vector)" = "engine_sf", 
                                                                     "terra (High Performance)" = "engine_terra"), 
                                                         selected = "engine_sf", inline = TRUE),
                                            
                                            # 2.2 Processing Mode (Serial vs Parallel)
                                            tags$label("2. Processing Core Mode:"),
                                            radioButtons("calc_mode", NULL, 
                                                         choices = c("Serial (Single Core)" = "mode_serial", 
                                                                     "Parallel (Multi-Core)" = "mode_parallel"), 
                                                         selected = "mode_serial", inline = TRUE),
                                            
                                            # Conditional: Parallel Settings
                                            conditionalPanel(
                                                condition = "input.calc_mode == 'mode_parallel'",
                                                sliderInput("num_cores", "Number of Cores:", min = 2, max = parallel::detectCores()-1, value = 2, step = 1),
                                                tags$small(style="color:gray", icon("info-circle"), " Parallel processing requires more RAM.")
                                            ),
                                            
                                            # 2.3 Memory Strategy (Direct vs Chunks)
                                            tags$label("3. Memory Strategy:"),
                                            radioButtons("memory_strategy", NULL, 
                                                         choices = c("Load All (Fastest, High RAM)" = "mem_all", 
                                                                     "Chunked Processing (Low RAM)" = "mem_chunk"), 
                                                         selected = "mem_all", inline = FALSE),
                                            
                                            # Conditional: Chunk Settings
                                            conditionalPanel(
                                                condition = "input.memory_strategy == 'mem_chunk'",
                                                div(style="display: flex; align-items: center; gap: 10px;",
                                                    numericInput("chunk_size", "Batch Size (spp):", value = 20, min = 5, width = "150px"),
                                                    tags$small("Smaller batches = Lower RAM usage.")
                                                )
                                            ),
                                     ),   
                                        
                                        # --- 3 Update Cs Index table ---
                                        
                                        box(title = "3. Update Cs Index Table", status = "info", width = NULL, solidHeader = TRUE, 
                                            fileInput("upload_cs_matrix", "Upload pre-calculated Cs Matrix (.csv)",
                                                      accept = c("text/csv", "text/comma-separated-values", ".csv"),
                                                      placeholder = "No file selected"),
                                            tags$small("Note: Uploading a file overrides the calculation settings above.")
                                        ),
                                        
                                        # Cs BUTTON 
                                        actionButton("calculate_Cs", "RUN CS ANALYSIS", 
                                                     class = "btn-success", 
                                                     style = "width: 100%; font-weight: bold; font-size: 1.2em; margin-top: 10px;", 
                                                     icon = icon("play-circle"))
                                    )
                             )
                    ),
                    
                    
                    
                    
                    
                    
                    
                    # SCAN ----
                    tabPanel("SCAN", 
                        
                        box(title="SCAN Engine", width=NULL, "SCAN Execution here", 
                            status = "primary",  solidHeader = T,
                            
                            # BOX 3.1: Configuração SCAN
                            box(width = NULL, title = "1. Algorithm Configuration", status = "danger", solidHeader = TRUE,
                                fluidRow(
                                    column(6, numericInput("resolution", "Resolution (Step):", value = 0.1, step = 0.01)),
                                    column(6, numericInput("threshold_min", "Min Threshold:", value = 0.2, step = 0.05)),
                                    column(6, numericInput("threshold_max", "Max Threshold:", value = 0.9, step = 0.05)),
                                    column(6, 
                                           checkboxInput("filter_diameter", "Limit Diameter?", value = TRUE),
                                           conditionalPanel("input.filter_diameter == true",
                                                            numericInput("max_diameter", "Max Diameter:", value = 15)
                                           )
                                    )
                                ),
                                fluidRow(
                                    column(3, checkboxInput("overlap", "Require Full Overlap (Clique)?", value = TRUE))
                                ),
                                tags$hr(),
                                actionButton("run_scan", "RUN SCAN ANALYSIS", class = "btn-danger", icon = icon("rocket"), width = "200px")
                            ),
                            
                            # Results Box
                            box(width = NULL, title = "2. Results & Downloads", status = "danger",
                                fluidRow(
                                    column(8, uiOutput("names_scan_list")),
                                    column(4, downloadButton("downloadData", "Download Selected", class = "btn-default align-btn"))
                                ),
                                tags$hr(),
                                tags$h4("Preview Data:"),
                                DT::DTOutput('table_download_preview'),
                                tags$br(),
                                tags$h4("Chorotypes List:"),
                                tableOutput("scan_chorotypes")
                            )
                            
                        ),
                            
                    ),
                        
                ),   # tabsetPanel (Map, Cs, SCAN)
            )    # leftsidebar conditional
        )   # absolutePanel
    ), # scan main conditional tab
    
    # --- 6. SCAN Viewer ----
    conditionalPanel(
        condition = "input.top_nav == 'SCAN Viewer'",
        absolutePanel(top = 70, left = "15%", right = "15%",
                      div(class = "scroll-panel", h2("SCAN Viewer"),
                          
                          fluidRow(
                                  column(5, 
                                         box(width = NULL, title = "Static Map", status = "primary", solidHeader = TRUE,
                                             plotOutput("ggplot_map", height = "500px")
                                         )
                                  ),
                                  column(5,
                                         box(width = NULL, title = "Network Topology", status = "primary", solidHeader = TRUE,
                                             plotOutput("graph_plot", height = "500px")
                                         )
                                  ), 
                                  
                                  column(1,),
                                  
                          ),
                          
                          fluidRow(
                              column(12,
                                     box(width = NULL, title = "Species List (Selected Groups)", status = "success", solidHeader = TRUE,
                                         DT::DTOutput("view_species_table")
                                     )
                              )
                          )
                          
                      ) # div
                  ) # absolute panel scan
    ), # conditional panel scan
    
    # --- 7. Settings & Files ----
  # =========================================================================
  # --- PANEL: SETTINGS & FILES (Downloads) ----
  # =========================================================================
  conditionalPanel(
    condition = "input.top_nav == 'Settings&Files'",
    absolutePanel(
      top = 70, left = "15%", right = "15%",
      div(class = "scroll-panel",
          h2(icon("download"), "Data Export & Downloads"),
          p("Download processed maps, calculated matrices, and analysis results."),
          hr(),
          
          # --- SECTION 1: PROCESSED INPUTS ---
          box(title = "1. Processed Inputs", status = "info", width = NULL, solidHeader = TRUE,
              fluidRow(
                column(6, 
                       h4("Geospatial Data"),
                       p("The reprojected/buffered map currently in memory."),
                       downloadButton("dl_map", "Download Map Shapefile (.zip)", class = "btn-primary btn-block")
                ),
                column(6, 
                       h4("Calculated Matrix"),
                       p("The Cs matrix currently loaded/calculated."),
                       downloadButton("dl_cs", "Download Cs Matrix (.csv)", class = "btn-primary btn-block")
                )
              )
          ),
          
          # --- SECTION 2: SCAN RESULTS ---
          box(title = "2. SCAN Analysis Results", status = "success", width = NULL, solidHeader = TRUE,
              p("Results based on the current 'Cs Threshold' defined in the SCAN tab."),
              fluidRow(
                column(4, 
                       downloadButton("dl_chorotypes", "📥 Chorotypes (Groups)", class = "btn-success btn-block")
                ),
                column(4, 
                       downloadButton("dl_edges", "📥 Graph Edges", class = "btn-default btn-block")
                ),
                column(4, 
                       downloadButton("dl_nodes", "📥 Graph Nodes", class = "btn-default btn-block")
                )
              ),
              br(),
              tags$small(icon("info-circle"), " Note: You must run the SCAN analysis first for these to be available.")
          )
      )
    )
  ),
  
  # =========================================================================
  # --- FLOATING BOX: CS PREVIEW (Appears only on Cs Tab) ----
  # =========================================================================
  conditionalPanel(
    # Condition: User is on 'SCAN Analysis' -> 'Cs' tab AND data is available
    condition = "input.top_nav == 'SCAN Analysis' && input.analysis_subtabs == 'Cs' && output.cs_data_available == true",
    
    absolutePanel(
      id = "cs_floating_box",
      class = "panel panel-info",
      fixed = TRUE, draggable = TRUE,
      top = 130, right = 20, width = 300, height = "auto",
      style = "z-index: 2000;  box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
      
      #div(class = "panel-heading", tags$h4("📊 Cs Results Preview", style="margin: 0; font-size: 16px;")),
      div(class = "panel-body", style = "max-height: 400px; overflow-y: auto; padding: 10px;",
          p(class = "text-muted", "Top strong connections:"),
          tableOutput("mini_nodes_table"), # Defined in server
          hr(),
          downloadButton("dl_cs_float", "Download Full Matrix", class = "btn-xs btn-primary btn-block")
      )
    )
  ),
  
  
  # =========================================================================
  # --- FLOATING BOX: SCAN RESULTS (Appears only on SCAN Tab) ----
  # =========================================================================
  conditionalPanel(
    # Condition: User is on 'SCAN Analysis' -> 'SCAN' tab AND results exist
    condition = "input.top_nav == 'SCAN Analysis' && input.analysis_subtabs == 'SCAN' && output.scan_results_ready == true",
    
    absolutePanel(
      id = "scan_floating_box",
      class = "panel panel-success", # Green style for Success/Results
      fixed = TRUE, draggable = TRUE,
      top = 130, right = 20, width = 320, height = "auto",
      style = "z-index: 2000;  box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
      
      # --- Header ---
      div(class = "panel-heading", 
          tags$h4("🧬 SCAN Chorotypes", style="margin: 0; font-size: 16px;")
      ),
      
      # --- Body: List of Groups ---
      div(class = "panel-body", style = "padding: 10px;",
          
          # 1. Summary Text
          htmlOutput("scan_summary_text"),
          hr(style="margin: 5px 0;"),
          
          # 2. Scrollable List of Chorotypes
          p(class = "text-muted", style="font-size: 12px;", "Species & Group Assignment:"),
          div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; background: white;",
              tableOutput("scan_chorotype_list")
          ),
          
          hr(),
          
          # 3. Quick Downloads
          div(class = "btn-group-vertical", style="width: 100%;",
              downloadButton("dl_chorotypes_float", "📥 Download List (.csv)", class = "btn-xs btn-success"),
              downloadButton("dl_edges_float", "📥 Download Graph Edges", class = "btn-xs btn-default")
          )
      )
    )
  ),
    
    # --- The Glass Sidebar Container ----
    # The renderUI now generates the entire sidebar structure (Position, Color, Content)
    uiOutput("right_panel_container")
    
)  # Ends ui fillPage # deleted