


# ///O>\\\ \\\O>//// ///O>\\\ \\\O>//// 
#       ...  THIS IS SCAN V2 ...                          #
# ///O>\\\ \\\O>//// ///O>\\\ \\\O>//// 

library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)

ui <- fillPage(
    useShinyjs(), # Crucial for our navigation buttons
    
    # --- 1. CSS & Header ----
    tags$style(type = "text/css", "
    html, body {width:100%; height:100%; margin:0; padding:0; overflow: hidden;}
    
    /* --- TOP NAV BAR --- */
    .top-nav-bar {
      position: absolute; top: 0; left: 0; width: 100%; height: 50px;
      background-color: rgba(44, 62, 80, 0.95); z-index: 2000; 
      display: flex; align-items: center; padding: 0 20px; color: white;
    }
    .nav-item { margin-right: 25px; cursor: pointer; font-weight: 500; }
    .nav-item:hover { color: #18bc9c; }

    /* --- UNIFIED GLASS EFFECT --- */
    /* Applies to the Scroll Panel (Settings/Workspace), Sidebars, and Floating Panels */
    .scroll-panel, .left-sidebar, .panel-default, .panel-info, .panel-primary, .panel-success {
      background-color: rgba(255, 255, 255, 0.7) !important; 
      backdrop-filter: blur(10px); 
      border: 1px solid rgba(255, 255, 255, 0.3) !important;
      box-shadow: 0 8px 32px rgba(0,0,0,0.2) !important;
      border-radius: 12px !important;
    }

    .left-sidebar {
      height: calc(100vh - 70px);
      overflow-y: auto; padding: 15px; border-radius: 0 15px 15px 0 !important;
      z-index: 1000;
    }

    .scroll-panel {
      max-height: 85vh; overflow-y: auto; padding: 30px; z-index: 1500;
    }

    /* --- CLEANER HEADINGS --- */
    .panel-heading {
      background-color: rgba(44, 62, 80, 0.05) !important;
      color: #2c3e50 !important;
      font-weight: bold;
      border-bottom: 1px solid rgba(0,0,0,0.05) !important;
    }
    
    .panel-default, .panel-info, .panel-primary, .panel-success {
    min-width: 280px; /* Prevents the box from getting too skinny */
    }   
  "),
    

  # ---  Top Navigation Bar (Updated) ----
  tags$div(class = "top-nav-bar",
           
           # ABOUT
           tags$div(class = "nav-item", 
                    onclick = "Shiny.setInputValue('top_nav', 'About SCAN');", 
                    "About SCAN"),
           
           # WORKSPACE MANAGER (NEW)
           tags$div(class = "nav-item", 
                    onclick = "Shiny.setInputValue('top_nav', 'Workspace');", 
                    "Workspace"),
           
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
                    "Settings&Files"),
  ),
    
    # --- 3. The Leaflet Map Background ----
    leafletOutput("map", width = "100%", height = "100%"),
    
    
    # --- 4. Merged Content: Info & Documentation updated 2may2026 ----
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
    
    # --- 4.5 NEW TAB: WORKSPACE MANAGER ----
  conditionalPanel(
      condition = "input.top_nav == 'Workspace'",
      absolutePanel(
          top = 70, left = "15%", right = "15%",
          div(class = "scroll-panel",
              h2(icon("folder-open"), " Workspace Manager"),
              p("Start a new analysis from scratch, or load a previously saved session."),
              hr(),
              
              fluidRow(
                  # Left Side: Start New
                  column(6,
                         box(title = "Start New Analysis", status = "primary", width = NULL, solidHeader = TRUE,
                             p("Upload raw shapefiles, configure parameters, and run the SCAN engine from scratch."),
                             tags$br(),
                             actionButton("btn_jump_analysis", "Go to SCAN Analysis", class = "btn-primary btn-block", 
                                          icon = icon("arrow-right"), onclick = "Shiny.setInputValue('top_nav', 'SCAN Analysis');")
                         )
                  ),
                  
                  # Right Side: Load Existing
                  column(6,
                         box(title = "Load Existing Project", status = "warning", width = NULL, solidHeader = TRUE,
                             p("Resume a previously saved session (.rds file) without recalculating the Cs matrix or graphs."),
                             fileInput("load_project", "Upload Project File (.rds)", accept = c(".rds")),
                             tags$hr(),
                             p(strong("Save Current Workspace:")),
                             downloadButton("save_project", "Save Current Project (.rds)", class = "btn-default btn-block")
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
                        # --- NEW CONSOLIDATED ANALYSIS CHECKLIST ---
                        div(class = "analysis-checklist", style = "margin-top: 15px;",
                            
                            # --- BOX 1: MAP WORKSHOP ---
                            box(title = "Step 1: Map Upload", status = "primary", width = NULL, solidHeader = TRUE,
                                tags$small("Upload map geometry (all components):"),
                                fileInput("filemap", NULL, multiple = TRUE, accept = c('.shp','.dbf','.shx',".prj")),
                                uiOutput("map_diagnosis_ui")
                            ),
                            
                            # --- BOX 2: SPATIAL CONGRUENCE (Cs) ---
                            box(title = "Step 2: Calculate Cs Index", status = "primary", width = NULL, solidHeader = TRUE,
                                numericInput("filter_Cs", "Minimum Cs Threshold (0 - 1)", value = 0.1, min = 0, max = 1, step = 0.05),
                                
                                # Warning Box
                                div(style = "font-size: 0.9em; color: #856404; background-color: rgba(255, 243, 205, 0.7); border: 1px solid #ffeeba; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                                    icon("exclamation-triangle"), 
                                    "Note: Cutting the tail of lower values optimizes computation.But remmeber: low Cs values can still be biogeographically informative for some unusual groups."
                                ),
                                
                                actionButton("calculate_Cs", "RUN Cs ANALYSIS", 
                                             class = "btn-success btn-block", style = "font-weight: bold; font-size: 1.1em;", icon = icon("play-circle")),
                                
                                hr(),
                                tags$b("Or Upload Cs Matrix (.csv):"),
                                fileInput("upload_cs_matrix", NULL, accept = ".csv")
                            ),
                            
                            # --- BOX 3: NETWORK ANALYSIS (SCAN) ---
                            box(title="Step 3: Run SCAN Engine", status = "danger", width=NULL, solidHeader = TRUE,
                                numericInput("resolution", "Resolution (Ct Step):", value = 0.1, step = 0.01, min=0.01),
                                fluidRow(
                                    column(6, numericInput("threshold_min", "Min Ct:", value = 0.2)),
                                    column(6, numericInput("threshold_max", "Max Ct:", value = 0.9))
                                ),
                                actionButton("run_scan", "RUN SCAN ANALYSIS", 
                                             class = "btn-danger btn-block", icon = icon("rocket")),
                                
                                hr(),
                                tags$b("Or Load Previous Results:"),
                                # Only requests Edges and Nodes now
                                fileInput("upload_scan_csvs", "Upload Edges & Nodes (.csv)", 
                                          multiple = TRUE, accept = ".csv")
                            )
                        ) # End checklist div
                    ) # End left-sidebar div
      ) # End absolutePanel
    ), # End SCAN Analysis conditionalPanel
    
    
    # --- 6. SCAN Viewer (Floating Widgets Architecture - DETACHED) ----
    conditionalPanel(condition = "input.top_nav == 'SCAN Viewer'",
                   
             # FLOATING WIDGET 1: GRAPH PLOT
             absolutePanel(
                 id = "float_network", class = "panel panel-primary",
                 top = "5%", left = "30%", width = "25%", 
                 draggable = TRUE, fixed = TRUE,
                 style = "z-index: 1050;",
                 
                 div(class = "panel-heading", style="cursor: move;", 
                     tags$strong(icon("project-diagram"), " Network Topology"),
                     tags$button(type="button", class="pull-right btn btn-xs btn-primary", 
                                 onclick="$('#net_plot_body').slideToggle();", icon("minus"))
                 ),
                 div(id = "net_plot_body", class = "panel-body", 
                     plotOutput("graph_plot", height = "350px")) # Back to fixed height
             ),
                 
             # FLOATING WIDGET 2: GGMAP PLOT
             absolutePanel(
                 id = "float_static_map", class = "panel panel-info",
                 top = "5%", left = "57%", width = "25%", 
                 draggable = TRUE, fixed = TRUE, style = "z-index: 1050;",
                 
                 div(class = "panel-heading", style="cursor: move;", 
                     tags$strong(icon("map"), " Static Map (ggplot)"),
                     tags$button(type="button", class="pull-right btn btn-xs btn-info", 
                                 onclick="$('#stat_plot_body').slideToggle();", icon("minus"))
                 ),
                 div(id = "stat_plot_body", class = "panel-body", 
                     plotOutput("ggplot_map", height = "350px")) # Back to fixed height
             ),
               
            # FLOATING WIDGET 3: Species Table
            absolutePanel(
               id = "float_species_list", class = "panel panel-success",
               top = "5%",   
               left = "1%",  
               width = "25%", 
               #max-height = "90%",
               draggable = TRUE, fixed = TRUE,
               style = "z-index: 1060;
               max-height: 1000px; overflow-y: auto;", 
               div(class = "panel-heading", style="cursor: move;", 
                   tags$strong(icon("table"), " Species List (Selected Groups)"),
                   tags$button(type="button", class="pull-right btn btn-xs btn-success", 
                               onclick="$('#species_table_body').slideToggle();", icon("minus"))
               ),
               div(id = "species_table_body", class = "panel-body", 
                   style="padding: 10px;",
                   DT::DTOutput("view_species_table")
               )
            )
    ), # End SCAN Viewer Conditional Panel


    # --- PANEL: SETTINGS & FILES (Downloads) ----
  
     conditionalPanel(
         condition = "input.top_nav == 'Settings&Files'",
         absolutePanel(
             top = 70, left = "10%", right = "10%",
             div(class = "scroll-panel",
                 h2(icon("cogs"), " Global Settings & Workspace Office"),
                 hr(),
                 
                 fluidRow(
                     # COLUMN 1: PREPARATION
                     column(6,
                            # --- BOX: GEOSPATIAL OFFICE ---
                            box(title = "Geospatial Office", status = "primary", width = NULL, solidHeader = TRUE,
                                checkboxInput("fix_invalid", "Fix Geometries (st_make_valid)?", TRUE),
                                checkboxInput("modify_crs", "Enable Custom Projection", value = FALSE),
                                conditionalPanel("input.modify_crs == true",
                                                 textInput("map_projection", "EPSG String:", value = "102033")
                                ),
                                hr(),
                                checkboxInput("use_buffer_map", "Enable Geometry Buffering?", value = FALSE),
                                conditionalPanel("input.use_buffer_map == true",
                                                 numericInput("buffer_dist", "Buffer Dist (meters/deg):", value = 0),
                                                 checkboxGroupInput("quantiles_to_buffer", "Size Quartiles:", 
                                                                    choices = c("Q1"=1, "Q2"=2, "Q3"=3, "Q4"=4), selected = 1, inline = TRUE)
                                ),
                                actionButton("apply_mods", "APPLY GEOSPATIAL SETTINGS", class = "btn-primary btn-block")
                            ),
                            
                            # --- BOX: Cs FORMULA LAB ---
                            box(title = "Cs Laboratory (Formulas)", status = "info", width = NULL, solidHeader = TRUE,
                                textInput("cs_similarity_index", "Custom Formula:", value = '(area_overlap / area_sp1) * (area_overlap / area_sp2)'),
                                tags$small(em("Available: area_overlap, area_sp1, area_sp2")),
                                tags$small(em("Alternatively you can build your own Cs similarity matrix and upload; use a standard SCAN Cs matrix as a template."))
                            )
                     ),
                     
                     # COLUMN 2: ENGINE & PERFORMANCE
                     column(6,
                            box(title = "Computational Engine", status = "warning", width = NULL, solidHeader = TRUE,
                                radioButtons("calc_engine", "Calculus Library:", choices = c("sf" = "engine_sf", "terra" = "engine_terra"), inline = TRUE),
                                radioButtons("calc_mode", "Core Mode:", choices = c("Serial" = "mode_serial", "Parallel" = "mode_parallel"), inline = TRUE),
                                conditionalPanel("input.calc_mode == 'mode_parallel'",
                                                 sliderInput("num_cores", "Cores:", min = 2, max = 8, value = 2)
                                ),
                                radioButtons("memory_strategy", "Memory Strategy:", 
                                             choices = c("Full Load" = "mem_all", "Chunked" = "mem_chunk")),
                                conditionalPanel("input.memory_strategy == 'mem_chunk'",
                                                 numericInput("chunk_size", "Batch Size (spp):", value = 20)
                                )
                            ),
                            
                            # --- BOX: VISUAL PREFERENCES ---
                            box(title = "Visual Defaults", status = "success", width = NULL, solidHeader = TRUE,
                                sliderInput("alpha_global", "Default Transparency:", min=0, max=1, value = 0.3, step=0.1),
                                selectInput("palette_global", "Default Palette:", choices = c("Set2", "Set1", "Paired", "Dark2"), selected = "Set2")
                            )
                     )
                 ),
                 
                 # --- SECTION: DOWNLOADS (Cleaned up) ---
                 hr(),
                 h3(icon("download"), "Data Export"),
                 box(title = "Results Export", status = "danger", width = NULL,
                     fluidRow(
                         # FIRST ROW: GEOMETRY & MATRIX
                         column(6, downloadButton("dl_map", "📥 Export Map (SHP)", class = "btn-block btn-primary")),
                         column(6, downloadButton("dl_cs", "📥 Export Cs Matrix", class = "btn-block btn-success"))
                     ),
                     br(), # Tiny space between rows
                     fluidRow(
                         # SECOND ROW: NETWORK & GROUPS
                         column(4, downloadButton("dl_chorotypes", "📥 Chorotypes", class = "btn-block")),
                         column(4, downloadButton("dl_edges", "📥 Graph Edges", class = "btn-block")),
                         column(4, downloadButton("dl_nodes", "📥 Graph Nodes", class = "btn-block"))
                     )
                 )
             )
         )
     ),
  

    # --- The Glass Sidebar Container ----
    # The renderUI now generates the entire sidebar structure (Position, Color, Content)
    uiOutput("right_panel_container")
    
)  # Ends ui fillPage # deleted
