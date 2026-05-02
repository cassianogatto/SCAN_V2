


# ///O>\\\ \\\O>//// ///O>\\\ \\\O>//// 
#       ...  THIS IS SCAN V2 ...                          #
# ///O>\\\ \\\O>//// ///O>\\\ \\\O>//// 

library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyjs)

ui <- fillPage(
    useShinyjs(), # Crucial for our navigation buttons
    
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
                tabsetPanel(id = "analysis_subtabs", type = "pills",
                    # --- SUB-TAB 1: MAP ---
                    tabPanel("Map",
                             br(),
                             box(title = "1. Input Map", status = "primary", width = NULL, solidHeader = TRUE,
                                 fileInput("filemap", "Upload Shapefile Components", 
                                           multiple = TRUE, 
                                           accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj")),
                                 
                                 # Diagnosis UI (Dynamic Warning)
                                 uiOutput("map_diagnosis_ui")
                             )
                    ),
                    
                    # --- SUB-TAB 2: Cs ---
                    tabPanel("Cs",
                             br(),
                             box(title = "1. Cs Index Configuration", status = "primary", width = NULL, solidHeader = TRUE,
                                 numericInput("filter_Cs", "Minimum Cs Threshold (0 - 1)", value = 0.1, min = 0, max = 1, step = 0.05),
                                 helpText("Only pairs above this value will be processed."),
                                 
                                 actionButton("calculate_Cs", "RUN Cs ANALYSIS", 
                                              class = "btn-success btn-block", 
                                              style = "font-weight: bold;", icon = icon("play-circle")),
                                 
                                 hr(),
                                 tags$b("or upload a matrix:"),
                                 fileInput("upload_cs_matrix", NULL, accept = ".csv", placeholder = "Upload .csv")
                             )
                    ),
                    
                    # --- SUB-TAB 3: SCAN ---
                    tabPanel("SCAN", 
                             br(),
                             box(title="SCAN Engine", status = "primary", width=NULL, solidHeader = TRUE,
                                 numericInput("resolution", "Resolution (Step):", value = 0.1, step = 0.01, min=0.01),
                                 fluidRow(
                                     column(6, numericInput("threshold_min", "Min Ct:", value = 0.2, step = 0.05)),
                                     column(6, numericInput("threshold_max", "Max Ct:", value = 0.9, step = 0.05))
                                 ),
                                 actionButton("run_scan", "RUN SCAN ANALYSIS", 
                                              class = "btn-danger btn-block", icon = icon("rocket"))
                             )
                    )
                    
                ),   # tabsetPanel (Map, Cs, SCAN) 2may2026
                
            )    # leftsidebar conditional
            
        )   # absolutePanel
        
    ), # scan main conditional tab
    
    # --- 6. SCAN Viewer (Floating Widgets Architecture) ----
    conditionalPanel(condition = "input.top_nav == 'SCAN Viewer'",
                   
                   # FLOATING WIDGET 1: Network Topology
                   absolutePanel(
                       id = "float_network",
                       class = "panel panel-primary",
                       top = 70, left = 500, width = 450, height = "auto",
                       draggable = TRUE, fixed = TRUE,
                       style = "z-index: 1050; box-shadow: 0 4px 15px rgba(0,0,0,0.2); background: rgba(255,255,255,0.95);",
                       
                       div(class = "panel-heading", style="cursor: move; padding: 8px 15px;", 
                           tags$strong(icon("project-diagram"), " Network Topology"),
                           # Minimize Button
                           tags$button(type="button", class="pull-right btn btn-xs btn-primary", 
                                       style="margin-top:-3px; background: transparent; border: none;",
                                       onclick="$('#net_plot_body').slideToggle();", icon("minus"))
                       ),
                       div(id = "net_plot_body", class = "panel-body", style="padding: 5px; ",
                           plotOutput("graph_plot", height = "350px")
                       )
                   ),
                   
                   # FLOATING WIDGET 2: Static Map
                   absolutePanel(
                       id = "float_static_map",
                       class = "panel panel-info",
                       top = 70, left = 1000, width = 450, height = "auto",
                       draggable = TRUE, fixed = TRUE,
                       style = "z-index: 1050; box-shadow: 0 4px 15px rgba(0,0,0,0.2); background: rgba(255,255,255,0.95);",
                       
                       div(class = "panel-heading", style="cursor: move; padding: 8px 15px;", 
                           tags$strong(icon("map"), " Static Map (ggplot)"),
                           # Minimize Button
                           tags$button(type="button", class="pull-right btn btn-xs btn-info", 
                                       style="margin-top:-3px; background: transparent; border: none;",
                                       onclick="$('#stat_plot_body').slideToggle();", icon("minus"))
                       ),
                       div(id = "stat_plot_body", class = "panel-body", style="padding: 5px; ",
                           plotOutput("ggplot_map", height = "350px")
                       ), # <-- 1 may 2026
                       
                       # FLOATING WIDGET 3: Species Table
                       absolutePanel(
                           id = "float_species_list",
                           class = "panel panel-success", # Green styling to distinguish from plots
                           bottom = 20, left = 20, width = 350, height = "auto", # Anchored to the bottom, wide enough for both plots
                           draggable = TRUE, fixed = TRUE,
                           style = "z-index: 1050; box-shadow: 0 4px 15px rgba(0,0,0,0.2); background: rgba(255,255,255,0.95);",
                           
                           div(class = "panel-heading", style="cursor: move; padding: 8px 15px;", 
                               tags$strong(icon("table"), " Species List (Selected Groups)"),
                               # Minimize Button
                               tags$button(type="button", class="pull-right btn btn-xs btn-success", 
                                           style="margin-top:-3px; background: transparent; border: none;",
                                           onclick="$('#species_table_body').slideToggle();", icon("minus"))
                           ),
                           div(id = "species_table_body", class = "panel-body", style="padding: 10px;",
                               DT::DTOutput("view_species_table")
                           )
                       )
                   )
    ), # End SCAN Viewer Conditional Panel

    # --- 7. SCAN VIEWER CONTROLLER (Right Side) ----
    conditionalPanel(
      condition = "input.top_nav == 'SCAN Viewer'",
      
      absolutePanel(
        id = "scan_viewer_controls",
        class = "panel panel-info", 
        top = 70, right = 20, width = 280, # Fixed width, sitting on the right
        style = "z-index: 1100; opacity: 0.95;", # Slightly above the main panel
        draggable = TRUE,
        
        div(class = "panel-heading", tags$h4(icon("cogs"), " Viewer Settings")),
        
        # diagnostics here
        # Keeping the debugger for one last check
        box(width=NULL, title="System Diagnostics", status="warning",
            verbatimTextOutput("debug_viewer_console"))
 # old settings in right panel
      )
    ),
  
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
                                tags$small(em("Available: area_overlap, area_sp1, area_sp2"))
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

# THRASH
# --- FLOATING BOX: CS PREVIEW (Appears only on Cs Tab) ---
# conditionalPanel(
#   # Condition: User is on 'SCAN Analysis' -> 'Cs' tab AND data is available
#   condition = "input.top_nav == 'SCAN Analysis' && input.analysis_subtabs == 'Cs' && output.cs_data_available == true",
#   
#   absolutePanel(
#     id = "cs_floating_box",
#     class = "panel panel-info",
#     fixed = TRUE, draggable = TRUE,
#     top = 130, right = 20, width = 300, height = "auto",
#     style = "z-index: 2000;  box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
#     
#     #div(class = "panel-heading", tags$h4("📊 Cs Results Preview", style="margin: 0; font-size: 16px;")),
#     div(class = "panel-body", style = "max-height: 400px; overflow-y: auto; padding: 10px;",
#         p(class = "text-muted", "Top strong connections:"),
#         tableOutput("mini_nodes_table"), # Defined in server
#         hr(),
#         downloadButton("dl_cs_float", "Download Full Matrix", class = "btn-xs btn-primary btn-block")
#     )
#   )
# ),


# --- FLOATING BOX: SCAN RESULTS (Appears only on SCAN Tab) ---
# conditionalPanel(
#   # Condition: User is on 'SCAN Analysis' -> 'SCAN' tab AND results exist
#   condition = "input.top_nav == 'SCAN Analysis' && input.analysis_subtabs == 'SCAN' && output.scan_results_ready == true",
#   
#   absolutePanel(
#     id = "scan_floating_box",
#     class = "panel panel-success", # Green style for Success/Results
#     fixed = TRUE, draggable = TRUE,
#     top = 130, right = 20, width = 320, height = "auto",
#     style = "z-index: 2000;  box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
#     
#     # --- Header ---
#     div(class = "panel-heading", 
#         tags$h4("🧬 SCAN Chorotypes", style="margin: 0; font-size: 16px;")
#     ),
#     
#     # --- Body: List of Groups ---
#     div(class = "panel-body", style = "padding: 10px;",
#         
#         # 1. Summary Text
#         htmlOutput("scan_summary_text"),
#         hr(style="margin: 5px 0;"),
#         
#         # 2. Scrollable List of Chorotypes
#         p(class = "text-muted", style="font-size: 12px;", "Species & Group Assignment:"),
#         div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; background: white;",
#             tableOutput("scan_chorotype_list")
#         ),
#         
#         hr(),
#         
#         # 3. Quick Downloads
#         div(class = "btn-group-vertical", style="width: 100%;",
#             downloadButton("dl_chorotypes_float", "📥 Download List (.csv)", class = "btn-xs btn-success"),
#             downloadButton("dl_edges_float", "📥 Download Graph Edges", class = "btn-xs btn-default")
#         )
#     )
#   )
# ),