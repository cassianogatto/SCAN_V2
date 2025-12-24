# ==============================================================================
# PROJECT: SPATIAL COHESION (Cs) CALCULATOR - FINAL VERIFIED ADDRESSING
# ==============================================================================



# 1. SETUP & LIBRARIES ---------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
pacman::p_load(sf, terra, data.table, parallel, doParallel, foreach, dplyr)
library(dplyr)
library(sf)
library(terra)
library(data.table)
library(parallel)
library(doParallel)
library(foreach)
# Configuration
# --- NEW: AUTO-NAMING CHECKPOINT ---

# 1. SETUP PATHS
 setwd("D:/Amazon_Birds/Rego_Thesis") 
# setwd("D:/Amazon_monkeys")
# setwd("D:/Amazon_birds/Data_tables_&_Shapefiles")
working_dir <- getwd() ; working_dir

shp_path    <- file.choose() ; shp_path
shp_base_name <- tools::file_path_sans_ext(basename(shp_path)) ; shp_base_name

# --- THE FIX: Create ONE absolute path variable ---
checkpoint_full_path <- file.path(working_dir, paste0(shp_base_name, "_Cs_Checkpoint.csv")) ; checkpoint_full_path
final_output_file    <- file.path(working_dir, paste0(shp_base_name, "_Cs_FINAL_CLEAN.csv")) ; final_output_file 

message(">>> WORKERS WILL WRITE TO: ", checkpoint_full_path)
target_crs      <- "ESRI:102033" # South America Albers Equal Area

# number of processor cores in parallel
cores_to_use    <- 4             # KEEP LOW (4-6) to save RAM for the geometries

# ==============================================================================
# PHASE 1: LOAD & CLEAN (SF ENGINE)
# ==============================================================================
message(">>> Phase 1: Loading Shapefile...")
shp_sf <- st_read(shp_path)#, quiet = TRUE)

# Standardize 'sp' column
taxon_col <- grep("taxon|sp|species", names(shp_sf), ignore.case = TRUE, value = TRUE)[1]
if (is.na(taxon_col)) stop("ERROR: No 'sp' or 'Taxon' column found!")
names(shp_sf)[names(shp_sf) == taxon_col] <- "sp"

# Project, Validate, and Dissolve (Aggregate)
message(">>> Phase 1b: Cleaning and Dissolving geometries...")
shp_sf_clean <- shp_sf %>% 
  select(sp) %>% 
  st_transform(target_crs) %>% 
  st_make_valid() %>% 
  group_by(sp) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  st_make_valid() %>% 
  ungroup()

message(paste(">>> Loaded", nrow(shp_sf_clean), "unique species."))

# ==============================================================================
# PHASE 2: CONVERT TO TERRA (SPEED ENGINE)
# ==============================================================================
message(">>> Phase 2: Converting to Terra & Pre-calculating Areas...")

# Convert to Terra Vector
shp_v <- vect(shp_sf_clean); shp_v

# *** CRITICAL STEP: Pre-calculate Area ***
# We do this NOW so we don't calculate it 4 million times later.
shp_v$area_total <- terra::expanse(shp_v)

# Pack object for Parallel Export
shp_packed <- terra::wrap(shp_v); shp_packed
n_sp <- nrow(shp_v) ; n_sp

# 4. FREE MEMORY (The Critical Step)
rm(shp_v, shp_sf, shp_sf_clean)
gc() # Force R to release the RAM back to Windows immediately

message(">>> Memory cleaned. Ready for parallel export.")


# ==============================================================================
# PHASE 3: PARALLEL CLUSTER SETUP & EXECUTION (CORRECTED)
# ==============================================================================

# 1. RESUME LOGIC --------------------------------------------------------------
start_from <- 1
if (exists("final_output_file") && file.exists(final_output_file)) {
  existing_data <- fread(final_output_file)
  if (nrow(existing_data) > 0) {
    last_sp <- tail(existing_data$sp1, 1)
    match_idx <- which(shp_v$sp == last_sp)
    if (length(match_idx) > 0) start_from <- match_idx + 1
  }
}
message(paste(">>> Starting processing from Index:", start_from))

# 2. CLUSTER SETUP -------------------------------------------------------------
if(exists("cl")) stopCluster(cl)
cl <- makePSOCKcluster(cores_to_use)
registerDoParallel(cl)

# Export 'shp_base_name' to prevent silent crashes
clusterExport(cl, varlist = c("shp_packed", "n_sp", "working_dir", "shp_base_name"))

clusterEvalQ(cl, { 
  library(terra)
  library(data.table)
  setwd(working_dir) 
})


# 3. THE LOOP (MULTI-FILE MODE) ------------------------------------------------
message(">>> Phase 4: Running Parallel Intersections (Cs > 0.1)...")

 start_time <- Sys.time()
final_results <- foreach(i = start_from:(n_sp - 1), .combine = rbind, 
                         .packages = c("terra", "data.table")) %dopar% {
                           
                           worker_id <- Sys.getpid()
                           worker_filename <- file.path(working_dir, paste0(shp_base_name, "_worker_", worker_id, ".csv"))
                           
                           tryCatch({
                             local_shp <- terra::vect(shp_packed)
                             s1 <- local_shp[i]
                             s1_name <- as.character(s1$sp)
                             s1_area <- s1$area_total
                             
                             remaining_vect <- local_shp[(i + 1):n_sp]
                             hits <- as.vector(terra::relate(s1, remaining_vect, "intersects"))
                             
                             if (any(hits)) {
                               neighbors <- remaining_vect[hits]
                               # Rename Area to avoid confusion, but 'sp' name collision is handled below
                               names(neighbors)[names(neighbors) == "area_total"] <- "s2_area"
                               
                               intersections <- terra::intersect(s1, neighbors)
                               
                               if (!is.null(intersections) && nrow(intersections) > 0) {
                                 intersections$area_int <- terra::expanse(intersections)
                                 res_dt <- as.data.table(intersections)
                                 res_dt[, Cs := (area_int / s1_area) * (area_int / s2_area)]
                                 
                                 # --- THE FIX: BULLETPROOF COLUMN DETECTION ---
                                 # We check ALL possible names 'terra' might generate
                                 if ("sp_2" %in% names(res_dt)) {
                                   sp2_col <- "sp_2"
                                 } else if ("sp.2" %in% names(res_dt)) {
                                   sp2_col <- "sp.2"
                                 } else if ("sp_1" %in% names(res_dt) && "sp" %in% names(res_dt)) {
                                   # If s1 became 'sp' and s2 became 'sp_1'
                                   sp2_col <- "sp_1" 
                                 } else if ("sp.1" %in% names(res_dt)) {
                                   sp2_col <- "sp.1"
                                 } else {
                                   sp2_col <- "sp" # Fallback
                                 }
                                 
                                 # Scientific Filter
                                 final_dt <- res_dt[Cs > 0.1, .(sp1 = s1_name, sp2 = get(sp2_col), Cs = Cs)]
                                 
                                 if (nrow(final_dt) > 0) {
                                   data.table::fwrite(final_dt, worker_filename, append = TRUE)
                                   return(final_dt) 
                                 }
                               }
                             }
                             return(NULL)
                           }, error = function(e) return(NULL))
                         }



message(">>> SUCCESS! Total time: ", difftime(Sys.time(), start_time, units="hours"), " hours")

# Stop cluster to release RAM and File Locks
stopCluster(cl)


# ==============================================================================
# PHASE 5: CONSOLIDATION (The "Safety Net")
# ==============================================================================
temp_files <- list.files(path = working_dir, pattern = paste0(shp_base_name, "_worker_"), full.names = TRUE)
message(">>> Worker files found: ", length(temp_files))

if(length(temp_files) > 0){
  # Load all worker files
  all_data <- rbindlist(lapply(temp_files, fread))
  
  # --- SAFETY: Normalize Column Names ---
  if ("sp_1" %in% names(all_data)) setnames(all_data, "sp_1", "sp1")
  if ("sp_2" %in% names(all_data)) setnames(all_data, "sp_2", "sp2")
  
  # Clean Headers and Duplicates
  # Now safe to use 'sp1' because of the safety check above
  if ("sp1" %in% names(all_data)) {
    all_data <- all_data[sp1 != "sp1"]
    all_data <- unique(all_data, by = c("sp1", "sp2"))
    
    # Save Final
    final_output <- file.path(working_dir, paste0(shp_base_name, "_Cs_FINAL_TOTAL.csv"))
    fwrite(all_data, final_output)
    
    message(">>> SUCCESS! Final data saved to: ", final_output)
    message(">>> Total pairs: ", nrow(all_data))
    
    # Optional: Clean up temp files
    # file.remove(temp_files)
  } else {
    message(">>> ERROR: Column names mismatch. Check temp files manually.")
  }
  
} else {
  message(">>> FINISHED. No files generated.")
}

# old
# # 3. THE LOOP
# message(">>> Phase 4: Running Parallel Intersections...")
# start_time <- Sys.time()
# message(start_time)
# 
# # 3. THE LOOP (Multi-File Approach)
# message(">>> Phase 4: Running Parallel Intersections (Multi-File Mode)...")
# 
# final_results <- foreach(i = start_from:(n_sp - 1), .combine = rbind, 
#                          .packages = c("terra", "data.table")) %dopar% {
#                            
#                            # NEW: Identify the worker to create a unique filename
#                            worker_id <- Sys.getpid()
#                            # This creates something like: range_polygons_worker_12440.csv
#                            worker_filename <- file.path(working_dir, paste0(shp_base_name, "_worker_", worker_id, ".csv"))
#                            
#                            tryCatch({
#                              local_shp <- terra::vect(shp_packed)
#                              s1 <- local_shp[i]
#                              s1_name <- as.character(s1$sp)
#                              s1_area <- s1$area_total
#                              
#                              remaining_vect <- local_shp[(i + 1):n_sp]
#                              hits <- as.vector(terra::relate(s1, remaining_vect, "intersects"))
#                              
#                              if (any(hits)) {
#                                neighbors <- remaining_vect[hits]
#                                names(neighbors)[names(neighbors) == "area_total"] <- "s2_area"
#                                
#                                intersections <- terra::intersect(s1, neighbors)
#                                
#                                if (!is.null(intersections) && nrow(intersections) > 0) {
#                                  intersections$area_int <- terra::expanse(intersections)
#                                  res_dt <- as.data.table(intersections)
#                                  res_dt[, Cs := (area_int / s1_area) * (area_int / s2_area)]
#                                  
#                                  sp2_col <- if("sp.1" %in% names(res_dt)) "sp.1" else "sp"
#                                  final_dt <- res_dt[Cs > 0.1, .(sp1 = s1_name, sp2 = get(sp2_col), Cs = Cs)]
#                                  
#                                  if (nrow(final_dt) > 0) {
#                                    # Each worker writes to its OWN private file
#                                    data.table::fwrite(final_dt, worker_filename, append = TRUE)
#                                    return(final_dt) 
#                                  }
#                                }
#                              }
#                              return(NULL)
#                            }, error = function(e) return(NULL))
#                          }

# # Once the loop is done, stop the cluster immediately
# stopCluster(cl)
# 
# # 1. List all worker files
# temp_files <- list.files(path = working_dir, 
#                          pattern = paste0(shp_base_name, "_worker_"), 
#                          full.names = TRUE)
# 
# message(">>> Merging ", length(temp_files), " worker files...")
# 
# # 2. Read and Bind all tables
# all_data <- rbindlist(lapply(temp_files, fread))
# 
# # 3. Clean and Save Final Result
# # Remove headers and duplicates
# all_data <- all_data[sp1 != "sp1"]
# all_data <- unique(all_data, by = c("sp1", "sp2"))
# 
# final_output <- file.path(working_dir, paste0(shp_base_name, "_Cs_FINAL_TOTAL.csv"))
# fwrite(all_data, final_output)
# 
# # 4. Cleanup: Delete the temp worker files
# # file.remove(temp_files) # Uncomment this if you want to delete them automatically
# 
# message(">>> DONE! Final consolidated file: ", final_output)


# 
# stopCluster(cl)
# 
# # 4. FINAL CLEANUP (Matching the absolute path)
# if (file.exists(checkpoint_full_path)) {
#   full_data <- fread(checkpoint_full_path)
#   full_data <- full_data[sp1 != "sp1"] # Remove headers
#   full_data <- unique(full_data, by = c("sp1", "sp2"))
#   full_data[, Cs := as.numeric(Cs)]
#   
#   fwrite(full_data, final_output_file)
#   
#   message(">>> SUCCESS! Total time: ", difftime(Sys.time(), start_time, units="hours"), " hours")
#   message(">>> Results saved to: ", final_output_file)
# } else {
#   message(">>> CRITICAL ERROR: File not found at ", checkpoint_full_path)
# }
