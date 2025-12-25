# --- PARALLEL TERRA Cs CALCULATOR ---- 
# --- NON PARALLEL IS BELOW THIS CHUNK OF CODE ---
# Optimized for Windows & Large Datasets (4,200+ species)

library(dplyr)
library(sf)
library(terra)
library(data.table)
library(parallel)
library(doParallel)
library(foreach)

#### SF LOAD, CRS, VALID, PREPARATION ####
message("Phase 1: Loading and cleaning in SF...")
shp_path <- file.choose()

#shp_sp_column <- readline(prompt = "Which column is unique ID?: ")

shp_sf <- st_read(shp_path)

# Find which column contains 'taxon' (case-insensitive)
taxon_col <- grep("taxon", names(shp_sf), ignore.case = TRUE, value = TRUE)

if (!("sp" %in% names(shp_sf)) && length(taxon_col) > 0) {
    shp_sf <- shp_sf %>% rename(sp = !!sym(taxon_col[1]))
    message(paste("Renamed", taxon_col[1], "to sp"))
}
if (!"sp" %in% names(shp_sf)) {
    stop("Error: Could not find a species column named 'sp' or 'Taxon'. 
        Please check your shapefile attributes: ", paste(names(shp_sf), collapse=", "))
}

shp_sf <- shp_sf %>%  select(sp) %>%                     # DROP all other columns immediately
    st_transform(102033) %>%           # Project first
    st_make_valid()

# Proceed with the dissolve (Aggregation)
message("Phase 1b: Dissolving geometries by species. This might take a while...")

shp_sf_clean <- shp_sf %>% 
    group_by(sp) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_make_valid() %>%
    ungroup()

# Check the result
message("Success! You now have ", nrow(shp_sf_clean), " unique species.")
print(head(shp_sf_clean))

#check sp column
if (!"sp" %in% names(shp_sf_clean)) {
    stop("Error: Could not find a species column named 'sp' or 'Taxon'. 
        Please check your shapefile attributes: ", paste(names(shp_sf_clean), collapse=", "))
}
message("Phase 2: Converting to Terra for high-speed calculus...")
# Converting at this stage is safe because bird_sf is now simple and clean
shp_v <- vect(shp_sf_clean)


#### TERRA ENVIRONMENT ####
# Load your bird shapefile (or continue with shp_v if already loaded)
# shp_v <- vect("your_bird_file.shp")
# shp_v <- project(shp_v, "epsg:102033") 

message("Cleaning geometries and calculating areas...")

n_sp <- nrow(shp_v)
sp_names <- as.character(shp_v$sp)

# terra serialization
shp_packed <- wrap(shp_v)

# 2. SETUP CLUSTER
# --- FINAL AVIAN SCAN ENGINE ---
# Optimized for 4,200+ species on Windows i7

# 1. PREPARATION
# Load your bird shapefile here
# shp_v <- vect("Birds_Subspecies.shp") 
shp_v <- terra::makeValid(shp_v)
shp_v$area_total <- as.numeric(terra::expanse(shp_v))


# 2. CHECKPOINT SETUP
checkpoint_file <- "Birds_Cs_Checkpoint.csv"
# If restarting, check which species was last saved
start_from <- 1 
if (file.exists(checkpoint_file)) {
    existing_data <- fread(checkpoint_file)
    last_sp1 <- tail(existing_data$sp1, 1)
    start_from <- which(as.character(shp_v$sp) == last_sp1) + 1
}

# 3. CLUSTER SETUP
# 2. Setup Cluster
# cores <- detectCores() / 2 # Standard procedure for 16G RAM or detectCores() - 2 32G RAM
cores <- 3 # Leaving 1 thread for Windows stability on your old i7
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
clusterExport(cl, varlist = c("shp_packed", "n_sp"))

message(sprintf("Starting birds from index %d on %d cores...", start_from, cores))
start_time <- Sys.time()

# 4. PARALLEL LOOP
final_results <- foreach(i = start_from:(n_sp - 1), .combine = rbind, .packages = c("terra", "data.table")) %dopar% {
    
    local_shp <- vect(shp_packed)
    s1 <- local_shp[i]
    s1_name <- as.character(s1$sp)
    s1_area <- as.numeric(s1$area_total)
    
    remaining_vect <- local_shp[(i+1):nrow(local_shp)]
    hits <- terra::relate(s1, remaining_vect, "intersects")
    
    if (any(hits)) {
        potential_indices <- which(as.vector(hits))
        worker_results <- list()
        
        for (idx in potential_indices) {
            s2 <- remaining_vect[idx]
            s2_area <- as.numeric(s2$area_total)
            inter <- terra::intersect(s1, s2)
            
            area_int <- 0
            if (!is.null(inter)) {
                if (is.list(inter)) {
                    area_int <- sum(sapply(inter, function(x) sum(as.numeric(terra::expanse(x)))))
                } else if (nrow(inter) > 0) {
                    area_int <- sum(as.numeric(terra::expanse(inter)))
                }
            }
            
            if (area_int > 0) {
                cs_val <- (area_int / s1_area) * (area_int / s2_area)
                if (cs_val > 0.1) {
                    worker_results[[length(worker_results) + 1]] <- data.table(
                        sp1 = s1_name, sp2 = as.character(s2$sp), Cs = cs_val
                    )
                }
            }
        }
        
        # Save progress immediately to disk
        if(length(worker_results) > 0) {
            res_dt <- rbindlist(worker_results)
            fwrite(res_dt, "Birds_Cs_Checkpoint.csv", append = TRUE)
            return(res_dt)
        }
    }
    return(NULL)
}

stopCluster(cl)
message(sprintf("Spatial Congruence Index Analysis Complete! Total time: %.2f hours", 
                difftime(Sys.time(), start_time, units="hours")), " / pls check Cs sheet Birds_Cs_Checkpoint.csv")



head(final_results)





#### NON PARALLEL CALCULUS USING TERRA ----
library(sf)
library(terra)
library(dplyr)
library(data.table)

# --- PHASE 1: SF CLEANING ---
message("Phase 1: Loading and cleaning in SF...")
shp_path <- file.choose()

#shp_sp_column <- readline(prompt = "Which column is unique ID?: ")

shp_sf <- st_read(shp_path)

# Find which column contains 'taxon' (case-insensitive)
taxon_col <- grep("taxon", names(shp_sf), ignore.case = TRUE, value = TRUE)

if (!("sp" %in% names(shp_sf)) && length(taxon_col) > 0) {
    shp_sf <- shp_sf %>% rename(sp = !!sym(taxon_col[1]))
    message(paste("Renamed", taxon_col[1], "to sp"))
}
if (!"sp" %in% names(shp_sf)) {
    stop("Error: Could not find a species column named 'sp' or 'Taxon'. 
        Please check your shapefile attributes: ", paste(names(shp_sf), collapse=", "))
}

shp_sf <- shp_sf %>%  select(sp) %>%                     # DROP all other columns immediately
    st_transform(102033) %>%           # Project first
    st_make_valid()

# Proceed with the dissolve (Aggregation)
message("Phase 1b: Dissolving geometries by species. This might take a while...")

shp_sf_clean <- shp_sf %>% 
    group_by(sp) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_make_valid() %>%
    ungroup()



# Check the result
message("Success! You now have ", nrow(shp_sf_clean), " unique species.")
print(head(shp_sf_clean))

# --- PHASE 2: HANDOFF TO TERRA ---
message("Phase 2: Converting to Terra for high-speed calculus...")
# Converting at this stage is safe because shp_sf is now simple and clean
shp_v <- vect(shp_sf_clean)

# Double check terra alignment
if(nrow(shp_v) != nrow(as.data.frame(shp_v))) {
    stop("Mismatch detected! Reconstructing pointer...")
    shp_v <- setValues(geom(shp_v), as.data.frame(shp_v))
}

# Add area (expanse is faster in terra)
# 1. Force the area calculation directly in terra to be sure
shp_v$area_total <- as.numeric(terra::expanse(shp_v))

# 2. Check if it worked (You should see numbers now, not empty spaces)
print(shp_v$area_total[1:5])

# 3. Double check the column names
print(names(shp_v))

shp_v

# 1. Final check on variables
list_of_cs_tables <- list()
n_sp <- nrow(shp_v)
start_time <- Sys.time()

# 2. The Robust Loop
for (i in 1:(n_sp - 1)) {
    s1 <- shp_v[i]
    s1_name <- as.character(s1$sp)
    s1_area <- as.numeric(s1$area_total)
    
    # Filter candidates
    remaining_vect <- shp_v[(i+1):n_sp]
    hits <- tryCatch({ terra::relate(s1, remaining_vect, "intersects") }, 
                     error = function(e) return(NULL))
    
    if (!is.null(hits) && any(hits)) {
        potential_indices <- which(as.vector(hits))
        
        for (idx in potential_indices) {
            s2 <- remaining_vect[idx]
            s2_name <- as.character(s2$sp)
            s2_area <- as.numeric(s2$area_total)
            
            # Intersect
            inter <- tryCatch({ terra::intersect(s1, s2) }, 
                              error = function(e) return(NULL))
            
            # Extract area safely from SpatVector or List
            area_int <- 0
            if (!is.null(inter)) {
                if (is.list(inter)) {
                    area_int <- sum(sapply(inter, function(x) sum(as.numeric(terra::expanse(x)))))
                } else if (nrow(inter) > 0) {
                    area_int <- sum(as.numeric(terra::expanse(inter)))
                }
            }
            
            # Cs Math
            if (isTRUE(area_int > 0)) {
                cs_val <- (area_int / s1_area) * (area_int / s2_area)
                
                if (isTRUE(cs_val > 0.1)) {
                    list_of_cs_tables[[length(list_of_cs_tables) + 1]] <- data.table(
                        sp1 = s1_name, sp2 = s2_name, Cs = cs_val
                    )
                }
            }
        }
    }
    
    # Checkpoint every 20 species
    if (i %% 20 == 0) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        message(sprintf("Sp %d/%d | Found %d pairs | Time elapsed: %.1f seconds", 
                        i, n_sp, length(list_of_cs_tables), elapsed))
    }
}

total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
final_results <- rbindlist(list_of_cs_tables)
fwrite(final_results, "Cs_Results_Final.csv")

message("--- TEST COMPLETE ---")
message(sprintf("Total Time: %.1f seconds", total_time))
message(sprintf("Total Congruent Pairs Found: %d", nrow(final_results)))








#### APENDIX convert between similarity matrices styles

# Convert Square Matrix to Pairwise Dataframe ---- 
#' @param mat A square matrix with species names as row/column names
#' @param threshold Minimum Cs value to keep (default 0.1)
#' @param remove_self Boolean; if TRUE, removes the diagonal (e.g., Alouatta vs Alouatta)
matrix_to_pairwise <- function(mat, threshold = 0.1, remove_self = TRUE) {
    
    # 1. Convert matrix to a long-format dataframe
    # using lower.tri to avoid duplicate pairs (A-B and B-A)
    if (remove_self) {
        # Keep only the lower triangle (one direction, no diagonal)
        mask <- lower.tri(mat)
    } else {
        # Keep lower triangle plus the diagonal
        mask <- lower.tri(mat, diag = TRUE)
    }
    
    # 2. Extract indices and values
    indices <- which(mask, arr.ind = TRUE)
    values <- mat[mask]
    
    # 3. Build the dataframe
    df <- data.frame(
        sp1 = rownames(mat)[indices[, 1]],
        sp2 = colnames(mat)[indices[, 2]],
        Cs  = as.numeric(values)
    )
    
    # 4. Filter by threshold and return
    return(df[df$Cs >= threshold, ])
}


# Example usage with your monkey matrix
my_tidy_df <- matrix_to_pairwise(cs_matrix, threshold = 0.1)

# View the result
head(my_tidy_df)


# R Code for your 4,200 Bird Matrix spp x spp ---- 

# Once your bird run finishes, you can use this R script to generate the final matrix from your Birds_Cs_Checkpoint.csv. It ensures that every species from your original shapefile is included, even if it had zero congruent pairs.
# R

library(data.table)

# 1. Load your results
dt <- fread("Birds_Cs_Checkpoint.csv")

# 2. Define the full species list (use the original order from your shp_v)
# This ensures the matrix matches your GIS attributes perfectly
all_sp <- as.character(shp_v$sp)
n <- length(all_sp)

# 3. Create a sparse matrix (memory efficient for 4,200 species)
# If RAM allows, you can use a standard matrix:
cs_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(all_sp, all_sp))

# 4. Fill the matrix symmetrically
# We use match() to find the correct coordinates for each species name
idx1 <- match(dt$sp1, all_sp)
idx2 <- match(dt$sp2, all_sp)

for(i in 1:nrow(dt)) {
    r <- idx1[i]
    c <- idx2[i]
    val <- dt$Cs[i]
    
    cs_matrix[r, c] <- val
    cs_matrix[c, r] <- val
}

# 5. Set Identity Diagonal
diag(cs_matrix) <- 1

# 6. Save for SCAN analysis
write.csv(cs_matrix, "SCAN_Final_Matrix_Birds.csv", row.names = TRUE)
message(sprintf("Final Matrix Saved: %d x %d species.", n, n))