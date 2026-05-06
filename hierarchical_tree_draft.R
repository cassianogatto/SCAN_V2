# install.packages(c("dplyr", "stringr", "ggplot2", "ggalluvial", "tidygraph", "ggraph"))

library(dplyr)
library(stringr)
library(ggplot2)
library(ggalluvial)
library(tidygraph)
library(ggraph)


# ==========================================
# 1. READ & PREPARE THE DATA
# ==========================================
file_path <- "SCAN_Groups_2026-05-04_Birds_50prcnt_AE_ECO_SIRGAS2000_0.15-0.9Ct_0.05res.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Create a "Root Lineage" column (e.g., 1.1.2 becomes 1) so we can color-code 
# entire evolutionary families with a single color.
df <- df %>%
  mutate(Root_Lineage = str_extract(Chorotype_ID, "^[0-9]+"))

# ==========================================
# 2. PLOT 1: THE ALLUVIAL FLOW (SANKEY)
# ==========================================
# This tracks the actual flow of individual species as the network fragments.

p_alluvial <- ggplot(df,
                     aes(x = as.factor(Threshold), 
                         stratum = Chorotype_ID, 
                         alluvium = Species,
                         fill = Root_Lineage, 
                         label = Chorotype_ID)) +
  # Draw the flows between thresholds
  geom_flow(stat = "alluvium", lode.guidance = "frontback", color = "darkgray", alpha = 0.4) +
  # Draw the blocks for each chorotype
  geom_stratum(alpha = 0.9, color = "black") +
  # Add labels inside the blocks
  geom_text(stat = "stratum", aes(label = Chorotype_ID), size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank()) +
  labs(title = "Chorotype Evolution (Species Flow)",
       subtitle = "Block height = N Species. Ribbons = Species tracking across thresholds.",
       x = "Spatial Congruence Threshold (Ct)",
       y = "Number of Species")

# 3 alternativo
# ==========================================
# 3. PREPARE THE TREE DATA (NODES & EDGES) - FIXED
# ==========================================
# 1. Create a unique ID for every single point on the graph (Chorotype + Threshold)
nodes <- df %>%
  arrange(Threshold) %>%
  select(Chorotype_ID, Threshold, N_Species, Root_Lineage) %>%
  distinct() %>%
  mutate(
    Node_UID = paste(Chorotype_ID, Threshold, sep = "_"),
    Parent_Name = ifelse(str_detect(Chorotype_ID, "\\."), 
                         str_replace(Chorotype_ID, "\\.[^\\.]+$", ""), 
                         NA_character_)
  )

# 2. Build edges by looking step-by-step at the thresholds
thresholds <- sort(unique(nodes$Threshold))
edges_list <- list()

for(i in 2:length(thresholds)) {
  curr_t <- thresholds[i]
  prev_t <- thresholds[i-1]
  
  curr_nodes <- nodes %>% filter(Threshold == curr_t)
  prev_nodes <- nodes %>% filter(Threshold == prev_t)
  
  for(j in 1:nrow(curr_nodes)) {
    child_id <- curr_nodes$Chorotype_ID[j]
    child_uid <- curr_nodes$Node_UID[j]
    parent_name <- curr_nodes$Parent_Name[j]
    
    # Case A: SURVIVAL (Identity) - Name is exactly the same as the previous step
    if(child_id %in% prev_nodes$Chorotype_ID) {
      parent_uid <- paste(child_id, prev_t, sep="_")
      edges_list[[length(edges_list)+1]] <- data.frame(from = parent_uid, to = child_uid)
    }
    # Case B: SPLIT (Differentiation) - Name is a child of a previous step
    else if(!is.na(parent_name) && parent_name %in% prev_nodes$Chorotype_ID) {
      parent_uid <- paste(parent_name, prev_t, sep="_")
      edges_list[[length(edges_list)+1]] <- data.frame(from = parent_uid, to = child_uid)
    }
  }
}
edges <- do.call(rbind, edges_list)

# 3. Build the tidygraph object using the Unique IDs
tree_graph <- tbl_graph(nodes = nodes, edges = edges, node_key = "Node_UID")


# ==========================================
## ==========================================
# 4. PLOT 2: THE LINEAGE TREE (DAG)
# ==========================================
# We create a layout and forcefully map the Y-axis to the exact Ct Threshold

# Generate a basic tree layout
lay <- create_layout(tree_graph, layout = 'tree')

# Override the Y coordinate with the actual Threshold values
lay$y <- lay$Threshold

p_tree <- ggraph(lay) +
  # Draw branching lines
  geom_edge_diagonal(alpha = 0.6, color = "gray40", width = 1) +
  # Draw nodes sized by species count
  geom_node_point(aes(size = N_Species, fill = Root_Lineage), shape = 21, color = "black", alpha = 0.9) +
  # Add labels slightly offset from the nodes (UPDATED: label = name)
  #geom_node_text(aes(label = name), vjust = -1, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  scale_size_continuous(range = c(3, 7)) +
  theme_graph() +
  theme(legend.position = "right") +
  labs(title = "Chorotype Lineage Dendrogram",
       subtitle = "Y-Axis mapped to strict Ct Thresholds",
       size = "Species Count",
       fill = "Root Lineage")

p_tree <- ggraph(lay) +
  # Draw branching lines
  geom_edge_diagonal(alpha = 0.6, color = "gray40", width = 1) +
  
  # Draw nodes sized by species count (using your adjusted sizes)
  geom_node_point(aes(size = N_Species, fill = Root_Lineage), shape = 21, color = "black", alpha = 0.9) +
  
  # Add labels (assuming you filtered or cleaned these as you mentioned)
  # geom_node_text(aes(label = name), vjust = -1, size = 3.5, fontface = "bold") +
  
  scale_fill_brewer(palette = "Set1") +
  scale_size_continuous(range = c(3, 7)) + # Your size scale
  
  # --- NEW: Explicitly set the Y-axis breaks to match your Ct thresholds ---
  scale_y_continuous(breaks = unique(nodes$Threshold)) + 
  ylab("Spatial Congruence Threshold (Ct)") +
  
  theme_graph() +
  
  # --- NEW: Turn the Y-axis visual elements back on ---
  theme(
    legend.position = "right",
    axis.title.y = element_text(size = 12, face = "bold", angle = 90, margin = margin(r = 15)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.ticks.y = element_line(color = "black", size = 0.5),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.line.y = element_line(color = "black", size = 0.5)
  )
# ==========================================
# 5. RENDER IN DISTINCT WINDOWS
# ==========================================
# Open Window 1 for Alluvial
# dev.new(width = 12, height = 7) 
# print(p_alluvial)

# Open Window 2 for Tree
# dev.new(width = 10, height = 8) 
print(p_tree)
 head(df)
 
 
 