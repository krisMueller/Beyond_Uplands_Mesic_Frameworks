################################################################################
# SCRIPT: 03_R_Conservation_Tiers.R
# MANUSCRIPT: Beyond Uplands: Integrating Riparian Areas into Sagebrush Conservation Design
#
# DESCRIPTION:
# This script processes HUC12 metrics, classifies watersheds into conservation 
# tiers, generates summary figures (including state-level reports), and exports 
# a final classified spatial dataset.
#
# USAGE:
# 1. Open in RStudio.
# 2. Set Working Directory to Source File Location.
# 3. Ensure "YYYYMMDD_HUC12_DGO_proportions_Metrics_SHP.shp" and "tl_2022_us_state.shp" are in "../data/" relative to this script. (this folder and files are there by default)
################################################################################

# ==============================================================================
# PART 0: INITIAL SETUP & DATA CLASSIFICATION
# ==============================================================================

# --- Load Required Libraries ---
library(sf)
library(tidyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(scales)
library(forcats)
library(patchwork)

# --- Setup Output Directories ---
export_date <- format(Sys.Date(), "%Y%m%d")   # ---- ASSIGNS CURRENT DATE TO BEGINNING OF OUTPUTS -----

# Define output folders
output_fig_dir <- "outputs/figures"
output_tab_dir <- "outputs/tables"
output_shp_dir <- "outputs/spatial"

dir.create(output_fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(output_shp_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load Data ---
# Using the cleaned filenames and relative paths
hucs_path   <- "data/input_data/YYYYMMDD_HUC12_DGO_proportions_Metrics_SHP.shp"
states_path <- "data/input_data/tl_2022_us_state.shp"

if (!file.exists(hucs_path)) stop("DATA MISSING: HUC12 Shapefile not found in data/")
if (!file.exists(states_path)) stop("DATA MISSING: States Shapefile not found in data/")

hucs <- st_read(hucs_path)
states <- st_read(states_path)

# Transform to Albers Equal Area (EPSG:5070)
hucs <- st_transform(hucs, crs = 5070)
states_proj <- st_transform(states, crs = 5070)

# Create ID and separate geometry
hucs <- hucs %>% mutate(HUC_ID = row_number())
hucs_df <- st_drop_geometry(hucs)

# --- Define Thresholds ---
core_growth_threshold <- 0.15
protect_high_persistence_threshold <- 0.65
restore_foundation_threshold_H <- 0.15
restore_opportunity_threshold_M <- 0.15
restore_opportunity_threshold_L <- 0.15
manage_persistence_threshold <- 0.15

# --- Classify Watersheds ---
hucs_classified <- hucs_df %>%
  mutate(
    # Clean NAs
    PrVW5kC_clean = coalesce(PrVW5kC, 0),
    PrVW5kG_clean = coalesce(PrVW5kG, 0),
    PrVPclL_clean = coalesce(PrVPclL, 0),
    PrVPclM_clean = coalesce(PrVPclM, 0),
    PrVPclH_clean = coalesce(PrVPclH, 0),
    PrVPr_clean   = coalesce(PrVPr, 0),
    PrVPu_clean   = coalesce(PrVPu, 0),
    
    # Calculate Metrics
    core_growth_proportion     = PrVW5kC_clean + PrVW5kG_clean,
    high_mod_persistence_score = PrVPclH_clean + PrVPclM_clean,
    vb_area_private_m2         = ArVBm2 * PrVPr_clean,
    vb_area_public_m2          = ArVBm2 * PrVPu_clean,
    
    # Assign Tiers
    management_category = case_when(
      core_growth_proportion >= core_growth_threshold & PrVPclH_clean >= protect_high_persistence_threshold ~ "Tier 1",
      core_growth_proportion >= core_growth_threshold & PrVPclH_clean >= restore_foundation_threshold_H & (PrVPclM_clean >= restore_opportunity_threshold_M | PrVPclL_clean >= restore_opportunity_threshold_L) ~ "Tier 2",
      core_growth_proportion >= core_growth_threshold & high_mod_persistence_score >= manage_persistence_threshold ~ "Tier 3",
      TRUE ~ "Other"
    )
  )

# Global Plotting Variables
priority_colors <- c("Tier 1" = "#ce4800", "Tier 2" = "#016228", "Tier 3" = "#aa9e00")
priority_levels <- c("Tier 1", "Tier 2", "Tier 3")

cat("--- Setup Complete: Data classified. ---\n")


# ==============================================================================
# PART 1: OVERALL SUMMARY REPORT AND PLOT
# ==============================================================================
cat("\n--- Running Part 1: Overall Summary ---\n")

management_summary <- hucs_classified %>%
  group_by(management_category) %>%
  summarise(
    num_hucs = n(), 
    total_vb_area_ha = sum(ArVBm2, na.rm = TRUE) / 10000,
    total_vb_area_public_ha = sum(vb_area_public_m2, na.rm = TRUE) / 10000,
    total_vb_area_private_ha = sum(vb_area_private_m2, na.rm = TRUE) / 10000, 
    .groups = 'drop'
  ) %>%
  mutate(
    percent_total_hucs = (num_hucs / nrow(hucs_classified)) * 100,
    management_category = fct_rev(factor(management_category, levels = c("Tier 1", "Tier 2", "Tier 3", "Other")))
  )

# Export Table
total_row <- tibble(
  management_category = "Total", 
  num_hucs = nrow(hucs_classified),
  total_vb_area_ha = sum(hucs_classified$ArVBm2, na.rm=TRUE)/10000
)
write.csv(bind_rows(management_summary, total_row), 
          file = file.path(output_tab_dir, sprintf("%s_Overall_Summary_Table.csv", export_date)), 
          row.names = FALSE)

# Generate Plot
plot_df <- management_summary %>% filter(management_category %in% priority_levels)

if (nrow(plot_df) > 0) {
  max_pct_overall <- max(plot_df$percent_total_hucs, na.rm = TRUE)
  top_limit_overall <- ceiling(max_pct_overall / 5) * 5
  
  p_hucs <- ggplot(plot_df, aes(x = percent_total_hucs, y = management_category, fill = management_category)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = sprintf("%.0f%%", percent_total_hucs), hjust = -0.2), size = 6, fontface = "bold") +
    scale_fill_manual(values = priority_colors, guide = "none") +
    scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, top_limit_overall), expand = expansion(mult = c(0, 0.1))) +
    labs(title = "Proportion of Watersheds", x = NULL, y = NULL) +
    theme_minimal(base_size = 14) 
  
  ggsave(filename = file.path(output_fig_dir, sprintf("%s_Overall_Priority_Summary.png", export_date)), 
         plot = p_hucs, width = 6, height = 4.5, dpi = 300, bg = "white")
}


# ==============================================================================
# PART 2: INDIVIDUAL STATE-LEVEL REPORTS
# ==============================================================================
cat("\n--- Running Part 2: State-Level Reports ---\n")

# Identify states that intersect with the HUCs
intersecting_states_list <- st_join(hucs, states_proj, join = st_intersects) %>%
  st_drop_geometry() %>% pull(NAME) %>% unique() %>% na.omit()

# Function to generate report
generate_state_report <- function(current_state_name, hucs_in_state, state_boundary) {
  
  hucs_df_state <- st_drop_geometry(hucs_in_state)
  
  # Filter to just this state's data in the classified dataframe
  state_classified <- hucs_classified %>% 
    filter(HUC_ID %in% hucs_df_state$HUC_ID)
  
  plot_df_state <- state_classified %>%
    group_by(management_category) %>%
    summarise(
      num_hucs = n(), 
      total_vb_area_ha = sum(ArVBm2, na.rm=TRUE)/10000,
      total_vb_area_public_ha = sum(vb_area_public_m2, na.rm=TRUE)/10000,
      total_vb_area_private_ha = sum(vb_area_private_m2, na.rm=TRUE)/10000, 
      .groups='drop'
    ) %>%
    filter(management_category %in% priority_levels) %>%
    mutate(
      percent_total_hucs = (num_hucs / nrow(state_classified)) * 100, 
      management_category = fct_rev(factor(management_category, levels = priority_levels))
    )
  
  if(nrow(plot_df_state) > 0) {
    
    # 1. Map Panel
    map_data <- hucs_in_state %>% 
      left_join(select(state_classified, HUC_ID, management_category), by = "HUC_ID") %>% 
      mutate(management_category = factor(management_category, levels = c("Tier 1", "Tier 2", "Tier 3", "Other")))
    
    p_map_state <- ggplot() +
      geom_sf(data = map_data, aes(fill = management_category), color = "lightgrey", lwd = 0.03) + 
      geom_sf(data = state_boundary, fill = NA, color = "black", lwd = 1) +
      scale_fill_manual(name = NULL, values = c(priority_colors, "Other"=NA), na.translate=FALSE) + 
      theme_minimal(base_size = 14) + theme(legend.position = "bottom") + labs(title = current_state_name)
    
    # 2. Watershed Count Panel
    max_pct_state <- max(plot_df_state$percent_total_hucs, na.rm = TRUE)
    top_limit_state <- max(5, ceiling(max_pct_state / 5) * 5)
    
    p_hucs_state <- ggplot(plot_df_state, aes(x = percent_total_hucs, y = management_category, fill = management_category)) +
      geom_col(width = 0.8) + 
      geom_text(aes(label = sprintf("%.0f%%", percent_total_hucs), hjust = -0.1), size = 5, fontface = "bold") +
      scale_fill_manual(values = priority_colors, guide = "none") +
      scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, top_limit_state * 1.2)) +
      labs(title = "Proportion of Watersheds", x = NULL, y = NULL) + theme_minimal(base_size = 14)
    
    # 3. Ownership Panel
    ownership_df_state <- plot_df_state %>%
      mutate(pct_public = if_else(total_vb_area_ha > 0, total_vb_area_public_ha / total_vb_area_ha, 0), 
             pct_private = if_else(total_vb_area_ha > 0, total_vb_area_private_ha / total_vb_area_ha, 0)) %>%
      select(management_category, pct_public, pct_private) %>%
      pivot_longer(cols = c("pct_public", "pct_private"), names_to = "ownership_type", values_to = "percentage") %>%
      mutate(ownership_type = recode(ownership_type, "pct_public" = "Public", "pct_private" = "Private"))
    
    p_ownership_state <- ggplot(ownership_df_state, aes(x = percentage, y = management_category, fill = factor(ownership_type, levels = c("Private", "Public")))) +
      geom_col(width = 0.8) + 
      geom_text(aes(label = if_else(percentage > 0.05, sprintf("%.0f%%", percentage * 100), ""), color = ownership_type), position = position_stack(vjust = 0.5), size = 6, fontface = "bold") +
      scale_fill_manual(name = "", values = c("Public" = "black", "Private" = "lightgrey")) + 
      scale_color_manual(values = c("Private" = "black", "Public" = "white"), guide = "none") +
      scale_x_continuous(labels = scales::percent_format(), breaks = c(0, 0.5, 1.0)) +
      labs(title = "Ownership of Valley Bottoms", x = NULL, y = NULL) + 
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
    
    # Combine
    combined_plot_state <- p_map_state | (p_hucs_state / p_ownership_state) + plot_layout(widths = c(1.5, 1))
    
    safe_state_name <- gsub(" ", "_", current_state_name)
    ggsave(filename = file.path(output_fig_dir, sprintf("%s_State_Summary_%s.png", export_date, safe_state_name)), 
           plot = combined_plot_state, width = 10, height = 7.5, dpi = 300, bg = "white")
  }
}

# Run Loop
for (state_name in intersecting_states_list) {
  cat(paste("Processing:", state_name, "...\n"))
  state_boundary <- states_proj %>% filter(NAME == state_name)
  hucs_in_state <- st_filter(hucs, state_boundary)
  
  if (nrow(hucs_in_state) > 0) {
    generate_state_report(state_name, hucs_in_state, state_boundary)
  }
}


# ==============================================================================
# PART 3: CROSS-STATE OWNERSHIP COMPARISON
# ==============================================================================
cat("\n--- Running Part 3: Cross-State Comparison ---\n")

hucs_with_states <- st_join(hucs, st_transform(states, crs = st_crs(hucs)), join = st_intersects) %>%
  st_drop_geometry() %>% select(HUC_ID, State = NAME) %>% na.omit() %>% distinct(HUC_ID, .keep_all = TRUE)

classified_with_states <- hucs_classified %>%
  left_join(hucs_with_states, by = "HUC_ID") %>% filter(!is.na(State))

m2_to_acres <- 1 / 4046.86

state_summary_acres <- classified_with_states %>%
  filter(management_category %in% priority_levels) %>%
  group_by(State, management_category) %>%
  summarise(
    public_acres = sum(vb_area_public_m2, na.rm = TRUE) * m2_to_acres,
    private_acres = sum(vb_area_private_m2, na.rm = TRUE) * m2_to_acres,
    .groups = 'drop'
  )

state_order <- state_summary_acres %>%
  group_by(State) %>% summarise(total_acres = sum(public_acres, na.rm=TRUE) + sum(private_acres, na.rm=TRUE)) %>%
  arrange(desc(total_acres)) %>% pull(State)

plot_data_states <- state_summary_acres %>%
  mutate(State = factor(State, levels = rev(state_order)), management_category = factor(management_category, levels = priority_levels))

state_totals <- state_summary_acres %>%
  group_by(State) %>% summarise(total_public = sum(public_acres), total_private = sum(private_acres), .groups = 'drop')
max_limit <- max(c(state_totals$total_public, state_totals$total_private), na.rm = TRUE) * 1.05

final_state_plot <- ggplot(plot_data_states, aes(y = State, fill = management_category)) +
  geom_col(aes(x = -private_acres), width = 0.8) + 
  geom_col(aes(x = public_acres), width = 0.8) +
  geom_vline(xintercept = 0, color = "grey40") +
  annotate("text", x = -max_limit, y = length(state_order) + 0.8, label = "Private Lands", fontface = "bold", hjust = 0) +
  annotate("text", x = max_limit, y = length(state_order) + 0.8, label = "Public Lands", fontface = "bold", hjust = 1) +
  scale_x_continuous(labels = function(x) scales::comma(abs(x) / 1000000, accuracy = 1), limits = c(-max_limit, max_limit)) +
  scale_fill_manual(values = priority_colors, name = "") +
  coord_cartesian(clip = 'off') +
  labs(x = "Area (millions of acres)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.margin = margin(t = 25, r = 10, b = 5, l = 10), legend.position = "bottom", axis.text.y = element_text(face = "bold"))

print(final_state_plot)
ggsave(filename = file.path(output_fig_dir, sprintf("%s_State_Ownership_Comparison.png", export_date)), 
       plot = final_state_plot, width = 6, height = 5, dpi = 300, bg = "white")


# ==============================================================================
# PART 4: BIOME-WIDE MAP & SHAPEFILE EXPORT
# ==============================================================================
cat("\n--- Running Part 4: Biome Map and Spatial Export ---\n")

# 1. Join Classification to Geometry
hucs_spatial_classified <- hucs %>%
  left_join(hucs_classified %>% select(HUC_ID, management_category), by = "HUC_ID") %>%
  mutate(Tier = management_category) %>%
  mutate(Tier = factor(Tier, levels = c("Tier 1", "Tier 2", "Tier 3", "Other")))

# 2. Generate Biome-Wide Map
cat("Generating Biome-wide Map (this may take a moment)...\n")
biome_states <- states_proj %>% filter(NAME %in% intersecting_states_list)

biome_map <- ggplot() +
  geom_sf(data = hucs_spatial_classified, aes(fill = Tier), color = NA) +
  geom_sf(data = biome_states, fill = NA, color = "black", lwd = 0.5) +
  scale_fill_manual(values = c(priority_colors, "Other" = "grey90"), na.value = "grey90", name = "Conservation Tier") +
  labs(title = NULL, subtitle = NULL) +
  coord_sf(datum = st_crs(5070)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right", 
    panel.grid.major = element_line(color = "grey90", linetype = "dashed"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

print(biome_map)
ggsave(filename = file.path(output_fig_dir, sprintf("%s_Biome_Wide_Tier_Map.png", export_date)),
       plot = biome_map, width = 12, height = 8, dpi = 300, bg = "white")

# 3. Export Classified Shapefile
cat("Exporting classified Shapefile...\n")

# UPDATED: Only retain HUC Name, Tier, and Management Action
final_export_sf <- hucs_spatial_classified %>%
  mutate(
    # Create the text description field based on the Tier
    # Using 'MgmtAction' (10 chars) to ensure shapefile DBF compatibility
    MgmtAction = case_when(
      Tier == "Tier 1" ~ "Protect & Maintain",
      Tier == "Tier 2" ~ "Restore & Enhance",
      Tier == "Tier 3" ~ "Strategically Manage",
      TRUE ~ "Other"
    )
  ) %>%
  # Select EXACTLY and ONLY the requested fields (geometry is kept automatically by sf)
  select(HUC_Name = name, Tier, MgmtAction)

st_write(final_export_sf, dsn = file.path(output_shp_dir, sprintf("%s_Classified_Watersheds_Tiered.shp", export_date)), delete_dsn = TRUE, quiet = TRUE)

cat(paste0("\n--- SCRIPT EXECUTION COMPLETE ---\n"))