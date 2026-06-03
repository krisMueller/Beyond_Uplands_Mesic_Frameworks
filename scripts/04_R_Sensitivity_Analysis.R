library(sf)
library(dplyr)
library(jsonlite)
library(ggplot2)

# ── 1. Load ────────────────────────────────────────────────────────────────────
df <- read.csv("../data/input_data/YYYYMMDD_valleyBottom_persistence_stratifiedSample_75k.csv")

# ── 2. Parse geometry ──────────────────────────────────────────────────────────
coords <- lapply(df$.geo, function(g) fromJSON(g)$coordinates)

valley_sample <- df |>
  mutate(
    longitude = sapply(coords, `[`, 1),
    latitude  = sapply(coords, `[`, 2)
  ) |>
  filter(
    !is.na(longitude),
    !is.na(latitude),
    # persistence_pct_ndvi03 > 0      # ← replaces persistence_class != 0
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ── 3. Fit model ───────────────────────────────────────────────────────────────
model <- lm(persistence_pct_ndvi04 ~ persistence_pct_ndvi03, data = valley_sample)
r2    <- round(summary(model)$r.squared, 3)
slope <- round(coef(model)[2], 3)
n     <- format(nrow(valley_sample), big.mark = ",")

# ── 4. Plot ────────────────────────────────────────────────────────────────────
ggplot(valley_sample, aes(x = persistence_pct_ndvi03, y = persistence_pct_ndvi04)) +
  geom_jitter(alpha = 0.08, size = 0.6, color = "steelblue",
              width = 1.0, height = 1.0) +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE, linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  annotate("text", x = 5, y = 95,
           label = paste0("R² = ", r2, "\nslope = ", slope, "\nn = ", n),
           hjust = 0, vjust = 1, size = 4, color = "gray20", fontface = "bold") +
  labs(
    x        = "Persistence % — NDVI 03",
    y        = "Persistence % — NDVI 04",
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.subtitle = element_text(color = "gray50", size = 10))