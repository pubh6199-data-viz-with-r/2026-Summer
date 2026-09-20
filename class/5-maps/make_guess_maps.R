# ---------------------------------------------------------------
# make_guess_maps.R
# Renders 3 mystery + 3 reveal choropleth PNGs for the
# "Guess That Map" in-class activity (Lecture 5).
#
# Run once before class:
#   Rscript make_guess_maps.R
# Outputs land in images/ and are referenced by index.qmd.
#
# Values are approximate, drawn from public CDC / ACS reporting
# circa 2022 -- intended for classroom pattern-recognition,
# not for analysis. Update freely.
# ---------------------------------------------------------------

if (!requireNamespace("usmap", quietly = TRUE)) install.packages("usmap")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(usmap)
library(ggplot2)

out_dir <- "images"
dir.create(out_dir, showWarnings = FALSE)

# state.abb is alphabetical (50 states, no DC)
maps_data <- list(
  diabetes = list(
    label  = "Adult diabetes prevalence",
    units  = "% adults",
    source = "CDC BRFSS, 2022",
    values = c(14.4, 8.3, 11.1, 13.6, 10.5, 7.2, 9.5, 11.5, 11.2, 12.0,
               11.0, 9.8, 10.9, 11.5, 9.6, 10.6, 13.7, 13.7, 10.9, 11.1,
               8.8, 11.4, 8.5, 14.8, 11.7, 8.6, 9.7, 11.1, 9.2, 9.4,
               11.5, 10.6, 12.0, 8.4, 12.5, 13.2, 9.7, 11.1, 9.5, 13.2,
               9.5, 13.7, 12.4, 8.0, 7.7, 11.4, 9.7, 16.2, 8.7, 8.5)
  ),
  overdose = list(
    label  = "Drug overdose death rate",
    units  = "deaths per 100k (age-adjusted)",
    source = "CDC, 2022",
    values = c(25.8, 35.0, 32.0, 22.0, 27.0, 28.5, 38.5, 54.0, 33.5, 19.0,
               17.0, 18.5, 35.0, 38.5, 12.4, 18.0, 49.3, 56.8, 39.4, 45.0,
               36.6, 31.0, 27.5, 19.0, 32.6, 18.0,  8.5, 41.5, 39.0, 35.7,
               41.0, 32.5, 38.0, 11.0, 47.5, 24.5, 26.5, 41.0, 41.5, 28.0,
               14.5, 47.0, 21.0, 22.0, 33.0, 29.0, 33.5, 80.9, 23.0, 14.0)
  ),
  uninsured = list(
    label  = "Uninsured rate (under 65)",
    units  = "% uninsured",
    source = "ACS, 2022",
    values = c(9.2, 13.2, 11.8, 8.5, 6.5, 7.6, 5.0, 5.7, 11.2, 11.4,
               3.9, 9.9, 6.7, 7.8, 4.4, 8.0, 6.0, 8.3, 5.8, 6.0,
               2.4, 5.0, 4.0, 11.5, 9.6, 7.7, 7.0, 11.4, 5.4, 7.4,
               10.0, 5.3, 10.4, 6.6, 6.5, 13.6, 6.2, 5.5, 4.0, 9.7,
               8.7, 9.2, 16.6, 9.1, 3.7, 6.7, 5.7, 6.1, 5.4, 11.9)
  )
)

# Use the SAME palette and a consistent visual style across all three maps
# so students can't cheat by palette differences -- the only signal is the
# spatial pattern itself. That is the point of the game.
PALETTE <- "YlOrRd"

render <- function(values, slug, label, units, source) {
  df <- data.frame(state = state.abb, value = values)

  # ---- Mystery: no title, no legend, no source ----
  p_mystery <- plot_usmap(data = df, values = "value", color = "white") +
    scale_fill_distiller(palette = PALETTE, direction = 1, guide = "none") +
    theme(legend.position = "none",
          plot.margin = margin(0, 0, 0, 0),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))

  ggsave(file.path(out_dir, paste0("guess_map_", slug, ".png")),
         p_mystery, width = 10, height = 6.5, dpi = 200, bg = "white")

  # ---- Reveal: title + legend + source ----
  p_reveal <- plot_usmap(data = df, values = "value", color = "white") +
    scale_fill_distiller(palette = PALETTE, direction = 1, name = units) +
    labs(title = label,
         caption = paste("Source:", source)) +
    theme(legend.position = "right",
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 10, color = "gray40"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))

  ggsave(file.path(out_dir, paste0("guess_map_", slug, "_reveal.png")),
         p_reveal, width = 10, height = 6.5, dpi = 200, bg = "white")

  message("  wrote guess_map_", slug, ".png + _reveal.png")
}

message("Rendering Guess That Map images...")
for (slug in names(maps_data)) {
  m <- maps_data[[slug]]
  render(m$values, slug, m$label, m$units, m$source)
}
message("done.")
