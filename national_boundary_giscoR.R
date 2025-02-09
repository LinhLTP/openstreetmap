# Load required libraries
library(osmdata)
library(sf)
library(ggplot2)
library(giscoR)

# Get the Czechia national boundary using giscoR
czechia_sf <- gisco_get_countries(
  year = "2016", epsg = "4326",
  resolution = "3", country = "CZ"
)

# Define a bounding box from the Czechia boundary
bbox <- st_bbox(czechia_sf)

# Build an OSM query for railway features (both 'rail' and 'narrow_gauge')
query <- opq(bbox = bbox, timeout = 180) %>% # increase timeout for querry 
  add_osm_feature(key = 'railway', value = c('rail', 'narrow_gauge'))

rail_osm <- osmdata_sf(query)

# The railway data is typically available as lines
rail_lines <- rail_osm$osm_lines
print(head(rail_lines))

# Visualise the Czech national boundary and railways
p <- ggplot() +
  # Plot Czechia boundary
  geom_sf(
    data = czechia_sf,
    fill = "transparent",
    colour = "#07CFF7", 
    size = 0.1
  ) +
  # Plot railway lines
  geom_sf(
    data = rail_lines,
    colour = "#FFB115", 
    size = 0.15
  ) +
  labs(
    x = "",
    y = "",
    title = "Czech Railways",
    subtitle = "",
    caption = "©2025 Data: ©OpenStreetMap contributors"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(colour = "#010D1F", size = 0),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 24, colour = "grey90", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 10, colour = "grey90", hjust = 0.5, vjust = 0
    ),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    plot.background = element_rect(fill = "#010D1F", colour = NA),
    panel.background = element_rect(fill = "#010D1F", colour = NA),
    legend.background = element_rect(fill = "#010D1F", colour = NA),
    panel.border = element_blank()
  )
p

# Save the map as a PNG image
ggsave(
  filename = "czechia_railways_osmdata.png",
  plot = p,
  width = 7.5, height = 7, dpi = 600, device = "png"
)