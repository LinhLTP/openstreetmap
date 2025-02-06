# Load required packages
library(extrafont)
library(tidyverse)
library(osmdata)     # For working with OpenStreetMap data
library(showtext)    # For custom fonts
library(leaflet)
library(rvest)
library(here)

# ------------------------------------------------------------------------------
# Note. Remove file committed and pushed from rstudio to github repo
# ------------------------------------------------------------------------------
system("git rm --cached /Users/linhlinh/Documents/github/osm/leaflet_map.html")
system('git commit -am "Remove file"')
system("git push")

# ------------------------------------------------------------------------------
# 1. Set Up Custom Fonts
# ------------------------------------------------------------------------------
# Download and register the Google font "Lato"
font_add_google(name = "Lato", family = "lato")
showtext_auto()  # Automatically use showtext for future plots

# ------------------------------------------------------------------------------
# 2. Define Parameters
# ------------------------------------------------------------------------------
# Define the city location and fetch its bounding box only once
city_location <- "HCMC Vietnam"
bbox <- getbb(city_location)

# Define the OSM features (primary and secondary streets, plus rivers)
primary_street_types <- c("motorway", "primary", "secondary", "tertiary")
secondary_street_types <- c("residential", "living_street", "unclassified", "service", "footway")
water_feature_key <- "waterway"
water_feature_value <- "river"

# Define map limits 
# geo_spatial_limits <- getbb(city_location)
map_xlim <- c(106.60, 106.75)
map_ylim <- c(10.75, 10.86)

# Define colour palette and background colour
col_primary   <- "cyan"
col_secondary <- "#ffbe7f"
col_river     <- "blue"
back_colour   <- "#1d1330"

# ------------------------------------------------------------------------------
# 3. Define Helper Function for Fetching OSM Data
# ------------------------------------------------------------------------------
get_osm_data <- function(bbox, key, value) {
  opq(bbox = bbox) %>% 
    add_osm_feature(key = key, value = value) %>% 
    osmdata_sf()
}

# ------------------------------------------------------------------------------
# 4. Retrieve Spatial Data from OpenStreetMap
# ------------------------------------------------------------------------------
streets       <- get_osm_data(bbox, "highway", primary_street_types)
small_streets <- get_osm_data(bbox, "highway", secondary_street_types)
rivers        <- get_osm_data(bbox, water_feature_key, water_feature_value)

view(streets[["osm_lines"]]) 

# ------------------------------------------------------------------------------
# 5. Create a Base Map 
# ------------------------------------------------------------------------------
# (a) Create the base map
ggplot() + 
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black")

ggplot() +
  geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = "cyan", size = 0.4, alpha = 0.8) +
  geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = "#ffbe7f", size = 0.2, alpha = 0.6) +
  geom_sf(data = river$osm_lines, inherit.aes = FALSE, color = "blue", size = 1) +
  coord_sf(xlim = c(106.60, 106.75), ylim = c(10.75, 10.86)) + 
  theme(plot.background = element_rect(fill = back_color, colour = NA),
        panel.background = element_rect(fill = back_color, colour = NA), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) + 
  theme(plot.title = element_text(family = my_font, size = 13, color = "white")) + 
  theme(plot.margin = unit(rep(0.5, 4), "cm")) + 
  labs(title = "Streetmap of HCMC from OpenStreetMap")

ggplot() +
  geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = "cyan", size = 0.4, alpha = 0.8) +
  geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = "#666666", size = 0.2, alpha = 0.6) +
  geom_sf(data = river$osm_lines, inherit.aes = FALSE, color = "blue", size = 1) +
  coord_sf(xlim = c(106.60, 106.75), ylim = c(10.75, 10.86)) + 
  theme_void() + # get rid of background color, grid lines, etc.
  theme(plot.title = element_text(size = 20, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "HCMC Vietnam", subtitle = "107.0°E / 11.2°N")

# (b) Optional: Highlight a specific street (e.g., "Đinh Tiên Hoàng")
special_street <- streets$osm_lines %>% filter(name == "Đinh Tiên Hoàng")

p <- ggplot() +
  geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = "cyan", size = 0.4, alpha = 0.8) +
  geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = "#666666", size = 0.2, alpha = 0.6) +
  geom_sf(data = river$osm_lines, inherit.aes = FALSE, color = "blue", size = 1) +
  geom_sf(data = tsn, 
          inherit.aes = FALSE,
          color = "red",
          size = 1,
          alpha = 1) + 
  coord_sf(xlim = c(106.60, 106.75), ylim = c(10.75, 10.86)) + 
  theme_void() + # get rid of background color, grid lines, etc.
  theme(plot.title = element_text(size = 10, family = "lato", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "lato", size = 8, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "HCMC Vietnam", subtitle = "107.0°E / 11.2°N")

p

# (c) Optional: Retrieving data of hospitals (polygon)
hcmc_hospitals <- bbox %>%
  opq() %>%
  add_osm_feature("amenity", "hospital") %>%
  osmdata_sf()

g <- ggplot() +
  geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = "cyan", size = 0.4, alpha = 0.8) +
  geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = "#666666", size = 0.2, alpha = 0.6) +
  geom_sf(data = river$osm_lines, inherit.aes = FALSE, color = "blue", size = 1) +
  geom_sf(data = tsn, 
          inherit.aes = FALSE,
          color = "red",
          size = 1,
          alpha = 1) + 
  geom_sf(data = hcmc_hospitals$osm_polygons, inherit.aes = FALSE, colour = "blue", fill = "red", alpha = .5, size = 1) +
  coord_sf(xlim = c(106.60, 106.75), ylim = c(10.75, 10.86), expand = FALSE) + 
  theme_bw() +
  labs(
    title = "Hospitals in HCMC (Vietnam)",
    x = "Latitude",
    y = "Longitude"
  )

g

# ------------------------------------------------------------------------------
# 6. Save the Map
# ------------------------------------------------------------------------------
ggsave(filename = "plot/hcmcmap.pdf", plot = p, bg = "white")
ggsave(filename = "plot/hcmcmap_polygon.jpg", plot = g, bg = "white")

# ------------------------------------------------------------------------------
# 7. Create leaflet map 
# ------------------------------------------------------------------------------
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = hcmc_hospitals$osm_polygons,
    label = hcmc_hospitals$osm_polygons$name
  )

#--- Ref
# Define the city location and fetch its bounding box only once
city_location <- "HCMC Vietnam"
# OSM objects that we want to get spatial 
all_street_types <- c("motorway", "primary", "secondary", "tertiary")
secondary_streets <- c("residential", "living_street", "unclassified", "service", "footway")
# Spatial data
streets <- getbb(city_location) %>% 
  opq() %>% 
  add_osm_feature(key = "highway", value = all_street_types) %>% 
  osmdata_sf()

small_streets <- getbb(city_location) %>% 
  opq() %>% 
  add_osm_feature(key = "highway", value = secondary_streets) %>% 
  osmdata_sf()

river <- getbb(city_location) %>% 
  opq() %>% 
  add_osm_feature(key = "waterway", value = "river") %>% 
  osmdata_sf()
# view data
view(streets[["osm_lines"]]) 
# LONG and LAT limits
geo_spatial_limits <- getbb(city_location)
geo_spatial_limits

