#----------------------------------------------------------------------------------------
# Analysis of Bicycle Infrastructure Using OpenStreetMap Data
#
# The workflow includes:
#
#   1. Extracting various roadway types from Atlanta and combining them.
#   2. Visualising the road network at broad and local (1-mile buffer) scales.
#   3. Extracting and processing bicycle-related infrastructure data for the Ponce & Monroe area.
#   4. Categorising on-street and off-street bike infrastructure and generating interactive 
#      maps and static plots.
#   5. Calculating the total lengths of different bike infrastructure types.
#----------------------------------------------------------------------------------------

#------------------------------------------------------------
# Load Required Libraries
#------------------------------------------------------------
library(osmdata)      # For querying and retrieving OSM data
library(sf)           # For handling spatial data using simple features
library(mapview)      # For creating interactive maps
library(ggmap)        # For additional map visualisation tools
library(tidygeocoder) # For geocoding addresses via OSM (Nominatim)
library(dplyr)        # For data manipulation
library(tidyverse)    # For data manipulation and visualisation functions
library(RColorBrewer) # For colour palettes in plots
library(gridExtra)    # For arranging multiple plots into a grid layout
library(knitr)

#------------------------------------------------------------
# Data Extraction: Retrieve Atlanta Road Network Data
#------------------------------------------------------------
# Extract primary roads from Atlanta, GA and convert to an sf object.
atl_primary = opq(bbox = "Atlanta, Georgia, USA") %>%
  add_osm_feature(key = "highway", value = "primary") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%    # Extract line features representing primary roads
  st_as_sf()         # Convert to a simple features (sf) object

atl_primary %>% mapview()  

# Extract motorway roads from Atlanta.
atl_motorway = opq(bbox = "Atlanta, Georgia, USA") %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%   
  st_as_sf() 

# Extract trunk roads from Atlanta.
atl_trunk = opq(bbox = "Atlanta, Georgia, USA") %>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%    
  st_as_sf() 

# Extract secondary roads from Atlanta.
atl_secondary = opq(bbox = "Atlanta, Georgia, USA") %>%
  add_osm_feature(key = "highway", value = "secondary") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%    
  st_as_sf()         

# Extract tertiary roads from Atlanta.
atl_tertiary = opq(bbox = "Atlanta, Georgia, USA") %>%
  add_osm_feature(key = "highway", value = "tertiary") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%    
  st_as_sf()         

class(atl_primary)  # Verify that the primary road dataset is of class 'sf'

# Combine the various road datasets into one comprehensive dataset.
atl_roads <-  atl_primary %>%
  bind_rows(
    atl_motorway,
    atl_trunk,
    atl_secondary,
    atl_tertiary
  )

# Display a frequency table of the highway types in the combined dataset.
table(atl_roads$highway)

#------------------------------------------------------------
# Data Visualisation: Explore the Atlanta Road Network
#------------------------------------------------------------

# Attempt an initial interactive visualisation of the combined road network.
atl_roads %>% 
  mapview(zcol = "highway")
# Note: An error may occur here due to duplicate column names.
# Error fix: 
# Check column names across individual datasets to determine common fields.
colnames(atl_primary)
colnames(atl_motorway)
colnames(atl_trunk)
colnames(atl_secondary)
colnames(atl_tertiary)

# Identify the intersection of column names shared by all datasets.
common_cols <- Reduce(intersect, list(
  colnames(atl_primary),
  colnames(atl_motorway),
  colnames(atl_trunk),
  colnames(atl_secondary),
  colnames(atl_tertiary)
))

# Rebind the datasets using only the common columns to avoid duplicate names.
atl_roads <- bind_rows(
  select(atl_primary, all_of(common_cols)),
  select(atl_motorway, all_of(common_cols)),
  select(atl_trunk, all_of(common_cols)),
  select(atl_secondary, all_of(common_cols)),
  select(atl_tertiary, all_of(common_cols))
)

colnames(atl_roads)  # Confirm the unified column structure

# Re-visualise the cleaned road network, with features coloured by highway type.
atl_roads %>% 
  mapview(zcol = "highway")

#------------------------------------------------------------
# Define a Localised Study Area: 1-Mile Buffer Around "Five Points, GA"
#------------------------------------------------------------
# Geocode the "Five Points, GA" location using the free Nominatim service.
five_points_coords <- tibble(address = "Five Points, GA") %>%
  tidygeocoder::geocode(address, method = "osm") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)  # Convert the result into an sf point

# Transform the coordinate system to US survey feet, create a 1-mile (5280 feet) buffer,
# and reproject back to WGS84 for consistency with the OSM data.
five_points_1mi_rad <- five_points_coords %>%
  st_transform("+proj=tmerc +lat_0=30 +lon_0=-84.16666666666667 +k=0.9999 
               +x_0=699999.9999999999 +y_0=0 +datum=NAD83 +units=us-ft 
               +no_defs +ellps=GRS80 +towgs84=0,0,0") %>% 
  st_buffer(5280) %>%  # Create the 1-mile buffer
  st_transform(4326)   # Reproject back to WGS84

print(five_points_1mi_rad)  # Display the geometry of the buffered area

#------------------------------------------------------------
# Projection String Explanation:
#
# The projection string provided describes a Transverse Mercator projection with parameters 
# suitable for mapping in the United States.
#
# Standard Parameters:
#   +proj=tmerc        -> Specifies the Transverse Mercator projection.
#   +lat_0=30          -> Sets the latitude of origin (30°N).
#   +lon_0=-84.166667  -> Sets the central meridian (84.1667°W).
#   +k=0.9999          -> Scale factor applied to the projection.
#   +x_0=699999.9999999999 -> False Easting to avoid negative coordinates.
#   +y_0=0             -> False Northing.
#   +no_defs           -> Prevents external definitions from overriding these settings.
#
# USA-Specific Parameters:
#   +datum=NAD83       -> Uses the North American Datum 1983.
#   +ellps=GRS80       -> Specifies the GRS80 ellipsoid.
#   +towgs84=0,0,0     -> Indicates no transformation is needed from NAD83 to WGS84.
#   +units=us-ft       -> Uses US survey feet as the unit of measurement.
#------------------------------------------------------------

colnames(atl_roads)  

# Define a colour palette for visualising different highway types.
set1_5 = RColorBrewer::brewer.pal(n=5, name  = "Set1") 

# Clip the Atlanta roads to the 1-mile buffer around Five Points, GA, select key attributes,
# and visualise the intersecting roads coloured by highway type.
atl_roads %>%
  st_intersection(five_points_1mi_rad) %>%
  dplyr::select(osm_id, name, bicycle, highway) %>%    
  mapview(
    zcol = "highway",
    color = set1_5,
    map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "OpenStreetMap")
  )

#------------------------------------------------------------
# Bicycle Infrastructure Dataset Extraction for Ponce & Monroe, GA
#------------------------------------------------------------

#--1. Define the bounding box for the Ponce & Monroe area.
ponce_monroe_bbox <- osmdata::opq(bbox = c(-84.38882296093993, 33.786737, -84.352018, 33.759368705358256))

#--2. Extract motorised roadway features (primary, secondary, tertiary, residential) within the bounding box.
pm_primary = ponce_monroe_bbox %>% 
  add_osm_feature(key = "highway", value = "primary") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%  
  st_as_sf() 

pm_secondary = ponce_monroe_bbox %>% 
  add_osm_feature(key = "highway", value = "secondary") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%  
  st_as_sf()  

pm_tertiary = ponce_monroe_bbox %>% 
  add_osm_feature(key = "highway", value = "tertiary") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%  
  st_as_sf()  

pm_residential = ponce_monroe_bbox %>% 
  add_osm_feature(key = "highway", value = "residential") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%  
  st_as_sf()  

# Combine the extracted road types into a single dataset.
pm_roadways <- pm_primary %>% 
  bind_rows(
    pm_secondary,
    pm_tertiary,
    pm_residential
  ) %>% 
  # Select relevant attributes including highway type and various surface or cycle-related tags.
  dplyr::select(
    osm_id, name, contains("HFCS"), 
    starts_with("highway"), # Captures possible alternative highway tags (e.g., highway_1)
    contains("surface"), contains("sidewalk"),
    contains("foot"),
    contains("cycle"), contains("bicycle"), 
    contains("segregated")
  ) %>% 
  # Add an indicator to denote these as motorised routes.
  mutate(
    motor_or_nah = "motor"
  )

# Visualise the motorised roadways interactively.
pm_roadways %>%
  mapview(
    zcol = "highway",
    color = set1_5,
    map.types = c("CartoDB.DarkMatter", "CartoDB.Positron", "OpenStreetMap")
  )

# Create a static ggplot visualisation of roadway classification.
color_palette <- scale_color_manual(values = RColorBrewer::brewer.pal(5, "Set1"))

roadways_plot <- ggplot(data = pm_roadways) +
  geom_sf(aes(color = highway), size = 0.7, alpha = 0.8) +  
  color_palette +
  theme_minimal() +
  labs(
    title = "Roadway Classification",
    subtitle = "Categorised by Highway Type",
    color = "Highway Type"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(roadways_plot)  

ggsave("roadways_plot.png", plot = roadways_plot, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------
# Process On-Street Bicycle Infrastructure from Motorised Roads
#------------------------------------------------------------

# Examine the distribution of cycle-related tags in the motorised dataset.
table(pm_roadways$cycleway)
table(pm_roadways$`cycleway:left`)
table(pm_roadways$`cycleway:right`)
table(pm_roadways$`cycleway:both`)
table(pm_roadways$highway_1)
table(pm_roadways$bicycle)

# Create a new variable 'bike_infra' to categorise on-street bicycle infrastructure based on OSM attributes.
pm_roadways_wrangle = pm_roadways %>%  
  mutate(
    bike_infra = case_when(
      highway_1 == "cycleway" ~ "protected lane",  # Feature identified as a dedicated cycleway
      
      cycleway == "lane" ~ "lane of unknown type",   # Bike lane present but type unspecified
      `cycleway:right` == "lane" ~ "lane of unknown type",
      `cycleway:left` == "lane" ~ "lane of unknown type",
      
      cycleway == "shared_lane" ~ "sharrow",         # Indication of a shared lane (sharrow) for bikes and vehicles
      `cycleway:right` == "shared_lane" ~ "sharrow",
      `cycleway:left` == "shared_lane" ~ "sharrow" 
    )
  )

# Visualise on-street bike infrastructure interactively (features with a defined classification).
dark2_3 <- RColorBrewer::brewer.pal(n=3, name  = "Dark2") 

pm_roadways_wrangle %>% 
  filter(is.na(bike_infra) == FALSE) %>%  # Keep only features with a bike infrastructure classification
  mapview(zcol = "bike_infra", color = dark2_3)

# Create a static ggplot visualisation for on-street bicycle infrastructure.
pm_roadways_filtered <- pm_roadways_wrangle %>% 
  filter(!is.na(bike_infra))

bike_infra_plot <- ggplot(data = pm_roadways_filtered) +
  geom_sf(aes(color = bike_infra), size = 0.7, alpha = 0.8) +  # Plot with colours representing bike infrastructure type
  scale_color_manual(values = dark2_3) +  
  theme_minimal() +
  labs(
    title = "On-Street Bicycle Infrastructure",
    subtitle = "Categorised by Type",
    color = "Bicycle Infrastructure"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(bike_infra_plot)  

ggsave("bike_infra_plot.png", plot = bike_infra_plot, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------
# Extract Non-Motorised Pathways (e.g., paths, cycleways, footways) for Ponce & Monroe
#------------------------------------------------------------
pm_path = ponce_monroe_bbox %>% 
  add_osm_feature(key = "highway", value = "path") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%  
  st_as_sf()  

pm_cycleway = ponce_monroe_bbox %>% 
  add_osm_feature(key = "highway", value = "cycleway") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%  
  st_as_sf()  

pm_footway = ponce_monroe_bbox %>% 
  add_osm_feature(key = "highway", value = "footway") %>%
  osmdata_sf() %>%  
  .$osm_lines %>%  
  st_as_sf()  

pm_non_motor = pm_path %>% 
  bind_rows(
    pm_cycleway, pm_footway
  ) %>% 
  dplyr::select(
    osm_id, name, contains("HFCS"),
    starts_with("highway"), 
    contains("surface"), contains("sidewalk"),
    contains("cycle"), contains("bicycle"),
    contains("segregated")
  ) %>% 
  mutate(
    motor_or_nah = "nah"  # Mark these features as non-motorised routes
  )

# Visualise non-motorised pathways interactively, using a Dark2 colour palette.
dark2_3 = RColorBrewer::brewer.pal(n=3, name  = "Dark2") 

pm_non_motor %>% 
  mapview(
    zcol = "highway",
    color = dark2_3,
    map.types = c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap")
  )

# Create a static ggplot visualisation of non-motorised infrastructure.
non_motor_plot <- ggplot(data = pm_non_motor) +
  geom_sf(aes(color = highway), size = 0.7, alpha = 0.8) +  
  scale_color_manual(values = dark2_3) +  
  theme_minimal() +
  labs(
    title = "Non-Motorised Infrastructure Around Ponce & Monroe",
    subtitle = "Pathways, Cycleways, and Footways",
    color = "Highway Type"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(non_motor_plot)  

ggsave("non_motor_plot.png", plot = non_motor_plot, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------
# Consolidate On-Street and Off-Street Bicycle Infrastructure
#
# This section integrates bike infrastructure that is part of the road network (on-street)
# with dedicated or separate paths (off-street). These features may have distinct attributes,
# such as a dedicated cycle track or a paved trail.
#------------------------------------------------------------
pm_non_motor_wrangle = pm_non_motor %>% 
  mutate(
    bike_infra =
      case_when(
        osm_id == 741964056 ~ "protected lane",  # Specific feature classified as a protected lane
        name == "Portman PATH" ~ "protected lane", # Local knowledge identifies this as a protected lane
        name == "Peachtree Center Cycle Track" ~ "protected lane",  # Classified as a protected cycle track
        name == "Stone Mountain Trail" ~ "protected lane",  # Identified locally as a protected lane
        
        # Otherwise, classify based on highway and surface attributes.
        highway == "cycleway" ~ "paved trail",
        highway == "path" & surface == "paved" ~ "paved trail"
      )
  )

# Visualise the classified non-motorised bike infrastructure with thicker lines.
pm_non_motor_wrangle %>% 
  filter(is.na(bike_infra) == FALSE) %>% 
  mapview(
    lwd = 4,
    zcol = "bike_infra", color = c("purple", "green")
  )

bike_colors <- c("protected lane" = "purple", "paved trail" = "green")

# Create a static ggplot visualisation for the non-motorised (off-road) infrastructure.
pm_non_motor_filtered <- pm_non_motor_wrangle %>% 
  filter(!is.na(bike_infra))

non_motor_wrangle_plot <- ggplot(data = pm_non_motor_filtered) +
  geom_sf(aes(color = bike_infra), size = 1.2, alpha = 0.8) +  # Plot with classification-based colouring
  scale_color_manual(values = bike_colors) +  # Apply the custom colour mapping
  theme_minimal() +
  labs(
    title = "Non-Motorised Infrastructure (On-Road & Off-Road)",
    subtitle = "Categorised by Type",
    color = "Bike Infrastructure"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(non_motor_wrangle_plot)

ggsave("non_motor_wrangle_plot.png", plot = non_motor_wrangle_plot, width = 10, height = 6, dpi = 300)

# Combine the on-street and off-street bike infrastructure datasets for an integrated view.
dark2_5 <- RColorBrewer::brewer.pal(n=4, name  = "Dark2") 

pm_non_motor_wrangle %>%  # Non-motorised (off-road) features
  bind_rows(pm_roadways_wrangle) %>%  # On-street features from motorised roads
  filter(is.na(bike_infra) == FALSE) %>% 
  mapview(
    zcol = "bike_infra",
    layer.name = "Bike infrastructure",  # Label the combined layer
    color = dark2_5,
    lwd = 4,
    map.types = c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap")
  )

# Prepare a combined dataset for static visualisation.
dark2_5 <- brewer.pal(n = 4, name = "Dark2")

pm_combined <- pm_non_motor_wrangle %>% 
  bind_rows(pm_roadways_wrangle) %>% 
  filter(!is.na(bike_infra))  # Retain only features with a defined bike infrastructure classification

# Create a combined ggplot visualisation of both on-street and off-street bike infrastructure.
combined_bike_plot <- ggplot(data = pm_combined) +
  geom_sf(aes(color = bike_infra), size = 1.2, alpha = 0.8) +  # Plot combined features with consistent line width
  scale_color_manual(values = dark2_5) +  # Apply the Dark2 colour palette
  theme_minimal() +
  labs(
    title = "Combined Bike Infrastructure (On-Street & Off-Street)",
    subtitle = "Categorised by Type",
    color = "Bike Infrastructure"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

print(combined_bike_plot)  

ggsave("combined_bike_plot.png", plot = combined_bike_plot, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------
# Arrange All Plots in a Grid Layout for Comparison
#------------------------------------------------------------
gridExtra::grid.arrange(
  roadways_plot,             # Motorised roadway classification
  bike_infra_plot,           # On-street bicycle infrastructure
  non_motor_plot,            # Non-motorised pathway visualisation
  non_motor_wrangle_plot,    # Classified non-motorised (off-road) infrastructure
  combined_bike_plot,        # Combined bike infrastructure (on-street & off-street)
  ncol = 3  # Arrange plots in 3 columns
)

#------------------------------------------------------------
# Calculate Total Length of Bike Infrastructure
#------------------------------------------------------------
# Combine both on-street and off-street bike infrastructure datasets,
# excluding features without a bike infrastructure classification.
pm_bike_lanes <- pm_non_motor_wrangle %>%
  bind_rows(pm_roadways_wrangle) %>%
  filter(!is.na(bike_infra))

# Check the current coordinate system of the combined dataset.
st_crs(pm_bike_lanes)

# Transform to a projected CRS (UTM Zone 16N) that is appropriate for distance calculations.
pm_bike_lanes <- st_transform(pm_bike_lanes, crs = 32616)

# Calculate the length (in metres) of each bike infrastructure segment.
pm_bike_lanes <- pm_bike_lanes %>%
  mutate(length_meters = st_length(geometry))

# Summarise the total length of bike infrastructure by type.
bike_lane_summary <- pm_bike_lanes %>%
  group_by(bike_infra) %>%
  summarise(total_length_meters = sum(length_meters)) %>%
  arrange(desc(total_length_meters))

# Convert the lengths from metres to miles.
bike_lane_summary <- bike_lane_summary %>%
  mutate(total_length_miles = total_length_meters / 1609.34)

print(bike_lane_summary)

# Display the summary in a formatted table.
knitr::kable(bike_lane_summary, caption = "Total Length of Bike Lane Types")


#------------------------------------------------------------
# Reference
#------------------------------------------------------------
# Michael D. Garber's blog post on using OSM data for bicycle infrastructure:
# https://rpubs.com/michaeldgarber/osm_data_for_bikes_2
# Using OpenStreetMap to inventory bicycle infrastructure: A comparison with open data from cities
# https://www.tandfonline.com/doi/full/10.1080/15568318.2018.1519746 
# 

