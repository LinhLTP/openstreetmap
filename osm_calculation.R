#------------------------------------------------------------
# Load Required Libraries
#------------------------------------------------------------
library(dplyr)        # Data manipulation
library(tidyr)        # Data tidying
library(ggplot2)      # Data visualisation
library(sf)           # Handling geospatial geometries
library(osmdata)      # Extracting OpenStreetMap (OSM) data
library(units)        # Managing measurement units
library(mapview)      # Interactive map viewing
library(ggmap)        # Fetching raster maps from online sources
library(ggspatial)    # Adding spatial annotations to ggplot maps
library(tmap)         # Static and interactive mapping with ggplot-like syntax

#------------------------------------------------------------
# OpenStreetMap Data Extraction
#------------------------------------------------------------

## Obtain Polygon and Bounding Box for Ho Chi Minh City (HCMC)
# Retrieve the polygon boundary for the HCMC 
city_polygon <- getbb("HCMC Vietnam", featuretype = "settlement", format_out = "polygon")

# Get the rectangular bounding box for Edinburgh
city_rect <- getbb("HCMC Vietnam", featuretype = "settlement")

## Retrieve Green Spaces Data from OSM
greensp_osm <- opq(bbox = city_polygon) %>%                      # Initiate an OSM query with the bounding box
  add_osm_feature(key = "leisure",                               # Filter for specific leisure-related features
                  value = c("park", "nature_reserve", "golf_course")) %>%
  osmdata_sf() %>%                                               # Convert to Simple Features (sf)
  trim_osmdata(city_polygon)                                     # Clip data to match the Edinburgh polygon boundary

# Examine the structure of the retrieved data
glimpse(greensp_osm$osm_polygons)
glimpse(greensp_osm$osm_multipolygons)

#------------------------------------------------------------
# Data Processing and Cleaning
#------------------------------------------------------------

## Convert POLYGON to MULTIPOLYGON and Merge
# Ensure consistency by converting all polygons to multipolygons
greensp_sf <- bind_rows(
  st_cast(greensp_osm$osm_polygons, "MULTIPOLYGON"),             # Convert POLYGON to MULTIPOLYGON
  greensp_osm$osm_multipolygons                                  # Merge with existing MULTIPOLYGONs
) %>%
  select(name, osm_id, leisure)                                  # Keep relevant attributes

# Save intermediate data (optional)
# saveRDS(greensp_sf, "greensp_sf.rds")

# Load previously saved data if required
# greensp_sf <- readRDS("greensp_sf.rds")

# Preview data
head(greensp_sf)
unique(greensp_sf$leisure)

## Clean the Data
greensp_sf <- greensp_sf %>%
  filter(!is.na(leisure)) %>%                                    # Remove entries with missing leisure types
  rename(greensp_type = leisure) %>%                             # Rename 'leisure' column for clarity
  st_make_valid()                                                # Ensure all geometries are valid

#------------------------------------------------------------
# Data Visualisation
#------------------------------------------------------------
plot(greensp_sf["greensp_type"])                                 # Basic plot of green spaces by type

#------------------------------------------------------------
# Saving and Loading Spatial Data
#------------------------------------------------------------

## Save as a GeoPackage (.gpkg)
st_write(
  greensp_sf,
  dsn = "greenspaces_Edi_OSM.gpkg",                              # File path
  layer = "greenspaces",                                         # Layer name
  layer_options = c(paste0("DESCRIPTION=Contains spatial multipolygons for parks, ",
                           "nature reserves, and golf courses in Edinburgh, Scotland. ",
                           "Copyright OpenStreetMap contributors. ODbL ",
                           "https://www.openstreetmap.org/copyright")), 
  delete_dsn = TRUE                                              # Overwrite the file if it exists
)

## Load GeoPackage
greensp_sf <- st_read(dsn = "greenspaces_Edi_OSM.gpkg", layer = "greenspaces")

#------------------------------------------------------------
# Coordinate Reference System (CRS)
#------------------------------------------------------------

## Check the current CRS
st_crs(greensp_sf)

## Transform to British National Grid (EPSG: 27700)
greensp_sf <- st_transform(greensp_sf, 27700)

## Confirm CRS transformation
st_crs(greensp_sf)

#------------------------------------------------------------
# Spatial Analysis
#------------------------------------------------------------

## Calculate Area for Each Green Space
greensp_sf <- mutate(greensp_sf, area = st_area(greensp_sf))
head(greensp_sf$area)

### Convert m² to hectares
greensp_sf <- greensp_sf %>% 
  mutate(area_ha = set_units(area, "ha")) %>% 
  select(-area) # Remove 'area' column 

### Display spatial data interactively
mapview(greensp_sf)

## Additional Data Cleaning
# Remove unnecessary shapes and invalid geometries
greensp_sf <- greensp_sf %>%
  filter(!is.na(greensp_type)) %>%  
  rename(greensp_type = greensp_type) %>%  
  st_make_valid()  # Ensure valid geometries

# Remove small green spaces (<2 ha)
greensp_sf <- filter(greensp_sf, as.numeric(area_ha) >= 2)

### View updated dataset
mapview(greensp_sf)

## Separate Green Spaces by Type
greensp_sf_list <- greensp_sf %>% split(.$greensp_type) # Create a list of green spaces by type

# Access specific types using the $ operator
# Example: greensp_sf_list$nature_reserve contains only nature reserves

## Remove Overlapping Areas
### Remove parts of parks overlapping with nature reserves
greensp_sf_list$park <- st_difference(greensp_sf_list$park,
                                      st_union(greensp_sf_list$nature_reserve))

### Remove parts of parks overlapping with golf courses
greensp_sf_list$park <- st_difference(greensp_sf_list$park,
                                      st_union(greensp_sf_list$golf_course))

### Recalculate Area
greensp_sf <- bind_rows(greensp_sf_list) %>%
  mutate(area_ha = set_units(st_area(.), "ha")) %>%
  filter(as.numeric(area_ha) >= 2) # Remove small areas again

#------------------------------------------------------------
# Mapping and Visualisation
#------------------------------------------------------------

## Prepare Data for Mapping
greensp_sf_forplot <- greensp_sf %>%
  mutate(greensp_type = factor(greensp_type,
                               levels = c("park", "nature_reserve", "golf_course"),
                               labels = c("Park", "Nature Reserve", "Golf Course")))

## Create Interactive Map with tmap
tmap_mode("view") # Enable interactive mode

hcmc_greenspace_tmap <-
  tm_basemap("Esri.WorldStreetMap") +  # Use an Esri basemap
  tm_shape(greensp_sf_forplot) +       # Add green space layer
  tm_sf(col = "greensp_type",          # Colour by green space type
        title = "",                     # Hide legend title
        palette = c("#44AA99", "#117733", "#AA4499"), # Custom colours
        popup.vars = c("Area  " = "area_ha"), # Show area in popups
        popup.format = list(digits=1)) +  # Round area to 1 decimal place
  tm_scalebar() # Add a scale bar

# Save the interactive map
tmap_save(tm = hcmc_greenspace_tmap, filename = "/Users/linhlinh/Documents/github/osm/hcmc_greenspace_tmap.html")

#------------------------------------------------------------
# Green Space Area Analysis
#------------------------------------------------------------

## Calculate the total area for each green space type, accounting for overlaps
greensp_type_area <-
  lapply(greensp_sf_list,
         function(x) set_units(st_area(st_union(x)), "ha")) %>% # Calculate total area
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "greensp_type",
               values_to = "area_ha")
