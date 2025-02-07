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

#------------------------------------------------------------
# Point 
#------------------------------------------------------------

## Load required packages
library(osmdata)  # For OpenStreetMap data
library(dplyr)    # For data manipulation
library(sf)       # For spatial operations
library(tmap)     # For visualization
library(units)    # For handling units like km

## Build an OSM query to get libraries in Meanjin, Australia
query <- opq(bbox = "Glasgow, UK") %>% 
  add_osm_feature(key = "amenity", value = "library")

# Retrieve library data as an sf object
libraries <- osmdata_sf(query)

# Explore the retrieved data
nrow(libraries$osm_points)   # Number of point features
nrow(libraries$osm_polygons) # Number of polygon features
names(libraries$osm_points)  # List available attributes
View(libraries$osm_points)   # Open a table view in RStudio

# Convert polygon data to points

point_libs <- libraries$osm_points %>%  # Extract point-based library locations
  filter(amenity == "library")

centroid_libs <- libraries$osm_polygons %>%  # Convert polygon-based libraries to centroids
  st_centroid() 

all_libs <- bind_rows(centroid_libs, point_libs) # Combine both datasets into a single dataset

# Investigate missing names in the dataset
all_libs %>% 
  filter(is.na(name)) %>%  # Find features without a name
  pull(osm_id)             # Extract their OSM ID

# Reproject data to local CRS
st_crs(all_libs)           # Check current CRS
st_crs(all_libs) <- 4326   # Set CRS to WGS 84 (lat/lon)
all_libs <- st_transform(all_libs, crs = 27700)  # Convert to local CRS

## Basic visualization
plot(all_libs)             # Quick plot of all libraries
st_geometry(all_libs) %>%   # Plot geometries only
  plot()

tmap_mode("view")          # Switch to interactive map mode

tm_shape(all_libs) +       # Display libraries on the map
  tm_sf()

library(sf)
library(dplyr)  # For filtering
library(tmap)

# Load the shapefile (adjust path accordingly)
scotland_boundaries <- st_read("/Users/linhlinh/Documents/github/openstreetmap/Local_Authority_Boundaries_-_Scotland/pub_las.shp")

# Check column names to identify the relevant one for filtering
print(names(scotland_boundaries))

# Assuming "LA_NAME" contains local authority names, filter for Glasgow
glasgow_boundary <- scotland_boundaries %>%
  filter(local_auth == "Glasgow City")  # Adjust if the actual name differs

# Check the result
print(glasgow_boundary)

# Plot to verify
plot(st_geometry(glasgow_boundary), col = "blue", main = "Glasgow Boundary")

tm_shape(glasgow_boundary) + tm_borders() + tm_shape(all_libs) + tm_dots()

# Clip libraries to the Meanjin region
clipped_libs <- st_intersection(all_libs, st_transform(glasgow_boundary, 27700)) # 7856

# Visualize clipped data
tm_shape(glasgow_boundary) +
  tm_borders() +
  tm_shape(clipped_libs) +
  tm_dots()

## Visualise data with buffer area 
# Create a 2-km buffer around libraries
buffer <- clipped_libs %>% 
  st_buffer(dist = set_units(1, km)) %>% 
  summarise()

# Plot buffer zone and libraries
plot(buffer)
plot(clipped_libs, add = TRUE)

# Visualize buffer on the map
tm_shape(glasgow_boundary) +
  tm_borders() +
  tm_shape(buffer) +
  tm_polygons(col = "pink", alpha = 0.3) +
  tm_shape(clipped_libs) +
  tm_dots()

## Handle missing values in the 'toilets' column
summary(clipped_libs$toilets)  # Check for NA values

# Convert 'toilets' to factor and add 'Unknown' level
clipped_libs$toilets <- factor(clipped_libs$toilets, 
                               levels = c(levels(clipped_libs$toilets), "Unknown"))
clipped_libs$toilets[is.na(clipped_libs$toilets)] <- "Unknown"

summary(clipped_libs$toilets)  # Verify changes

## Create interactive map
lib_map <- tm_basemap(server = "CartoDB.PositronNoLabels") +
  tm_shape(meanjin) +
  tm_borders() +
  tm_shape(buffer) +
  tm_polygons(col = "pink", alpha = 0.3) +
  tm_shape(clipped_libs) +
  tm_dots(size = 0.5,                        # Adjust dot size
          col = "toilets",                   # Colour by toilet availability
          id = "name",                       # Tooltip shows name
          popup.vars = c("internet_access", "wheelchair", "operator"),
          palette = "Set1") +                 # Define color scheme
  tm_scale_bar() +                           # Add scale bar
  tm_layout(title = "Libraries in Meanjin", 
            legend.outside = TRUE)           # Move legend outside

lib_map  # Display the map

# Explore available basemap providers
providers %>% names() %>% head(20)  # Show the first few options

## Save a static visualization
tmap_mode("plot")   # Switch to static mode
lib_map             # Render static plot

tmap_save(lib_map, "meanjin_libs.html")  # Save as an interactive HTML file
