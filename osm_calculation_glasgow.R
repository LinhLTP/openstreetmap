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
# Data Cleaning 
#------------------------------------------------------------
# Build an OSM query to get libraries in Glasgow, Scotland UK
query <- opq(bbox = "Glasgow, UK") %>% 
  add_osm_feature(key = "amenity", value = "library")

# Retrieve library data as an sf object
libraries <- osmdata_sf(query)

# Explore the retrieved data
nrow(libraries$osm_points)   # Number of point features
nrow(libraries$osm_polygons) # Number of polygon features
names(libraries$osm_points)  # List available attributes
View(libraries$osm_points)   # Open a table view in RStudio

# Convert polygon data to points to make data consistency 
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

#------------------------------------------------------------
# Visualization
#------------------------------------------------------------
## Basic visualization
plot(all_libs)             # Quick plot of all libraries
st_geometry(all_libs) %>%   # Plot geometries only
  plot()

tmap_mode("view")          # Switch to interactive map mode

tm_shape(all_libs) +       # Display libraries on the map
  tm_sf()

#------------------------------------------------------------
# Other Visualization
#------------------------------------------------------------

#------------------------------------------------------------
# Note: 
# It is important to display the study area by showing the bounding polygon used to obtain the data. Typically, we can use the following code to extract the bounding polygon:

# Get a bounding box (we need to extract the multipolygon)
## Location <- getbb("location name", format_out = "sf_polygon")$multipolygon

# Map
## tm_shape(Location) + tm_borders() + tm_shape(all_libs) + tm_dots()  # For point geometries only

# However, in some cities, such as Glasgow, OSM cannot query the multipolygon.
# In such cases, an alternative is to use administrative boundary (.shp) files published by the government.
#------------------------------------------------------------

# Load the shapefile 
scotland_boundaries <- st_read("/Users/linhlinh/Documents/github/openstreetmap/Local_Authority_Boundaries_-_Scotland/pub_las.shp")

# Check column names to identify the relevant one for filtering
print(names(scotland_boundaries))

# Assuming "local_auth" contains local authority names, filter for Glasgow
glasgow_boundary <- scotland_boundaries %>%
  filter(local_auth == "Glasgow City")  # Adjust if the actual name differs

# Check the result
print(glasgow_boundary)

# Plot to verify
plot(st_geometry(glasgow_boundary), 
     col = "blue", 
     main = "Glasgow Boundary")

tm_shape(glasgow_boundary) + 
  tm_borders() + 
  tm_shape(all_libs) + 
  tm_dots()

#------------------------------------------------------------
# Spatial Subset from rectangular region
#------------------------------------------------------------
# Notice OSM data that was return by Overpass was for the rectangular region that contains the urban area. 
# If we wanted to focus on only the points contained in the area, we could do a spatial subset:
# Clip libraries to the Glasgow region
clipped_libs <- st_intersection(all_libs, 
                                st_transform(glasgow_boundary, 27700)) # 7856 # can use trim_osmdata()

# Visualize clipped data
tm_shape(glasgow_boundary) +
  tm_borders() +
  tm_shape(clipped_libs) +
  tm_dots()

#------------------------------------------------------------
# Mapping with Buffer Area 
#------------------------------------------------------------
# Create a 1-km buffer around libraries
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

# Handle missing values in the 'toilets' column
summary(clipped_libs$toilets)  # Check for NA values

# Convert 'toilets' to factor and add 'Unknown' level
clipped_libs$toilets <- factor(clipped_libs$toilets, 
                               levels = c(levels(clipped_libs$toilets), "Unknown"))

clipped_libs$toilets[is.na(clipped_libs$toilets)] <- "Unknown"

summary(clipped_libs$toilets)  # Verify changes

# Create interactive map
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

# Save a static visualization
tmap_mode("plot")   # Switch to static mode
lib_map             # Render static plot

tmap_save(lib_map, "meanjin_libs.html")  # Save as an interactive HTML file
