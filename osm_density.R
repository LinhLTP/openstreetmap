#------------------------------------------------------------
# Load Libraries
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
library(viridis)      # for colour scales

#------------------------------------------------------------
# Retrieve boundary lines
#------------------------------------------------------------
# retrieve bounding box for region of interest 
iz_bbox <- getbb("konak", format_out = "polygon") # matrix 
print(iz_bbox)

# retrieve level 8 administrative boundaries 
iz_boundary <- opq(iz_bbox) %>% 
  add_osm_feature(key = "admin_level", value = "8") %>% 
  osmdata_sf() 

# select only df multipolygons
iz_polys <- iz_boundary$osm_multipolygons

# clean district names
clean_name <- function(name) {
  # Remove digits
  name <- gsub('[[:digit:]]+', '', name)
  # Remove periods
  name <- gsub('[.]', '', name)
  # Trim whitespace from both ends
  name <- trimws(name, which = "both")
  return(name)
}

# apply the cleaning function and convert the names to a factor
iz_polys$name <- as.factor(clean_name(iz_polys$name))

# calculate polygon areas for later analysis and append to new column
iz_polys$poly_area <- st_area(iz_polys)

# remove original osmdata object
rm(iz_boundary)

ggplot(iz_polys) +
  geom_sf()

#------------------------------------------------------------
# Retrieve building 
#------------------------------------------------------------
iz_buildings <- opq(iz_bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

# get rid of the excess data, only keep the polygon sf object
build_polys <- iz_buildings$osm_polygons
rm(iz_buildings)

# drop unecessary columns
build_polys <- build_polys %>%
  select(osm_id, geometry)

# plot the result
ggplot(iz_polys) + 
  geom_sf() + 
  geom_sf(data = build_polys)

#------------------------------------------------------------
# Clipping point data
#------------------------------------------------------------
## The buildings returned don’t really follow the borders of any polygon, so need do some spatial trimming.
build_polys$area <- sf::st_area(build_polys) # calculate surface area of buildings
build_cents <- sf::st_centroid(build_polys) #calculate centroids

#------------------------------------------------------------
# Note: 
# We need to create a polygon geometry to filter out the outlying buildings. 
# This can be achieved by using the original bounding box `iz_bbox`. 
# However, since this object is a matrix, it must be converted into an `sf` object—a task that can be performed using `sf_headers::sf_polygon`
# Please note that the initial solution (see Appendix 01) presents issues (feel free to run the code to verify), 
# and the below recommended fix involves correcting self-intersections and duplicate edges.
#------------------------------------------------------------
# Retrieve bounding polygon as a list of matrices
iz_bbox <- getbb("konak", format_out = "polygon")

# Flatten list into a single dataframe
iz_bbox_df <- do.call(rbind, iz_bbox)  
iz_bbox_df <- as.data.frame(iz_bbox_df)  

# Ensure correct column names
colnames(iz_bbox_df) <- c("x", "y")

# Add a group column (needed for sfheaders)
iz_bbox_df$polygon_id <- 1  

# Convert to sf polygon
iz_bbox_geom <- sfheaders::sf_polygon(iz_bbox_df, x = "x", y = "y", polygon_id = "polygon_id")

# Validate geometry (Fix self-intersections, duplicate edges) 
iz_bbox_geom <- st_make_valid(iz_bbox_geom)

# Convert to sf object
iz_bbox_sf <- st_sf(geometry = iz_bbox_geom)

# Convert MULTIPOLYGON to POLYGON if necessary
if (st_geometry_type(iz_bbox_sf) == "MULTIPOLYGON") {
  iz_bbox_sf <- st_cast(iz_bbox_sf, "POLYGON")
}

# Simplify to reduce small topological errors
iz_bbox_sf <- st_simplify(iz_bbox_sf, dTolerance = 0.0001)

st_crs(iz_bbox_sf) <- 4326  

# Ensure CRS matches build_cents
if (st_crs(build_cents) != st_crs(iz_bbox_sf)) {
  iz_bbox_sf <- st_transform(iz_bbox_sf, st_crs(build_cents))
}

ggplot(iz_polys) +
  geom_sf() +
  geom_sf(data=iz_bbox_sf, col="red")

# Clip the data 
clipped <- st_join(build_cents, iz_bbox_sf, join = st_within, left = FALSE)

# Plot results
ggplot(clipped) + 
  geom_sf() + 
  geom_sf(data = iz_bbox_sf, color = 'red', fill = NA)

#------------------------------------------------------------
# Density Calculation 
#------------------------------------------------------------
# join sf objects
joined <- st_join(clipped, iz_polys)

# aggregating and summing total building area
density_calc <- aggregate(joined$area, 
                          list(joined$osm_id.y),
                          FUN = sum)
# rename columns
colnames(density_calc) <- c("osm_id", "area")

# create final df that contains district polygons and building area
bounds_blds_sf <- merge(iz_polys, density_calc) 

# calculate building density
bounds_blds_sf <- bounds_blds_sf %>%
  mutate(b_dens = as.numeric(area/poly_area * 100))

#------------------------------------------------------------
# ggplot viz 
#------------------------------------------------------------
plot <- ggplot() +
    geom_sf(data = bounds_blds_sf, aes(fill = b_dens), colour = "grey30", alpha = 0.8) +
    scale_fill_viridis(name = "Building Density\n(% of Land Area)", option = "C") +
    labs(title = "Building Density in Konak",
         subtitle = "Buildings as percentage of land area",
         caption = "Data source: OSM") +
    theme_bw() +
    # theme_minimal() +  
    # Add a scale bar and a north arrow for context
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering())

print(plot) 

ggsave(filename = "plot/density.pdf", plot = plot, bg = "white")

#------------------------------------------------------------
# tmap viz 
#------------------------------------------------------------
# Coordinate Reference System (CRS):
# When tmap is in interactive mode (using tmap_mode("view")), 
# it leverages leaflet, which expects data in Web Mercator (EPSG:3857). 
# In our code, the spatial objects (e.g. bounds_blds_sf) are in EPSG:4326. 
# Although tmap can sometimes reproject on the fly, it’s safer to explicitly transform your data. 

bounds_blds_sf <- st_transform(bounds_blds_sf, 3857)

# set interactive mode
tmap_mode('view')
  
# plot with basemap
tm_shape(bounds_blds_sf) +
  tm_polygons(col="b_dens",
                id="name",
                title= "Building Density as % of Land Area",
                alpha=.8) 

tm_shape(bounds_blds_sf) +
  tm_polygons(col = "b_dens",
                id = "name",
                title = "Building Density as % of Land Area",
                alpha = 0.8,
                textNA = NA,       # Remove text for NA values
                colorNA = NA)      # Do not display a colour for NA values

#------------------------------------------------------------
# Appendix 01 
#------------------------------------------------------------
# iz_bbox does not have format V1, V2 
# code 205 - 209 is replaced by 86-113
iz_bbox_geom <- 
  sfheaders::sf_polygon(as.data.frame(iz_bbox),
                        x="V1",
                        y="V2"
  )

st_crs(iz_bbox_geom) <- 4326

ggplot(iz_polys) +
  geom_sf() +
  geom_sf(data=iz_bbox_geom, col="red")

clipped <- st_join(build_cents, iz_bbox_geom, join = st_within)

clipped %>% 
  filter(id == 1) %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf(data=iz_bbox_geom, color = 'red', fill = NA)

clipped <- clipped %>% filter(id == 1) 

joined <- st_join(clipped, iz_polys)

#------------------------------------------------------------
# Ref: Calculating Building Density in R with OSM data by Gregg Saldutti
#------------------------------------------------------------