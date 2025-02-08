#------------------------------------------------------------
# To check what admin levels exist in a specific area and their values
#------------------------------------------------------------

library(osmdata)

# Define the bounding box 
iz_bbox <- getbb("Glasgow UK")

# Query all administrative boundaries (without filtering)
iz_data <- opq(iz_bbox) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

# Check unique admin levels in the osm_multipolygons layer
unique(iz_data$osm_multipolygons$admin_level)
unique(iz_data$osm_polygons$admin_level)

# view data
colnames(iz_data$osm_multipolygons)
head(iz_data$osm_multipolygons)

# Filter level 8 
admin_6 <- iz_data$osm_multipolygons %>%
  dplyr::filter(admin_level == "6")

print(admin_6)

# viz 
library(ggplot2)

p <- ggplot() +
  geom_sf(data = admin_6, fill = "blue", alpha = 0.5) +
  ggtitle("Admin Level 6 Boundaries")
p

ggsave(filename = "plot/glasgow_admin_boundary.pdf", plot = p, bg = "white")

