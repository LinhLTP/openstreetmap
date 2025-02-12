## OpenStreetMap (OSM) Geospatial Analysis

OpenStreetMap (OSM) is a collaborative mapping project that provides freely available and editable geospatial data. It is maintained by a community of volunteers who contribute geographic information, including roads, buildings, natural features, and amenities. OSM data is widely used in geographic information system (GIS) applications, urban planning, navigation, and environmental analysis due to its open and community-driven nature.

### Scope
This repo contains the focus on extracting, processing, and analysing geospatial data from OSM for various urban environments. The analysis includes green space visualisation, library accessibility, and bicycle infrastructure assessment.

### Spatial Analysis
- Space areas are calculated in hectares, and small green spaces (<2 hectares) are filtered out.
- POI accessibility is assessed by generating a 1 km spatial buffer around POI.
- Bicycle infrastructure is categorised into on-street and off-street infrastructure, and the total lengths of different bike infrastructure types are calculated in meter & miles 

### Mapping and Visualisation
- Interactive maps are generated using `tmap` and `mapview`, allowing users to explore spatial data.
- Static plots are created with `ggplot2` for visualisation of infrastructure classifications.
- Road networks and cycleways are visualised at both broad and local scales.
- A 1-mile buffer is generated around specific location

### Saving and Loading Data
- Processed spatial data is saved in GeoPackage format to preserve geospatial attributes and enable further analysis without repeated OSM queries.

### Coordinate Reference Systems (CRS)
- The analysis includes CRS transformations for spatial accuracy, including:
  - EPSG:27700 (British National Grid) for Glasgow.
  - NAD83 (EPSG:4269) and UTM projections for Atlanta.

### Additional Resources
- [OpenStreetMap GitHub](https://github.com/openstreetmap)
- [OSM Data by rOpenSci](https://github.com/ropensci/osmdata)