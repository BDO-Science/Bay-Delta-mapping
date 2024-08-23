##########################
#mapping redd distributions by year
##########################
library(sf)
library(deltamapr)
library(ggspatial)
library(tidyverse)
library(tidyr)
library(viridis)
library(gganimate)

# Load the WW_Watershed shapefile from deltamapr
data("WW_Watershed")

# Replace the file path with the actual path to your KML file
channel_kml <- st_read("2012_channel.kml")

# Read in the shapefile
# Replace 'path_to_shapefile' with the actual path to your shapefile
shapefile_path <- "california_watershed_shapefile/Data/cdfg_100k_2003_6.shp"
california_watersheds <- st_read(shapefile_path) |>
  st_zm(drop = TRUE, what = "ZM") |>
  unnest(cols = c(NAME))

# Create a vector of creek and river names to filter by
creeks_rivers <- c("Sacramento River", "Shasta Lake", "Clear Creek", "Battle Creek", 
                   "Cottonwood Creek", "Paynes Creek", "Antelope Creek", 
                   "Elder Creek", "Thomes Creek", "Stony Creek", 
                   "Cow Creek", "Bear Creek", "Mill Creek", 
                   "Deer Creek", "Big Chico Creek", "Butte Creek", 
                   "Feather River", "Yuba River", "Bear River", 
                   "American River", "Cosumnes River", 
                   "Mokelumne River", "Calaveras River", 
                   "Stanislaus River", "Tuolumne River", 
                   "Merced River", "San Joaquin River")

# Use str_detect to filter the dataset based on partial string matches
filtered_watersheds <- california_watersheds |>
  filter(str_detect(NAME, paste(creeks_rivers, collapse = "|")))

# Check the structure of the filtered object
str(filtered_watersheds)

# Filter the WW_Watershed dataset to only include the specified creeks and rivers
filtered_watersheds <- california_watersheds |>
  filter(NAME %in% creeks_rivers)

shapefile_path <- "California_Lakes/California_Lakes.shp"
california_lakes <- st_read(shapefile_path) |>
  st_zm(drop = TRUE, what = "ZM")

lakes_reservoirs <- c("Lake Shasta", "Lake Oroville")

filtered_lentic <- california_lakes |>
  filter(sfc_acres > 1000)

shapefile_path <- "Data and Code/Data/GIS/Winter Run Redds 1990-2017 Single Redds.shp"
shapefile_data <- st_read(shapefile_path) 

filtered_shapefile_data <- shapefile_data |>
  filter(year >= 1996 & year <= 2015)

# Check the structure of the shapefile
print(shapefile_data)

# Ensure the datasets have only one geometry for each feature
lake_shasta_geometry <- filtered_lentic %>% filter(NAME == "Lake Shasta")  # Replace 'NAME' with the appropriate column name
sacramento_river_geometry <- filtered_watersheds %>% filter(NAME == "Sacramento River")  # Replace 'NAME' with the appropriate column name
battle_creek_geometry <- filtered_watersheds %>% filter(NAME == "Battle Creek")  # Replace 'NAME' with the appropriate column name

##########################################################################
#making sure all layers have the same coordinate systems and geometry types
##########################################################################
# Check geometry types for each data frame
print(st_geometry_type(filtered_lentic))
print(st_geometry_type(filtered_watersheds))
print(st_geometry_type(lake_shasta_geometry))
print(st_geometry_type(battle_creek_geometry))
print(st_geometry_type(filtered_shapefile_data))

# Convert to spatial features if needed
if (!inherits(filtered_lentic, "sf")) {
  filtered_lentic <- st_as_sf(filtered_lentic)
}
if (!inherits(filtered_watersheds, "sf")) {
  filtered_watersheds <- st_as_sf(filtered_watersheds)
}
if (!inherits(lake_shasta_geometry, "sf")) {
  lake_shasta_geometry <- st_as_sf(lake_shasta_geometry)
}
if (!inherits(battle_creek_geometry, "sf")) {
  battle_creek_geometry <- st_as_sf(battle_creek_geometry)
}
if (!inherits(filtered_shapefile_data, "sf")) {
  filtered_shapefile_data <- st_as_sf(filtered_shapefile_data)
}

crs_value <- st_crs(filtered_shapefile_data)  # Set CRS to match one of your spatial objects

filtered_lentic <- st_transform(filtered_lentic, crs_value)
filtered_watersheds <- st_transform(filtered_watersheds, crs_value)
lake_shasta_geometry <- st_transform(lake_shasta_geometry, crs_value)
battle_creek_geometry <- st_transform(battle_creek_geometry, crs_value)
filtered_shapefile_data <- st_transform(filtered_shapefile_data, crs_value)

# Print the structure of the geometry column for each data frame
str(filtered_lentic$geometry)
str(filtered_watersheds$geometry)
str(lake_shasta_geometry$geometry)
str(battle_creek_geometry$geometry)
str(filtered_shapefile_data$geometry)

# Optionally, make geometries valid if they aren't
filtered_lentic <- st_make_valid(filtered_lentic)
filtered_watersheds <- st_make_valid(filtered_watersheds)
lake_shasta_geometry <- st_make_valid(lake_shasta_geometry)
battle_creek_geometry <- st_make_valid(battle_creek_geometry)
filtered_shapefile_data <- st_make_valid(filtered_shapefile_data)



# Plot the shapefile using ggplot2
ggplot() +
  # Plot reservoirs and lakes with Lake Shasta labeled
  geom_sf(data = filtered_lentic, fill = "lightblue", color = "black") +
  geom_sf_label(data = lake_shasta_geometry, aes(label = "Lake Shasta"), size = 3, color = "black", fontface = "bold", nudge_y = 0, nudge_x = 0.2) +
  
  # Plot the upper Sacramento River watershed with a label
  geom_sf(data = filtered_watersheds, fill = "lightblue", color = "black") +
  # Manually place the Sacramento River label using annotate with a label box
  annotate("label", x = -122.27, y = 40.65, label = "Sacramento River", size = 3, color = "black", fontface = "bold", 
           fill = "white", label.size = 0.5, label.r = unit(0.25, "lines")) +
  geom_sf_label(data = battle_creek_geometry, aes(label = "Battle Creek"), size = 3, color = "black", fontface = "bold", nudge_y = 0, nudge_x = 0.2) +
  geom_sf(data = WW_Watershed, fill = "lightblue", color = "black") +  # Watershed map
  geom_sf(data = channel_kml, fill = "lightblue", color = "black", size = 1) +  # Add the KML data with red color
  # Plot the points for each year, but keep their position fixed and color them by year
  geom_sf(aes(geometry = geometry, color = factor(year)), data = filtered_shapefile_data, size = 3) +
  coord_sf(xlim = c(-122.5, -121.5), ylim = c(39.8, 41), expand = FALSE, crs = 4326) +  # Set the correct bounding box limits
  # Add labels for Sacramento River and Lake Shasta
  #geom_label(aes(x = -122.0, y = 40.9, label = "Sacramento River"), 
             #fill = "white", color = "black", size = 3, fontface = "bold") +
  #geom_label(aes(x = -122.3, y = 40.7, label = "Lake Shasta"), 
             #fill = "white", color = "black", size = 3, fontface = "bold") +
  labs(title = "Winter-run Chinook Salmon Redd Locations", x = "Longitude", y = "Latitude") +
  # Set the theme to theme_bw and remove gridlines
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +  # Optional scale bar
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)  # Optional north arrow

# Create the animated plot
animated_map <- ggplot() +
  # Plot reservoirs and lakes with Lake Shasta labeled
  geom_sf(data = filtered_lentic, fill = "lightblue", color = "black") +
  geom_sf_label(data = lake_shasta_geometry, aes(label = "Lake Shasta"), size = 3, color = "black", fontface = "bold", nudge_y = 0, nudge_x = 0.2) +
  
  # Plot the upper Sacramento River watershed with a label
  geom_sf(data = filtered_watersheds, fill = "lightblue", color = "black") +
  # Manually place the Sacramento River label using annotate with a label box
  annotate("label", x = -122.28, y = 40.65, label = "Sacramento River", size = 3, color = "black", fontface = "bold", 
           fill = "white", label.size = 0.5, label.r = unit(0.25, "lines")) +
  geom_sf_label(data = battle_creek_geometry, aes(label = "Battle Creek"), size = 3, color = "black", fontface = "bold", nudge_y = 0, nudge_x = 0.2) +
  geom_sf(data = WW_Watershed, fill = "lightblue", color = "black") +  # Watershed map
  geom_sf(data = channel_kml, fill = "lightblue", color = "black", size = 3) +  # Add the KML data
  
  # Plot the points for each year, but keep their position fixed and color them by year
  geom_sf(aes(geometry = geometry, color = factor(year)), data = filtered_shapefile_data, size = 2) +
  
  # Use a categorical color scale for years
  scale_color_viridis_d(option = "viridis", direction = 1, name = "Year") +
  coord_sf(xlim = c(-122.5, -121.5), ylim = c(39.8, 41), expand = FALSE, crs = 4326) +  # Set the correct bounding box limits
  
  # Set up animation by discrete years
  transition_states(states = factor(filtered_shapefile_data$year), transition_length = 2, state_length = 1, wrap = FALSE) +
  
  # Add title and labels
  labs(title = 'Winter-run Chinook Salmon Redd Locations by Year: {closest_state}',
       x = "Longitude", y = "Latitude") +
  
  # Set the theme to theme_bw and remove gridlines
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, face = "bold", colour = "black"),
    axis.title = element_text(size = 16, face = "bold", colour = "black"),
    plot.title = element_text(size = 18, face = "bold", colour = "black", hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  ) 
# Render the animation
# Increase nframes for a smoother animation and reduce fps for a slower effect
anim <- animate(animated_map, nframes = 200, fps = 5, rewind = FALSE, width = 2400, height = 1600, res = 200)

# Save the animation as a gif
anim_save("animated_map.gif", animation = anim)
