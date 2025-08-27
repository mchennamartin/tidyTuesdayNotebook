# Initialize -------------------------------------------------
# custom package function to avoid manual downloads when changing r versions
load <- function(package){
  # package - character - represents package or packages to install e.g. 'dplyr', 'limma'

  # check if input is one package or a list of them, by detecting any commas
  check_if_list <- any(grepl(',' , package))
  # if package variable has commas, check_if_list will return True

  # For listed input
  if(check_if_list == TRUE){
    # create list type from character/string
    package_list <- strsplit(package, ', ')
    package_list <- package_list[[1]]

    # look at number of elements in packageList
    number_of_packages <- length(package_list)

    # create loop with length equal to number of packages
    for(i in 1:number_of_packages){

      # if package isn't installed, install it
      if(!require(package_list[[i]], character.only = TRUE)){
        install.packages(package_list[[i]], ask = FALSE)
      } }

    # load package into environment once installed

    library(package_list[[i]], character.only = TRUE)

    }


  # For sole input
  if(check_if_list == FALSE){
     if(!require(package, character.only = TRUE)){
        install.packages(package, ask = FALSE)
      }
      # check to see if package installed, or if there was an error.
      # if packages is not installed yet, repeat with biocmanager
      if(!require(package, character.only = TRUE)){

    # load package into environment
    library(package, character.only = TRUE)
    }

  }}

# Load packages
load('sf, maps, tidyverse, here, ggthemes')

# Define directory
directory <- file.path(here(), '')

# Download data
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv')

# Slice and put this table in the long format
data_reshaped <- data |>
 select("Name", "Height_m", "Height_ft", "1891":"2021") |>
 pivot_longer(
    cols = "1891":"2021",
    names_to = "Year",
    values_to = "Classification"
  )
#rename NAs to "NA"s
temp <- is.na(data_reshaped["Classification"])
data_reshaped[temp, "Classification"] <- "NA"

# Exploraroty plots ------------------------------------------------------
#  Make plot where heights of hills are displayed, showing relative height of cutoffs;
#  classifications shown in blue munroe, white for top, and grey for NA
png('heights.png')
ggplot(data_reshaped, aes(
   x = Year,
   y = Height_ft,
   color = Classification)) +
  geom_point() +
  ylim(0, NA) +
  scale_color_excel() +
  labs(title = "Heights and Classifications of Munros Over Time",
       y = "Heights in feet") +
  geom_hline(yintercept = 0)
dev.off()
# MAPPING ----------------------------
# Make a coordinate map with munroes and tops displayed as spots on coordinate
# peaks as large dots, tops as small dots

# Make Map Data ---------------------------------
data_mapping <- data |>
  select("Name", "Height_m", "xcoord", "ycoord", "1891", "2021") |>
  filter(!is.na(ycoord) | !is.na(xcoord)) |>
  pivot_longer(
    cols = c("1891", "2021"),
    names_to = "Year" ,
    values_to = "Classification")

# add latitude and longitude to data_mapping
coords <- st_as_sf(data_mapping,
                   coords = c('xcoord', 'ycoord'),
                   crs = 27700)
coords <- st_transform(coords, 4326) |> st_coordinates()

data_mapping <- data_mapping |>
  mutate(
    long = coords[,1],
    lat = coords[,2])



# Make background map ------------------------------
# get map of UK, exclude southern portion of map
map_UK <- map_data("world", region = "UK") |>
  filter(lat > 56, lat <59)

map_UK <- st_as_sf(map_UK,
                   coords = c('long', 'lat'),
                   crs = 4326)
# convert type from point to polygon map
coords_UK <- map_UK |>
  group_by(group) |>
  summarize(do_union = FALSE) |>
  st_cast("POLYGON")


# Make map -------------------------------------
png("plot.png", width = 1200, height = 750)
ggplot(coords_UK) +
  geom_sf() +
  facet_wrap(~Year) +
  geom_point(data = data_mapping, aes(
    x = long,
    y = lat,
    color = Classification)) +
  labs(title = "Map of Munros Within Scotland",
       x = "Longitude",
       y = "Latitude") +
  scale_color_wsj()
dev.off()



