# Initialize ---------------------------
# Functions
#function to donwload and load packages if needed
load <- function(package){
  # package - character - represents package or packages to install e.g. 'dplyr', 'limma'
  # output - loaded packages, and installed packages if needed

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

# Read in tables
frogID_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv')
frog_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv')

# Tidy up data ------------------------------------
# Tidy up frogID_data
new_colnames <- c("occurenceID", "eventID", "lat", "long",
                  "latin_name", "event_date", "event_time", "timezone",
                  "coord_uncertainty_m", "userID", "province")

frogID_data_cleaned <- frogID_data |>
  # rename columns to be less cumbersome
  setNames(new_colnames) |>
  # separate event_time into columns Hours and minutes
  separate(
    col = event_time,
    into = c("hours", "minutes", "seconds"),
    sep = ":") |>
  select(-"seconds") |>
  # fix sigfigs of some columns
  # reduces size of DataFrame
  mutate(
    userID = str_remove(userID, "\\..*"),
    coord_uncertainty_m = signif(coord_uncertainty_m, digits = 2))

# Tidy up frog_names
new_colnames <- c("subfamily", "tribe", "latin_name", "common_name", "secondary_common_names")

frog_names_cleaned <- frog_names |>
  #rename columns
  setNames(new_colnames)

# Make sf object with map of Australia
aus_map <- map_data("world", "Australia")
aus_map <- st_as_sf(aus_map,
                    coords = c("long", "lat"),
                    crs = 4326) |>
  group_by(group) |>
  summarize(do_union = FALSE) |>
  st_cast("POLYGON")

# Make plots --------------------------------------
# create data frame tidied to make plot
frogmap_data <- frogID_data_cleaned |>
  left_join(frog_names_cleaned, relationship = "many-to-many") |>
  separate(
    col = "latin_name",
    into = c("genus", "species"),
    sep = " "
  ) |>
#separate year, month, day
  separate(
    col = "event_date",
    into = c("year", "month", "day"),
    sep = "-"
  ) |>
  #change months from numbers to names

# make a map of where observations occur for each subfamily
ggplot(aus_map) +
  geom_sf() +
  geom_point(data = frogmap_data,  size = 0.5,
             aes(x = long,
                 y = lat,
                 color = subfamily)) +
  scale_color_few(palette = "Medium")

# make histogram of frog encounters(subfamily, genus) per province(stack),
ggplot(frogmap_data, aes(y = subfamily,
                         fill = hours)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ province, scales = "free_y")
# make gif of frog encounters over time

# Prepare data for shiny app --------------------
# map of australia with marks for each frog sighting
# shiny app will allow users to pick from frog genus and species
# visualizes the location and time of year of each sound ID for that subfamily/tribe
# hovering over the dots should allow user to see/get collection information