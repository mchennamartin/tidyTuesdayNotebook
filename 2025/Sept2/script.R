# Initialize ---------------------------
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
load('sf, janitor, tidyverse, here, ggthemes')

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
    coord_uncertainty_m = signif(coord_uncertainty_m, digits = 2)) |>

  # split up latin name into genus and epithet
  # this makes it easier to group maps by genus/epithet in shiny app

# Tidy up frog_names
# TODO separate genus and epithet from scientific name
# TODO remove rows with NAs in the epithet
# TODO replace empty values with NAs
# 



# Make plots --------------------------------------
# standardize time to eliminate need for time zone; will use GMT +900

# make gif of frog encounters over time

# Prepare data for shiny app --------------------
# map of australia with marks for each frog sighting
# shiny app will allow users to pick from frog genus and species
# visualizes the location and time of year of each sound ID for that subfamily/tribe
# hovering over the dots should allow user to see/get collection information