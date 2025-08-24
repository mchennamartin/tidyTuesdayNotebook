
# INITIALIZE----------------------------------------
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
      }

      # if packages is still not installed yet, biocmanager installer is required
      if(!require(package_list[[i]], character.only = TRUE)){
        if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

        BiocManager::install(package_list[[i]], ask = FALSE)
       }

    # load package into environment once installed

    library(package_list[[i]], character.only = TRUE)

    }
    }


  # For sole input
  if(check_if_list == FALSE){
     if(!require(package, character.only = TRUE)){
        install.packages(package, ask = FALSE)
      }
      # check to see if package installed, or if there was an error.
      # if packages is not installed yet, repeat with biocmanager
      if(!require(package, character.only = TRUE)){
        if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

        BiocManager::install(package, ask = FALSE)
       }

    # load package into environment
    library(package, character.only = TRUE)
    }

  }

# Load packages
load('tidyverse, here, janitor, glue')

# Define directories
dir_main <- file.path(here(), 'Aug11')
dir_data <- file.path(dir_main, 'data')
dir_plots <- file.path(dir_main, 'plots')

# Set working directory
setwd(dir_main)

# READ IN TABLES ---------------------------------------------
  data_raw <- read.csv(file.path(dir_data, 'attribution_studies_raw.csv') ) |>
            janitor::clean_names()

  data_autoclean <- read.csv(file.path(dir_data, 'attribution_studies.csv')) |>
          janitor::clean_names()



# DATA CLEANING --------------------------------------------
attribution_studies_raw <- readr::read_csv(
  file.path(dir_data, 'attribution_studies_raw.csv')
) |>
  janitor::clean_names()


# Helper functions -------------------------------------------------------

# Function to standardize year spans to consistent yyyy-yyyy format
clean_yearspan <- function(match) {
  # Split the span using "-" as a delimiter
  parts <- stringr::str_split(match, "-")[[1]]
  start_year <- as.numeric(parts[1])
  # Extract century from start year (e.g. 20 from 2020)
  century <- start_year %/% 100
  # Combine the years
  glue::glue("{parts[1]}-{century}{parts[2]}")
}

# Function to clean and standardize data strings
clean_date_string <- function(col) {
  col |>
    stringr::str_replace_all("–", "-") |>
    # Find yyyy-yy patters and convert to yyyy-yyyy
    stringr::str_replace_all("(\\d{4})-(\\d{2}$)", \(match) {
      clean_yearspan(match)
    }) |>
    stringr::str_replace_all(" & ", ", ")
}

# Data Cleaning ----------------------------------------------------------

data <- attribution_studies_raw |>
  janitor::clean_names() |>
  # Separate event names from time periods
  # and split them into separate 'event_name' and 'event_period' columns
  tidyr::separate_wider_regex(
    name,
    patterns = c(
      event_name = ".*",
      ", ",
      event_period = ".*",
      "\\s\\(.*"
    ),
    too_few = "align_start"
  ) |>
  # Create standardized variables
  dplyr::mutate(
    event_year_trend = clean_date_string(event_year_trend),
    event_period = dplyr::case_when(
      is.na(event_period) & event_year_trend != "Trend" ~
        dplyr::coalesce(event_period, event_year_trend),
      TRUE ~ event_period
    ) |>
      clean_date_string(),
    event_year = dplyr::case_when(
      event_year_trend != "Trend" ~ event_year_trend,
      TRUE ~ NA_character_
    ),
    study_focus = dplyr::case_when(
      event_year_trend == "Trend" ~ "Trend",
      TRUE ~ "Event",
    ),
    .before = iso_country_code
  ) |>
  dplyr::select(!event_year_trend)


# ADJUST TABLES------------------------------------------------

# Add seasons column
#TODO inverse seasons for southern hemisphere
# add winter to column for winter events
temp <- grepl('winter|Winter|January|Febuary|December', data$event_name)
data[temp, 'seasons'] <- 'Winter'
# add spring, summer, fall
temp <- grepl('spring|Spring|March|April|May', data$event_name)
data[temp, 'seasons'] <- 'Spring'
temp <- grepl('summer|Summer|June|July|August', data$event_name)
data[temp, 'seasons'] <- 'Summer'
temp <- grepl('Fall|autumn|Autumn|September|October|November', data$event_name)
data[temp, 'seasons'] <- 'Fall'

# Consolidate Event Types
data <- data |>
  mutate(event_type = case_when(
    grepl('Drought|Wildfire|Heat', event_type) ~ "Heat & Drought-Related",
    TRUE ~ event_type
  )) |>
  mutate(event_type = case_when(
    grepl('Colorado river drought|Colorado River discharge deficit', event_name) ~ "Heat & Drought-Related",
    TRUE ~ event_type
   )) |>
  mutate(event_type = case_when(
    grepl("Ocean|Atmosphere", event_type) ~ "Ocean &  Atmosphere",
    TRUE ~ event_type
   ))|>
  mutate(event_type = case_when(
    grepl("Susquehanna River extreme streamflow", event_name) ~ "Rain & flooding",
    TRUE ~ event_type))

# EXTRACT COUNTS -----------------------
# make a matrix with counts of each event for each year
count_matrix_years <- data |>
  # filter to rows only containing years
  filter(!is.na(event_year))|>
  tabyl(publication_year, classification, cb_region)

# PLOTS  ---------------------------
# plot functions
simple_boxplot <- function(table, independent, dependent){
  # This function creates a boxplot using an independent and dependent variable
  # input independent and dependent variables as strings E.g 'size','volume'
  # table = data frame
  # independent = string representing column with numeric values (qualitative)
  # dependent = string representing table column with qualitative values (quantitative)

  p <- ggboxplot(table, x=independent,y=dependent,
                 color=independent, palette =c("#00AFBB", "#E7B800", '#FF7F00'),
                 add="jitter",shape=independent)

  print(p)
}

# make plots

# make stacked histogram with facets,
# each facet should be a season, x as publication year, y as number of pubs per event type
data_seasons <- data |>
  filter(!is.na(seasons))

ggplot(data, aes(x = publication_year, fill = classification)) +
  geom_histogram(position = 'stack') +
  facet_wrap(~event_type, ncol = 4) +
  labs(title = "Attribution Studies by Event Type",
       x = 'Year',
       y = 'Number of Publications') +
  geom_hline(yintercept = 0, linewidth = .2)






