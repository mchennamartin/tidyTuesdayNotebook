
# INITIALIZE----------------------------------------
# custom package function to avoid manual downloads when changing r versions
load <- function(package){
  # package - character - represents package or packages to install e.g. 'dplyr', 'limma'

  # determine whether it is one package or a list of them, by detecting commas
  check_if_list <- any(grepl(',' , package))
  # if package variable has commas, checkList will return True

  # for listed input
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


  # for sole input
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


# load packages
load('tidyverse, here, janitor')

# define directories
dir_main <- file.path(here(), 'Aug11')
dir_data <- file.path(dir_main, 'data')
dir_plots <- file.path(dir_main, 'plots')

# set working directory
setwd(dir_main)

# READ IN TABLES---------------------------------------------
  data_raw <- read.csv(file.path(dir_data, 'attribution_studies_raw.csv') ) |>
            janitor::clean_names()

  data <- read.csv(file.path(dir_data, 'attribution_studies.csv')) |>
          janitor::clean_names()

# DATA CLEANING (me) ----------------------------------
#use janitor, tabyls,

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

attribution_studies <- attribution_studies_raw |>
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


# FIX TABLES------------------------------------------------
studies <- attribution_studies_raw
# rename columns
colnames(studies) <- c('name', 'year', 'iso', 'region', 'type', 'classification', 'summary', 'publication',
                       'citation', 'source', 'rapid', 'link')
studies$length <- as.character(studies$year)
# create column defining whether it is a trend or event
trend <- grepl('Trend', studies$year)
# change elements based on whether it is a trend or event
studies[trend, 'length'] <- 'Longitudinal'
studies[!trend, 'length'] <- 'Event'

# change year to NA where longitudinal
studies[trend, 'year'] <- NA

# add seasons column

# add winter to column for winter events
temp <- grepl('winter|Winter|January|Febuary|December', studies$name)
studies[temp, 'seasons'] <- 'Winter'
# add spring, summer, fall
temp <- grepl('spring|Spring|March|April|May', studies$name)
studies[temp, 'seasons'] <- 'Spring'
temp <- grepl('summer|Summer|June|July|August', studies$name)
studies[temp, 'seasons'] <- 'Summer'
temp <- grepl('Fall|autumn|Autumn|September|October|November', studies$name)
studies[temp, 'seasons'] <- 'Fall'

# EXTRACT COUNTS -----------------------
countMatrix <-
# PLOTS  ---------------------------
# plot functions

simpleBoxPlot <- function(table, independent, dependent){
  # This function creates a boxplot using an independent and dependent variable
  # input independent and dependent variables as strings E.g 'size','volume'
  # table = data frame
  # Independent = string representing column with numeric values (qualitative)
  # Dependent = String representing table column with qualitative values (quantitative)

  p <- ggboxplot(table, x=independent,y=dependent,
                 color=independent, palette =c("#00AFBB", "#E7B800", '#FF7F00'),
                 add="jitter",shape=independent)

  print(p)
}

# make plots
simpleBoxPlot(studies, 'seasons', )
