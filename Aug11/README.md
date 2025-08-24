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
    load('tidyverse, here, janitor, glue, pandoc, quarto')

    ## Loading required package: tidyverse

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## Loading required package: here
    ## 
    ## here() starts at C:/Users/mchen/PycharmProjects/tidyTuesday
    ## 
    ## Loading required package: janitor
    ## 
    ## 
    ## Attaching package: 'janitor'
    ## 
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test
    ## 
    ## 
    ## Loading required package: glue
    ## 
    ## Loading required package: pandoc
    ## 
    ## Loading required package: quarto

    # read in table and clean
    attribution_studies_raw <- readr::read_csv(
      "https://interactive.carbonbrief.org/attribution-studies/data/papers-download.csv"
    ) |>
      janitor::clean_names()

    ## Rows: 744 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): Name, Event year/Trend, iso country code, cb-region, Event type, C...
    ## dbl  (1): Publication year
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

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

    # Consolidate similar event types
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

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_bin()`).

![](C:/Users/mchen/PycharmProjects/tidyTuesday/Aug11/README_files/figure-markdown_strict/unnamed-chunk-3-1.png)
