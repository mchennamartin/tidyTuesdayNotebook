#read in packages----------------------------------------
#custom package function to avoid extra inputs when changing r versions
load <- function(package){
  #package - character - represents package or packages to install e.g. 'dplyr', 'limma'

  #determine whether it is one package or a list of them, by detecting commas
  checkList <- grepl(', ', package)
  #if package variable has commas, checkList will return True

  #for listed input
  if(checkList == TRUE){
    #create list type from character/string
    packageList <- strsplit(package, ', ')
    packageList <- packageList[[1]]

    #look at number of elements in packageList
    packageNumber <- length(packageList)

    #create loop with length equal to number of packages
    for(i in 1:packageNumber){
      if(!require(packageList[[i]], character.only = TRUE)){
        install.packages(packageList[[i]], ask = FALSE)
      }
      #check to see if package installed, or if there was an error.
      #if packages is not installed yet, repeat with biocmanager
      if(!require(packageList[[i]], character.only = TRUE)){
        if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

        BiocManager::install(packageList[[i]], ask = FALSE)
       }

    #load package into environment
    library(packageList[[i]], character.only = TRUE)
    }
    }


  #for sole input
  if(checkList == FALSE){
     if(!require(package, character.only = TRUE)){
        install.packages(package, ask = FALSE)
      }
      #check to see if package installed, or if there was an error.
      #if packages is not installed yet, repeat with biocmanager
      if(!require(package, character.only = TRUE)){
        if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

        BiocManager::install(package, ask = FALSE)
       }

    #load package into environment
    library(package, character.only = TRUE)
    }

  }

#load packages
load('tidyverse, here')

#read in tables---------------------------------------------
studies <- read.csv('attribution_studies.csv')

studies_raw <- read.csv('attribution_studies_raw.csv')

#fix tables------------------------------------------------

