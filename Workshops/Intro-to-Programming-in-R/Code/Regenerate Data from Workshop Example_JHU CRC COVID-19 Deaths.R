## ----------------------------------------------------------------------------
## Produced by Yale's Public Health Data Science and Data Equity (DSDE) team
##
##     Workshop: A Journey Into The World of tidyverse
##      Authors: Shelby Golden, M.S. and Howard Baik, M.S.
## Last Updated: 2026-02-16
## 
##       R version: 4.5.2
## RStudio version: 2026.01.0+392
##    renv version: 1.1.7
##
## Description: For in-session participation, participants need the dataset from
##              the end of the worked-through example. This script quickly
##              regenerates that dataset.


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here
renv::init()          # Initialize the project
renv::restore()       # Download packages and their version saved in the lockfile.

suppressPackageStartupMessages({
  library("readr")      # For reading in the data
  library("tidyr")      # For tidying data 
  library("dplyr")      # For data manipulation 
  library("stringr")    # For string manipulation
})


# Function to select "Not In"
'%!in%' <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This section has been included to regenerate the necessary data format used 

# Read COVID-19 death data
cleaned_url <- "https://raw.githubusercontent.com/ysph-dsde/Book-of-Workshops/refs/heads/main/Workshops/Intro-to-Programming-in-R/Data/Deaths%20and%20Cases%20Aggregated%20by%20Month.csv"
cleaned     <- read_csv(file = cleaned_url) #, show_col_types = FALSE)


# Select the columns we need and adjust the column names.
df <- cleaned |>
  select(Combined_Key, Month, Deaths_Count_Daily) |>
  # Change the column names using a pipe syntax. The row-equivalent is 
  # `rownames<-`(), and is often used to clear row names.
  `colnames<-`(c("Combined_Key", "Month", "Deaths_Count"))


## The cleaned data set from the `Cleaning Script_JHU CRC COVID-19 Deaths.R`
## file drops the county-, state-, and country-level columns and retains the
## "Combined_Key" only. We will want to regenerate those columns so they resemble
## the output from the workshop cleaning steps.

# Generate an empty data frame that will be filled.
empty_data <- data.frame("County" = rep(NA, nrow(df)), 
                         "Province_State" = rep(NA, nrow(df)), 
                         "Country_Region" = rep("US", nrow(df)))

# Combine the empty data frame into the main one.
df <- cbind(df[, 1, drop = FALSE], empty_data, df[, 2:ncol(df)])

for(i in 1:2) {
  # Search for which index corresponds with the county- or state-level of information.
  index = which(str_count(df$Combined_Key, ",") == i)
  
  if(i < 2) {
    # When the index indicates state-level, only fill in the "Province_State"
    # variable.
    df[index, "Province_State"] <- df[index, "Combined_Key"] |> 
      str_split(",", simplify = TRUE, n = 2) |> 
      _[, 1]
    
  } else{
    # When the index indicates county-level, fill in both the "Province_State"
    # and "County" columns.
    split_result <- df[index, "Combined_Key"] |> 
      str_split(",", simplify = TRUE, n = 3)
    
    df[index, "County"] <- split_result |> _[, 1]
    
    df[index, "Province_State"] <- split_result |> _[, 2] |>
      str_trim(side = "both")
  }
}


## Now we are ready to review those solutions.
head(df)




## ----------------------------------------------------------------------------
## SAVE CLEANED DATA FOR PARTICIPANTS

write.csv(df, "Workshops/Intro-to-Programming-in-R/Data/Data for the Questions_Aggregated by Month.csv", row.names = FALSE)


