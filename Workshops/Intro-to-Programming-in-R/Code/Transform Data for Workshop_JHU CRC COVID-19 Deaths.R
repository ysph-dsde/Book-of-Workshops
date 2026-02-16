## ----------------------------------------------------------------------------
## Produced by Yale's Public Health Data Science and Data Equity (DSDE) team
##
##     Workshop: A Journey into the World of Tidyverse
##      Authors: Shelby Golden, M.S. and Howard Baik, M.S.
## Last Updated: 2024-11-29
## 
##    R version: 4.5.2
## renv version: 1.1.7
## 
## Description: The raw COVID-19 deaths data from the JHU Center for Systems 
##              Science and Engineering (CSSE) GitHub repository 
##              (CSSEGISandData) required extensive preprocessing. This script 
##              intentionally reintroduces select data quality issues that can 
##              be addressed using straightforward tidying techniques in the 
##              workshop. For the complete data cleaning workflow, refer to 
##              "Cleaning Script_JHU CRC COVID-19 Deaths.R" on the Book of
##              Workshops webpage in "Workshops/Git-and-GitHub/Code/Data Cleaning/."


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here
renv::init()          # Initialize the project
renv::restore()       # Download packages and their version saved in the lockfile.

suppressPackageStartupMessages({
  library("readr")         # For reading in the data
  library("tidyr")         # For tidying data 
  library("dplyr")         # For data manipulation
  library("stringr")       # For string manipulation
  library("lubridate")     # Facilitates working with dates and times
})

# Function to select "Not In"
'%!in%' <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

# Read COVID-19 death data
cleaned_url <- "https://raw.githubusercontent.com/ysph-dsde/A-Journey-Into-The-World-of-tidyverse-Workshop/refs/heads/main/Data%20Sets/COVID-19%20Deaths_Cleaned_Aggregated%20by%20Month.csv"
cleaned     <- read_csv(file = cleaned_url) #, show_col_types = FALSE)

covid19_death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid19_death_raw <- read_csv(file = covid19_death_url) #, show_col_types = FALSE)




## ----------------------------------------------------------------------------
## TRANSFORM DATA FOR WORKSHOP

## The full data cleaning was conducted in a separate script that goes through
## all the the transformations and quality checks warranted by the raw set
## from JHU CRC's GitHub. Here, we transform that output into a semi-cleaned
## state so that limited operations can be conducted during the workshop.

## First we will port in the cleaned data and transform it to a long format.

df <- cleaned |> 
  # Select the metadata (region and date) and the smoothed cumulative counts.
  # The daily counts and raw cumulative counts are not needed in the workshop
  # worked through example.
  select(Combined_Key, Month, Deaths_Count_Cumulative_yf) |>
  # Convert long-format date columns to wide-format.
  pivot_wider(
    names_from = Month,
    values_from = Deaths_Count_Cumulative_yf
  ) |>
  # Change table format to "data frame" for convenience.
  as.data.frame()


## Now we will add in values that were removed during the cleaning process. The
## only values excluded were the two entries for COVID-19 deaths recorded on
## two cruise ships: the Diamond Princess and the Grand Princess.

# Pull the entries for these two cruise ships from JHU CRC's raw data.
cruise_ships <- covid19_death_raw[str_detect(covid19_death_raw$Combined_Key, "Princess"), ]

# Process these entries so that monthly deaths can be calculated from the daily
# updated record.
cruise_ships <- cruise_ships |>
  # Convert wide-format date columns to long-format.
  pivot_longer(
    cols = "1/22/20":"3/9/23",
    names_to = "date",
    values_to = "cumulative_count"
  ) |>
  # Subset the columns for the desired variables.
  select(Combined_Key, date, cumulative_count) |>
  # Convert the "date" variable class from character to Date.
  mutate(date = mdy(date)) |>
  # Group the values by month using lubridate to generate a new column with the
  # month for a respective daily entry.
  group_by(month = lubridate::floor_date(date, "month")) |> 
  # Remove the grouping and classify as a data frame.
  ungroup() |>
  # Change table format to "data frame" for convenience.
  as.data.frame() 


# Generate a by-month summary from the by-day updates. Group by "Combined_Key"
# and "month".
maxCumulative <- aggregate(. ~ Combined_Key + month, cruise_ships[, c(1, 3:4)], max) |>
  `colnames<-`(c("Combined_Key", "Month", "Deaths_Count_Cumulative_yf"))


# Transform this into a wide-format for adding back into the working data frame.
ships <- maxCumulative |> 
  # Convert long-format date columns to wide-format.
  pivot_wider(
    names_from = Month,
    values_from = Deaths_Count_Cumulative_yf
  ) |>
  # Change table format to "data frame" for convenience.
  as.data.frame() 


# Reintroduce the cruise ship values.
df <- bind_rows(df, ships)


## We will also change the string for the Virgin Islands so that it is not
## specified to be the US Virgin Islands

df[str_detect(df$Combined_Key, "Virgin Islands"), "Combined_Key"] <- "Virgin Islands, US"


## In the workshop, the total US counts were calculated by summing over all of
## the states. We will remove the existing US counts.

df <- df[!str_detect(df$Combined_Key, "^US"), ] |> `rownames<-`(NULL)


## Another necessary element is to split the "Combined_Key" into three columns
## representing the value (if present) of that regional level. We do this
## by iterating str_split() over subsets based on what regional level the
## entry represents.

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





## ----------------------------------------------------------------------------
## SAVE CLEANED DATA FOR ANALYSIS

## Finally, we will save this transformed data set for use in the workshop.

write.csv(df, "Data for the Workshop_Aggregated by Month.csv", row.names = FALSE)






