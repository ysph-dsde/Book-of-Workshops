## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
## 
## Workshop: Getting Started with Git and GitHub
##  Authors: Shelby Golden, M.S.
##     Date: 2024-11-21
## 
##    R version: 4.4.3
## renv version: 1.0.11
## 
## 
## Description: This script imports the Johns Hopkins University Coronavirus
##              Resource Center (JHU CRC) confirmed COVID-19 cases and deaths 
##              data for U.S. states and territories, and pre-processes it for 
##              time-series plotting. The data was assessed for irregularities 
##              and errors (i.e. duplicated entries and NA's). The cumulative 
##              counts were smoothed to be fully monotonically increasing before
##              calculating the daily counts.


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here.
renv::restore()

## Load in the R packages used in this script from the project library.
suppressPackageStartupMessages({
  library("readr")      # Read in csv's
  library("tidyr")      # Convert data frames into tidy format
  library("dplyr")      # For data manipulation
  library("stringr")    # For string manipulation
  library("MMWRweek")   # Convert Dates to MMWR day, week, and year
  library("lubridate")  # For date manipulation
  library("stats")      # Compilation of statistical functions
  library("ggplot2")    # For creating static visualizations
  library("scales")     # Override default ggplot2 axes and legend settings
  library("plotly")     # For interactive plots
})


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University (JHU), GitHub 
## CSSEGISandData. Additional details can be found in the project repository's 
## main directory's README file.

## We load it in directly from the JHU CRC's GitHub page using the raw URL.
covid19_death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid19_death_raw <- read_csv(file = covid19_death_url) |>
  as.data.frame()

covid19_confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid19_confirmed_raw <- read_csv(file = covid19_confirmed_url, show_col_types = FALSE) |>
  as.data.frame()


## The JHU Coronavirus Resource Center (JHU CRC) GovEX repository includes a
## census population file with intercensal estimates and projections from 2010:
## location:      govex/COVID-19/tree/master/data_tables/Data_for_UScounty_map
## file:          PovertyEstimates.xls
## downloaded as: U.S. Department of Agriculture_Population Estimates 2010 to 2018_JHU CRC.xls
##
## This file only covers U.S. population estimates from 2010 to 2018, and do not
## include representation of the U.S. Territories, with the exception of Puerto
## Rico. In the GitHub for this project, file "Git and GitHub/Population Estimates 
## and Projections" describes how three files from U.S. Census Bureau and one
## from the U.S. Department of Agriculture with census year population number, 
## estimates, and projections were harmonized.

## We load this harmonized file. It spans population intercensal estimations
## and projections from 2010 to 2023 based on the 2010 and 2020 U.S. Census'.
## 
## NOTE: To combine U.S. Census Bureau counts for the U.S. states and Puerto Rico
## with the remaining U.S. territories, two methods for generating the 
## estimates and projections are represented. Refer to this projects GitHub
## for guidance on which method is most appropriate to use for regions that
## are represented by both methods:
##    - Official U.S. Estimates   denoted by OE
##    - International Database    denoted by IDB

census_2010to2023 <- read.csv(
  "Git and GitHub/Population Estimates and Projections/US_Census Population Estimates_2010 to 2023.csv",
  header = TRUE)




## ----------------------------------------------------------------------------
## DATA PREPARATION

# -----------------------------
# FORMATTING

## Death counts due to COVID-19 are expected to be more accurate to the daily 
## count level as opposed to confirmed cases. For this example, we will combine 
## the two and aggregate to the nearest week.
## 
## First we will inspect the characteristics of our data set.

head(covid19_death_raw)[, 1:15]
dim(covid19_death_raw)

head(covid19_confirmed_raw)[, 1:15]
dim(covid19_confirmed_raw)


## We see that new observations were added as new columns, so that each row
## represents values for one, unique county in the U.S. To tidy the data, we
## need to reshape the dates columns as added rows so that columns only
## represent variables while rows represent new observations. This can be done 
## using pivot_longer().

# Find the start and end date of the new observation columns.
colnames(covid19_death_raw)[c(13, ncol(covid19_death_raw))]

covid19_death_raw_long <- 
  covid19_death_raw |> 
  # Step 1: Convert wide-format date columns to long-format.
  pivot_longer(
    # Designate which columns need to be pivoted.
    cols = "1/22/20":"3/9/23", 
    # Name the variable that will store newly pivoted column names.
    names_to = "date", 
    # Name the variable that will store respective cell values.
    values_to = "cumulative_count"
  ) |> 
  # Step 2: Change table format to "data frame" for convenience.
  as.data.frame()


# Find the start and end date of the new observation columns.
colnames(covid19_confirmed_raw)[c(12, ncol(covid19_confirmed_raw))]

covid19_confirmed_raw_long <- 
  covid19_confirmed_raw |> 
  # Step 1: Convert wide-format date columns to long-format.
  pivot_longer(
    # Designate which columns need to be pivoted.
    cols = "1/22/20":"3/9/23", 
    # Name the variable that will store newly pivoted column names.
    names_to = "date", 
    # Name the variable that will store respective cell values.
    values_to = "cumulative_count"
  ) |> 
  # Step 2: Change table format to "data frame" for convenience.
  as.data.frame()


## Looking again at the top rows and dimensions of the long-form data set shows
## that our transformation was successful.

covid19_death_raw_long     |> glimpse()
covid19_confirmed_raw_long |> glimpse()


## We can now combine the two datasets into one for easier processing.

covid19_death_raw_long <- covid19_death_raw_long |>
  rename(deaths = cumulative_count)

covid19_confirmed_raw_long <- covid19_confirmed_raw_long |>
  rename(confirmed_cases = cumulative_count)

covid19_combined_raw <- left_join(covid19_death_raw_long, covid19_confirmed_raw_long)


## We see that a number of the columns represent information that is already 
## implied or is not relevant. For example, UID, or the unique identifier for 
## each row entry, no longer has meaning or contain information we need after 
## pivoting the data long.
## 
## We will remove these unnecessary columns and adjust the remaining variable
## names so that they are clearer. The data dictionary for the raw data set can
## be found in the JHU CRC CSSEGISandData GitHub README file:
## 
## https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#field-description-1

colnames(covid19_combined_raw)


## As an aside, the population column denotes one static representation of the
## U.S. population in that county. This value does not change over the entire
## span of time represented, which is not idea. We would like to add values from
## a harmonized U.S. census dataset that spans state-level population estimates
## from 2010 to 2023. We have put this ancillary dataset together, and this can
## be found in: https://github.com/ysph-dsde/JHU-CRC-Vaccinations
## 
## We can confirm that our interpretation of the "Population" column is true by 
## examining the number of times a different population count is represented 
## for unique counties.
 
expected_count         <- unique(covid19_combined_raw$date) |> length()
diff_population_counts <- table(covid19_combined_raw$Admin2, 
                                covid19_combined_raw$Population, 
                                covid19_combined_raw$Province_State) |> 
                          as.data.frame()


# The following Boolean test will be TRUE if only one population count is
# represented for each county over the span of time represented in the data set.
# The function all() is going to check the vector of Boolean results and confirm
# if all of the Boolean values are TRUE. If they are, the results will say "TRUE".

unique(diff_population_counts$Freq) %in% c(0, expected_count) |> all()


## Now we can subset the columns for the desired variables using select().

df_subset <- covid19_combined_raw |> 
  select(Admin2, Province_State, Country_Region, Combined_Key, 
         date, deaths, confirmed_cases)

## We'll adjust the column names so that they are more intuitive.

colnames(df_subset) <- c("County", "Province_State", "Country_Region", 
                         "Combined_Key", "Date", "Deaths", "Confirmed_Cases")




# -----------------------------
# STATE- AND COUNTRY-LEVEL VALUES

## We would like to see state- and country-level counts. Currently, the
## data set only reports county-level counts. We calculate these values by
## summing the cumulative counts over entries that have been grouped by
## "Province_State" and "Date". Then, we do the same operation over all of
## the U.S. state entries that have been grouped by "Date" only.
## 
## First we start by calculating the state-level data from the county-level data.

counts_by_state <- df_subset |> 
  # Groups the table by unique entries in "Province_State" followed by "Date".
  group_by(Province_State, Date) |>
  # Summing "Deaths" and "Confirmed_Cases" over the grouped rows. Note that 
  # '.groups = "keep"' will maintain the grouping for the summation.
  summarise(Deaths = sum(Deaths), Confirmed_Cases = sum(Confirmed_Cases), .groups = "keep") |>
  # Mutate will generate new columns. These can be functions of existing columns 
  # or static operations. For example, we can generate a new "Combined_Key" for 
  # the state-level data that excludes counties using the stringr concatenate 
  # function, str_c().
  mutate(Country_Region = "US", Combined_Key = str_c(Province_State, ", US")) |> 
  # Remove grouping to work with the full data frame again.
  ungroup()

head(counts_by_state)
dim(counts_by_state)


## Next we calculate the country-level data by summing over all of the U.S.
## state entries.

counts_by_country <- counts_by_state |> 
  # Filter subsets the data set by rows that match the condition. This will 
  # subset the data set for entries that are U.S. states including the District 
  # of Columbia.
  filter(Province_State %in% c(datasets::state.name, "District of Columbia")) |>
  # Groups the table by unique entries in "Date".
  group_by(Date) |>
  # Calculate the cumulative deaths variable by summing over the grouped rows.
  summarise(Deaths = sum(Deaths), Confirmed_Cases = sum(Confirmed_Cases), .groups = "keep") |>
  # Generate new columns using mutate.
  mutate(Country_Region = "US", Combined_Key = "US") |> 
  # Remove grouping to work with the full data frame again.
  ungroup()

head(counts_by_country)
dim(counts_by_country)


## Now that we have our state- and country-level data, we need to combine them.
## We no longer require the county-level information.
df <- bind_rows(counts_by_country, counts_by_state) |> as.data.frame()


## We can confirm that this operation was successful by examining the first and
## last few rows.

head(df)
tail(df)




# -----------------------------
# CORRECT MONOTONICITY

## The variable classes look as we'd expect. Now we'll check for missing values.
sapply(df, function(x) sum(is.na(x))) |>
  as.data.frame() |>
  `colnames<-`("NA Count")

# As expected, there would be some entries in "Province_State". These reflect
# the national level counts.
nrow(counts_by_country)


## We expect that the U.S. states and territories will be included. We can 
## examine other entries by matching unique entries of "Province_State" to 
## datasets::state.name.

unique(df$Province_State)[unique(df$Province_State) %!in% c(datasets::state.name, "District of Columbia")]


## Notice that there are "NAs". This should be correct, since the country-level
## counts are NA at the state-level. We can confirm this by showing all
## "Combined_Key" entries are "US" for rows with "Province_State" = NA.

df[df$Province_State %in% NA, "Combined_Key"] |> unique()


## In addition to the District of Columbia and five U.S. territories, there
## are entries for two cruise ships. These are not relevant to our analysis,
## and so we exclude them.
df_filtered <- df[!str_detect(df$Province_State, "Princess") %in% TRUE, ]


## The "Combined_Key" variable combines information from "County",
## "Province_State", and "Country_Region". We do not need this level of 
## information, only "Province_State". The "Province_State = NA" will need to 
## be changed to "United States".
df_filtered[is.na(df_filtered$Province_State), "Province_State"] <- "United States"




## ----------------------------------------------------------------------------
## FORMATTING AND CALCULATIONS

## Commit formatting changes and reorganize the columns.
df <- df_filtered |>
  select(-Country_Region, -Combined_Key) |>
  rename(Week = Date) |>
  select("Province_State", "Week", "Deaths", "Confirmed_Cases")

## Finally, we will confirm that our variables are set to the correct class.

sapply(df, class)

## Currently, "Date" is classified as a character. We can change the class to 
## date using the lubridate package from tidyverse. This package is not covered
## in this introductory workshop, but those interested to find out more can
## review the package documentation: https://lubridate.tidyverse.org/
## 
## Notice that the data has been reported in mm/dd/yy format. Therefore, we
## use mdy() to correctly convert the character to the date class.

df$Week <-  mdy(df$Week)

# We will reorder the dates so that they are in ascending order.
df <- df |>
  group_by(Province_State) |>
  arrange(Week, .by_group = TRUE) |>
  ungroup()




# -----------------------------
# CORRECT MONOTONICITY

## One critical assumption for cumulative counts is that they are monotonically
## increasing, which means that counts for subsequent dates should be at least
## as equal or greater in value than the previous one. We need to verify that
## this is the case before calculating our daily counts. 
## 
## In the following for loop, we are going to confirm that the date ranges
## for each unique region is represented once ("dates_match") and that the
## deaths cumulative counts are monotonically increasing ("boolean").

result <- list()
for (i in 1:length(unique(df$Province_State))) {
  # Separate out the dates vector associated with one "Province_State" entry.
  subset <- df[df$Province_State %in% unique(df$Province_State)[i], "Week"] |> 
    as.data.frame() |> _[[1]]
  
  check <- c()
  for (j in 1:(length(subset) - 1)) {
    # Count the number of days between each successive date.
    check[j]  <- difftime(subset[j + 1], subset[j], units = "days") |> 
      as.numeric()
  }
  
  # Store results as a Boolean. Test will report TRUE if at least one span
  # between dates satisfies the conditional test.
  result[[i]] <- data.frame(
    "Equal 0" = 0 %in% unique(check),
    "Equal 1" = 1 %in% unique(check),
    "Else"    = any(unique(check) > 1 | unique(check) < 0)
  )
}
# Combine the results (list format) into a data frame by adding rows. Each 
# row reflects the results for one "Province_State" span of dates.
result <- do.call(rbind, result) |> 
  `colnames<-`(c("Equal 0", "Equal 1", "Else")) |>
  `rownames<-`(unique(df$Province_State))

result

# Now we define the search space for the unique regions, and the reference weeks,
# where the weeks for "Province_State = United States" is expected to be the
# maximal set.
unique_regions  <- unique(df$Province_State)
dates_reference <- df[df$Province_State %in% "United States", "Week"] |> 
  as.data.frame() |> _[[1]]

# Define empty placeholder vectors that we will fill with results from the for loop.
boolean_1   <- c()
boolean_2   <- c()
dates_match <- c()
# Define the loop range to cover the entire length of unique regions.
for(i in 1:length(unique_regions)) {
  # Subset the data frame so that only one unique regions is represented.
  subset <- df[df$Province_State %in% unique_regions[i], ]
  
  # Notice that the structure of the following tests is only possible because
  # we have organized the rows so that the dates are ascending.
  
  # Check that the successive dates are greater or equal in value from the
  # previous value.
  boolean_1[i] <- all(diff(subset$Deaths) >= 0)
  boolean_2[i] <- all(diff(subset$Confirmed_Cases) >= 0)
  # If all dates are represented as expected, then we expect an exact match
  # to our reference vector.
  dates_match[i] <- all(subset$Week == dates_reference)
}
# Compile the results into a data frame.
result <- data.frame("Region" = unique_regions, "Result_Deaths" = boolean_1,
                     "Result_Cases" = boolean_2, 
                     "Dates_Match_Reference" = dates_match)


## Now we can check the results.
result[result$Result_Deaths == TRUE, ]  |> nrow()
result[result$Result_Deaths == FALSE, ] |> nrow()

result[result$Result_Cases == TRUE, ]  |> nrow()
result[result$Result_Cases == FALSE, ] |> nrow()

result[result$dates_match == FALSE, ] |> nrow()


## Unfortunately, we see that most state-level entries failed and the one value
## for the US also failed. For example, we can see in the plot below that there 
## are dates where deaths counts were not perfectly monotonically increasing 
## as expected. Fortunately, none of the dates failed to match the reference.

df |>
  # Filter the data set for one specific "Province_State".
  filter(Province_State %in% "Oklahoma") |>
  ggplot(data = _, aes(x = Week, y = Deaths, color = Province_State)) +
  geom_line() +
  # Format the y-axis to show values in terms of thousands.
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
  # every four months.
  scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
  # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
  labs(title = "Deaths from COVID-19 in Oklahoma",
       x = "Week", y = "Cumulative Deaths Recorded") +
  # Graph displays as minimal with a legend.
  theme_minimal()


## Most entries are monotonic, with minor blips. In the cleaning script for
## vaccinations, we show that the isotonic regression works to smooth this
## result.


## All entries returned TRUE, which is a good sign we can proceed with the 
## isotonic regression method of data smoothing for all of the data.

smooth <- colnames(df[, -c(1:2)])

result = list()
for (i in 1:length(unique(df$Province_State))) {
  # Subset the data set for one "Province_State" entry. This time the
  # search space is not restricted.
  subset <- df[df$Province_State %in% unique(df$Province_State)[i], ]
  
  ir_yf = list()
  for (j in 1:length(smooth)) {
    # Filter out the vaccine count method vector and fit using isoreg().
    ir <- isoreg(subset[, smooth[j]], y = NULL)
    
    # Save the fitting results predicted y-values. NOTE: In "yf" "f" means
    # "fitted"and "y" denote the prediction based on the isotonic fitting
    # results.
    ir_yf[[j]] <- ir$yf
  }
  
  # Combine the results (list format) into a data frame by adding columns. Each
  # new column represents a different vaccination counting method.
  dataTable <- do.call(cbind, ir_yf) |>
    `colnames<-`(str_c(smooth, "_yf")) |> 
    as.data.frame()
  
  # Separate out the "Date" and "Province_State" columns for the vector of
  # newly fitted results. Everything should be in the same row-order and not
  # require matching to merge. These columns will be used to merge the fitted
  # values back to the main data set.
  rowNames  <- df[df$Province_State %in% unique(df$Province_State)[i], c("Province_State", "Week")]
  
  # Combine the metadata (rowNames) to the built vector of fitted vaccination
  # counts (dataTable).
  result[[i]] <- cbind(rowNames, dataTable)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) |>
  `colnames<-`(c("Province_State", "Week", str_c(smooth, "_yf"))) |>
  as.data.frame()

# Merge the previous data set with the newly smoothed values, combining by
# unique matches with the "Date" and "Province_State" columns.
df_monotonic <- merge(df, result, by = c("Province_State", "Week"))


## We can confirm that this results worked by rerunning the same plot. Here,
## the purple line represents the raw data while the blue line represents the
## isotonic regression fitting results. We see that the fitting corrects
## existing problems related to monotonicity.

df_monotonic |>
  # Filter the data set for one specific "Province_State". 
  filter(Province_State %in% "Oklahoma") |>
  ggplot(data = _) +
  # Line for the raw values. Colored purple.
  geom_line(aes(x = Week, y = Deaths, color = Province_State), 
            color = "#A353FF", size = 1) +
  # Line for the yf fitted values. Colored blue.
  geom_line(aes(x = Week, y = Deaths_yf, color = Province_State), 
            color = "#00356B", size = 1) +
  # Format the y-axis to show values in terms of thousands.
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
  # every four months.
  scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
  # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
  labs(title = "Deaths from COVID-19 in Oklahoma",
       x = "Week", y = "Cumulative Deaths Recorded") +
  # Graph displays as minimal with a legend.
  theme_minimal()


## Now that we have finished cleaning the data we are ready to backcalculate 
## the daily counts from the cumulative values.

search_space = unique(df_monotonic$Province_State)

result = list()
for (i in 1:length(search_space)) {
  # Subset the data set one for "Province_State" entry.
  subset <- df_monotonic[df_monotonic$Province_State %in% search_space[i], ]
  
  # Calculate the difference between the ith and i+1 value. Use the first value
  # in the original vector as the first value in the new vector and shift 
  # difference counts down one position. In this case we use the fitted values
  # so that no negative counts are introduced.
  daily_count_1 <- c(subset[1, "Deaths_yf"], diff(subset[, "Deaths_yf"]))
  daily_count_2 <- c(subset[1, "Confirmed_Cases_yf"], diff(subset[, "Confirmed_Cases_yf"]))
  
  # Add the results as a new column back to the originally subsetted data.
  result[[i]] <- cbind(subset, daily_count_1, daily_count_2) |> 
    `colnames<-`(c(colnames(subset), "Deaths_Daily", "Confirmed_Cases_Daily"))
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Combined_Key".
df_final <- do.call(rbind, result)


## We can confirm that our calculation was done correctly by plotting the
## results for one region.

df_final |>
  # Filter the data set for one specific "Combined_Key".
  filter(Province_State %in% "Oklahoma") |>
  ggplot(data = _) +
  # Line for the raw values. Colored purple.
  geom_line(aes(x = Week, y = Deaths_Daily)) +
  # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
  # every four months.
  scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
  # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
  labs(title = "Deaths from COVID-19 in Oklahoma",
       x = "Week", y = "Daily Deaths Recorded") +
  # Graph displays as minimal with a legend.
  theme_minimal()


## It is difficult to interpret trends over the daily counts. It would be easier
## to see how things changed over three years if counts were combined by monthly
## updates. We can do this using aggregate, which is an alternative to dplyr's
## summarise() function.

# Create a new vector that rounds "Dates" down to the nearest week.
df_weekly <- df_final |>
  # Group the values by week using lubridate to generate a new column with the
  # week for a respective daily entry.
  group_by(week = lubridate::floor_date(Week, "week")) |> 
  # Remove the grouping and classify as a data frame.
  ungroup() |> as.data.frame()

# In the aggregation, retain only the max value of the cumulative counts.
# Organize aggregation by unique "Province_State" entries and dates rounded down 
# to the nearest week
maxCumulative <- aggregate(. ~ Province_State + week, df_weekly[, c(1, 3:6, 9)], max) |>
  `colnames<-`(c(colnames(df_weekly)[c(1:6)]))

# Do the same aggregation over the daily counts, but retain the sum.
sumDaily<- aggregate(. ~ Province_State + week, df_weekly[, c(1, 7:9)], sum) |>
  `colnames<-`(c(colnames(df_weekly)[c(1:2, 7:8)]))

# Merge these two aggregated data sets, combining by unique matches in the
# "Date" and "Province_State" columns.
df_byWeek <- merge(maxCumulative, sumDaily, by = c("Province_State", "Week"))


## If we replot the same values, but aggregated by months, we can much more
## clearly see trends over the duration of the pandemic.

df_byWeek |>
  # Filter the data set for one specific "Combined_Key".
  filter(Province_State %in% "Oklahoma") |>
  ggplot(data = _) +
  # Line for the raw values. Colored purple.
  geom_line(aes(x = Week, y = Deaths_Daily)) +
  # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
  # every four months.
  scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
  # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
  labs(title = "Deaths from COVID-19 in Oklahoma",
       x = "Month", y = "Cumulative Deaths Recorded") +
  # Graph displays as minimal with a legend.
  theme_minimal()




## ----------------------------------------------------------------------------
## NORMALIZE TO GET PERCENTAGE OF POPULATION VACCINATED

## It is easier to compare vaccinations between states when they have been
## normalized to the population of that state. To do this, we need to port
## in our state-level population values using the harmonized census population 
## estimates dataset we've prepare.
## 
## NOTE: Vintages are updated annually based on the current year of the update
## and only spans the dates between the recent census year to the last
## intercensal year preceding the next census. The harmonized data set combines
## vintages from the 2010 to 2019 census estimates and the 2020 to 2023
## census estimates. Additional details can be found in the "Git and GitHub/
## Population Estimates and Projections" subdirectory of this projects GitHub 
## repo.

colnames(census_2010to2023)

## There are a number of columns of information that we do not require; we
## only need those reflecting the population estimates for each year.

subset_census <- cbind(census_2010to2023[, c("NAME", "METHOD")], 
                       # Extract columns with that contain the "POPESTIMATE"
                       # string, with a string detection boundary preceding
                       # the word.
                       census_2010to2023[str_detect(colnames(census_2010to2023), 
                                                    "\\bPOPESTIMATE")])

dim(subset_census)
colnames(subset_census)


## We see that this successfully extracted the columns we require for our
## normalization. Now we'll check the different states or territories
## that are included in this census data set.

# All uniquely represented state, territories, and regions.
census_2010to2023$NAME |> unique()

# State, territories, and regions represented more than once.
census_2010to2023$NAME |> table() |>
  (\(x) { x[x > 1] }) () |>
  as.data.frame()

# "Province_State" entries not represented in the U.S. Census data.
unique(df_byWeek$Province_State) |>
  (\(x) { x[x %!in% unique(census_2010to2023$NAME)] }) ()


## The first thing to notice is the Virgin Islands is represented in the
## U.S. Census data, but with a different string: "Virgin Islands, U.S.". We
## can correct this by changing the df_byWeek name.

df_byWeek$Province_State[df_byWeek$Province_State %in% "Virgin Islands"] <- "Virgin Islands, U.S."

# Double check that the Virgin Islands entry is identified as a match.
unique(df_byWeek$Province_State) |> 
  (\(x) { x[x %!in% unique(census_2010to2023$NAME)] }) ()


## The second thing to notice is that the United States and Puerto Rico
## census data is represented twice. This is because the U.S. Census Bureau 
## uses two different algorithms for calculating the population estimates and
## projections for census and intercensal years: Official U.S. Estimates (OE) 
## and International Database (IDB). On their website, they recommend using
## the OE for both metrics for the United States, and the OE for population
## estimates and IDB for population projections for Puerto Rico.
## 
## We filter the census data to reflect this recommendation.

subset_census <- subset_census[!c(subset_census$NAME == "United States" & subset_census$METHOD == "IDB") &
                                 !c(subset_census$NAME == "Puerto Rico" & subset_census$METHOD == "OE"), ]


## Now we can filter out the extra entries that are not represented in the
## census data set.
df_filtered <- filter(df_byWeek, df_byWeek$Province_State %in% subset_census$NAME)


## There are a few regional calculations in the census data that we do not
## have in the vaccination data set.

missing_entries <- unique(census_2010to2023$NAME) |>
  (\(x) { x[x %!in% unique(df_byWeek$Province_State)] }) ()

missing_entries




# -----------------------------
## ADD MISSING REGIONS AND DIVISIONS TO MAIN DATA SET

## We can generate broader regions using the regional and division map provided by
## the U.S. Census Bureau. Refer to this projects GitHub, folder "Git and GitHub/
## Population Estimates and Projections" subdirectory README for additional 
## information.

# Create a reference to map census counts when aggregating sums.
us_regions_divisions <- 
  data.frame("Province_State" = c("Connecticut", "Maine", "Massachusetts", 
                                  "New Hampshire", "Rhode Island", "Vermont", 
                                  "New Jersey", "New York", "Pennsylvania", "Indiana", 
                                  "Illinois", "Michigan", "Ohio", "Wisconsin", "Iowa", 
                                  "Kansas", "Minnesota", "Missouri", "Nebraska", 
                                  "North Dakota", "South Dakota", "Delaware", 
                                  "District of Columbia", "Florida", "Georgia", 
                                  "Maryland", "North Carolina", "South Carolina", 
                                  "Virginia", "West Virginia", "Alabama", "Kentucky", 
                                  "Mississippi", "Tennessee", "Arkansas", "Louisiana", 
                                  "Oklahoma", "Texas", "Arizona", "Colorado", "Idaho", 
                                  "New Mexico", "Montana", "Utah", "Nevada", "Wyoming", 
                                  "Alaska", "California", "Hawaii", "Oregon", "Washington"),
             "Region" = c(rep("Northeast Region", 9), rep("Midwest Region", 12), 
                          rep("South Region", 17), rep("West Region", 13)),
             "Division" = c(rep("New England", 6), rep("Middle Atlantic", 3),
                            rep("East North Central", 5), 
                            rep("West North Central", 7),
                            rep("South Atlantic", 9), rep("East South Central", 4), 
                            rep("West South Central", 4), rep("Mountain", 8), 
                            rep("Pacific", 5)) )


## To generate the vaccination counts by U.S. region an division we need to
## use the daily counts. We can then re-generate cumulative values from
## the daily counts and row-add this back into the main data set.

# Remove the regions that are not represented in the us_regions_divisions
# reference table.
filtered_search_space <- df_byWeek[df_byWeek$Province_State %in% us_regions_divisions$Province_State, 
                                   c(1:2, which(str_detect(colnames(df_byWeek), "Daily") ))] |>
  `rownames<-`(NULL)

# Add a U.S. divisions column by merging the two data sets by "Province_State".
added_regions <- merge(us_regions_divisions, filtered_search_space, by = c("Province_State"))


# Make aggregated sums by rows.
filtered_search_space <- rbind(
  filtered_search_space,
  # Sum over the regions by "Province_State" and "Week" to get their aggregate sums.
  aggregate(added_regions[, -c(1:4)], list(added_regions$Region, added_regions$Week), sum) |>
    `colnames<-`(c("Province_State", "Week", colnames(added_regions)[c(5:6)])),
  # Sum over the divisions by "Province_State" and "Week" to get their aggregate sums.
  aggregate(added_regions[, -c(1:4)], list(added_regions$Division, added_regions$Week), sum) |>
    `colnames<-`(c("Province_State", "Week", colnames(added_regions)[c(5:6)]))
)


# The ordered search space to generate counts.
querry <- c(unique(us_regions_divisions$Region)[c(1, 2, 4, 3)],
            unique(us_regions_divisions$Division))

## For convenience, we'll order the rows by granular regions, starting
## from the complete United States to regions to states/territories.
order_by <- c("United States", querry,
              sort(c(datasets::state.name, "District of Columbia")), 
              c("American Samoa", "Guam", "Northern Mariana Islands", 
                "Puerto Rico", "Virgin Islands, U.S."))


# Construct the cumulative sums over the span of dates for each "Province_State"
build <- c()
for(i in 1:length(querry)){
  # Add new cumulative calculations row-wise to the previous build iteration.
  build <- rbind(build,
                 # Subset rows for one "Province_State".
                 filtered_search_space[filtered_search_space$Province_State == querry[i], ] %>% 
                   # The following four lines generate the cumulative sums and add them
                   # as a new column to the data set.
                   mutate(Deaths_yf = cumsum(Deaths_Daily) ) |>
                   mutate(Confirmed_Cases_yf = cumsum(Confirmed_Cases_Daily))
  )
}


# Confirm the column name orders are correct before rejoining the newly
# constructed data back into the main df_byWeek.
all(colnames(build)[c(1:2, 5:6, 3:4)] %in% colnames(df_byWeek))

# Commit the changes.
df <- bind_rows(build[, c(1:2, 5:6, 3:4)], df_byWeek) |>
  select(-Deaths, -Confirmed_Cases)


# Confirm that the different state regions and divisions. Boolean test will
# say TRUE if this is completed.
unique(census_2010to2023$NAME) |>
  (\(x) { x[x %!in% unique(df$Province_State)] }) () |>
  (\(y) { length(y) == 0 }) ()


## Now we'll normalize the vaccination daily counts by the population estimates
## for that year to get the percent of the population that was vaccinated.

# Store the column names we are interested in by cumulative and daily counts.
cumulative_counts <- colnames(df)[3:4]
daily_counts      <- colnames(df)[5:6]

result = list()
for (i in 1:length(subset_census$NAME)) {
  # Separate out the rows associated with one "Province_State" entry.
  subset <- df |> 
    (\(x) { x[x$Province_State %in% subset_census$NAME[i], ] }) ()
  
  # Collect the year that the observation was made.
  staged_dates <- subset[, "Week"] %>% year()
  
  relevant_pop_est = c()
  for (j in 1:length(staged_dates)) {
    # Create a vector with the appropriate intercensal year population estimates
    # that matches the staged_dates vector entry-by-entry.
    relevant_pop_est[j] <- subset_census[i, str_detect(colnames(subset_census), as.character(staged_dates[j]))]
  }
  
  # Normalize over all columns for vaccination daily counts.
  population  <- relevant_pop_est |> unlist()
  percentages <- subset[, daily_counts] |>
    (\(y) { 
      sapply(y, function(x) {
        round((x / population) * 100, 0)
      }) 
    }) ()
  
  # Save the results and join them with the metadata columns for merging 
  # back to the main data set.
  result[[i]] <- cbind(subset[, c("Province_State", "Week")], population, percentages)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the percentage results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) |>
  as.data.frame() |>
  `colnames<-`(c("Province_State", "Week", "Population_Estimate", str_c(daily_counts, "_Percent")))


# Merge the previous data set with the normalized daily counts, combining by
# unique matches with the "Month" and "Province_State" columns.
df_total <- merge(df, result, by = c("Province_State", "Week"))


# We will reorder the dates so that they are in ascending order.
df_total <- df_total |>
  select(colnames(df_total)[c(1:2)], "Population_Estimate", colnames(df_total)[c(3:6, 8:9)]) |>
  group_by(Province_State) |>
  arrange(Week, .by_group = TRUE) |>
  ungroup()


## For convenience, we'll also order the rows by granular regions, starting
## from the complete United States to regions to states/territories.
order_by <- c("United States", querry,
              sort(c(datasets::state.name, "District of Columbia")), 
              c("American Samoa", "Guam", "Northern Mariana Islands", 
                "Puerto Rico", "Virgin Islands, U.S."))


build <- c()
for (i in 1:length(order_by)) {
  # Iterate ordering over the U.S. states, territories, and regions.
  ordered <- df_total[df_total$Province_State %in% order_by[i], ]
  
  # Add to the previous data set to commit newly ordered row entries.
  build <- rbind(build, ordered)
}

# Commit the full, row organized data set and clear the row names.
df_total <- build |> `rownames<-`(NULL)


# In our following analysis, it will be useful to report the dates in the
# standard epidemiologic year format: MMWR.
epiDates <- df_total[!is.na(df_total$Week), ] |> 
  (\(x) { cbind(x, MMWRweek(x$Week)) }) () |> 
  mutate(MMWRyear = MMWRyear, 
         MMWRyear = if_else(MMWRweek <= 26, MMWRyear - 1 , MMWRyear),
         MMWRweek = if_else(MMWRweek <= 26, MMWRweek + 52, MMWRweek),
         MMWRweek = MMWRweek - 26
  ) 

# Integrate these new columns into the full dataset.
df_final <- left_join(df_total, epiDates) |>
  # Reorder the columns for clarity.
  select(`Province_State`, Week, MMWRyear, MMWRweek, MMWRday, 
         colnames(df_total)[c(3:9)])




## ----------------------------------------------------------------------------
## SAVE CLEANED DATA FOR ANALYSIS

## Now that basic Extract, Transforming, and Loading (ETL) has been completed,
## and we understand the contents of our data set a little better, we are 
## ready to proceed with plotting.
## 
## For brevity in the tutorial, plotting the cleaned and aggregated data
## will be recorded here and an appended description of what was done
## will be discussed in the tutorial itself.

write.csv(df_final, "Git and GitHub/Deaths and Cases Aggregated by Week.csv", row.names = FALSE)








