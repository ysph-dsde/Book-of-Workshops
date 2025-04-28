## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
## 
## Workshop: Getting Started with Git and GitHub
##  Authors: Shelby Golden, M.S.
##     Date: 2024-10-11
## 
##    R version: 4.4.3
## renv version: 1.0.11
## 
## 
## Description: This script imports the Johns Hopkins University Coronavirus
##              Resource Center (JHU CRC) vaccination data for U.S. states
##              and territories, and pre-processes it for visualization.
##              The data was assessed for irregularities and errors (i.e.
##              duplicated entries and NA's). The cumulative counts were
##              smoothed to be fully monotonically increasing before
##              calculating the daily vaccination counts.

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
  library("lubridate")  # Facilitates working with dates and times
  library("MMWRweek")   # Convert Dates to MMWR day, week, and year
  library("stats")      # Compilation of statistical functions
  library("ggplot2")    # For creating static visualizations
  library("scales")     # Override default ggplot2 axes and legend settings
})


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))

## Read in custom functions used in this script.
source("Git-and-GitHub/R/Population Estimates and Projections/Functions.R")




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository managed by the Center for 
## Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU), 
## GitHub GovEX. Additional details can be found in the project GitHub repo's 
## main directory README file.

## We load it in directly from the JHU CRC's GitHub page using the raw URL.
df.url <- "https://raw.githubusercontent.com/govex/COVID-19/refs/heads/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv"
df     <- read_csv(file = df.url, show_col_types = FALSE) |>
  as.data.frame()

# glimpse() allows us to see the dimensions of the dataset, column names,
# the first few entries, and the vector class in one view.
df |> glimpse()


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
  "Git-and-GitHub/Data/US_Census Population Estimates_2010 to 2023.csv",
  header = TRUE)




## ----------------------------------------------------------------------------
## DATA PREPARATION

## Check the variable class of each column.
## NOTE: sapply() works better than apply() in this scenario.
sapply(df, class) |> as.data.frame()


## Format some of the column names.
colnames(df)[5:8] <- c("Doses", "People_At_Least_One_Dose", 
                       "People_Fully_Vaccinated", "Total_Additional_Doses")


## The variable classes look as we'd expect. Now we'll check for missing values.
sapply(df, function(x) sum(is.na(x))) |>
  as.data.frame() |>
  `colnames<-`("NA Count")


## There are a large number of NA's associated with the "Province_State" variable.
na_province  = df[df$Province_State %in% NA, ] 

na_province     |> head()
na_province$UID |> unique()


## UID is the Unique Identifier for each row entry, and we see that all NA's
## are associated with the same unique row entry. Based on JHU CRC's
## UID_ISO_FIPS_LookUp_Table.csv, we see that this UID corresponds to the U.S.
## total counts.
## 
## Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data

df[df$Province_State %in% NA, "Province_State"] <- "United States"


## There are 50 states, 5 major territories, 1 district, and 1 total metric.
unique_provinces = df$Province_State %>% unique()
length(unique_provinces)


## We have an excess of 5 rows, which are not states or territories.
## What are they?
us_states       = c(datasets::state.name, "District of Columbia") |> sort()
us_territories  = c("American Samoa", "Guam", "Northern Mariana Islands", 
                    "Puerto Rico", "Virgin Islands")

extra_provinces = unique_provinces |>
  (\(x) { x[x %!in% c("United States", us_states, us_territories)] }) ()

  extra_provinces
## NOTE: Some of these extra types of provinces will double count individuals
## that are also represented in the state/territory-level classification.

# Remove the extraneous provinces. These are not used in our analysis.
df <- df[df$Province_State %!in% extra_provinces, ]


## For clarity, we'll reorder the columns so that columns for doses administered
## and columns for people vaccinated are grouped together.
df <- df |>
  select(colnames(df)[c(1:5, 8, 6:7)])

## Now we'll see how the data plots. The GovEX README file for the data set
## says that reported vaccinations are cumulative. Therefore, we expect the
## plots to appear monotonic.
df |>
  filter(Province_State %in% us_states) |>
  ggplot(data = _, aes(x = Date, y = People_At_Least_One_Dose)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Raw Data Line Plot",
           x = "Daily Updates", y = "People With at Least One Dose") +
      theme_minimal() + theme(legend.position = "none")




# -----------------------------
# CORRECT MONOTONICITY

## It appears that some state/territories cumulative counts are purely monotonic, 
## but some appear to have some irregularities. These irregularities seem to be
## effecting a limited range of dates, and are not wide-spread nor do they show
## a dramatic departure from what we expect.
## 
## We can double check the accuracy of our qualitative assessment with the 
## following loop. This will assess if the data for each "Province_State" (row)
## and vaccine counting type (columns) pairs are monotonically increasing over
## the whole range of dates. TRUE means that it is and FALSE means that there is
## at least one sequence of dates that is not monotonically increasing.

# Store the column names that we will be processed for smoothing.
make_daily_counts <- colnames(df[, -c(1:4)])

monotonic_result <- list()
for (i in 1:length(make_daily_counts)) {
  check <- list()
  for (j in 1:length(unique(df$Province_State))) {
    # Separate out the vector associated with one "Province_State" entry
    # and one vaccination count method (column).
    subset <- df[df$Province_State %in% unique(df$Province_State)[j], make_daily_counts[i]]
    
    # A monotonically increasing range of dates will always have positive
    # successive differences, including zero to reflect no change.
    check[[j]] <- all(diff(subset) >= 0)
  }
  
  # Combine the results (list format) into one data frame by adding rows.
  # Each row reflects the results for one "Province_State".
  monotonic_result[[i]] <- do.call(rbind, check) |> 
    `rownames<-`(unique(df$Province_State))
}
# Combine the monotonic result (list format) into one data frame by adding
# columns. Each column reflects the results for one vaccination count method.
monotonic_result <- do.call(cbind, monotonic_result) |>
  `colnames<-`(make_daily_counts) |> 
  as.data.frame()

monotonic_result


# Table how many states/territories are not monotonically increasing by the 
# vaccination count method out of a total of 57 "Province_State" variables.
sapply(monotonic_result, function(x)
  length(x) - sum(x, na.rm = TRUE)) |> 
  as.data.frame() |>
  `colnames<-`("State/Territories Not Monotonic")

# Table how many states/territories have monotonically increasing vaccine counts
# for the full range of dates reported across all four vaccine count methods.
apply(monotonic_result, 1, function(x) all(x)) |> table()


## Unlike what we would expect, not all of the entries are monotonically
## increasing. Only 10 "Province_State" entries are for all four 
## vaccine count methods.
## 
## One source of this problem could be duplicate entries. We start checking for
## duplicates or other obvious data entry errors.

result <- list()
for (i in 1:length(unique(df$Province_State))) {
  # Separate out the dates vector associated with one "Province_State" entry.
  subset <- df[df$Province_State %in% unique(df$Province_State)[i], "Date"]
  
  # Store the date ranges that occur more than once.
  result[[i]] <- table(subset)[table(subset) %!in% 1]
}
# Convert the results into a vector and report only the unique values.
lapply(result, names) %>% do.call(c, .) |> unique()


## Looks like the only entry that was duplicated is associated with 2022-06-17.
## We will check the entries are different.

# Filter out the dates so only the duplicate entries are shown and excess
# metadata columns are removed.
subset <- df[df$Date == "2022-06-17", -c(2, 4)]
for (i in 1:length(unique(df$Province_State))) {
  # Iterate the row-uniqueness test over each possible "Province_State"
  check_matched_row_entries(subset, "Province_State", unique(df$Province_State)[i])
}


## Duplicate entries report the same information. We will remove these by
## arbitrarily selecting one of the duplicates for retention.
df <- df[!duplicated(df), ]

df |>
  filter(Province_State %in% us_states) |>
  ggplot(data = _, aes(x = Date, y = People_At_Least_One_Dose)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Raw Data Line Plot - Duplicate Dates Removed",
           x = "Daily Updates", y = "People With at Least One Dose") +
      theme_minimal() + theme(legend.position = "none")


## Removing duplicated dates did not seem to help reduce the observed
## irregularities. Now we'll confirm that the dates are evenly spaced, with
## entries added each day.
## 
## The following test will check for the number of days between successive
## dates and test the value against zero days, one day, or any length not
## zero or one. Results are aggregated for the entire span of dates by
## "Province_State" entries. A passed test will only show TRUE under the
## "Equal 1" column and FALSE for the other two.

result <- list()
for (i in 1:length(unique(df$Province_State))) {
  # Separate out the dates vector associated with one "Province_State" entry.
  subset <- df[df$Province_State %in% unique(df$Province_State)[i], "Date"]
  
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


## Looks like they are all spaced by one day, which is what we are looking for.
## The problem seems to be a deeper issue than obvious data-entry errors that
## can be reconciled with the information we have on hand. In the GovEX GitHub, 
## it says the a states vaccination reports were compared with the CDC's Vaccine 
## Tracker report. If they differed, the larger of the two values was saved.
## It is possible that on a few occasions there was an error when recording the 
## largest of the two reported values or that some daily updates were corrected
## to show less vaccinations from a previous date.
##
## To compensate for these blips, we will use an isotonic (monotonically 
## increasing) least squares regression to smooth the data. The function
## that will be used is isoreg() from the stats package. 


## Recall the results from above.
all_col_monotonic <- apply(monotonic_result, 1, function(x)
  all(x)) |> as.data.frame() |> `colnames<-`("All Monotonic")

monotonic_result
all_col_monotonic


## First we'll check to confirm the algorithm does a decently good job
## predicting entries we know are monotonically increasing for all vaccine
## counting methods. Outcomes equal to TRUE imply that the predictions are 
## exactly equal with the reported value.

# Extract which "Province_State" entries were monotonically increasing
# for all vaccine counting methods.
test_ir <- row.names(all_col_monotonic)[all_col_monotonic$`All Monotonic` == TRUE]

result = list()
for (i in 1:length(test_ir)) {
  # Subset the data set for one "Province_State" entry, restricting the
  # search space to only those that were monotonically increasing.
  subset <- df[df$Province_State %in% test_ir[i], ]
  
  check_counts = c()
  for (j in 1:length(make_daily_counts)) {
    # Filter out the vaccine count method vector and fit using isoreg().
    ir <- isoreg(subset[, make_daily_counts[j]], y = NULL)
    
    # Check the difference between the reported value and the "yf" prediction.
    # This is an all-or-nothing Boolean test. NOTE: In "yf" "f" means "fitted" 
    # and "y" denote the prediction based on the isotonic fitting results.
    check_counts[j] <- all(subset[, make_daily_counts[j]] - ir$yf == 0)
  }
  result[[i]] <- check_counts
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) |> `colnames<-`(make_daily_counts) |>
  `rownames<-`(test_ir) |> as.data.frame()

result


## All entries returned TRUE, which is a good sign we can proceed with the 
## isotonic regression method of data smoothing for all of the data.

result = list()
for (i in 1:length(unique(df$Province_State))) {
  # Subset the data set for one "Province_State" entry. This time the
  # search space is not restricted.
  subset <- df[df$Province_State %in% unique(df$Province_State)[i], ]
  
  ir_yf = list()
  for (j in 1:length(make_daily_counts)) {
    # Filter out the vaccine count method vector and fit using isoreg().
    ir <- isoreg(subset[, make_daily_counts[j]], y = NULL)
    
    # Save the fitting results predicted y-values. NOTE: In "yf" "f" means
    # "fitted"and "y" denote the prediction based on the isotonic fitting
    # results.
    ir_yf[[j]] <- ir$yf
  }
  
  # Combine the results (list format) into a data frame by adding columns. Each
  # new column represents a different vaccination counting method.
  dataTable <- do.call(cbind, ir_yf) |>
    `colnames<-`(str_c(make_daily_counts, "_yf")) |> 
    as.data.frame()
  
  # Separate out the "Date" and "Province_State" columns for the vector of
  # newly fitted results. Everything should be in the same row-order and not
  # require matching to merge. These columns will be used to merge the fitted
  # values back to the main data set.
  rowNames  <- df[df$Province_State %in% unique(df$Province_State)[i], c("Date", "Province_State")]
  
  # Combine the metadata (rowNames) to the built vector of fitted vaccination
  # counts (dataTable).
  result[[i]] <- cbind(rowNames, dataTable)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) |>
  `colnames<-`(c("Date", "Province_State", str_c(make_daily_counts, "_yf"))) |>
  as.data.frame()

# Merge the previous data set with the newly smoothed values, combining by
# unique matches with the "Date" and "Province_State" columns.
df_monotonic <- merge(df, result, by = c("Date", "Province_State"))


## If we plot the same data using the smoothed regression results, we see that we
## indeed have our monotonic data.
df_monotonic |>
  filter(Province_State %in% us_states) |>
  ggplot(data = _, aes(x = Date, y = People_At_Least_One_Dose_yf)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Smoothed Data Line Plot - Isotonic `yf` Prediction",
           x = "Daily Updates", y = "People With at Least One Dose `yf`") +
      theme_minimal() + theme(legend.position = "none")




# -----------------------------
# BACK CALCULATE DAILY COUNTS

## Now that we have our smoothed, monotonic data we can proceed with generating
## the daily vaccination counts from the cumulative sums without having to worry
## introducing negative values.

# Store the column names reflecting the smoothed, monotonically increasing
# fittings for calculating the daily counts from the cumulative sum.
make_daily_counts_yf <- colnames(df_monotonic[, -c(1:4)]) |>
  (\(x) { x[str_detect(x, "yf")] }) ()

result = list()
for (i in 1:length(unique(df_monotonic$Province_State))) {
  # Subset the data set one for "Province_State" entry.
  subset <- df_monotonic[df_monotonic$Province_State %in% unique(df$Province_State)[i], ]
  
  counts = list()
  for (j in 1:length(make_daily_counts_yf)) {
    # Separate out the vector associated with one vaccination counting method
    # and calculate the difference between the ith and i+1 value. Use the
    # first value in the original vector as the first value in the new vector,
    # and shift difference counts up one position.
    counts[[j]] <- c(subset[, make_daily_counts_yf[j]][1], diff(subset[, make_daily_counts_yf[j]]))
  }
  
  # Combine the results (list format) into a data frame by adding columns. Each
  # column represents the vaccine counting method. Also join the metadata
  # columns to this data set for merging daily counts back to the main data set.
  result[[i]] <- do.call(cbind, counts) |> 
    as.data.frame() |>
    (\(x) { cbind(subset[, c("Date", "Province_State")], x) }) () |>
    `colnames<-`(c("Date", "Province_State", str_c(make_daily_counts_yf, "_Daily")))
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Province_State" and each column
# represents the vaccine counting method.
result   <- do.call(rbind, result)

# Merge the previous data set with the daily counts, combining by unique
# matches with the "Date" and "Province_State" columns.
df_daily <- merge(df_monotonic, result, by = c("Date", "Province_State"))


## We can double check that this calculation worked by plotting the calculated
## daily count values.
df_daily |>
  filter(Province_State %in% us_states[c(1, 4, 20)]) |>
  ggplot(data = _, aes(x = Date, y = Doses_yf_Daily)) +
      geom_bar(stat = "identity", alpha = 0.25, 
               aes(color = Province_State, fill = Province_State)) +
      #geom_line(aes(color = Province_State, fill = Province_State)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Daily Counts Bar Plot - Calculated from `yf`\nFor Alabama, Arkansas, and Maine", 
           x = "Daily Updates", y = "Daily Administration of Vaccines `yf`") +
      theme_minimal()


## Looks good. Now that we have our monotonic smoothed data and back-calculated
## daily counts, we are ready to proceed with finding other opportunities to
## clean or transform the data in preparation for analysis.
## 
## We'll check to confirm that each "Province_State" entry covers the same
## number of days. The Boolean test is TRUE when all provinces have the same
## number of time-stamped entries.

table(df_daily$Province_State, df_daily$Date) |>
  sapply(., function(x) x == 1) |> all()




## ----------------------------------------------------------------------------
## DATA CLEANING

# -----------------------------
# AGGREGATE BY WEEK

## We see that not all states/territories have the same number of time-stamped
## entries. Vaccination reports are also not expected to be precise to the
## degree where daily reported updates are meaningful (within error).
## 
## Therefore, we will aggregate "Date" by week for each "Province_State". After
## this, we no longer need columns "Date", "UID", or "Country_Region", and so
## we'll remove them from our aggregated data set. Because the raw values
## were not monotonic we will only keep the isotonic regression smoothed values.

# Create a new vector that rounds "Dates" down to the nearest month.
df_daily <- df_daily |>
  group_by(week = lubridate::floor_date(Date, "week")) |> as.data.frame()

# In the aggregation, retain only the max value of the cumulative counts.
# Organize aggregation by unique "Province_State" entries and dates
# rounded down to the nearest month.
maxCumulative <- aggregate(. ~ week + Province_State, df_daily[, -c(1, 3:8, 13:16)], max) |>
  `colnames<-`(c("Week", colnames(df_daily)[c(2, 9:12)]))

# Do the same aggregation over the daily counts, but retain the sum.
sumDaily <- aggregate(. ~ week + Province_State, df_daily[, -c(1, 3:12)], sum) |>
  `colnames<-`(c("Week", colnames(df_daily)[c(2, 13:16)]))

# Merge these two aggregated data sets, combining by unique matches with the
# "Date" and "Province_State" columns.
df_byWeek <- merge(maxCumulative, sumDaily, by = c("Week", "Province_State"))


## Good sanity check that the rows are aggregated as expected.
head(df_byWeek)
dim(df_byWeek)

# Plot the aggregated cumulative vaccination counts to confirm it retained
# the monotonically increasing feature.
df_byWeek |>
  filter(Province_State %in% us_states) |>
  ggplot(data = _, aes(x = Week, y = People_At_Least_One_Dose_yf)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Aggregated Data Line Plot\nMax of Cumulative Sum",
           x = "Weekly Updates", y = "People With at Least One Dose `yf`") +
      theme_minimal() + theme(legend.position = "none")


# Plot the daily vaccination counts, to confirm there are no unexpected features.
df_byWeek |>
  filter(Province_State %in% us_states) |>
  ggplot(data = _, aes(x = Week, y = Doses_yf_Daily)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Aggregated Data Line Plot\nSum of Daily Counts",
           x = "Weekly Updates", y = "Daily Doses Administered `yf`") +
      theme_minimal() + theme(legend.position = "none")


# Boolean test is TRUE when all provinces have the same number of weeks
table(df_byWeek$Province_State, df_byWeek$Week) |>
  (\(x) { sapply(x, function(x) x == 1) }) () |> 
  all()


## Looks like the aggregation step went as expected. In the last few steps we
## will organize and clean up the data. 




# -----------------------------
# ORGANIZE AND ANNOTATE

## For convenience, we'll order the rows by "Province_State" and then by "Week".
order_by <- c("United States", us_states, us_territories)

# Start building ordered set with "Province_State" == "United States"
build <- df_byWeek[df_byWeek$Province_State %in% order_by[1] &
                     order(df_byWeek$Week), ]

for (i in 2:length(order_by)) {
  # Iterate the same initial process over the remaining ordered
  # "Province_State" row entries.
  ordered <- df_byWeek[df_byWeek$Province_State %in% order_by[i] &
                         order(df_byWeek$Week), ]
  
  # Add to the previous data set to commit newly ordered row entries.
  build   <- rbind(build, ordered)
}

# Commit the full, row organized data set.
df_byWeek <- build |>
  `rownames<-`(NULL)


# Now we will round down all numeric values to the nearest whole integer (i.e.
# the nearest person).
df_byWeek[, -c(1:2)] <- sapply(df_byWeek[, -c(1:2)], floor)




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




## ----------------------------------------------------------------------------
## ADD MISSING REGIONS AND DIVISIONS TO MAIN DATA SET

## We can generate those using the regional and division map provided by
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
    `colnames<-`(c("Province_State", "Week", colnames(added_regions)[c(5:8)])),
  # Sum over the divisions by "Province_State" and "Week" to get their aggregate sums.
  aggregate(added_regions[, -c(1:4)], list(added_regions$Division, added_regions$Week), sum) |>
    `colnames<-`(c("Province_State", "Week", colnames(added_regions)[c(5:8)]))
)

# We can confirm that we now have all of the regional sums. Only the territories
# and total U.S. counts are showing up, which is good because we used a filtered
# data set to generate our aggregates.
unique(census_2010to2023$NAME) |>
  (\(x) { x[x %!in% unique(filtered_search_space$Province_State)] }) ()

# The ordered search space to generate counts.
querry <- c(unique(us_regions_divisions$Region)[c(1, 2, 4, 3)],
            unique(us_regions_divisions$Division))

# Construct the cumulative sums over the span of dates for each "Province_State"
build <- c()
for(i in 1:length(querry)){
  # Add new cumulative calculations row-wise to the previous build iteration.
  build <- rbind(build,
                 # Subset rows for one "Province_State".
                 filtered_search_space[filtered_search_space$Province_State == querry[i], ] %>% 
                   # The following four lines generate the cumulative sums and add them
                   # as a new column to the data set.
                   mutate(Doses_yf = cumsum(Doses_yf_Daily) ) |>
                   mutate(People_At_Least_One_Dose_yf = cumsum(People_At_Least_One_Dose_yf_Daily)) |>
                   mutate(People_Fully_Vaccinated_yf = cumsum(People_Fully_Vaccinated_yf_Daily)) |>
                   mutate(Total_Additional_Doses_yf = cumsum(Total_Additional_Doses_yf_Daily))
  )
}

# Confirm the column name orders are correct before rejoining the newly
# constructed data back into the main df_byWeek.
all(colnames(build[, c(2:1, 7, 10, 8:9, 3:6)]) %in% colnames(df_byWeek))

# Commit the changes.
df <- bind_rows(build[, c(2:1, 7, 10, 8:9, 3:6)], df_byWeek)

# Confirm that the different state regions and divisions. Boolean test will
# say TRUE if this is completed.
unique(census_2010to2023$NAME) |>
  (\(x) { x[x %!in% unique(df$Province_State)] }) () |>
  (\(y) { length(y) == 0 }) ()


## Now we'll normalize the vaccination daily counts by the population estimates
## for that year to get the percent of the population that was vaccinated.

# Store the column names we are interested in by cumulative and daily counts.
cumulative_counts <- colnames(df)[3:6]
daily_counts      <- colnames(df)[7:10]

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
  select(colnames(df_total)[c(1:2)], "Population_Estimate", colnames(df_total)[c(3:10, 12:15)]) |>
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
         colnames(df_total)[c(3:15)])




## ----------------------------------------------------------------------------
## CUMULATIVE VACCINATION TO SEPCIFIED DATE

## Say we want to plot the changes in percent vaccinated over time. We
## need to generate cumulative vaccination counts to specific dates
## and normalize based on the projected population was that year. This will
## require creating a new data set that spans discrete years instead of months.
##
## To make our life easier, we can extract the cumulative counts to specific
## points of time for each unique "Province_State" entry. As a sanity check,
## we will confirm that each "Province_State" has the same dates.

# Search space for the sub setting is by unique "Province_State" entries.
search_space = df_final$Province_State %>% unique()
# Pull the unique months detected in the whole data set.
unique_weeks = df_final$Week %>% unique()
# Extract out the cumulative counts. These are columns that do not have
# "..._daily" or "..._percent" in their column names.
subset_cum_counts = df_final[, c(1:2, which(str_detect(colnames(df_final), "_yf\\b")))] %>%
  as.data.frame()


result = c()
# Boolean test that each "Province_State" subset has the same months. Everything
# should be ordered in increasing dates, and so we do an exact match test.
for (i in 1:length(search_space)) {
  # Extract the months listed for each "Province_State" and test for exact match
  # with the reference unique_months vector.
  test <- subset_cum_counts |>
    (\(x) { x[x$Province_State %in% search_space[1], "Week"] == unique_weeks }) ()
  
  result[i] <- test |> all()
  
}
# Organize the results into a data frame. Each row reflects the Boolean test
# results for one "Province_State".
as.data.frame(result) |> `rownames<-`(search_space) |> `colnames<-`("Same Dates?")


## Looks like all results are true. Therefore, we will take the max month
## listed under each year and save the row result that matches that date.

data_range = str_c("20", c("20", "21", "22", "23"))

build = list()
for (i in 1:length(data_range)) {
  querry     <- data.frame("Weeks" = unique_weeks, "Year" = year(unique_weeks)) |>
    (\(x) { x[x$"Year" %in% data_range[i], "Weeks"] }) () |> max()
  
  build[[i]] <- subset_cum_counts |>
    (\(x) { x[x$Week %in% querry, ] }) ()
}
# Combine the results (list format) into a data frame by adding rows. Each
# list element includes the subset of our data set by the max month in
# each year.
df_cumulative <- do.call(rbind, build) |> `rownames<-`(NULL)


## Now we'll normalize the vaccination daily counts by the population estimates
## for that year to get the percent of the population that was vaccinated.

result = list()
for (i in 1:length(subset_census$NAME)) {
  # Separate out the rows associated with one "Province_State" entry.
  subset <- df_cumulative |>
    (\(x) { x[x$Province_State %in% subset_census$NAME[i], ] }) ()
  
  # Collect the year that the observation was made.
  staged_dates <- subset[, "Week"] |> year()
  
  relevant_pop_est = c()
  for (j in 1:length(staged_dates)) {
    # Create a vector with the appropriate intercensal year population estimates
    # that matches the staged_dates vector entry-by-entry.
    relevant_pop_est[j] <- subset_census[i, str_detect(colnames(subset_census), as.character(staged_dates[j]))]
  }
  
  # Normalize over all columns for vaccination daily counts.
  percentages <- subset[, str_detect(colnames(subset), "_yf")] |>
    (\(y) { sapply(y, function(x){
      round((x / relevant_pop_est) * 100, 0)
    }) }) ()
  
  # Save the results and join them with the metadata columns for merging
  # back to the main data set.
  result[[i]] <- cbind(subset[, c("Province_State", "Week")], percentages)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the percentage results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) |> `colnames<-`(c("Province_State", "Week", str_c(colnames(subset)[str_detect(colnames(subset), "_yf")], "_Percent")))

# Merge the previous data set with the normalized daily counts, combining by
# unique matches with the "Month" and "Province_State" columns.
df_cumulative <- merge(df_cumulative, result, by = c("Province_State", "Week"))


## Because the raw data is imperfect, it is possible that cumulative counts will
## reflect over 100% vaccinated. We can check which columns have this issue.

sapply(df_cumulative[, str_detect(colnames(df_cumulative), "Percent")], function(x)
  any(x > 100)) |> as.data.frame() |> `colnames<-`("Over 100%?")


## Fix the values that are over 100% vaccinated and reset to 100%.
df_cumulative$Doses_yf_Percent[df_cumulative$Doses_yf_Percent > 100] <- 100
df_cumulative$People_At_Least_One_Dose_yf_Percent[df_cumulative$People_At_Least_One_Dose_yf_Percent > 100] <- 100




## ----------------------------------------------------------------------------
## SAVE CLEANED DATA FOR ANALYSIS

## Now that basic Extract, Transforming, and Loading (ETL) has been completed,
## and we understand the contents of our data set a little better, we are 
## ready to proceed with plotting.
## 
## For brevity in the tutorial, plotting the cleaned and aggregated data
## will be recorded here and an appended description of what was done
## will be discussed in the tutorial itself.


write.csv(df_final, "Git-and-GitHub/Data/Vaccinations Aggregated by Week.csv")
write.csv(df_cumulative, "Git-and-GitHub/Data/Vaccinations Aggregated by Week_Annual Cumulative.csv")




