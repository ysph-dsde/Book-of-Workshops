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




## ----------------------------------------------------------------------------
## DATA PREPARATION

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
## implied or not relevant. For example, UID, or the unique identifier for each 
## row entry, no longer has meaning or contain information we need after 
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
                         "Combined_Key", "Date", "Deaths_Count_Cumulative")


## We would like to see state- and country-level counts. Currently, the
## data set only reports county-level counts. We calculate these values by
## summing the cumulative counts over entries that have been grouped by
## "Province_State" and "Date". Then, we do the same operation over all of
## the U.S. state entries that have been grouped by "Date" only.
## 
## First we start by calculating the state-level data from the county-level data.

counts_by_state <- df_subset |> 
  # Step 1: Groups the table by unique entries in "Province_State" followed 
  #         by "Date".
  group_by(Province_State, Date) |>
  # Step 2: Summing "Deaths_Count_Cumulative" over the grouped rows. Note that 
  #         .groups = "keep" will maintain the grouping for the summation.
  summarise(Deaths_Count_Cumulative = sum(Deaths_Count_Cumulative), .groups = "keep") |>
  # Step 3: Mutate will generate new columns. These can be functions of existing 
  #         columns or static operations. For example, we can generate a new
  #         "Combined_Key" for the state-level data that excludes counties 
  #         using the stringr concatenate function, str_c().
  mutate(Country_Region = "US", Combined_Key = str_c(Province_State, ", US")) |> 
  # Step 4: Remove grouping to work with the full data frame again.
  ungroup()


head(counts_by_state)
dim(counts_by_state)


## Next we calculate the country-level data by summing over all of the U.S.
## state entries.

counts_by_country <- counts_by_state |> 
  # Step 1: Filter subsets the data set by rows that match the condition. This 
  #         will subset the data set for entries that are U.S. states including
  #         the District of Columbia.
  filter(Province_State %in% c(datasets::state.name, "District of Columbia")) |>
  # Step 2: Groups the table by unique entries in "Date".
  group_by(Date) |>
  # Step 3: Calculate the cumulative deaths variable by summing over the 
  #         grouped rows.
  summarise(Deaths_Count_Cumulative = sum(Deaths_Count_Cumulative), .groups = "keep") |>
  # Step 4: Generate new columns using mutate.
  mutate(Country_Region = "US", Combined_Key = "US") |> 
  # Step 5: Remove grouping to work with the full data frame again
  ungroup()


head(counts_by_country)
dim(counts_by_country)


## Now that we have our state- and country-level data, we need to combine them
## back into the main data set. bind_rows() is a tidyverse function that is 
## similar to do.call(), but it will also fill missing columns with NA so that
## the maximum number of uniquely defined variables are maintained.

df <- bind_rows(counts_by_country, counts_by_state, df_subset) |> as.data.frame()


## We can confirm that this operation was successful by examining the first and
## last few rows.

head(df)
tail(df)




## ----------------------------------------------------------------------------
## STRINGR

## After completing bulk data wrangling operations, like the ones completed above,
## it is good practice to examine variable classifications and nomenclature
## before any calculations. For example, if you have a variable for the sex
## of a participant, you will want to confirm that all entries of that variable
## say "M" and "F" for Male and Female, respectively. You might also need to
## confirm that zero's are not being used in place of NAs when the value is
## not determined.
## 
## In this data set, we are going to assume the county-level entries are correct.
## Therefore, we only need to inspect "Province_State". We expect that the
## U.S. states and territories will be included. We can examine other entries
## by matching unique entries of "Province_State" to datasets::state.name.

unique(df$Province_State)[unique(df$Province_State) %!in% c(datasets::state.name, "District of Columbia")]


## Notice that there are "NAs". This should be correct, since the country-level
## counts are NA at the state-level. We can confirm this by showing all
## "Combined_Key" entries are "US" for rows with "Province_State" = NA.

df[df$Province_State %in% NA, "Combined_Key"] |> unique()


## In addition to the District of Columbia and five U.S. territories, there
## are entries for two cruise ships. These are not relevant to our analysis,
## and so we exclude them using str_detect() or str_which() to find strings
## with "Princess" in them. These two methods using stringr are shown:

# Option #1: Use the Boolean test that detects the "Princess" string.
# Note that the added " %in% TRUE" is used to address NA outcomes that need
# to be interpreted as FALSE.
df_filtered <- df[!str_detect(df$Province_State, "Princess") %in% TRUE, ]

# Option #2: Find the index that detects the "Princess" string and use the
#            indexes that do not contain that string to subset.
df_filtered <- df[-str_which(df$Province_State, "Princess"), ]

# Notice one could use str_which(df$Province_State, "Princess", negate = TRUE).
# However, because there are NAs in the "Province_State" vector the 
# version displayed above needs to be used to maintain those rows.


## Doing this removes the following number of rows from the larger data set.

nrow(df) - nrow(df_filtered)


## The "Combined_Key" variable combines information from "County",
## "Province_State", and "Country_Region". Earlier in code, we generated the
## new "Combined_Key" entries for the state- and country-level data using
## mutate() and str_c(). Assume we only have the county-level data and wish to
## generate the state- and country-level "Combined_Key" variable. We can do this
## in two ways: str_c() or str_split().
## 
## For this example we will subset the data by county-level information only. We
## use str_count() to represent the number of times a string pattern is detected
## within any given string. County-level data will have two commas, so this is
## one method we can use to subset our data.

df_county <- df_filtered[str_count(df_filtered$Combined_Key, ",") == 2, ]

# Option #1: Generate a new column by combining the desired columns with ", "
#            as the separator.
str_c(df_county$Province_State, df_county$Country_Region, sep = ", ") |> unique()

# Option #2: Split the string only to the first observation of the string match.
str_split(df_county$Combined_Key, ",", simplify = TRUE, n = 2)[, 2] |> 
  str_trim(side = "both") |> unique()

## While Option #2 works in principle, and is demonstrated here, we see that
## there are significant inconsistencies with formatting in the "Combined_Key"
## column. For example, some names have spaces between commas and some lack
## spaces, which is causing them to be recognized as different strings. We can 
## reconcile these problems by regenerating the "Combined_Key" for county-level 
## entries in the main data set using str_c().

# Identify the rows for county-level data where "Combined_Key" needs to be fixed.
index = which(str_count(df_filtered$Combined_Key, ",") == 2)

# Correct the county-level "Combined_Key" entries only with str_c().
df_filtered[index, "Combined_Key"] <- 
  str_c(df_filtered[index, "County"], df_filtered[index, "Province_State"], 
        df_filtered[index, "Country_Region"], sep = ", ")

# To confirm this worked, we can rerun the str_split() command and see that
# there are no more duplicated entries detected on account of formatting
# variations.
str_split(df_filtered$Combined_Key, ",", simplify = TRUE, n = 2)[, 2] |> 
  str_trim(side = "both") |> unique()


## Say we wish to specify that the Virgin Islands entries only reflect results
## for the U.S. territory. We can replace specific strings exactly using
## str_replace().
## 
## Going forward, we only require the "Combined_Key" variable, as the
## "Country_Region", "Province_State", and "County" variables are succinctly
## represented there. We can adjust the "Virgin Islands" entries for that 
## column only.

df_filtered[, "Combined_Key"] <- str_replace(df_filtered[, "Combined_Key"],  "Virgin Islands", "US Virgin Islands")


## Note that it is possible to adjust multiple columns at once. Notice that with
## the base pipe "|>" the standard placeholder "_" does not move information
## into the sapply() function. To fix this, we wrap the sapply() pipe level and
## specify a placeholder for the values being passed from the left-side
## to sapply(). In this scenario, we are calling that information "x". 
##
## sapply() is one of a few useful functions that repeats operations over 
## columns, rows, or lists. sapply() will only apply the function over a
## the columns of a data frame.

# Identify the row indices where "Virgin Islands" is detected in column
# "Province_State".
index = str_which(df_filtered[, c("Province_State")], "Virgin Islands")

df_filtered[index, c("Province_State", "Combined_Key")] |>
  # Define the wrapper of this pipe-level and specify the information from
  # the left to be "x".
  (\(x) {
    # Apply the str_replace() function over "Province_State" and "Combined_Key".
    # Notice that the added "^ " is a regex statement that defines a hard
    # boundary on the string.
    sapply(x, function(y) str_replace(y,  "^Virgin Islands", "US Virgin Islands"))
  }) () |> 
  # Show that the changes have been completed.
  _[1:15, ]




## ----------------------------------------------------------------------------
## FORMATTING AND CALCULATIONS

## Now that we have completed tidying our data, we can clean the columns by 
## removing redundant information and reorder them. With the "Combined_Key", 
## we no longer need "Country_Region",  "Province_State", or "County". Notice 
## that select() will also reorder our columns.

df <- df_filtered |> select(Combined_Key, Date, Deaths_Count_Cumulative)


## Finally, we will confirm that our variables are set to the correct class.

sapply(df, class)

## Currently, "Date" is classified as a character. We can change the class to 
## date using the lubridate package from tidyverse. This package is not covered
## in this introductory workshop, but those interested to find out more can
## review the package documentation: https://lubridate.tidyverse.org/
## 
## Notice that the data has been reported in mm/dd/yy format. Therefore, we
## use mdy() to correctly convert the character to the date class.

df$Date <- mdy(df$Date)


## We can also order the rows so that the data frame has all dates ascending
## for each unique "Combined_Key" entry. We will also ensure that the larger
## regions precede the smaller regional divisions: country then state then
## county.

for(i in 1:3) {
  # The intention is to keep rows organized in order of larger regions to smaller:
  # country- to state- to county-level. This can be achieved by ordering
  # the rows by data subsets at these levels of information.
  index = which(str_count(df$Combined_Key, ",") == i - 1)
  
  # arrange() is a dplyr function that orders the data frame by the selected
  # column(s). In this case, that is by "Combined_Key" followed by "Date".
  df[index, ] <- df[index, ] |> arrange(Combined_Key, Date)
}


## One critical assumption for cumulative counts is that they are monotonically
## increasing, which means that counts for subsequent dates should be at least
## as equal or greater in value than the previous one. We need to verify that
## this is the case before calculating our daily counts. 
## 
## In the following for loop, we are going to confirm that the date ranges
## for each unique region is represented once ("dates_match") and that the
## deaths cumulative counts are monotonically increasing ("boolean").

# Because the values calculated for the US ultimately constitutes all available
# date ranges in the data set, we can use this as a reference vector.
dates_reference <- df[df$Combined_Key %in% "US", "Date"]

# Now we want to confirm that every day between the first and final date where
# values were recorded is represented. If there are gaps, we will need to
# take this into consideration in our daily count back calculation.
all(diff(dates_reference) == 1)

# Now we define the search space for unique country-, state-, and county-level 
# entries.
unique_regions <- unique(df$Combined_Key)

# Define empty placeholder vectors that we will fill with results from the for loop.
boolean     <- c()
dates_match <- c()
# Define the loop range to cover the entire length of unique regions.
for(i in 1:length(unique_regions)) {
  # Subset the data frame so that only one unique regions is represented.
  subset          <- df[df$Combined_Key %in% unique_regions[i], ]
  
  # Notice that the structure of the following tests is only possible because
  # we have organized the rows so that the dates are ascending.
  
  # Check that the successive dates are greater or equal in value from the
  # previous value.
  boolean[i]     <- all(diff(subset$Deaths_Count_Cumulative) >= 0)
  # If all dates are represented as expected, then we expect an exact match
  # to our reference vector.
  dates_match[i] <- all(subset$Date == dates_reference)
}
# Compile the results into a data frame.
result <- data.frame("Region" = unique_regions, "Result" = boolean, 
                     "Dates_Match_Reference" = dates_match)


## Now we can check the results.

result[result$Result == TRUE, ]  |> nrow()
result[result$Result == FALSE, ] |> nrow()

## We see that over twice as many entries fail the monotonically increasing test
## compared to those that passed. The country- and state-level information is 
## more important than the county-level data. We can see how many of these
## passed or failed the same test.

result[result$Region %in% c("US", str_c(c(datasets::state.name, "District of Columbia"), ", US")), ]

## Unfortunately, we see that most state-level entries failed and the one value
## for the US also failed. For example, we can see in the plot below that there 
## are dates where deaths counts were not perfectly monotonically increasing 
## as expected.

df |>
  # Filter the data set for one specific "Combined_Key".
  filter(Combined_Key %in% "Oklahoma, US") |>
  ggplot(data = _, aes(x = Date, y = Deaths_Count_Cumulative)) +
  geom_line() +
  # Format the y-axis to show values in terms of thousands.
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
  # every four months.
  scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
  # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
  labs(title = "Deaths from COVID-19 in Oklahoma",
       x = "Month", y = "Cumulative Deaths Recorded") +
  # Graph displays as minimal with a legend.
  theme_minimal()


## Now we can check the second part of the test confirming if all expected 
## dates are represented.

result[result$Dates_Match_Reference == FALSE, ]

## Fortunately a very limited number of entries failed this part of the test.
## We can inspect what the source of this problem is by taking a closer look.

index = which(df$Combined_Key %in% result[result$Dates_Match_Reference == FALSE, "Region"])

head(df[index, ])
tail(df[index, ])
dim(df[index, ])

# The number of unique date entries is 1143. We can see how many times these 
# US territories are represented by dividing the number of rows by that value.
# We expect only 4 to be detected.

nrow(df[index, ])/1143

## It looks like all of these dates are entered in twice, but still in increasing
## order. This explains why some of these entries passed the monotonically 
## increasing test despite the duplication of values. There are a few different 
## reasons why this duplication can be occurring:
##    a) The raw data ported in came with the duplication.
##    b) The duplication was introduced as some point, likely during the
##       state-level value summation over all counties.
## 
## Without going back to the raw data, we can see if the second option is a
## possibility. For this duplication to be introduced, there would have to be
## no county-level subdivisions of a region. Of all five U.S. territories,
## we expect that Puerto Rico would have counties. We can compare this 
## assumption over all U.S. territories by extracting unique "Combined_Key"
## entries containing the territory name.

df[str_detect(df$Combined_Key, "Puerto Rico"), "Combined_Key"]      |> unique()
df[str_detect(df$Combined_Key, "American Samoa"), "Combined_Key"]   |> unique()
df[str_detect(df$Combined_Key, "Guam"), "Combined_Key"]             |> unique()
df[str_detect(df$Combined_Key, "Northern Mariana"), "Combined_Key"] |> unique()
df[str_detect(df$Combined_Key, "Virgin Islands"), "Combined_Key"]   |> unique()


## As expected for the second option to be correct, there are no county-level
## subdivisions for American Samoa, Guam, the Northern Mariana Islands, or
## the U.S. Virgin Islands. In contrast, there are 80 counties detected for 
## Puerto Rico.
## 
## We can safely assume that the duplication was introduced during the 
## state-level calculation earlier in the code. Meaning, we do not need to fix 
## anything that was done earlier, and can simply remove the duplicates here.

# distinct() is a dplyr function that will remove duplicated rows.
df <- df |> distinct(.keep_all = TRUE) |> `rownames<-`(NULL)

## We can double check that all unique "Combined_Key" entries are only represented
## once by checking their length is 1143, the expected length of dates covered.

table(df$Combined_Key) == 1143 |> as.data.frame()


## All entries returned TRUE, which is a good sign we can proceed with the 
## isotonic regression method of data smoothing for all of the data.

search_space = unique(df$Combined_Key)

result = list()
for (i in 1:length(search_space)) {
  # Subset the data set for one "Combined_Key" entry.
  subset <- df[df$Combined_Key %in% search_space[i], ]
  
  # Save the fitting results predicted y-values. NOTE: In "yf" "f" means
  # "fitted" and "y" denote the prediction based on the isotonic fitting
  # results.
  ir_yf <- isoreg(subset[, "Deaths_Count_Cumulative"], y = NULL) |> _$yf
  
  # Add the results as a new column back to the originally subsetted data.
  result[[i]] <- cbind(subset, ir_yf) |> 
    `colnames<-`(c(colnames(subset), "Deaths_Count_Cumulative_yf"))
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Combined_Key".
df_monotonic <- do.call(rbind, result) |> as.data.frame()

# Round values to the nearest whole value.
df_monotonic[, c(3:4)] <- sapply(df_monotonic[, c(3:4)], function(x) round(x, digits = 0))


## We can confirm that this results worked by rerunning the same plot. Here,
## the purple line represents the raw data while the blue line represents the
## isotonic regression fitting results. We see that the fitting corrects
## existing problems related to monotonicity.

df_monotonic |>
  # Filter the data set for one specific "Combined_Key".
  filter(Combined_Key %in% "Oklahoma, US") |>
  ggplot(data = _) +
    # Line for the raw values. Colored purple.
    geom_line(aes(x = Date, y = Deaths_Count_Cumulative), 
              color = "#A353FF", size = 1) +
    # Line for the yf fitted values. Colored blue.
    geom_line(aes(x = Date, y = Deaths_Count_Cumulative_yf), 
              color = "#00356B", size = 1) +
    # Format the y-axis to show values in terms of thousands.
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
    # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
    # every four months.
    scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
    # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
    labs(title = "Deaths from COVID-19 in Oklahoma",
         x = "Month", y = "Cumulative Deaths Recorded") +
    # Graph displays as minimal with a legend.
    theme_minimal()


## Now that we have finished cleaning the data (clearing duplicate values,
## smoothing inconsistent results, and calculating new regions) we are ready
## to backcalculate the daily death counts from the cumulative values.

search_space = unique(df$Combined_Key)

result = list()
for (i in 1:length(search_space)) {
  # Subset the data set one for "Combined_Key" entry.
  subset <- df_monotonic[df_monotonic$Combined_Key %in% search_space[i], ]
  
  # Calculate the difference between the ith and i+1 value. Use the first value
  # in the original vector as the first value in the new vector and shift 
  # difference counts down one position. In this case we use the fitted values
  # so that no negative counts are introducted.
  daily_count <- c(subset[1, "Deaths_Count_Cumulative_yf"], diff(subset[, "Deaths_Count_Cumulative_yf"]))
  
  # Add the results as a new column back to the originally subsetted data.
  result[[i]] <- cbind(subset, daily_count) |> 
    `colnames<-`(c(colnames(subset), "Deaths_Count_Daily"))
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one "Combined_Key".
df_final <- do.call(rbind, result)


## We can confirm that our calculation was done correctly by plotting the
## results for one region.

df_final |>
  # Filter the data set for one specific "Combined_Key".
  filter(Combined_Key %in% "Oklahoma, US") |>
  ggplot(data = _) +
    # Line for the raw values. Colored purple.
    geom_line(aes(x = Date, y = Deaths_Count_Daily)) +
    # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
    # every four months.
    scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
    # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
    labs(title = "Deaths from COVID-19 in Oklahoma",
         x = "Month", y = "Cumulative Deaths Recorded") +
    # Graph displays as minimal with a legend.
    theme_minimal()


## It is difficult to interpret trends over the daily counts. It would be easier
## to see how things changed over three years if counts were combined by monthly
## updates. We can do this using aggregate, which is an alternative to dplyr's
## summarise() function.

# Create a new vector that rounds "Dates" down to the nearest month.
df_monthly <- df_final |>
  # Group the values by month using lubridate to generate a new column with the
  # month for a respective daily entry.
  group_by(month = lubridate::floor_date(Date, "month")) |> 
  # Remove the grouping and classify as a data frame.
  ungroup() |> as.data.frame()

# In the aggregation, retain only the max value of the cumulative counts.
# Organize aggregation by unique "Combined_Key" entries and dates rounded down 
# to the nearest month.
maxCumulative <- aggregate(. ~ Combined_Key + month, df_monthly[, c(1, 3:4, 6)], max) |>
  `colnames<-`(c("Combined_Key", "Month", "Deaths_Count_Cumulative", "Deaths_Count_Cumulative_yf"))

# Do the same aggregation over the daily counts, but retain the sum.
sumDaily      <- aggregate(. ~ Combined_Key + month, df_monthly[, c(1, 5:6)], sum) |>
  `colnames<-`(c("Combined_Key", "Month", "Deaths_Count_Daily"))

# Merge these two aggregated data sets, combining by unique matches in the
# "Date" and "Combined_Key" columns.
df_byMonth    <- merge(maxCumulative, sumDaily, by = c("Combined_Key", "Month"))


# Organize rows by ascending dates and smaller regions: country- to state- to
# county-level.
build = list()
for(i in 1:3) {
  # The intention is to keep rows organized in order of larger regions to smaller:
  # country- to state- to county-level. This can be achieved by ordering
  # the rows by data subsets at these levels of information.
  index = which(str_count(df_byMonth$Combined_Key, ",") == i - 1)
  
  # arrange() is a dplyr function that orders the data frame by the selected
  # column(s). In this case, that is by "Combined_Key" followed by "Date".
  build[[i]] <- df_byMonth[index, ] |> arrange(Combined_Key, Month)
}
df_byMonth <- do.call(rbind, build) |> as.data.frame()


## If we replot the same values, but aggregated by months, we can much more
## clearly see trends over the duration of the pandemic.

df_byMonth |>
  # Filter the data set for one specific "Combined_Key".
  filter(Combined_Key %in% "Oklahoma, US") |>
  ggplot(data = _) +
    # Line for the raw values. Colored purple.
    geom_line(aes(x = Month, y = Deaths_Count_Daily)) +
    # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
    # every four months.
    scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
    # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
    labs(title = "Deaths from COVID-19 in Oklahoma",
         x = "Month", y = "Cumulative Deaths Recorded") +
    # Graph displays as minimal with a legend.
    theme_minimal()


## ----------------------------------------------------------------------------
## SAVE CLEANED DATA FOR ANALYSIS

## Now that basic Extract, Transforming, and Loading (ETL) has been completed,
## and we understand the contents of our data set a little better, we are 
## ready to proceed with plotting.
## 
## For brevity in the tutorial, plotting the cleaned and aggregated data
## will be recorded here and an appended description of what was done
## will be discussed in the tutorial itself.


# The following saves the complete data set. The resulting file is over 150 MB
# which is too large to save in GitHub. Those interested can save the data
# on their local device by removing the comment.
#write.csv(df_final, "COVID-19 Deaths_Cleaned.csv", row.names = FALSE)

write.csv(df_byMonth, "COVID-19 Deaths_Cleaned_Aggregated by Month.csv", row.names = FALSE)








