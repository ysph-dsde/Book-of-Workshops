## ----------------------------------------------------------------------------
## Produced by Yale's Public Health Data Science and Data Equity (DSDE) team
##
## Workshop: Getting Started with Git and GitHub
## Authors:  Shelby Golden, M.S.
## Date:     2024-10-11
## 
## R version:    4.4.1
## renv version: 1.0.11
##
## Description: Worked-through example generating line and bar graphs of
##              time-series vaccination rates using the JHU CRC's data from 
##              their GovEX GitHub repository. Refer to the main directory 
##              README file for additional information.


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT

## renv() will install all of the packages and their correct version used here.
renv::restore()

## Load in the R packages used in this script from the project library.
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## This data is from the COVID-19 Data Repository by the Center for Systems 
## Science and Engineering (CSSE) at Johns Hopkins University (JHU), GitHub 
## GovEX. Additional details can be found in the project repositories main 
## directory's README file.

## We load it in directly from their GitHub page using the raw URL.

df.url <- "https://raw.githubusercontent.com/govex/COVID-19/refs/heads/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_us.csv"
df     <- read_csv(file = df.url, show_col_types = FALSE) %>% as.data.frame()


## The JHU Coronavirus Resource Center (JHU CRC) GovEX repository includes a
## census population file with intercensal estimates and projections from 2010:
## location:      govex/COVID-19/tree/master/data_tables/Data_for_UScounty_map
## file:          PovertyEstimates.xls
## downloaded as: U.S. Department of Agriculture_Population Estimates 2010 to 2018_JHU CRC.xls
##
## This file only covers U.S. population estimates from 2010 to 2018, and do not
## include representation of the U.S. Territories, with the exception of Puerto
## Rico. In the GitHub for this project, file "Population Estimates and 
## Projections" describes how three files from U.S. Census Bureau and one from
## the U.S. Department of Agriculture with census year population number, 
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
  "Population Estimates and Projections/US_Census Population Estimates_2010 to 2023.csv",
  header = TRUE)


## View the first and last few rows of the data.
head(df)
tail(df)


## Number of rows and columns.
dim(df)




## ----------------------------------------------------------------------------
## DATA PREPARATION

## First we'll see how the raw data plots. The GovEX README file for the data
## set says that reported vaccinations are cumulative. Therefore, we expect the
## plots to appear monotonic.

df %>%
  filter(Province_State %!in% NA) %>%
  ggplot(data = ., aes(x = Date, y = People_at_least_one_dose)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Raw Data Line Plot - Cumulative\nBy State",
           x = "Daily Updates", y = "People With at Least One Dose") +
      theme_minimal() + theme(legend.position = "none")


## It appears that some state/territories cumulative counts are purely monotonic, 
## but some appear to have some irregularities. These irregularities seem to be
## effecting a limited range of dates, and are not wide-spread nor do they show
## a dramatic departure from what we expect.
## 
## Please refer to the Cleaning Raw Data/Cleaning Script_Vaccinations.R script
## for the complete details about how the raw data from JHU CRC's GovEX repo was
## cleaned and transformed. Here, we will only load the cleaned data produced
## by that script.
## 
## To summarize, the following data cleaning and transforming steps were taken:
##    - Reconcile presence of NA's
##    - Identify and remove duplicate row entries.
##    - Smooth the cumulative counts so they are monotonically increasing over
##      the entire span of dates by fitting with an isotonic regression. NOTE:
##      Column names with a "yf" indicate those values are smoothed.
##    - Back-calculate daily rates from the cumulative sums using the smoothed
##      values, so as to prevent introduction of negative counts.
##    - Aggregate the data to from daily counts to monthly counts.
##    - Basic data set reformatting: reorder the row entries by state names, etc.

df_cleaned <- read_csv("Vaccinations Aggregated by Month.csv", show_col_types = FALSE) %>%
  as.data.frame() %>% .[, -1]

# Reorder the columns.
df_cleaned <- df_cleaned[, c(2, 1, 3:10)]


## View the first and last few rows of the data.
head(df_cleaned)
tail(df_cleaned)


## Number of rows and columns.
dim(df_cleaned)


## We can double check that the imported, cleaned data set plots the cumulative
## and daily counts correctly when aggregated to monthly updates.
df_cleaned %>%
  filter(Province_State %!in% "United States") %>%
      ggplot(data = ., aes(x = Month, y = People_at_least_one_dose_yf)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Total Number of People With at Least One Dose - Cumulative\nBy State",
           x = "Monthly Updates", y = "People With at Least One Dose") +
      theme_minimal() + theme(legend.position = "none")



df_cleaned %>%
  filter(Province_State %!in% "United States") %>%
  ggplot(data = ., aes(x = Month, y = People_at_least_one_dose_yf_daily)) +
      geom_line(aes(color = Province_State)) +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      labs(title = "Total Number of People With at Least One Dose - Daily\nBy State",
           x = "Monthly Updates", y = "People With at Least One Dose") +
      theme_minimal() + theme(legend.position = "none")


## All seems to be in order so far, however, while we are comparing the raw
## counts between states, it is much more meaningful to compare their
## vaccination rates as a percentage of the population. We can do this
## normalization using the harmonized census population estimates.




## ----------------------------------------------------------------------------
## NORMALIZE TO GET PERCENTAGE OF POPULATION VACCINATED

## First we will inspect the harmonized census population estimates.
## NOTE: Vintages are updated annually based on the current year of the update
## and only spans the dates between the recent census year to the last
## intercensal year preceding the next census. The harmonized data set combines
## vintages from the 2010 to 2019 census estimates and the 2020 to 2023
## census estimates. Additional details can be found in the "Population 
## Estimates and Projections" subdirectory of this projects GitHub repo.

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
census_2010to2023$NAME %>% unique()

# State, territories, and regions represented more than once.
census_2010to2023$NAME %>% table() %>% .[. > 1] %>% as.data.frame()

# "Province_State" entries not represented in the U.S. Census data.
unique(df_cleaned$Province_State) %>% .[. %!in% unique(census_2010to2023$NAME)]


## The first thing to notice is the Virgin Islands is represented in the
## U.S. Census data, but with a different string: "Virgin Islands, U.S.". We
## can correct this by changing the df_cleaned name.

df_cleaned$Province_State[df_cleaned$Province_State %in% "Virgin Islands"] <- "Virgin Islands, U.S."

# Double check that the Virgin Islands entry is identified as a match.
unique(df_cleaned$Province_State) %>% .[. %!in% unique(census_2010to2023$NAME)]


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
df_filtered <- filter(df_cleaned, df_cleaned$Province_State %in% subset_census$NAME)


## There are a few regional calculations in the census data that we do not
## have in the vaccination data set.

missing_entries <- unique(census_2010to2023$NAME) %>% .[. %!in% unique(df_cleaned$Province_State)]
missing_entries




## ----------------------------------------------------------------------------
## ADD MISSING REGIONS AND DIVISIONS TO MAIN DATA SET


## We can generate those using the regional and division map provided by
## the U.S. Census Bureau. Refer to this projects GitHub, folder "Population 
## Estimates and Projections" subdirectory README for additional information.

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
filtered_search_space <- df_cleaned[df_cleaned$Province_State %in% us_regions_divisions$Province_State, 
                                    c(1:2, which(str_detect(colnames(df_cleaned), "daily") ))] %>% 
  `rownames<-`(NULL)

# Add a U.S. divisions column by merging the two data sets by "Province_State".
added_regions <- merge(us_regions_divisions, filtered_search_space, by = c("Province_State"))


# Add the newly aggregated sums by rows.
filtered_search_space <- rbind(
  filtered_search_space,
  # Sum over the regions by "Province_State" and "Month" to get their aggregate sums.
  aggregate(added_regions[, -c(1:4)], list(added_regions$Region, added_regions$Month), sum) %>% 
              `colnames<-`(c("Province_State", "Month", colnames(.)[-c(1:2)])),
  # Sum over the divisions by "Province_State" and "Month" to get their aggregate sums.
  aggregate(added_regions[, -c(1:4)], list(added_regions$Division, added_regions$Month), sum) %>% 
    `colnames<-`(c("Province_State", "Month", colnames(.)[-c(1:2)]))
)

# We can confirm that we now have all of the regional sums. Only the territories
# and total U.S. counts are showing up, which is good because we used a filtered
# data set to generate our aggregates.
unique(census_2010to2023$NAME) %>% .[. %!in% unique(filtered_search_space$Province_State)]

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
    mutate(Doses_admin_yf = cumsum(Doses_admin_yf_daily) ) %>% 
    mutate(People_at_least_one_dose_yf = cumsum(People_at_least_one_dose_yf_daily)) %>% 
    mutate(People_fully_vaccinated_yf = cumsum(People_fully_vaccinated_yf_daily)) %>% 
    mutate(Total_additional_doses_yf = cumsum(Total_additional_doses_yf_daily))
    )
}

# Confirm the column name orders are correct before rejoining the newly
# constructed data back into the main df_cleaned.
all(colnames(build[, c(1:2, 7:10, 3:6)]) == colnames(df_cleaned))

# Commit the changes.
df <- rbind(build[, c(1:2, 7:10, 3:6)], df_cleaned)

# Confirm that the different state regions and divisions. Boolean test will
# say TRUE if this is completed.
unique(census_2010to2023$NAME) %>% .[. %!in% unique(df$Province_State)] %>% length(.) == 0


## Now we'll normalize the vaccination daily counts by the population estimates
## for that year to get the percent of the population that was vaccinated.

# Store the column names we are interested in by cumulative and daily counts.
cumulative_counts <- colnames(df)[3:6]
daily_counts      <- colnames(df)[7:10]

result = list()
for (i in 1:length(subset_census$NAME)) {
  # Separate out the rows associated with one "Province_State" entry.
  subset <- df %>% .[.$Province_State %in% subset_census$NAME[i], ]
  
  # Collect the year that the observation was made.
  staged_dates <- subset[, "Month"] %>% year()
  
  relevant_pop_est = c()
  for (j in 1:length(staged_dates)) {
    # Create a vector with the appropriate intercensal year population estimates
    # that matches the staged_dates vector entry-by-entry.
    relevant_pop_est[j] <- subset_census[i, str_detect(colnames(subset_census), as.character(staged_dates[j]))]
  }
  
  # Normalize over all columns for vaccination daily counts. Excluding doses
  # administered because it does not mean as much as the other metrics when
  # normalized by the total population.
  percentages <- subset[, daily_counts[-1]] %>% sapply(., function(x)
    round((x / relevant_pop_est) * 100, 0))
  
  # Save the results and join them with the metadata columns for merging 
  # back to the main data set.
  result[[i]] <- cbind(subset[, c("Province_State", "Month")], percentages)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the percentage results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) %>% as.data.frame() %>%
  `colnames<-`(c("Province_State", "Month", str_c(cumulative_counts[-1], "_percent")))

# Merge the previous data set with the normalized daily counts, combining by
# unique matches with the "Month" and "Province_State" columns.
df_total <- merge(df, result, by = c("Province_State", "Month"))


## For convenience, we'll order the rows by granular regions, starting
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
df_total <- build %>% `rownames<-`(NULL)




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
search_space      = df_total$Province_State %>% unique()
# Pull the unique months detected in the whole data set.
unique_months     = df_total$Month %>% unique()
# Extract out the cumulative counts. These are columns that do not have
# "..._daily" or "..._percent" in their column names. We're going to exclude
# the column representing the doses administered, as it is not going to be
# meaningful in this evaluation.
subset_cum_counts = df_total[, c(1:2, which(str_detect(colnames(df_total), "_yf\\b"))[-1])]


result = c()
# Boolean test that each "Province_State" subset has the same months. Everything
# should be ordered in increasing dates, and so we do an exact match test.
for (i in 1:length(search_space)) {
  # Extract the months listed for each "Province_State" and test for exact match
  # with the reference unique_months vector.
  test      <- subset_cum_counts %>% .[.$Province_State %in% search_space[i], "Month"] == unique_months
  result[i] <- test %>% all()
}
# Organize the results into a data frame. Each row reflects the Boolean test
# results for one "Province_State".
as.data.frame(result) %>% `rownames<-`(search_space) %>% `colnames<-`("Same Dates?")


## Looks like all results are true. Therefore, we will take the max month
## listed under each year and save the row result that matches that date.

data_range = str_c("20", c("20", "21", "22", "23"))

build = list()
for (i in 1:length(data_range)) {
  querry     <- data.frame("Months" = unique_months, "Year" = year(unique_months)) %>% .[.$"Year" %in% data_range[i], "Months"] %>% max()
  build[[i]] <- subset_cum_counts %>% .[.$Month %in% querry, ]
}
# Combine the results (list format) into a data frame by adding rows. Each
# list element includes the subset of our data set by the max month in
# each year.
df_cumulative <- do.call(rbind, build) %>% `rownames<-`(NULL)




## Now we'll normalize the vaccination daily counts by the population estimates
## for that year to get the percent of the population that was vaccinated.

result = list()
for (i in 1:length(subset_census$NAME)) {
  # Separate out the rows associated with one "Province_State" entry.
  subset <- df_cumulative %>% .[.$Province_State %in% subset_census$NAME[i], ]
  
  # Collect the year that the observation was made.
  staged_dates <- subset[, "Month"] %>% year()
  
  relevant_pop_est = c()
  for (j in 1:length(staged_dates)) {
    # Create a vector with the appropriate intercensal year population estimates
    # that matches the staged_dates vector entry-by-entry.
    relevant_pop_est[j] <- subset_census[i, str_detect(colnames(subset_census), as.character(staged_dates[j]))]
  }
  
  # Normalize over all columns for vaccination daily counts.
  percentages <- subset[, str_detect(colnames(subset), "_yf")] %>% sapply(., function(x)
    round((x / relevant_pop_est) * 100, 0))
  
  # Save the results and join them with the metadata columns for merging
  # back to the main data set.
  result[[i]] <- cbind(subset[, c("Province_State", "Month")], percentages)
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the percentage results for one "Province_State" and each column
# represents the vaccine counting method.
result <- do.call(rbind, result) %>% `colnames<-`(c("Province_State", "Month", str_c(colnames(subset)[str_detect(colnames(subset), "_yf")], "_percent")))

# Merge the previous data set with the normalized daily counts, combining by
# unique matches with the "Month" and "Province_State" columns.
df_cumulative <- merge(df_cumulative, result, by = c("Province_State", "Month"))


## Because the raw data is imperfect, it is possible that cumulative counts will
## reflect over 100% vaccinated. We can check which columns have this issue.

sapply(df_cumulative[, str_detect(colnames(df_cumulative), "percent")], function(x)
  any(x > 100)) %>% as.data.frame() %>% `colnames<-`("Over 100%?")


## Fix the values that are over 100% vaccinated and reset to 100%.
df_cumulative$People_at_least_one_dose_yf_percent[df_cumulative$People_at_least_one_dose_yf_percent > 100] <- 100




## ----------------------------------------------------------------------------
## GENERATE AND SAVE OUR PLOT

# Vectors with the unique entries for "Province_State", excluding the "United
# States" total counts.
unique_states      = sort(c(datasets::state.name, "District of Columbia"))
unique_territories = c("American Samoa", "Guam", "Northern Mariana Islands", 
                       "Puerto Rico", "Virgin Islands, U.S.")
unique_regions     = df_total$Province_State %>% .[. %!in% c("United States", unique_states, unique_territories)] %>% unique() %>% .[1:4]
unique_districts   = df_total$Province_State %>% .[. %!in% c("United States", unique_states, unique_territories)] %>% unique() %>% .[-c(1:4)]


# Vector of the dates for cumulative vaccination proportions.
cumulative_dates = df_cumulative$Month %>% unique()


# Vectors with the different column-value names by the types of counts they
# represent. Recall the following y-axis variable options:
#     - "_yf"         <- cumulative counts smoothed to be monotonically increasing.
#     - "_yf_daily"   <- back-calculated daily counts.
#     - "_yf_percent" <- daily counts normalized by census data for percentage
#                        of the population that is vaccinated.

percentages_counts = colnames(df_total)[str_detect(colnames(df_total), "percent")]

cumulative_counts
daily_counts
percentages_counts


# Recall the following table relating U.S. states with regions and districts.
us_regions_divisions



# Line plot

line_plot <- df_total %>%
  # Filter the data to plot only a selection of "Province_State" entries.
  filter(Province_State %in% unique_regions) %>%
  # Generate the plot with time as the x-axis and vaccinations as the y-axis.
  ggplot(data = ., aes(x = Month, y = Doses_admin_yf_daily)) +
      # Generate a line plot and separate data by unique "Province_State".
      geom_line(aes(color = Province_State)) +
      # Format the y-axis to show values as a percent.
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      # Format the x-axis to show dates as Jan 2020 from 01/01/2020, spaced
      # every four months.
      scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") +
      # To adjust the legend title both the color and fill need to by
      # changed to the same string.
      scale_fill_discrete(name  = "U.S. Regions") +
      scale_color_discrete(name = "U.S. Regions") +
      # Title, x-axis, and y-axis labels. NOTE: "\n" forms a new line.
      labs(title = "Doses Administered by U.S. Regions",
           x = "Month", y = "Number of Doses Administered") +
      # Graph displays as minimal with a legend.
      theme_minimal() #+ theme(legend.position = "none")


line_plot



# Bar plot

# Get the reference horizontal line for overall U.S. vaccinations.
overall <- df_cumulative[df_cumulative$Province_State == "United States" & 
                           df_cumulative$Month == cumulative_dates[4], 
                         percentages_counts[1]]

bar_plot <- df_cumulative %>%
  filter(Province_State %in% unique_states & Month %in% cumulative_dates[4]) %>%
  ggplot(data = ., aes(x = reorder(Province_State, People_at_least_one_dose_yf_percent), 
                       y = People_at_least_one_dose_yf_percent/100)) +
      # stat = "identity" tells the algorithm to not aggregate values, but
      # plot them as provided. alpha = 0.25 fills the bars with 25% opacity.
      geom_bar(stat = "identity", position = "dodge", alpha = 0.3, fill = "#A353FF") +
      scale_y_continuous(labels = scales::percent) +
      geom_hline(yintercept = overall/100, linetype = "dashed", color = "#00356B") +
      annotate("text", x = 3, y = overall/100, label = "U.S. Rate", 
               vjust = -0.5, colour = "#00356B") +
      #scale_fill_discrete(name  = "U.S. Regions") +
      #scale_color_discrete(name = "U.S. Regions") +
      labs(title = "Percentage of Population With At Least One Dose by March 2023",
           x = "", y = "Percentage of the Population") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 65,  hjust = 1))


bar_plot


# Save a plot as a jpeg file.

bar_plot %>% ggsave("plot.jpeg", ., width = 10, height = 6, units = "in", dpi = 300)






