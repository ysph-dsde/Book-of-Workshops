## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Getting Started with Git and GitHub
##  Authors: Shelby Golden, M.S.
##     Date: 2024-10-25
## 
##    R version: 4.4.3
## renv version: 1.0.11
## 
## Description: This script harmonizes U.S. census year data and intercensal
##              year population estimates from the U.S. Census Bureau. Two 
##              methods are reflected: the Official U.S. Projections (OE) and 
##              International Database (IDB). Total U.S. population and the
##              population of Puerto Rico are present in both. Refer to the
##              "Population Estimates and Projections for the U.S. Island Areas"
##              webpage for additional details about when to use which source.
##              Pertinent links and other details about the data can be found 
##              in the README for this projects GitHub repository subdirectory,
##              folder "Population Estimates and Projections".

## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT

## renv() will install all of the packages and their correct version used here.
renv::restore()

## Load in the R packages used in this script from the project library.
suppressPackageStartupMessages({
  library("readxl")     # Read in xlsx files
  library("readr")      # Read in csv's
  library("tidyr")      # Convert data frames into tidy format
  library("dplyr")      # For data manipulation
  library("stringr")    # For string manipulation
})


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))

## Read in custom functions used in this script.
source("Git-and-GitHub/R/Population Estimates and Projections/Functions.R")




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## The COVID-19 Data Repository by the Center for Systems Science and 
## Engineering (CSSE) at Johns Hopkins University used census data compiled
## by the U.S. Department of Agriculture. It spans 2010 to 2018, and the data
## ultimately comes from the U.S. Census Bureau annual vintage release for 2018.
## 
## Here we harmonized three data sets from the U.S. Census Bureau: U.S. States
## and Puerto Rico counts for the 2010 to 2019 vintage, the 2020 to 2023
## vintage, and IDB population estimates and projections for U.S. territories
## from 2010 to 2023. The U.S. Department of Agriculture data is ported in only
## to supplement for missing 2020 Census year counts from the 2020 to 2023
## vintage. Further details about the data can be found in the README 
## file for this projects GitHub repository subdirectory.

census_2010to2019_raw <- read_csv(
  "Git and GitHub/Population Estimates and Projections/U.S. Census Bureau_2010 to 2019 State Population Estimates_All Components_Downloaded 10.25.2024.csv",
) |> as.data.frame()

census_2020to2023_raw <- read_csv(
  "Git and GitHub/Population Estimates and Projections/U.S. Census Bureau_2020 to 2023 State Population Estimates_All Components_Downloaded 10.25.2024.csv"
) |> as.data.frame()

census_idb_2010to2023_raw <- read_csv(
  "Git and GitHub/Population Estimates and Projections/U.S. Census Bureau_2010 to 2023 IDB Population Estimates_Some Components_Downloaded 10.25.2024.csv",
) |> as.data.frame()




## ----------------------------------------------------------------------------
## HARMONIZE THE DATASETS

dim(census_2010to2019_raw)
dim(census_2020to2023_raw)
dim(census_idb_2010to2023_raw)

## We see these data sets have different row and column dimensions. This is to
## be expected, as they span different date ranges. The IDB data is also in a
## long format and not a wide format, like the other sources. The API to
## compile the IDB did not offer all of the same variables as the other ones,
## but there may be opportunities to calculate some of these values with
## the available variables.
## 
## Therefore, we will need to do some data transformations to get them into one 
## standardized format. First we will add the missing 2020 Census year counts
## to our census_2020to2023_raw file.




## ----------------------------------------------------------------------------
## SUPPLEMENT THE MISSING 2020 CENSUS YEAR COUNTS

## census_2020to2023_raw is missing the 2020 Census values. This can be
## supplemented using the U.S. Department of Agriculture's compilation of
## the same U.S. Census Bureau data. This source has census counts represented
## down to the county-level.

# Read in alternative source for the 2020 Census data
census_2020to2023_depAg_raw <- read_excel(
  "Git and GitHub/Population Estimates and Projections/U.S. Department of Agriculture_Population Estimates 2020 to 2023_Downloaded 10.18.2024.xlsx",
  skip = 4
) |> as.data.frame()


## Now we'll extract out the specific column and row entries to supplement the
## missing 2020 Census, observed counts in census_2020to2023_raw.

# State/territory and U.S. regions included in census_2020to2023_raw. This is
# our search space from the supplementary information
search_space = census_2020to2023_raw$NAME

# Retrieve the 2020 Census counts.
census_2020 <- census_2020to2023_depAg_raw |> 
  (\(x) { x[x$Area_Name %in% search_space, c("Area_Name", "CENSUS_2020_POP")] }) () |>
  distinct() |>
  `colnames<-`(c("NAME", "CENSUS2020POP"))


## We have our census data, but the target search space includes sums over
## different regional and divisional groupings of U.S. states. Using the
## regional map from the U.S. Census Bureau, we can calculate these sums.
## Source: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf

# Create a reference to map census counts when aggregating sums.
us_regions_divisions <- 
  data.frame("NAME" = c("Connecticut", "Maine", "Massachusetts", 
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


# Check all the states, regions, and divisions are entered correctly. The
# following lines are Boolean tests that will be TRUE if they are correct.
us_regions_divisions |>
  filter(NAME %!in% search_space) |>
  nrow() == 0

us_regions_divisions |>
  filter(Region %!in% search_space) |>
  nrow() == 0

us_regions_divisions |>
  filter(Division %!in% search_space) |>
  nrow() == 0


# Check that all states that need to be represented are.
all_entries <- c(us_regions_divisions[, "NAME"],
                 unique(us_regions_divisions[, "Region"]), unique(us_regions_divisions[, "Division"]))

search_space |>
  (\(x) { x[x %!in% all_entries] }) ()


## Our quality checks show the regional reference is correctly compiled. Now
## we will aggregate by these regions and divisions to generate the census
## counts that are missing from the supplementary source.
filtered_search_space <- census_2020[census_2020[, "NAME"] %!in% c("United States", "Puerto Rico"), ] |>
  `colnames<-`(c("NAME", "CENSUS2020POP")) |> 
  `rownames<-`(NULL)

census_2020_regions <- merge(us_regions_divisions, filtered_search_space, by = c("NAME"))


## As a final step, we will now add the 2020 Census data into the full data
## frame and reorganize the new column to match the 2010 to 2019 formatting.

census_2020to2023_raw <- merge(
  rbind(aggregate(census_2020_regions$CENSUS2020POP, list(census_2020_regions$Region), sum) |>
          `colnames<-`(c("NAME", "CENSUS2020POP")),
        aggregate(census_2020_regions$CENSUS2020POP, list(census_2020_regions$Division), sum) |>
          `colnames<-`(c("NAME", "CENSUS2020POP")),
        census_2020),
  census_2020to2023_raw, by = c("NAME")
)

# Inspect the column order of the first few column names.
colnames(census_2010to2019_raw)[1:10]
colnames(census_2020to2023_raw)[1:10]

# Sort the columns to match this order of information.
census_2020to2023_raw <- cbind(census_2020to2023_raw[, c(3:6, 1:2)], census_2020to2023_raw[, -c(1:6)])


## Now we will reformat IDB to a wide format and calculating missing values, 
## where it is appropriate to do so.




## ----------------------------------------------------------------------------
## REFORMAT THE IDB RAW DATA SET

census_idb_2010to2023_raw$Name |> unique()

## There are a lot of rows that do not denote populations values for a U.S.
## territory. We can confirm by selecting these out.
census_idb_2010to2023_raw[str_detect(census_idb_2010to2023_raw$Name, "->"), ]

## Looks like that is the case, and so we will remove those rows.
census_idb_2010to2023_raw <- census_idb_2010to2023_raw %>% .[str_detect(.$Name, "->", negate = TRUE), ]


## Before expanding the data into a wide format, we'll rename the columns to 
## match the format of the other two data sets. We will also calculate the
## missing columns of information that was not available on the IDB API.

# Create a data frame, containing the following column elements:
unique_names <- cbind(
  # Removing the non-repeated column elements (metadata and census year 
  # population and base estimates), save the column names.
  census_2010to2019_raw[-c(1:6)] |> colnames() |>
    # Remove the non-alphanumeric characters and numbers, which will strip the 
    ## date designation. Also count the frequency a name occurs.
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    # Reformat the reported results.
    as.data.frame() |> `colnames<-`(c("2010to2019", "Freq")),
  
  # Repeat the same steps for the other data sets.
  census_2020to2023_raw[-c(1:6)] |> colnames() |>
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    as.data.frame()|> `colnames<-`(c("2020to2023", "Freq"))
)

unique_names


# Remove the redundant columns of information.
census_idb_2010to2023_raw <- census_idb_2010to2023_raw[, -c(2:3, 6)]

# Create a reference to map old and standard column names.
column_names_key <- data.frame("Reference" = colnames(census_idb_2010to2023_raw),
                               "Standardized" = c("NAME", "YEAR", "POPESTIMATE",
                                                  "RNATURALINC", "NATURALINC", 
                                                  "RBIRTH", "BIRTHS", "RDEATH", 
                                                  "DEATHS", "RINTERNATIONALMIG", 
                                                  "INTERNATIONALMIG"))

# Standardize the column names.
colnames(census_idb_2010to2023_raw) <- column_names_key$Standardized


## Now we will check which column names are not currently represented.
as.vector(unique_names$`2010to2019`) |>
  (\(x) { x[x %!in% colnames(census_idb_2010to2023_raw)] }) ()

## Referring to the 2020 to 2023 methods, we see that:
##    - Domestic immigration measures movement of people within U.S. states. 
##    - Net migration requires information about the number of people who enter 
##      the country and the number who leave. 
##    - When using the Official U.S. Projections method, the residual measures 
##      deviation between the balancing equation and the final estimates. The 
##      balancing equation is used to make estimates consistent across
##      different geographic locations and demographic characteristics.
##    - Base population estimates are the starting point for vintage
##      population estimations.
##
## We cannot back-calculate any of these variables using the information 
## available from the IDB API, leaving "NPOPCHG". This is simply calculated by
## taking the difference between projected population estimates for the i+1
## and ith year. In the 2010 to 2019 and 2020 to 2023 data sets, the first 
## entry under 2010 and 2020, respectively, shows the difference between the 
## census year population estimate and base estimate. We will set these
## positions to NA here, as the population estimates for 2010 and 2020 were
## not available from the IDB API.

# Before calculating values, we'll check the column vectors are numeric.
sapply(census_idb_2010to2023_raw, class) |> as.data.frame()

# We need to convert vectors from characters to numeric. First we need
# to strip the non-alpha numeric characters, which in this case is a comma.
census_idb_2010to2023_raw[, -c(1:2)] <- sapply(census_idb_2010to2023_raw[, -c(1:2)], function(x) str_replace_all(x, "[^\\d]", ""))
# Now convert the column vectors to numeric.
census_idb_2010to2023_raw[, -c(1:2)] <- sapply(census_idb_2010to2023_raw[, -c(1:2)], as.numeric)

# We'll confirm that these columns have been all converted. No errors came up
# during the class conversion, and so we expect values that need to be retained
# were coerced to NA by error.
sapply(census_idb_2010to2023_raw, class) |> as.data.frame()


# Extract the U.S. territory names that will be used to subset the data.
search_space = unique(census_idb_2010to2023_raw$NAME)

result <- list()
for (i in 1:length(search_space)) {
  # Create a data frame where the name column is the current U.S. territory
  # used to subset the data.
  result[[i]] <- data.frame("NAME" = rep(search_space[i], 14),
    # Save the vector of years that was subset.
    "YEAR" = unique(census_idb_2010to2023_raw$YEAR),
    # Calculate the successive differences between population projections
    # between years by each U.S. territory.
    "NPOPCHG" = census_idb_2010to2023_raw[census_idb_2010to2023_raw$NAME %in% search_space[i], "POPESTIMATE"] |>
      diff() |> (\(x) { c(NA, x) }) ()
  )
}
# Combine the results (list format) into a data frame by adding rows. Each
# row reflects the results for one territory.
result <- do.call(rbind, result) |> as.data.frame()

# Merge the previous data set with the newly calculated population differences, 
# combining by unique matches with the "NAME" and "YEAR" columns.
census_idb_2010to2023_raw <- merge(census_idb_2010to2023_raw, result, by = c("NAME", "YEAR"))


# Add columns for the remaining information as vectors of NA's.
census_idb_2010to2023_raw <- cbind(census_idb_2010to2023_raw, 
      data.frame("DOMESTICMIG" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "RDOMESTICMIG" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "ESTIMATESBASE2010" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "CENSUS2010POP" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "ESTIMATESBASE2020" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "CENSUS2020POP" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "NETMIG" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "RNETMIG" = rep(NA, nrow(census_idb_2010to2023_raw)),
                 "RESIDUAL" = rep(NA, nrow(census_idb_2010to2023_raw)) ))


## Now that we have filled in all the missing columns of information that
## were missing from the IDB data set, we will reformat the data into the
## pattern of the other two. First we need to find out what that pattern is.
## 
## All data sets repeat the same type of information for different range
## of years. The IDB census data does not have the same metadata as the
## 2010 to 2019 or 2020 to 2023 data sets, and so we can remove those
## extraneous details.

# The only meta data is the country name and the year of the population estimate.
colnames(census_idb_2010to2023_raw)

# The years of information is spread column-wise. And so the only metadata
# column we need to retain is "NAME".
census_2010to2019_raw <- census_2010to2019_raw[, -c(1:4)]
census_2020to2023_raw <- census_2020to2023_raw[, -c(1:4)]


## Now we'll look at those columns that contain the intercensal population
## projections for each year, including the base census year (2010 or 2020 as 
## indicated).
dates_included <- str_c("20", 10:23)

repeated_info(census_2010to2019_raw, dates_included)
repeated_info(census_2020to2023_raw, dates_included)


## We see that the first year only has 11 columns of information associated,
## while the others have an additional four columns. We will identify which
## columns of information are needed for the census year and remove the
## excess columns.

# Recall
unique_names <- cbind(
  # Removing the non-repeated column elements (metadata and census year 
  # population and base estimates), save the column names.
  census_2010to2019_raw |> colnames() |>
    # Remove the non-alphanumeric characters and numbers, which will strip the 
    ## date designation. Also count the frequency a name occurs.
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    # Reformat the reported results.
    as.data.frame() |> `colnames<-`(c("2010to2019", "Freq")),
  
  # Repeat the same steps for the other data sets.
  census_2020to2023_raw |> colnames() |>
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    as.data.frame() |> `colnames<-`(c("2020to2023", "Freq")),
  
  census_idb_2010to2023_raw[, -2] |> colnames() |>
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    as.data.frame() |> `colnames<-`(c("2010to2023_IDB", "Freq"))
)

unique_names


## Columns of information with an "R" preceding the name, such as "RBIRTH" and
## "RNETMIG" denote the rate. There is no rate calculations for the census year, 
## and so we'll remove those columns from 2010 and 2020 in the IDB wide format.
## 
## NOTE: unique_names only shows the repeated columns. The "CENSUSPOP" and
## "ESTIMATESBASE" results were not represented in the IDB API. These columns 
## of information will be left NA for both the 2010 and 2020 census years.
## Now we'll extend out the columns.


# Store the columns of information that need to be expanded out.
search_space <- as.vector(unique_names[, 1])[sapply(unique_names[, 2], function(x) x > 1)]

# Start building the wide format with the following preceding columns. Additional
# columns that are spread will be merged into this set by the "NAME" column
build <- census_idb_2010to2023_raw[, c("NAME", "CENSUS2010POP", "ESTIMATESBASE2010", 
                                       "CENSUS2020POP", "ESTIMATESBASE2020")] |> distinct()
for(i in 1:length(search_space)){
  # Expand the rows for one U.S. territory, subset of information into columns
  # for each year.
  wide <- spread(census_idb_2010to2023_raw[, c("NAME", "YEAR", search_space[i])], key = YEAR, value = search_space[i])
  # Construct column names in the format of the 2010 to 2019 and 2020 to 2023
  # data sets, appending the year at the end.
  colnames(wide)[-1] <- str_c(rep(search_space[i], 14), "", colnames(wide)[-1])
  
  # Merge the previous data set with the newly calculated population differences, 
  # combining by unique matches with the "NAME" column.
  build <- merge(build, wide, by = c("NAME"))
}

# Commit the result.
census_idb_2010to2023_wide <- build
dim(census_idb_2010to2023_wide)

## The dimensions show that we successively expanded out the data. Now we need
## to remove the erroneous columns that represent rate in the census years,
## 2010 and 2020.

# Save the column names that need to be removed for the 2010 and 2020 census year.
not_censal_year <- as.vector(unique_names[, 1][str_detect(unique_names[, 1], "\\bR[^RES]")])
# Remove these from the data frame.
census_idb_2010to2023_wide <- census_idb_2010to2023_wide[, colnames(census_idb_2010to2023_wide) %!in% 
                                                           c(str_c(not_censal_year, c("2010")), 
                                                             str_c(not_censal_year, c("2020")))]


## Now we'll confirm that the columns are repeated as expected. If this is
## true, then the sum of the column names frequency for the 2010 to 2019 and
## 2020 to 2023 will equal the column names frequency for 2010 to 2023 IBB.

unique_names <- cbind(
  # Removing the non-repeated column elements (metadata and census year 
  # population and base estimates), save the column names.
  census_2010to2019_raw |> colnames() |>
    # Remove the non-alphanumeric characters and numbers, which will strip the 
    ## date designation. Also count the frequency a name occurs.
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    # Reformat the reported results.
    as.data.frame() |> `colnames<-`(c("2010to2019", "Freq")),
  
  # Repeat the same steps for the other data sets.
  census_2020to2023_raw |> colnames() |>
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    as.data.frame() |> `colnames<-`(c("2020to2023", "Freq")),
  
  census_idb_2010to2023_wide |> colnames() |>
    (\(x) { str_replace_all(x, "[^[:alnum:]]|[0-9]{1,4}", "") }) () |> table() |>
    as.data.frame() |> `colnames<-`(c("2010to2023_IDB", "Freq"))
)

unique_names


## This confirms that our work reformatting the IDB data frame is done. Now
## we will look into the remaining two data sets to confirm formatting
## or resolve other errors (i.e. duplicate entries, NA's coerced to 0 and 
## vice versa).




## ----------------------------------------------------------------------------
## HARMONIZE 2010 TO 2019 AND 2020 TO 2023 DATA SETS

dim(census_2010to2019_raw)
dim(census_2020to2023_raw)
dim(census_idb_2010to2023_wide)

# NOTE: The target number of columns will be the same as census_idb_2010to2023_wide

## We expect that all entries are non-zero, except for rare circumstances
## where the rate or differences are zero. We'll investigate all instantiations
## of zero to confirm these are intended to be zero instead of NA.

# Extract the columns where there is any occurance of a zero value.
zeros_2010to2019 <- census_2010to2019_raw[, sapply(census_2010to2019_raw, function(x) any(x == 0))] |> colnames()
zeros_2020to2023 <- census_2020to2023_raw[, sapply(census_2020to2023_raw, function(x) any(x == 0))] |> colnames()

# Show the column names and state/territories associated with these outcomes.
zeros_2010to2019 %>% str_replace_all(., "[^[:alnum:]]|[0-9]{1,4}", "") |> table()
census_2010to2019_raw[apply(census_2010to2019_raw[, zeros_2010to2019], 1, function(x) any(x == 0)), "NAME"]

zeros_2020to2023 %>% str_replace_all(., "[^[:alnum:]]|[0-9]{1,4}", "") |> table()
census_2020to2023_raw[apply(census_2020to2023_raw[, zeros_2020to2023], 1, function(x) any(x == 0)), "NAME"]


## Recall that "RDOMESTICMIG" is not reported for the census year. Considering
## this, we see that all entries associated with "DOMESTICMIG", "RDOMESTICMIG",
## and "RESIDUAL" for the United States and Puerto Rico only. Because the
## column-wise search was for any occurrence, we will confirm that each
## instantiation for both territories are zero.

census_2010to2019_raw[census_2010to2019_raw$NAME %in% c("United States", "Puerto Rico" ), zeros_2010to2019]
census_2020to2023_raw[census_2020to2023_raw$NAME %in% c("United States", "Puerto Rico" ), zeros_2020to2023]


## Looks like they indeed are. It makes sense that the "DOMESTICMIG" and
## "RDOMESTICMIG" are actually NA's instead of zeros, as represent migration
## between U.S. states. As stated previously, "RESIDUAL" represents the
## deviation between the balancing equation and the final estimates. This is
## done to make estimates consistent across different geographic locations and 
## demographic characteristics. As with the domestic migration metric, a
## balancing equation is likely not applied to the United States or Puerto
## Rico, as they are not parsed representations of a larger area, like the 
## U.S. states. Therefore, we'll set these entries to NA instead of zero.

census_2010to2019_raw[census_2010to2019_raw$NAME %in% c("United States", "Puerto Rico" ), zeros_2010to2019] <- NA
census_2020to2023_raw[census_2020to2023_raw$NAME %in% c("United States", "Puerto Rico" ), zeros_2020to2023] <- NA


## Looks like they did. Now we are ready to proceed with inspecting differences
## with the row entries.
## 
## Recall that above we found the number of rows differed between the 2010 to
## 2019 and the 2020 to 2023 raw formats. One likely source of this difference
## could be one set calculates over specific regions in addition to the
## state-level counts. Another could be duplicated entries.

us_regions = c("United States", sort(c(datasets::state.name, "District of Columbia")) )

# Find the non-state and Puerto Rico entries.
extra_counts2010to2019 <- census_2010to2019_raw$NAME |> (\(x) { x[x %!in% us_regions] }) ()
extra_counts2020to2023 <- census_2020to2023_raw$NAME |> (\(x) { x[x %!in% us_regions] }) ()

# See which entries are and are not included in the smaller set.
matched_counts <- extra_counts2020to2023 |> (\(x) { x[x %in% extra_counts2010to2019] }) ()
matched_counts

missing_counts <- extra_counts2020to2023 |> (\(x) { x[x %!in% extra_counts2010to2019] }) ()
missing_counts


## We see that the 2010 to 2019 data set is missing entries calculating sums
## over specific division of the U.S., but all of the regional counts are
## included. We will therefore supplement row-entries for sums over the
## divisions in the 2010 to 2019 intercensal year estimates.

# Recall
us_regions_divisions

# Remove the regions that are not represented in the us_regions_divisions
# reference table.
filtered_search_space <- census_2010to2019_raw[census_2010to2019_raw$NAME %!in% c("United States", matched_counts), ] |> `rownames<-`(NULL)

# Add a divisions column by merging the two data sets by "NAME".
census_2010_regions <- merge(us_regions_divisions[, c(1, 3)], filtered_search_space, by = c("NAME"))

# Add the newly aggregated sums by rows.
census_2010to2019_raw <- rbind(
  census_2010to2019_raw,
  # Sum over the divisions to get their aggregate counts.
  aggregate(census_2010_regions[, -c(1:2)], list(census_2010_regions$Division), sum) |>
    `colnames<-`(c("NAME", colnames(census_2010_regions)[-c(1:2)]))
)


# Check that all row entries are the same. The BOOLEAN test will show TRUE
# if this is the case.
all(census_2010to2019_raw$NAME %in% census_2020to2023_raw$NAME)


## Now that the row-entries are resolved, we can proceed with mixing in
## the columns for both sets. First, we will force all of the column names
## to be standardized by removing non-alpha numeric components that may
## exist.

# Check that this step is indeed required.
any(str_detect(colnames(census_2010to2019_raw), "[^[:alnum:]]"))
any(str_detect(colnames(census_2020to2023_raw), "[^[:alnum:]]"))

# Strip the non-alpha numeric components of the names.
colnames(census_2010to2019_raw) <- str_remove_all(colnames(census_2010to2019_raw), "[^[:alnum:]]")
colnames(census_2020to2023_raw) <- str_remove_all(colnames(census_2020to2023_raw), "[^[:alnum:]]")


# Recall
unique_names

## From these results, we see that most column names are the same between
## the 2010 to 2019 and 2023 to 2023 data sets. There is one slight difference:
## "NATURALINC" and "NATURALCHG". Therefore, we will change the 2020 to 2023
## column names to adhere to the "...INC" format.

colnames(census_2020to2023_raw)[str_detect(colnames(census_2020to2023_raw), "NATURAL")] <-
  # Find column names that include the string "NATURAL".
  colnames(census_2020to2023_raw)[str_detect(colnames(census_2020to2023_raw), "NATURAL")] |>
  # Replace "...CHG" in that column name with "...INC".
  (\(x) { str_replace(x, "CHG", "INC") }) ()


## When mixing in the columns, the goal is to keep the order of information
## the same. Using the standardized names, we will build out a combined set.

# Standardized column names for the repeated sections.
use_names <- unique_names$`2010to2019` |> 
  as.vector() |>
  (\(x) { x[-c(2, 5, 7)] }) ()


## The following for loop series will combine the two data sets by column name
## then date so that the columns are organized in the same manner as the 
## raw formats.

# Start with the metadata columns.
build <- merge(census_2010to2019_raw[, c(1:3)], census_2020to2023_raw[, 1:3], by = c("NAME"))
for (i in 1:length(use_names)) {
  
  for (j in 1:length(dates_included)) {
    # If the dates are between 2010 and 2019, then build from the
    # census_2010to2019_raw data.
    if (dates_included[j] %in% c(dates_included[1:10])) {
      data = census_2010to2019_raw
      
      # If the dates are between 2020 and 2023, then build from the
      # census_2020to2023_raw data.
    } else if (dates_included[j] %in% c(dates_included[11:14])) {
      data = census_2020to2023_raw
    }
    
    # Find the column index in the data that matches the current column name
    # with current the date included.
    index <- str_detect(colnames(data), str_c("\\b", use_names[i], dates_included[j])) |> which()
    # Add this column to the building data set.
    build <- merge(build, data[, c(1, index), drop = FALSE], by = c("NAME"))
  }
  
}

## Before saving the results, we will confirm that the mixing worked.

colnames(build)

# Test that the number of columns expected to be added match. Boolean test
# will be TRUE if they do.
ncol(build) == ncol(census_2010to2019_raw) + ncol(census_2020to2023_raw) - 1

# Confirm that the representation of columns spans the date range correctly.
# recall, that census years will have 11 columns of information while all the
# remaining intercensal years will have 15 columns.
repeated_info(build, dates_included)


## These results confirm that the build was successful
census_2010to2023 <- build




## ----------------------------------------------------------------------------
## ROW-ADD THE IDB DATA

dim(census_2010to2023)
dim(census_idb_2010to2023_wide)


## Now we are ready to add the row entries representing the U.S. territories,
## sourced from the IDB. Because these two sets were generated using different
## methods, we will add column specifying which method was used to generate
## the estimates and projections: 
##    - Official U.S. Estimates   denoted by OE
##    - International Database    denoted by IDB

# Generate the new methods vector and add it to census_2010to2023.
census_2010to2023$METHOD <- rep("OE", nrow(census_2010to2023))

# Reorganize the "METHOD" column to the metadata section, next to "NAME".
census_2010to2023 <- cbind(census_2010to2023[, c("NAME", "METHOD")],
  census_2010to2023[, c(2, 4, 3, 5)],
  census_2010to2023[, -c(1:5, 204)]
)


# Generate the new methods vector and add it to census_idb_2010to2023_wide
census_idb_2010to2023_wide$METHOD <- rep("IDB", nrow(census_idb_2010to2023_wide))

# Reorganize the "METHOD" column to the metadata section, next to "NAME".
census_idb_2010to2023_wide <- cbind(census_idb_2010to2023_wide[, c("NAME", "METHOD")],
  census_idb_2010to2023_wide[, c(2, 4, 3, 5)],
  census_idb_2010to2023_wide[, -c(1:5, 204)]
)


## Before adding the census_idb_2010to2023_wide rows to census_2010to2023,
## we need to confirm that the names are the same. We can do this by
## checking there is exact matches.

all(colnames(census_2010to2023) == colnames(census_idb_2010to2023_wide))


## Because this test passes, we will now add in the census_idb_2010to2023_wide
## rows to the census_2010to2023 data set.

census_2010to2023 <- rbind(census_2010to2023, census_idb_2010to2023_wide)


## For convenience, we'll order the rows by granular regions, starting
## from the complete United States to regions, to states/territories.
order_by <- c("United States", unique(us_regions_divisions$Region)[c(1, 2, 4, 3)],
              unique(us_regions_divisions$Division),
              c(datasets::state.name, "District of Columbia"), 
              c("American Samoa", "Guam", "Northern Mariana Islands", 
                "Puerto Rico", "Virgin Islands, U.S."))


# Start building ordered set with "NAME" == "United States"
build <- census_2010to2023[census_2010to2023$NAME %in% order_by[1], ]
for (i in 2:length(order_by)) {
  # Iterate the same initial process over the remaining states/territories
  # and regions.
  ordered <- census_2010to2023[census_2010to2023$NAME %in% order_by[i], ]
  
  # Add to the previous data set to commit newly ordered row entries.
  build <- rbind(build, ordered)
}

# Commit the full, row organized data set and clear the row names.
census_2010to2023 <- build |> `rownames<-`(NULL)




## ----------------------------------------------------------------------------
## WRITING THE COMBINED FILE

write.csv(census_2010to2023, "Git and GitHub/Population Estimates and Projections/US_Census Population Estimates_2010 to 2023.csv", row.names = FALSE)







