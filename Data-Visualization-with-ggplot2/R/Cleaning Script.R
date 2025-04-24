## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Data Visualization with ggplot2
##  Authors: Shelby Golden, M.S.
##     Date: 2025-03-18
## 
##    R version: 4.4.3
## renv version: 1.0.11


## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

# NOTE: renv initializing might need to be run twice after the repo is
#       first copied.
renv::restore()


suppressPackageStartupMessages({
  library("arrow")      # For reading in the data
  library("dplyr")      # For data manipulation 
  library("ggplot2")    # For creating static visualizations
  library("MMWRweek")   # Convert Dates to MMWR day, week, and year
  library("stats")      # Compilation of statistical functions
  library("locfit")     # Local regression, likelihood and density estimation
})


# Function to select "Not In"
'%!in%' <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------
## LOAD THE HARMONIZED DATASET

# Parquet is a column-oriented data file that allows for efficient data storage 
# and lightweight information retrieval. It is best suited for large data sets 
# that cannot be easily handled "in-memory".
#
# Using the arrow() package, we can read and manipulate files in this form.
df <- read_parquet(file.path(getwd(), "Data-Visualization-with-ggplot2/Data/All Programs_Respiratory Infections.gz.parquet"))




## ----------------------------------------------------------------
## PREPARE MISSING VARIABLES

# Fortunately, the arrow() package developers made their work compatible with
# tidyverse. We will now cull the full dataset so that it is selected for
# entries associated with the RSV-NET surveillance program at the state-level.
# We will also only focus on values updated weekly, and remove entries that
# represent infections for the overall season ("Week Observed = NA").
df <- df |> 
  filter(Dataset == "RSV-NET", Region != "All Sites" & !is.na(`Week Observed`)) |>
  select(-c(Dataset, Disease, `Region Type`, `Tests Administered`,
            `Age-Adjusted Rate`, `Cumulative Age-Adjusted Rate`)) |>
  # Add scaled values for plotting.
  group_by(Region, Level) |>
  mutate(`Scaled Positives` = `Positives Detected`/max(`Positives Detected`)*100) |>
  ungroup()


# Some "Scaled Positives = NA" were induced for "Positives Detected = 0". Correct
# these values back to zero.
df |> filter(is.na(`Scaled Positives`)) |> as.data.frame()

df[is.na(df$`Scaled Positives`), "Scaled Positives"] <- 0


# In our following analysis, it will be useful to report the dates in the
# standard epidemiologic year format: MMWR.
epiDates <- df[!is.na(df$`Week Observed`), ] |> 
  (\(x) { cbind(x, MMWRweek(x$`Week Observed`)) }) () |> 
  mutate(MMWRyear = MMWRyear, 
         MMWRyear = if_else(MMWRweek <= 26, MMWRyear - 1 , MMWRyear),
         MMWRweek = if_else(MMWRweek <= 26, MMWRweek + 52, MMWRweek),
         MMWRweek = MMWRweek - 26
  ) 

# Integrate these new columns into the full dataset.
df <- left_join(df, epiDates) |>
  # Reorder the columns for clarity.
  select(colnames(df)[1:3], MMWRyear, MMWRweek, MMWRday, colnames(df)[c(4:6, 9, 7:8)]) |>
  # There are a few entries where the 
  filter(!is.na(`Scaled Positives`))


# glimpse() allows us to see the dimensions of the dataset, column names,
# the first few entries, and the vector class in one view.
df |> glimpse()




## ----------------------------------------------------------------
## SMOOTHING

# The raw data is choppy, making the visualization look coarse. We can smooth
# these values using two methods of interpolation: a polynomial spline
# or Gaussian kernel density.
# Sources:
#   - https://teazrq.github.io/SMLR/spline.html#fitting-smoothing-splines
#   - https://teazrq.github.io/SMLR/kernel-smoothing.html


# Add the respective fits for subsets grouped by "Region", "Level", and "MMWRyear"
fitted_df <- df |>
  group_by(Region, Level, MMWRyear) |>
  do({ 
    # Explicitly define the dataset, as the "." placeholder is not compatible
    # with locfit()
    data = .
    # Calculate the spline. Allow the parameters to be optimized, as the
    # results are not intended for statistical inference.
    Spline <- smooth.spline(data$MMWRweek, data$`Scaled Positives`) |> 
      (\(x) { predict(x, data$MMWRweek)$y }) ()
    # Calculate the kernel. Base setting is method Gaussian, with the other 
    # parameters tweaked to minimize errors.
    Kernel <- locfit(`Scaled Positives` ~ lp(MMWRweek, nn = 0.3, h = 0.05, deg = 2), data = data) |>
      fitted()
    data.frame(., Spline, Kernel)
  }) |>
  ungroup()


# View the fitting results by plotting by the different subsets. Change the
# value extracted from region or level to view those combinations.
select_region = unique(df$Region)[8]
select_level  = unique(df$Level)[2]

# Examine the Spline fit.
fitted_df |>
  filter(Region == select_region & Level == select_level) |>
  ggplot() +
  geom_line(aes(x = MMWRweek, y = Spline, 
                group = as.factor(MMWRyear), color = as.factor(MMWRyear) )) +
  # Add the raw data points for perspective.
  geom_point(aes(x = MMWRweek, y = Scaled.Positives, 
                 group = as.factor(MMWRyear), color = as.factor(MMWRyear) )) +
  theme_minimal() +
  xlab('Weeks since July') +
  ylab('RSV positive tests') +
  scale_colour_viridis_d()


# Examine the Gaussian Kernel fit.
fitted_df |>
  filter(Region == select_region & Level == select_level) |>
  ggplot() +
  geom_line(aes(x = MMWRweek, y = Kernel, 
                group = as.factor(MMWRyear), color = as.factor(MMWRyear) )) +
  # Add the raw data points for perspective.
  geom_point(aes(x = MMWRweek, y = Scaled.Positives, 
                 group = as.factor(MMWRyear), color = as.factor(MMWRyear) )) +
  theme_minimal() +
  xlab('Weeks since July') +
  ylab('RSV positive tests') +
  scale_colour_viridis_d()



# We see that some of the outcomes give a negative estimation for the detected
# positives. This is not possible, and so we will force all negative values
# to be zero.

fitted_df[fitted_df$Spline < 0, "Spline"] <- 0 
fitted_df[fitted_df$Kernel < 0, "Kernel"] <- 0

# Round the scaled and smoothed detection counts to the nearest whole number.
fitted_df[, c("Scaled Positives", "Spline", "Kernel")] <- 
  round(fitted_df[, c("Scaled Positives", "Spline", "Kernel")], digits = 0)

# Clean up the column names and reorder the variables for clarity.
colnames(fitted_df) <- c(colnames(df), "Spline", "Kernel")

fitted_df <- fitted_df |> 
  select(colnames(fitted_df)[1:10], "Spline", "Kernel", colnames(fitted_df)[11:12])




## ----------------------------------------------------------------
## SAVE RESULTS

write_parquet(fitted_df, "RSV-NET Infections.gz.parquet", compression = "gzip", compression_level = 5)








