## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
## 
## Workshop: Code Smarter, Not Harder: Unlocking AI Assisted Coding
##  Authors: Shelby Golden, M.S.
##     Date: 2026-03-09
## 
##    R version: 4.5.2
## renv version: 1.1.5
## 
## 
## Description: Thomas et al. 2023 analyzed NHANES cycles from 1999-2017 
##              and prepared datasets with specific metrics for their analysis. 
##              This script combines all datasets and calculates those metrics. 
##              Pre-computed results are available for participants to import 
##              directly or use throughout the worked-through example.
## 
## Sections: 
##    1) Set Up the Environment
##    2) Load in the Data
##    3) Review the Datasets
##    4) Prepare Metrics
##        4a) Mediterranean Diet Scoring
##        4b) Leisure Time and Physical Activity Scoring
##        4c) Differences and Standardization of Participant PhenoAge
##        4d) Smoking Status
##        43) Classify Body Mass Index (BMI)
##        4f) Classify Family Income to Poverty Ratio
##    7) Merging the Datasets
##    8) Filter to the Target Population


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here.
#renv::init()
renv::restore()

# Install development version of package (not yet on CRAN). Only necessary
# during the initial environment set up.
#install.packages("devtools")
#devtools::install_github("dayoonkwon/BioAge")

## Load in the R packages used in this script from the project library.
suppressPackageStartupMessages({
  library("nhanesdata") # Access compiled NHANES data
  library("BioAge")     # Estimates cellular age
  library("readr")      # For reading in the data
  library("tidyr")      # For tidying data 
  library("dplyr")      # For data manipulation
  library("stringr")    # For string manipulation
  library("ggplot2")    # For creating static visualizations
  library("gridExtra")  # Format multiple plots together
})


## This function will check if an element is not in a vector.
"%!in%" <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## NHANES is a cross-sectional study using a stratified, multistage, 
## probability-clustered sample of approximately 5,000 participants from 15 
## counties. As a cross-sectional survey, NHANES provides sampling weights to 
## produce nationally representative estimates. Weight selection varies by data 
## file combinations, and weighted datasets must be analyzed using 
## survey-specific functions (e.g., svyglm instead of glm).
##
## We use unweighted data for consistency and simplicity. This approach will also
## allow us to use external resources like the BioAge R package's PhenoAge 
## projections, which were trained and computed without applying survey weights. 
## Still, merging biannual NHANES cycles requires several considerations:
##
## First, each cycle collects multiple datasets with different structures 
## (demographics, dietary, examination, etc.). Some datasets have one row per 
## participant, while others, like dietary data, contain multiple rows per 
## participant.
##
## Second, variable names and coding may differ across cycles, requiring 
## harmonization.
##
## Fortunately, pre-merged and harmonized datasets are available through the 
## nhanesdata R package, which we use to load our data.

# Load demographics data and reference the Codebook (data dictionary) URL
demo <- read_nhanes("demo")
demo_var <- get_url("demo")

# Load dietary individual food data for day 1 and 2, and reference the Codebook URLs
diet1 <- read_nhanes("dr1iff")
diet2 <- read_nhanes("dr2iff")

diet1_var <- get_url("dr1iff_l")
diet2_var <- get_url("dr2iff_l")

# Load physical activity data and reference the Codebook URL
physical_1999to05 <- read_nhanes("paqiaf")
physical_2007plus <- read_nhanes("paq")

physical_var <- get_url("paqiaf")
physical_var <- get_url("paq_e")

# Load smoking - cigarette/tobacco use data and reference the Codebook URL
smoking <- read_nhanes("smq")

smoking_var <- get_url("smq")

# Load body measures data and reference the Codebook URL
body_measures <- read_nhanes("bmx")

body_measures_var <- get_url("bmx")

## For scoring purposes, we will need to associate food groups from participants' 
## 24-hour dietary recalls with Mediterranean diet categories. The pre-prepared 
## mapping datasets are imported here; refer to "Formatting and Saving FNDDS 
## Encodings.R" for details on how these datasets were constructed.

fndds_2021_23_food_code <- read_csv(file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "FNDDS Food Group Codes_2021 to 23.csv"),
                                    col_types = cols(code = col_character()))

mediterranean_groups <- read_csv(file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Mediterranean Diet Food Groups_Trichopoulou et al 2003.csv"),
                                 col_types = cols(code = col_character()))

## PhenoAge estimates are loaded from pre-computed BioAge package values.
phenoage = phenoage_nhanes(biomarkers=c("albumin_gL", "alp", "lncrp", "totchol", "lncreat_umol", "hba1c", "sbp", "bun", "uap", "lymph", "mcv", "wbc"))




## ----------------------------------------------------------------------------
## REVIEW THE DATASETS

## Great—we now have all the discrete datasets needed for our analysis. Each 
## represents all available cross-sectional iterations of the Continuous NHANES 
## survey. For our analysis, we need specific variables, some calculated 
## variables, and ideally, all data sources combined into a single table.
##
## Before we proceed with merging, two things to keep in mind:
##    1) Not all survey cycles collected identical datasets. The good news is 
##       that variations across biannual iterations have already been addressed 
##       in the pre-merged and harmonized datasets.

demo$year              |> unique()
diet1$year             |> unique()
diet2$year             |> unique()
physical_1999to05$year |> unique()
physical_2007plus$year |> unique()

##    2) Merging by participant ID and year is complicated by the fact that some 
##       datasets have multiple rows per participant ID (one-to-many rather than 
##       one-to-one).

demo              |> count(seqn) |> pull(n) |> unique()
diet1             |> count(seqn) |> pull(n) |> unique()
diet2             |> count(seqn) |> pull(n) |> unique()
physical_1999to05 |> count(seqn) |> pull(n) |> unique()
physical_2007plus |> count(seqn) |> pull(n) |> unique()


## The individual foods dataset captures up to 62 different food types that 
## each participant consumed in the preceding 24 hours. Our analysis uses these 
## records to score adherence to the Mediterranean diet (Trichopoulou et al., 
## 2003), then collapses the multiple food entries into a single row per 
## participant.

diet1 |> pull(dr1iline) |> unique()
diet2 |> pull(dr2iline) |> unique()

## Additionally, physical activity tracking methods changed fundamentally during 
## Continuous NHANES. From 1999 to 2005, up to 48 individual physical activities 
## were tracked. From 2007 onward, the WHO Global Physical Activity Questionnaire 
## was used instead, which tracked the total number of days and minutes 
## participants engaged in moderate or vigorous physical activity. To account 
## for this methodological shift, our analysis calculates total Leisure-Time 
## Physical Activity (LTPA) separately for each tracking approach.

physical_1999to05 |> pull(padactiv) |> unique()




## ----------------------------------------------------------------------------
## PREPARE METRICS

# -----------------------------
# MEDITERRANEAN DIET SCORING

## Thomas et al. (2023) assessed participants' Mediterranean diet adherence 
## using a "MeDi score":
##    1) Sex-specific medians are calculated for each food category as reference 
##       values. Alcohol and fat consumption is scored using different criteria.
## 
##    2) For beneficial foods (vegetables, potatoes, fruits, legumes and nuts, 
##       fish, cereals), participants receive +1 point if their caloric intake 
##       meets or exceeds the median.
## 
##    3) For detrimental foods (meat, poultry, dairy products), participants 
##       receive +1 point if their caloric intake is below the median.
## 
##    4) For alcohol, +1 point is awarded for mild-to-moderate consumption (0–1 
##       drink per day for females; 0–2 drinks per day for males).
## 
##    5) For fats, particiants with a high monosaturated to saturated ratio
##       receive +1 point if their caloric intake meets or exceeds the median.

# Summarize the number of FNDDS 
scoring_groups <- mediterranean_groups |>
  group_by(med_group, benefice) |>
  summarise(count = n(), .groups = "drop") |>
  pivot_wider(names_from = benefice, values_from = count, values_fill = list(count = 0))

scoring_groups |> filter(Yes > 0)
scoring_groups |> filter(No > 0)
scoring_groups |> filter(`N/A` > 0)

# Some participants only completed one 24-hour dietary recall (either Day 1 or 
# Day 2). We process each day separately, then merge them when averaging dietary 
# records across days.
all(diet1_annotated$seqn %in% diet2_annotated$seqn == TRUE)
all(diet2_annotated$seqn %in% diet1_annotated$seqn == TRUE)

# Isolate the relevant columns needed for the following calculations and merge
# in participant gender from the DEMO file. NOTE: the dataset contains a gender
# variable but does not include a separate variable for biological sex.
diet1_sub <- diet1 |>
  select(seqn, dr1ifdcd, dr1ikcal, dr1iprot, dr1icarb, dr1isugr, dr1itfat, 
         dr1imfat, dr1isfat, dr1icalc, dr1ialco) |>
  left_join(demo |> select(seqn, riagendr), by = c("seqn"))

diet2_sub <- diet2 |>
  select(seqn, dr2ifdcd, dr2ikcal, dr2iprot, dr2icarb, dr2isugr, dr2itfat, 
         dr2imfat, dr2isfat, dr2icalc, dr2ialco) |>
  left_join(demo |> select(seqn, riagendr), by = c("seqn"))

# Annotate individual food records using the Mediterranean diet classification 
# scheme described above.
diet1_annotated <- diet1_sub |>
  mutate(
    dr1ifdcd = as.character(format(dr1ifdcd, scientific = FALSE)),
    first_two_digits = str_sub(dr1ifdcd, 1, 2),
  ) |>
  left_join(mediterranean_groups, by = c("first_two_digits" = "code"))

diet2_annotated <- diet2_sub |>
  mutate(
    dr2ifdcd = as.character(format(dr2ifdcd, scientific = FALSE)),
    first_two_digits = str_sub(dr2ifdcd, 1, 2),
  ) |>
  left_join(mediterranean_groups, by = c("first_two_digits" = "code"))

# Confirm that there were no missed matches after joining the tables
diet1_annotated |> filter(med_group %in% NA) |> pull(first_two_digits) |> unique()
diet2_annotated |> filter(med_group %in% NA) |> pull(first_two_digits) |> unique()

# As a sanity check, we examine the distribution of key nutrients (protein, 
# carbohydrates, sugar, total fat, calcium, and alcohol) to confirm that 
# Mediterranean food groups are correctly associated. Obvious deviations would 
# indicate potential issues with the joining process that require investigation.
diet1_annotated |>
  group_by(med_group) |>
  summarize(
    avg_prot = mean(dr1iprot, na.rm = TRUE),
    avg_carb = mean(dr1icarb, na.rm = TRUE),
    avg_sugar = mean(dr1isugr, na.rm = TRUE),
    avg_fat = mean(dr1itfat, na.rm = TRUE),
    avg_cal = mean(dr1icalc, na.rm = TRUE),
    avg_alc = mean(dr1ialco, na.rm = TRUE),
    .groups = 'drop'
  )

## Entries that report food consumed but lack nutritional attributes (such as 
## calories) are removed. Retaining these incomplete records would bias our 
## dietary calculations.

diet1_annotated <- diet1_annotated |> filter(!is.na(dr1ikcal))
diet2_annotated <- diet2_annotated |> filter(is.na(dr2ikcal))

# Calculate the monosaturated:saturated ratio, number of drinks consumed per
# day, and total calories consumed.
diet1_fat_alc <- diet1_annotated |>
  group_by(seqn, riagendr) |>
  summarise(
    total_dr1ikcal = sum(dr1ikcal, na.rm = TRUE),
    total_dr1imfat = sum(dr1imfat, na.rm = TRUE),
    total_dr1isfat = sum(dr1isfat, na.rm = TRUE),
    total_dr1ialco = sum(dr1ialco, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup()

diet2_fat_alc <- diet2_annotated |>
  group_by(seqn, riagendr) |>
  summarise(
    total_dr2ikcal = sum(dr2ikcal, na.rm = TRUE),
    total_dr2imfat = sum(dr2imfat, na.rm = TRUE),
    total_dr2isfat = sum(dr2isfat, na.rm = TRUE),
    total_dr2ialco = sum(dr2ialco, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup()

# Average the results for monosaturated:saturated ratio and number of drinks 
# consumed per day over the two days recorded, as defined by the National 
# Institute on Alcohol Abuse and Alcoholism (NIAAA).
diet_fat_alc <- full_join(diet1_fat_alc, diet2_fat_alc, by = c("seqn", "riagendr")) |>
  rowwise() |>
  mutate(
    avg_kcal = mean(c(total_dr1ikcal, total_dr2ikcal), na.rm = TRUE),
    sd_kcal  = sd(c(total_dr1imfat, total_dr2imfat), na.rm = TRUE),
    avg_mono = mean(c(total_dr1imfat, total_dr2imfat), na.rm = TRUE),
    sd_mono  = sd(c(total_dr1imfat, total_dr2imfat), na.rm = TRUE),
    avg_sat  = mean(c(total_dr1isfat, total_dr2isfat), na.rm = TRUE),
    sd_sat   = sd(c(total_dr1isfat, total_dr2isfat), na.rm = TRUE),
    avg_drialco = mean(c(total_dr1ialco, total_dr2ialco), na.rm = TRUE),
    avg_drinks_per_day = round(avg_drialco / 14),
    sd_drinks_per_day  = sd(c(total_dr1ialco, total_dr2ialco), na.rm = TRUE),
    ratio_mono_to_sat  = avg_mono / avg_sat,
    sd_ratio_mono_to_sat = abs(ratio_mono_to_sat) * sqrt((sd_mono / avg_mono)^2 + (sd_sat / avg_sat)^2),
  ) |>
  ungroup() |>
  select(-total_dr1ikcal, -total_dr2ikcal, -total_dr1imfat, -total_dr1isfat, 
         -total_dr1ialco, -total_dr2imfat, -total_dr2isfat, -total_dr2ialco,
         -avg_drialco, -avg_mono, -sd_mono, -avg_sat, -sd_sat)

# Calculate the total caloric intake (kcal) by both beneficial and deleterious
# food groups.
diet1_food_groups <- diet1_annotated |>
  filter(benefice != "N/A", med_group != "alcoholic beverages") |>
  group_by(seqn, riagendr, med_group, benefice) |>
  summarise(total_dr1ikcal = sum(dr1ikcal, na.rm = TRUE), .groups = "drop") |>
  ungroup()

diet2_food_groups <- diet2_annotated |>
  filter(benefice != "N/A", med_group != "alcoholic beverages") |>
  group_by(seqn, riagendr, med_group, benefice) |>
  summarise(total_dr2ikcal = sum(dr2ikcal, na.rm = TRUE), .groups = "drop") |>
  ungroup()

# Average the results over the two days recorded. 
diet_food_groups <- full_join(diet1_food_groups, diet2_food_groups, by = c("seqn", "riagendr", "med_group", "benefice")) |>
  rowwise() |>
  mutate(
    avg_total_kcal = mean(c(total_dr1ikcal, total_dr2ikcal), na.rm = TRUE),
    sd_total_kcal = sd(c(total_dr1ikcal, total_dr2ikcal), na.rm = TRUE)
  ) |>
  ungroup()

# Calculate the threshold (sex-based mean) for all eating outcomes.
mean_fat_cal_groups <- diet_fat_alc |>
  group_by(riagendr) |>
  summarise(mean_ratio_mono_to_sat = mean(ratio_mono_to_sat, na.rm = TRUE),
            .groups = "drop") |>
  ungroup()

mean_food_groups <- diet_food_groups |>
  group_by(riagendr, med_group) |>
  summarise(mean_total_kcal = mean(avg_total_kcal, na.rm = TRUE), .groups = "drop") |>
  ungroup()

# Calculate the MeDi scores and then combine the MeDi scores from both sources
# into one final table.
diet_fat_alc_MeDi <- diet_fat_alc |>
  left_join(mean_fat_cal_groups, by = c("riagendr")) |>
  mutate(
    MeDi_mono_to_sat = if_else(ratio_mono_to_sat >= mean_ratio_mono_to_sat, 1, 0, missing = 0),
    MeDi_alc = if_else((riagendr == "Female" & avg_drinks_per_day <= 1) |
                         (riagendr == "Male" & avg_drinks_per_day <= 2), 1, 0, missing = 0)
  ) |>
  replace_na(list(MeDi_mono_to_sat = 0, MeDi_alc = 0)) |>
  (\(x) x |> mutate(sum_MeDi = rowSums(select(x, MeDi_mono_to_sat, MeDi_alc))))() |>
  select(-avg_drinks_per_day, -sd_drinks_per_day, -ratio_mono_to_sat, 
         -sd_ratio_mono_to_sat, -mean_ratio_mono_to_sat, -MeDi_mono_to_sat,
         -MeDi_alc)

diet_food_groups_MeDi <- diet_food_groups |>
  left_join(mean_food_groups, by = c("riagendr", "med_group")) |>
  mutate(MeDi_food_groups = case_when(
    benefice == "Yes" & avg_total_kcal >= mean_total_kcal ~ 1,
    benefice == "No" & avg_total_kcal <= mean_total_kcal ~ 1,
    TRUE ~ 0
  )) |> 
  as.data.frame() |>
  group_by(seqn) |>
  summarise(sum_MeDi = sum(MeDi_food_groups), .groups = "drop")

MeDi_scores <- diet_fat_alc_MeDi |>
  full_join(diet_food_groups_MeDi, by = c("seqn")) |>
  (\(x) x |> mutate(sum_MeDi = rowSums(select(x, sum_MeDi.x, sum_MeDi.y), na.rm = TRUE)))() |>
  select(-riagendr, -sum_MeDi.x, -sum_MeDi.y) |>
  rename(MeDi = sum_MeDi)

# Save the result
write_csv(MeDi_scores, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "MeDi Scores.csv"))


# -----------------------------
# LEISURE TIME AND PHYSICAL ACTIVITY SCORING

## As noted previously, physical activities were fundamentally tracked in two
## different manners. For both of these surveys, we want to get the total number
## of minutes each week participants engage in moderate or vigorous physical
## activities-frequency for each multiplied by their duration.
## 
## To calculate total weekly activity minutes, we need to multiply days per 
## week by minutes per day for both moderate and vigorous activity. Some 
## participants reported only days without minutes, but these cases are rare 
## (<1% of entries) and are excluded from the calculation.

# Evaluate whether any cases have days reported without minutes (or vice versa)
test <- physical_1999to05 |>
  mutate(
    dayNA = is.na(padtimes) & !is.na(paddurat),
    minNA = !is.na(padtimes) & is.na(paddurat)
  ) |>
  select(-year, -padactiv, -padlevel, -padmets, -paaquex)

# Summarize results to show if any cases are TRUE
test |> 
  summarise(across(ends_with("NA"), ~any(.x))) |>
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Boolean")

# Count participants with incomplete activity records
test |>
  summarise(
    dayNA = sum(dayNA & !minNA),
    minNA = sum(!dayNA & minNA),
  ) |>
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Count")

# Tag the rows for removal
physical_1999to05 <- physical_1999to05 |>
  mutate(dayNA = is.na(padtimes) & !is.na(paddurat))

# Calculate total minutes participants engaged in physical activity
physical_scores_1999to05 <- physical_1999to05 |>
  filter(!dayNA) |>
  mutate(
    total_mod_weekly = if_else(
      padlevel == "MODERATE",
      (padtimes * paddurat) / 4.33,
      NA_real_
    ),
    total_vig_weekly = if_else(
      padlevel == "VIGOROUS",
      (padtimes * paddurat) / 4.33,
      NA_real_
    )
  ) |>
  select(seqn, total_mod_weekly, total_vig_weekly)

## Moderate and vigorous physical activities were captured in one variable 
## (padactiv - leisure time activity). If someone reported only one intensity 
## level, we can treat the missing value as zero. However, if neither intensity 
## was reported, we retain NA (cannot assume zero activity).

# Calculate the sum over entries associated with one participant
physical_scores_1999to05 <- physical_scores_1999to05 |>
  group_by(seqn) |>
  summarize(
    total_mod_weekly = sum(total_mod_weekly, na.rm = TRUE),
    total_vig_weekly = sum(total_vig_weekly, na.rm = TRUE)
  ) |>
  mutate(
    total_mod_weekly = if_else(
      is.na(total_mod_weekly) & is.na(total_vig_weekly), 
      NA, 
      total_mod_weekly
    ),
    total_vig_weekly = if_else(
      is.na(total_mod_weekly) & is.na(total_vig_weekly), 
      NA, 
      total_vig_weekly
    )
  )

## We compiled physical activity data from 1999 to 2005 using individual reports 
## (above). For the remaining analysis, we focus on surveys from 2007 to 2017, 
## selecting only relevant years and variables. We exclude post-2017 data as it 
## falls outside our analysis period.

# Update the compiled Physical Activity survey
physical_2007plus <- physical_2007plus |>
  filter(year %!in% c(1999, 2001, 2003, 2005, 2021)) |>
  select(where(~ !all(is.na(.))))

## Variable availability is inconsistent across cycles—some appear throughout, 
## others only in later iterations. We retain only variables measuring 
## participants' average moderate and vigorous physical activity levels.
## 
##                  Work          Recreation
##   Vigorous:  PAQ605:PAD615   PAQ650:PAD660
##   Moderate:  PAQ620:PAD630   PAQ665:PAD675

# Subset the dataset to variables measuring moderate and vigorous activity
subset_physical_2007plus <- physical_2007plus |>
  select(seqn, year, paq605, paq610, pad615, paq620, paq625, pad630,
         paq650, paq655, pad660, paq665, paq670, pad675)
  
# Confirm data exists for all biannual cycles
subset_physical_2007plus |>
  group_by(year) |>
  summarise(across(everything(), ~ all(is.na(.))))

## Exclude "Refused" and "Don't know" responses from vigorous and moderate work 
## and recreational activity variables. These responses may appear as string 
## labels or numeric codes depending on the dataset. The numeric encodings are 
## 77 and 99 for "days" variables, and 7777 and 9999 for "minutes" variables.

# Confirm consistent nomenclature
vars <- colnames(subset_physical_2007plus)[-c(1:2)]

tibble::tibble(
  variable = colnames(subset_physical_2007plus)[-c(1:2)],
  unique_values = purrr::map_chr(subset_physical_2007plus[vars], ~ paste(sort(unique(.x)), collapse = " | ")),
  n_unique = purrr::map_int(subset_physical_2007plus[vars], ~ dplyr::n_distinct(.x, na.rm = FALSE))
)

# Remove the incomplete responses
subset_physical_2007plus <- subset_physical_2007plus |>
  filter(paq605 %!in% c("Don't know", "Refused") | 
           paq620 %!in% c("Don't know", "Refused") |
           paq650 %!in% c("Don't know", "Refused") | 
           paq665 %!in% c("Don't know", "Refused") |
           paq610 %!in% c(77, 99) | paq625 %!in% c(77, 99) |
           paq655 %!in% c(77, 99) | paq670 %!in% c(77, 99) |
           pad615 %!in% c(7777, 9999) | pad630 %!in% c(7777, 9999) |
           pad660 %!in% c(7777, 9999) | pad675 %!in% c(7777, 9999))

## To calculate total weekly activity minutes, we need to multiply days per 
## week by minutes per day for both moderate and vigorous activity. Some 
## participants reported only days without minutes, but these cases are rare 
## (<1% of entries) and are excluded from the calculation.

# Evaluate whether any cases have days reported without minutes or vice versa
test <- subset_physical_2007plus |>
  mutate(
    vig_work_dayNA = is.na(paq610) & !is.na(pad615),
    vig_work_minNA = !is.na(paq610) & is.na(pad615),
    mod_work_dayNA = is.na(paq625) & !is.na(pad630),
    mod_work_minNA = !is.na(paq625) & is.na(pad630),
    vig_rec_dayNA  = is.na(paq655) & !is.na(pad660),
    vig_rec_minNA  = !is.na(paq655) & is.na(pad660),
    mod_rec_dayNA  = is.na(paq670) & !is.na(pad675),
    mod_rec_minNA  = !is.na(paq670) & is.na(pad675)
  ) |>
  select(-paq605, -paq610, -pad615, -paq620, -paq625, -pad630, 
         -paq650, -paq655, -pad660, -paq665, -paq670, -pad675)

# Summarize results to show if any cases are TRUE
test |> 
  summarise(across(ends_with("NA"), ~any(.x))) |>
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Boolean")

## Missingness is unidirectional: only cases where the minutes is missing but 
## days are recorded exist. There are no instances of the reverse pattern.
## We cannot calculate use these in our analysis.

# Count participants with incomplete activity records
test |>
  summarise(
    vig_work_count = sum(!vig_work_dayNA & vig_work_minNA),
    mod_work_count = sum(!mod_work_dayNA & mod_work_minNA),
    vig_rec_count  = sum(!vig_rec_dayNA & vig_rec_minNA),
    mod_rec_count  = sum(!mod_rec_dayNA & mod_rec_minNA)
  ) |>
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Count")

# Tag the rows for removal
subset_physical_2007plus <- subset_physical_2007plus |>
  mutate(
    vig_work_minNA = !is.na(paq610) & is.na(pad615),
    mod_work_minNA = !is.na(paq625) & is.na(pad630),
    vig_rec_minNA  = !is.na(paq655) & is.na(pad660),
    mod_rec_minNA  = !is.na(paq670) & is.na(pad675)
  )

## We expect different participants to have missing data for different activity 
## types. Removing all flagged rows together would unnecessarily discard valid 
## observations. We handle each activity type separately to retain maximum data, 
## then merge the cleaned results.

vig_work <- subset_physical_2007plus |>
  select(seqn, paq605, paq610, pad615, vig_work_minNA) |>
  filter(!vig_work_minNA) |>
  mutate(
    vig_work_weekly = case_when(
      paq605 == "Yes"  ~ paq610 * pad615,
      paq605 == "No"   ~ 0,
      TRUE             ~ NA_real_
    )
  ) |>
  select(-paq605, -paq610, -pad615, -vig_work_minNA)

vig_rec <- subset_physical_2007plus |>
  select(seqn, paq650, paq655, pad660, vig_rec_minNA) |>
  filter(!vig_rec_minNA) |>
  mutate(
    vig_work_weekly = case_when(
      paq650 == "Yes"  ~ paq655 * pad660,
      paq650 == "No"   ~ 0,
      TRUE             ~ NA_real_
    )
  ) |>
  select(-paq650, -paq655, -pad660, -vig_rec_minNA)

mod_work <- subset_physical_2007plus |>
  select(seqn, paq620, paq625, pad630, mod_work_minNA) |>
  filter(!mod_work_minNA) |>
  mutate(
    vig_work_weekly = case_when(
      paq620 == "Yes"  ~ paq625 * pad630,
      paq620 == "No"   ~ 0,
      TRUE             ~ NA_real_
    )
  ) |>
  select(-paq620, -paq625, -pad630, -mod_work_minNA)

mod_rec <- subset_physical_2007plus |>
  select(seqn, paq665, paq670, pad675, mod_rec_minNA) |>
  filter(!mod_rec_minNA) |>
  mutate(
    vig_work_weekly = case_when(
      paq665 == "Yes"  ~ paq670 * pad675,
      paq665 == "No"   ~ 0,
      TRUE             ~ NA_real_
    )
  ) |>
  select(-paq665, -paq670, -pad675, -mod_rec_minNA)

## Great! Now we can combine all activity types to calculate total weekly 
## minutes of moderate and vigorous physical activity for each participant.

# Join together all the tables
cal_physical_2007plus <- vig_work |>
  full_join(vig_rec, by = "seqn") |>
  full_join(mod_work, by = "seqn") |>
  full_join(mod_rec, by = "seqn") |> 
  `colnames<-`(c("seqn", "vig_work_weekly", "vig_rec_weekly", 
                 "mod_work_weekly", "mod_rec_weekly"))

# Calculate total minutes, work + recreational. NAs are ignored in the sum, but 
# if both are NA, keep the result as NA.
cal_physical_2007plus <- cal_physical_2007plus |>
  rowwise() |>
  mutate(
    total_vig_weekly = ifelse(
      is.na(vig_work_weekly) & is.na(vig_rec_weekly),
      NA,
      sum(c(vig_work_weekly, vig_rec_weekly), na.rm = TRUE)
    ),
    total_mod_weekly = ifelse(
      is.na(mod_work_weekly) & is.na(mod_rec_weekly),
      NA,
      sum(c(mod_work_weekly, mod_rec_weekly), na.rm = TRUE)
    )
  ) |>
  ungroup() %>%
  select(-vig_work_weekly, -vig_rec_weekly, -mod_work_weekly, -mod_rec_weekly)

# Combine results from all participants
physical_scores <- bind_rows(physical_scores_1999to05, cal_physical_2007plus)

## Great! As a final step we need to calculate the metabolic equivalent of task 
## (MET). This is done by multiplying the intensity-specific minutes per week
## with either 4.0 MET for moderate or 8.0 MET for vigorous intensity LTPA. We
## will then classify each by four levels:
##    - 0 MET min/wk as sedentary (no regular physical activity)
##    - <500 MET min/wk as low (insufficient regular activity)
##    - 500–1000 MET min/wk as moderate
##    - >1 000 MET min/wk as high

physical_scores <- physical_scores |>
  mutate(
    MET = if_else(
      is.na(total_mod_weekly) & is.na(total_vig_weekly),
      NA_real_,
      rowSums(cbind(4*total_mod_weekly, 8*total_vig_weekly), na.rm = TRUE)
    ),
    activity_level = case_when(
      MET == 0 ~ "Sedentary",
      MET < 500 ~ "Low",
      MET >= 500 & MET <= 1000 ~ "Moderate",
      MET > 1000 ~ "High"
    )
  ) |>
  select(seqn, MET, activity_level)

# Save the result
write_csv(physical_scores, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Physical MET Scores.csv"))


# -----------------------------
# DIFFERENCES AND STANDARDIZATION OF PARTICIPANT PHENOAGE

## Biological age is estimated using pre-computed PhenoAge algorithm projections. 
## PhenoAge advancement is calculated as the difference between biological and 
## chronological age, then standardized to mean = 0 and SD = 1.
##
## The BioAge package provides three biological aging algorithms:
##    - Klemera-Doubal method (KDM)
##    - PhenoAge using "Levine Original" formula (9 biomarkers)
##    - PhenoAge using "Modified Levine" formula (12 biomarkers)
##
## NOTE: The BioAge package also includes homeostatic dysregulation measures, 
## but these were not included in pre-computed projections for NHANES 1999-2017. 
## We retain both PhenoAge variants (Original and Modified) for our analyses.

# Subset to phenoage projections and clarify variable naming
phenoage_sub <- phenoage$data |>
  (\(x) x |> mutate(seqn = str_split_fixed(x$sampleID, "_", 2)[, 2]))() |>
  select(seqn, age, phenoage0, phenoage) |>
  rename(phenoage_original = phenoage0, phenoage_modified = phenoage)

# Calculate age advancement and standardize
phenoage_advancement <- phenoage_sub |> 
  mutate(
    advancement_original = phenoage_original - age,
    advancement_modified =  phenoage_modified - age,
    advancement_original_std = scale(advancement_original)[, 1],
    advancement_modified_std = scale(advancement_modified)[, 1]
  ) |>
  mutate(seqn = as.integer(seqn)) |>
  select(seqn, phenoage_original, phenoage_modified, advancement_original,
         advancement_modified, advancement_original_std, advancement_modified_std)

# Confirm visually that the scaling was successful
p1 <- ggplot() +
  geom_histogram(data = phenoage_advancement, aes(x = phenoage_original, fill = "Original"), 
                 position = "identity", alpha = 0.5, bins = 30, color = "blue") +
  geom_histogram(data = phenoage_advancement, aes(x = phenoage_modified, fill = "Modified"), 
                 position = "identity", alpha = 0.5, bins = 30, color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "PhenoAge Advancement - Unscaled",
       x = "Advancement", y = "Frequency") +
  scale_fill_manual(values = c("Original" = "blue", "Modified" = "red"),
                    name = "Advancement Type",
                    labels = c("Original", "Modified")) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ggplot() +
  geom_histogram(data = phenoage_advancement, aes(x = advancement_original_std, fill = "Original"), 
                 position = "identity", alpha = 0.5, bins = 30, color = "blue") +
  geom_histogram(data = phenoage_advancement, aes(x = advancement_modified_std, fill = "Modified"), 
                 position = "identity", alpha = 0.5, bins = 30, color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "PhenoAge Advancement - Scaled",
       x = "Advancement", y = "Frequency") +
  scale_fill_manual(values = c("Original" = "blue", "Modified" = "red"),
                    name = "Advancement Type",
                    labels = c("Original", "Modified")) +
  theme_minimal() +
  theme(legend.position = "none")

# Arrange the two plots side-by-side
grid.arrange(p1, p2, ncol = 2)

# Save the result
write_csv(phenoage_advancement, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "PhenoAge Advancement.csv"))


# -----------------------------
# SMOKING STATUS

## Three lifestyle factors were evaluated: smoking status, body mass index 
## (BMI), and total caloric intake from 24-hour dietary recall. Smoking 
## status is not directly captured in NHANES and must be constructed from 
## two variables: (1) lifetime cigarette use (≥ 100 cigarettes), and 
## (2) current smoking status.
## 
## We generate three categories:
##  - Never smoker: < 100 cigarettes in lifetime
##  - Former smoker: ≥ 100 lifetime cigarettes AND does not currently smoke
##  - Current smoker: ≥ 100 lifetime cigarettes AND currently smokes

smoking_sub <- smoking |>
  select(seqn, smq020, smq040)

## We verify that respondents who reported smoking at least 100 lifetime 
## cigarettes also provided current smoking frequency. Records with 
## inconsistent responses (e.g., "Yes" to 100 lifetime cigarettes but NA 
## for current smoking) or "Don't know"/"Refused" responses for either 
## variable are recoded as NA.

# Some outcomes in smq040 have nomenclature deviations
smoking_sub$smq040 |> unique()

# Standardize the nomenclature
smoking_sub <- smoking_sub %>%
  mutate(smq040 = case_when(
    smq040 == "Not at all?"       ~ "Not at all",
    smq040 == "Every day,"        ~ "Every day",
    smq040 == "Some days, or"     ~ "Some days",
    TRUE                          ~ smq040
  ))

# Summarize the unique combinations of outcomes
smoking_sub %>%
  select(-seqn) %>%
  distinct()

# Code the smoking status
smoking_status <- smoking_sub %>%
  mutate(smoking_status = case_when(
    smq020 == "No" ~ "Never",
    smq020 == "Yes" & smq040 == "Not at all" ~ "Former",
    smq020 == "Yes" & (smq040 == "Every day" | smq040 == "Some days") ~ "Current",
    TRUE ~ NA_character_
  )) |>
  select(-smq020, -smq040)

# Confirm the correct attribute was applied
smoking_sub %>%
  select(-seqn) %>%
  distinct()

# Save the result
write_csv(smoking_sub, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Smoking Status.csv"))


# -----------------------------
# CLASSIFY BODY MASS INDEX (BMI)

body_measures_sub <- body_measures |> 
  select(seqn, bmxbmi)

# Code the smoking status
bmi <- body_measures_sub %>%
  mutate(bmi_category = case_when(
    bmxbmi < 25 ~ "Normal weight",
    bmxbmi >= 25 & bmxbmi < 30 ~ "Overweight",
    bmxbmi >= 30 ~ "Obesity",
    TRUE ~ NA_character_
  )) |>
  select(-bmxbmi)

# Save the result
write_csv(bmi, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Body Mass Index (BMI) Categories.csv"))


# -----------------------------
# CLASSIFY FAMILY INCOME TO POVERTY RATIO

## We retain the following demographic variables: chronological age, gender, 
## ethnicity, education level (adults 20+), marital status, and family poverty 
## income ratio (PIR). Family PIR is categorized into low, middle, and high 
## income groups and later the population is restricted to non-pregnant 
## participants only.

demo_sub <- demo |>
  select(seqn, year, sddsrvyr, ridageyr, riagendr, ridreth1, dmdeduc2, dmdmartl, indfmpir, ridexprg)

# Code the smoking status
demo_calc <- demo_sub %>%
  mutate(family_pir_category = case_when(
    indfmpir < 1 ~ "Below the federal poverty level",
    indfmpir >= 1 & indfmpir < 4 ~ "Middle income",
    indfmpir >= 4 ~ "High income",
    TRUE ~ NA_character_
  )) |>
  select(-indfmpir) |>
  select(seqn, year, sddsrvyr, ridageyr, riagendr, ridreth1, dmdeduc2, dmdmartl, family_pir_category, ridexprg)

# Save the result
write_csv(demo_calc, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Demographics.csv"))



## ----------------------------------------------------------------------------
## MERGING THE DATASETS

## With all of our metrics calculated, we are ready to merge everything together
## for our analysis. This dataframe will be 

df_merged <- demo_sub |>
  left_join(smoking_status, by = c("seqn")) |>
  left_join(body_measures_sub, by = c("seqn")) |>
  left_join(physical_scores, by = c("seqn")) |>
  left_join(MeDi_scores, by = c("seqn")) |>
  left_join(phenoage_advancement, by = c("seqn"))

# Save the result
write_csv(df_merged, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Combined NHANES Dataset.csv"))


## ----------------------------------------------------------------------------
## FILTER TO THE TARGET POPULATION


# years old who were seen at the medical examination, we excluded participants 
# with missing data for diet or leisure time physical activity (LTPA; n = 4 181) 
# and blood chemistries (n = 3 507; Figure 1). In total, 42 625 participants 
# were included in the analysis.

# After compiling the datasets, there are a few details that they 

# There are more people in our starting, merged dataset than is indicated in
# the paper.
nrow(df_merged) - 101316

## 1. Keep years 1999 to 2018, or cycles 1999-2000 to 2017-2018.

# Display unique years, NHANES cycles, represented
df_merged$year |> unique()

df_filtered <- df_merged |>
  filter(year %in% seq(1999, 2017, by = 2))


## 2. Keep non-pregnant participants. Based on the exclusion count reported 
##    in the paper (n = 1,541), it appears that females who indicated they were 
##    pregnant were removed, while those with an undetermined pregnancy status 
##    were retained.

# Display unique values for participant pregnancy status
df_merged$ridexprg |> unique()

# There are more women who are pregnant that was indicated in the paper
table(df_filtered$ridexprg)

df_filtered <- df_filtered |>
  filter(riagendr == "Male" | 
           (riagendr == "Female" & ridexprg %!in% "Yes, positive lab pregnancy test or self-reported pregnant at exam"))


## 3. Keep participants between 20 and 85 years of age, that is [20, 85).

# Display unique values for participant pregnancy status
df_filtered$ridageyr |> unique() |> sort()

df_filtered <- df_filtered |>
  filter(ridageyr %in% c(20:84))

# At this stage, the exclusion count aligns with the number reported in the paper
nrow(df_filtered) - 52738


## 4. Exclude participants with missing BMI values. Note that this differs from 
##    the paper's exclusion criterion, which removed participants who did not 
##    complete a medical examination.

# The paper selected out 2,425 participants, but we select out almost 1,000 more.
table(is.na(df_filtered$bmxbmi))

df_filtered <- df_filtered |>
  filter(!is.na(bmxbmi))


## 5. Exclude participants with NA for leisure of dietary data. Note that this
##    again differs from the paper, as these metrics were calculated using
##    the full dataset. NA's were applied when missing or incomplete data
##    were present.

## Considerably more participants were identified as having partial or missing 
## leisure activity data compared to the paper, with our count exceeding theirs 
## by approximately 10,000. The paper's methods do not clearly describe how 
## missing data were detected, and the approach may vary depending on which of 
## the two datasets was being evaluated. In our analysis, any partially missing 
## data were flagged as NA where appropriate, which may account for the larger 
## number of missing entries identified at this step.

table(
  Activity = is.na(df_filtered$activity_level),
  MedDi = is.na(df_filtered$MeDi)
)


df_filtered <- df_filtered |>
  filter(!is.na(activity_level) | !is.na(MeDi))

# At this stage, we have filtered out about 800 more participants than the paper
46132 - nrow(df_filtered)

## 6. Filter out missing PhenoAge values. Given that PhenoAge projections have 
##    already been calculated and imported, this step only requires removing 
##    records where the calculated value is absent.

# Again, a lot more values are identified as missing
table(
  Modified = is.na(df_filtered$phenoage_original),
  Original = is.na(df_filtered$phenoage_modified)
)

df_filtered <- df_filtered |>
  filter(!is.na(phenoage_original) | !is.na(phenoage_modified))

# At this stage, we have filtered out about 11,000 more participants than
# the paper
42625 - nrow(df_filtered)

# Save the result
write_csv(df_filtered, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Complete Dataset.csv"))


