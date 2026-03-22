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
## Description: The eight digit dr1ifdcd and dr2ifdcd use USDA Food and Nutrient 
##              Database for Dietary Studies (FNDDS) variable encoding, where the 
##              first two digits represent the food group. No generalized 
##              codebook (data dictionary) was publicly available from NCHS for 
##              the Dietary Interview Files Individual Foods First and Second 
##              Day (datasets DR1IFF and DR2IFF). Since a more recent codebook 
##              indicated that FNDDS 2021-2023 was used, this version was 
##              applied here.

# Codebook URLs
diet1_var <- get_url("dr1iff_l")
diet2_var <- get_url("dr2iff_l")


## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT
## renv() will install all of the packages and their correct version used here.
renv::restore()

## Load in the R packages used in this script from the project library.
suppressPackageStartupMessages({
  library("readr")       # For reading in the data
})




## ----------------------------------------------------------------------------
## LOAD IN THE DATA

## The codebooks say that FNNDS encodings can be found on http://www.ars.usda.gov/nea/bhnrc/fsrg.
## Unfortunately, it proved challenging to attain the needed encoding for this
## project this way. Instead, the following tables from USDA were found through
## organic searching and used:
##    - Appendix H from https://www.ars.usda.gov/ARSUserFiles/80400530/pdf/fndds/2021_2023_FNDDS_Doc.pdf
##    - Appendix B from https://www.ars.usda.gov/ARSUserFiles/80400530/pdf/fndds/fndds_doc.pdf


# Translated encoding from Appendix H, with supplemented entries for the "3. Eggs"
# category added from Appendix B.
fndds_2021_23_food_code <- tibble(
  code = c("1", "11", "12", "13", "14", "2", "20", "21", "22", "23", "24", 
           "25", "26", "27", "28", "3", "31", "32", "33", "34", "35", "4", 
           "41", "42", "43", "44", "5", "50", "51", "52", "53", "54", "55",
           "56", "57", "58", "59", "6", "61", "62", "63", "64", "67", "7", 
           "71", "72", "73", "74", "75", "76", "77", "78", "8", "81", "82", 
           "83", "89", "9", "91", "92", "93", "94", "95", "99"),
  food_group = c(
    "Milk and Milk Products", 
    "Milks, milk drinks, yogurts, infant formulas", "Creams and cream substitutes", 
    "Milk desserts and sauces", "Cheeses",
    "Meat, Poultry, Fish, and Mixtures", 
    "Meat", "Beef", "Pork", "Lamb, veal, game", "Poultry", "Organ meats, frankfurters, sausages, lunchmeats",
    "Fish, shellfish", "Meat, poultry, fish mixtures", "Frozen meals, soups, gravies", 
    "Eggs", 
    "Eggs", "Egg mixtures", "Egg substitutes", "Eggs baby food",
    "Frozen plate meals with egg as major ingredient",
    "Dry Beans, Peas, Other Legumes, Nuts, and Seeds", 
    "Legumes", "Nuts, nut butters, nut mixtures", "Seeds and seed mixtures", 
    "Carob products",
    "Grain Products", 
    "Flour and dry mixes", "Yeast breads, rolls", "Quick breads", 
    "Cakes, cookies, pies, pastries, bars", "Crackers, snack products",
    "Pancakes, waffles, French toast, other grain products", 
    "Pastas, rice, cooked cereals", "Cereals, not cooked", 
    "Grain mixtures, frozen meals, soups", "Meat substitutes",
    "Fruits", 
    "Citrus fruits, juices", "Dried fruits", "Other fruits", 
    "Fruit juices and nectars excluding citrus", "Fruits and juices baby food",
    "Vegetables", 
    "White potatoes, starchy vegetables", "Dark-green vegetables", 
    "Orange vegetables", "Tomatoes, tomato mixtures", "Other vegetables",
    "Vegetables and mixtures mostly vegetables baby food", "Vegetables with meat, poultry, fish", 
    "Mixtures mostly vegetables without meat, poultry, fish",
    "Fats, Oils, and Salad Dressings", 
    "Fats", "Oils", "Salad dressings", "‘For use’ with a sandwich or vegetable",
    "Sugars, Sweets, and Beverages", 
    "Sugars, sweets", "Nonalcoholic beverages", "Alcoholic beverages", 
    "Noncarbonated water", "Formulated nutrition beverages, energy drinks, sports drinks", 
    "Used as an ingredient, not for coding"
  )
)


## This analysis assesses adherence to the Mediterranean diet (Trichopoulou et 
## al., 2003). The table below maps FNDDS food codes to diet components. Food 
## categories marked "N/A" were excluded because they were either irrelevant to 
## the population (e.g., baby food) or difficult to categorize (e.g., meat 
## alternatives).
## 
## Additionally, a third column classifies food categories as beneficial or 
## detrimental. Food categories not described in the Mediterranean diet framework 
## are labeled "N/A" and excluded from overall diet scoring.

mediterranean_groups <- tibble(
  code = c("1", "11", "12", "13", "14", "2", "20", "21", "22", "23", "24", 
           "25", "26", "27", "28", "3", "31", "32", "33", "34", "35", "4", 
           "41", "42", "43", "44", "5", "50", "51", "52", "53", "54", "55",
           "56", "57", "58", "59", "6", "61", "62", "63", "64", "67", "7", 
           "71", "72", "73", "74", "75", "76", "77", "78", "8", "81", "82", 
           "83", "89", "9", "91", "92", "93", "94", "95"),
  med_group = c(
    rep("dairy products", 5), rep("meat", 7), "fish", "meat", "N/A",
    rep("eggs", 3), "N/A", "eggs", "N/A", rep("legumes and nuts", 5), rep("cereals", 4), 
    "sugar and sweets", rep("cereals", 5), "N/A", rep("fruits", 4), "sugar and sweets", 
    "N/A", "vegetables", "potatoes", rep("vegetables", 7), "other fats", "fats", "oils", 
    rep("other fats", 2), rep("sugar and sweets", 2), "nonalcoholic beverages", 
    "alcoholic beverages", rep("nonalcoholic beverages", 2)
  ),
  benefice = c(
    rep("No", 12), "Yes", "No", rep("N/A", 7), rep("Yes", 9), "N/A", rep("Yes", 5),
    "N/A", rep("Yes", 4), rep("N/A", 2), rep("Yes", 9), rep("N/A", 8), "No", 
    rep("N/A", 2))
)






## ----------------------------------------------------------------------------
## SAVE THE TABLES

write_csv(fndds_2021_23_food_code, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "FNDDS Food Group Codes_2021 to 23.csv"))
write_csv(mediterranean_groups, file.path(getwd(), "Workshops/Code-Smarter-Not-Harder/Data", "Mediterranean Diet Food Groups_Trichopoulou et al 2003.csv"))


