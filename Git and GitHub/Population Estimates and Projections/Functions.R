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
## Description: Functions used the "Census Data Harmonization Script.R". Notice
##              that as the data was harmonized, some functions were not used
##              in subsetquent cleaning script versions.

## ----------------------------------------------------------------------------
## SET UP THE ENVIRONMENT

## renv() will install all of the packages and their correct version used here.
suppressWarnings(renv::restore())

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




## ----------------------------------------------------------------------------
## FUNCTIONS

repeated_info <- function(data, date_range) {
  #       "data": the data set that needs evaluating.
  # "date_range": string of dates, based on the format present in the data
  #               column names.
  # 
  # This function checks for the number of variables associated with each year
  # in the provided date range.
  
  hits <- c()
  for (i in 1:length(date_range)) {
    # Find the number of column names with the current date included. Save
    # the length of this subset of names.
    hits[i] <- names(data) %>% .[str_detect(., date_range[i], )] %>% length()
  }
  hits
}




check_matched_row_entries <- function(data, variable, query_filter){
  #         "data": the data set that needs evaluating.
  #     "variable": the variable name this could individualize the result.
  #                 Iterate over unique instantiations of this variable.
  # "query_filter": an instantiation for "variable".
  # 
  # This function confirms that row entries satisfying a filter are the same.
  
  # Filter the data set by the variable outcome (i.e. "Province_State" == 
  # "United States"). Iterate Boolean test for the number of equal entries
  # over each column in the subset.
  check = data |>
    (\(x) { x[x[, variable] == query_filter, ] }) () |> 
    (\(y) { apply(y, 2, function(x) length(unique(x)) == 1) }) () |>
    all()
  
  # If statement to interpret the outcome of the Boolean test.
  if(check == TRUE){
    print("All entries are the same.")
  }else{
    print("Some entries are not the same.")
    
  }
}




check_col_matches <- function(data, names_11, names_16, date_range, with_underscore) {
  #            "data": the data set that needs evaluating.
  #        "names_11": variable names that occur 11 times.
  #        "names_16": variable names that occur 16 times.
  #      "date_range": string of dates, based on the format present in the data
  #                    column names.
  # "with_underscore": some variable names utilize an underscore. Toggle if
  #                    this is the case for "data"
  # 
  # This function checks for column name differences compared with a reference.
  # It assumes that the order of information is the same and the length of the 
  # vectors is the same (as shown by function repeated_info()).
  
  result <- list()
  for (i in 1:length(date_range)) {
    
    # Some of the data we are harmonizing has an underscore while others do
    # not have an underscore.
    if (with_underscore == TRUE) {
      # Prompt to remove the date with a preceding underscore.
      remove_string <- str_c("_", date_range[i])
    } else if (with_underscore == FALSE) {
      # Prompt to remove the date without a preceding underscore.
      remove_string <- str_c(date_range[i])
    }
    
    # Exclude the non-repeated columns (metadata and census year
    # population and base estimates) and find columns with the prompted year.
    query <- names(data)[-c(1:4)] %>% .[str_detect(., date_range[i], )] %>%
      # Remove the year based on the if statement above and covert all names
      # to lowercase.
      str_remove(., remove_string) %>% str_to_lower()
    
    
    if (all(is.na(query)) == FALSE) {
      # If the length of column names subset is 11, then:
      if (length(query) == 11) {
        # Compare names that are not matched with the reference column
        # names vector of the same length.
        not_matched <- which(names_11 %!in% query)
        
        # Store the number of columns, the names not matched to the reference,
        # and the names not matched in the query.
        result[[i]] <- data.frame(
          "Name_length" = 11,
          "Ref" = c(names_11[not_matched]),
          "Querry" = c(query[not_matched])
        )
        
        # If the length of the column names subset is 16, then do the same
        # comparison as for column names length of 11 above.
      } else if (length(query) == 16) {
        not_matched <- which(names_16 %!in% query)
        result[[i]] <- data.frame(
          "Name_length" = 16,
          "Ref" = c(names_16[not_matched]),
          "Querry" = c(query[not_matched])
        )
        
        # If there are no matches, then store "NA".
      } else{
        result[[i]] <- result[[i]] <- data.frame(
          "Name_length" = "No Querry",
          "Ref" = "NA",
          "Querry" = "NA"
        )
      }
    }
  }
  # Combine the results as added rows to a new data set.
  do.call(rbind, result)
}




change_col_names <- function(data, subset_range, custom_subset_names, names_to) {
  #                "data": the data set that needs evaluating.
  #        "subset_range": the range of column names in "data" where the
  #                        repetitive naming structure occurs.
  # "custom_subset_names": any custome naming conventions the user wants to add.
  #            "names_to": reference column nomenclature to convert to.
  # 
  # This function standardizes the column naming format to a reference. It
  # allows subsetting the column names with varying naming standards between
  # raw data sets for customization.
  
  # Separate out the columns where the repeated column names occurs.
  subset = colnames(data)[-subset_range]
  
  # Generate a universal names search that is stripped of formatting and
  # link to the target naming convention.
  universal <- data.frame(
    "Target" = names_to,
    "Search" = names_to %>%
      # Remove the non-alphabetical characters, spaces, and make the string
      # lower case for matching.
      str_replace_all(., "[^[:alnum:]]|[0-9]", "") %>% str_to_lower()
  )
  
  # Store the dates associated with each column for appending later.
  ordered_dates <- subset %>% str_extract(., "[0-9]{1,4}$")
  
  # Remove the non-alphabetical characters, spaces, and make the string
  # lower case for matching.
  subset <- subset %>% str_replace_all(., "[^[:alnum:]]|[0-9]", "") %>% str_to_lower()
  
  for (i in 1:nrow(universal)) {
    # Find the column indices where the un-formatted string matches.
    query <- str_detect(subset, str_c("\\b", universal[i, "Search"], "\\b")) %>% which()
    
    # Change the column names at those indices to the target format.
    subset[query] <- universal[i, "Target"]
  }
  
  # Piece together the naming elements and reintroduce the date associated.
  colnames(data) <- c(custom_subset_names, str_c(subset, "_", ordered_dates))
  
  data
}




us_sum_test <- function(data, no_na = TRUE) {
  #  "data": the data set that needs evaluating.
  # "no_na": subset when no NA's are found over all columns.
  # 
  # This function checks if the values for the United States are the same as
  # the column sums over the values for all states.
  
  # Only looking at the non-meta data columns, sum over the columns, excluding
  # the row entry where County == "United States".
  sum_test <- rbind(data %>% .[.$County %!in% "United States", -c(1:4)] %>%
                      sapply(., function(x) sum(x)),
                    # Save values for the County == "United States" row only.
                    data %>% .[.$County == "United States", -c(1:4)]) %>%
    # Adjust the row names to reflect which rows the sums reflect.
    `row.names<-`(c("States", "United States")) %>% as.data.frame()
  
  # We expect that columns with an NA in them might not match with the
  # sum over any of the other columns, and so we separate those out.
  if (no_na == TRUE) {
    # Search for NA's column-by-column and subset the data for those
    # where no NA was found.
    subset = sum_test %>% .[sapply(., function(x) ! any(is.na(x)))]
    
  } else if (no_na == FALSE) {
    # Similar as for no_na == TRUE, except subset when an NA is found.
    subset = sum_test %>% .[sapply(., function(x) any(is.na(x)))]
  }
  
  # If the two rows for any column is equal, then they will not be unique.
  result = subset %>% apply(., 2, function(x) length(unique(x)) == 1)
  # The Boolean check is all-or-nothing.
  check  = result %>% all()
  # Subset the results to show columns where the sums do not match.
  output = subset[, !result]
  
  
  # If statement to condition the output based on the results.
  if (check == TRUE) {
    list(print("All entries are the same."), output)
    
  } else{
    list(print("Some entries are not the same."), output)
  }
}














