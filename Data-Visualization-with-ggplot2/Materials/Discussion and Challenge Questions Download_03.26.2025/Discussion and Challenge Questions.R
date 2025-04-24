## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
##
## Workshop: Data Visualization with ggplot2
## Authors:  Shelby Golden, M.S.
## Date:     2025-03-18
## 
## R version:    4.4.3
## renv version: 1.0.11
## 
## Description: Enclosed are the discussion questions posited during the workshop
##              with code to help you answer them. At the end are a couple of
##              challenge questions where you can test your knowledge followed
##              by suggested answers. Remember that there are different ways to
##              generate the same graph, and it is possible your answer is
##              correct but looks different than my suggestion!


## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

# NOTE: renv initializing might need to be run twice after the repo is
#       first copied.
#renv::init()
renv::restore()


suppressPackageStartupMessages({
  library("arrow")         # For reading in the data
  library("dplyr")         # For data manipulation
  library("ggplot2")       # For creating static visualizations
  library("plotly")        # For interactive plots
  library("cowplot")       # ggplot add on for composing figures
  library("tigris")        # Imports TIGER/Line shapefiles from the Census Bureau
  library("sf")            # Handles "Special Features": spatial vector data
  library("RColorBrewer")  # Load Color Brewer color palettes
  library("viridis")       # Load the Viridis color pallet
})


# Function to select "Not In"
'%!in%' <- function(x,y)!('%in%'(x,y))




## ----------------------------------------------------------------
## LOAD THE DATASETS

# The RSV-NET Infections dataset used in the workshop.
df <- read_parquet(file.path(getwd(), "RSV-NET Infections.gz.parquet"))

# Dataset provided with the ggplot2 package instillation.
data("diamonds")

# Call the TIGER/Line Shapefiles from the Census Bureau.
us_geo <- tigris::states(class = "sf", cb = TRUE) |>
  # Translate the geometric locations for Alaska and Hawaii to plot underneath
  # the contiguous states.
  shift_geometry()


## ----------------------------------------------------------------
## WORKSHOP DISCUSSIONS

## You can find the context for these discussion questions by reviewing
## the slides and the "Worked Through Example.qmd" file.


## ---------------------
## DISCUSSION #1 - THE GEOMETRY LAYER

## The following two plots give the same results, but map the aesthetics in
## different layers.


# Map aesthetics in the plot object layer.
df |>
  filter(Region == "Connecticut", Season == "2021-22", Level == "N/A") |>
  ggplot(aes(x = MMWRweek, y = Kernel)) +
    geom_line()

# Map aesthetics in the Geometry layer.
df |>
  filter(Region == "Connecticut", Season == "2021-22", Level == "N/A") |>
  ggplot() +
    geom_line(aes(x = MMWRweek, y = Kernel))


## Can you think of reasons why you would choose one over the other?




## ---------------------
## DISCUSSION #2 - THE SCALES LAYER

# Specify the infection seasons we'd like to see out of the range of options.
include_seeasons <- c("2022-23", "2023-24", "2024-25")

## We see that associating a variable with an aes(color) will also group 
## by that same variable.

# Using Scales to highlight insights to infection trends across seasons.
df |>
  filter(Region == "Connecticut", Season %in% include_seeasons, Level == "N/A") |>
  ggplot(aes(x = MMWRweek, y = Kernel, color = Season) ) +
    geom_line() +
    scale_color_brewer(type = "qual", palette = "Dark2")


## a. What would happen if we only group the outcomes and exclude the color 
##    statement: i.e. aes(group = Season)?


## b. Does adding a scale_color_*() statement change the result?


## c. Why do you see the result you get?




## ---------------------
## DISCUSSION #3 - THE COORDINATES LAYER

## In this discussion, we are going to follow the example laid out in the
## ggplot2 package documentation for coord_trans().
## 
## Link: https://ggplot2.tidyverse.org/reference/coord_trans.html
## 
## The example uses one of the sample datasets that gets loaded as part of the
## ggplot2 instillation. The "diamonds" dataset gives characteristics of
## diamonds, including their price, carat, cut, clarity, and color.
## 
## We will be comparing the diamonds price against its carat. Both variables
## will need to be transformed using a base 10 log.

# The basic, un-transformed plot.
ggplot(diamonds, aes(carat, price)) +
  stat_bin2d() + 
  geom_smooth(method = "lm")

## a. Create three separate plots that apply following three modifications to the
##    above basic plot.
##      - Transform the variables in aes() with log10()
##      - Add a Scales layer: scale_*_log10() or scale_*_continuous(trans = "log10")
##      - Add a Coordinates layer: coord_trans(* = "log10")



## b. Do they all plot the same way? If not, what do you think is happening and 
##    how would you fix the code?




## ----------------------------------------------------------------
## CHALLENEGE QUESTIONS

## ---------------------
## QUESTION #1 - THE FACETS LAYER

# Copying the subsetting vectors for Season and Characteristic Level.
include_seeasons <- c("2022-23", "2023-24", "2024-25")
ages_ordered <- c("5-17 Years", "18-49 Years", "75+ Years")

# Copying the data subsetting code to get the leading point.
leading_point = df |> 
  filter(Region == "Connecticut", Season %in% include_seeasons, 
         Level %in% ages_ordered) |>
  filter(MMWRyear == max(MMWRyear)) |>
  filter(MMWRweek == max(MMWRweek))


## In the worked through example we leverage two of the three distinguishing
## variables: Season and Characteristic Level. In this questions, we want to
## focus on the third one, Region.
## 
## Recall the available outcomes for Region.
df$Region |> unique()


## Using the final line-graph code from the worked through example, add a 
## row-wise facet to compare two states or HHS regions.

# Modify the following block of code.
df |>
  filter(Region == "Connecticut", Season %in% include_seeasons, Level %in% ages_ordered) |>
  ggplot(aes(x = MMWRweek, y = Kernel, color = Season) ) +
    geom_line() +
    geom_point(data = leading_point, aes(x = MMWRweek, y = Kernel), 
               color = "red") +
    labs(title = "RSV Infection Trends Since 2022",
         x = "Weeks Since July", 
         y = "Positive RSV Tests\n(scaled and kernel smoothed)") +
    ylim(0, 100) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    facet_grid(~factor(Level, levels = ages_ordered)) + 
    theme_linedraw()




## ---------------------
## QUESTION #2 - OVERLAYING LAYERS

## In the polished version of the line plot we layered an additional point
## on the graph that highlighted the most currently represented date in the
## active infection season. To get this data, we externally filtered down the
## dataset to give that point, repeating some of the filtering we are already
## doing before the plot gets generated.
## 
## a. Can you modify this approach so that we no longer require the external
##    dataset, thus simplifying our code?
## 
## b. Now try using stat_summary(filtered_data, geom = "point", fun = "max", 
##    color = "red") instead of geom_point(). If you only filter to MMWRyear, do
##    you get the same result, and if not why?


# Modify the following two blocks code.
leading_point = df |> 
  filter(Region == "Connecticut", Season %in% include_seeasons, 
         Level %in% ages_ordered) |>
  filter(MMWRyear == max(MMWRyear)) |>
  filter(MMWRweek == max(MMWRweek))

df |>
  filter(Region == "Connecticut", Season %in% include_seeasons, Level %in% ages_ordered) |>
  ggplot(aes(x = MMWRweek, y = Kernel, color = Season) ) +
  geom_line() +
  geom_point(data = leading_point, aes(x = MMWRweek, y = Kernel), 
             color = "red") +
  labs(title = "RSV Infection Trends Since 2022",
       x = "Weeks Since July", 
       y = "Positive RSV Tests\n(scaled and kernel smoothed)") +
  ylim(0, 100) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  facet_grid(~factor(Level, levels = ages_ordered)) + 
  theme_linedraw()




## ---------------------
## QUESTION #3 - MAP PROJECTIONS

# Copying the data subsetting code.
subset <- df |>
  filter(Region %in% c(datasets::state.name, "District of Columbia"),
         Season %in% "2024-25", MMWRweek %in% c(24:26))

# Copying the data joining and filtering code adding the SF polygons to our
# RSV infections metadata.
geo_df <- us_geo |> 
  left_join(subset, c("NAME" = "Region")) |>
  filter(GEOID < 60)


## In our line graph we faceted by age groups. But we see that if we apply
## a simple Facet  layer to our map plot the NA values get plotted in their 
## own panel.
## 
## a. Why is this happening?
## b. Fix this code to prevent this panel from being generated.

# Modify the following block of code.
geo_df |>
  ggplot(aes(fill = Kernel), color = "black") +
    geom_sf() +
    scale_fill_viridis(direction = -1, na.value = "grey92") +
    labs(fill = NULL, title = "Peak 2024-25 Season RSV Infection Trends\nResults Scaled and Gaussian Kernel Smoothed") +
    facet_grid(~factor(Level, levels = ages_ordered)) + 
    coord_sf() +
    theme_map()




## ----------------------------------------------------------------
## SUGGESTED TO SOLUTIONS TO THE CHALLENEGE QUESTIONS

## ---------------------
## QUESTION #1 - THE FACETS LAYER

# List the HHS regions we want to plot.
include_regions <- c("Region 1", "Region 4")

# Recalculate the subset for our leading point.
leading_point = df |> 
  filter(Region %in% include_regions, Season %in% include_seeasons, 
         Level %in% ages_ordered) |>
  filter(MMWRyear == max(MMWRyear)) |>
  filter(MMWRweek == max(MMWRweek))

# Define our plot object.
p <- df |>
  filter(Region %in% include_regions, Season %in% include_seeasons, Level %in% ages_ordered) |>
  ggplot(aes(x = MMWRweek, y = Kernel, color = Season) ) +
    geom_line() +
    geom_point(data = leading_point, aes(x = MMWRweek, y = Kernel), 
               color = "red") +
    labs(title = "RSV Infection Trends Since 2022",
         x = "Weeks Since July", 
         y = "Positive RSV Tests\n(scaled and kernel smoothed)") +
    ylim(0, 100) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    facet_grid(~factor(Level, levels = ages_ordered)) +
    theme_linedraw()

# Apply faceting.
p + facet_grid(Region~factor(Level, levels = ages_ordered))




## ---------------------
## QUESTION #2 - OVERLAYING LAYERS

df %>%
  filter(Region == "Connecticut", Season %in% include_seeasons, 
         Level %in% ages_ordered) %>%
  #-- Wrap the entire ggplot() object with the function assigning the left result
  #-- to the variable "y". Notice that we now need to change the pipe from
  #-- base R's "|>" to tidyverse's "%>%" to allow this operation.
  (\(y) { 
    ggplot(y, aes(x = MMWRweek, y = Kernel, color = Season) ) +
      geom_line() +
      
      #-- Sequentially filter the data frame (here represented "y") by MMWRyear
      #-- and then by MMWRweek. NOTE: these operations must be done sequentially
      #-- as opposed to within the same filter() expression.
      geom_point(data = filter(y, MMWRyear == max(MMWRyear)) %>% 
                   filter(MMWRweek == max(MMWRweek)), color = "red") +
      
      #-- Alternatively, you can nest the two filter() statements:
      #geom_point(data = filter(filter(y, MMWRyear == max(MMWRyear)), 
      #                         MMWRweek == max(MMWRweek)), color = "red") +
      
      #-- Using stat_summary() instead will plot the max y detected for each
      #-- available x.
      #stat_summary(data = filter(y, MMWRyear == max(MMWRyear)), 
      #             geom = "point", fun = "max", color = "red") +
      
      #-- For example, we do filter the MMWRweek instead of MMWRyear we get a
      #-- point at the max y detected at the max week.
      
      #stat_summary(data = filter(y, MMWRweek == max(MMWRweek)), 
      #             geom = "point", fun = "max", color = "red") +
      
      #-- Technically, we can do the sequential filter() over both variables,
      #-- but this operation is redundant because there is nothing left to
      #-- calculate the max over. It is therefore better to use geom_point().
      #stat_summary(data = filter(y, MMWRyear == max(MMWRyear)) %>% 
      #               filter(MMWRweek == max(MMWRweek)), 
      #             geom = "point", fun = "max", color = "red") +
      
      labs(title = "RSV Infection Trends Since 2022",
           x = "Weeks Since July", 
           y = "Positive RSV Tests\n(scaled and kernel smoothed)") +
      ylim(0, 100) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      facet_grid(~factor(Level, levels = ages_ordered)) + 
      theme_linedraw() 
  })




## ---------------------
## QUESTION #3 - MAP PROJECTIONS

## a. Why is this happening?
## 
## When we joined the polygon vector dataset with our metadata dataset we
## induced NA's for the missing states. These get registered as a new outcome
## of Level to plot separately.

## b. Fix this code to prevent this panel from being generated.

ggplot() +
  # Layers are plotted one on top of another. If we want the background to
  # show the states, then we add our new layer before the data is abstracted
  # onto the map.
  geom_sf(data = filter(us_geo, GEOID < 60)) +
  # The Data layer will be inherited from the previous geom_sf(). Therefore
  # we must override the inherited setting. Notice also that we need to
  # denote the aesthetic here too. Kernel is not present in the us_geo dataset,
  # and because it comes first we cannot place it in ggplot() without drawing
  # an error.
  geom_sf(data = na.omit(geo_df), aes(fill = Kernel), color = "black") +
  scale_fill_viridis(direction = -1, na.value = "grey92") +
  labs(fill = NULL, title = "Peak 2024-25 Season RSV Infection Trends\nResults Scaled and Gaussian Kernel Smoothed") +
  facet_grid(~factor(Level, levels = ages_ordered)) + 
  coord_sf() +
  theme_map()




