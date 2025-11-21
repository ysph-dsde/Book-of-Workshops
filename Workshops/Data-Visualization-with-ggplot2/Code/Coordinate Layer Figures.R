## ----------------------------------------------------------------------------
## From Yale's Public Health Data Science and Data Equity (DSDE) Team
## Coffee, Cookies, and Coding (C-Cubed) Workshops
##
## Data Visualization with ggplot2
##
##  Author: Shelby Golden, M.S.
##    Date: 2025-11-19
## 
##.   R version: 4.5.1
## renv version: 1.1.5
## 
## Description: This script regenerates the plots used in section 15.2.1,
##              "Transformations with coord_trans()" of "ggplot2: Elegant 
##              Graphics for Data Analysis (3rd Edition)". It also generates 
##              four plots using different coordinate systems in ggplot2,
##              including Cartesian, log-log, polar, and map coordinates. These 
##              plots are included in the workshop slides to make the figures 
##              easier to read.
##
## Note: Written with the assistance of Yale's AI, Clarity.


## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

# NOTE: renv initialization might need to be run twice after the repo is
#       first copied.
# renv::init()
renv::restore()

suppressPackageStartupMessages({
  library("ggplot2")      # For creating static visualizations
  library("gridExtra")    # For arranging multiple grid-based plots
  library("scales")       # For scaling functions
  library("maps")         # For map data
})




## ----------------------------------------------------------------
## TRANSFORMATIONS EXAMPLE

# Sources: https://ggplot2.tidyverse.org/reference/coord_trans.html
#          https://ggplot2-book.org/coord.html#transformations-with-coord_trans

plot1 <- ggplot(diamonds, aes(carat, price)) + 
  stat_bin2d() + 
  geom_smooth(method = "lm") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")


# Better fit on log scale, but harder to interpret
plot2 <- plot1 +
  scale_x_log10() + 
  scale_y_log10()

# Fit on log scale, then backtransform to original.
# Highlights lack of expensive diamonds with large carats
pow10 <- scales::exp_trans(10)
plot3 <- plot1 +
  scale_x_log10() + 
  scale_y_log10() + 
  coord_trans(x = pow10, y = pow10)

# Arrange the plots side-by-side
grid.arrange(plot1, plot2, plot3, ncol = 3)




## ----------------------------------------------------------------
## COORDINATE SYSTEMS GRAPHS

# Source: https://dark-star-161610.appspot.com/secured/_book/grammar-and-vocabulary.html#fig:figcoord

# Define the Cartesian plot
cartesian_plot <- ggplot(data.frame(x=c(0, 10), y=c(0, 10)), aes(x, y)) +
  geom_blank() +
  labs(x = "X-axis", y = "Y-axis") +
  ggtitle("Cartesian Coordinates\n") + 
  coord_fixed(ratio = 1) +
  theme_linedraw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(colour = "black", size = 0.5),
    panel.grid.minor = element_line(colour = "black", size = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank()
  )

# Define the log-log plot with transformation back to original scale
pow10 <- scales::exp_trans(10)
log_log_plot <- ggplot(data.frame(x=c(1, 10), y=c(1, 10)), aes(x, y)) +
  geom_blank() +
  scale_x_log10() + 
  scale_y_log10() + 
  coord_trans(x = pow10, y = pow10) +
  labs(x = "Log10 X-axis", y = "Log10 Y-axis") +
  ggtitle("Log-Log Coordinates\n") +
  theme_linedraw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(colour = "black", size = 0.5),
    panel.grid.minor = element_line(colour = "black", size = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank()
  )

# Define the polar plot
polar_plot <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x = factor(x), y = y)) +
  geom_blank() +
  coord_polar() +
  labs(x = "X-axis (Theta)", y = "Y-axis (Radius)") +
  ggtitle("Polar Coordinates\n") +
  theme_linedraw() + 
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(colour = "black", size = 0.5),
    panel.grid.minor = element_line(colour = "black", size = 0.5),
    panel.background = element_blank()
  )

# Define the round map plot
world_map <- map_data("world")
round_map_plot <- ggplot(world_map, aes(long, lat, group = group)) +
  geom_path() +
  coord_map("ortho", orientation = c(40, 50, 0)) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Map\n") +
  theme_linedraw() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank()
  )

# Function to set equal sizes for the plots
fix_plot_size <- function(plot, height, width) {
  plot +
    theme(
      aspect.ratio = height / width,
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
}

# Correct application of fixed plot sizes
fixed_cartesian_plot <- fix_plot_size(cartesian_plot, 1, 1)
fixed_log_log_plot <- fix_plot_size(log_log_plot, 1, 1)
fixed_polar_plot <- fix_plot_size(polar_plot, 1, 1)
fixed_round_map_plot <- fix_plot_size(round_map_plot, 1, 1)

# Arrange the plots side-by-side
grid.arrange(
  fixed_cartesian_plot, fixed_log_log_plot, fixed_polar_plot, fixed_round_map_plot,
  ncol = 4
)

