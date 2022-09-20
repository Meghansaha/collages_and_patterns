#Patterns Example #3 Recreating Rainbow Wheel-----------------------------------

#==============================================================================#
#Library Load-in----------------------------------------------------------------
#==============================================================================#
library(ggplot2) #For making the piece w/ ggplot
library(dplyr) #For data wrangling
library(tibble) #To work with tibbles
library(purrr) #iteration help

#==============================================================================#
#Creating and Viewing Base Data ------------------------------------------------
#==============================================================================#

#1) Calculate data for a circle from scratch------------------------------------
#This requires some geometry/trig knowledge------------------------------------#
#ggplot works on a cartesian plot system so inputs need to be in X and Y values#
#Regular circle equation: x^2 + y^2 = r^2
#Converted circle equation for cartesian system:-------------------------------#
# x = cos(each angle)----------------------------------------------------------#
# y = sin(each angle)----------------------------------------------------------#
#------------------------------------------------------------------------------#

#Make a vector with angles for the circle#
angles <- seq(0, 2*pi, length.out = 100)

#Make the full circle with "converted" equations# 
circle <- tibble(x = cos(angles),
                 y = sin(angles))

#View the base_circle plot#
circle |>
  ggplot(aes(x,y))+
  geom_polygon()+
  coord_equal()

#==============================================================================#
#Iterating the Data Set--------------------------------------------------------#
#==============================================================================#

#2) Iterate the circle to copy and shift in a row-------------------------------
# We'll use purrr's map2_df for this since we want to map colors and n value---#
#-------------------------------------------------------------------------------

#Number of circles we want in total#
n = 20

#Setting up the spectrum of colors we want to use#
rainbow_pal <-  c("#af3918", "#a21152", "#822b75", "#612884","#154baf",
                  "#0b82b9", "#277e9d", "#488e35","#e3a934","#b2336a")

#Expanding this spectrum to match the number of circles we want#
final_pal <- colorRampPalette(rainbow_pal)(n)

#Creating a final data frame with circles, colors, and group variables---------#
circle_row <- map2_df(1:n,final_pal, ~circle |>
                       mutate(x = x + .x,
                              color = .y,
                              group = paste0("group",.x)))

#==============================================================================#
#Tweaking the Aesthetics and Plotting the Final Image---------------------------
#==============================================================================#

#3) Add final aesthetic options to make it pretty-------------------------------
#We need to remove all the grid lines and labels#
#We need to make the circles all a different color#
#we need to make each circle border white and dashed#
#We need to make the background color black("#000000")#
#We need to convert the plot to polar coordinates#
#------------------------------------------------------------------------------#

#Setting options for the image#
line_style <- 2
line_color <- "#ffffff"
background_color <- "#000000"

#Plotting the final image while feeding in options#
#Fill variable gets the colors from the dataset#
circle_row |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  geom_polygon(fill = circle_row$color, color = line_color, linetype = line_style)+
  theme(plot.background = element_rect(fill = background_color))+
  coord_polar() #warps the circles into a "wheel"

