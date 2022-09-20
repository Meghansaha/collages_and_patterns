#Patterns Example #1 Recreating Polka Dots--------------------------------------

#==============================================================================#
#Library Load-in----------------------------------------------------------------
#==============================================================================#
library(ggplot2) #For making the piece w/ ggplot
library(dplyr) #For data wrangling and the "old" pipe if desired
library(tibble) #To work with tibbles instead of regular dataframes

#==============================================================================#
#Viewing a "Blank" Canvas-------------------------------------------------------
#==============================================================================#

#Sometimes it helps to set arbitrary x/y limits to make a "blank" canvas#
axis_min <- 0
axis_max <- 10

#Creating a "base" data frame to feed into ggplot#
base_data <- tibble(x = axis_min:axis_max,
                    y = x)

#Plotting the "blank" canvas#
base_data |>
  ggplot(aes(x,y))

#==============================================================================#
#Looking at the base_data frame points on the canvas----------------------------
#==============================================================================#

base_data |>
  ggplot(aes(x,y))+
  geom_point()

#==============================================================================#
#Tweaking the base_data to "recreate" the polka dots----------------------------
#==============================================================================#

#1) Make a grid of points-------------------------------------------------------
#We need a grid of points instead of a single line#
#Use expand.grid or tidyr's crossing fx#
#------------------------------------------------------------------------------#

grid_points <- expand.grid(base_data) #Creates all combos of #s in the frame#

#View the new dataset if needed/desired#
View(grid_points)

#View the new dataset on the canvas#
grid_points |>
  ggplot(aes(x,y))+
  geom_point()

#2) Add spacing variations------------------------------------------------------
#We need each row to be a bit varied in spacing#
#We can easily do this by "removing" some data points in an ordered way#
#We'll remove a y point when y is even AND x is odd - using %%#
#We'll remove a x point when x is even AND y is odd - using %%#
#------------------------------------------------------------------------------#

#Make a new "polka dot" set without the "dropped" values#
polka_dots <- grid_points |>
  mutate(x = case_when(y %% 2 == 0 & x %% 2 > 0 ~ NA_integer_,
                       TRUE ~ x),
         y = case_when(x %% 2 == 0 & y %% 2 > 0 ~ NA_integer_,
                       TRUE ~ y)) |>
  filter(if_all(everything(), ~!is.na(.))) #Removes all rows with an NA#

#View the new dataset on the canvas#
polka_dots |>
  ggplot(aes(x,y))+
  geom_point()

#==============================================================================#
#Tweaking the Aesthetics and Plotting the Final Image---------------------------
#==============================================================================#

#3) Add final aesthetic options to make it pretty-------------------------------
#We need to remove all the grid lines and labels#
#We need to make the dots bigger and white#
#We need to make the background color green("#598C4C")#
#------------------------------------------------------------------------------#

#Setting options for the image#
dot_size <- 10
dot_color <- "#ffffff" #"white" works too
background_color <- "#598C4C"

#Plotting the final image - while feeding in the options#
polka_dots |>
  ggplot(aes(x,y))+
  theme_void()+ #Removes axes, labels, etc.
  geom_point(size = dot_size, 
             color = dot_color)+
  theme(plot.background = element_rect(fill = background_color))
  