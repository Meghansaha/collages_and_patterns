#Patterns Example #2 Recreating Chevrons "ZigZags"------------------------------

#==============================================================================#
#Library Load-in----------------------------------------------------------------
#==============================================================================#
library(ggplot2) #For making the piece w/ ggplot
library(dplyr) #For data wrangling and the "old" pipe if desired
library(tibble) #To work with tibbles instead of regular dataframes
library(purrr) #For iteration work

#==============================================================================#
#Creating and Viewing Base Data ------------------------------------------------
#==============================================================================#

#Sometimes it helps to set arbitrary x/y limits to make a "blank" canvas#
axis_min <- 0
axis_max <- 10

#Creating a "base" data frame to feed into ggplot#
base_data <- tibble(x = axis_min:axis_max,
                    y = axis_min)

#Plotting the "blank" canvas#
base_data |>
  ggplot(aes(x,y))+
  geom_point()+
  geom_path()

#==============================================================================#
#Tweaking the base_data to "recreate" the zig-zags----------------------------
#==============================================================================#

#1) "Bump" up every other point-------------------------------------------------
#We need to mutate every other Y point (because that can move us "up" the plot)
#------------------------------------------------------------------------------#

#Creating a vector of "bumps"#
bumps <- rep_along(1:nrow(base_data), c(0,1))

#Mutating the base_data frame to add the "bumps"#
zigs <- base_data |>
          mutate(y = y + bumps)

#Viewing the zigs frame#
zigs |>
  ggplot(aes(x,y))+
  geom_path()

#==============================================================================#
#Iterating over the base_data to "recreate" the zig-zags------------------------
#==============================================================================#

#2) Copy and Paste the same line up the plot------------------------------------
#We need to iteratively copy the dataset while shifting "up" each copy
#------------------------------------------------------------------------------#

#Setting up the number of iterations and "shifts" up the plot#
n <- 1:6

#Creating a new dataframe with "copied and shifted" lines#
#n value is added to each y value 6 times#
#group value create a string to keep each "line" together#
zigzags <- map_df(n, ~ zigs |>
                        mutate(y = y + .x,
                               group = paste0("line",.x))) 

#Viewing the zigzags frame#
zigzags |>
  ggplot(aes(x,y, group = group))+
  geom_path()


#==============================================================================#
#Tweaking the Aesthetics and Plotting the Final Image---------------------------
#==============================================================================#

#3) Add final aesthetic options to make it pretty-------------------------------
#We need to remove all the grid lines and labels#
#We need to make the lines thicker#
#we need to make each line each of the six colors#
#We need to make the background color yellow("#E3A934")#
#------------------------------------------------------------------------------#

#Setting options for the image#
line_size <- 10
line_colors <- c("#c89658", "#b6886f", "#8867ab", 
                 "#bbaa5f", "#9fab7c", "#5aabc4")
background_color <- "#E3A934"

#Easier to map colors onto the dataset since multiple points exist in one line#
#Make a modified dataset with map_df to achieve this#
#We should have done this earlier on line 60 "ideally"#
zigzag_colored <- map2_df(n, line_colors, ~ zigs |>
                                             mutate(y = y + .x,
                                                    group = paste0("line",.x),
                                                    color = .y)) 

#Plotting the final image - while feeding in the options#
zigzag_colored |>
  ggplot(aes(x,y, group = group))+
  theme_void()+ #Removes axes, labels, etc.
  geom_path(size = line_size, 
            lineend = "square", #makes the ends squares#
            linejoin = "mitre", #makes the joints "pointy"#
            color = zigzag_colored$color)+ #pull it in from the data frame#
  theme(plot.background = element_rect(fill = background_color))
