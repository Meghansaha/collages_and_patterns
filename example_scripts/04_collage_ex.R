#Collage Example - Sunset-------------------------------------------------------

#==============================================================================#
#Library Load-in----------------------------------------------------------------
#==============================================================================#
library(sp) #For doing more complicated polygon work
library(ggplot2) #For making the piece w/ ggplot
library(dplyr) #For data wrangling and the "old" pipe if desired
library(tibble) #To work with tibbles instead of regular dataframes

#==============================================================================#
#Creating a "Blank" Landscape Canvas--------------------------------------------
#==============================================================================#
#1) Set up our ggplot to be landscape-------------------------------------------
#X's max limit need to be larger than Y's max limit#
#------------------------------------------------------------------------------#

#X limits#
xmin <- 0
xmax <- 20

#Y limits#
ymin <- 0
ymax <- 10

#Creating the base_data#
base_data <- tibble(x = seq(xmin,xmax, length.out = 100),
                    y = seq(ymin,ymax, length.out = 100))

#Viewing the plot#
base_data |>
  ggplot(aes(x,y))

#==============================================================================#
#Creating The Background Color Gradient-----------------------------------------
#==============================================================================#
#2) Set up a color palette and data for geom_segment()--------------------------
#Other geoms can be used to create gradients, but segments tend to be easier#
#------------------------------------------------------------------------------#

#Geom Segment takes xend and yend with x and y values#
#Vertical lines: x == xend with range in y (What we want)# 
#Horizontal Lines: y == yend with range in x#

#Data frame with background segment lines#
back_segments <- tibble(x = seq(xmin, xmax, length.out = 100),
                 xend = x,
                 y = ymin,
                 yend = ymax)

#Create a color palette to fit the data frame (100 rows)#
color_pal <- c("#9C2C77", "#CD104D", "#E14D2A", "#FD841F")
segment_colors <- colorRampPalette(color_pal)(nrow(back_segments))

#View the plot if you want while feeding in color palette and tweaking size#
back_segments |>
  ggplot(aes(x,y, xend = xend, yend = yend))+
  theme_void()+
  geom_segment(color = segment_colors, size = 10)

#==============================================================================#
#Creating The Background Line Overlay-------------------------------------------
#==============================================================================#
#3) Expand the base data with expand.grid() to create a pattern-----------------
#We'll use expand.grid to fill out the canvas#
#Most of the pattern is created with aesthetic tweaks in the geom#
#To get horizontal lines, we'll need to use a group argument across the y value#
#We need separate lines based on existing data, geom_path() is best here#
#------------------------------------------------------------------------------#

#Create a new expanded data frame off of base_data#
overlay <- expand.grid(base_data)

#Overlay Options#
#Set the color for the overlay pattern
overlay_color <- "#ffffff"

#Create varying sizes for the overlay at different points to fill the data#
overlay_sizes <- sample(seq(.01, .2, length.out = nrow(overlay)))

#Layer the overlay data frame onto the back_segments and View if desired#
back_segments |>
  ggplot(aes(x,y, xend = xend, yend = yend))+
  theme_void()+
  geom_segment(color = segment_colors, size = 10)+
  geom_path(data = overlay, aes(x,y, group = y),
            color = overlay_color, 
            size = overlay_sizes,
            inherit.aes = FALSE)

#==============================================================================#
#Creating The Fancy Circle------------------------------------------------------
#==============================================================================#
#4) Create a circle in the middle of the image from scratch---------------------
#Use the `point.in.polygon` fx from the sp package to find points in the circle#
#Store those results in a new data frame and tweak the aesthetics in a new geom#
#Need to group on the x value to get vertical lines#
#We need separate lines based on existing data, geom_path() is best here#
#------------------------------------------------------------------------------#
  
#Making a circle in the middle of the image from scratch#
#Setting the angles#
angles <- seq(0,2*pi, length.out = 100)

#Storing in a dataframe for reference#
circle <- tibble(x = cos(angles) * (ymax/2) + (xmax/2) ,
                 y = sin(angles) * (ymax/2) + (ymax/2))

#Detecting points that only fit inside the circle#
#Basing off overlay data because it's already created#
fancy_circle <- overlay |>
  mutate(logic = point.in.polygon(x,y, circle$x, circle$y)) |>
  filter(logic == 1) 

#Setting fancy circle options#
circle_width <- 2
circle_color <- "#ffffff"

#==============================================================================#
#Layering Everything Together---------------------------------------------------
#==============================================================================#
#Layering this way reduces some extra code#
#inherit.aes is only needed in geom_segment#
#Because the requirements for geom_segment is different than the others#

overlay |>
  ggplot(aes(x,y))+
  theme_void()+
  geom_segment(data = back_segments, aes(x,y, xend = xend, yend = yend),
               color = segment_colors, 
               size = 10,
               inherit.aes = FALSE)+
  geom_path(aes(x,y, group = y),
            color = overlay_color, 
            size = overlay_sizes) +
  geom_path(data = fancy_circle, 
            aes(x,y, group = x), 
            color = circle_color, 
            size = 2, 
            lineend = "butt")+
  coord_equal(expand = FALSE)
