# Curve Manipulations

library(tidyverse)
library(purrr)

n <- 100

basic_sine <- tibble(x = seq(0,1000, length = 100),
                     y = sin(x))

basic_sine %>%
  ggplot(aes(x,y))+
  geom_path()+
  coord_equal()

transformers <- seq(1,10, length = n)

wave_number <- 1:n

rb_colors <- c("#af3918", "#a21152", "#822b75","#612884","#154baf",
               "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a")

color_pal <- colorRampPalette(rb_colors)(n)

sines_df <- map2_df(transformers, 
                    color_pal, ~ basic_sine %>%
                                    mutate(y = y/.x,
                                           colors = .y,
                                           group = .x))

sines_df %>%
  ggplot(aes(x,y, group = group))+
  theme_void()+
  geom_path(color = "#1a1a1a", size = 3, linejoin = "mitre", lineend = "round")+
  geom_path(color = sample(sines_df$colors), size = 2, linejoin = "round", lineend = "round")+
  geom_path(color = "#000000", size = .1, linejoin = "mitre", lineend = "round")+
  theme(plot.background = element_rect(fill = "#000000"))+
  coord_polar()


divide_sine <- basic_sine %>%
  mutate(y = y/seq(2,10, length = 100))

divide_sine %>%
  ggplot(aes(x,y))+
  geom_path()+
  coord_equal()

multi_sine <- basic_sine %>%
  mutate(y = y * seq(2,10, length = 100))

multi_sine %>%
  ggplot(aes(x,y))+
  geom_path()+
  coord_equal()

exp_sine <- basic_sine %>%
  mutate(y = y ^ seq(2,10, length = 100))

exp_sine %>%
  ggplot(aes(x,y))+
  geom_path()+
  coord_equal()

add_sine <- basic_sine %>%
  mutate(y = y + seq(2,10, length = 100))

add_sine %>%
  ggplot(aes(x,y))+
  geom_path()+
  coord_equal()

min_sine <- basic_sine %>%
  mutate(y = y - seq(2,10, length = 100))

min_sine %>%
  ggplot(aes(x,y))+
  geom_path()+
  coord_equal()




#===============================================================================
#Library Load In----------------------------------------------------------------
#===============================================================================

library(dplyr) # For manipulations and because I like magrittr's pipe 
library(ggplot2) # For plotting
library(purrr) # For iteration handling

#===============================================================================
#Data Creation -----------------------------------------------------------------
#===============================================================================

#Number of points we want to exist in our wave
n <- 100

#The basic sine wave
basic_sine <- tibble(crossing(x = seq(0,10, length = n),
                     y = sin(x)))

#Altering the basic wave by adding alternate values to it's y value
alt_sine <- basic_sine %>%
  mutate(y = y + (seq(1,10, length = n) * rep_along(1:nrow(basic_sine),c(1,-100))))

#===============================================================================
#Color Selection ---------------------------------------------------------------
#===============================================================================

#My rainbow color palette
rb_colors <- c("#af3918", "#a21152", "#822b75","#612884","#154baf",
            "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a")

#expanding the palette
color_pal <- colorRampPalette(rb_colors)(nrow(alt_sine))

#===============================================================================
#Plotting the aRt --------------------------------------------------------------
#===============================================================================

alt_sine %>%
  ggplot(aes(x,y))+
  theme_void()+
  geom_path(color = color_pal, size = 6, linejoin = "round", lineend = "round")+
  geom_point(shape = 21, fill = NA, color = "#000000", size = 2, linejoin = "mitre", lineend = "round")+
  theme(plot.background = element_rect(fill = "#000000"))+
  coord_polar()

#===============================================================================
#Library Load In----------------------------------------------------------------
#===============================================================================

library(dplyr) # For manipulations and because I like magrittr's pipe 
library(ggplot2) # For plotting
library(purrr) # For iteration handling

#===============================================================================
#Data Creation -----------------------------------------------------------------
#===============================================================================

#How many waves/iterations we want
n <- 100 

full_n <- n*2

#The basic sine wave
basic_sine <- tibble(x = c(1,5,5,1,1),
                     y = c(1,1,5,5,1),
                     group = "base")

#Numbers to divide/transform the basic wave by
transformers <- sample(seq(1,500, length.out = n)) 

#Number of iterations we'll preform in the map2 fx
wave_number <- 1:n

#My rainbow color palette
rb_colors <- c("#af3918", "#a21152", "#822b75","#612884","#154baf",
               "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a")

#Expanding the palette to reach our "n" value
color_pal <- colorRampPalette(rb_colors)(n)

#Iterating over our base wave to create a df with multiple waves/transformations
square_row <- map_df(transformers, ~ basic_sine %>%
                                      mutate(x = x + .x ))

square_grid <- map2_df(transformers, 
                       color_pal, ~ square_row %>%
                                       mutate(y = y + .x ,
                                              color = .y,
                                              group = .x)) 



#===============================================================================
#Plotting the aRt --------------------------------------------------------------
#===============================================================================

square_grid %>%
  ggplot(aes(x,y, group = group))+
  theme_void()+
  geom_path(fill = "#000000", size = 4, alpha = .9, lineend = "round")+
  geom_path(color = sort(square_grid$color), size = 5, linejoin = "round", lineend = "round")+
  geom_path(color = "#025ab3", size = .1, linejoin = "mitre", lineend = "round")+
  theme(plot.background = element_rect(fill = "#025ab3"))+
  coord_polar()







