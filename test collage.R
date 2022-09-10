library(tidyverse)
library(ggpattern)



circle_fills <- list.files(here::here("images/materials"), 
                           pattern = "paint", 
                           full.names = TRUE)

half_cirle_maker <- function(min_x, max_x, min_y, max_y, pattern_fills){

  #Creating the "half-circle" shape====
  theta <- seq(0,pi, length.out = 100)
  half_circle <- tibble(x = cos(theta) ,
                        y = sin(theta) )
  
  #Calculating shifts in the half_circles====
  x_shifts <- seq(min_x, max_x, by = 2)
  y_shifts <- seq(min_y, max_y, by = 1)
  

  
  #Making the top base row====
  base_row <- map_df(x_shifts, ~ half_circle %>%
                                      mutate(x = x + .x,
                                             y = y + max_y - 1))

  
  #Row iterations====
  x_iters <- 1:length(x_shifts)
  
  #Column iterations====
  y_iters <- 1:length(y_shifts)
  
  #Group calculations
  group_total <- rep(1:(length(x_iters) * length(y_iters)), each = length(theta))
  
  #Making the full grid of shapes====
  final_grid <- map_df(y_shifts,~ base_row %>%
                                        mutate(y = y - .x))
  
  
  #Making the final grid of shapes====
  final_grid$group <- group_total
  
  #Randomizing and setting the patterns====
  circle_fills <- sample(pattern_fills, max(group_total), replace = TRUE)
  
  #applying colors
  final_grid$fill <- rep(circle_fills, each = length(theta)) 
  
  #Plotting the patterns====
  final_grid %>%
    ggplot(aes(x,y, group = group))+
    theme_void()+
    theme(plot.background = element_rect(fill = "black"))+
    geom_polygon_pattern(pattern = "image", 
                         pattern_filename = final_grid$fill, 
                         pattern_type = "expand")+
    
    coord_equal()
}


half_cirle_maker(0,10,0,10, circle_fills)

