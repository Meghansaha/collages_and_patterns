library(tidyverse)
library(sp)

set.seed(221)

angles <- seq(0,2*pi, length.out = 100)

circle <- tibble(x = sin(angles),
                 y = cos(angles))

patterns <- tibble(crossing(x = sin(angles),
                   y = cos(angles)))
  

  patterns_cut <- patterns %>%
    mutate(logic = point.in.polygon(x,y, circle$x, circle$y)) %>%
    filter(logic == 1) %>%
    mutate(logic = if_else(tan(x)^5 +  cos(x*y) < 1, TRUE, FALSE)) %>%
    mutate(color = if_else(logic == TRUE, "#000000", "#8c8989")) %>%
    arrange(desc(y))
  
  colors <- c("#af3918", "#a21152", "#822b75","#612884","#154baf",
              "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a")
  
  color_pal <- tibble(color = colorRampPalette(colors)(nrow(patterns_cut))) %>%
    arrange(desc(color))
  
  
  
  circle %>%
    ggplot(aes(x,y))+
    theme_void()+
    geom_point(data = patterns_cut, aes(x,y), 
               shape = 21,
               stroke = seq(3,.1, length.out = nrow(patterns_cut)),
               color = "#000000",
              fill = sort(color_pal$color), size = 8,
              alpha = sort(sample(seq(.1,.5, length.out = nrow(patterns_cut)), replace = TRUE)),
              position = position_jitter(width = .05, height = .09),inherit.aes = FALSE)+
    theme(
          plot.background = element_rect(fill = "#ffffff"))+
    coord_equal()
  
