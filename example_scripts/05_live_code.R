#====R-Ladies Philly Live Code===#

#===============================================================================
#Library Load-in----------------------------------------------------------------
#===============================================================================

library(tidyverse) # For everything data
library(sp) # For polygon manipulation

#===============================================================================
#Set Color Palettes-------------------------------------------------------------
#===============================================================================

retro_blues <- c("#12012E", "#144267", "#15698C", "#0695AA", "#00F3FF")

retro_brights <- c("#F6BF07", "#F67C21", "#ED155A", "#F61867")

retro_purples <- c("#0F061B", "#1D0B35", "#350A3C", "#471049", "#881F74")

#===============================================================================
#X and y limits-----------------------------------------------------------------
#===============================================================================

xlim <- c(0,10)
ylim <- c(0,10)

#===============================================================================
#Sky Data-----------------------------------------------------------------------
#===============================================================================
sky <- tibble(x = xlim[1],
              xend = xlim[2],
              y = seq(10,2, length.out = 100),
              yend = y, #Horizontal lines means yend == y
              color = colorRampPalette(retro_purples)(100))
#===============================================================================
#Stars Data---------------------------------------------------------------------
#===============================================================================

stars <- expand.grid(tibble(x = seq(0,10, length.out = 50),
                            y = seq(2.5,10, length.out = 50))) |>
  slice_sample(prop= .10) #Pull out only 10% so it doesn't look like snow

#===============================================================================
#Mountains Data-----------------------------------------------------------------
#===============================================================================
#Or TREES I GUESS -.-
mountains <- tibble(x = seq(xlim[1],xlim[2], length.out = 100),
                   y = rnorm(x, mean = 4, sd = .5))

#===============================================================================
#Ocean Data---------------------------------------------------------------------
#===============================================================================

ocean_breaks <- seq(2,0, length.out = 100) # The "height" of the data
ocean_n <- 1:length(ocean_breaks) #Matching the length of ocean_breaks for clean iterations
ocean_colors <- colorRampPalette(retro_blues)(100) 
ocean_comps <- list(ocean_breaks, ocean_n, ocean_colors) #Easier notation for pmap_df

ocean <- pmap_df(ocean_comps, ~ tibble(x = seq(0,10, length.out = 100),
                                    y = ..1,
                                    group = paste0("wave",..2), #Need a group variable for distinct lines/patterns
                                    color = ..3))

#===============================================================================
#Sun Data-----------------------------------------------------------------------
#===============================================================================
angles <- seq(0, 2*pi, length.out = 100) # Creating all of the angles in a circle

sun_border <- tibble(x = cos(angles) * 2.5 + 5,
                     y = sin(angles) * 2.5 + 7) #Acts as a reference or boundary box of points to map eventually

sun <- expand.grid(tibble(x = seq(0,10, length.out = 100),
                          y = x)) |>
  mutate(logic = point.in.polygon(x,y, sun_border$x, sun_border$y)) |> # this fx from sp calculates points to fit the boundary from sun_border
  filter(logic == 1) #Filter only for the points that fit inside the reference circle (sun_border)

sun_colored <- sun |>
  mutate(color = colorRampPalette(retro_brights)(nrow(sun))) #Add on the colors for easier ggplot handling

#===============================================================================
#Final Image--------------------------------------------------------------------
#===============================================================================

sky |>
  ggplot(aes(x,y))+
  theme_void()+
  geom_segment(aes(xend = xend, yend = yend), #Need to manually name xend and yend here
               color = sky$color, 
               size = 10)+
  geom_point(data = stars,
             color = "#ffffff", 
             size = sample(seq(.01,.3, length.out = nrow(stars))),
             position = "jitter")+
  geom_path(data = sun_colored, aes(group = y),
            color = sun_colored$color, 
            size = seq(.9,4, length.out = nrow(sun_colored)))+
  geom_area(data = mountains,
            color = "black", 
            fill = retro_purples[2], #just picked a random "purple" here
            size = .5)+
  geom_path(data = ocean, aes(group = group),
            color = ocean$color, 
            position = "jitter")+
  coord_equal(xlim = xlim, 
              ylim = ylim, 
              expand = FALSE)

