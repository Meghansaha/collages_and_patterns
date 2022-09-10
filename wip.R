# Pattern Background Image #

library(tidyverse)




colors <- c("#181818", "#d3d3d3", "#88398A","#562457","#ccbdcc")

rows <- 9
cols <- 9
lines <- 20

grid_iters <- 1:(rows*cols)
row_iters <- 1:cols
col_iters <- 1:rows

start_ori <- sample(c("vert", "horiz"),1)

base_oris <- if(start_ori == "vert"){
        rep_along(1:cols, c("vert","horiz"))
} else{
  rep_along(1:cols, c("horiz", "vert"))
}

inverse_oris <- if_else(base_oris == "vert", "horiz", "vert")

grid_oris <- rep_along(1:rows, c("base","inverse"))

vert_base <- tibble(x = seq(0,1, length.out = lines),
                    xend = x,
                    y = 0,
                    yend = 1,
                    group = "vert")

horiz_base <- tibble(y = seq(0,1, length.out = lines),
                    yend = y,
                    x = 0,
                    xend = 1, 
                    group = "horiz")


base_row <- pmap_df(list(row_iters,
                        base_oris,
                        sample(colorRampPalette(colors)(100), rows, replace = TRUE)), ~if(..2 == "vert"){
                                          vert_base %>%
                                            mutate(x = x + ..1,
                                                   xend = x,
                                                   group = paste0(group,..1),
                                                   fill = ..3)
                        } else {
                          horiz_base %>%
                            mutate(x = x + ..1,
                                   xend = xend + ..1,
                                   group = paste0(group,..1),
                                   fill = ..3)
                        }
                   )

inverse_row <- pmap_df(list(row_iters,
                            inverse_oris,
                            sample(colorRampPalette(c(colors))(100), rows, replace = TRUE)), ~if(..2 == "vert"){
                              vert_base %>%
                                mutate(x = x + ..1,
                                       xend = x,
                                       group = paste0(group,..1),
                                       fill = ..3)
                            } else {
                              horiz_base %>%
                                mutate(x = x + ..1,
                                       xend = xend + ..1,
                                       group = paste0(group,..1),
                                       fill = ..3)
                            }
)

grid_frame <- pmap_df(list(0:(rows - 1),
                           grid_oris), ~if(..2 == "base"){
                             base_row %>%
                               mutate(y = y + ..1,
                                      yend = yend + ..1)
                           } else {
                             inverse_row %>%
                               mutate(y = y + ..1,
                                      yend = yend + ..1)
                           })

line_sizes <- sample(seq(.7,1, length.out = nrow(grid_frame)))

grid_frame2 <- grid_frame %>%
  slice_sample(prop = .20)

line_sizes2 <- sample(seq(.7,1, length.out = nrow(grid_frame2)))

grid_frame2 %>%
  ggplot(aes(x,y, xend = xend, yend = yend))+
  theme_void()+
  geom_segment(color = sort(grid_frame2$fill), size = line_sizes2, linetype = "1232")+
  geom_segment(color = map_chr(grid_frame2$fill, ~colorRampPalette(c(.x,"#000000"))(10)[7]), size = line_sizes2 - .59)+
  theme(plot.background =  element_rect(fill = "#000000"))+
  coord_polar()