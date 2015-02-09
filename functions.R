library(ggplot2)

drawMap <- function(df) {
  mapWorld <- borders('world', colour='white', fill='white')
  mp <- ggplot() + mapWorld + 
    theme(panel.background = element_rect(fill = 'lightblue')) +
    geom_point(aes(x = df[,1], y = df[,2]), color = 'blue', size = 3) +
    coord_cartesian(xlim=c(min(df[,1]) - 5, max(df[,1]) + 5), 
                    ylim=c(min(df[,2]) - 5, max(df[,2]) + 5))
  mp
}