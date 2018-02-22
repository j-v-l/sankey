library(pacman)
p_load(dplyr, ggplot2, reshape2)


ribbon <- function(x0, y0, h0, x1, y1, h1, stepsize = 0.05) {
  # Create a df to pass to geom_ribbon that will draw a ribbon from (x0, y0) to (x1, y1), 
  # with height (above the base points) going from h0 to h1
  
  x <- seq(x0, x1, by = stepsize)
  nsteps <- length(x)
  
  xx <- seq(-pi/2, pi/2, length.out = nsteps)
  ymin <- y0 + (y1 - y0) * (sin(xx) + 1) / 2
  ymax <- ymin + seq(h0, h1, length.out = nsteps)
  
  data.frame(x, ymin, ymax)
}


fill <- "grey70"
color <- "grey30"
ggplot(data.frame(), aes(x = x, ymin = ymin, ymax = ymax)) + 
  geom_ribbon(data = ribbon(1, 1.25, 1.5, 3, 1, 1.5), fill = fill, color = color) +
  geom_ribbon(data = ribbon(1, 2.75, 1, 3, 3, 1), fill = fill, color = color) +
  geom_ribbon(data = ribbon(3, 1, 1, 5, 0.75, 1), fill = fill, color = color) +
  geom_ribbon(data = ribbon(3, 2, 0.5, 5, 1.875, 0.5), fill = fill, color = color) +
  xlim(0, 6) + ylim(0, 5)

# Seems like heights of parent + child should be the same for a particular ribbon, given the context
