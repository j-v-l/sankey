p_load(tidyr)

node <- 1:5
child <- list(c(2, 3), NA, c(4, 5), NA, NA)
parent <- c(NA, 1, 1, 3, 3) # Only one parent per node for now
xpos <- c(1, 5, 3, 5, 5)
ypos <- c(1.25, 3, 1, 0.75, 1.875)
size <- c(2.5, 1.0, 1.5, 1.0, 0.5)

node_descriptions <- unnest(data.frame(node, xpos, ypos, size))
node_relationships <- unnest(data.frame(node, parent, I(child)))
combined <- node_relationships %>%
  left_join(node_descriptions, by = "node") %>%
  left_join(node_descriptions, by = c("child" = "node"), suffix = c("_p", "_c"))  

# Want to create a ribbon for each row in the combined df, where
# (x0, y0, h0) = (xpos_p, ypos_p, size_p) and
# (x1, y1, h1) = (xpos_c, ypos_c, size_c)


