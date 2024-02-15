# Libraries
# https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram.html
# https://stackoverflow.com/questions/51277206/plotting-a-tree-hierarchy-using-ggraph
# https://exts.ggplot2.tidyverse.org/ggraph.html

# https://stackoverflow.com/questions/54185514/how-to-control-node-color-in-ggraph
# https://www.data-imaginist.com/2017/ggraph-introduction-edges/
library(ggraph)
library(igraph)
library(tidyverse)
theme_set(theme_void())
library(magrittr)

#### 自定义写法

dt_kin01_v2 <- data.frame(
  from = c('wangbo'),
  to = c('wanga', 'wangb', 'wangc')
)

dt_kin02_v2 <- data.frame(
  from = c('wanga', 'wanga', 'wangb', 'wangb', 'wangb'),
  to = c('wang1', 'wang2', 'wang3', 'wang4', 'wang5')
)

dt_kin03_v2 <- data.frame(
  from = c('wang1', 'wang1', 'wang1', 'wang2'),
  to = c('a', 'b', 'c', 'd')
)

dt_kin_v2 <- rbind(dt_kin01_v2, dt_kin02_v2, dt_kin03_v2)

name2 <- unique(c(as.character(dt_kin_v2$from), as.character(dt_kin_v2$to)))
vertices2 <- data.frame(
  name=name2,
  female=sample(c('1','0'), length(name2), replace=T)
  #cluster=sample(letters[1:4], length(name), replace=T),
  #value=sample(seq(10,30), length(name), replace=T)
)

# Create a graph object
mygraph2 <- graph_from_data_frame( dt_kin_v2, vertices=vertices2)

ggraph(mygraph2, 'igraph', algorithm = 'tree'
       #, circular = TRUE
       ) + 
  geom_edge_diagonal(aes(alpha = ..index..)) +
  #geom_node_text(aes(label = name), size = 3) +
  geom_node_point(aes(color = as.factor(female)))+
  coord_fixed() + 
  #scale_edge_alpha('Direction', guide = 'edge_direction') +
  #geom_node_point(aes(filter = degree(mygraph2, mode = 'out') == 0), 
  #                color = 'steelblue', size = 1) +
  ggforce::theme_no_axes()



.vertices <- data.frame(
  flare$vertices,
  female=sample(c('1','0'), dim(flare$vertices)[1], replace=T)
)
.vertices %<>% select(name, female)
.edges <- flare$edges %>%
  left_join(.vertices, by = c('to' = 'name'))
flareGraph <- graph_from_data_frame(.edges, vertices = .vertices)
ggraph(flareGraph, 'igraph', algorithm = 'tree'
       #, circular = TRUE
) + 
  geom_edge_diagonal(aes(color = as.factor(female)), alpha = .3, size = .5) +
  #geom_node_text(aes(label = name), size = 3) +

  geom_node_point(aes(color = as.factor(female)), size = 1, alpha = .5)+
  #coord_fixed() + 
  #scale_edge_alpha('Direction', guide = 'edge_direction') +
  #geom_node_point(aes(filter = degree(mygraph2, mode = 'out') == 0), 
  #                color = 'steelblue', size = 1) +
  scale_y_reverse() +
  ggforce::theme_no_axes()
