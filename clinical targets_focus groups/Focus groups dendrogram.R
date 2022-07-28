# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(viridis)
library(igraph)
library(readxl)

d2 <- read_excel("C:/Users/valer/Desktop/focus groups .xlsx", 1)
d1 <- read_excel("C:/Users/valer/Desktop/focus groups .xlsx", 2)
d3 <- read_excel("C:/Users/valer/Desktop/focus groups .xlsx", 3)


d1=data.frame(d1)
d2=data.frame(d2)
edges=rbind(d1, d2)

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = d3
  #value = runif(33)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name, edges$to ) ]


#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves
vertices$angle= 0

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
vertices$hjust<-ifelse(vertices$id > 10 & vertices$id < 24, 1, 0)
# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# Create a graph object
mygraph <- graph_from_data_frame(edges, vertices=vertices)

# prepare color
#mycolor <- colormap(colormap = colormaps$viridis, nshades = 6, format = "hex", alpha = 1, reverse = FALSE)[sample(c(1:6), 10, replace=TRUE)]

# Make the plot
graph <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.10, y=y*1.20, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=3.5, alpha=1) +
  #geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=3.5, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.03, y=y*1.03, colour=group, size=value, alpha=0.2)) +
  #geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  #scale_colour_manual(values= mycolor) +
  scale_size_continuous( range = c(0.1,7) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-2.5, 2.5), y = c(-2.5, 2.5))
graph

