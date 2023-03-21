
############## Importing libraries #################

library("knitr")
include_graphics("C:/Users/ximen/OneDrive/MMA/4/Organizational Network/Centrality/busmap.png")

library(readxl)
library(igraph)


############## Reading data #################

# Translating image into a database 

edges = read_excel("C:/Users/ximen/OneDrive/MMA/4/Organizational Network/Centrality/edges.xlsx")


nodes = c("1", "2", "3", "4", "5", "6", "A", "B", "C", "D")
nodes = data.frame(nodes)

############## Centrality Values #################

routes = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

degree_cent = degree(routes, mode="all")

closeness_cent = closeness(routes, mode="all")
betweenness_cent = betweenness(routes)

# create a vector of vertex labels
vertex_labels = paste("Node:", V(routes)$name, "\nDeg:", degree_cent, "\nBet:", round(betweenness_cent,2), "\nClo:", round(closeness_cent,2))

# create a vector of vertex sizes
vertex_sizes = degree_cent*5

# Generate a new layout with increased spacing between nodes
layout = layout_with_fr(routes, niter=1000)

############## Plotting #################
# plot the graph

plot(routes, layout=layout, vertex.label=vertex_labels, vertex.size=vertex_sizes, vertex.label.cex = 1.2, vertex.color="light blue", main="Fakebook Network with Centrality Values")
