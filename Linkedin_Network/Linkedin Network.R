
# Import CSV
connections = read.csv("C:/Users/ximen/OneDrive/MMA/4/Organizational Network/Ex 1 Linkedin/Connections.csv")
attach(connections)


# Create a table with 
connections$name = paste(connections$First.Name, connections$Last.Name, sep = " ")
connections = connections[, c("name", "Company","Position", "Connected.On")]

# create a frequency table
freq_table = table(connections$Company)
freq_table = sort(freq_table, decreasing = TRUE)
top15= head(freq_table, n = 15)

# display the frequency table
top15

# create a bar chart of the frequency table
barplot(top15, main = "Top 15 Connections on Linkedin", 
        xlab = "Company", ylab = "Number of Connections",
        col = "steelblue",las = 2,cex.names = 0.8)


barplot(top15, main = "Top 15 Connections on Linkedin", 
         ylab = "Number of Connections",
        col = "steelblue", las = 2, cex.names = 0.8)


barplot(top15, main = "Top 15 Connections on Linkedin", 
        xlab = "Company", ylab = "Number of Connections",
        col = "steelblue", las = 2, cex.names = 0.8, cex.axis = 0.7)



######### Creating nodes

library(tidyverse)

people <- connections %>%
  distinct(name) %>%
  rename(label = name)

companies <- connections %>%
  distinct(Company) %>%
  rename(label = Company)

nodes <- full_join(people, companies, by = "label")
nodes <- rowid_to_column(nodes, "id")
nodes

#### Creating edges

edges <- connections[, c("name", "Company")]

edges <- edges %>% 
  left_join(nodes, by = c("name" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Company" = "label")) %>% 
  rename(to = id)

edges <- unique(select(edges, from, to))
edges

## Building network
library(network)

routes_network <- network(edges,
                          vertex.attr = nodes,
                          matrix.type = "edgelist",
                          ignore.eval = FALSE)
plot(routes_network, vertex.cex = 1)

## igraph
library(igraph)
routes_igraph <- graph_from_data_frame(d = edges,
                                       vertices = nodes,
                                       directed = TRUE)

plot(routes_igraph,
     vertex.size = 3,
     vertex.label.cex = 0.2,
     edge.arrow.size = 0.01)

## visNetwork

install.packages("visNetwork")
install.packages("networkD3")
library(visNetwork)
library(networkD3)
visNetwork(nodes, edges)

#############CF#############

data = connections
nodes <- data %>% select(c("name", "Company"))

library(tidyverse)
nodes <- nodes %>% rowid_to_column("id")

edges <- data %>% select(c(name, Company)) %>% 
  left_join(nodes %>% select(c(id,name)), by = c("name"="name"))

edges <- edges %>% left_join(edges, by = "Company", keep=FALSE) %>% 
  select(c("id.x", "id.y", "Company")) %>% 
  filter(id.x!=id.y)


colnames(edges) <- c("x", "y", "Company")

graph <- tbl_graph(edges = edges, nodes=nodes, directed = FALSE)

ggraph(graph, layout = "graphopt") + 
  geom_edge_link( show.legend = FALSE) + 
  geom_node_point()+
  theme_graph()




##CF#

connections$id = 1:nrow(connections)

nodes = connections %>% select(c("id","name", "Company"))

edges = connections %>% select(c(name, Company)) %>% 
  left_join(nodes %>% select(c(id,name)), by = c("name"="name"))

edges <- edges %>% left_join(edges, by = "Company", keep=FALSE) %>% 
  select(c("id.x", "id.y", "Company")) %>% 
  filter(id.x!=id.y)


colnames(edges) <- c("x", "y", "Company")

graph <- tbl_graph(edges = edges, nodes=nodes, directed = FALSE)

ggraph(graph, layout = "graphopt") + 
  geom_edge_link( show.legend = FALSE) + 
  geom_node_point()+
  theme_graph()

############XR#



names(connections)[1] = "to"
names(connections)[2] = "from"

library(tidygraph)

graph_connections <- as_tbl_graph(connections)

graph_connections

#2
library(stringr)

graph_connections <- graph_connections %>%
  activate(nodes) %>%
  mutate(
    title = str_to_title(name),
    label = str_replace_all(title, " ", "\n")
  )

graph_connections

library(ggraph) 

graph_connections %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_diagonal() 


graph_connections %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = name, color = name), size = 3) +
  geom_edge_diagonal(color = "gray", alpha = 0.4) 
