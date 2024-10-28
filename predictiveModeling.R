# Loading required libraries
install.packages("igraph")
install.packages("sna")
library(igraph)
library(sna)
library(pacman)
p_load(igraph, readr)

# Reading the graph data from a file
my_graph <- read.table("/Users/rithik/Documents/3rd semester/Big data/Class_Project_1/soc-Epinions1_adj.tsv", header = FALSE)

# Displaying the first few rows of the graph data
head(my_graph)

# Converting the graph data to a matrix and removing the third column
my_graph <- as.matrix(my_graph)
my_graph_1 <- my_graph[, -3]

# Displaying the first few rows of the modified graph data
head(my_graph_1)

# Converting the modified graph data to an edge list matrix
my_graph_2 <- as.matrix(my_graph_1)

# Displaying the first few rows of the edge list matrix
head(my_graph_2)

# Extracting vertices and creating a data frame of relations
n <- nrow(my_graph_2)
V_1 <- my_graph_2[1:n, 1]
V_2 <- my_graph_2[1:n, 2]
relations_V1_V2 <- data.frame(from = V_1, to = V_2)

# Creating a graph from the data frame of relations
my_graph_3 <- graph_from_data_frame(relations_V1_V2, directed = TRUE) 

# Plotting the graph
plot.igraph(my_graph_3)


# Using Walktrap Community detection algorithm to identify communities
community <- cluster_walktrap(my_graph_3, steps = 10)

# Contracting vertices based on community membership
E(my_graph_3)$weight <- 1
V(my_graph_3)$weight <- 1
simplified_graph <- contract(my_graph_3, community$membership, vertex.attr.comb = list(weight = "sum", name = function(x) x[1], "ignore"))

# Simplifying edges by combining them
simplified_graph <- simplify(simplified_graph, edge.attr.comb = list(weight = "sum", function(x) length(x)))

# Creating a simplified subgraph based on vertex weights
my_simplified_graph <- induced_subgraph(simplified_graph, V(simplified_graph)$weight > 20)

# Calculating and adding degrees to vertices
V(my_simplified_graph)$degree <- unname(degree(my_simplified_graph))

# Deleting vertices with zero degree
my_simplified_graph <- delete_vertices(my_simplified_graph, which(degree(my_simplified_graph) == 0))

# Plotting the simplified graph
plot.igraph(my_simplified_graph)

# Displaying vertices and edges of the simplified graph
V(my_simplified_graph)
E(my_simplified_graph)

# Extracting adjacency matrix of the simplified graph
my_simplified_graph_adj <- igraph::as_adjacency_matrix(my_simplified_graph)
head(my_simplified_graph_adj)

# Calculating density of the simplified graph
my_simplified_graph_density <- sna::gden(my_graph_2)
my_simplified_graph_density

# Calculating edge density of the simplified graph
my_simplified_graph_edge_density <- igraph::edge_density(my_simplified_graph)
my_simplified_graph_edge_density

# Calculating edge density of the original graph
igraph::edge_density(my_simplified_graph, loops = TRUE)
igraph::edge_density(my_graph_3, loops = TRUE)

# Extracting ego networks from the simplified graph
my_simplified_graph_ego <- sna::ego.extract(my_graph_2[1:5000, 1:2])
my_simplified_graph_ego[1]

# Calculating degrees of vertices in the simplified graph
my_simplified_graph_degree <- igraph::degree(my_simplified_graph)

# Calculating betweenness centrality of vertices in the simplified graph
my_simplified_graph_between <- igraph::centr_betw(my_simplified_graph)
my_simplified_graph_between

# Calculating closeness centrality of vertices in the simplified graph
my_simplified_graph_closeness <- igraph::centr_clo(my_simplified_graph)
my_simplified_graph_closeness

# Calculating shortest paths in the simplified graph
my_simplified_graph.sp <- igraph::distances(my_simplified_graph)
igraph::shortest_paths(my_simplified_graph, from = 5)

# Calculating the square of the adjacency matrix
my_simplified_graph_np <- my_simplified_graph_adj %*% my_simplified_graph_adj
my_simplified_graph_np

# Plotting a histogram of degrees in the simplified graph
hist(igraph::degree(my_simplified_graph), breaks = 40)

# Calculating the diameter of the simplified graph
my_simplified_graph_d <- igraph::diameter(my_simplified_graph)
my_simplified_graph_d

# Finding maximum cliques in the simplified graph
node <- c(5)
my_simplified_graph_5clique <- igraph::max_cliques(my_simplified_graph, min = NULL, max = NULL, subset = node)
my_simplified_graph_5clique


# Extract the edge list from the simplified graph
my_simplified_graph_edgelist <- get.edgelist(my_simplified_graph)

# Compute geodesic distances between pairs of vertices in the graph
# The geodist function from the sna package calculates the shortest path distances
# between all pairs of vertices in a graph
my_graph_geos <- sna::geodist(my_simplified_graph_edgelist)

# Display the geodesic distances
my_graph_geos


# Calculate edge density for the simplified graph with loops allowed
igraph::edge_density(my_simplified_graph, loops = TRUE)

# Calculate edge density for the original graph with loops allowed
igraph::edge_density(my_graph_3, loops = TRUE)


# Finding central nodes based on degree centrality
V(my_simplified_graph)$central_degree <- centr_degree(my_simplified_graph)$res
V(my_simplified_graph)$name[V(my_simplified_graph)$central_degree == max(centr_degree(my_simplified_graph)$res)]

# Extracting the longest path in the graph
longestpath <- induced_subgraph(my_simplified_graph, get_diameter(my_simplified_graph))
plot(longestpath, vertex.color = "lightblue", edge.arrow.size = 0.5, vertex.label.cex = 0.4, vertex.size = 15, layout = layout_with_graphopt)

# Calculating the number of cliques in the simplified graph
my_simplified_graph_lgcliques <- igraph::clique_num(my_simplified_graph)
my_simplified_graph_lgcliques

# Finding largest cliques in the simplified graph
my_simplified_graph_lgcliques_path <- igraph::largest_cliques(my_simplified_graph)
my_simplified_graph_lgcliques_path

# Creating a subgraph of the largest clique
graph_clique <- induced.subgraph(my_simplified_graph, my_simplified_graph_lgcliques_path[[1]])
graph_clique

# Plotting the subgraph of the largest clique
plot(graph_clique, vertex.color = "lightblue", edge.arrow.size = 0.5, vertex.label.cex = 0.4, vertex.size = 15, layout = layout_with_graphopt)

# Extracting the diameter of the graph
diameter <- get_diameter(my_simplified_graph)
diam <- induced.subgraph(my_simplified_graph, diameter)

# Plotting the graph with the diameter
plot(diam)

# Extracting ego networks in the graph
ego_graph <- igraph::ego(my_simplified_graph)
ego_graph

# Calculating power centrality
power_centrality(my_simplified_graph, exponent = 0.8)
