####
# Marriage and the Medici clan
####

## load the igraph package
library(igraph) 

medici = as.matrix(read.table(url("https://raw.githubusercontent.com/jgscott/STA380/master/data/medici.txt")))

## create the graph object
marriage = graph.edgelist(medici, directed=FALSE)

## set some color atributes (V() gives back the 'vertices' = nodes)
V(marriage)$color = "orange" #the nodes color
V(marriage)["Medici"]$color = "lightblue" #only MEDICI is blue
V(marriage)$frame.color = 0
V(marriage)$label.color = "black" #no background

## plot it
plot(marriage, edge.curved=FALSE)

## print the degree for each family
sort(degree(marriage))

## calculate and color a couple shortest paths
#get.shortest.path only return 1st shorest path
PtoA = get.shortest.paths(marriage, from="Peruzzi", to="Acciaiuoli")
allPtoA = all_shortest_paths(marriage, from="Peruzzi", to="Acciaiuoli")


# Somewhat confusing return value
# vpath is a list of the shortest paths 
# First element of vpath is then your vector 
# of vertices along the path.

PtoA$vpath[[1]] #1st element in vpath is the vetor of the nodes in the shortest path

GtoS = get.shortest.paths(marriage, from="Ginori", to="Strozzi")
GtoS$vpath[[1]]

# color the edges along these paths
# and set the rest to grey
E(marriage)$width = 2
E(marriage)$color = "grey"
E(marriage, path=PtoA$vpath[[1]])$color = "purple"
E(marriage, path=GtoS$vpath[[1]])$color = "darkgreen"
plot(marriage)

## print the betweenness for each family
sort(round(betweenness(marriage),1))
#Pick any two random nodes expect Medici
#all the shortest path that connect these two nodes
#47.5% of those path pass through Medici