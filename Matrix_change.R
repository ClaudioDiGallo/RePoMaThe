#Differences of networks

#Also wir wollen hier unterschiede von zwei netzwerken zeigen. DAs heisst, Netzwerk1 zu t1 unterschied zu netzwerk2 zu t2. Wir wollen die Matrizen minus-rechnen.
#DAs problem ist, dass nicht alle spalten und reihen sind bei beiden vorhanden. Wenn also matrix1 - matrix2 gerechnet wird, werden nur am Anfang die richtigen minus-gerechnet.
#Lösung: Brechstange: Die Netzwerke werden aufgemacht, es wird eine edgelist gebildet, die mit Gewichtung verdichtet wird.
#Die Gewichtungen der Edges von N2 wird minus die gewichteten Edges von N1 gerechnet. Das ist die Veränderung der Anzahl Edges zwischen allen relevanten Knotenpaaren.
#Für jeden Punkt werden dann alle zugehörigen Edge-Gewichtungen summiert. Weil die Edges auch negative Gewichtungen haben können, kann damit auch gezeigt werden,
#Welche PUnkte an degree eingebüsst haben.


graph1 <- erdos.renyi.game(20, p=0.46, type = c("gnp", "gnm"), directed = FALSE,
                 loops = FALSE)
plot(graph1)
graph2 <- erdos.renyi.game(20, p=0.10, type = c("gnp", "gnm"), directed = FALSE,
                           loops = FALSE)
plot(graph2)

erdos.renyi.game()

tbl <- get.edgelist(graph1)# Wir want here to get a weigth for our number of edges between every pair of nodes

a <- tibble(tbl) %>% count(tbl[,1], tbl[,2]) #this counts all the edges between every possible pair of edges, but also of every edges with itself (which is bad)
b <- a[1]==a[2] #this is a vector which is true, if a row is not an edge (i.e. the diagonals i.e. r/politics coupled with r/politics). (The diagonals don't give us information and it isn't how we want to order the edges)

a <- a[!b,] #Getting rid of the diagonals
top_half <- a #This are the all the realised edges and how many times this edges was present in the graph.
top_half$n <- top_half$n*-1 #Because we want to have a difference, we have to make the weight negative in the first network



tbl <- get.edgelist(graph2)# The same as above, but with the second Network
# tbl <- tibble(tbl)
# tbl <- add_row(tbl,tbl)

a <- tibble(tbl) %>% count(tbl[,1], tbl[,2])
b <- a[1]==a[2] 
a <- a[!b,] 

top_half$n <- top_half$n*-1 #Because we want to have a difference, we have to make the weight negative in the first network

difference_matrix <- add_row(top_half,a) #We combine the to weigthed edgelists

difference_matrix$n



#The next line is more complicated: First we group the combined weighted edgelist into all unique realized edge-combinations and sum all the weighted-Values.
#That means, if we started with 2 Edges between Node 1 and 2 and in the second network we have 1 Edge between Node 1 and 2.
#We calculate 1 Edge minus 2 Edges. This gives us the change of number of edges between Node 1 and 2, which is -1.
#And we to this for all realised Node-Pairs.
#Note: if an edge between Node 1 and 2 ist not realized in network 1 but in network 2, the edgeweight is for network 1 is zero and the change is the amount of edgweight of network 2.

difference_matrix <- difference_matrix%>% group_by(difference_matrix[,1],difference_matrix[,2]) %>% summarise(n=sum(n))

difference_matrix

edgelist2 <- graph_from_edgelist(as.matrix(difference_matrix[1:2]),directed = FALSE) #We construct a graph wich has from all realised Edges of both networks
E(edgelist2)$n <- difference_matrix$n #We add the edge difference to the edges.
E(edgelist2)$n


# edgelist2 <- graph_from_edgelist(as.matrix(a[1:2]),directed = FALSE) #we construct a graph from the bottom. We need to do this, because rearranging the sequence of edges is hard. And now the edges are ordered alphabetically. This is important for adding further attributes.
# # V(edgelist2)

# E(edgelist2)$weigth <- a$n #we add the weights, that means the number of edges between each pair of nodes 

#degree() Fails



# V(edgelist2)$degree <- difference_matrix %>% ungroup() %>% group_by(difference_matrix[,1]) %>% summarise(n=sum(n)) %>% select(n) %>% unlist() %>% unname()

#Now we want to get the change of degree for every Node
#That means we have to sum all the edges which are related to a Node. And do this for every node. At the end, we need a vector the lenght and ordering the number of nodes.
#Because the edgelist is not symmetrical, we construct such a vector for both columns.
#We group by the nodes and sum all the edge-weights of this node.
#The ordering is in a way, that the last node ist not in the first column and the first node is not in the second column. That's why we ned to construct vectors:
#So that they have the appropriate length and the positioning of the nodes-values is right.
# r <- 0
# r <- rep(0,length(V(edgelist2)))
# r[1:(length(V(edgelist2))-1)]<- difference_matrix %>% ungroup() %>% group_by(difference_matrix[,1]) %>% summarise(n=sum(n)) %>% select(n) %>% unlist() %>% unname()
# (r)
# s <- rep(0,length(V(edgelist2)))
# s[2:length(V(edgelist2))]<- difference_matrix %>% ungroup() %>% group_by(difference_matrix[,2]) %>% summarise(n=sum(n)) %>% select(n) %>% unlist() %>% unname()
# s
# V(edgelist2)$degree <- unlist(r+s)
# V(edgelist2)$degree
# 
# difference_matrix %>% ungroup() %>% group_by(difference_matrix[,2]) %>% summarise(n=sum(n)) %>% select(n) %>% unlist() %>% unname()

#"Description

#Summing up the edge weights of the adjacent edges for each vertex. " - Igraph packeg manual to strength()
#For each vertex, all difference in edges will be summed up, which gives us the degree.
V(edgelist2)$degree <- strength(edgelist2, vids = V(edgelist2), mode = c("all", "out", "in", "total"),
         loops = TRUE, weights = E(edgelist2)$n)

V(edgelist2)$degree



geom_point(aes(colour = z1)) +
  scale_colour_gradient2()


ggraph(edgelist2) +
  geom_edge_link0(
    aes(#width = E(edgelist2)$weigth),
      alpha = 2)) +
  #E(edgelist2)$weigth)) +
  #scale_edge_width(range = c(0, 20)) +
  #geom_edge_density(aes(fill =reduced_weight)) +
  geom_node_point(aes(size= 3,colour= unlist(V(edgelist2)$degree))) +
  scale_colour_gradient2()+
  ggtitle("helloworld")
  #geom_node_text(aes(), repel = TRUE, colour="blue") +
  theme_graph()



######### Legacy Code

#Matrix Changes


graph1 <- erdos.renyi.game(20, p=0.16, type = c("gnp", "gnm"), directed = FALSE,
                           loops = FALSE)
plot(graph1)
graph2 <- erdos.renyi.game(20, p=0.16, type = c("gnp", "gnm"), directed = FALSE,
                           loops = FALSE)
plot(graph2)



tbl <- get.edgelist(graph1)# Wir want here to get a weigth for our number of edges between every pair of nodes
tbl
a
a <- tibble(tbl) %>% count(tbl[,1], tbl[,2]) #this counts all the edges between every possible pair of edges, but also of every edges with itself (which is bad)
b <- a[1]==a[2] #this is a vector which is true, if a row is not an edge (i.e. the diagonals i.e. r/politics coupled with r/politics). (The diagonals don't give us information and it isn't how we want to order the edges)

a <- a[!b,] #This are all existing edges between the important_subreddits-Vertices and we have a column which is a weight called "n".
top_half <- a


tbl <- get.edgelist(graph2)# Wir want here to get a weigth for our number of edges between every pair of nodes
tbl
a
a <- tibble(tbl) %>% count(tbl[,1], tbl[,2]) #this counts all the edges between every possible pair of edges, but also of every edges with itself (which is bad)
b <- a[1]==a[2] #this is a vector which is true, if a row is not an edge (i.e. the diagonals i.e. r/politics coupled with r/politics). (The diagonals don't give us information and it isn't how we want to order the edges)

a <- a[!b,] #This are all existing edges between the important_subreddits-Vertices and we have a column which is a weight called "n".

top_half$negative <- top_half$n*-1

difference_matrix <- add_row(top_half,a)
difference_matrix %>% mutate(asdf=count(difference_matrix[,1],difference_matrix[,2]))
difference_matrix

r <- difference_matrix %>% group_by(difference_matrix[,1],difference_matrix[,2])

difference_matrix%>% group_by(difference_matrix[,1],difference_matrix[,2]) %>% summarise(n=sum(n))
?group_by


edgelist2 <- graph_from_edgelist(as.matrix(a[1:2]),directed = FALSE)
edgelist2



edgelist2 <- graph_from_edgelist(as.matrix(a[1:2]),directed = FALSE) #we construct a graph from the bottom. We need to do this, because rearranging the sequence of edges is hard. And now the edges are ordered alphabetically. This is important for adding further attributes.
# V(edgelist2)

E(edgelist2)$weigth <- a$n #we add the weights, that means the number of edges between each pair of nodes 


top_edgenumbers <- sort(E(edgelist2)$weigth, decreasing = TRUE)[1:(length(E(edgelist2))%/%100*percent_top_edges)] #we want the x percent most multiple edges. Change here the value for having mor edges in the graph.
top_edges <- E(edgelist2)[E(edgelist2)$weigth %in%  top_edgenumbers] #We find a vector of the actual most importnat edges
reduced_weight <- ifelse(E(edgelist2) %in% top_edges, E(edgelist2)$weigth, 0)