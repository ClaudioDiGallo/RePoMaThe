

# dependencies ------------------------------------------------------------


library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(statnet)

source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")

# Source Functions and load data ------------------------------------------

tibble_all <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/incelswithouthateselected_authorsCOMMENTSBEFORE_BAN_3_UNCOMPLETE.csv")

# Data Wrangling + cleanup ----------------------------------------------------------
#unique, because I create an unweighted edgelist
edgelist <- as.data.frame(tibble_all %>% select(c("author", "subreddit")) %>% unique()) 
events <- unlist((unique(edgelist[2])))
players <- unlist(unique(edgelist[1]))

#Sometimes user have the same name as subreddit, which confuses igraph and disallows creating bipartite networks

author <- unlist(edgelist$author)
subreddit <- unlist(edgelist$subreddit)

#first checking, if author names and subreddits are the same, then the author name will be changed

author[which(author %in% subreddit)] <- paste(author[which(author %in% subreddit)],"FALSENAME", sep = "")

edgelist$author <- author
edgelist$subreddit <- subreddit
edgelist <- graph_from_data_frame(edgelist)


# creating bipartite matrices ---------------------------------------------


V(edgelist)$type <- bipartite_mapping((edgelist))$type
bipartite_matrix <- as_incidence_matrix((edgelist))

#creating networks for users, which share subreddits and vice versa
#there is no information in knowing an author or subreddit engaged with itself, so removing diagonals????

authors <- (bipartite_matrix %*% t(bipartite_matrix))
subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
subreddits <- graph_from_adjacency_matrix(subreddits)
authors <- graph_from_adjacency_matrix(authors)


#we want to reduce the amount of vertices to the most important ones. Which is determined by degree

important_subreddits <- induced.subgraph(subreddits, igraph::degree(subreddits) > 345)
V(important_subreddits)$name

#hist(degree.distribution(subreddits))
#hist(igraph::degree(subreddits))

f <- igraph::degree(subreddits)
twentypercent <- length((f))/100*20
f[twentypercent]

  sort(f,decreasing = TRUE)[1:twentypercent]
  
  filter(sort(f,decreasing = TRUE),f >=345) 
filter
  
  #Information lost
  informationloss <- 
    
sort(igraph::degree(authors), decreasin = TRUE)    
#vertex_attr(subreddits, "label") <- V(subreddits)$name


ggraph(important_subreddits) +
  geom_edge_link() + 
  geom_node_point(aes(size=igraph::degree(important_subreddits))) +
  geom_node_text(aes(label=name), repel = TRUE)+
  theme_graph()


V(important_subreddits)$name
# #The same as above,but for accounts -------------------------------------

graphproducingfunction(authors,500)



# trash -------------------------------------------------------------------


max(subreddits)

unique(edgelist[2])

name

for( i in players) {
  if( i %in% events){
    print(i)
    continue
  }
  
}
for( i in players) {
  if( i %in% events){
    print(i)
    continue
  }
  
}
a <- 0
for( i in f){
  if(i > 345){
    
    a <- a+1
  }
}
a
f <- matrix(1:9, nrow = 3, ncol = 3)
t(f)%*%f


author[author=="IncelWolf_eeee"]
author=="Incelwolf_"

edgelist

paste(h[which(h %in% g)],"eeee")


is.contained(events, players)
g <-  c("C", "D", "E")

h <- c("A", "E", "B", "C", "D", "E", "A", "B", "C", "D", "E")

class(g)
class(events)
which(h %in% g)


edgelist[550:575,1:2]
create_bipartite(8,4)

?create_bipartite

players[events]

# %>% graph_from_data_frame(directed = FALSE)
as_incidence_matrix(two_mode)

q <- as.edgelist(as.network(two_mode, matrix.type = "edgelist", directed = TRUE,  hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE))
q
plot(q)
two_mode
bipartite.mapping(two_mode)
two_mode <- graph_from_data_frame(two_mode)
two_mode <- bipartite.mapping(two_mode)$type

as.network(two_mode,directed=FALSE, multiple=TRUE, edgelist=TRUE)



ggraph(two_mode) + geom_edge_link() + geom_node_point() + theme_graph()




plot(two_mode)
make_bipartite_graph(two_mode)
f <- graph.from

?

f <- bipartite_graph(two_mode)
autograph(f)



two_mode <- as_incidence_matrix(two_mode)
autograph(two_mode)
?as_incidence_matrix
?make_bipartite_graph
g <- make_bipartite_graph( c(0,1,0,1,0,0), c(1,2,2,3,3,4) )
as_incidence_matrix(g)

igraph <- graph_from_data_frame(d = , vertices = nodes, directed = TRUE)


plot(f)

bip = network(two_mode,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

tbl_graph(two_mode)
?create_bipartite
create_bipartite(two_mode[1],two_mode[2])

class(subreddits)

induced.sub
f(i in 1:length(subreddits)) {
  if(subreddits[i,i] <5){
    important_subreddits <- subreddits[]
    
  }
}
?degree

subreddits
f <- as_edgelist(subreddits)


f <- cbind(c(1,0,1,1),c(1,0,1,0),c(0,0,1,0),c(0,1,0,1))
a <- t(f)%*%(f)
a <- graph_from_adjacency_matrix(a)
igraph::degree(a)
