# dependencies ------------------------------------------------------------



source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")

subreddit <- "incelswithouthate"
filepath <- filepathgenerator(subreddit)

############################################
# Matrix Pre-Ban ----------------------------------------------------
############################################

# load data ------------------------------------------

#Vielleicht mÃ¶chte ich hier nochmal GEWICHTET haben

tibble_submissions <-  read.csv(filepath[[5]]) %>% select(c("author", "subreddit")) %>% unique()
tibble_comments <-  read.csv(filepath[[7]]) %>%  select(c("author", "subreddit")) %>% unique()
tibble_all <- add_row(tibble_comments, tibble_submissions) %>% unique()


activity_per_author <- tibble_all %>% count(author)
hist(activity_per_author$n)
max(activity_per_author$n)



# Data Wrangling + cleanup ----------------------------------------------------------
#unique, because I create an unweighted edgelist
edgelist <- as.data.frame(tibble_all %>% select(c("author", "subreddit")) %>% unique()) 
# events <- unlist((unique(edgelist[2])))
# players <- unlist(unique(edgelist[1]))

#Sometimes user have the same name as subreddit, which confuses igraph and disallows creating bipartite networks

 author <- unlist(edgelist$author)
 subreddit <- unlist(edgelist$subreddit)

#first checking, if author names and subreddits are the same, then the author name will be changed

author[which(author %in% subreddit)] <- paste(author[which(author %in% subreddit)],"FALSENAME", sep = "")

edgelist
 edgelist$author <- author
 edgelist$subreddit <- subreddit
edgelist <- graph_from_data_frame(edgelist)


# creating bipartite matrices ---------------------------------------------


V(edgelist)$type <- bipartite_mapping((edgelist))$type
bipartite_matrix <- as_incidence_matrix((edgelist))

#creating networks for users, which share subreddits and vice versa
#there is no information in knowing an author or subreddit engaged with itself, so removing diagonals????

authors <- (bipartite_matrix %*% t(bipartite_matrix))
authors <- graph_from_adjacency_matrix(authors)

subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
subreddits_ellenbogendiagramm <- graph_from_adjacency_matrix(subreddits, mode = "undirected")
subreddits <- graph_from_adjacency_matrix(subreddits) #undirected while more efficient (everything is doubled otherwise), it seems to behave arbitrary with the ordering (i.e. it seems to be alphabetically, but it isn't) For example the Vertices for 4Chan is in unidrected at the end, that means at the beginning of the second round of alphabetically ordered vertices. Probably because the edges are not all mirrored on the same side of the matrix or something.....
#we want to reduce the amount of vertices to the most important ones. Which is determined by degree

# important_subreddits <- induced.subgraph(subreddits, igraph::degree(subreddits) > 345)
# V(important_subreddits)$name



#hist(degree.distribution(subreddits))
#hist(igraph::degree(subreddits))

degree_subs <- igraph::degree(subreddits) #degree of all subreddits-vertices. Degree is the sum of all shared users with all other subreddits

top_subreddits <- length((degree_subs ))%/%100*1#number of the top_reddits



top_percent_subs <- sort(degree_subs ,decreasing = TRUE)[1:top_subreddits] #top 5 percent subreddits based on degree
top_percent_subs

important_subreddits <- induced_subgraph(subreddits, names(top_percent_subs)) #Igraph with only the x percent most important subreddits-Vertices
E(important_subreddits)

# 
# q <- simplify(important_subreddits) #Dies ist equal to E(important_subreddits) %>% unique()
#                                     # was aber nicht geht. Das Ziel ist, die Edges zu reduzieren

tbl <- get.edgelist(important_subreddits)# Wir want here to get a weigth for our number of edges between every pair of nodes
tbl
a <- tibble(tbl) %>% count(tbl[,1], tbl[,2]) #this counts all the edges between every possible pair of edges, but also of every edges with itself (which is bad)
a
b <- a[1]==a[2] #this is a vector which is true, if a row is not an edge (i.e. the diagonals i.e. r/politics coupled with r/politics)

a <- a[!b,] #âº This are all existing edges between the important_subreddits-Vertices and we have a column which is a weight called "n".


# Ellebogendiagramm edges ANFANG -------------------------------------------------


a
sum(a$n)
sort(a$n,decreasing = TRUE)

sort(rrr$n, decreasing = TRUE)
a
a$n
plot(ecdf(a$n))
a[,"n"]

q <- sort(a$n,decreasing = TRUE)
a %>% sort()
?ecdf
i <- 0 
r <- 0
while(r <= 0.8*sum(a$n)){
  i <- i+1
  r <- q[i] +r
  if(r >= 0.8*sum(a$n)){
    print(i)
    
  }
}
a$n[i]
a$n[0+1]
selfmade_plot_data <- 0
selfmade_plot_data[i] <- for(i in 1:length(a$n)){
  if(i == 1){
    a$n[i] <- a$n[i]
  }
  else{
    a$n[i] <- a$n[i]+a$n[i-1]  
  }
  
}

q<-quantile(a$n, .999)
q


x <- rnorm(12)
x
Fn <- ecdf(x)
plot(Fn)     # a *function*
Fn(x)  # returns the percentiles for x



# Ellenbogendiagramm ENDE -------------------------------------------------


edgelist2 <- graph_from_edgelist(as.matrix(a[1:2]),directed = FALSE) #we construct a graph from the bottom. We need to do this, because rearranging the sequence of edges is hard. And now the edges are ordered alphabetically. This is important for adding further attributes.
# V(edgelist2)
E(edgelist2)$weigth[11441]

E(edgelist2)$weigth <- a$n #we add the weights, that means the number of edges between each pair of nodes 
V(edgelist2)



top_edgenumbers <- sort(E(edgelist2)$weigth, decreasing = TRUE)[1:(length(E(edgelist2))%/%100*10)] #we want the x percent most multiple edges. Change here the value for having mor edges in the graph.
E(edgelist2)$weigth %in%  top_edgenumbers #this is a vector of all the edges we want

#sum(E(edgelist2)$weigth %in%  asdf) #there are like 2 too much, because the cutoff point for the nth row has multiple of the same value, which are included here
top_edges <- E(edgelist2)[E(edgelist2)$weigth %in%  top_edgenumbers] #We find a vector of the actual most importnat edges
reduced_weight <- ifelse(E(edgelist2) %in% top_edges, E(edgelist2)$weigth, 0) #we want the actual weights of our most important edges, all other shall be zero, because otherwise every node is connected to every other node. Not much insight to be gained.
reduced_weight #This Vector is, that we want only the 1% most important edges on our graph


#Information lost
informationloss <- 
  
  sort(igraph::degree(authors), decreasing  = TRUE)    
#vertex_attr(subreddits, "label") <- V(subreddits)$name

top_percent_subs
t <- as.data.frame( sort(names(top_percent_subs), decreasing = FALSE))#We want here to label the most important nodes. First we Create anlphabetically ordered df of the names of the most important subs (by degree)
p <- as.data.frame(top_percent_subs, row.names=NULL) ##here we creat a df which has the right information (names with degree) but in the wrong order (not alphabetically)
p["names"] <- rownames(p) #the rownames are the sub names, and we need that in a column of its own.
  
most_important_degree_subs <- left_join(t, p, by = c("sort(names(top_percent_subs), decreasing = FALSE)" = "names")) #here we add the degree information to the rightly ordered df
most_important_degree_subs

r <- as.vector(most_important_degree_subs[2]) #This is the rightly ordered vector of the degrees of the subs. This shall be the size of the nodes.
V(edgelist2)$size <- unlist(r) #We need to unlist, because this brute-force way is really complicated
#V(edgelist2)$name <- unlist(most_important_degree_subs[1]) #the names are also alphabetically ordered
V(edgelist2)

r
# V(important_subreddits)[V(important_subreddits)$name %in% names(sort(degree_subs ,decreasing = TRUE)[1:20])]
# 
# V(important_subreddits)$name %in% names(sort(degree_subs ,decreasing = TRUE)[1:20])
# V(important_subreddits)$name



V(edgelist2)$name
l <- if_else(V(edgelist2)$name %in% names(sort(degree_subs ,decreasing = TRUE)[1:20]), V(edgelist2)$name, "") #We want to only label the top twenty subs by degree (because ortherwise we cannot read anything)
l
# r <- as.numeric(str_extract(top_5_percent_subs, "\\d+")) #We just want the degree (number) of our top subreddits in a vector. Regex ftw. Then convert it to numeric again
# r

##############################
###
####################################

E(edgelist2)$weigth <-  reduced_weight
E(edgelist2)$weigth%/%50
V(edgelist2)$name
V(edgelist2)$size

deletion_vector <- E(edgelist2)$weigth == 0 #This gives us a vector which is FALSE for all edges we wanna keep

for(i in 1:length(deletion_vector)){ #For deleting edges, we need the position number of the edge (the index)
                                    #replaces all TRUE with the position in the vector (the index)
  if(deletion_vector[i] == TRUE){
    deletion_vector[i] <- i
   
  }
  else{
    deletion_vector[i] <- NA #The FALSE will be replaced with NA, which are sadly not permitted in the "delete_edges"Function
  }
  assign("deletion_vector", deletion_vector) # Maybe I should switch to Python in which this isn't necessary....
}

deletion_vector <- deletion_vector[!is.na(deletion_vector)] # delete_edges cannot have NA's

asdf <- igraph::delete_edges(edgelist2, deletion_vector) # For some reason, we cannot replace the edges in our igraph, so we have to create a new one. This seems to work like "create Subgraph". 
 E(asdf)$weigth
E(asdf)
  V(asdf)$size
V(asdf)$name

  ggraph(asdf) +
  geom_edge_link(
    aes(#width = E(edgelist2)$weigth),
      alpha = E(asdf)$weigth)) +
  #E(edgelist2)$weigth)) +
  #scale_edge_width(range = c(0, 20)) +
  #geom_edge_density(aes(fill =reduced_weight)) +
  geom_node_point(aes(size=unlist(V(asdf)$size))) +
  geom_node_text(aes(label=l), repel = TRUE, colour="blue") +
  theme_graph()


# true_edges<- igraph:: delete_edges(edgelist2, deletion_vector)
# 
# E(edgelist2) <- true_edges
# true_edges
# 
# E(edgelist2) <- delete_edges(edgelist2, 1:length(E(edgelist2)))
# 
# as.data.frame(V(edgelist2), E(edgelist2), E(edgelist)$weigth, V(edgelist2)$name)
# 
# ?delete_edges
# 
# filter(edgelist2, E(edgelist2)$weigth == 0)
# 
# deletion_vector





# 
# igraph::delete.edges(edgelist2, c(1,2,3))
# 
# 
# 
# V(asdf)
# V(edgelist2)
# row_number(deletion_vector)
# seq(1, 9, by = 2)
# sum(deletion_vector)
# E(edgelist2)$weigth
# 
# nrow(deletion_vector[2])
# 
# deletion_vector which()
# ?which
# 
# deletion_vector
# 
# ?assign
# deletion_vector
# 
# 
# edgelist2$weigth
# delete.edges(edgelist2)
# 
# 
# 
#  ggraph(edgelist2) +
#      geom_edge_link(
#                   aes(#width = E(edgelist2)$weigth),
#                   alpha = E(edgelist2)$weigth%/%100)) +
#    #E(edgelist2)$weigth)) +
#    #scale_edge_width(range = c(0, 20)) +
#      #geom_edge_density(aes(fill =reduced_weight)) +
#      geom_node_point(aes(size=unlist(V(edgelist2)$size))) +
#      geom_node_text(aes(label=l), repel = TRUE, colour="blue") +
#      theme_graph()
# 
#  
#  unlist(V(edgelist2)$size)
# normalize() 
#  E(edgelist2)$weight
# 
#  
# 
#  # 
# 
# ggraph(q) +
#   geom_edge_link() + 
#   geom_node_point(aes(size=log(f))) +
#   geom_node_text(aes(label=l, repel = TRUE))+
#   theme_graph()
# 
# 
# ggraph(q) +
#   geom_edge_link() + 
#   geom_node_point(aes(size=log(f))) +
#   geom_node_text(aes(label=name[V(important_subreddits)$name %in% names(sort(f,decreasing = TRUE)[1:20])], repel = TRUE))+
#   theme_graph()
# 
# 
# q
# 
# 
# ggraph(q) +
#   geom_edge_link() + 
#   geom_node_point(aes(size=r)) +
#   geom_node_text(aes(label=l), repel = TRUE)+
#   theme_graph()
# 
# 
# V(important_subreddits)$name
# #The same as above,but for accounts -------------------------------------

graphproducingfunction(authors,500)