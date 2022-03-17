# dependencies ------------------------------------------------------------



source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")


#font_import()



subreddit <- "me_ira"
filepath <- filepathgenerator(subreddit)
# 
# library(ggraph)
# library(igraph)
# library(tidygraph)
# library(tidyverse)


# Making Subreddit*Subreddit-Matrix t1 and t2 -----------------------------

#First I cycle all the accounts, which at least posted 3 times on the sub

keepers <- finding_keepers(subreddit,ban="before",min_num_post_on_subreddit = 3)


#Matrix t1 that means before


subreddit_t1 <- matrix_making_subreddits(subreddit,ban = "before", only_keepers = TRUE, keepers = keepers)

subreddit_t2 <- matrix_making_subreddits(subreddit,ban = "after",only_keepers=TRUE,keepers=keepers)


#NO we construct the meta matrix which accounts for the difference bewtween subreddit_t1 and subreddit_t2



change_matrix <- matrix_of_change(before_igraph=subreddit_t1,after_igraph = subreddit_t2)


summary(change_matrix)
summary(edgelist2)
change_matrix


# Plotting graph ----------------------------------------------------------

#the graph needs to be fully connected (only relevant for r/shortcels)

components <- clusters(change_matrix, mode="weak")

change_matrix <- induced_subgraph(change_matrix, V(change_matrix)[components$membership==which.max(components$csize)])



#Summing up the edge weights of the adjacent edges for each vertex. " - Igraph packeg manual to strength()
#For each vertex, all difference in edges will be summed up, which gives us the degree.
V(change_matrix)$degree <- strength(change_matrix, vids = V(change_matrix), mode = c("all", "out", "in", "total"),
                                    loops = TRUE, weights = E(change_matrix)$n)


#normalizing the edgeweights

min <- min(E(change_matrix)$n)
max <- max(E(change_matrix)$n)
E(change_matrix)$norm <- (E(change_matrix)$n-min)/(max-min)

#normalizing the degrees
V(change_matrix)$degree <- V(change_matrix)$degree*-1
V(change_matrix)$degree <- V(change_matrix)$degree+min(V(change_matrix)$degree)
min <- min(V(change_matrix)$degree)
max <- max(V(change_matrix)$degree)
V(change_matrix)$degree <- (V(change_matrix)$degree-min)/(max-min)
V(change_matrix)$degree
V(change_matrix)$degree <- round(V(change_matrix)$degree,2)
(mean(V(change_matrix)$degree))
max
min
#Creating Components
positive_weights <- E(change_matrix)$n-min(E(change_matrix)$n)
cluster_membership <- cluster_louvain(change_matrix,weights = positive_weights)
V(change_matrix)$membership <- cluster_membership$membership

#getting labels



label_flop_subs <- if_else(V(change_matrix)$name %in% names(sort(V(change_matrix)$degree ,decreasing = TRUE)[1:round(length(V(change_matrix)$degree)*0.001,1)]), V(change_matrix)$name, "") #This checks for every name of subreddit, if it is one of the top 1 promille in degrees. if it is this name is taken, if not only "" (that means empty) will be saved.
label_top_subs<- if_else(V(change_matrix)$name %in% names(sort(V(change_matrix)$degree ,decreasing = FALSE)[1:round(length(V(change_matrix)$degree)*0.001,1)]), V(change_matrix)$name, "")

#Size
Veränderung_Degree <- V(change_matrix)$degree
clusters <- as.factor(V(change_matrix)$membership)



#weights = E(change_matrix)$norm
ggraph(change_matrix, layout = "sparse_stress", pivots = 15, weights = 1) + #weights are ignored, but if you give nothing, there will be a nasty error
  geom_edge_link0()+
  geom_node_point(aes(size=Veränderung_Degree,colour= clusters)) +
  #aes(#width = E(edgelist2)$weigth),
  #alpha = E(finished_igraph)$weigth)) +
  #E(edgelist2)$weigth)) +
  #scale_edge_width(range = c(0, 20)) +
  #geom_edge_density(aes(fill =reduced_weight)) +
  #??? geom_node_point(aes(size=1, colour= as.factor(V(change_matrix)$membership))) +
  geom_node_label(aes(label=label_flop_subs), repel = TRUE, colour="blue") +
  geom_node_label(aes(label=label_top_subs), repel = TRUE) +
  ggtitle(paste0("Netzwerk der Veränderung Subredditsaktivitäten nach Bann von r/",subreddit))+
  theme_graph()

ggsave(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/netzwerk_change",subreddit,".png"))


f <- sort(V(change_matrix)$degree, decreasing = TRUE)[1:50]
loser_labels <- V(change_matrix)$name[V(change_matrix)$degree>=f[50]]

f <- sort(V(change_matrix)$degree, decreasing = FALSE)[1:50]
f
winner_labels <- V(change_matrix)$name[V(change_matrix)$degree<=f[50]]
winner_labels
loser_labels

f <- ((V(change_matrix)$membership) )
cluster_count <- tibble(communities=(V(change_matrix)$membership)) %>% count(communities)

textual_output <- paste(list("DAs Netzwerk besteht aus",length(V(change_matrix)),"Knoten und aus",length(E(change_matrix)),"Kanten.
      Die 50 Subreddits,die zugelegt haben sind",winner_labels, "die 50 Subreddits, die abgenommen haben isnd", loser_labels,
      "Die Cluster mit Anzahl der Knoten",cluster_count))

textual_output <- list(textual_output)
textual_output
write.list(textual_output,paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/textual_output_Matrix",subreddit,".csv"))



sum(E(change_matrix)$n>=10)
max(E(change_matrix)$n)
E(change_matrix)[E(change_matrix)$n>=10]

min(E(change_matrix)$n)
V(change_matrix)

E(change_matrix)$n[E(change_matrix)$n<=-100]


# aber hier glaub legacy-code ---------------------------------------------------
# 
# 
# 
# min(E(change_matrix)$norm)
# min(E(change_matrix)$n)
# max(V(change_matrix)$degree)
# min(V(change_matrix)$degree)
# loser_labels
# 
# size=unlist(V(change_matrix)$degree)
# 
# 
# 
# #extracting biggest component
# components <- igraph::clusters(change_matrix, mode="weak")
# components
# components$membership == which.max(components$csize)
# asdf <- induced_subgraph(change_matrix, V(change_matrix)[components$membership==which.max(components$csize)])
# E(asdf)$n
# 
# components$membership==which.max(components$membership)
# components$membership==1
# which.max(components$csize)
# 
# # gsize(change_matrix)
# gsize(edgelist2)
# V(change_matrix)=="me_ira"
# V(edgelist2)
# 
# summary(change_matrix)
# summary(edgelist2)
# 
# ggraph(change_matrix) +
#   geom_edge_link(
#     aes(#width = E(edgelist2)$weigth),
#       alpha = E(change_matrix)$weights)) +
#   #E(edgelist2)$weigth)) +
#   #scale_edge_width(range = c(0, 20)) +
#   #geom_edge_density(aes(fill =reduced_weight)) +
#   geom_node_point(aes(size= 3,colour= unlist(V(change_matrix)$degree))) +
#   scale_colour_gradient2()+
#   #geom_node_text(aes(), repel = TRUE, colour="blue") +
#   theme_graph()
# 
# 
# 
# bb <- ggraph:: layout_with_sparse_stress(change_matrix)
# layout_
# #Thank you very much: https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=11
# ggraph(change_matrix,layout="sparse_stress", pivots=100) +
#   geom_edge_link0(
#     aes(#width = E(edgelist2)$weigth),
#       alpha = E(change_matrix)$weigths)) +
#   #E(edgelist2)$weigth)) +
#   #scale_edge_width(range = c(0, 20)) +
#   #geom_edge_density(aes(fill =reduced_weight)) +
#   geom_node_point(aes(size=unlist(V(change_matrix)$size))) +
#   
#   geom_node_text(aes(label=NA), repel = TRUE, colour="blue") +
#   theme_graph()
# 
# E(change_matrix)$n
# V(finishe)
# 
# 
# 
# #ggraph:: create_layout(change_matrix,layout="sparse")
# ggraph(change_matrix, layout="sparse_stress") +
#   geom_edge_link(
#     aes(#width = E(edgelist2)$weigth),
#       alpha = E(subreddits)$weights)) +
#   #E(edgelist2)$weigth)) +
#   #scale_edge_width(range = c(0, 20)) +
#   #geom_edge_density(aes(fill =reduced_weight)) +
#   geom_node_point(aes(size= 3,colour= unlist(V(subreddits)$degree))) +
#   scale_colour_gradient2()+
#   #geom_node_text(aes(), repel = TRUE, colour="blue") +
#   theme_graph()
# 
# 
# ggraph(subreddit_t2)+
#   geom_edge_link0(aes(E(subreddit_t2)))+
#   geom_node_point(aes(V(subreddit_t2)))+
#   theme_graph()
# 
# ggraph(tbl)+
#   geom_edge_link()+
#   geom_node_point()+
#   theme_graph()
# 
# subreddit_t1
# subreddit_t2
# 
# min(V(change_matrix)$degree)
# 
# normalized_weigth <- normalize(E(change_matrix$weigth), method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
# 
# weights
# E(change_matrix)$weights <- E(change_matrix)$n
# sum(is.na(V(change_matrix)$degree))
# 
# 
# 
# 
# 
# beginning <- Sys.time()
# if(ban=="before"){
#   path_comments <- 7
#   path_submissions <- 5
# } else{
#   path_comments <- 8
#   path_submissions <- 6
# }
# 
# 
# filepath <- filepathgenerator(actual_subreddit)
# 
# 
# # load data ------------------------------------------
# 
# #Vielleicht mÃƒÂ¶chte ich hier nochmal GEWICHTET haben
# sum(is.na(edgelist))
# 
# tibble_submissions <-  read.csv(filepath[[7]]) %>% select(c("author", "subreddit")) %>% unique()
# tibble_comments <-  read.csv(filepath[[5]]) %>%  select(c("author", "subreddit")) %>% unique() #These will be our edges. Account X was active on sub a,b,c etc.
# 
# if(only_keepers==TRUE){
#   edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
#   edgelist <- left_join(keepers, edgelist, by="author")
#   edgelist <- drop_na(edgelist) #because some authors have no activity in after the ban. And they don't have a subreddit, so it's NA and breaks everything.
# } else{
#   edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
# }
# 
# #Sometimes user have the same name as subreddit, which confuses igraph and disallows creating bipartite networks
# 
# author <- unlist(edgelist$author)
# subreddit <- unlist(edgelist$subreddit)
# 
# #first checking, if author names and subreddits are the same, then the author name will be changed
# 
# author[which(author %in% subreddit)] <- paste(author[which(author %in% subreddit)],"FALSENAME", sep = "")
# 
# 
# edgelist$author <- author
# edgelist$subreddit <- subreddit
# edgelist <- graph_from_data_frame(edgelist)
# sum(is.na(edgelist$subreddit))
# 
# edgelist
# 
# # creating bipartite matrices ---------------------------------------------
# 
# 
# V(edgelist)$type <- bipartite_mapping((edgelist))$type
# sum(is.na(bipartite_matrix))
# bipartite_matrix <- as_incidence_matrix((edgelist))
# bipartite_matrix
# 
# #creating networks for subreddits, which share users
# 
# subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
# subreddits
# End <- Sys.time()
# print(End-beginning)
# subreddits <- graph_from_adjacency_matrix(subreddits) #undirected while more efficient (everything is doubled otherwise), it seems to behave arbitrary with the ordering (i.e. it seems to be alphabetically, but it isn't) For example the Vertices for "4Chan" is in undirected at the end, that means at the beginning of the second round of alphabetically ordered vertices. Probably because the edges are not all on the same diagonal-side of the matrix or something.....
# f <- subreddits[1:100]
# f <- graph_from_adjacency_matrix(f)
# 
# ggraph(subreddits[1:100])
# #we want to reduce the amount of vertices to the most important ones. Which is determined by degree
# #
# 
# # only relevant bleow, if mattrix_making_subreddits breaks again ----------
# 
# 
# # #Tracebakc
# # 
# # 
# # tibble_submissions <-  read.csv(filepath[[8]]) %>% select(c("author", "subreddit")) 
# # tibble_comments <-  read.csv(filepath[[6]]) %>%  select(c("author", "subreddit"))
# # edgelist <- add_row(tibble_submissions,tibble_comments)
# # 
# # 
# # 
# # list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",darknetmarkets="DarkNetMarkets",me_ira="me_ira",gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
# # subreddit_exact_name <- list["me_ira"]
# # keepers <- edgelist %>% filter(subreddit == subreddit_exact_name) %>% count(author) %>% filter(n>=3) %>% select(author)
# # 
# # 
# # tibble_submissions <-  read.csv(filepath[[8]]) %>% select(c("author", "subreddit")) %>% unique()
# # tibble_comments <-  read.csv(filepath[[6]]) %>%  select(c("author", "subreddit")) %>% unique() #These will be our edges. Account X was active on sub a,b,c etc.
# # 
# # 
# # 
# # edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
# # edgelist
# # drop.na(edgelist)
# # drop_na(edgelist)
# # 
# # edgelist <- left_join(keepers, edgelist, by="author")
# # 
# # keepers
# # 
# # 
# # #Sometimes user have the same name as subreddit, which confuses igraph and disallows creating bipartite networks
# # 
# # author <- unlist(edgelist$author)
# # subreddit <- unlist(edgelist$subreddit)
# # 
# # edgelist$author <- author
# # edgelist$subreddit <- subreddit
# # edgelist <- graph_from_data_frame(edgelist)
# # 
# # edgelist
# # 
# # # creating bipartite matrices ---------------------------------------------
# # 
# # 
# # V(edgelist)$type <- bipartite_mapping((edgelist))$type
# # bipartite_matrix <- as_incidence_matrix((edgelist))
# # 
# # #creating networks for subreddits, which share users
# # 
# # subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
# # End <- Sys.time()
# # print(End-beginning)
# # subreddits <- graph_from_adjacency_matrix(subreddits) #undirected while more efficient (everything is doubled otherwise), it seems to behave arbitrary with the ordering (i.e. it seems to be alphabetically, but it isn't) For example the Vertices for "4Chan" is in undirected at the end, that means at the beginning of the second round of alphabetically ordered vertices. Probably because the edges are not all on the same diagonal-side of the matrix or something.....
# # #we want to reduce the amount of vertices to the most important ones. Which is determined by degree
# # #
# 
# 
# 
# # mini igraph -------------------------------------------------------------
# beginning <- Sys.time()
# if(ban=="before"){
#   path_comments <- 7
#   path_submissions <- 5
# } else{
#   path_comments <- 8
#   path_submissions <- 6
# }
# 
# 
# filepath <- filepathgenerator(actual_subreddit)
# 
# 
# # load data ------------------------------------------
# 
# #Vielleicht mÃƒÂ¶chte ich hier nochmal GEWICHTET haben
# 
# 
# tibble_submissions <-  read.csv(filepath[[8]]) %>% select(c("author", "subreddit")) %>% unique()
# tibble_comments <-  read.csv(filepath[[6]]) %>%  select(c("author", "subreddit")) %>% unique() #These will be our edges. Account X was active on sub a,b,c etc.
# sum(is.na(tibble_comments))
# 
# tibble_submissions <- tibble_submissions[1:100,]
# tibble_comments <- tibble_comments[1:100,]
# 
# if(only_keepers==TRUE){
#   edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
#   edgelist <- left_join(keepers, edgelist, by="author")
#   edgelist <- drop_na(edgelist) #because some authors have no activity in after the ban. And they don't have a subreddit, so it's NA and breaks everything.
# } else{
#   edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
# }
# 
# #Sometimes user have the same name as subreddit, which confuses igraph and disallows creating bipartite networks
# 
# author <- unlist(edgelist$author)
# subreddit <- unlist(edgelist$subreddit)
# 
# #first checking, if author names and subreddits are the same, then the author name will be changed
# 
# author[which(author %in% subreddit)] <- paste(author[which(author %in% subreddit)],"FALSENAME", sep = "")
# 
# 
# edgelist$author <- author
# edgelist$subreddit <- subreddit
# edgelist <- graph_from_data_frame(edgelist)
# 
# 
# # creating bipartite matrices ---------------------------------------------
# 
# 
# V(edgelist)$type <- bipartite_mapping((edgelist))$type
# bipartite_matrix <- as_incidence_matrix((edgelist))
# 
# #creating networks for subreddits, which share users
# 
# subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
# End <- Sys.time()
# print(End-beginning)
# subreddits <- graph_from_adjacency_matrix(subreddits) #undirected while more efficient (everything is doubled otherwise), it seems to behave arbitrary with the ordering (i.e. it seems to be alphabetically, but it isn't) For example the Vertices for "4Chan" is in undirected at the end, that means at the beginning of the second round of alphabetically ordered vertices. Probably because the edges are not all on the same diagonal-side of the matrix or something.....
# #we want to reduce the amount of vertices to the most important ones. Which is determined by degree
# #
# #  subreddits_ellenbogendiagramm <- graph_from_adjacency_matrix(subreddits, mode = "undirected") #This might be important later, if we want to calculate information-loss
# #For each vertex, all difference in edges will be summed up, which gives us the degree.
# V(subreddits)$degree <- strength(subreddits, vids = V(subreddits), mode = c("all", "out", "in", "total"),
#                                  loops = TRUE, weights = E(subreddits)$n)
# 
# gsize(change_matrix)
# gsize(edgelist2)
# V(change_matrix)=="me_ira"
# V(edgelist2)
# 
# summary(change_matrix)
# summary(edgelist2)
# 
# l <- ggraph:: create_layout(subreddits,layout="sparse")
# ggraph(subreddits) +
#   geom_edge_link(
#     aes(#width = E(edgelist2)$weigth),
#       alpha = E(subreddits)$weights)) +
#   #E(edgelist2)$weigth)) +
#   #scale_edge_width(range = c(0, 20)) +
#   #geom_edge_density(aes(fill =reduced_weight)) +
#   geom_node_point(aes(size= 3,colour= unlist(V(subreddits)$degree))) +
#   scale_colour_gradient2()+
#   #geom_node_text(aes(), repel = TRUE, colour="blue") +
#   theme_graph()
# 
# s <- as_edgelist(subreddits)
# components(subreddits)
# sum(component_distribution(subreddits))
# subreddits
# 
# as.matrix.network.adjacency(E(subreddits))
