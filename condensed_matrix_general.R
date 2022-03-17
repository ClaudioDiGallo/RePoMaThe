# dependencies ------------------------------------------------------------



source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")

subreddit <- "darknetmarkets"
filepath <- filepathgenerator(subreddit)

############################################
# Matrix Pre-Ban ----------------------------------------------------
############################################


# List of Subreddit with num_subscribers ----------------------------------

subscriber_list <- read.csv(filepath[[1]])
subscriber_list <- subscriber_list %>%select(subreddit,subreddit_subscribers) %>% 
  group_by(subreddit) %>% summarise(average_count=mean(subreddit_subscribers))
subscriber_list

# Creating Network --------------------------------------------------------




keepers <- finding_keepers(subreddit,ban="before",min_num_post_on_subreddit = 3)


#Matrix t1 that means before


subreddit_t1 <- matrix_making_subreddits(subreddit,ban = "before", only_keepers = TRUE, keepers = keepers)

#Reduce the matrix to the 1 percent most important subreddits and the 10% most important edges of them

condensed_matrix_subreddits(subreddit_t1,percent_or_num = "num", num_top_subreddits = 70)

condensed_igraph <- igraph::delete_edges(edgelist2, deletion_vector)


#Normalizing the edgeweights

min <- min(E(condensed_igraph)$weigth)
max <- max(E(condensed_igraph)$weigth)
E(condensed_igraph)$norm <- (E(condensed_igraph)$weigth-min)/(max-min)
E(condensed_igraph)$norm



degree <- unlist(V(condensed_igraph)$size)
Kantengewicht <- E(condensed_igraph)$norm



 cluster <-   cluster_louvain(condensed_igraph,E(condensed_igraph)$weigth)
 V(condensed_igraph)$membership <-as.factor(cluster$membership)
V(condensed_igraph)$membership


label_subscriber<- left_join(tibble(subreddit=label_top_subs),subscriber_list, by="subreddit")
label_subscriber

label_subscriber$average_count

label_subscriber$average_count <-  ifelse(label_subscriber$average_count>=1000000,"blue","red")
label_subscriber$average_count

label_subscriber$average_count <- ifelse(is.na(label_subscriber$average_count),"black",label_subscriber$average_count)


label_subscriber

#extracting the direct connections to the central subreddit.


list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",
            darknetmarkets="DarkNetMarkets",me_ira="me_ira",
            gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
subreddit_exact_name <- list[subreddit]


r <- get.edgelist(condensed_igraph)#we make an edgelist
a <- r[,1][r[,1]==subreddit_exact_name | r[,2]==subreddit_exact_name] #we only want edges with the central subredditname
b <- r[,2][r[,1]==subreddit_exact_name | r[,2]==subreddit_exact_name] 
f <- tibble(a,b) #we combined it to an edgelist
f$weigth <- E(condensed_igraph)$weigth[r[,1]==subreddit_exact_name | r[,2]==subreddit_exact_name] #we give the edgelist the weights

f$a <- ifelse(f$a==subreddit_exact_name,NA,f$a)
f$b <- ifelse(f$b==subreddit_exact_name,NA,f$b)

ordering <- base:: order(f$weigth,decreasing=TRUE)
f <- f[ordering,]

label_subscriber$average_count
#if has no labels in the graphics below, pls reload again. 

ggraph(condensed_igraph) + 
  geom_edge_link(   aes( alpha= Kantengewicht)) + 
  geom_node_point(aes(size=degree, color=as.factor(V(condensed_igraph)$membership))) +
  geom_node_text(aes(label=label_top_subs), repel = TRUE, colour=label_subscriber$average_count) +
  ggtitle(paste0("Netzwerk der wichtigsten Subreddits und Verbindungen von r/",subreddit))+
  labs(size = "Degree", alpha="")+
  theme_graph()+
  guides(Kantengewicht="none",
         color="none",
         alpha="none") 



ggsave(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/Netzwerk_vor_bann_num",subreddit,".png"))


#here with all labels

#First the weighting of the labelcolors
label_subscriber<- left_join(tibble(subreddit=V(condensed_igraph)$name),subscriber_list, by="subreddit")
label_subscriber

label_subscriber$average_count <-  ifelse(label_subscriber$average_count>=1000000,"blue","red")

label_subscriber$average_count <- ifelse(is.na(label_subscriber$average_count),"black",label_subscriber$average_count)



#The plotting


ggraph(condensed_igraph) + 
  geom_edge_link(   aes( alpha= Kantengewicht)) + 
  geom_node_point(aes(size=degree, color=as.factor(V(condensed_igraph)$membership))) +
  geom_node_text(aes(label=V(condensed_igraph)$name), repel = TRUE, colour=label_subscriber$average_count) +
  ggtitle(paste0("Alle Labels der wichtigsten Subreddits und Verbindungen von r/",subreddit))+
  labs(size = "Degree", alpha="")+
  theme_graph()+
  guides(Kantengewicht="none",
         color="none",
         alpha="none")

ggsave(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/Netzwerk_vor_bann_ALLLABELS_num",subreddit,".png"))



# creating textual output -------------------------------------------------

#total number accounts. 
tibble_submissions <-  read.csv(filepath[[7]]) %>% select(c("author", "subreddit")) 
tibble_comments <-  read.csv(filepath[[5]]) %>%  select(c("author", "subreddit"))
edgelist <- add_row(tibble_submissions,tibble_comments)


total_accounts_qualified <- left_join(keepers,edgelist, by="author")
tot_num <- total_accounts_qualified %>% select(author) %>% unique() %>% nrow()

not_num <- total_accounts_qualified[total_accounts_qualified$subreddit != subreddit_exact_name,] %>% select(author) %>% unique() %>% nrow() #we filter out all the central_subreddit-rows. if an accounts only posted on the central-subreddit it is not in the list anymore
difference <- tot_num-not_num


textual_output <- paste(list("DAs Netzwerk besteht aus",length(V(condensed_igraph)),"Knoten und aus",length(E(condensed_igraph)),
"Kanten maximum/minimum degreee",max(V(condensed_igraph)$size),min(V(condensed_igraph)$size),
"maximum/minimum Kantengewicht", max(E(condensed_igraph)$weigth),min(E(condensed_igraph)$weigth), "total Accounts/qualified_accounts",
length(total_accounts$author),"/",length(keepers$author), "Die anzahl Accounts nur auf dem Zentralen Subreddit:",difference,not_num))


textual_output <- list(textual_output)
textual_output
write.list(textual_output,paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/condensed_matri_textual_output_num",subreddit,".csv"))
write.csv(f,paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/ordered_list_of_subreddit_overlap",subreddit,".csv"))


# Debugging ---------------------------------------------------------------
# subreddits <- subreddit_t1
# degree_subs <- igraph::degree(subreddits) #degree of all subreddits-vertices. Degree of a subreddit is the sum of all shared users with all other subreddits
# max(degree_subs) DAS STEMMT
# 
# top_subreddits <- length((degree_subs ))%/%100*1#number of the top sub_reddits
# top_subreddits
# 
# 
# top_percent_subs <- sort(degree_subs ,decreasing = TRUE)[1:top_subreddits] #top 5 percent subreddits based on degree
# top_percent_subs
# 
# important_subreddits <- induced_subgraph(subreddits, names(top_percent_subs)) #Igraph with only the x percent most important subreddits-Vertices
# E(important_subreddits)
# 
# 
# 
# tbl <- get.edgelist(important_subreddits)# Wir want here to get a weigth for our number of edges between every pair of nodes
# tbl
# 
# a <- tibble(tbl) %>% count(tbl[,1], tbl[,2]) #this counts all the edges between every possible pair of edges, but also of every edges with itself (which is bad)
# b <- a[1]==a[2] #this is a vector which is true, if a row is not an edge (i.e. the diagonals i.e. r/politics coupled with r/politics). (The diagonals don't give us information and it isn't how we want to order the edges)
# 
# a <- a[!b,] #This are all existing edges between the important_subreddits-Vertices and we have a column which is a weight called "n".
# a
# 
# f <- as.matrix(a[1:2])
# sort(f[1:2],decreasing=FALSE)
# order(f)
# view(f)
# edgelist2 <- graph_from_data_frame(tibble(a[1:2]),directed = FALSE)
# V(edgelist2)
# tibble(a[1:2])
# 
# edgelist2 <- graph_from_edgelist(as.matrix(a[1:2]),directed = FALSE)
# 
# view(V(edgelist2))#we construct a graph from the bottom. We need to do this, because rearranging the sequence of edges is hard. And now the edges are ordered alphabetically. This is important for adding further attributes.
# V(edgelist2)$name <- sort(V(edgelist2)$name,decreasing=FALSE)#sometimes it is ordered alphabetically for 99% and everything fails. So we fix that.
# E(edgelist2) <- sort(E(edgelist2), decreasing=FALSE)
# 
# a$n
# # V(edgelist2)
# 
# ?graph_from_edgelist
# edgelist2
# E(edgelist2)$weigth <- a$n #we add the weights, that means the number of edges between each pair of nodes 
# V(edgelist2)$weigth
# V(edgelist2)
# ORDER()
# 
# top_edgenumbers <- sort(E(edgelist2)$weigth, decreasing = TRUE)[1:(length(E(edgelist2))%/%100*10)] 
# top_edgenumbers#we want the x percent most multiple edges. Change here the value for having more edges in the graph.
# top_edges <- E(edgelist2)[E(edgelist2)$weigth %in%  top_edgenumbers] #We find a vector of the actual most importnat edges
# top_edges
# reduced_weight <- ifelse(E(edgelist2) %in% top_edges, E(edgelist2)$weigth, 0) #we want the actual weights of our most important edges, all other shall be zero, because otherwise every node is connected to every other node. Not much insight to be gained.
# #reduced_weight #This Vector is, that we want only the x% most important edges on our graph
# reduced_weight
# 
# t <- as.data.frame( sort(names(top_percent_subs), decreasing = FALSE))#We want here to label the most important nodes. First we Create anlphabetically ordered df of the names of the most important subs (by degree)
# p <- as.data.frame(top_percent_subs, row.names=NULL) ##here we creat a df which has the right information (names with degree) but in the wrong order (not alphabetically)
# p["names"] <- rownames(p) #the rownames are the sub names, and we need that in a column of its own.
# t
# p[1]
# 
# most_important_degree_subs <- left_join(t, p, by = c("sort(names(top_percent_subs), decreasing = FALSE)" = "names")) #here we add the degree information to the rightly ordered df
# most_important_degree_subs
# 
# r <- as.vector(most_important_degree_subs[2]) #This is the rightly ordered vector of the degrees of the subs. This shall be the size of the nodes.
# V(edgelist2)$size <- unlist(r) #We need to unlist, because this brute-force way is really complicated
# V(edgelist2)$size
# V(edgelist2)$name
# V(edgelist2)[60]
# 
# asdfasdf <- V(edgelist2)$name
# asdfasdf
# 
# 
# label_top_subs <- if_else(V(edgelist2)$name %in% names(sort(degree_subs ,decreasing = TRUE)[1:number_of_top_labeled]), V(edgelist2)$name, "") #We want to only label the top twenty subs by degree (because ortherwise we cannot read anything)
# assign("label_top_subs", label_top_subs, envir =  .GlobalEnv)
# 
# assign("edgelist2", edgelist2, envir =  .GlobalEnv)
# ##############################
# ###
# ####################################
# 
# E(edgelist2)$weigth <-  reduced_weight
# E(edgelist2)$weigth%/%50
# V(edgelist2)$name
# V(edgelist2)$size
# 
# deletion_vector <- E(edgelist2)$weigth == 0 #This gives us a vector which is FALSE for all edges we wanna keep
# 
# for(i in 1:length(deletion_vector)){ #For deleting edges, we need the position number of the edge (the index)
#   #replaces all TRUE with the position in the vector (the index)
#   if(deletion_vector[i] == TRUE){
#     deletion_vector[i] <- i
#     
#   }
#   else{
#     deletion_vector[i] <- NA #The FALSE will be replaced with NA, which are sadly not permitted in the "delete_edges"Function
#   }
#   assign("deletion_vector", deletion_vector) # Maybe I should switch to Python in which this isn't necessary....
# }
# 
# deletion_vector <- deletion_vector[!is.na(deletion_vector)] # delete_edges cannot have NA's
# 
# 
# assign("deletion_vector", deletion_vector, envir =  .GlobalEnv)
# 
# 
