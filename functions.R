
# General Filpath Structure -----------------------------------------------

#############################################################################################
reddit_data_filepath <- "E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/"###############
######## Write here your own path for reddit_data, this above is only an example#############
#############################################################################################


#############################################################################################
masterarbeit_code_filepath <- "C:/Users/Claudio/Desktop/Masterarbeit Code/"##################
######## Write here your own path for Masterarbeit Code, this above is only an example#######
#############################################################################################





# Graphproducing Function -------------------------------------------------


#we want to reduce the amount of vertices to the most important ones. Which is determined by degree

graphproducingfunction <- function(graph,cutoffvalue){
  important_graph <- induced.subgraph(graph, igraph::degree(graph) > cutoffvalue)
  
  

  

  
  #vertex_attr(subreddits, "label") <- V(subreddits)$name
  
  
  ggraph(important_graph) +
    geom_edge_link() + 
    geom_node_point(aes(size=igraph::degree(important_graph))) +
    geom_node_text(aes(label=name), repel = TRUE)+
    theme_graph()
}





# Filepathgenerator -------------------------------------------------------

filepathgenerator <- function(subreddit){
  filepath  <- list(
    paste0(reddit_data_filepath,subreddit,"selected_authorsSUBMISSIONSBEFORE_BAN.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsSUBMISSIONSAFTER_BAN.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsCOMMENTSBEFORE_BAN.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsCOMMENTSAFTER_BAN.csv"),
    
    paste0(reddit_data_filepath,subreddit,"selected_authorSUBMISSIONSBEFORE_BAN_SANITIZED.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsSUBMISSIONSAFTER_BAN_SANITIZED.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsCOMMENTSBEFORE_BAN_SANITIZED.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsCOMMENTSAFTER_BAN_SANITIZED.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsSUBMISSIONSBEFORE_BANCONTROL.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsSUBMISSIONSAFTER_BANCONTROL.csv"),
    paste0(reddit_data_filepath,subreddit,"COMMENT.csv"),
    paste0(reddit_data_filepath,subreddit,"SUBMISSION.csv"),
    paste0(reddit_data_filepath,subreddit,"BANISHMENT.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsSUBMISSIONSBEFORE_BANCONTROL_SANITIZED.csv"),
    paste0(reddit_data_filepath,subreddit,"selected_authorsSUBMISSIONSAFTER_BANCONTROL_SANITIZED.csv"))
  
  
  
}



#Author filtering by mind number of activities

filter_author <- function(subreddit=subreddit,min_number=3,tibble=tbl_comb){
list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",darknetmarkets="DarkNetMarkets",me_ira="me_ira",gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
subreddit_exact_name <- list[subreddit]
keepers <- tibble %>% filter(subreddit == subreddit_exact_name) %>% count(author) %>% filter(n>=min_number) %>% select(author)
tibble <- left_join(keepers, tibble, by="author")}


# bad_accounts appendfunction ---------------------------------------------


#This function appends a list with one or more bad account strings to my bad account file, while always generating a unique backup
#This is important for me, because while manually looking for bots i can write them down here, as to not remove them again in the next data-set.
#the botlist is also used in the python script for omitting those users in scraping data (sometimes they make up for 70% of scraped data)
#because it is used in the other script it is important, that I don't change anything.


bad_accounts_append <- function(list){
  


bad_accounts <- read.csv(paste0(masterarbeit_code_filepath,"botlist.csv"))
write_csv(bad_accounts,
          paste0(paste0(masterarbeit_code_filepath,"backup/botlist"),
                 str_remove_all(
                   as.character(Sys.time()),
                   "\\s|-|:"),".csv"))#this is for generating a backupfile, because losing the botlist would be some of the worst that could happen to me, because I change a lot of it incrementally and manually
                                       #The Systime makes that i don't overwrite my backup                                                                                                                           

bad_accounts<- append(list,bad_accounts$Botnames) %>% 
  flatten_chr() %>% 
  list() %>% 
  as.data.frame.list(header=FALSE)
names(bad_accounts)[1] <- "Botnames"
write_csv(bad_accounts,paste0(masterarbeit_code_filepath,"botlist.csv"))}

# spammers detection function ---------------------------------------------


bad_accounts_detector_posts <- function(tibble_all, samplesize = 20,max_var=30){
    
q <- tibble_all %>% filter(title != "") %>%  count(author) %>%  filter(n>=50) #bei submissions anzahl wichtiger weil gleicher post auf mehreren subs. nchar dabei nicht so wichtig
s <- tibble_all %>% filter(author %in% q$author)  %>% summarise(groups=author,title) %>% mutate(nchar=nchar(title)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
a <- s %>% filter(nchar_var <= max_var & nchar_var >0) #%>% print(mapply( sample(tibble_all$body), 10))
watchlist <- list(s %>% filter(nchar_var <= max_var) %>% select(groups))
names(watchlist) <- "watchlist"
assign("watchlist_posts",watchlist, envir =  .GlobalEnv)  


if(length(watchlist_posts$watchlist) == 0){
  print("Offenbar no suspicious accounts")
  
}   else{
  for( i in 1:length(a$groups)){
    print(paste("Sample of",samplesize,"Posts of Account", a$groups[i]))
    print(sample( tibble_all$title[tibble_all$author == a$groups[i]], samplesize))#body is the content of the comment
    
  }
  
}

}



#From all titles which are not missing. count the number of remaining submissions by author if is higher than 50, we want to check this author.
#50 is more or less an arbitrary number. The reasoning behind it is, that a lot of titles are the same, because the account submits it to a variety of subs.
#so this number needs to be higher than the reasonable number of related subs where an account could submit it at the same time.
#The priority lies on not excluding humans and less on detecting every last bot. Also: the lower the total amount of the post the less problematic is a false negative.

#If something is detected, it creates a "watchlist" and from all the elements of this list, which have a variance at all (=are not all the same titles)
#there will be printed out a sample of the titles, so that I can check if this really looks like a bot. So the idea behind this is not, that I have a perfect way of finding bots
#but that I created a helper which makes that I just have to identify manually way less accounts.


# Spammers Detection function for comments --------------------------------

bad_accounts_detector_comments <- function(tibble_all, samplesize = 15,max_var = 10, min_comments=20){
  post_per_author <- tibble_all %>% count(author)
  s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
  post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))
  watchlist <- post_per_author %>% filter(post_per_author$nchar_var<= max_var & post_per_author$n>=min_comments) %>% select(author) %>% list()
  
  names(watchlist) <- "watchlist"
  assign("watchlist_comments",watchlist,envir =  .GlobalEnv)
  
  if(length(watchlist_comments$watchlist$author) == 0){
    print("Offenbar no suspicious accounts")
  }
  else{
    for( i in 1:length(watchlist_comments$watchlist$author)){
      print(paste("Sample of",samplesize,"Comments of Account", watchlist_comments$watchlist$author[i]))
      print(sample( tibble_all$body[tibble_all$author == watchlist_comments$watchlist$author[i]], samplesize))#body is the content of the comment
      
    }
    
  }
  
}

# First it calculates how many comments an author has written. Then it calculates the variance of the lenght of comments of everry author. Then this is combined.
# If an author has less than min_comments comments AND has between all his or her comments a variance less than max_var it is added to the watchlist.
# From all the accounts in the watchlist, a sample of n = samplesize is drawn, so that a human can have a look.

##legacy code below

# bad_accounts_detector_comments <- function(tibble_all, samplesize = 15,max_var = 10){
#   q <- tibble_all %>% mutate(nchar=nchar(body)) %>% filter(body != "[removed]" |body != "") %>% filter(nchar >= 15) %>%  count(author) %>%  filter(n>=5) #hier anzahl nicht so wichtig, weil nur ein kommentar pro post, dafür länge des kommentars wichitger, weil jeamnd vielleicht einfach 4 mal "no" oder "yes", "indeed" oder eine meme-antwort gibt.
#   s <- tibble_all %>% filter(author %in% q$author)  %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
#   a <- s %>% filter(nchar_var <= max_var & nchar_var >=0) #%>% print(mapply( sample(tibble_all$body), 10))
#   watchlist <- list(s %>% filter(nchar_var <= max_var) %>% select(groups))
#   names(watchlist) <- "watchlist"
#   assign("watchlist_comments",watchlist,envir =  .GlobalEnv)
# 
#   if(length(watchlist_comments) == 0){
#     print("Offenbar no suspicious accounts")
#   }
#   else{
#     for( i in 1:length(a$groups)){
#       print(paste("Sample of",samplesize,"Comments of Account", a$groups[i]))
#       print(sample( tibble_all$body[tibble_all$author == a$groups[i]], samplesize))#body is the content of the comment
# 
#     }
# 
#   }
# 
# }


# Finding Accounts with >=3 Post on subreddit -----------------------------
#With this function, we find the accounts which at least have posted 3 times on the subreddit

# finding_keepers <- function(actual_subreddit,ban ="before",min_num_post_on_subreddit = 0){
# 
#   if(ban=="before"){
#     path_comments <- 7
#     path_submissions <- 5
#   } else{
#     path_comments <- 8
#     path_submissions <- 6
#   }
#   
#   
#   filepath <- filepathgenerator(actual_subreddit)
#   
# 
#   
#   tibble_submissions <-  read.csv(filepath[[path_submissions]]) %>% select(c("author", "subreddit")) 
#   tibble_comments <-  read.csv(filepath[[path_comments]]) %>%  select(c("author", "subreddit"))
#   edgelist <- add_row(tibble_submissions,tibble_comments)
#   
#   
#   
#   list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",darknetmarkets="DarkNetMarkets",me_ira="me_ira",gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
#   subreddit_exact_name <- list[actual_subreddit]
#   keepers <- edgelist %>% filter(subreddit == subreddit_exact_name) %>% count(author) %>% filter(n>=min_num_post_on_subreddit) %>% select(author)
# }



# experiment finding keepers bleow ----------------------------------------



finding_keepers <- function(actual_subreddit,ban ="before",min_num_post_on_subreddit = 0){
  
  if(ban=="before"){
    path_comments <- 7
    path_submissions <- 5
  } else{
    path_comments <- 8
    path_submissions <- 6
  }
  
  if(ban=="experiment_control"){#this code is twice,because the second part came in much later in the process. It needs to be backwards compatible to older scripts.
    path_comments <- 14
    path_submissions <- 15
  } 
  
  
  if(ban=="experiment_treatment"){#this code is twice,because the second part came in much later in the process. It needs to be backwards compatible to older scripts.
    path_comments <- 6
    path_submissions <- 5
  } 
  
  filepath <- filepathgenerator(actual_subreddit)
  
  
  
  tibble_submissions <-  read.csv(filepath[[path_submissions]]) %>% select(c("author", "subreddit")) 
  tibble_comments <-  read.csv(filepath[[path_comments]]) %>%  select(c("author", "subreddit"))
  edgelist <- add_row(tibble_submissions,tibble_comments)
  
  
  
  list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",darknetmarkets="DarkNetMarkets",me_ira="me_ira",gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
  subreddit_exact_name <- list[actual_subreddit]
  keepers <- edgelist %>% filter(subreddit == subreddit_exact_name) %>% count(author) %>% filter(n>=min_num_post_on_subreddit) %>% select(author)
}
# Matrix-making -----------------------------------------------------------
#This function takes in the subreddit-name and if it is before or after the ban
#Then it loads the sanitized relevant data and turns it into a birpatite network. It also works if different types of verttices havbe the same name.
#for example an account has the same name as a subreddit. 
#Then it creates a author*author matrix out of the bipartite matrix, this is very slow (around 2 min on my ryzen 3600x, also obiiously depends on size of data (not linearly)) and puts out an igraph

#Also to use it, you don't need to assign it to anything, it copies itself in the environment. So if you something in there called "deletion_vector", this will be deleted. As well as "edglelist2".

matrix_making_authors <- function(actual_subreddit, ban="before"){
  
  beginning <- Sys.time()
  if(ban=="before"){
    path_comments <- 7
    path_submissions <- 5
  } else{
    path_comments <- 8
    path_submissions <- 6
  }
  
  filepath <- filepathgenerator(actual_subreddit)
  
  "inncelswithouthateselected_authorsSUBMISSIONSAFTER_BAN_SANITIZED.csv" == "incelswithouthateselected_authorsSUBMISSIONSAFTER_BAN_SANITIZED.csv"
  filepath[[6]]
  
  # load data ------------------------------------------
  
  #Vielleicht mÃ¶chte ich hier nochmal GEWICHTET haben
  
  tibble_submissions <-  read.csv(filepath[[path_submissions]]) %>% select(c("author", "subreddit")) %>% unique()
  tibble_comments <-  read.csv(filepath[[path_comments]]) %>%  select(c("author", "subreddit")) %>% unique() #These will be our edges. Account X was active on sub a,b,c etc.
  edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
  
  
  
  
  
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
  End <- Sys.time()
  print(End-beginning)
  authors <- graph_from_adjacency_matrix(authors)
  # 
  # subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
  # subreddits_ellenbogendiagramm <- graph_from_adjacency_matrix(subreddits, mode = "undirected")
  # subreddits <- graph_from_adjacency_matrix(subreddits) #undirected while more efficient (everything is doubled otherwise), it seems to behave arbitrary with the ordering (i.e. it seems to be alphabetically, but it isn't) For example the Vertices for "4Chan" is in undirected at the end, that means at the beginning of the second round of alphabetically ordered vertices. Probably because the edges are not all on the same diagonal-side of the matrix or something.....
  #we want to reduce the amount of vertices to the most important ones. Which is determined by degree
  
  
}

################ The same matrix making for the conversion of the bipartite network in a subreddit*subreddit network
#This function takes in the subreddit-name and if it is before or after the ban
#Then it loads the sanitized relevant data and turns it into a birpatite network. It also works if different types of verttices havbe the same name.
#for example an account has the same name as a subreddit. 
#Then it creates a subreddit*subreddit matrix out of the bipartite matrix, this is very slow (around 5 min on my ryzen 3600x, also obiiously depends on size of data (not linearly)) and in between it can need a gb of RAM for the matrix (the output igraph is way more efficient)
#???and puts out an igraph
#Also to use it, you don't need to assign it to anything, it copies itself in the environment. So if you something in there called "deletion_vector", this will be deleted. As well as "edglelist2".


matrix_making_subreddits <- function(actual_subreddit, ban = "before",only_keepers=TRUE, keepers=0){
  beginning <- Sys.time()
  if(ban=="before"){
    path_comments <- 7
    path_submissions <- 5
  } else{
    path_comments <- 8
    path_submissions <- 6
  }
  
  
  filepath <- filepathgenerator(actual_subreddit)
  
  
  # load data ------------------------------------------
  
  #Vielleicht mÃ¶chte ich hier nochmal GEWICHTET haben
  

  tibble_submissions <-  read.csv(filepath[[path_submissions]]) %>% select(c("author", "subreddit")) %>% unique()
  tibble_comments <-  read.csv(filepath[[path_comments]]) %>%  select(c("author", "subreddit")) %>% unique() #These will be our edges. Account X was active on sub a,b,c etc.
  
  if(only_keepers==TRUE){
    edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
    edgelist <- left_join(keepers, edgelist, by="author")
    edgelist <- drop_na(edgelist) #because some authors have no activity in after the ban. And they don't have a subreddit, so it's NA and breaks everything.
  } else{
    edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
  }
  
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
  
  #creating networks for subreddits, which share users

  subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
  End <- Sys.time()
  print(End-beginning)
  subreddits <- graph_from_adjacency_matrix(subreddits) #undirected while more efficient (everything is doubled otherwise), it seems to behave arbitrary with the ordering (i.e. it seems to be alphabetically, but it isn't) For example the Vertices for "4Chan" is in undirected at the end, that means at the beginning of the second round of alphabetically ordered vertices. Probably because the edges are not all on the same diagonal-side of the matrix or something.....
  #we want to reduce the amount of vertices to the most important ones. Which is determined by degree
  #
  #  subreddits_ellenbogendiagramm <- graph_from_adjacency_matrix(subreddits, mode = "undirected") #This might be important later, if we want to calculate information-loss

}


# Condensing Matrix -------------------------------------------------------

condensed_matrix_subreddits <- function(subreddits, percent_top_subreddits = 1,num_top_subreddits = 50, percent_or_num = "percent",percent_top_edges = 10, number_of_top_labeled = 20){
  
  degree_subs <- igraph::degree(subreddits) #degree of all subreddits-vertices. Degree of a subreddit is the sum of all shared users with all other subreddits
  
  if(percent_or_num=="percent"){
    top_subreddits <- length((degree_subs ))%/%100*percent_top_subreddits}#number of the top sub_reddits
  else{
    top_subreddits <- num_top_subreddits
  }
  
  
  top_percent_subs <- sort(degree_subs ,decreasing = TRUE)[1:top_subreddits] #top 5 percent subreddits based on degree
  
  
  important_subreddits <- induced_subgraph(subreddits, names(top_percent_subs)) #Igraph with only the x percent most important subreddits-Vertices
  
  
  
  
  tbl <- get.edgelist(important_subreddits)# Wir want here to get a weigth for our number of edges between every pair of nodes
  
  
  a <- tibble(tbl) %>% count(tbl[,1], tbl[,2]) #this counts all the edges between every possible pair of edges, but also of every edges with itself (which is bad)
  b <- a[1]==a[2] #this is a vector which is true, if a row is not an edge (i.e. the diagonals i.e. r/politics coupled with r/politics). (The diagonals don't give us information and it isn't how we want to order the edges)
  
  a <- a[!b,] #This are all existing edges between the important_subreddits-Vertices and we have a column which is a weight called "n".
  
  
  
  
  # edgelist2 <- graph_from_edgelist(as.matrix(a[1:2]),directed = FALSE) #we construct a graph from the bottom. We need to do this, because rearranging the sequence of edges is hard. And now the edges are ordered alphabetically. This is important for adding further attributes.
  # V(edgelist2)$name <- sort(V(edgelist2)$name,decreasing=FALSE)#sometimes it is ordered alphabetically for 99% and everything fails. So we fix that.
  # 
  assign("edgetibble", tibble(a[1:2]), envir =  .GlobalEnv)
  edgelist2 <- graph_from_data_frame(tibble(a[1:2]),directed = FALSE)
  
  E(edgelist2)$weigth <- a$n #we add the weights, that means the number of edges between each pair of nodes 
  
  
  top_edgenumbers <- sort(E(edgelist2)$weigth, decreasing = TRUE)[1:(length(E(edgelist2))%/%100*percent_top_edges)] #we want the x percent most multiple edges. Change here the value for having more edges in the graph.
  top_edges <- E(edgelist2)[E(edgelist2)$weigth %in%  top_edgenumbers] #We find a vector of the actual most importnat edges
  reduced_weight <- ifelse(E(edgelist2) %in% top_edges, E(edgelist2)$weigth, 0) #we want the actual weights of our most important edges, all other shall be zero, because otherwise every node is connected to every other node. Not much insight to be gained.
  #reduced_weight #This Vector is, that we want only the x% most important edges on our graph
  
  
  t <- as.data.frame( sort(names(top_percent_subs), decreasing = FALSE))#We want here to label the most important nodes. First we Create anlphabetically ordered df of the names of the most important subs (by degree)
  p <- as.data.frame(top_percent_subs, row.names=NULL) ##here we creat a df which has the right information (names with degree) but in the wrong order (not alphabetically)
  p["names"] <- rownames(p) #the rownames are the sub names, and we need that in a column of its own.
  
  most_important_degree_subs <- left_join(t, p, by = c("sort(names(top_percent_subs), decreasing = FALSE)" = "names")) #here we add the degree information to the rightly ordered df
  
  
  r <- as.vector(most_important_degree_subs[2]) #This is the rightly ordered vector of the degrees of the subs. This shall be the size of the nodes.
  V(edgelist2)$size <- unlist(r) #We need to unlist, because this brute-force way is really complicated
  
  
  
  label_top_subs <- if_else(V(edgelist2)$name %in% names(sort(degree_subs ,decreasing = TRUE)[1:number_of_top_labeled]), V(edgelist2)$name, "") #We want to only label the top twenty subs by degree (because ortherwise we cannot read anything)
  assign("label_top_subs", label_top_subs, envir =  .GlobalEnv)
  
  assign("edgelist2", edgelist2, envir =  .GlobalEnv)
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
  
  
  assign("deletion_vector", deletion_vector, envir =  .GlobalEnv)
  
}


#This is the same as the bad_account_detector for submissions. Except, there is a minimum from characters a comment needs to have. This is more important as
#an account with only 5 Comments which always read "no" or "yes" or a meme-answer has a high chance of being a human. The total number of rows per author is lower
#than with the submissions, because it is a bad solution and because there doesn't exist an analog to submitting to multiple subreddits, it shouldn't be a big problem.




# difference_filemaker ----------------------------------------------------

difference_filemaker <- function(subreddit=subreddit,author_activity=author_activity,author_frequence=author_frequence){
  
  before <- author_activity %>% filter(before==TRUE) 
    after <- author_activity %>% filter(before==FALSE)
  combined <- left_join(before,after,by="author")
  
  w <- ifelse(is.na(combined$activity_before_after_ban.y),0,combined$activity_before_after_ban.y)
  combined$activity_before_after_ban.y    <- w
  
  combined <- combined %>% mutate(account=author,activity_before=activity_before_after_ban.x,activity_after=activity_before_after_ban.y)
  combined <- combined[6:8]
  combined <- mutate(combined,expected_loss=expected_loss)
  combined["subredditname"] <- subreddit
  combined
  
  weighted_difference2 <- author_frequence  %>% select(author,n,before) %>% 
    group_by(author,before) %>%
    summarise(activity_before_after_ban= sum(n)) %>% 
    mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
    summarise(difference=sum(for_subtraction), total=sum(activity_before_after_ban)) %>% 
    mutate(weighted_difference=difference/total)
  weighted_difference2 <- weighted_difference2 %>% select(author,weighted_difference)
  combined <- left_join(combined,weighted_difference2,by=c("account"="author"))
  combined
  
  
  write_csv2(combined,paste0(masterarbeit_code_filepath,"activity_difference",subreddit,".csv"))

  

  
}


# Matrix of Change --------------------------------------------------------

#Differences of networks

#Also wir wollen hier unterschiede von zwei netzwerken zeigen. DAs heisst, Netzwerk1 zu t1 unterschied zu netzwerk2 zu t2. Wir wollen die Matrizen minus-rechnen.
#DAs problem ist, dass nicht alle spalten und reihen sind bei beiden vorhanden. Wenn also matrix1 - matrix2 gerechnet wird, werden nur am Anfang die richtigen minus-gerechnet.
#LÃ¶sung: Brechstange: Die Netzwerke werden aufgemacht, es wird eine edgelist gebildet, die mit Gewichtung verdichtet wird.
#Die Gewichtungen der Edges von N2 wird minus die gewichteten Edges von N1 gerechnet. Das ist die VerÃ¤nderung der Anzahl Edges zwischen allen relevanten Knotenpaaren.
#FÃ¼r jeden Punkt werden dann alle zugehÃ¶rigen Edge-Gewichtungen summiert. Weil die Edges auch negative Gewichtungen haben kÃ¶nnen, kann damit auch gezeigt werden,
#Welche PUnkte an degree eingebÃ¼sst haben.


matrix_of_change <- function(before_igraph=subreddit_t1,after_igraph=subreddit_t2){
  begin <- Sys.time()
  tbl <- get.edgelist(subreddit_t1)# Wir want here to get a weigth for our number of edges between every pair of nodes

  a <- tibble(tbl) %>% count(tbl[,1], tbl[,2]) #this counts all the edges between every possible pair of edges, but also of every edges with itself (which is bad)
  b <- a[1]==a[2] #this is a vector which is true, if a row is not an edge (i.e. the diagonals i.e. r/politics coupled with r/politics). (The diagonals don't give us information and it isn't how we want to order the edges)

  a <- a[!b,] #Getting rid of the diagonals
  top_half <- a #This are the all the realised edges and how many times this edges was present in the graph.
  top_half$n <- top_half$n*-1 #Because we want to have a difference, we have to make the weight negative in the first network

  
  
  tbl <- get.edgelist(subreddit_t2)# The same as above, but with the second Network
  #tbl <- tibble(tbl)
 # tbl <- add_row(tbl,tbl) this is porobably very wrong
  
  a <- tibble(tbl) %>% count(tbl[,1], tbl[,2])
  b <- a[1]==a[2] 
  a <- a[!b,] 
  
 

  difference_matrix <- add_row(top_half,a) #We combine the to weigthed edgelists
  
  
  #The next line is more complicated: First we group the combined weighted edgelist into all unique realized edge-combinations and sum all the weighted-Values.
  #That means, if we started with 2 Edges between Node 1 and 2 and in the second network we have 1 Edge between Node 1 and 2.
  #We calculate 1 Edge minus 2 Edges. This gives us the change of number of edges between Node 1 and 2, which is -1.
  #And we to this for all realised Node-Pairs.
  #Note: if an edge between Node 1 and 2 ist not realized in network 1 but in network 2, the edgeweight is for network 1 is zero and the change is the amount of edgweight of network 2.
  
  difference_matrix <- difference_matrix%>% group_by(difference_matrix[,1],difference_matrix[,2]) %>% summarise(n=sum(n))
  
  
  
  edgelist2 <- graph_from_edgelist(as.matrix(difference_matrix[1:2]),directed = FALSE) #We construct a graph wich has from all realised Edges of both networks
  E(edgelist2)$n <- difference_matrix$n #We add the edge difference to the edges.
  end <- Sys.time()
  print(end-begin)
  edgelist2
  
  
}

data_count <- function(subreddit){
  
  filepath <- filepathgenerator(subreddit=subreddit)
  
  
  #loading Data
  
  raw_sub_before <-  read.csv(filepath[[1]]) %>% select(created_utc) %>% nrow()
  raw_sub_after <-  read.csv(filepath[[2]]) %>% select(created_utc) %>% nrow()
  raw_comments_before <-  read.csv(filepath[[3]]) %>% select(created_utc) %>% nrow()
  raw_comments_after <-  read.csv(filepath[[4]]) %>% select(created_utc) %>% nrow()
  sub_comment<-  read.csv(filepath[[11]]) %>% select(created_utc) %>% nrow()
  sub_submission <-  read.csv(filepath[[12]]) %>% select(created_utc) %>% nrow()
  banishment <-  read.csv(filepath[[13]]) %>% select(created_utc) %>% nrow()
  
  
  total <- raw_sub_before+raw_sub_after+raw_comments_after+raw_comments_before+sub_comment+sub_submission+banishment
  
  
  
  
}






# for(i in (1:length(n$n))){
#   f <- tibble_all$title[tibble_all$author == n$author[i]]
#   q <- f[1:2]
#   
#   if(sum(f[1:length(f)] %in% q) == length(f)){
#     watchlist <- append(n$author[i],watchlist)
#   }
# }


#Below: Slithly more complex Legacy Code. Unfortunately it finds a lot of humans as well :(
# 
# for(i in (1:length(n$n))){
#   f <- tibble_all$title[tibble_all$author == n$author[i]]
#   q <- f[1:20]
#   
#   if(sum(f[21:length(f)] %in% q) > 5){
#     watchlist <- append(n$author[i],watchlist)
#   }
# }



# for(i in (1:length(n$n))){
#   
#   
# 
#     watchlist[i] <- var(nchar(tibble_all$body[tibble_all$author == n$author[i]]))
#   
# }
# 
# median(watchlist)
# min(watchlist)
# 
# n$author[watchlist == 0]
# 
# watchlist <- ""
# 
# 
# #Das Fonktioniert ned, philospherneku esch en mönsch, sundaydiscovery esch vermuetli en art sekte. En Huufe lüüt uploaded ei post of 10 verschednigi subs. Lueg a moxyaproved, de dude hed en oyutube channel woner am promote esch. Fuck. resp.
# 
# tibble_all$title[tibble_all$author == watchlist]
# 
# var(nchar(tibble_all$body[tibble_all$author == "RemindMeBot"]))
# var(nchar(tibble_all$body[tibble_all$author == "-CHAD__THUNDERCOCK-"]))
# tibble_all$body[tibble_all$author == "bettereveryday1985"]
# 
# 
# tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted)) 

# 
# f <- tibble_all$title[tibble_all$author ==  n$author[33]]
# q <- f[1:20]
# if(sum(f[21:length(f)] %in%  q) > 5){
#   watchlist[1] <- n$author[33]}
# 
# 
# 
# 
# for( i in (1:length(n$n))){
#   f <- tibble_all$title[tibble_all$author == n$author[i]]
#   q <- f[1:20]
#   print(q)
# if(sum(tibble_all$title[tibble_all$author == n$author[i]][21:length(tibble_all$title[tibble_all$author == n$author[i]])] %in%  q > 5))
#       {watchlist[i] <- n$author[i]}
#     
# }



# 
# sum(tibble_all$title[tibble_all$author == n$author[1]][21:length(tibble_all$title[tibble_all$author == n$author[1]])])
# 
# 
# 
# f <- tibble_all$title[tibble_all$author == (post_per_author$author[post_per_author$n ==max(post_per_author$n)])]
# 
# 
# 
# 
# n$author[3]
# sum(f[21:length(f)] %in%  q) > 5
# 
# q <- f[1:20]
# 
#    sum(f[21:length(f)] %in%  q)
# 
# 
#    
#    
# f
# 
# f[20:length(f)]

# 
# to_be_deleted <- post_per_author$author[post_per_author$n == 100000]
# tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted)) #if row i is not in forbidden list, put it in sanitized tibble 
# 
# post_per_author <- rows_delete(post_per_author,tibble(author = to_be_deleted))


# not relevant for master thesis, but for me :) Pickachu-Companion --------

companion <- function(){

  a <- sample(1:11,1)
if(a==10){
  print("Hello, I like you")}
if(a==9){
    print("Feed me more data!")}
if(a==8){
  print("When I grow up, I wanne be a Neural Network. Could you help me with it?")
}
  
if(a==7){
    print("Don't be too hard on yourself. You are doing your best! You are getting more efficaous with every step!")}
  
if(a==6){
    print("Am I doing good?")}
if(a==5){
  print("Find Solace in the Brushstroke")}
if(a==4){
print("You can now pet_pikachu()")  
pet_pikachu <- function(){
  print("PIKA!!")
}}
  if(a==3){
    print("I'm sending out an S-O-S to the world \n
          I hope that someone gets my
          I hope that someone gets my 
          
          Message in a bottle")}
  if(a==2){
    print("I don't believe what I saw,
          a hundred billion messages at the shore")}
 
if(a==1){
  print("Pika, THUNBERBOLT!!!!")}
  if(a==11){
    print("what do you expect? a shitty motivational quote? GET BACK TO WORK, pika-pika! >:)")}
}  
 companion()
# 
# rock_paper <- function(your_choice){
#   a <- sample(1:3,1)
#   list <- list("rock","paper","scissors")
#   if(list[a])
# } 
#  

# Graveyard ---------------------------------------------------------------

#This matrix has a minimum of number of posts of a account on the subreddit to consider built in. But it fails on after,because there is no subreddit anymore
 
 # 
 # matrix_making_subreddits <- function(actual_subreddit, ban = "before",min_num_post_on_subreddit = 0){
 #   beginning <- Sys.time()
 #   if(ban=="before"){
 #     path_comments <- 7
 #     path_submissions <- 5
 #   } else{
 #     path_comments <- 8
 #     path_submissions <- 6
 #   }
 #   
 #   
 #   filepath <- filepathgenerator(actual_subreddit)
 #   
 #   
 #   # load data ------------------------------------------
 #   
 #   #Vielleicht mÃ¶chte ich hier nochmal GEWICHTET haben
 #   
 #   tibble_submissions <-  read.csv(filepath[[path_submissions]]) %>% select(c("author", "subreddit")) 
 #   tibble_comments <-  read.csv(filepath[[path_comments]]) %>%  select(c("author", "subreddit"))
 #   edgelist <- add_row(tibble_submissions,tibble_comments)
 #   
 #   
 #   
 #   list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",darknetmarkets="DarkNetMarkets",me_ira="me_ira",gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
 #   subreddit_exact_name <- list[actual_subreddit]
 #   keepers <- edgelist %>% filter(subreddit == subreddit_exact_name) %>% count(author) %>% filter(n>=min_num_post_on_subreddit) %>% select(author)
 #   
 #   
 #   tibble_submissions <-  read.csv(filepath[[path_submissions]]) %>% select(c("author", "subreddit")) %>% unique()
 #   tibble_comments <-  read.csv(filepath[[path_comments]]) %>%  select(c("author", "subreddit")) %>% unique() #These will be our edges. Account X was active on sub a,b,c etc.
 #   
 #   
 #   
 #   edgelist <- as.data.frame(add_row(tibble_comments, tibble_submissions) %>% unique())
 #   edgelist <- left_join(keepers, edgelist, by="author")
 #   
 #   
 #   
 #   
 #   #Sometimes user have the same name as subreddit, which confuses igraph and disallows creating bipartite networks
 #   
 #   author <- unlist(edgelist$author)
 #   subreddit <- unlist(edgelist$subreddit)
 #   
 #   #first checking, if author names and subreddits are the same, then the author name will be changed
 #   
 #   author[which(author %in% subreddit)] <- paste(author[which(author %in% subreddit)],"FALSENAME", sep = "")
 #   
 #   
 #   edgelist$author <- author
 #   edgelist$subreddit <- subreddit
 #   edgelist <- graph_from_data_frame(edgelist)
 #   
 #   
 #   # creating bipartite matrices ---------------------------------------------
 #   
 #   
 #   V(edgelist)$type <- bipartite_mapping((edgelist))$type
 #   bipartite_matrix <- as_incidence_matrix((edgelist))
 #   
 #   #creating networks for subreddits, which share users
 #   
 #   subreddits <- (t(bipartite_matrix) %*% bipartite_matrix)
 #   End <- Sys.time()
 #   print(End-beginning)
 #   subreddits <- graph_from_adjacency_matrix(subreddits) #undirected while more efficient (everything is doubled otherwise), it seems to behave arbitrary with the ordering (i.e. it seems to be alphabetically, but it isn't) For example the Vertices for "4Chan" is in undirected at the end, that means at the beginning of the second round of alphabetically ordered vertices. Probably because the edges are not all on the same diagonal-side of the matrix or something.....
 #   #we want to reduce the amount of vertices to the most important ones. Which is determined by degree
 #   #
 #   #  subreddits_ellenbogendiagramm <- graph_from_adjacency_matrix(subreddits, mode = "undirected") #This might be important later, if we want to calculate information-loss
 #   
 # }

