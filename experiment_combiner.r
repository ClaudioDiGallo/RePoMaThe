
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")


############################################
# expermient combiner  ----------------------------------------------------
############################################

subreddits <- list("gendercritical","me_ira","braincels","shortcels")

for(subreddit in subreddits){
  # loading data and keeping the accounts with activit > 1 ----------------------------------
  name <- subreddit
  filepath <- filepathgenerator(subreddit)
  tbl_before <-  read.csv(filepath[[5]]) %>% select(c("author","created_utc")) %>% mutate(before=TRUE,treatment=TRUE, sub=name)
  tbl_after <-  read.csv(filepath[[6]]) %>% select(c("author","created_utc")) %>% mutate(before=FALSE,treatment=TRUE, sub=name) 
  tbl_comb <- add_row(tbl_before,tbl_after)
  
  
  keepers <- finding_keepers(name,ban="experiment_treatment",min_num_post_on_subreddit = 2)
  tbl_treatment <- left_join(tbl_comb,keepers,by="author")
  
  

  #creating difference per author
  
  
  author_frequence <- tbl_treatment %>% group_by(before,author) %>% count()
  
  
  author_difference <- author_frequence  %>% select(author,n,before) %>% 
    group_by(author,before) %>%
    summarise(activity_before_after_ban= sum(n)) %>% 
    mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
    summarise(difference=sum(for_subtraction))
  tbl_comb2 <- left_join(author_difference,tbl_treatment,by="author")
  tbl_comb3 <- tbl_comb2 %>% select(-created_utc,-before) %>% unique()
  
  #loading data control-group
  
  tbl_before <-  read.csv(filepath[[14]]) %>% select(c("author","created_utc")) %>% mutate(before=TRUE,treatment=FALSE, sub=name)
  tbl_after <-  read.csv(filepath[[15]]) %>% select(c("author","created_utc")) %>% mutate(before=FALSE,treatment=FALSE, sub=name) 
  tbl_comb <- add_row(tbl_before,tbl_after)
  
  keepers <- finding_keepers(name,ban="experiment_control",min_num_post_on_subreddit = 2)
  tbl_control <- left_join(tbl_comb,keepers,by="author")
  

  
  
  
  #creating difference per author
  
  
  
  author_frequence <- tbl_control%>% group_by(before,author) %>% count()
  author_frequence
  
  author_difference <- author_frequence  %>% select(author,n,before) %>% 
    group_by(author,before) %>%
    summarise(activity_before_after_ban= sum(n)) %>% 
    mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
    summarise(difference=sum(for_subtraction))
  tbl_comb2 <- left_join(author_difference,tbl_control,by="author")
  tbl_comb4 <- tbl_comb2 %>% select(-created_utc,-before) %>% unique()
  tbl_comb <- add_row(tbl_comb4,tbl_comb3)
  
  assign(paste0(subreddit,"_combined"),tbl_comb, envir =  .GlobalEnv) 
  

# activity-difference-per-account -----------------------------------------

    
  
}



hyper_combiner <- add_row(braincels_combined,shortcels_combined)
ultra_combiner <- add_row(gendercritical_combined,me_ira_combined)
ultra_hyper_combiner <- add_row(hyper_combiner,ultra_combiner)




write_csv2(ultra_hyper_combiner,"C:/Users/Claudio/Desktop/Masterarbeit Code/experiment_combined.csv")

# 
# # later_tryhard seciton ---------------------------------------------------
# 
# name <- "braincels"
# filepath <- filepathgenerator(name)
# tbl_before <-  read.csv(filepath[[5]]) %>% select(c("author","created_utc")) %>% mutate(before=TRUE,treatment=TRUE, sub=name)
# tbl_after <-  read.csv(filepath[[6]]) %>% select(c("author","created_utc")) %>% mutate(before=FALSE,treatment=TRUE, sub=name) 
# tbl_comb <- add_row(tbl_before,tbl_after)
# 
# 
# keepers <- finding_keepers(name,ban="experiment_treatment",min_num_post_on_subreddit = 2)
# tbl_treatment <- left_join(tbl_comb,keepers,by="author")
# 
# 
# filepath[[5]]
# filepath[[7]]
# #creating difference per author
# 
# # tbl_comb <- add_row(tbl_control,tbl_treatment)
# 
# 
# author_frequence <- tbl_treatment %>% group_by(before,author) %>% count()
# 
# 
# author_difference <- author_frequence  %>% select(author,n,before) %>% 
#   group_by(author,before) %>%
#   summarise(activity_before_after_ban= sum(n)) %>% 
#   mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
#   summarise(difference=sum(for_subtraction))
# tbl_comb2 <- left_join(author_difference,tbl_treatment,by="author")
# tbl_comb3 <- tbl_comb2 %>% select(-created_utc,-before) %>% unique()
# 
# #loading data control-group
# 
# tbl_before <-  read.csv(filepath[[14]]) %>% select(c("author","created_utc")) %>% mutate(before=TRUE,treatment=FALSE, sub=name)
# tbl_after <-  read.csv(filepath[[15]]) %>% select(c("author","created_utc")) %>% mutate(before=FALSE,treatment=FALSE, sub=name) 
# tbl_comb <- add_row(tbl_before,tbl_after)
# 
# keepers <- finding_keepers(name,ban="experiment_control",min_num_post_on_subreddit = 2)
# tbl_control <- left_join(tbl_comb,keepers,by="author")
# 
# tbl_control
# 
# 
# 
# #creating difference per author
# 
# 
# 
# author_frequence <- tbl_control%>% group_by(before,author) %>% count()
# author_frequence
# 
# author_difference <- author_frequence  %>% select(author,n,before) %>% 
#   group_by(author,before) %>%
#   summarise(activity_before_after_ban= sum(n)) %>% 
#   mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
#   summarise(difference=sum(for_subtraction))
# tbl_comb2 <- left_join(author_difference,tbl_control,by="author")
# tbl_comb4 <- tbl_comb2 %>% select(-created_utc,-before) %>% unique()
# 
# filepath[[1]]
# 
# sub <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/braincelsSUBMISSION_CONTROLGROUP.csv")
# sub %>% select(author) %>% unique() %>% nrow()
# tbl_before <- read.csv(filepath[[9]])
# tbl_before %>% select(author) %>% unique() %>% nrow()
# 
# # try-hard-section --------------------------------------------------------
# 
# name <- "braincels"
# filepath <- filepathgenerator(name)
# tbl_before <-  read.csv(filepath[[5]]) %>% select(c("author","created_utc")) %>% mutate(before=TRUE,treatment=TRUE, sub=name) %>% unique()
# tbl_after <-  read.csv(filepath[[6]]) %>% select(c("author","created_utc")) %>% mutate(before=FALSE,treatment=TRUE, sub=name) %>% unique()
# tbl_comb <- add_row(tbl_before,tbl_after)
# 
# 
# 
# tbl_comb
# keepers <- finding_keepers(name,ban="experiment_treatment",min_num_post_on_subreddit = 2)
# tbl_treatment <- left_join(tbl_comb,keepers,by="author")
# tbl_treatment
#                  
# 
# 
# tbl_treatment %>% group_by(before,author,treatment)
# 
# 
# author_frequence <- tbl_treatment %>% group_by(before,author,treatment) %>% count()
# author_frequence
# 
# author_difference <- author_frequence  %>% select(author,n,before,treatment) %>% 
#   group_by(author,before,treatment) %>%
#   summarise(activity_before_after_ban= sum(n)) %>% 
#   mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
#   summarise(treatment=treatment,difference=sum(for_subtraction))
# 
# 
# 
# 
# tbl_comb1 <- left_join(author_difference,tbl_comb,by=c("author","treatment"))
# tbl_comb2 <- tbl_comb1 %>% select(before,author,treatment,subreddit)
# 
# 
# 
# # experiment vorher -------------------------------------------------------
# 
# for(subreddit in subreddits){
#   # loading data and keeping the accounts with activit > 1 ----------------------------------
#   name <- subreddit
#   filepath <- filepathgenerator(subreddit)
#   tbl_before <-  read.csv(filepath[[5]]) %>% select(c("author", "created_utc")) %>% mutate(before=TRUE,treatment=TRUE, sub=name) %>% unique()
#   tbl_after <-  read.csv(filepath[[6]]) %>% select(c("author", "created_utc")) %>% mutate(before=FALSE,treatment=TRUE, sub=name) %>% unique()
#   tbl_comb <- add_row(tbl_before,tbl_after)
#   
#   keepers <- tbl_comb %>% count(author) %>% filter(n>=2) 
#   tbl_treatment <- left_join(keepers,tbl_comb,by="author")
#   
#   #loading data control-group
#   
#   tbl_before <-  read.csv(filepath[[14]]) %>% select(c("author","created_utc")) %>% mutate(before=TRUE,treatment=FALSE, sub=name) %>% unique()
#   tbl_after <-  read.csv(filepath[[15]]) %>% select(c("author","created_utc")) %>% mutate(before=FALSE,treatment=FALSE, sub=name) %>% unique()
#   tbl_comb <- add_row(tbl_before,tbl_after)
#   
#   keepers <- tbl_comb %>% count(author) %>% filter(n>=2) 
#   tbl_control <- left_join(keepers,tbl_comb,by="author")
#   
#   #creating difference per author
#   
#   tbl_comb <- add_row(tbl_control,tbl_treatment)
#   
#   
#   author_frequence <- tbl_comb %>% group_by(before,author) %>% count()
#   
#   
#   author_difference <- author_frequence  %>% select(author,n,before) %>% 
#     group_by(author,before) %>%
#     summarise(activity_before_after_ban= sum(n)) %>% 
#     mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
#     summarise(difference=sum(for_subtraction))
#   tbl_comb <- left_join(author_difference,tbl_comb,by="author")
#   
#   
#   assign(paste0(subreddit,"_combined"),tbl_comb, envir =  .GlobalEnv) 
#                       