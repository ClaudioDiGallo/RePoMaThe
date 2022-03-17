#Basic Statstical Differences and Plots


source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")






# -------------------------------------------------------------------------
############### Significance-tests


#######################################################################
subreddit <- "braincels" #######Change here the name of the subreddit you want to work #########
#######################################################################
filepath <- filepathgenerator(subreddit=subreddit)

banishment <- read_csv(paste0("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/",subreddit,"BANISHMENT.csv"))
banishment <- banishment$created_utc %>% max()








# Constructing and combining Before and After Tibbles -----------------------------------

tibble_submissions <-  read_csv(filepath[[5]]) %>% 
  select(author,created_utc,subreddit,subreddit_subscribers, whitelist_status
         #,removed_by_category
         ,date,day, selftext) %>% 
  mutate(Post=TRUE,body=NA,  nchar = nchar(selftext, allowNA = TRUE))


tibble_comments <- read_csv(filepath[[7]]) %>%
  select(author,created_utc,subreddit, date,day,body) %>%
  mutate(selftext= NA,Post=FALSE,subreddit_subscribers=NA,whitelist_status=NA
         #,removed_by_category =NA
         ,nchar = nchar(body, allowNA = TRUE))


tbl_before <- add_row(tibble_submissions,tibble_comments)


tibble_submissions <-  read_csv(filepath[[6]]) %>% 
  select(author,created_utc,subreddit,subreddit_subscribers, whitelist_status
         #,removed_by_category
         ,date,day, selftext) %>% 
  mutate(Post=TRUE,body=NA,  nchar = nchar(selftext, allowNA = TRUE))


tibble_comments <- read_csv(filepath[[8]]) %>%
  select(author,created_utc,subreddit, date,day,body) %>%
  mutate(selftext= NA,Post=FALSE,subreddit_subscribers=NA,whitelist_status=NA
         #,removed_by_category =NA
         ,nchar = nchar(body, allowNA = TRUE))


tbl_after <- add_row(tibble_submissions,tibble_comments)
tbl_comb <- add_row(tbl_before,tbl_after)

#keeping only accounts with at least 3 comments on subreddit
n_author <- tbl_comb$author %>% unique()


list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",darknetmarkets="DarkNetMarkets",me_ira="me_ira",gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
subreddit_exact_name <- list[subreddit]
keepers <- tbl_comb %>% filter(subreddit == subreddit_exact_name) %>% count(author) %>% filter(n>=3) %>% select(author)
tbl_comb <- left_join(keepers, tbl_comb, by="author")



#getting author data and standard deviations

author_frequence <- tbl_comb  %>% mutate(before= if_else(created_utc<banishment,TRUE,FALSE)) 
author_frequence <- author_frequence %>% group_by(author,before) %>% count()
all_authors <- author_frequence %>% ungroup() %>%  select(author) %>% unique()

author_frequence_before <- author_frequence %>% filter(before==TRUE) 
before_sd <- sd(author_frequence_before$n)
before_mad <- mad(author_frequence_before$n)

author_frequence_after <- author_frequence %>% filter(before==FALSE) 
author_frequence_after <- right_join(author_frequence_after, all_authors,by="author") #this part is, because we don't want to lose the inactive accounts. They are important too
author_frequence_after$n <- ifelse(is.na(author_frequence_after$n),0,author_frequence_after$n)

author_frequence_after$n

after_sd <- sd(author_frequence_after$n)
after_mad <- mad(author_frequence_after$n)




# Creating the histograms -------------------------------------------------

plot1 <- ggplot()+
  geom_histogram(binwidth = 10, aes(y=author_frequence_before$n, fill =1))+
  geom_histogram(binwidth = 10, aes(y=author_frequence_after$n,fill =5,alpha=0.1))+
  ggtitle( paste0("Veränderung der Aktivität der Accounts von r/",subreddit))+
  xlab( "Anzahl Accounts")+
  ylab("Veränderung der Account-Aktivität")+
  geom_hline(yintercept = mean(author_frequence_before$n), 
             color = "red", size=1, alpha=1, linetype="dotted")+
  geom_hline(yintercept = mean(author_frequence_after$n), 
             color = "green", size=1, alpha=1,linetype="dotted")+
#:  geom_errorbar(aes(ymin=(mean(author_frequence_after$n)-after_mad), ymax=(mean(author_frequence_after$n)+after_mad), width=.2))+
  geom_errorbar(aes(x=150,y=mean(author_frequence_after$n),ymin=mean(author_frequence_before$n)-1.96*before_mad, ymax=mean(author_frequence_before$n)+1.96*before_mad, width=.2))+
  # geom_hline(yintercept= 0-2,71*mad(author_difference$difference), color="pink")+
  coord_flip()+
  theme(text= element_text(size = 20))
plot1

mean(author_frequence_after$n)



#making relative change per author
# 
# relative_change <- left_join(author_frequence_after,author_frequence_before, by="author")
# relative_change <- relative_change %>% mutate(quotient= n.x/n.y)
# mean <- mean(relative_change$quotient)
# sd <- sd(relative_change$quotient)
# mean
# sd
# hist(relative_change$quotient, breaks=1000)
# ?hist
# median(relative_change$quotient)
