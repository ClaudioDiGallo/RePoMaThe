
# homophly long exposure --------------------------------------------------

source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


tibble <- read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/INCEL_ACCOUNTSselected_authorsSUBMISSIONSLONGEXPOSURE_SANITIZED.csv")


anytime(min(tibble$created_utc))
anytime(max(tibble$created_utc))
#we tracked them for 19.5 months

#creating time units
episode <- (max(tibble$created_utc)-min(tibble$created_utc))/19


tibble <- tibble %>% select(created_utc,author,subreddit) %>% mutate(t0=created_utc-min(tibble$created_utc))%>% mutate(timeframe=t0%/%episode)

max(tibble$timeframe)


# total amount of unique author subreddit combinations per timeframe

tibble %>% select(author, subreddit,timeframe) %>%  group_by(author,timeframe) %>% ungroup() %>% count(timeframe) #these are the unique author subreddit combinations per timeframe
#total authors per timeframe
tot_auth <- tibble %>% select(author,timeframe) %>% unique() %>% count(timeframe)
tot_auth

# Crazy DPLyer Cohesion Finder awesomeness

#Also. Ziel ist es , herausezufinden, wie sich die Wahrscheinlichkeit verhält, jemand oder mehrere meiner Accounts auf Reddit auf einem Subreddit zu treffen.
#Also wir verdichten das tibble zu unique author-subreddit-timeframe kombinationen. Dann zählen wir die Subreddits und counten sie gruppiert nach timeframes.
#Dann werden die Zahlen mit lapply mal (n*(n-1))/2 gerechnet zum die Edges zwischen den Accounts zu bekommen. Das normalisiere ich dann durch die Anzahl Accounts.
#WEnn zwei Accounts auf zwei gleichen Subreddits sind, dann bekomme ich aber 2 Edges zwischen ihnen und nciht nur eine. Tja...





#Jetzt wollen wir das noch gewichtet haben.


zwischen_save <- tibble %>% select(author,subreddit,timeframe) %>% group_by(author,subreddit,timeframe) %>%ungroup() %>%
  unique() %>% 
  select(subreddit,timeframe) %>% group_by(timeframe) %>% 
  count(subreddit) #recht spannend
zwischen_save$timeframe
tot_auth <- tot_auth %>% mutate(inverse=1/n)
zwischen_norm <- left_join(zwischen_save,tot_auth,by="timeframe") %>% mutate(n_norm=n.x/n.y)
zwischen_norm

zwischen_save %>% ungroup()

edges_per_subreddit <- zwischen_norm %>% ungroup()%>%group_by(timeframe) 



# calculating baseline homphily -------------------------------------------
#we want: num of accounts per episode. num of different subreddits per account per episode. 

baseline_homo <- tibble %>%select(author,subreddit,timeframe) %>%  group_by(author,subreddit,timeframe) %>% unique()
tot_auth #num accounts per episode
baseline_homo <- baseline_homo %>% group_by(author, timeframe) %>% count(author) #this is per account the number of subreddits for every episode
baseline_homo <- baseline_homo %>%ungroup() %>% group_by(timeframe) %>%  summarise(sub_per_acc=mean(n))

homophily <- edges_per_subreddit[edges_per_subreddit$inverse==edges_per_subreddit$n_norm,]
homophily <- homophily %>% group_by(timeframe) %>% summarise(n=sum(n_norm))
homophily <- left_join(baseline_homo,homophily, by="timeframe")
homophily <- homophily %>% mutate(meetings_per_num_subreddit=n/sub_per_acc)
homophily

# Interesting Plots: ------------------------------------------------------

#Plot 1, total number of Active Accounts vs. banishment times

list <- list("braincels","shortcels","incelswithouthate")
banishment <-  add_column(c(0,0,0)) 
 for(i in 1:length(list)){
  #print(list[i])
  banishment[i] <- read_csv(paste0("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/",list[i],"BANISHMENT.csv")) %>% select(created_utc) %>% max()
  #banishment[i] <- banishment$created_utc %>% max()
  
}

banishment_episode <- (banishment-min(tibble$created_utc))%/%episode
sd <- sd(tot_auth$n)

plot1<- ggplot(tot_auth,aes(x=timeframe,y=n)) + 
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(ymin=n-sd, ymax=n+sd), width=.2) +
  ggtitle("Anzahl Aktive Incel-Accounts über der Zeit")+
  xlab("Monate nach Sampling")+
  ylab("Anzahl aktive Accounts")+
  geom_vline(xintercept=banishment_episode, color = "blue")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme(text=element_text(size=24))

plot1
ggsave(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/long_exposure_incels.png"))




#top subreddits
top <- sort(zwischen_norm$n_norm,decreasing=TRUE)
top <- top[1:30]


f <- zwischen_norm$n_norm %in% top
top_nnorm <- zwischen_norm[f,1:6] 

write.csv(top_nnorm,"C:/Users/Claudio/Desktop/Masterarbeit Code/images/long_exposure_incels.csv")




#third stuff

edges_per_subreddit

write.csv(edges_per_subreddit,"C:/Users/Claudio/Desktop/Masterarbeit Code/images/long_exposure_incels_total.csv")
sd <- sd(edges_per_subreddit$total_homophily)

plot1<- ggplot(homophily,aes(x=timeframe,y=meetings_per_num_subreddit)) + 
  geom_point() +
  geom_line() +
   #geom_errorbar(aes(ymin=total_homophily-sd, ymax=total_homophily+sd), width=.2) +
  ggtitle("Wahrscheinlichkeit einer Begegnung auf einem Subreddit")+
  xlab("Monate nach Sampling")+
  ylab("Wahrscheinlichkeit")+
  geom_vline(xintercept=banishment_episode, color = "blue")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme(text=element_text(size=20))

plot1
ggsave(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/long_exposure_incels_homophily_error_bars.png"))




meetings <- function(n){
  solution <- (n*(n-1))/2
  return(solution)
}

tbl <- left_join(zwischen_save,tot_auth,by="timeframe")
meetings_min <- zwischen_save %>% ungroup()%>% select(n) %>% sapply(meetings)
max_meetings <- tbl %>% ungroup()%>% select(n.y) %>% sapply(meetings)

tbl <- add_column(tbl,meetings_min) 
tbl <- add_column(tbl,max_meetings) 
tbl <- tbl %>% mutate(homophily=meetings_min/max_meetings)
tbl <- tbl %>% group_by(timeframe) %>% summarise(total_homophily=sum(homophily))
tbl



plot1<- ggplot(tbl,aes(x=timeframe,y=total_homophily)) + 
  geom_point() +
  geom_line() +
  #geom_errorbar(aes(ymin=total_homophily-sd, ymax=total_homophily+sd), width=.2) +
  ggtitle("Normalisierte Begegnungen in Subreddits")+
  xlab("Monate nach Sampling")+
  ylab("Normalisierte Begnungen")+
  geom_vline(xintercept=banishment_episode, color = "blue")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme(text=element_text(size=20))

plot1
ggsave(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/long_exposure_incels_homophily_bebegnungen.png"))



# trash -------------------------------------------------------------------

# 
# 
# zwischen_save <- tibble %>% select(author,subreddit,timeframe) %>% group_by(author,subreddit,timeframe) %>%ungroup() %>%
#   unique() %>%
#   select(subreddit,timeframe) %>% group_by(timeframe) %>%
#   count(subreddit) #recht spannend
# zwischen_save
# 
# 
# zwischen_save <- add_column(zwischen_save,edges_per_subreddit2)
# 
# top <- sort(zwischen_save$edges_per_subreddit2,decreasing=TRUE)
# top <- top[1:30]
# f <- zwischen_save$edges_per_subreddit2 %in% top
# f
#  zwischen_save[f,] 
# 
# 
# 
# edges_per_subreddit <- edges_per_subreddit %>% tibble(timeframe=zwischen_save$timeframe, n=edges_per_subreddit) %>% select(timeframe,n) #
# #edges_per_subreddit$timeframe <- zwischen_save$timeframe
# edges_per_subreddit %>% group_by(timeframe) %>% summarise(edges_per_timeframe=sum(n)) %>% mutate(e_per_timeframe_normalized=edges_per_timeframe/tot_auth$n)
# 
# edges_per_subreddit
# combinator_2000(zwischen_save$n)



