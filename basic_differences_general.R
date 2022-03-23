#Basic Statstical Differences and Plots


source(paste0(masterarbeit_code_filepath,"Dependencies.R"))
source(paste0(masterarbeit_code_filepath,"functions.R"))






#you should run "estimatin control groups.R" at least once for each subreddit to not get an error in one plot here.

# -------------------------------------------------------------------------
############### Filepath and Subreddit and time of banishment



                                #######################################################################
subreddit <- "gendercritical" #######Change here the name of the subreddit you want to work #########
                                #######################################################################
filepath <- filepathgenerator(subreddit=subreddit)

banishment <- read_csv(paste0(reddit_data_filepath,subreddit,"BANISHMENT.csv"))




#Combining Datasets

tibble_submissions <-  read.csv(filepath[[5]]) %>% select(created_utc,retrieved_on,date)
tibble_submissions2 <- read.csv(filepath[[6]]) %>%  select(created_utc,retrieved_on,date)
tibble_submission <- add_row(tibble_submissions, tibble_submissions2)
tibble_comments <-  read.csv(filepath[[7]]) %>%  select(created_utc,retrieved_on,date)
tibble_comments2 <- read.csv(filepath[[8]])%>%  select(created_utc,retrieved_on,date) 
tibble_comments <- add_row(tibble_comments, tibble_comments2)


#Creating Plots of lost Data

submission_date <-  tibble_submission %>% select(c(created_utc,retrieved_on,date)) %>% mutate(Farbe = "Posts")
comment_date <- tibble_comments %>% select(c(created_utc,retrieved_on,date)) %>% mutate(Farbe = "Kommentare")
date_all <- add_row(submission_date,comment_date) 

plot1 <- ggplot(date_all, aes(x=anytime(created_utc),y=(retrieved_on-created_utc)/3600 ))+
  geom_point(aes(color = Farbe))+
  ggtitle( paste0("Zeit zwischen Reddit und Pushshift r/",subreddit))+
  xlab( "Datum der Posts/Kommentare auf Reddit")+
  ylab("Lag der Pushshift-Server (Stunden)")+
  geom_vline(xintercept = banishment, linetype="dotted", 
             color = "red", size=.5)+
  theme(text= element_text(size = 20))
  
plot1
ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/Lag_pushshift",subreddit,".png"))


#hist(date_all$created_utc)



# Constructing and combining Before and After Tibbles -----------------------------------

tibble_submissions <-  read_csv(filepath[[5]]) %>% 
  select(author,created_utc,subreddit,subreddit_subscribers, whitelist_status
         #,removed_by_category
         ,date,day, selftext) %>% 
  mutate(Post=TRUE,body=NA,  nchar = nchar(selftext, allowNA = TRUE))
#There is an Error, which is not a problem

print(paste("there are",sum(is.na(tibble_submissions$nchar)), "missing Values. Which is ",(sum(is.na(tibble_submissions$nchar))/length(tibble_submissions$nchar))*100, "percent"))


tibble_comments <- read_csv(filepath[[7]]) %>%
  select(author,created_utc,subreddit, date,day,body) %>%
  mutate(selftext= NA,Post=FALSE,subreddit_subscribers=NA,whitelist_status=NA
         #,removed_by_category =NA
         ,nchar = nchar(body, allowNA = TRUE))
#There is an Error, which is not a problem
print(paste("there are",sum(is.na(tibble_comments$nchar)), "missing Values. Which is ",(sum(is.na(tibble_comments$nchar))/length(tibble_comments$nchar))*100, "percent"))



tbl_before <- add_row(tibble_submissions,tibble_comments)


tibble_submissions <-  read_csv(filepath[[6]]) %>% 
  select(author,created_utc,subreddit,subreddit_subscribers, whitelist_status
         #,removed_by_category
         ,date,day, selftext) %>% 
  mutate(Post=TRUE,body=NA,  nchar = nchar(selftext, allowNA = TRUE))
  #There is an Error, which is not a problem

print(paste("there are",sum(is.na(tibble_submissions$nchar)), "missing Values. Which is ",(sum(is.na(tibble_submissions$nchar))/length(tibble_submissions$nchar))*100, "percent"))


tibble_comments <- read_csv(filepath[[8]]) %>%
  select(author,created_utc,subreddit, date,day,body) %>%
  mutate(selftext= NA,Post=FALSE,subreddit_subscribers=NA,whitelist_status=NA
         #,removed_by_category =NA
         ,nchar = nchar(body, allowNA = TRUE))
#There is an Error, which is not a problem
print(paste("there are",sum(is.na(tibble_comments$nchar)), "missing Values. Which is ",(sum(is.na(tibble_comments$nchar))/length(tibble_comments$nchar))*100, "percent"))


tbl_after <- add_row(tibble_submissions,tibble_comments)

#Total amount of Data
n_activity_before <- length(tbl_before$author)
n_activity_after <- length(tbl_after$author)

#constructing 10 Day Chunks

tbl_before <- tbl_before %>% drop_na(created_utc) #this drops one row of r/darknetmarkets. Otherwise there is no missing data.
#before
a_third <- (max(tbl_before$created_utc)-min(tbl_before$created_utc))%/%3 #these are 10.33 Days
tbl_before <- tbl_before %>% mutate(period= ifelse(created_utc<(min(created_utc)+a_third), 1, ifelse(created_utc<(min(created_utc)+2*a_third), 2,3)))      


#after
a_third1 <- (max(tbl_after$created_utc)-min(tbl_after$created_utc))%/%3 #these are 10.33 Days
tbl_after <- tbl_after %>% mutate(period= ifelse(created_utc<(min(created_utc)+a_third1), 4, ifelse(created_utc<(min(created_utc)+2*a_third1), 5,6)))  

tbl_comb <- add_row(tbl_before,tbl_after)


#keeping only accounts with at least 3 comments on subreddit
n_author <- tbl_comb$author %>% unique()


list <-list(shortcels="shortcels",braincels="Braincels",incelswithouthate="IncelsWithoutHate",darknetmarkets="DarkNetMarkets",me_ira="me_ira",gendercritical="GenderCritical") #because I don't write in capital letters, but sometimes the subreddit is with capital letters I need to account for this.
subreddit_exact_name <- list[subreddit]
keepers <- tbl_comb %>% filter(subreddit == subreddit_exact_name) %>% count(author) %>% filter(n>=3) %>% select(author)
tbl_comb <- left_join(keepers, tbl_comb, by="author")


#how many accounts are left out, because of that?


filtered_author <- length(n_author)-length(unique(tbl_comb$author))

#time between post by median account

count <- tbl_comb %>% count(author) #number of posts/comments per author
#tbl_comb %>% group_by(author) %>% min(created_utc)

post_frequency <- tbl_comb %>% group_by(author) %>% mutate(first_activity=min(created_utc)) %>%
  mutate(last_activity= max(created_utc)) 

post_frequency
post_frequency <- left_join(post_frequency,count, by="author")    

post_frequency <- post_frequency %>%ungroup() %>%  group_by(author) %>% summarise(frequence= (last_activity-first_activity)/n) %>% unique() 



post_frequency <- post_frequency %>%ungroup() %>%  group_by(author) %>% summarise(frequence= mean(last_activity-first_activity)/n) %>% unique() 
post_frequency <- post_frequency$frequence%>% median()
post_frequency_hours <- post_frequency/3600



###############################
##Conducting basic statistics##
###############################

# 1.1 ---------------------------------------------------------------------

# Change in Activity
#1.1 Change in overall ACtivity before and after ban

n_per_p <- tbl_comb %>% group_by(period) %>% count()
n_per_p
print(paste("the activity pre ban is", sum(n_per_p[1:3,2]),". The activity post ban is", sum(n_per_p[4:6,2]),
            ". This makes for a difference of",sum(n_per_p[4:6,2])-sum(n_per_p[1:3,2]),
            "which accounts for a change of ",abs(sum(n_per_p[4:6,2])-sum(n_per_p[1:3,2]))/(sum(n_per_p[1:3,2]))*100,
            "percent of the pre ban value."))

oneone <- print(paste("the activity pre ban is", sum(n_per_p[1:3,2]),". The activity post ban is", sum(n_per_p[4:6,2]),
                   ". This makes for a difference of",sum(n_per_p[4:6,2])-sum(n_per_p[1:3,2]),
                   "which accounts for a change of ",abs(sum(n_per_p[4:6,2])-sum(n_per_p[1:3,2]))/(sum(n_per_p[1:3,2]))*100,
                   "percent of the pre ban value."))


# #1.2 Change in Activity before and after ban per account ----------------



author_frequence <- tbl_comb %>% group_by(period,author) %>% count() %>% mutate(before= if_else(period<4,TRUE,FALSE)) #Returns a tbl with the activity per account per period and if that period is before or after
author_difference <- author_frequence  %>% select(author,n,before) %>% 
  group_by(author,before) %>%
  summarise(activity_before_after_ban= sum(n)) %>% 
  mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
  summarise(difference=sum(for_subtraction), total=sum(activity_before_after_ban)) %>% 
  mutate(weighted_difference=difference/total)


author_activity <- author_frequence  %>% select(author,n,before) %>% 
  group_by(author,before) %>%
  summarise(activity_before_after_ban= sum(n))

sd <- sd(author_difference$difference)/(length(author_difference$author))^(1/2)
sd_weighted <- sd(author_difference$weighted_difference)/(length(author_difference$author))^(1/2)
author_activity$activity_before_after_ban

author_difference

print(paste("the activity per account pre ban is on average",mean(author_activity$activity_before_after_ban[author_activity$before==TRUE]),
            "on median",median(author_activity$activity_before_after_ban[author_activity$before==TRUE]),
            "and on post ban on average of the accounts still active",mean(author_activity$activity_before_after_ban[author_activity$before==FALSE]),
            "on median",median(author_activity$activity_before_after_ban[author_activity$before==FALSE]),
            "on average with the inactive accounts", sum(n_per_p[4:6,2])/length(unique(author_frequence$author)),
             ".On median, accounts changed their activity by",median(author_difference$difference), ". On Average this is a change of",
            mean(author_difference$difference)
            ))

onetwo <- print(paste("the activity per account pre ban is on average",mean(author_activity$activity_before_after_ban[author_activity$before==TRUE]),
            "on median",median(author_activity$activity_before_after_ban[author_activity$before==TRUE]),
            "and on post ban on average of the accounts still active",mean(author_activity$activity_before_after_ban[author_activity$before==FALSE]),
            "on median",median(author_activity$activity_before_after_ban[author_activity$before==FALSE]),
            "on average with the inactive accounts", sum(n_per_p[4:6,2])/length(unique(author_frequence$author)),
            ".On median, accounts changed their activity by",median(author_difference$difference), ". On Average this is a change of",
            mean(author_difference$difference)
))




plot1 <- ggplot(author_difference)+
  geom_histogram(binwidth = 10, aes(y=difference))+
  ggtitle( paste0("Veränderung der Aktivität der Accounts von r/",subreddit))+
  xlab( "Anzahl Accounts")+
  ylab("Veränderung der Account-Aktivität")+
  geom_hline(yintercept = 0, 
             color = "red", size=1, alpha=1, linetype="dotted")+
  geom_hline(yintercept = median(author_difference$difference), 
            color = "blue", size=1, alpha=1,linetype="dotted")+
  geom_hline(yintercept = mean(author_difference$difference), 
             color = "green", size=1, alpha=1,linetype="dotted")+
 # geom_hline(yintercept= 0-2,71*mad(author_difference$difference), color="pink")+
  coord_flip()+
  theme(text= element_text(size = 20))
plot1

ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/histogramm",subreddit,".png")) 

#plot with nullhypothesis
plot1 <- ggplot()+
  #geom_histogram(aes(y=density$y),binwidth = 10)+

  geom_histogram(aes(y=author_difference$difference),binwidth = 5)+
# geom_freqpoly(aes(y=rnorm(length(author_difference$difference),0,variance)),color=3)+
 # geom_histogram(aes(y=rnorm(length(author_difference$difference),0,sd)),fill=5,alpha=0.5,binwidth=5) +
  ggtitle( paste0("Veränderung der Aktivität der Accounts von r/",subreddit))+
  xlab( "Anzahl Accounts")+
  ylab("Veränderung der Account-Aktivität")+
  geom_hline(yintercept = 0, 
             color = "red", size=1, alpha=1, linetype="dotted")+
  geom_hline(yintercept = median(author_difference$difference), 
             color = "blue", size=1, alpha=1,linetype="dotted")+
  geom_hline(yintercept = mean(author_difference$difference), 
             color = "green", size=1, alpha=1,linetype="dotted")+
  coord_flip()+
  theme(text= element_text(size = 20))
plot1

# density <- density(author_difference$difference,kernel="gaussian")
# density
ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/histogramm_nullhypothesis",subreddit,".png")) 



#Plot with weighted difference
control <- read_csv2(paste0(masterarbeit_code_filepath,"images/",subreddit,"/korrektur_terme_",subreddit,".csv") )

author <- tibble(author=(unique(tbl_comb$author)))
author_post <-  tbl_comb %>% count(author,Post) %>% filter(Post==TRUE)
author_comment <-  tbl_comb %>% count(author,Post) %>% filter(Post==FALSE)



left <- left_join(author,author_comment,by="author")
left <- left_join(left,author_post,by="author")
left$n.y <- ifelse(is.na(left$n.y),0,left$n.y)
left$n.x <- ifelse(is.na(left$n.x),0,left$n.x)

#this below is an weighted average for the expected amount of activity loss, if no ban had occured. It is based on the data of a control-group 
exp_activity_loss <- left %>% mutate(exp_activity_loss=(n.x*control$`erwartet Unterschied Kommentare gewichtet Autor`+n.y*control$`erwartet Unterschied Posts gewichtet Autor`)/(n.x+n.y))
expected_loss <- mean(exp_activity_loss$exp_activity_loss)


plot1 <- ggplot()+
  #geom_histogram(aes(y=density$y),binwidth = 10)+
  
  geom_histogram(aes(y=author_difference$weighted_difference),binwidth = 0.01)+
  geom_errorbar(aes(x=length(author_difference$author)*0.2,y=mean(author_difference$weighted_difference),ymin=mean(author_difference$weighted_difference)-1.96*sd_weighted, ymax=mean(author_difference$weighted_difference)+1.96*sd_weighted, width=.2)) +
  # geom_freqpoly(aes(y=rnorm(length(author_difference$difference),0,variance)),color=3)+
  # geom_histogram(aes(y=rnorm(length(author_difference$difference),0,sd)),fill=5,alpha=0.5,binwidth=5) +
  ggtitle( paste0("Veränderung der Aktivität der Accounts von r/",subreddit))+
  xlab( "Anzahl Accounts")+
  ylab("Veränderung der Account-Aktivität")+
  geom_hline(yintercept = 0, 
             color = "red", size=1, alpha=1, linetype="dotted")+
   # geom_hline(yintercept = median(author_difference$weighted_difference), 
   #            color = "blue", size=1, alpha=1,linetype="dotted")+
  geom_hline(yintercept = mean(author_difference$weighted_difference), 
             color = "green", size=1, alpha=1,linetype="dotted")+
  geom_hline(yintercept = expected_loss, 
             color = "blue", size=1, alpha=1,linetype="dotted")+
   coord_flip()+
  theme(text= element_text(size = 20))
plot1

# density <- density(author_difference$difference,kernel="gaussian")
# density
ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/weighted_difference_signifkanzbalken",subreddit,".png")) 

# schöne-zusammenfassung-Generator --------------------------------------------------------------

difference_filemaker(subreddit,author_activity,author_frequence) #this creates a .csv with per account, activity before/after, expectation and weighted_difference


# #2.1 Change in overall activity per period ------------------------------




n_per_p #this is the total activity per period
twoone <- n_per_p %>% mutate(total_activity_per_period=n)


plot1 <- ggplot(n_per_p, aes(x=period,y=n))+
  geom_smooth(method=lm)+
  geom_point()+
  ggtitle( paste0("Aktivität pro Periode von r/",subreddit))+
  xlab( "10 Tage, Beginn 31 Tage vor Bann")+
  geom_vline(xintercept = 3.5, linetype="dotted", 
             color = "red", size=.5)+
  
  ylab("Gesamtzahl Kommentare und Posts")+
  theme(text= element_text(size = 20))

plot1

# plot1 <- ggplot(n_per_p, aes(x=period,y=n))+
#   geom_point()+
#   ggtitle( paste0("Aktivität pro Periode von r/",subreddit))+
#   xlab( "10 Tage, Beginn 31 Tage vor Bann")+
#   ylab("Gesamtzahl Kommentare und Posts")+
#   theme(text= element_text(size = 20))
# 
# plot1
# 
# plot1 <- ggplot(n_per_p, aes(x=period,y=cumsum(n_per_p$n)))+
#   geom_line()+
#   ggtitle( paste0("Aktivität pro Periode von r/",subreddit))+
#   xlab( "10 Tage, Beginn 31 Tage vor Bann")+
#   ylab("Gesamtzahl Kommentare und Posts")+
#   theme(text= element_text(size = 20))
# 
# plot1
ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/gesamtaktivität_pro_periode_",subreddit,".png")) 



# #2.2 Change in activity per period per account --------------------------


author_frequence <- tbl_comb %>% group_by(period,author) %>% count() %>% mutate(before= if_else(period<4,TRUE,FALSE))



plot1 <- ggplot(author_frequence)+
  #geom_point(aes(x=as_factor(period),y=n),position = "jitter")+
  geom_boxplot( aes(x=as_factor(period),y=n))+

    ggtitle( paste0("Aktivität pro Periode von r/",subreddit))+
  xlab( "10 Tage, Beginn 31 Tage vor Bann")+
  ylab("Gesamtzahl Kommentare und Posts pro Account")+

  theme(text= element_text(size = 20))
plot1 #The Points shown are outliers. That means they are over 1.5 times the interquartile-distance.

ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/aktivität_pro_periode_pro_Account",subreddit,".png")) 




# #3.1 Number of Inactive Accounts per Date -------------------------------


begin <- min(tbl_comb$created_utc)

last_activity <- tbl_comb %>% group_by(author) %>% summarise(last_activity=max(day))

  
  plot1 <- ggplot(last_activity)+
  geom_bar( aes(y=last_activity))+
  ggtitle( paste0("Accountinaktivität von r/",subreddit))+
  xlab( "Anzahl Accounts letzte Aktivitäten")+
  ylab("Datum")+
   geom_hline(yintercept = as.Date.POSIXct(begin+a_third), 
   color = "black", size=.2, alpha=0.2)+
  geom_hline(yintercept = as.Date.POSIXct(begin+2*a_third),
             color = "black", size=.2, alpha=0.2)+
  geom_hline(yintercept = as.Date.POSIXct(begin+3*a_third),
             color = "black", size=.2, alpha=0.2)+
  geom_hline(yintercept = as.Date.POSIXct(begin+a_third1+3*a_third),
             color = "black", size=.2, alpha=0.2)+
  geom_hline(yintercept = as.Date.POSIXct(begin+2*a_third1+3*a_third),
             color = "black", size=.2, alpha=0.2)+
  geom_hline(yintercept = as.Date.POSIXct(begin+3*a_third1+3*a_third),
             color = "black", size=.2, alpha=0.2)+
    geom_hline(yintercept = as.Date.POSIXct(max(banishment$created_utc)),
               color = "red", size=.5)+
    geom_hline(yintercept = as.Date.POSIXct(max(banishment$created_utc)-post_frequency), linetype="dotted",
               color = "blue", size=.5)+
   coord_flip()+
   theme(text= element_text(size = 20))
plot1



ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/Accountinaktivität_Datum_",subreddit,".png")) 

num_account_inactive_after_ban<- sum(last_activity$last_activity<=as.Date.POSIXct(max(banishment$created_utc)))

# #3.2 Number of inactive Accounts per Period -----------------------------


last_activity <- tbl_comb %>% group_by(author) %>% summarise(last_activity=max(period))
last_activity_period <- last_activity %>% count(last_activity)
last_activity_period$n
last_activity_period$n %>% cumsum()
threetwo <- last_activity_period 


# #4.1 Change of length of comments/posts ---------------------------------



tbl_nchar <- tbl_comb %>% mutate(nchar = nchar(body)) %>% mutate(nchar2 = nchar(selftext))
nchar_before1 <- tbl_nchar %>% drop_na(nchar) %>% filter(period < 4) %>% summarise(total=sum(nchar))
nchar_before2 <- tbl_nchar %>% drop_na(nchar2) %>% filter(period < 4) %>% summarise(total=sum(nchar2)) 
nchar_before <- nchar_before1+nchar_before2

tbl_nchar <- tbl_comb %>% mutate(nchar = nchar(body)) %>% mutate(nchar2 = nchar(selftext))
nchar_after1 <- tbl_nchar %>% drop_na(nchar) %>% filter(period > 3) %>% summarise(total=sum(nchar))
nchar_after2 <- tbl_nchar %>% drop_na(nchar2) %>% filter(period > 3) %>% summarise(total=sum(nchar2)) 
nchar_after <- nchar_after1+nchar_after2

after_total <- nchar_after %>% summarise(total=sum(total))
before_total <- nchar_before %>% summarise(total=sum(total))

n_post_before <- tbl_comb %>% filter(period <4) %>% count()
n_post_after <- tbl_comb %>% filter(period>3) %>% count()


print(paste0("the total amount of characters before ban was ",before_total," and the toal amount of character after ban was "
             ,after_total,". That's a difference of "
             ,before_total-after_total,
             " or a decline of ",
             after_total/before_total*100, "percent.",
             " The lenght of the average post changed from ", before_total/n_post_before,
             " to ", after_total/n_post_after))



fourone <- print(paste0("the total amount of characters before ban was ",before_total," and the toal amount of character after ban was "
             ,after_total,". That's a difference of "
             ,before_total-after_total,
             " or a decline of ",
             after_total/before_total*100, "percent.",
             " The lenght of the average post changed from ", before_total/n_post_before,
             " to ", after_total/n_post_after))




# # missing percentage
# 
# 
# ((sum(is.na(tbl_nchar$selftext[tbl_nchar$Post == TRUE]))+ sum(is.na(tbl_nchar$body[tbl_nchar$Post == FALSE]))))/
#   (((length(tbl_nchar$selftext[tbl_nchar$Post == TRUE])+length(tbl_nchar$body[tbl_nchar$Post == FALSE]))))*100


# # 4.2 Change of lenght of comments/posts per author ---------------------


tbl_nchar <- tbl_comb %>% mutate(nchar = nchar(body)) %>% mutate(nchar2 = nchar(selftext))
sum(!(is.na(tbl_nchar$selftext)) & !(is.na(tbl_nchar$body))) #to check if there ar NA's in the text.

nchar_before1 <- tbl_nchar %>% drop_na(nchar) %>% filter(period < 4) %>% group_by(author) %>%  summarise(total=sum(nchar))

nchar_before2 <- tbl_nchar %>% drop_na(nchar2) %>% filter(period < 4) %>% group_by(author) %>% summarise(total=sum(nchar2)) 
nchar_before <- rbind(nchar_before1,nchar_before2)
nchar_before <- nchar_before %>% group_by(author) %>% summarise(total=sum(total))


nchar_after1 <- tbl_nchar %>% drop_na(nchar) %>% filter(period > 3) %>% group_by(author) %>%summarise(total=sum(nchar))
nchar_after2 <- tbl_nchar %>% drop_na(nchar2) %>% filter(period > 3) %>% group_by(author) %>%summarise(total=sum(nchar2))
nchar_after <- rbind(nchar_after1,nchar_after2)
nchar_after <- nchar_after %>% group_by(author) %>% summarise(total=sum(total))

difference <- left_join(nchar_before,nchar_after, by= "author") 
difference$total.x <- difference$total.x*-1
difference$total.y <-  ifelse(is.na(difference$total.y),0,difference$total.y)


#total.x is the count for before ban and total.y the one for after the ban.

difference$change <- apply(difference[,2:3],1,sum)
difference


print(paste0("the median length of posts pre ban is ", median(-1*difference$total.x), " after ban it is ",
             median(difference$total.y), ". The mean length of posts pre ban is ", mean(-1*difference$total.x),
             " and post ban it is ", mean(difference$total.y),
                                           ". This is a average Change of ", mean(difference$change),
                                           " and a median change of ", median(difference$change)) )
fourtwo <- print(paste0("the median length of posts pre ban is ", median(difference$total.x), " after ban it is ",
                        median(difference$total.y), ". The median length of posts post ban is ", median(difference$total.y),
                        " and post ban it is ", median(difference$total.y),
                        ". This is a average Change of ", mean(difference$change),
                        " and a median change of ", median(difference$change)) )


plot1 <- ggplot(difference)+
  geom_histogram(binwidth = 1000, aes(y=change))+
  ggtitle( paste0("Veränderung der Post/Kommentarlänge der Accounts"))+
  xlab( "Anzahl Accounts")+
  ylab("Veränderung der Post/Kommentarlänge")+
  
  coord_flip()+
  geom_hline(yintercept = 0, size=0.2, color = "red",linetype="dotted")+
  geom_hline(yintercept= mean(difference$change), color ="green", linetype="dotted", size=0.2)+
  theme(text= element_text(size = 20))
plot1


ggsave(paste0(masterarbeit_code_filepath,"images/",subreddit,"/histogramm_kommentarlänge",subreddit,".png")) 

#5.1 Change in Subreddits per Account

unique_subreddits_pre <- tbl_comb %>% filter(tbl_comb$period <4) %>% select(author,subreddit) %>% group_by(author) %>% unique() %>% count()

unique_subreddits_post <- tbl_comb %>% filter(tbl_comb$period >3) %>% select(author,subreddit) %>% group_by(author) %>% unique() %>% count()

print(paste0("the average number of subreddits an active account was active on pre-ban is: ", mean(unique_subreddits_pre$n),
             " on median this is ", median(unique_subreddits_pre$n),
             ". Post ban the numbers are:  ", mean(unique_subreddits_post$n),
             " and ",  median(unique_subreddits_post$n)))

fiveone <- print(paste0("the average number of subreddits an active account was active on pre-ban is: ", mean(unique_subreddits_pre$n),
                        " on median this is ", median(unique_subreddits_pre$n),
                        ". Post ban the numbers are:  ", mean(unique_subreddits_post$n),
                        " and ",  median(unique_subreddits_post$n)))


#creating output of all the textes



post_frequency_hours


textual_output <- (list("total_activity"=oneone,"total_activity per author"=onetwo,"total activity per period"=twoone,"num_account_last_activity per period"=threetwo,
                        "total_characters"=fourone,"lenght of posts"=fourtwo,"number of subreddits"=fiveone,"filtered_authors"=filtered_author,
                        "hours between posts"=post_frequency_hours, "number authors"=n_author, "total Activitybefore"=n_activity_before, "total_activityafter"=n_activity_after))
textual_output


write.list(textual_output,paste0(masterarbeit_code_filepath,"images/",subreddit,"/textual_output_",subreddit,".csv"))

# oneone
# onetwo
# twoone
# threetwo
# fourone
# fourtwo
# fiveone



#???Ich kann mich nicht von dem unten trennen??????????????????????????????

#II
difference$total.y <- if_else(is.na(difference$total.y),0,difference$total.y)



print(paste0("the total lenght per account pre ban is on average ",mean(nchar_before$total),
            " on median ",median(nchar_before$total),
            " and after ban on average ",mean(nchar_after$total),
            " on median ",median(nchar_after$total),
            ". On median, accounts changed their number of characters by", median(difference$change), " this is a change of ",
            mean(author_difference$difference/author_difference$total*100)," percent",
            " and on median the reduction is",median(author_difference$difference/author_difference$total*100),"percent" 
))






#nchar before/after ban

tbl_nchar %>% filter(period<3) %>% count(nchar)  #%>% summarise(summand= sum(nchar,na.rm = TRUE))

tbl_nchar %>% filter(period<3) %>%group_by(period) %>%  mutate(sum = sum(nchar, na.rm = TRUE))

n_per_p <- tbl_nchar %>% group_by(period) %>% sum(nchar, na.rm=TRUE)
n_per_p


is.na(tbl_nchar$nchar)

tbl_nchar %>% filter(period<3) %>% tbl_nchar$nchar(is.na(nchar)==FALSE)




# -------------------------------------------------------------------------




































# tbl_comb %>% ungroup() %>% summarise(author,created_utc) %>% count()
# tbl_comb %>% summarise(author) %>% max(tbl_comb$created_utc) %>% max(created_utc)
# min(tbl_comb$date)

#Machine_learning Model
# training_num <- sample(1:nrow(author_frequence), 0.8*nrow(author_frequence))
# training <- author_frequence[training_num,]
# 
# test <- anti_join(author_frequence,training, by=c("author","n","period"))


a <- lm(author_frequence$n~author_frequence$period)
summary(a)

author_frequence <- author_frequence %>%  group_by(author) %>% summarise(n,period)  %>% mutate(relative_n=n/sum(n)) %>% cumsum(n) #%>% mutate(q=sum(relative_n))  #mutate(relative_n=n/)
author_frequence$relative_n


a <- lm(author_frequence$relative_n~author_frequence$period)
summary(a)


plot1 <- ggplot(author_frequence, aes(x=period,y=relative_n))+
  geom_point()+
  ggtitle( paste0("Aktivität pro Periode von r/",subreddit))+
  xlab( "10 Tage, Beginn 31 Tage vor Bann")+
  ylab("Gesamtzahl Kommentare und Posts")+
  geom_vline(xintercept = 4, linetype="dotted", 
               color = "blue", size=1.5)+
  theme()

plot1

author_frequence %>% filter(relative_n == 0)

#Creating Plots of lost Data

submission_date$retrieved_on
submission_date <-  tibble_submissions %>% select(c(created_utc,date)) %>% mutate(Farbe = "Posts")
comment_date <- tibble_comments %>% select(c(created_utc,date)) %>% mutate(Farbe = "Kommentare")
date_all <- add_row(submission_date,comment_date) 
tibble_all <- add_row(tibble_submissions,tibble_comments)

#Activity of accounts in each sector
date_all$
tbl_all %>% select(author) %>% count()
post_per_author <- tibble_all %>% mutate(n =count(author))
s <- tbl_comb %>% summarise(groups=author,body)


