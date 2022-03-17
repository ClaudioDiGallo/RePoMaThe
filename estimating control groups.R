#Basic Statstical Differences and Plots


source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


# -------------------------------------------------------------------------
############### Filepath and Subreddit and time of banishment



#######################################################################
subreddit <- "gendercritical" #######Change here the name of the subreddit you want to work #########
#######################################################################
filepath <- filepathgenerator(subreddit=subreddit)


#loading Data


tibble_submissions_before <-  read.csv(filepath[[9]]) %>% select(created_utc,retrieved_on,author,subreddit) %>% mutate(before=TRUE)
tibble_submissions_after <-  read.csv(filepath[[10]]) %>% select(created_utc,retrieved_on,author,subreddit)%>% mutate(before=FALSE)
tbl_all <- add_row(tibble_submissions_before,tibble_submissions_after)


#Creating Plots of lag


plot1 <- ggplot(tbl_all, aes(x=anytime(created_utc),y=(retrieved_on-created_utc)/3600 ))+
  geom_point()+
  ggtitle( paste0("Zeit zwischen Reddit und Pushshift r/",subreddit))+
  xlab( "Datum der Posts/Kommentare auf Reddit")+
  ylab("Lag der Pushshift-Server (Stunden)")+

  theme(text= element_text(size = 20))

plot1
ggsave(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/Lag_pushshift_control",subreddit,".png"))


#Unterschied vorher nachher gesamtaktivität

total_activity_change <- length(tibble_submissions_before$created_utc)-length(tibble_submissions_after$created_utc)

print(paste0("Das Verhältnis von Vormonat, nachmonat liegt bei ",(length(tibble_submissions_after$created_utc)/length(tibble_submissions_before$created_utc))))

#Unterschied mit mindestens zwei Posts


tibble_submissions_before <- filter_author(subreddit,min_number=2,tibble=tibble_submissions_before)
tibble_submissions_after <- filter_author(subreddit,min_number=2,tibble=tibble_submissions_after)
tbl_all <- add_row(tibble_submissions_before,tibble_submissions_after)

print(paste0("Das Verhältnis von Vormonat, nachmonat liegt bei den Accounts mit mindestens 2 Posts bei ",(length(tibble_submissions_after$created_utc)/length(tibble_submissions_before$created_utc))))
proport_control <- (length(tibble_submissions_after$created_utc)/length(tibble_submissions_before$created_utc))
proport_control

#getting author weighted change data and standard deviations

author_frequence <- tbl_all %>% group_by(before,author) %>% count()

author_difference <- author_frequence  %>% select(author,n,before) %>% 
  group_by(author,before) %>%
  summarise(activity_before_after_ban= sum(n)) %>% 
  mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
  summarise(difference=sum(for_subtraction), total=sum(activity_before_after_ban)) %>% 
  mutate(weighted_difference=difference/total)

author_difference_weighted_submission <- mean(author_difference$weighted_difference)

#median_author_difference_weighted_submission <- median(author_difference$weighted_difference)



#Weighted Difference per User Estimation of loss of comments by knowing the loss of submissions


tibble_submissions_before <-  read.csv(filepath[[5]]) %>% select(author,subreddit) %>% mutate(before=TRUE)
tibble_submissions_after <-  read.csv(filepath[[6]])%>% select(author,subreddit) %>% mutate(before=FALSE)
tbl_comb <- add_row(tibble_submissions_before,tibble_submissions_after)


tbl_comb <- filter_author(subreddit,min_number=2,tibble=tbl_comb)

author_frequence <- tbl_comb %>% group_by(before,author) %>% count()  #Returns a tbl with the activity per account per period and if that period is before or after
author_frequence
author_difference <- author_frequence  %>% select(author,n,before) %>% 
  group_by(author,before) %>%
  summarise(activity_before_after_ban= sum(n)) %>% 
  mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
  summarise(difference=sum(for_subtraction), total=sum(activity_before_after_ban)) %>% 
  mutate(weighted_difference=difference/total)

difference_submissions <- mean(author_difference$weighted_difference)


#The same but with comments


tibble_comments_before <-  read.csv(filepath[[7]]) %>% select(author,subreddit) %>% mutate(before=TRUE)
tibble_comments_after <-  read.csv(filepath[[8]])%>% select(author,subreddit) %>% mutate(before=FALSE)
tbl_comb <- add_row(tibble_comments_before,tibble_comments_after)


tbl_comb <- filter_author(subreddit,min_number=3,tibble=tbl_comb)

author_frequence <- tbl_comb %>% group_by(before,author) %>% count()  
author_frequence
author_difference <- author_frequence  %>% select(author,n,before) %>% 
  group_by(author,before) %>%
  summarise(activity_before_after_ban= sum(n)) %>% 
  mutate(for_subtraction= ifelse(before==TRUE,activity_before_after_ban*-1,activity_before_after_ban)) %>% 
  summarise(difference=sum(for_subtraction), total=sum(activity_before_after_ban)) %>% 
  mutate(weighted_difference=difference/total)

difference_comments <- mean(author_difference$weighted_difference)

expected_comment_difference<- difference_comments/difference_submissions*author_difference_weighted_submission
expected_comment_difference


result_tbl <- tibble("erwartet Unterschied Posts gewichtet Autor"=author_difference_weighted_submission,
                     "erwartet Unterschied Kommentare gewichtet Autor"=expected_comment_difference
                    # "Verh�ltnis Abnahme Posts insgesamt"=proport_control,"Verh�ltnis Abnahme Kommentare erwartet insgesamt"=correction_comments
                     )
result_tbl
write_csv2(result_tbl,paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/",subreddit,"/korrektur_terme_",subreddit,".csv") )


# 
# 
# 
# 
# #How do sinking submissions correlate with sinking comments?
# 
# 
# tibble_submissions_before <-  read.csv(filepath[[5]])
# tibble_submissions_after <-  read.csv(filepath[[6]]) 
# 
# 
# tibble_comments_before <-  read.csv(filepath[[7]]) 
# tibble_comments_after <-  read.csv(filepath[[8]])
# 
# # #As next we have the question: Wenn die Kommentare soviel sinken, wenn die Submissions um andersviel sinken. Wieviel wären sie gesunken, wenn es die Kontrollsubmissions sind. Das gibt und dann die Geschätzte Anzahl Kommentare, die verloren gingen 
# 
# print(paste("posts have to be corrected for:", proport_control,
#             "comments have to be corrected for:", ((length(tibble_comments_after$created_utc)/length(tibble_comments_before$created_utc))/(length(tibble_submissions_after$created_utc)/length(tibble_submissions_before$created_utc)))*proport_control))
# 
# correction_comments <- ((length(tibble_comments_after$created_utc)/length(tibble_comments_before$created_utc))/(length(tibble_submissions_after$created_utc)/length(tibble_submissions_before$created_utc)))*proport_control
# 
