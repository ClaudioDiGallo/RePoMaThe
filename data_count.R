#Datacount



source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


# -------------------------------------------------------------------------
############### Filepath and Subreddit and time of banishment

sublist <- list("darknetmarkets","incelswithouthate","shortcels","Braincels","me_ira","gendercritical")


total_darknetmarkets <- data_count(sublist[[1]])
total_incelswithouthate <- data_count(sublist[[2]])
total_me_ira <- data_count(sublist[[5]])
total_gendercritical <- data_count(sublist[[6]])


# for braincels i had to re-scrape data, and the format is not equ --------

subreddit <- "braincels"
filepath <- filepathgenerator(subreddit=subreddit)
tibble_all <- read.csv(filepath[[1]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/braincelsselected_authorsSUBMISSIONSBEFORE_BANCompleter.csv") 
raw_sub_before <- add_row(tibble_all, tibble_all_extended)%>% select(created_utc) %>% nrow()

tibble_all <- read.csv(filepath[[2]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/braincelsselected_authorsSUBMISSIONSAFTER_BANCompleter.csv") 
raw_sub_after <- add_row(tibble_all, tibble_all_extended)%>% select(created_utc) %>% nrow()

tibble_all_extended2 <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/Braincelsselected_authorsCOMMENTSBEFORE_BAN2.csv")
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/Braincelsselected_authorsCOMMENTSBEFORE_BAN1.csv") 
raw_comments_before <- add_row(tibble_all_extended2, tibble_all_extended)%>% select(created_utc) %>% nrow()

tibble_all <- read.csv(filepath[[4]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/Braincelsselected_authorsCOMMENTSAFTER_BANCompleter.csv") 
raw_comments_after <- add_row(tibble_all, tibble_all_extended)%>% select(created_utc) %>% nrow()

sub_comment<-  read.csv(filepath[[11]]) %>% select(created_utc) %>% nrow()
sub_submission <-  read.csv(filepath[[12]]) %>% select(created_utc) %>% nrow()
banishment <-  read.csv(filepath[[13]]) %>% select(created_utc) %>% nrow()

total_braincels <- raw_sub_before+raw_sub_after+raw_comments_after+raw_comments_before+sub_comment+sub_submission+banishment




# for shortcels i also had to re-scrape the data --------------------------

subreddit <- "shortcels"
filepath <- filepathgenerator(subreddit=subreddit)
tibble_all <- read.csv(filepath[[1]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsSUBMISSIONSBEFORE_BANCompleter.csv") 
raw_sub_before <- add_row(tibble_all, tibble_all_extended)%>% select(created_utc) %>% nrow()

tibble_all <- read.csv(filepath[[2]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsSUBMISSIONSAFTER_BANCompleter.csv") 
raw_sub_after <- add_row(tibble_all, tibble_all_extended)%>% select(created_utc) %>% nrow()

tibble_all <- read.csv(filepath[[3]])%>% select(created_utc)
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsCOMMENTSBEFORE_BANCompleter.csv") %>% select(created_utc)
raw_comments_before <- add_row(tibble_all_extended2, tibble_all_extended)%>% select(created_utc) %>% nrow()

tibble_all <- read.csv(filepath[[4]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsCOMMENTSAFTER_BANCompleter.csv") 
raw_comments_after <- add_row(tibble_all, tibble_all_extended)%>% select(created_utc) %>% nrow()

sub_comment<-  read.csv(filepath[[11]]) %>% select(created_utc) %>% nrow()
sub_submission <-  read.csv(filepath[[12]]) %>% select(created_utc) %>% nrow()
banishment <-  read.csv(filepath[[13]]) %>% select(created_utc) %>% nrow()

total_shortcels <- raw_sub_before+raw_sub_after+raw_comments_after+raw_comments_before+sub_comment+sub_submission+banishment









#long exposure submissions


total_long_exposure <-  read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/INCEL_ACCOUNTSselected_authorsSUBMISSIONSLONGEXPOSURE.csv") %>% select(created_utc) %>% nrow()


#controlsubs
subreddit <- "shortcels"
filepath <- filepathgenerator(subreddit=subreddit)

tibble_submissions_before <-  read.csv(filepath[[9]]) %>% select(created_utc,retrieved_on,author,subreddit) %>% mutate(before=TRUE)
tibble_submissions_after <-  read.csv(filepath[[10]]) %>% select(created_utc,retrieved_on,author,subreddit)%>% mutate(before=FALSE)
shortcels_control <- add_row(tibble_submissions_before,tibble_submissions_after)%>% select(created_utc) %>% nrow()


subreddit <- "braincels"
filepath <- filepathgenerator(subreddit=subreddit)

tibble_submissions_before <-  read.csv(filepath[[9]]) %>% select(created_utc,retrieved_on,author,subreddit) %>% mutate(before=TRUE)
tibble_submissions_after <-  read.csv(filepath[[10]]) %>% select(created_utc,retrieved_on,author,subreddit)%>% mutate(before=FALSE)
braincels_control <- add_row(tibble_submissions_before,tibble_submissions_after)%>% select(created_utc) %>% nrow()


subreddit <- "gendercritical"
filepath <- filepathgenerator(subreddit=subreddit)

tibble_submissions_before <-  read.csv(filepath[[9]]) %>% select(created_utc,retrieved_on,author,subreddit) %>% mutate(before=TRUE)
tibble_submissions_after <-  read.csv(filepath[[10]]) %>% select(created_utc,retrieved_on,author,subreddit)%>% mutate(before=FALSE)
gendercritical_control <- add_row(tibble_submissions_before,tibble_submissions_after)%>% select(created_utc) %>% nrow()



subreddit <- "me_ira"
filepath <- filepathgenerator(subreddit=subreddit)

tibble_submissions_before <-  read.csv(filepath[[9]]) %>% select(created_utc,retrieved_on,author,subreddit) %>% mutate(before=TRUE)
tibble_submissions_after <-  read.csv(filepath[[10]]) %>% select(created_utc,retrieved_on,author,subreddit)%>% mutate(before=FALSE)
me_ira_control <- add_row(tibble_submissions_before,tibble_submissions_after)%>% select(created_utc) %>% nrow()

total_control <- me_ira_control+gendercritical_control+braincels_control+shortcels_control

total_darknetmarkets <- data_count(sublist[1])
total_incelswithouthate <- data_count(sublist[2])
total_me_ira <- data_count(sublist[5])
total_gendercritical <- data_count(sublist[6])



total_total <- total_darknetmarkets+total_incelswithouthate+total_me_ira+total_gendercritical+total_braincels+total_shortcels+total_long_exposure+total_control

tbl <- tibble( total_darknetmarkets,total_incelswithouthate,total_me_ira,total_gendercritical,total_braincels,total_shortcels,total_long_exposure,total_control)
write_csv2(tbl,paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/total_count.csv") )


 #loading Data
# 
# raw_sub_before <-  read.csv(filepath[[1]]) %>% select(created_utc) %>% nrow()
# raw_sub_after <-  read.csv(filepath[[2]]) %>% select(created_utc) %>% nrow()
# raw_comments_before <-  read.csv(filepath[[3]]) %>% select(created_utc) %>% nrow()
# raw_comments_after <-  read.csv(filepath[[4]]) %>% select(created_utc) %>% nrow()
# sub_comment<-  read.csv(filepath[[11]]) %>% select(created_utc) %>% nrow()
# sub_submission <-  read.csv(filepath[[12]]) %>% select(created_utc) %>% nrow()
# banishment <-  read.csv(filepath[[13]]) %>% select(created_utc) %>% nrow()
# 
# 
# total <- raw_sub_before+raw_sub_after+raw_comments_after+raw_comments_before+sub_comment+sub_submission+banishment
# 
