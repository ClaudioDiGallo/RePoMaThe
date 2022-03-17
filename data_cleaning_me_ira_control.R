# #BotDetection me_ira_CONTROL -----------------------------------------



source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


#generate filepath object-list

subreddit <- "me_ira"
filepath <- filepathgenerator(subreddit) #put here the name of the sub to be cleansed

companion()

# Submissions Before Ban + DAta which has been re-scraped, because the limit in the account finder was to low --------------------------------------------------



tibble_all <- read.csv(filepath[[9]])



to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 

post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,title) %>% mutate(nchar=nchar(title)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))

post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))


hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts Ã¼ber Anzahl Posts pro Account for Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

view(post_per_author)

bad_accounts_detector_posts(tibble_all = tibble_all, samplesize = 15, max_var = 30) #if this results in an error, its because there were no suspicious accounts found. You can check by increasing the max_var-Argument

# bad_accounts_append(list("ujahir18"))
# 
#" tibble_all$title[tibble_all$author == "byzantine_meme"] 
#  tibble_all$title
# 
# to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
# tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) 

#adding column date.time -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")



hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day)

# saving ------------------------------------------------------------------

write.csv(tibble_all, file = filepath[[14]])



############################################
# SUBMISSIONS but Post-Ban ----------------------------------------------------
############################################


companion()


# Submissions AFter Ban --------------------------------------------------



tibble_all <- read.csv(filepath[[10]])
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 



post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,title) %>% mutate(nchar=nchar(title)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))

hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts Ã¼ber Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")
view(post_per_author)


bad_accounts_detector_posts(tibble_all, samplesize = 10)


view(post_per_author)
# 
# tibble_all$title[tibble_all$author == "dumbfuckjuice1"] 
# 
# 
# 
# tibble_all$date
#gaps in data


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")
hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) #Zwischen dem zwischen 17. und 28. Fehlen die Posts, der erste Tag is shorter because I didn't begin on 00:00 on the 11th, but 31*24*60*60 Seconds before the last post on the subreddit and post is after the last post


# saving ------------------------------------------------------------------

write.csv(tibble_all,file = filepath[[15]])
