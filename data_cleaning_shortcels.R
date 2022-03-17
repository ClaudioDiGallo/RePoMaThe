


# #BotDetection Shortcels -----------------------------------------



source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


#generate filepath object-list

filepath <- filepathgenerator(subreddit="shortcels") #put here the name of the sub to be cleansed
subreddit <- "shortcels"
companion()

# Submissions Before Ban + DAta which has been re-scraped, because the limit in the account finder was to low --------------------------------------------------



tibble_all <- read.csv(filepath[[1]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsSUBMISSIONSBEFORE_BANCompleter.csv") #there are 4 variables missing. I don't know why, but important ones are still there and plausible. Maybe the variables are missing, if they are empty for all rows.
tibble_all <- add_row(tibble_all, tibble_all_extended)


to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 

post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,title) %>% mutate(nchar=nchar(title)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))

post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))


hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account for Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

view(post_per_author)

bad_accounts_detector_posts(tibble_all = tibble_all, samplesize = 15, max_var = 30) #if this results in an error, its because there were no suspicious accounts found. You can check by increasing the max_var-Argument

bad_accounts_append(list("ujahir18"))

tibble_all$title[tibble_all$author == "baby-amber"] 
tibble_all$title

to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) 

#adding column date.time -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")



hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day)

# saving ------------------------------------------------------------------

write.csv(tibble_all, file = filepath[[5]])



############################################
# SUBMISSIONS but Post-Ban ----------------------------------------------------
############################################


companion()


# Submissions AFter Ban --------------------------------------------------



tibble_all <- read.csv(filepath[[2]])
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsSUBMISSIONSAFTER_BANCompleter.csv") #Yes, there are 4 variables missing. I don't know why, but important ones are still there and plausible.
tibble_all <- add_row(tibble_all, tibble_all_extended)

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 



post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,title) %>% mutate(nchar=nchar(title)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))

hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")
view(post_per_author)


bad_accounts_detector_posts(tibble_all, samplesize = 10)


view(post_per_author)

tibble_all$title[tibble_all$author == "dumbfuckjuice1"] 



tibble_all$date
#gaps in data


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")
hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) #Zwischen dem zwischen 17. und 28. Fehlen die Posts, der erste Tag is shorter because I didn't begin on 00:00 on the 11th, but 31*24*60*60 Seconds before the last post on the subreddit and post is after the last post


# saving ------------------------------------------------------------------

write.csv(tibble_all,file = filepath[[6]])



############################################
# Comments Pre-Ban ----------------------------------------------------
############################################


companion()






tibble_all <- read.csv(filepath[[3]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsCOMMENTSBEFORE_BANCompleter.csv") #Yes, there are 4 variables missing. I don't know why, but important ones are still there and plausible.
tibble_all <- add_row(tibble_all, tibble_all_extended)

to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 


post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))

post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))

          
view(post_per_author)


hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

bad_accounts_detector_comments(tibble_all, samplesize = 5, max_var = 50)

bad_accounts_append(watchlist_comments$watchlist)  



bad_accounts_append(list(post_per_author$author[post_per_author$n>2000]))
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames))
s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))

view(post_per_author[post_per_author$n>=50])

post_per_author$author[post_per_author$n >=50& post_per_author$nchar_var <=100]


bad_accounts_append(list("quadruple-snake-anal","Title2ImageBot","lerobinbot","AnotherCakeDayBot","simp-finder","theHelperdroid"))
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames))



tibble_all$body[tibble_all$author == "simp-finder"] 




#adding column date.time #gaps in data -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")




hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) #Zwischen dem zwischen 17. und 28. Fehlen die Posts, der erste Tag is shorter because I didn't begin on 00:00 on the 11th, but 31*24*60*60 Seconds before the last post on the subreddit and post is after the last post


# saving ------------------------------------------------------------------
 
write.csv(tibble_all,file = filepath[[7]])







############################################
# Comments Post-Ban ----------------------------------------------------
############################################


companion()


# comments AFter Ban --------------------------------------------------



tibble_all <- read.csv(filepath[[4]])
tibble_all_extended <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/shortcelsselected_authorsCOMMENTSAFTER_BANCompleter.csv") #Yes, there are 4 variables missing. I don't know why, but important ones are still there and plausible.
tibble_all <- add_row(tibble_all, tibble_all_extended)
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 


post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))

post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))


view(post_per_author)



hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")





bad_accounts_detector_comments(tibble_all, samplesize = 5, max_var = 100)

watchlist_comments <- NA

tibble_all$body[tibble_all$author == "JO82818493"] 


tibble_all$body[tibble_all$author == "BandMan69"]



#adding column date.time + #gaps in data -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")





hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) 

# saving ------------------------------------------------------------------

write.csv(tibble_all, file =filepath[[8]])

companion()




