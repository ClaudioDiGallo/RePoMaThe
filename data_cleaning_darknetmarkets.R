# #DataSanitation Me_IRA -----------------------------------------



source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


#generate filepath object-list

subreddit <- "darknetmarkets"
filepath <- filepathgenerator(subreddit) #put here the name of the sub to be cleansed

companion()

# Submissions Before Ban + DAta which has been re-scraped, because the limit in the account finder was to low --------------------------------------------------



tibble_all <- read.csv(filepath[[1]])


to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 

post_per_author <- tibble_all %>% count(author)
tbl_variance <- tibble_all %>% summarise(groups=author,title) %>% mutate(nchar=nchar(title)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))

post_per_author <- left_join(post_per_author, tbl_variance, by=c("author"="groups"))


hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account for Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

view(post_per_author)




bad_accounts_detector_posts(tibble_all = tibble_all, samplesize = 5, max_var = 30) #if this results in an error, its because there were no suspicious accounts found. You can check by increasing the max_var-Argument


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

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 



post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,title) %>% mutate(nchar=nchar(title)) %>% group_by(groups) %>% summarise(nchar_var = var(nchar))
post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))

hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")
view(post_per_author)


bad_accounts_detector_posts(tibble_all, samplesize = 10) #if it fails, it returns a list of length 0: that means there are no suspiocous accounts


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


# Submissions AFter Ban --------------------------------------------------



tibble_all <- read.csv(filepath[[3]])
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 


post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))

post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))


view(post_per_author)


hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")



bad_accounts_detector_comments(tibble_all, samplesize = 15)





bad_accounts_append(list(post_per_author$author[post_per_author$n>2000]))
bad_accounts_append(list("societybot"))
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames))
s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))
view(post_per_author)

#adding column date.time #gaps in data -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")




hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Kommentare pro Tag", col="darkgreen", xlab= "Datum", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) #Zwischen dem zwischen 17. und 28. Fehlen die Posts, der erste Tag is shorter because I didn't begin on 00:00 on the 11th, but 31*24*60*60 Seconds before the last post on the subreddit and post is after the last post


# saving ------------------------------------------------------------------

write.csv(tibble_all,file = filepath[[7]])







############################################
# Comments Post-Ban ----------------------------------------------------
############################################


companion()


# Submissions AFter Ban --------------------------------------------------



tibble_all <- read.csv(filepath[[4]])
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 


post_per_author <- tibble_all %>% count(author)
s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))

post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))


view(post_per_author)



hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Kommentare pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")





bad_accounts_detector_comments(tibble_all, samplesize = 20)

bad_accounts_append(list(post_per_author$author[post_per_author$n>2000]))

to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames))

s <- tibble_all %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
post_per_author <- left_join(post_per_author, s, by=c("author"="groups"))
view(post_per_author)

tibble_all$body[tibble_all$author == "PersnickeyPants"] 


tibble_all$body[tibble_all$author == "BandMan69"]



#adding column date.time + #gaps in data -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")





hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl kommentare pro Tag", col="darkgreen", xlab= "Datum", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) 

# saving ------------------------------------------------------------------

write.csv(tibble_all, file =filepath[[8]])

companion()
