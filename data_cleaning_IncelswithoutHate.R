


# #BotDetection IncelsWithoutHate -----------------------------------------



source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


#generate filepath object-list

filepath <- filepathgenerator(subreddit="incelswithouthate") #put here the name of the sub to be cleansed
subreddit <- "incelswithouthate"
companion()

# Submissions Before Ban --------------------------------------------------



tibble_all <- read.csv(filepath[[1]])
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 


post_per_author <- tibble_all %>% count(author)
hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account for Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

view(post_per_author)

bad_accounts_detector_posts(tibble_all = tibble_all, samplesize = 15, max_var = 15) #if this results in an error, its because there were no suspicious accounts found. You can check by increasing the max_var-Argument


bad_accounts_append(list("autotldr")) #This is a bot reddit.com/u/autotldr
bad_accounts_append(list(watchlist_comments$watchlist$groups))
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) 

#adding column date.time -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")

# more general sanity-tests -----------------------------------------------


#how much time between pushshift and real posting in hours
median(tibble_all$retrieved_on-tibble_all$created_utc)/3600
mean(tibble_all$retrieved_on-tibble_all$created_utc)/3600
max(tibble_all$retrieved_on-tibble_all$created_utc)/3600

# plot1 <- plot(tibble_all$date, (tibble_all$retrieved_on-tibble_all$created_utc)/3600, ylab= "Lag der Pushshift-Server (Stunden)", xlab= "Datum der Posts auf Reddit, 2021", main= "Zeit zwischen Reddit-Post und Pushshifts' Download r/IncelsWithoutHate")
# plot1
# png(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/images/LagPUshshift",subreddit,".png"))
# plot1 <- plot(tibble_all$date, (tibble_all$retrieved_on-tibble_all$created_utc)/3600, ylab= "Lag der Pushshift-Server (Stunden)", xlab= "Datum der Posts auf Reddit, 2021", main= "Zeit zwischen Reddit-Post und Pushshifts' Download r/IncelsWithoutHate")
# dev.off()



hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day)

# saving ------------------------------------------------------------------

write.csv(tibble_all, file = filepath[[5]])
            


############################################
# The same but Post-Ban ----------------------------------------------------
############################################


companion()


# Submissions AFter Ban --------------------------------------------------



tibble_all <- read.csv(filepath[[2]])
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 


post_per_author <- tibble_all %>% count(author)
hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

view(post_per_author)

bad_accounts_detector_posts(tibble_all, samplesize = 10)


#Spammers or bots?
ifelse(max(post_per_author$n) >= 10000, print("spammers or bots"), print("maybe no big spammers or bots"))


#adding column date.time -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")

# more general sanity-tests -----------------------------------------------



#how much time between pushshift and real posting in hours
median(tibble_all$retrieved_on-tibble_all$created_utc)/3600
mean(tibble_all$retrieved_on-tibble_all$created_utc)/3600
max(tibble_all$retrieved_on-tibble_all$created_utc)/3600



# plot(tibble_all$date, (tibble_all$retrieved_on-tibble_all$created_utc)/3600, ylab= "Lag der Pushshift-Server", xlab= "Datum, 2021", main= "Zeit zwischen Reddit-Post und Pushshifts' Download")



#gaps in data


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
hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

view(post_per_author)

tibble_all

var(nchar(tibble_all$body[tibble_all$author == "dadbot_3000"]))
tibble_all$body[tibble_all$author == "dadbot_3000"] #https://www.reddit.com/user/dadbot_3000
#Datbot ist ganz sicher ein Bot, aber es wäre sonst gar nicht so leicht, dass herauszufinden

var(nchar(tibble_all$body[tibble_all$author == "epic_gamer_4268"]))
tibble_all$body[tibble_all$author == "epic_gamer_4268"]

bad_accounts_detector_comments(tibble_all, samplesize = 10, max_var = 50)

bad_accounts_append(list("dadbot_3000"))
bad_accounts_append(list(watchlist_comments$watchlist$groups))
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 

# #Spammers or bots?
# ifelse(max(post_per_author$n) >= 10000, print("spammers or bots"), print("maybe no big spammers or bots"))
# 
# mapply(ifelse((post_per_author$n) >= 10000, )
# s <- tibble_all %>% filter(author %in% q$author)  %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
#        
# 

#adding column date.time -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")

# more general sanity-tests -----------------------------------------------


#how much time between pushshift and real posting in hours
median(tibble_all$retrieved_on-tibble_all$created_utc)/3600
mean(tibble_all$retrieved_on-tibble_all$created_utc)/3600
max(tibble_all$retrieved_on-tibble_all$created_utc)/3600



# plot(tibble_all$date, (tibble_all$retrieved_on-tibble_all$created_utc)/3600, ylab= "Lag der Pushshift-Server", xlab= "Datum, 2021", main= "Zeit zwischen Reddit-Post und Pushshifts' Download")




#gaps in data


hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) #Zwischen dem zwischen 17. und 28. Fehlen die Posts, der erste Tag is shorter because I didn't begin on 00:00 on the 11th, but 31*24*60*60 Seconds before the last post on the subreddit and post is after the last post


# saving ------------------------------------------------------------------
Submissions_tbl <- tibble_all 
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
hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account nach Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")

view(post_per_author)



bad_accounts_detector_comments(tibble_all, samplesize = 10, max_var = 50)


var(nchar(tibble_all$body[tibble_all$author == "dadbot_3000"]))
tibble_all$body[tibble_all$author == "decomposingbody"] #https://www.reddit.com/user/dadbot_3000
#Datbot ist ganz sicher ein Bot, aber es wäre sonst gar nicht so leicht, dass herauszufinden

var(nchar(tibble_all$body[tibble_all$author == "epic_gamer_4268"]))
tibble_all$body[tibble_all$author == "epic_gamer_4268"]



bad_accounts_append(list("image_linker_bot"))
to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 

# #Spammers or bots?
# ifelse(max(post_per_author$n) >= 10000, print("spammers or bots"), print("maybe no big spammers or bots"))
# 
# mapply(ifelse((post_per_author$n) >= 10000, )
# s <- tibble_all %>% filter(author %in% q$author)  %>% summarise(groups=author,body) %>% mutate(nchar=nchar(body)) %>% group_by(groups) %>%   summarise(nchar_var = var(nchar))
#        
# 

#adding column date.time -------------------------------------------------


tibble_all <- tibble_all %>% select_all()  %>% mutate(date = anytime(created_utc))
tibble_all <- tibble_all %>% select_all()  %>% mutate(day = as.Date.POSIXct(date), tryFormats = "%m/%d")

# more general sanity-tests -----------------------------------------------


#how much time between pushshift and real posting in hours
median(tibble_all$retrieved_on-tibble_all$created_utc)/3600
mean(tibble_all$retrieved_on-tibble_all$created_utc)/3600
max(tibble_all$retrieved_on-tibble_all$created_utc)/3600


# 
# plot(tibble_all$date, (tibble_all$retrieved_on-tibble_all$created_utc)/3600, ylab= "Lag der Pushshift-Server", xlab= "Datum, 2021", main= "Zeit zwischen Reddit-Post und Pushshifts' Download")
# 



#gaps in data


hist(tibble_all$date, breaks = 30,main = "Histogramm der Anzahl Posts pro Tag", col="darkgreen", xlab= "Datum, 2021", ylab="Anzahl Posts per Epoch-time")# mISSING DATA
tibble_all %>% count(day) #Zwischen dem zwischen 17. und 28. Fehlen die Posts, der erste Tag is shorter because I didn't begin on 00:00 on the 11th, but 31*24*60*60 Seconds before the last post on the subreddit and post is after the last post


# saving ------------------------------------------------------------------

write.csv(tibble_all, file =filepath[[8]])

companion()

tibble_submissions <-  read.csv(filepath[[8]]) %>% select(c("author", "subreddit")) %>% unique()
tibble_comments <-  read.csv(filepath[[7]]) %>%  select(c("author", "subreddit")) %>% unique()
tibble_all <- add_row(tibble_comments, tibble_submissions) %>% unique()



