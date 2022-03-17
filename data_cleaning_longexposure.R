
# data cleaning long exposure ---------------------------------------------





source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


#generate filepath object-list


subreddit <- "longexposure"
companion()

# Submissions Before Ban --------------------------------------------------

tibble_all <- read.csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/INCEL_ACCOUNTSselected_authorsSUBMISSIONSLONGEXPOSURE.csv")

to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")

# Studying the Accounts and removing spammers/bots ---------------------------------------------------


tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) #if row is not in forbidden list, put it in sanitized tibble 


post_per_author <- tibble_all %>% count(author)
hist(post_per_author$n, breaks = 30, main= paste("Histogramm der Frequenz der Accounts über Anzahl Posts pro Account for Ban des Subreddits", subreddit), xlab="Anzahl Posts pro Author",
     ylab = "Anzahl Accounts")



bad_accounts_detector_posts(tibble_all = tibble_all, samplesize = 15, max_var = 15) #if this results in an error, its because there were no suspicious accounts found. You can check by increasing the max_var-Argument


bad_accounts_append(list("74538")) 

to_be_deleted <- read.csv("C:/Users/Claudio/Desktop/Masterarbeit Code/botlist.csv")
tibble_all <-  filter(tibble_all, !(tibble_all$author %in%  to_be_deleted$Botnames)) 

tibble_all$title[tibble_all$author == "HurricaneDorian321"]

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

write.csv(tibble_all, file = "E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/INCEL_ACCOUNTSselected_authorsSUBMISSIONSLONGEXPOSURE_SANITIZED.csv")



