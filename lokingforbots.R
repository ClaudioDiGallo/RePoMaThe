
source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


#Below is for the chapter "Botlist"

q <- read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/incelswithouthateselective_authoriansSUBMISSIONS_20210310_86400.csv")
q <- q %>% mutate(colour= ifelse(author == "nsfw_celbs","navyblue",ifelse(author=="AutoModerator","darkgreen","red")))
q <- q %>% mutate(date = anytime(created_utc))

q$color


ggplot(q) + geom_histogram(binwidth = 5000, aes(x=q$date,  fill=colour ))+
  scale_fill_identity(labels=c("AutoModerator","nsfw_celbs", "andere Accounts"),guide = "legend")+
  ggtitle("Histogramm Posts von in r/incelwithouthate aktiven Accounts innerhalb eines Tages") +
  xlab("Zeit")+
  ylab("Anzahl Posts")+
  theme()


ggsave("C:/Users/Claudio/Desktop/Masterarbeit Code/images/spam.png")


ggplot(q) + geom_bar(aes(x=q$colour,  fill=colour ))+
  scale_fill_identity(labels=c("AutoModerator","nsfw_celbs", "andere Accounts"),guide = "legend")+
  ggtitle("Histogramm Posts von in r/incelwithouthate aktiven Accounts innerhalb eines Tages") +
  xlab("Account")+
  ylab("Anzahl Posts")+
  theme()

ggsave("C:/Users/Claudio/Desktop/Masterarbeit Code/images/spam_barplot.png")

unique(q$author) #dies sind die Anzahl anderer Autoren plus 2


sum(q$author == "nsfw_celbs") #wieviele Posts kommen von diesem account?

sum(q$author == "AutoModerator")

length(q$author)-sum(q$author == "AutoModerator")-sum(q$author == "nsfw_celbs") #Anzahl Posts von allen ACcounts ausser den zwei aktivsten

sum(q$author %>% count(subreddit))

q %>% filter(author=="nsfw_celbs") %>% count(subreddit) %>% str() #we get a tbl which is 19999 long, that means the account posted on that many subreddits

q$title[q$author == "nsfw_celbs"]
             
