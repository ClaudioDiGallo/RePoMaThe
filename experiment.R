
#Experiment


source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")


#Constructing difference tbl -------------------------------------------------------------------------




#######################################################################
#######EXPERIMENT #########
#######################################################################


tbl <- read_csv2("C:/Users/Claudio/Desktop/Masterarbeit Code/experiment_combined.csv")


#everything
treatment <- tbl[tbl$treatment==TRUE,]
control <- tbl[tbl$treatment==FALSE,]


t.test(control$difference,treatment$difference)
?t.test


#constructing each

subreddits <- list("gendercritical","me_ira","braincels","shortcels")

for(subreddit in subreddits){
  tbl_subset <- tbl[tbl$sub==subreddit,]
  
  
  
  treatment <- tbl_subset[tbl_subset$treatment==TRUE,]
  control <- tbl_subset[tbl_subset$treatment==FALSE,]
  
  assign(paste0(subreddit,"_treatment"),treatment, envir =  .GlobalEnv)
  assign(paste0(subreddit,"_control"),control, envir =  .GlobalEnv)


}

#t-test braincels

t_test <- t.test(braincels_treatment$difference,braincels_control$difference, alternative="less")
sd_treatment <- sd(braincels_treatment$difference)
sd_control <- sd(braincels_control$difference)
n_treatment <-  length(braincels_treatment$difference)
n_control <- length(braincels_control$difference)
context <- tibble(sd_treatment,sd_control,n_treatment,n_control,t_test$stderr)
context
t_test


#t-test me_ira

t.test(me_ira_treatment$difference,me_ira_control$difference, alternative="less")
sd_treatment <- sd(me_ira_treatment$difference)
sd_control <- sd(me_ira_control$difference)
n_treatment <-  length(me_ira_treatment$difference)
n_control <- length(me_ira_control$difference)
context <- tibble(sd_treatment,sd_control,n_treatment,n_control,t_test$stderr)
context


#t-test gendercritical

t.test(gendercritical_treatment$difference,gendercritical_control$difference, alternative="less")
sd_treatment <- sd(gendercritical_treatment$difference)
sd_control <- sd(gendercritical_control$difference)
n_treatment <-  length(gendercritical_treatment$difference)
n_control <- length(gendercritical_control$difference)
context <- tibble(sd_treatment,sd_control,n_treatment,n_control,t_test$stderr)
context
#t-test shortcels

t.test(shortcels_treatment$difference,shortcels_control$difference, alternative="less")
sd_treatment <- sd(shortcels_treatment$difference)
sd_control <- sd(shortcels_control$difference)
n_treatment <-  length(shortcels_treatment$difference)
n_control <- length(shortcels_control$difference)
context <- tibble(sd_treatment,sd_control,n_treatment,n_control,t_test$stderr)
context



# 
# #writing to csv with key indicators:
# 
# t.test(paste0("braincels","_treatment$difference"),paste0("braincels","_control$difference"), alternative="less")
# paste0(braincels,_treatment$difference)
# 
# t.test(toString(paste0("braincels","_treatment$difference")))
# 
# for(subreddit in subreddits){
#   t_test <- t.test(paste0(subreddit,"_treatment$difference"),paste0(subreddit,"_control$difference"), alternative="less")
#   sd_treatment <- sd(paste0(subreddit,"_treatment$difference"))
#   sd_control <- sd(paste0(subreddit,"_control$difference"))
#   n_treatment <- nrow(paste0(subreddit,"_treatment$difference"))
#   n_control <- nrow(paste0(subreddit,"_control$difference"))
#   
#   
#   context <- tibble(sd_treatment,sd_control,n_treatment,n_control)
#   
# 
#   assign(paste0(subreddit,"_context"),context, envir =  .GlobalEnv)
#   assign(paste0(subreddit,"_t_test"),t_test, envir =  .GlobalEnv)
#   
#   
# }
# 
# 
# 
# 
# tbl <- tbl[tbl$sub=="braincels",]
# 
# treatment <- tbl[tbl$treatment==TRUE,]
# control <- tbl[tbl$treatment==FALSE,]
# 
# 
# tbl %>% filter_all(sub=="gendercritical")# %>% select(author,difference) %>% unique()
# unique_difference <- tbl %>% filter(sub=="braincels") %>% select(author,difference) %>% unique()
# hist(unique_difference)
# 
# 
# hist(unique_difference$difference, prob = TRUE, main = "Histogramm der Differenzen", xlab = "Differenz (Negative Werte = Abnahme der Aktivität nach dem Bann)",breaks=100)
# curve(dnorm(x, mean=mean(unique_difference$difference), sd=sd(unique_difference$difference)), add=TRUE, col="red")
# 
# ggplot( aes(unique_difference$difference)) +
#   geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
#   ggtitle("Bin size = 3") +
#   theme_ipsum() +
#   theme(
#     plot.title = element_text(size=15)
#   )