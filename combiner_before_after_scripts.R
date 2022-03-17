#Combining all the before/after activity scripts

source("C:/Users/Claudio/Desktop/Masterarbeit Code/functions.R")
source("C:/Users/Claudio/Desktop/Masterarbeit Code/Dependencies.R")



subreddits <- c("shortcels","braincels","gendercritical","me_ira")
subreddits


shortcels <- read_csv2(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/activity_difference",subreddits[1],".csv"))
braincels  <- read_csv2(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/activity_difference",subreddits[2],".csv"))
gendercritical  <- read_csv2(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/activity_difference",subreddits[3],".csv"))
me_ira <-  read_csv2(paste0("C:/Users/Claudio/Desktop/Masterarbeit Code/activity_difference",subreddits[4],".csv"))

combined <- add_row(shortcels,braincels)
combined <- add_row(combined,gendercritical)
combined <- add_row(combined,me_ira)

combined



write_csv2(combined,"C:/Users/Claudio/Desktop/Masterarbeit Code/combined.csv")

#t-test

t.test(x=combined$weighted_difference,y=combined$expected_loss, data=combined, alternative = "less")





combined_normalized <- combined %>% mutate(normed = weighted_difference-expected_loss)


 t.test(combined_normalized$normed,data=combined_normalized, alternative = "less")

 
 t.test(combined$activity_before,combined$activity_after, paired=TRUE)
 