
# Load Data ---------------------------------------------------------------



library(tidyverse)
incel_df <- read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/sub_incelswithouthate_20201231_2678400.csv")

# Unique Authors and Counting


unique_auth_df <- count(incel_df,incel_df$author )


unique_auth_df <- subset(unique_auth_df, unique_auth_df$`incel_df$author`!="[deleted]" )
unique_auth_df <- unique_auth_df %>%   mutate("name" =unique_auth_df$`incel_df$author`) %>% select(2:3)



#view(unique_auth_df)

write.csv(unique_auth_df, file = "E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/list_of_cels.csv")


# Aber hier random stuff --------------------------------------------------



victor <- read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/sub_incl_sub_auth_20210304_2678400.csv")

view(read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/sub_incelswithouthate_20201231_2678400.csv"))


