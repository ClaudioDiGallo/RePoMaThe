library(tidyverse)
library(anytime)


a <- read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/INCEL_ACCOUNTSselected_authorsSUBMISSIONSLONGEXPOSURE.csv")
c <- read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/braincelsCOMMENT_complete.csv")
d <- read_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/BraincelsSUBMISSION.csv")
f <- unique(append(c$author,d$author))
r <- unique(a$author)

write_csv("E:/Claudio/Documents/Masterarbeit_Daten/reddit_data/all_authors_longexposure.csv",)

