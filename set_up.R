
# set up dataframes of scores

#libraries:
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(RColorBrewer)

# load data
df = read.csv("VALUE 1-4.csv")
year_2 = read.csv("DRAM 200 Value Rubric Data (Yr 2).csv")

# 1st year
dram_1 <- df %>% 
  filter(Program == 1 & Year == 1) %>% # first year dram
  select(-c(Student_ID, Program, Course_sample))  #take out unnecessary columns

n_1 <- nrow(dram_1)
dram_1 <- dram_1 %>%  summarise_each(funs(mean(., na.rm = TRUE))) # take average of each column

dram_4 <- df %>% 
  filter(Program == 1 & Year == 4) %>% # first year dram
  select(-c(Student_ID, Program, Course_sample)) #take out unnecessary columns

n_4 <- nrow(dram_4)
dram_4 <- dram_4 %>%  summarise_each(funs(mean(., na.rm = TRUE))) # take average of each column

## second year
year_2 <- year_2[ , -grep("Change", colnames(year_2))] #remove change columns

# average the 2 markers' scores for second year
score_1 <- year_2 %>% select(PS1.1:WC5.1)
score_2 <- year_2 %>% select(PS1.2:WC5.2)
colnames(score_2) <- colnames(score_1) # change column names for row bind

n_2 <- nrow(score_1) #sample size for second year

dram_2 <- bind_rows(score_1, score_2) %>% #second year data
  summarise_each(funs(mean(., na.rm = TRUE))) %>% # take average of each column
  cbind(Year = 2, .) # add year column at start

#reorder 2nd year and change names to match year 1 and 4 data for row bind:
dram_2 <- dram_2[c("Year", "PS1.1", "PS2.1", "PS3.1", "PS4.1", "PS5.1", "PS6.1", "CT1.1", "CT2.1", "CT3.1", "CT4.1", "CT5.1", "WC1.1", "WC2.1", "WC3.1", "WC4.1", "WC5.1")]
colnames(dram_2) <- colnames(dram_1)


#combine into one data frame:
dram <- bind_rows(dram_1, dram_2, dram_4)

# separate by VALUE:

ps <- dram %>% 
  subset(select = Year:PS6) %>% 
  gather(learning_outcome, mean, PS1:PS6, na.rm = TRUE)

ct <-  dram %>% 
  subset(select = c(Year, CT1:CT5)) %>% 
  gather(learning_outcome, mean, CT1:CT5, na.rm = TRUE)

wc <- dram %>% 
  subset(select = c(Year, WC1:WC5)) %>% 
  gather(learning_outcome, mean, WC1:WC5, na.rm = TRUE)


# summary:

#take average over all the subcategories for each outcome
ps_avg <- ps %>% spread(learning_outcome, mean) 
ps_avg <- data.frame(Year = ps_avg[,1], mean = rowMeans(ps_avg[,-1], na.rm = TRUE))
colnames(ps_avg) <- c("Year", "PS")

ct_avg <- ct %>% spread(learning_outcome, mean) 
ct_avg <- data.frame(Year = ct_avg[,1], mean = rowMeans(ct_avg[,-1], na.rm = TRUE))
colnames(ct_avg) <- c("Year", "CT")

wc_avg <- wc %>% spread(learning_outcome, mean) 
wc_avg <- data.frame(Year = wc_avg[,1], mean = rowMeans(wc_avg[,-1], na.rm = TRUE))
colnames(wc_avg) <- c("Year", "WC")

#combine averages into one data frame and tidy
summary <- cbind(ct_avg, wc_avg) %>% #combine columns but not problem solving
  subset(select = c("Year", "CT", "WC")) %>% #take out repeated course_sample column
  gather(learning_outcome, mean, CT:WC, na.rm = TRUE) #tidy


# x axis labels with sample sizes
n_3 <- 0  


year1 <- paste0("First Year\nn = ", n_1) #text string for xlabel including sample size    
year2 <- paste0("Second Year\nn = ", n_2) #text string for xlabel   
year3 <- paste0("Third Year\nn = ", n_3) #text string for xlabel    
year4 <- paste0("Fourth Year\nn = ", n_4) #text string for xlabel

