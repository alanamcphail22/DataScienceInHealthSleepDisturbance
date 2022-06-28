### Data cleaning 


library(dplyr)
dat <- read.csv("project_data.csv")


# Making variables binary 
dat2 <- dat %>% 
  mutate(ESSBinary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0)) %>% 
  mutate(PSGQBinary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 5, 1, 0)) %>%
  mutate(AISBinary = ifelse(Athens.Insomnia.Scale > 5, 1, 0)) 

# Removing all NAs in those columns 
dat3 <- dat2 %>%
  filter(!is.na(ESSBinary), !is.na(PSGQBinary), !is.na(AISBinary), !is.na(Berlin.Sleepiness.Scale))

