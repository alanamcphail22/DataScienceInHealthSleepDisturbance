### Data cleaning # ty Alana

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

View(dat3)

########################################################

# Q2: What predictors do we need for the analysis?
# SF36.PCS & SF36.MCS

# Filtering for the response and predictors we need (plus unique identifier)
data_Q2 <- dat3[c("Subject","ESSBinary", "PSGQBinary", "AISBinary", "Berlin.Sleepiness.Scale", "SF36.PCS", "SF36.MCS")]

# Removing NA's from predictors
data_Q2 <- data_Q2 %>%
  filter(!is.na(SF36.PCS), !is.na(SF36.MCS))


