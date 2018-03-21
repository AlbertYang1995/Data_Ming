library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(mice)
library(randomForest)
train <- read.csv('D:\\workdir\\Titanic_train.csv', stringsAsFactors = F)
test  <- read.csv('D:\\workdir\\Titanic_test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test)
str(full)
str(full$Name)
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
table(full$Sex, full$Title)
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
full$Surname
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep = '_')
full$Family
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()
full$FsizeD[full$Fsize == 1]                 <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4]                  <- 'large'
mosaicplot(table(full$FsizeD, full$Survived), main = 'Family Size by Survival', shade = T)




























