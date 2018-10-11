# Import data
library(xlsx)
library(svglite)
library(ggthemes)
library(tidyverse)

################# --------
# ############# # -------- table 1
################# --------

# Load data
hashtags <- read.xlsx("data/data_preprocessed.xlsx", sheetName = "Sheet1")

# Order factors
hashtags$Hashtag <- factor(x = hashtags$Hashtag, levels = hashtags$Hashtag[order(hashtags$Tweet.count)])

# Bar plot the number of tweets
hashtags %>% slice(1:nrow(.)-1) %>% # remove the total number of tweets
  ggplot(aes(x=Hashtag, y=Tweet.count, fill=Hashtag)) +
  geom_bar(stat="identity", width=0.8) +
  ggtitle("Collected Tweets per Hashtags") + xlab("Hashtags") + ylab("Count") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 24, face = "bold")) 

################# --------
# ############# # -------- table 4
################# --------

roles_human <- read.xlsx("data/data_preprocessed.xlsx", sheetName = "Sheet4")
roles_human$Role <- factor(x = roles_human$Role, levels = roles_human$Role[order(roles_human$Count)])

# Bar plot the number of tweets
roles_human %>% # remove the total number of tweets
  ggplot(aes(x=Role, y=Count, fill=Role)) +
  geom_bar(stat="identity", width=0.8) +
  ggtitle("Individual Accounts Coded") + xlab("Role") + ylab("Count") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 24, face = "bold")) 

################# --------
# ############# # -------- table 5
################# --------

roles_organization <- read.xlsx("data/data_preprocessed.xlsx", sheetName = "Sheet5")
roles_organization$Role <- factor(x = roles_organization$Role, levels = roles_organization$Role[order(roles_organization$Count)])

# Bar plot the number of tweets
roles_organization %>%
  ggplot(aes(x=Role, y=Count, fill=Role)) +
  geom_bar(stat="identity", width=0.8) +
  ggtitle("Organizational Accounts Coded") + xlab("Role") + ylab("Count") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 24, face = "bold")) 


################# --------
# ############# # -------- table 3 / opened conference
################# --------

opened <- read.xlsx("data/data_preprocessed.xlsx", sheetName = "Sheet3")
opened$Hashtag <- factor(x = opened$Hashtag, levels = opened$Hashtag[order(opened$Tweets)])

# Bar plot the number of tweets
opened %>%
  ggplot(aes(x=Hashtag, y=Tweets, fill=Hashtag)) +
  geom_bar(stat="identity", width=0.8) +
  ggtitle("Open Education Conference Tweet Frequencies by Year") + xlab("Open Education Conference by Year") + ylab("Count") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 16, face = "bold")) 


opened$Hashtag <- factor(x = opened$Hashtag, levels = opened$Hashtag[order(opened$Tweets.per.user)])

# Bar plot the number of tweets
opened %>%
  ggplot(aes(x=Hashtag, y=Tweets.per.user, fill=Hashtag)) +
  geom_bar(stat="identity", width=0.8) +
  ggtitle("Open Education Conference Tweet Frequencies per User by Year") + xlab("Open Education Conference by Year") + ylab("Count") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 16, face = "bold")) 


# Pie chart Male Female
data_gender_participation <- data.frame( Gender =  c("Male", "Female"), Participation = c(58.7 , 41.3) )
bp <- ggplot(data_gender_participation, aes(x = "", y = Participation, fill = Gender)) + geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y") + ggtitle("Participation by gender") +
  theme_solarized() + scale_colour_solarized() +
  theme(plot.title = element_text(size = 16, face = "bold")) 

