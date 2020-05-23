install.packages("ggplot2")
library(ggplot2)

setwd("/Users/nicolebohra/INFO371/Labs")
wvs<- read.delim("wvs.csv.bz2", header = TRUE)
summary(wvs)

ggplot(wvs) +
  geom_bar(aes(choice, y = ..prop.., fill = country), position = "dodge") +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "frequency", x = "how much freedom of choice do you feel?")

#Section 1: 

#1. 
model <- lm(choice~country, data = wvs)
summary(model)

#2. 
#American respondents feel 0.6 units more "freedom" than Chinese 
#respondents. In total, Americans are at a Freedom Index of 7.7 in 
#responding versus Chinese respondents having about an 
#index of 7.1.

#3
#The r2 value tells us that 2.5%  of the variance in the data can be explained by the model.

#SECTION 2

#1. 
model_sex <- lm(choice~country + sex, data = wvs)
summary(model_sex)

#2. 
#a) The level of choice freedom for American men would be about 7.73.
#b) The level of choice freedom for American women would be about 7.79.
#c) The level of choice freedom for Chinese men would be about 7.13.
#d) The level of choice freedom for Chinese women would be about 7.19.

#3.
model_3 <- lm(choice~sex*country, data = wvs)
summary(model_3)

#a) The level of choice freedom for American men would be about 6.54.
#b) The level of choice freedom for American women would be about 7.91.
#c) The level of choice freedom for Chinese men would be about 7.26.
#d) The level of choice freedom for Chinese women would be about 7.06.

# Based on the dataset, it seems that women are more free or have a higher 
# perceived freedom. However, it depends on what country we are looking at.
