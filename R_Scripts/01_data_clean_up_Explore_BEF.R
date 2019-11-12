#### The goal of this code is to analysis the keywords that are used to describe
#### papers that cite a key BEF review paper with keywords that are used to
#### describe papers that cite a key BES review paper. I hope to better 
#### understand what used are commonly used by both and which keywords are 
#### more associated with BES or BEF papers

## Setting the working directory

library(tidyverse)
library(dplyr)

setwd("~/Desktop/Current_Projects/BES_BEF_Keywords")

## lets get that raw data in here

bef.df <- read.csv("./Data/Raw_Data/BEF_Text.csv")

bes.df <- read.csv("./Data/Raw_Data/Kreman_Text.csv")

## Ok now lets melt this data

bef.df <- pivot_longer(bef.df, cols = starts_with("Keyword"),
                     names_to ="Rank",
                    values_to= "Keyword",
                     values_drop_na = TRUE )

dim(bef.df) # 42000 x 6

## removing nas

bef.df <- bef.df[complete.cases(bef.df),]

dim(bef.df) # 41846 x 6

## removing blanks

remove <-bef.df$Keyword[7] # blank

bef.df <- filter(bef.df , Keyword != remove )

dim(bef.df)  # 12812 x 6

bef.df$Keyword <- toupper( bef.df$Keyword)

bef.df$Field <- "BEF" 


### ok now lets do BES

bes.df <- pivot_longer(bes.df, cols = starts_with("Keyword"),
                       names_to ="Rank",
                       values_to= "Keyword",
                       values_drop_na = TRUE )

dim(bes.df) # 10992 x7

## removing nas

bes.df <- bes.df[complete.cases(bes.df),]

dim(bes.df) # 10992 X 7

## removing blanks

remove <-bes.df$Keyword[6] # blank

bes.df <- filter(bes.df , Keyword != remove )

dim(bes.df)  # 3342 X 7


bes.df$Keyword <- toupper( bes.df$Keyword)

bes.df <- select( bes.df, c("Title" ,"Journal", "ID" ,"Year" ,"Rank",
                            "Keyword" ))

bes.df$Field <- "BES" 

## combinded dataframes 
keyword.df <- rbind.data.frame(bes.df, bef.df)

#write.csv( keyword.df, file = "keyword.csv")



### ok lets paint ourselves a word cloud

# first step is to build a count for all the keywords
dim(keyword.df) ## 16254 x 7

sum.key.df <- keyword.df %>%
              group_by(Keyword, Field ) %>%
              summarise(
                count = n()
              )

dim(sum.key.df) ## now 9073 x 3

sum.bes.df <- filter( sum.key.df, Field == "BES")

dim(sum.bes.df)

simp.bes.df <- filter( sum.bes.df, count > 1)

dim(simp.bes.df)


sum.bef.df <- filter( sum.key.df, Field == "BEF")

dim(sum.bef.df)

simp.bef.df <- filter( sum.bef.df, count > 5)

dim(simp.bef.df)


### 

full.simp.df <- rbind.data.frame( sum.bef.df, sum.bes.df)


install.packages("ggwordcloud")

library(ggwordcloud)

ggplot(sum.bef.df, aes(label = Keyword, size=count, color=Field)) +
  geom_text_wordcloud() + scale_color_brewer(palette = "Set1")+
  facet_wrap(~Field)+
  geom_text_wordcloud(area_corr = TRUE) 


wordcloud(words = simp.bef.df$Keyword, freq = simp.bef.df$count,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = simp.bes.df$Keyword, freq = simp.bes.df$count/100,
          max.words = 100,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



