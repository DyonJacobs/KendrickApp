
#all albums
nrc_plot <- lyrics_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 2000)) + #Hard code the axis limit
  ggtitle("Kendrick Lyrics NRC Sentiment") +
  coord_flip()
plot(nrc_plot) 


getNRC <- function(albumName1, albumName2){
  lyrics_nrc_album1 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumName1), ]
  lyrics_nrc_album2 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumName2), ]
}
#album 1
nrc_plot1 <- lyrics_nrc_album1 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 500)) + #Hard code the axis limit
  ggtitle("Overly Dedicated Sentiment") +
  coord_flip()
plot(nrc_plot1) 
#album 2
nrc_plot2 <- lyrics_nrc_album2 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 500)) + #Hard code the axis limit
  ggtitle("Section.80 Sentiment") +
  coord_flip()
plot(nrc_plot2) 
#album 3
nrc_plot3 <- lyrics_nrc_album3 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 750)) + #Hard code the axis limit
  ggtitle("Good Kid MAAD City Sentiment") +
  coord_flip()
plot(nrc_plot3) 
#album 4
nrc_plot4 <- lyrics_nrc_album4 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 750)) + #Hard code the axis limit
  ggtitle("To Pimp A Butterfly Sentiment") +
  coord_flip()
plot(nrc_plot4) 
#album 5
nrc_plot5 <- lyrics_nrc_album5 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 500)) + #Hard code the axis limit
  ggtitle("Untitled Unmastered Sentiment") +
  coord_flip()
plot(nrc_plot5) 
#album 6
nrc_plot6 <- lyrics_nrc_album6 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 500)) + #Hard code the axis limit
  ggtitle("DAMN. Sentiment") +
  coord_flip()
plot(nrc_plot6) 

#function to make bar plot of angry words in album inserted
plotBarSentiment <- function(album){
  #Count sentiment values for this album
  sentimentCountAlbum <- album %>% group_by(track_number, sentiment) %>% summarize(count=n())
  #Only keep the anger sentiment values
  sentimentCountAlbumAngry <- sentimentCountAlbum[sentimentCountAlbum$sentiment == "anger",]
  barplot(sentimentCountAlbumAngry$count, width = .2, xlim = c(0,3), xlab = "Track #", main = "angry words per track", names.arg = sentimentCountAlbumAngry$track_number , col = "red")
  
}
plotBarSentiment(lyrics_nrc_album1)

plotBarSentimentAll <- function(album){
  #Count sentiment values for this album
  sentimentCountAlbum <- album %>% group_by(album_name, sentiment) %>% summarize(count=n())
  #Only keep the anger sentiment values
  sentimentCountAlbumAngry <- sentimentCountAlbum[sentimentCountAlbum$sentiment == "anger",]
  barplot(sentimentCountAlbumAngry$count, width = .2, xlim = c(0,2), xlab = "Album Name", main = "Angry Words Per Album", names.arg = sentimentCountAlbumAngry$album_name , col = "red")
  
}

plotBarSentimentAll(lyrics_nrc)  

getSentiment <- function(sentiment){
sentimentCountAlbum <- lyrics_nrc %>% group_by(album_name, sentiment) %>% summarize(count=n())
sentimentCountAlbum <- sentimentCountAlbum[sentimentCountAlbum$sentiment == sentiment,]
}

getSentimentSpecific <- function(sentiment, albumName){
  lyrics_nrc_albumKek <- lyrics_nrc[ which(lyrics_nrc$album_name== albumName), ]
  sentimentCountAlbum <- lyrics_nrc_albumKek %>% group_by(track_number, sentiment, album_name) %>% summarize(count=n())
  sentimentCountAlbum <- sentimentCountAlbum[sentimentCountAlbum$sentiment == sentiment,]
}
eoeorowerowoer1<- getSentimentSpecific("joy", "Overly Dedicated")
eoeorowerowoer2 <- rbind(eoeorowerowoer1, eoeorowerowoer)

barplot(eoeorowerowoer1$count, 
        width = .2, 
        ylim = c(0,max(eoeorowerowoer1$count)), 
        xlim = c(0,4), 
        xlab = "Track Number", 
        main = eoeorowerowoer1$album_name[1], 
        names.arg = eoeorowerowoer1$track_number , 
        col = color)


library(tidyverse)
library(gapminder)
theme_set(theme_bw())

ggplot(rbind(eoeorowerowoer1, eoeorowerowoer), aes(fill = eoeorowerowoer2$album_name, y = eoeorowerowoer2$count, x = eoeorowerowoer2$track_number)) +
  labs( x = "Track Number", y = "Relative Occurence Sentiment", title = "Comparison Relative Occurence Sentiment",
        subtitle = "Relative in Percentages") +
  geom_bar(position = "fill", stat = "identity", show.legend = NA) +
  labs(fill = "Album Name")

getColor <- function(sentiment){
  if(sentiment == "positive" || sentiment == "anticipation" || sentiment == "joy" || sentiment == "trust" || sentiment == "surprise")
    { color = "cyan"}
  else {color = "red"}
}


  
#including duplicate words get total amount of words
word_summary_album <- lyrics_nrc[which(lyrics_nrc$album_name == "Overly Dedicated"),] %>%
  group_by(track_number, album_name) %>%  #add ,track_name for per track overview
  mutate(word_count = length(word)) %>%
  select(track_number, album_name = album_name, word_count) %>% #add track_name for per track overview returned
  distinct() %>% 
  ungroup()

#Get total amount of words of a certain sentiment
sentiment_count_album <- lyrics_nrc[ which(lyrics_nrc$album_name== "Overly Dedicated"), ] 
sentiment_count_album <- sentiment_count_album %>% group_by(track_number, sentiment, album_name) %>% summarize(count=n())
sentiment_count_album <- sentiment_count_album[sentiment_count_album$sentiment == "positive",]

#Combine the two to get the total number of words and the total number of a certain sentiment
topkek <- cbind(word_summary_album$word_count, sentiment_count_album$count, word_summary_album$track_number)
topkek <- as.data.frame(topkek)
names(topkek) <- c("word_count", "sentiment_count","track_number")
sentimentPercentage <- transform(topkek, sentiment_percentage = word_count / sentiment_count)


#FUNCTION TO GET PERCENTAGE OF A SENTIMENT
getSentimentPercentage <- function(sentiment1, albumname){
word_summary_album <- lyrics_nrc[which(lyrics_nrc$album_name == albumname),] %>%
  group_by(track_number, album_name) %>%  #add ,track_name for per track overview
  mutate(word_count = length(word)) %>%
  select(track_number, album_name = album_name, word_count) %>% #add track_name for per track overview returned
  distinct() %>% 
  ungroup()

#Get total amount of words of a certain sentiment
sentiment_count_album <- lyrics_nrc[ which(lyrics_nrc$album_name== albumname), ] 
sentiment_count_album <- sentiment_count_album %>% group_by(track_number, sentiment, album_name) %>% summarize(count=n())
sentiment_count_album <- sentiment_count_album[sentiment_count_album$sentiment == sentiment1,]

#Combine the two to get the total number of words and the total number of a certain sentiment
topkek <- cbind(word_summary_album$album_name, word_summary_album$word_count, sentiment_count_album$count, word_summary_album$track_number)
topkek <- as.data.frame(topkek)
names(topkek) <- c("album_name","word_count", "sentiment_count","track_number")
topkek$word_count <- as.numeric(as.character(topkek$word_count))
topkek$sentiment_count <- as.numeric(as.character(topkek$sentiment_count))
topkek <- transform(topkek, sentiment_percentage = sentiment_count / word_count * 100)
}


test123 <- getSentimentPercentage("joy","Section.80")
test456 <- getSentimentPercentage("joy","Overly Dedicated")
test789 <- rbind(test123, test456)
test789$track_number <- as.numeric(test789$track_number)
  ggplot(test789, aes(fill = test789$album_name, y = test789$sentiment_percentage, x = test789$track_number))+ 
  labs( x = "Track Number", y = "Relative Occurence Sentiment", title = "Comparison Relative Occurence Sentiment",
        subtitle = "Relative in Percentages") +
  geom_bar(position = "fill", stat = "identity", show.legend = NA) +
    theme(legend.text = element_text(size = 10, face = "bold"), legend.title = element_text(size = 15, face = "bold")) +
  labs(fill = "Album Name")
