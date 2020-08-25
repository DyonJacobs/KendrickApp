WS_words <- tidy_lyrics %>%
  group_by(track_number, track_name, album_name) %>%  #add ,track_name for per track overview
  filter(!album_name == "NA") %>%
  mutate(word_count = length(word)) %>%
  select(track_number, track_name, album_name = album_name, word_count) %>% #add track_name for per track overview returned
  distinct() %>% 
  ungroup()

WS <- WS_words
WS <- merge(WS, ArtistComplete[ ,c("track_name", "duration_ms")], by = "track_name")
WS <- transform (WS, duration_seconds = duration_ms / 1000)
WS <- WS[-c(5)]
WS <- transform(WS, WPS = word_count / duration_seconds )
WS$WPS <- format(WS$WPS, digits = 2)

ggplot(WS) +
  geom_point(aes(x=duration_seconds, y = WPS, colour = album_name), shape = 20, size = 5) +
  labs( x = "Track Duration", y = "Words Per Second", title = "Rap Speed Scatter Plot",
        subtitle = "In Seconds", colour = "Album Name") +
  theme(axis.title.x = element_text(size = 15),
        title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 15),
        legend.background = element_rect(colour = "grey", linetype = 3),
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 20, face = "bold"))



#Sentiment Percentage Album------------------------------------------------------
sentiment <- "joy"
word_summary_album <- lyrics_nrc %>%
  group_by(track_number, album_name) %>%  #add ,track_name for per track overview
  mutate(word_count = length(word)) %>%
  select(track_number, album_name = album_name, word_count) %>% #add track_name for per track overview returned
  distinct() %>% 
  ungroup()

GetSentimentCareer <- function(sentiment){
#Get total amount of words of a certain sentiment
sentiment_count_album <- lyrics_nrc %>% group_by(track_number, sentiment, album_name) %>% summarize(count=n())
sentiment_count_album <- sentiment_count_album[sentiment_count_album$sentiment == sentiment,]

#Combine the two to get the total number of words and the total number of a certain sentiment
topkek <- cbind(word_summary_album$album_name, word_summary_album$word_count, sentiment_count_album$count, word_summary_album$track_number)
topkek <- as.data.frame(topkek)
names(topkek) <- c("album_name","word_count", "sentiment_count","track_number")
topkek$word_count <- as.numeric(as.character(topkek$word_count))
topkek$sentiment_count <- as.numeric(as.character(topkek$sentiment_count))
topkek <- transform(topkek, sentiment_percentage = sentiment_count / word_count * 100)

totalAlbums <- c("Overly Dedicated", "Section.80","good kid, m.A.A.d city", "To Pimp A Butterfly", "untitled unmastered.","DAMN. COLLECTORS EDITION.")
g <- topkek %>%
  group_by(album_name) %>%
  summarise(
    var1 = mean(sentiment_percentage)
  ) %>%
  slice(match(totalAlbums, album_name))
  

}

x3 <- factor(g$album_name, levels=c("Overly Dedicated", "Section.80","good kid, m.A.A.d city", "To Pimp A Butterfly", "untitled unmastered.","DAMN. COLLECTORS EDITION."))

ggplot(g) +
  geom_line(aes(x3, var1), group = 1, size = 1.5, colour = "orange") +
  ylim(min(g$var1) - 3 , max(g$var1)+5) +
  labs( x = "Album Name", y = "Percentage Lyrics of Sentiment", title = "Sentiment Values over Career",
        subtitle = "%") +
  theme(axis.title.x = element_text(size = 15),
        title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 15))



hey <- GetSentimentCareer("negative")

