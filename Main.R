library(spotifyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(extrafont)
library(textdata)
library(genius)
library(geniusr)
library(yarrr)

#getting an access token to be able to request data from Spotify
id <- '83ed6c8fe8b843b48729fbf7788fcca6'
secret <- 
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token = get_spotify_access_token()
genius_token <- 
genius_id = 'e_8ps-nUvUjxwfmoKXnfrYsBachgUdCm90ADDbpzsptcwAzXm4FdCwLUmaAjlUtT'
genius_secret 
#Create an ID for Kendrick Lamar and use it as parameter to run the GetDiscography function
ArtistID    <- 'Kendrick Lamar'

#Function to request the entire discography of an artist, sorted and all
GetDiscography <- function(ArtistID)
{
  #Request all audio features of every song done by Kendrick Lamar
  ArtistComplete <- get_artist_audio_features(ArtistID)
  
  #Remove all duplicate tracks within the collection
  #only keep unique track names, first entries get saved
  ArtistComplete <- ArtistComplete%>% distinct(ArtistComplete$track_name, .keep_all = TRUE)
  
  #Removing unnecessary columns
  ArtistComplete <- ArtistComplete[-c(2,3,4,7,8,20,23,25,28,29,31,33,34,40)]
  
  #Changing value of track numbers to vectors
  ArtistComplete$track_number <- as.vector(ArtistComplete$track_number)
  
  #Order by track number
  ArtistComplete <- ArtistComplete[order(ArtistComplete$track_number), ]
  
  #Order by album name
  ArtistComplete <- ArtistComplete[order(ArtistComplete$album_name), ]
  
  #Order albums by release date, oldest albums first
  ArtistComplete <- ArtistComplete %>% arrange(ArtistComplete$album_release_date)
  
  #Change order of columns to be more overseeable
  ArtistComplete <- ArtistComplete[c(21,20,23,3:14,16,18,19,24,25,26,2,17,15,22,1)]
  
  #Remove duplicate tracks from the dataset
  ArtistComplete <- ArtistComplete %>% distinct(ArtistComplete$track_number, ArtistComplete$album_name, .keep_all = TRUE)
  ArtistComplete <- ArtistComplete[-c(27,28)]
  
  #Get the rows of ArtistComplete I want for the complete albums overview
  Median_Albums <- ArtistComplete[c(3:17, 26)]
  #get median of the 3:15 columns
  Median_Albums <- aggregate(Median_Albums[c(3:15)], 
                             list(Median_Albums$album_name, Median_Albums$album_release_date, Median_Albums$artist_name), 
                             FUN = median)
# colnames(Median_Albums) = c("album_name", "release_date","danceability",
#                              "energy","key","loudness","mode","speechiness",
#                              "acousticness","instrumentalness", "liveness",
#                              "valence","tempo","time_signature","duration_ms")
  #Change names of the first 2 columns
  names(Median_Albums)[1:3] <-paste(c("album_name", "release_date", "artist_name"))
  #Only keep the year the album was released
  Median_Albums$release_date <- substr(Median_Albums$release_date, 0, 4)
  Median_Albums$release_date <- as.numeric(Median_Albums$release_date)
  #Remove duplicate albums caused by the SPotify library
  Median_Albums <- Median_Albums %>% distinct(Median_Albums$album_name, .keep_all = TRUE)
  Median_Albums <- Median_Albums[c(1:16)]
  Median_Artist <- aggregate(ArtistComplete[c(5:17)], list(ArtistComplete$artist_name), FUN = median)
  
  output <- list(ArtistComplete, Median_Albums, Median_Artist )
  return(output)
}


output <- GetDiscography(ArtistID)

ArtistComplete  <- as.data.frame(output[1])
MedianAlbums    <- as.data.frame(output[2])
MedianArtist    <- as.data.frame(output[3])
 
#Album names with artist name for Genius library
#Not used atm
albumNamesWithArtist <- cbind(MedianAlbums$artist_name, MedianAlbums$album_name)

#get list with all different album names
albumsNames <- MedianAlbums$album_name

#get album data including lyric data
song_data <- get_album_data(artist = ArtistID , albums = albumsNames, authorization = access_token )

#Remove rows without lyrics
song_dataClean<-song_data[!(song_data$lyrics=="NULL"),]

#Merge dataframes, add the individual song lyric data and remove unnecessary columns
test1 <- merge(ArtistComplete, song_dataClean[ , c("track_name","lyrics")], by = "track_name")
test1<-test1[!(test1$lyrics=="NA"),]
test1 <- test1[-c(22:25)]
test1 <- test1 %>% arrange(test1$album_release_date, test1$track_number)

#Acquire nrc lexicon and change its format
nrcLexicon = get_sentiments("nrc")
angry_words = subset.data.frame(nrcLexicon, nrcLexicon$sentiment == "anger")
names(angry_words) = c("word", "anger")

if(angry_words$anger == "anger") {angry_words$anger <- 'TRUE'}
  

undesired_words <- c("chorus", "repeat", "lyrics",
                       "yeah", "wanna", "gonna",
                       "verse", "hey", "yo", "verse", "gotta", "make", "hook", "niggas", "nigga",
                       "cube", "uh", "huh")

#t25<- paste(t1$lyric, t1$line, sep = "")
#t1$lyric <- as.(t1$lyric)

#Clean up lyrics
tidy_lyrics <- t1 %>%
  unnest_tokens(word, lyric) %>% #Break the lyrics into individual words
  filter(!word %in% undesired_words) %>% #Remove undesired words
  filter(!nchar(word) < 3) %>% #Short words are removed, ie 'ah' 'ooh'
  anti_join(stop_words) #Data provided by the tidytext package


#Distinct words by album

word_summary <- tidy_lyrics %>%
  group_by(track_name, album_name) %>%  #add ,track_name for per track overview
  filter(!album_name == "NA") %>%
  mutate(word_count = n_distinct(word)) %>%
  select(track_name, album_name = album_name, word_count) %>% #add track_name for per track overview returned
  distinct() %>% 
  ungroup()

word_summary <- word_summary %>% 
  mutate(album_name = replace(album_name, album_name == 'DAMN. COLLECTORS EDITION.', 'DAMN.'))

word_summary2 <- tidy_lyrics %>%
  group_by(album_name) %>%  #add ,track_name for per track overview
  filter(!album_name == "NA") %>%
  mutate(word_count = n_distinct(word)) %>%
  select(album_name = album_name, word_count) %>% #add track_name for per track overview returned
  distinct() %>% 
  ungroup()
word_summary2 <- word_summary2 %>% 
  mutate(album_name = replace(album_name, album_name == 'DAMN. COLLECTORS EDITION.', 'DAMN.'))
#word_summary <- tibble::rowid_to_column(word_summary, "ID")
#word_summary$album_name <- paste(word_summary$ID, word_summary$album_name)
#word_summary <- word_summary[-c(1)]

#order of albums stored in a factor
x1 <- factor(word_summary$album_name, levels=c("Overly Dedicated", "Section.80","good kid, m.A.A.d city", "To Pimp A Butterfly", "untitled unmastered.","DAMN."))
x2 <- factor(word_summary2$album_name, levels=c("Overly Dedicated", "Section.80","good kid, m.A.A.d city", "To Pimp A Butterfly", "untitled unmastered.","DAMN."))

#Plot unique words per album in a pirateplot from the yarrr package
#my.color <- c("azure4", "chocolate2", "cornflowerblue", "firebrick3")
pirateplot(formula =  word_count ~ x1, #Formula
           data = word_summary, #Data frame
           xlab = 'Album Name', ylab = "Distinct Word Count", #Axis labels
           main = "Average Unique words per album", #Plot title
           pal = "info2", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size

#Plot 2 with standardized theme offered by Yarrr library
pirateplot(formula = word_count ~ album_name,
           data = word_summary2,
           theme = 1,
           main = "Unique words per album by Kendrick Lamar",
           pal = "info2")


nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_lyrics %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

nrc_sadness <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")

tidy_lyrics %>%
  inner_join(nrc_sadness) %>%
  count(word, sort = TRUE)

tidy_lyrics %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

lyrics_bing <- tidy_lyrics %>%
  inner_join(get_sentiments("bing"))

lyrics_nrc <- tidy_lyrics %>%
  inner_join(get_sentiments("nrc"))


lyrics_nrc_sub <- tidy_lyrics %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))


theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none",
                         family = "Helvetica")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}


#Make separate datasets for each album
#Avoid using for loop as naming inside this loop will cause issues
lyrics_nrc_album1 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumsNames[1]), ]
lyrics_nrc_album2 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumsNames[2]), ]
lyrics_nrc_album3 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumsNames[3]), ]
lyrics_nrc_album4 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumsNames[4]), ]
lyrics_nrc_album5 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumsNames[5]), ]
lyrics_nrc_album6 <- lyrics_nrc[ which(lyrics_nrc$album_name==albumsNames[6]), ]


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
  scale_y_continuous(limits = c(0, 3000)) + #Hard code the axis limit
  ggtitle("Kendrick Lyrics NRC Sentiment") +
  coord_flip()
plot(nrc_plot) 

bing_plot <- lyrics_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 8000)) +
  ggtitle("Hip-Hop Bing Sentiment") +
  coord_flip()
plot(bing_plot)
