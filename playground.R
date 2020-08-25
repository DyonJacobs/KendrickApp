#START SCRAPING PROPERLY---------------------------------------------------------------------------

#Scrape from Genius URL #######################################################################
  currentSongLyrics <- get_lyrics_url('https://genius.com/Kendrick-lamar-yah-lyrics')

#Scrape through only the song name############################################################
  #singleSongLyrics <- get_lyrics_url(currentSongUrl) #Scrape lyrics URL Genius
  #scrape the lyrics
  currentSongID <- search_song(currentSong, n_results = 1, genius_token)
# currentSongID <- filter(currentSongID, currentSongID$song_name == currentSong)
  currentSongID <-head(currentSongID, 1) #get song data
  #currentSongID <- currentSongID$song_id
  currentSongLyrics <- get_lyrics_id(song_id = currentSongID$song_id, genius_token)
  
#correct format #####################################################################
  currentSongLyrics <- currentSongLyrics[1]
  names(currentSongLyrics) = c("lyric")
  currentSongLyrics <- tibble::rowid_to_column(currentSongLyrics, "line")
  currentSongLyrics <- as.data.frame.list(currentSongLyrics)
  currentSongLyrics$lyric <- as.character(currentSongLyrics$lyric)

  
  #Insert the line numbers and the lyrics into the final dataframe
  t1$line <- ifelse(t1$track_name %in% c(currentSong), currentSongLyrics$line, t1$line)
  t1$lyric <- ifelse(t1$track_name %in% c(currentSong), currentSongLyrics$lyric, t1$lyric)
  #t1 <- t1[-c(23)]
  
  
  #insert the lyrics data into the complete dataframe
  #test1$lyrics <- ifelse(test1$track_name %in% c(currentSong),currentSongLyrics[1] , test1$lyrics)
  
#  t1 <- test1
#  t1 <- t1 %>%
#    unnest(lyrics)


  
  
  
  
  
  
  currentSongUrl <- 'https://genius.com/Kendrick-lamar-growing-apart-to-get-closer-lyrics'
  
  singleSongLyrics <- get_lyrics_url(currentSongUrl) #Scrape lyrics URL Genius
  
  #correct format
  singleSongLyrics <- singleSongLyrics[1]
  names(singleSongLyrics) = c("lyric")
  singleSongLyrics <- tibble::rowid_to_column(singleSongLyrics, "line")
  singleSongLyrics <- as.data.frame.list(singleSongLyrics)
  singleSongLyrics$lyric <- as.character(singleSongLyrics$lyric)

  #insert the lyrics data into the complete dataframe
  test1$lyrics <- ifelse(test1$track_name %in% c(currentSong),singleSongLyrics , test1$lyrics)

  
  t1$line <- ifelse(t1$track_name %in% c(currentSong), singleSongLyrics$line, t1$line)
  t1$lyric <- ifelse(t1$track_name %in% c(currentSong), singleSongLyrics$lyric, t1$lyric)
  t1 <- t1[-c(23)]

  
  
  t1 <- t25
  t1 <- t1 %>%
    unnest(lyrics)  

  #Remove Black Panther Album  
t25 <- test1[!(test1$album_name=="Black Panther The Album Music From And Inspired By"),]
d<-d[!(d$A=="B" & d$E==0),]



#------------------------ Handmatig missende lyrics albums scrapen
DAMN <- genius_album(artist = "Weezer", album = "Weezer")
artist <- get_artist(1421, genius_token)
album <- get_album(337082, genius_token )
album <- album_to_df(album)
test123<- search_song("HUMBLE", n_results = 1, genius_token) #get song data
testsong123<- get_song(3039923, genius_token) 
testsong123 <- testsong123$content$lyric
#get album data through the song data

kek1 <- get_lyrics_id(song_id = 3039923, genius_token) #scrapen lyrics of a specific song ID

singleSongLyrics <- get_lyrics_url('https://genius.com/Kendrick-lamar-opposites-attract-tomorrow-w-o-her-lyrics') #Scrape lyrics URL Genius

#Get lyrics of a single song in the correct format--------------------------------
currentSong <- "Growing Apart (To Get Closer)"

#Scrape method 1
#singleSongLyrics <- get_lyrics_search(artist_name = "Kendrick Lamar", #Scrape lyrics through artist & song name
#                         song_title = currentSong)

#START SCRAPING pROPERLY---------------------------------------------------------------------------
currentSongUrl <- 'https://genius.com/Kendrick-lamar-growing-apart-to-get-closer-lyrics'
#Scrape method 2
singleSongLyrics <- get_lyrics_url(currentSongUrl) #Scrape lyrics URL Genius

#correct format
singleSongLyrics <- singleSongLyrics[1]
names(singleSongLyrics) = c("lyric")
singleSongLyrics <- tibble::rowid_to_column(singleSongLyrics, "line")
singleSongLyrics <- as.data.frame.list(singleSongLyrics)
singleSongLyrics$lyric <- as.character(singleSongLyrics$lyric)
currentSong <- 'Growing Apart (To Get Closer)'
#insert the lyrics data into the complete dataframe
test1$lyrics <- ifelse(test1$track_name %in% c(currentSong),singleSongLyrics , test1$lyrics)
t1$line <- ifelse(t1$track_name %in% c(currentSong), singleSongLyrics$line, t1$line)
t1$lyric <- ifelse(t1$track_name %in% c(currentSong), singleSongLyrics$lyric, t1$lyric)
t1 <- t1[-c(23)]
#--------------------------------------------------------------------------------------------------
#Get lyrics of an entire album, returns incomplete values
kek4 <- genius_album(artist = "Kendrick Lamar", album = "DAMN.", info = "title")

#---------------------------------------------------------------------------------









#VISUALIZING SPOTIFY STATISTICS
#Method 1-----------------------------------------
spotify_plot1 <- ggplot() +
  #geom_smooth(data = test1S80, aes(track_number, energy), colour = "blue", se = FALSE, method = loess) +
  #geom_path(data = test1S80, aes(track_number, energy), colour = "blue", linemitre = 5) +
  geom_line(data = test1OD, aes(track_number, danceability), colour = "blue", size = 1.2) +
  geom_line(data = test1S80, aes(track_number, danceability), colour = "brown", size = 1.2) +
  geom_line(data = test1GKMC, aes(track_number, danceability), colour = "orange", size = 1.2) +
  geom_line(data = test1TPAB, aes(track_number, danceability), colour = "black", size = 1.2) +
  geom_line(data = test1UU, aes(track_number, danceability), colour = "green", size = 1.2) +
  geom_line(data = test1DAMN, aes(track_number, danceability), colour = "red", size = 1.2) 
spotify_plot1

#Method 2---------------------------------
plot(test1OD$track_number, test1OD$danceability, col = "grey", type = 'l', ylim=range(0.2,.9), lwd = 2)
lines(test1S80$track_number, test1S80$danceability, col = "brown", type = 'l', lwd = 2)
lines(test1GKMC$track_number, test1GKMC$danceability, col = "blue", type = 'l', lwd = 2)
lines(test1TPAB$track_number, test1TPAB$danceability, col = "orange", type = 'l', lwd = 2)
lines(test1UU$track_number, test1UU$danceability, col = "green", type = 'l', lwd = 2)
lines(test1DAMN$track_number, test1DAMN$danceability, col = "red", type = 'l', lwd = 2)

#Method 3--------------------------------
ggplot(ArtistComplete, aes(track_number, danceability)) + 
  geom_line(aes(colour=album_name), position = "identity", size = 1.2)

#Method 3 function
getSpotifyPlot2 <- function(album1, album2, album3, album4){
  selectedAlbum1 <- album1
  selectedAlbum2 <- album2
  selectedAlbum3 <- album3
  selectedAlbum4 <- album4
  ArtistCompleteTest <- subset(ArtistComplete, album_name == selectedAlbum1)
  ArtistCompleteTest2 <- subset(ArtistComplete, album_name == selectedAlbum2)
  ArtistCompleteTest3 <- subset(ArtistComplete, album_name == selectedAlbum3)
  ArtistCompleteTest4 <- subset(ArtistComplete, album_name == selectedAlbum4)
  ArtistCompleteTestFinal <- rbind(ArtistCompleteTest, ArtistCompleteTest2, ArtistCompleteTest3,ArtistCompleteTest4)
  ggplot(ArtistCompleteTestFinal, aes(track_number, energy)) + 
    labs(x = "Track Number", colour = "Album Name") +
    geom_line(aes(colour=album_name), size = 1.2) +
    theme(axis.title.x = element_text(size = 15),
          title = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 15),
          legend.background = element_rect(colour = "grey", linetype = 3),
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 20, face = "bold")) 

  
}
getSpotifyPlot3 <- function(album1, album2, album3, album4){
  selectedAlbum1 <- album1
  selectedAlbum2 <- album2
  selectedAlbum3 <- album3
  selectedAlbum4 <- album4
  ArtistCompleteTest <- subset(ArtistComplete, album_name == selectedAlbum1)
  ArtistCompleteTest2 <- subset(ArtistComplete, album_name == selectedAlbum2)
  ArtistCompleteTest3 <- subset(ArtistComplete, album_name == selectedAlbum3)
  ArtistCompleteTest4 <- subset(ArtistComplete, album_name == selectedAlbum4)
  ArtistCompleteTestFinal <- rbind(ArtistCompleteTest, ArtistCompleteTest2, ArtistCompleteTest3,ArtistCompleteTest4)
  ggplot(ArtistCompleteTestFinal, aes(track_number, danceability)) + 
    labs(x = "Track Number", colour = "Album Name") +
    geom_line(aes(colour=album_name), size = 1.2) +
    theme(axis.title.x = element_text(size = 15),
          title = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 15),
          legend.background = element_rect(colour = "grey", linetype = 3),
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 20, face = "bold"))
}
getSpotifyPlot4 <- function(album1, album2, album3, album4){
  selectedAlbum1 <- album1
  selectedAlbum2 <- album2
  selectedAlbum3 <- album3
  selectedAlbum4 <- album4
  ArtistCompleteTest <- subset(ArtistComplete, album_name == selectedAlbum1)
  ArtistCompleteTest2 <- subset(ArtistComplete, album_name == selectedAlbum2)
  ArtistCompleteTest3 <- subset(ArtistComplete, album_name == selectedAlbum3)
  ArtistCompleteTest4 <- subset(ArtistComplete, album_name == selectedAlbum4)
  ArtistCompleteTestFinal <- rbind(ArtistCompleteTest, ArtistCompleteTest2, ArtistCompleteTest3,ArtistCompleteTest4)
  ggplot(ArtistCompleteTestFinal, aes(track_number, acousticness)) + 
    labs(x = "Track Number", colour = "Album Name") +
    geom_line(aes(colour=album_name), size = 1.2) +
    theme(axis.title.x = element_text(size = 15),
          title = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 15),
          legend.background = element_rect(colour = "grey", linetype = 3),
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 20, face = "bold"))
}
getSpotifyPlot5 <- function(album1, album2, album3, album4){
  selectedAlbum1 <- album1
  selectedAlbum2 <- album2
  selectedAlbum3 <- album3
  selectedAlbum4 <- album4
  ArtistCompleteTest <- subset(ArtistComplete, album_name == selectedAlbum1)
  ArtistCompleteTest2 <- subset(ArtistComplete, album_name == selectedAlbum2)
  ArtistCompleteTest3 <- subset(ArtistComplete, album_name == selectedAlbum3)
  ArtistCompleteTest4 <- subset(ArtistComplete, album_name == selectedAlbum4)
  ArtistCompleteTestFinal <- rbind(ArtistCompleteTest, ArtistCompleteTest2, ArtistCompleteTest3,ArtistCompleteTest4)
  ggplot(ArtistCompleteTestFinal, aes(track_number, liveness)) + 
    labs(x = "Track Number", colour = "Album Name") +
    geom_line(aes(colour=album_name), size = 1.2) +
    theme(axis.title.x = element_text(size = 15),
          title = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 15),
          legend.background = element_rect(colour = "grey", linetype = 3),
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 20, face = "bold")) 
}
getSpotifyPlot6 <- function(album1, album2, album3, album4){
  selectedAlbum1 <- album1
  selectedAlbum2 <- album2
  selectedAlbum3 <- album3
  selectedAlbum4 <- album4
  ArtistCompleteTest <- subset(ArtistComplete, album_name == selectedAlbum1)
  ArtistCompleteTest2 <- subset(ArtistComplete, album_name == selectedAlbum2)
  ArtistCompleteTest3 <- subset(ArtistComplete, album_name == selectedAlbum3)
  ArtistCompleteTest4 <- subset(ArtistComplete, album_name == selectedAlbum4)
  ArtistCompleteTestFinal <- rbind(ArtistCompleteTest, ArtistCompleteTest2, ArtistCompleteTest3,ArtistCompleteTest4)
  ggplot(ArtistCompleteTestFinal, aes(track_number, tempo)) + 
    labs(x = "Track Number", colour = "Album Name") +
    geom_line(aes(colour=album_name), size = 1.2) +
    theme(axis.title.x = element_text(size = 15),
          title = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 15),
          legend.background = element_rect(colour = "grey", linetype = 3),
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 20, face = "bold"))
}
getSpotifyPlot2("Overly Dedicated", "Section.80", "", "")

