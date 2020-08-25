
#MISSING ALBUM TITLES
currentSong <- 'Average Joe' 
currentSong <- 'Growing Apart (To Get Closer)' 
currentSong <- 'H.O.C' #-
currentSong <- 'F*ck Your Ethnicity' #-
currentSong <- 'A.D.H.D' #-
currentSong <- "Tammy's Song (Her Evils)"
currentSong <- 'Chapter Six'
currentSong <- 'Ronald Reagan Era'
currentSong <- 'Chapter Ten'
currentSong <- 'Rigamortus'
currentSong <- 'Kush & Corinthians (feat. BJ The Chicago Kid)'
currentSong <- 'Bitch, Don't Kill My Vibe'
currentSong <- 'Poetic Justice'
currentSong <- 'Swimming Pools (Drank) - Extended Version'
currentSong <- "Sing About Me, I'm Dying Of Thirst"
currentSong <- 'Real'
currentSong <- "Wesley's Theory"
currentSong <- 'Institutionalized'
currentSong <- 'Alright'
currentSong <- 'How Much A Dollar Cost'
currentSong <- 'Complexion (A Zulu Love)'
currentSong <- "You Ain't Gotta Lie (Momma Said)"
currentSong <- 'Mortal Man'
currentSong <- 'untitled 04 | 08.14.2014.'
currentSong <- 'untitled 07 | 2014 - 2016'
currentSong <- 'untitled 08 | 09.06.2014.'
currentSong <- 'GOD.'
currentSong <- 'LOVE. FEAT. ZACARI.'
currentSong <- 'LUST.'
currentSong <- 'PRIDE.'
currentSong <- 'FEEL.'
currentSong <- 'YAH.'

#complete dataset for Overly Dedicated without lyrics, with line #
test1OD   <- test1 [which(test1$album_name=="Overly Dedicated"),]
test1ODLyrics <- t1 [which(t1$album_name=="Overly Dedicated"),]

test1S80  <- test1 [which(test1$album_name=="Section.80"),]
test1S80Lyrics <- t1 [which(t1$album_name=="Section.80"),]

test1GKMC <- test1 [which(test1$album_name=="good kid, m.A.A.d city"),] 
test1GKMCLyrics <- t1 [which(t1$album_name=="good kid, m.A.A.d city"),]

test1TPAB <- test1 [which(test1$album_name=="To Pimp A Butterfly"),]
test1TPABLyrics <- t1 [which(t1$album_name=="To Pimp A Butterfly"),]

test1UU   <- test1 [which(test1$album_name=="untitled unmastered."),]
test1UULyrics <- t1 [which(t1$album_name=="untitled unmastered."),]

test1DAMN <-  test1 [which(test1$album_name=="DAMN. COLLECTORS EDITION."),]
test1DAMNLyrics <- t1 [which(t1$album_name=="DAMN. COLLECTORS EDITION."),]


CompleteDataset <- t1
CompleteDatasetSecure <- t1

