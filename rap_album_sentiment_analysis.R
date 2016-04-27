# An R Project to harvest data from Genius and perform a sentiment analysis
# Based on this blog post: http://juliasilge.com/blog/If-I-Loved-NLP-Less/
# 04.27.2016


library(dplyr)
library(rvest)
library(stringr)
library(syuzhet)
library(ggplot2)
library(png)

#==============================================================================


# Define functions used in downstream analysis


# Take a Genius album page html and scrub it to get track htmls
# The artist's name is used in a regex search to find the htmls
# Note that this function will likely return more than just the track htmls
# since the Genius tracklist will often include credits and other extra
# links
get_track_htmls <- function(album_html, artist_name) {
  
  # Define the regex search to be used after the html is parsed
  regex_search <- paste0("http://genius.com/", artist_name, "-(.*)")
  
  # Get the html nodes containing track htmls
  results <- read_html(album_html) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_extract(regex_search)
  
  results[!is.na(results)]
}


# Take a Genius song parsed html and extract the lyrics from the page
# The output is split by line as it appears on the original webpage
# Also, convert the list (which only contains one element) to a vector
get_lyrics <- function(html) {
  
  html_node(html, "lyrics") %>%
    html_text(trim = T) %>%
    str_split("\n") %>%
    unlist()
}
  

# Take Genius lyrics and remove lines surrounded by brackets since
# these represent production information, artist information, and other
# metadata that should not be used in the sentiment analysis
remove_brackets <- function(text) {
  
  indexes <- sapply(1:length(text), 
                    function(x) is.na(str_match(text[x], "\\[|\\]")))
  text[indexes]
}


# Take Genius lyrics and remove blank lines
remove_blanks <- function(text) {
  
  indexes <- sapply(1:length(text), 
                    function(x) str_length(text[x]) > 0)
  text[indexes]
}


# Take a vector of htmls for a given album on Genius and get all the lyrics
# The resulting character vector contains the entire album's lyrics as well
# as a marker (e.g., "1marker", "2marker") inserted to help record 
# the track breaks
process_album <- function(htmls) {
  
  album <- vector(mode = "character", length = 0)
  
  for (i in 1:length(htmls)) {
    
    song <- read_html(htmls[i]) %>%
      get_lyrics() %>%
      remove_brackets() %>%
      remove_blanks()
    
   album <- c(album, paste0(i, "marker"), song)
  }
  
  return(album)
}


# This function modified from Julia Silge
# Take text and process the sentiment using a given method
process_sentiment <- function(rawtext, mymethod) {
  
  chunkedtext <- data_frame(x = rawtext) %>% 
    group_by(linenumber = ceiling(row_number() / 10)) %>% 
    summarize(text = str_c(x, collapse = " "))
  
  mySentiment <- 
    data.frame(cbind(linenumber = chunkedtext$linenumber, 
                     sentiment = get_sentiment(chunkedtext$text, 
                                               method = mymethod)))
}


# This function modified from Julia Silge
# Chunk the text
chunk_text <- function(rawtext) {
  
  chunkedtext <- data_frame(x = rawtext) %>% 
    group_by(linenumber = ceiling(row_number() / 10)) %>% 
    summarize(text = str_c(x, collapse = " "))
}


# Take a chunked text (after process_album and chunk_text)
# and a vector of album htmls and return the chunked text vector elements 
# that contain a track break
get_track_splits <- function(chunkedtext, album_htmls) {
  
  indexes <- numeric(length = 0)
  
  for (i in 1:length(album_htmls)) {
    
    regex_search <- paste0("( ", i, "marker|^", i, "marker)( |$)")
    index <- str_locate(chunkedtext$text, regex_search)[, 1] > 0
    index <- which(index)
    indexes <- c(indexes, index)
  }  
  
  return(indexes)
}


# Take a vector of tracksplits and find where labels should go on the
# sentiment plot
# Subtracting 0.5 from every tracksplit value is necessary because vertical
# lines on the sentiment plot delineating tracks are also adjusted back by 
# 0.5 so that vertical lines do not fall in the middle of the histogram bars
# but before them
find_middle <- function(tracksplits, chunkedtext) {
  
  x <- c(tracksplits, length(chunkedtext$text))
  x <- x - 0.5
  result <- numeric(0)
  
  for (i in 2:length(x)) {
    mean <- mean(x[(i-1):i])
    result <- c(result, mean)
  }
  
  return(result)
}


# This function modified from Julia Silge
# Plot the text sentiment
plot_sentiment <- function(mySentiment, myAnnotate, mySplits, myImage) {
  
  mySplits <- mySplits - 0.5
  
  color.vec <- ifelse(mySentiment$sentiment >= 0, 
                      "forestgreen", "firebrick3")
  
  g <- ggplot(data = mySentiment, aes(x = linenumber, y = sentiment)) +
    labs(y = "Sentiment", size = 2) +
    geom_bar(stat = "identity", fill = color.vec) + 
    geom_vline(xintercept = mySplits[2:length(mySplits)], 
               linetype = "dotted", size = 0.5) +
    geom_label(data = myAnnotate, aes(x, y, label=label), 
               label.size = 0, size = 4, color="#2b2b2b",
               inherit.aes = FALSE) +
    # geom_segment(data = myAnnotate, aes(x = x, y = y1, xend = x, yend = y2),
                 # arrow = arrow(length = unit(0.04, "npc")), 
                 # inherit.aes = FALSE) +
    theme_minimal() +
    scale_x_discrete(expand=c(0.02,0)) +
    coord_cartesian(ylim = c(-12, 12)) +
    #theme(plot.caption=element_text(size=8)) +
    theme(axis.text.y=element_text(margin=margin(r=-10))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank()) +
    annotation_raster(myImage, 
                      xmin = 0, xmax = length(mySentiment$sentiment)/10, 
                      ymin = -13, ymax = -8)
}

#==============================================================================


# To Pimp A Butterfly Analysis


tpab.htmls <- get_track_htmls(
  "http://genius.com/albums/Kendrick-lamar/To-pimp-a-butterfly", 
  "Kendrick")
tpab.htmls <- tpab.htmls[2:17]
tpab.htmls


tpab.album <- process_album(tpab.htmls)
tpab.chunked <- chunk_text(tpab.album)
tpab.tracksplits <- get_track_splits(tpab.chunked, tpab.htmls)


tpab.sentiment <- process_sentiment(tpab.album, "bing")


tpab.anno <- data.frame(x = find_middle(tpab.tracksplits, tpab.chunked), 
                        y = rep(c(11, 9), 20)[1:length(tpab.tracksplits)], 
                        label = c("Wesley's Theory", "For Free?",
                                  "King Kunta", "Institutionalized",
                                  "These Walls", "u", "Alright",
                                  "For Sale?", "Momma",
                                  "Hood Politics", "How Much...",
                                  "Complexion...",
                                  "The Blacker The Berry",
                                  "You Ain't Gotta Lie...",
                                  "i", "Mortal Man"))
tpab.image <- readPNG("images/tpab.png")
p.tpab <- 
  plot_sentiment(tpab.sentiment, tpab.anno, tpab.tracksplits, tpab.image)
p.tpab + 
  labs(title = 
         expression(paste("Sentiment in ", italic("To Pimp A Butterfly"))))

#==============================================================================


# Take Care Analysis


tc.htmls <- get_track_htmls("http://genius.com/albums/Drake/Take-care", 
                            "Drake")
tc.htmls <- tc.htmls[1:21]
tc.htmls  


tc.album <- process_album(tc.htmls)
tc.chunked <- chunk_text(tc.album)
tc.tracksplits <- get_track_splits(tc.chunked, tc.htmls)


tc.sentiment <- process_sentiment(tc.album, "bing")


tc.anno <- data.frame(x = find_middle(tc.tracksplits, tc.chunked), 
                        y = rep(c(11, 9), 20)[1:length(tc.tracksplits)], 
                        label = c("Over My...", "Shot For Me",
                                  "Headlines", "Crew Love", "Take Care",
                                  "Marvin's Room", "Buried Alive...",
                                  "Under...", "We'll Be Fine",
                                  "Make Me Proud", "Lord Knows", "Cameras",
                                  "Good Ones...", "Doing...",
                                  "The Real Her", "Look What...",
                                  "HYFR", "Practice", "The Ride", "The Motto",
                                  "Hate Sleeping..."))
tc.image <- readPNG("images/tc.png")
p.tc <- plot_sentiment(tc.sentiment, tc.anno, tc.tracksplits, tc.image)
p.tc + labs(title = 
           expression(paste("Sentiment in ", italic("Take Care"))))

#==============================================================================


# Illmatic Analysis


ill.htmls <- get_track_htmls("http://genius.com/albums/Nas/Illmatic", "Nas")
ill.htmls <- ill.htmls[3:12]
ill.htmls


ill.album <- process_album(ill.htmls)
ill.chunked <- chunk_text(ill.album)
ill.tracksplits <- get_track_splits(ill.chunked, ill.htmls)


ill.sentiment <- process_sentiment(ill.album, "bing")


ill.anno <- data.frame(x = find_middle(ill.tracksplits, ill.chunked), 
                      y = rep(c(11, 9), 20)[1:length(ill.tracksplits)], 
                      label = c("The Genesis", "N.Y. State of Mind",
                                "Life's A Bitch", "The World Is Yours",
                                "Halftime", "Memory Lane (Sittin' In Da Park)",
                                "One Love", "One Time 4 Your Mind",
                                "Represent", "It Ain't Hard To Tell"))
ill.image <- readPNG("images/illmatic.png")
p.ill <- plot_sentiment(ill.sentiment, ill.anno, ill.tracksplits, ill.image)
p.ill + 
  labs(title = expression(paste("Sentiment in ", italic("Illmatic"))))

#==============================================================================


# The Blueprint Analysis


bp.htmls <- get_track_htmls("http://genius.com/albums/Jay-z/The-blueprint",
                            "Jay-z")
bp.htmls <- bp.htmls[2:16]
bp.htmls


bp.album <- process_album(bp.htmls)
bp.chunked <- chunk_text(bp.album)
bp.tracksplits <- get_track_splits(bp.chunked, bp.htmls)


bp.sentiment <- process_sentiment(bp.album, "bing")


bp.anno <- data.frame(x = find_middle(bp.tracksplits, bp.chunked), 
                       y = rep(c(11, 9), 20)[1:length(bp.tracksplits)], 
                       label = c("The Ruler's Back", "Takeover", 
                                 "Izzo (H.O.V.A.)", "Girls, Girls, Girls",
                                 "Jigga That...", "U Don't Know", 
                                 "Hola' Hovito", "Heart Of The City...",
                                 "Never Change", "Song Cry", "All I Need",
                                 "Renegade", "Blueprint...", "Breathe Easy...",
                                 "Girls, Girls, Girls, Pt. 2"))
bp.image <- readPNG("images/bp.png")
p.bp <- plot_sentiment(bp.sentiment, bp.anno, bp.tracksplits, bp.image)
p.bp + 
  labs(title = expression(paste("Sentiment in ", italic("The Blueprint"))))

#==============================================================================


# The College Dropout Analysis


cd.htmls <- 
  get_track_htmls("http://genius.com/albums/Kanye-west/The-college-dropout",
                  "Kanye")
cd.htmls <- cd.htmls[5:25]
cd.htmls


cd.album <- process_album(cd.htmls)
cd.chunked <- chunk_text(cd.album)
cd.tracksplits <- get_track_splits(cd.chunked, cd.htmls)

# Need this just because of the skits on this album
cd.tracksplits <- cd.tracksplits[2:21]


cd.sentiment <- process_sentiment(cd.album, "bing")


cd.anno <- data.frame(x = find_middle(cd.tracksplits, cd.chunked), 
                        y = rep(c(11, 9), 20)[1:length(cd.tracksplits)], 
                        label = c("We Don't Care", "Graduation...",
                                  "All Falls...", "I'll Fly Away",
                                  "Spaceship", "Jesus Walks",
                                  "Never Let Me Down", "Get Em High",
                                  "Workout Plan", "The New...",
                                  "Slow Jamz", "Breathe In...",
                                  "SS (S1)", "School Spirit",
                                  "SS (S2)", "Lil'...",
                                  "Two Words", "Through The Wire",
                                  "Family Business", "Last Call"))
cd.image <- readPNG("images/cd.png")
p.cd <- 
  plot_sentiment(cd.sentiment, cd.anno, cd.tracksplits, cd.image)
p.cd + labs(title = 
                expression(paste("Sentiment in ", 
                                 italic("The College Dropout"))))

#==============================================================================


# Late Registration Analysis


lr.htmls <- 
  get_track_htmls("http://genius.com/albums/Kanye-west/Late-registration",
                  "Kanye")
lr.htmls <- lr.htmls[1:23]
lr.htmls


lr.album <- process_album(lr.htmls)
lr.chunked <- chunk_text(lr.album)
lr.tracksplits <- get_track_splits(lr.chunked, lr.htmls)


lr.sentiment <- process_sentiment(lr.album, "bing")


lr.anno <- data.frame(x = find_middle(lr.tracksplits, lr.chunked), 
                      y = rep(c(11, 9), 20)[1:length(lr.tracksplits)], 
                      label = c("Wake...", "Heard 'Em Say",
                                "Touch The Sky", "Gold Digger",
                                "Skit #1", "Drive Slow", "My Way...",
                                "Crack Music", "Roses", "Bring Me...",
                                "Addiction", "Skit #2", "Diam... (Remix)",
                                "We Major", "Skit #3", "Hey Mama",
                                "Celebration", "Skit #4", "Gone",
                                "Diam...", "Late", "Back To Basics",
                                "We Can..."))
lr.image <- readPNG("images/lr.png")
p.lr <- 
  plot_sentiment(lr.sentiment, lr.anno, lr.tracksplits, lr.image)
p.lr + labs(title = 
              expression(paste("Sentiment in ", italic("Late Registration"))))

#==============================================================================


# Graduation Analysis


grad.htmls <- get_track_htmls("http://genius.com/albums/Kanye-west/Graduation",
                              "Kanye")
grad.htmls <- grad.htmls[1:15]
grad.htmls


grad.album <- process_album(grad.htmls)
grad.chunked <- chunk_text(grad.album)
grad.tracksplits <- get_track_splits(grad.chunked, grad.htmls)


grad.sentiment <- process_sentiment(grad.album, "bing")


grad.anno <- data.frame(x = find_middle(grad.tracksplits, grad.chunked), 
                       y = rep(c(11, 9), 20)[1:length(grad.tracksplits)], 
                       label = c("Good Morning", "Champion", "Stronger",
                                 "I Wonder", "Good Life",
                                 "Can't Tell Me Nothing", "Barry Bonds",
                                 "Drunk And Hot Girls", "Flashing Lights",
                                 "Everything I Am", "The Glory", "Homecoming",
                                 "Big Brother", 
                                 "Good Night", "Bittersweet Poetry"))
grad.image <- readPNG("images/grad.png")
p.grad <- 
  plot_sentiment(grad.sentiment, grad.anno, grad.tracksplits, grad.image)
p.grad + labs(title = 
               expression(paste("Sentiment in ", italic("Graduation"))))

#==============================================================================


# 808s & Heartbreak Analysis


eight.htmls <- 
  get_track_htmls("http://genius.com/albums/Kanye-west/808s-heartbreak",
                  "Kanye")
eight.htmls <- eight.htmls[1:12]
eight.htmls


eight.album <- process_album(eight.htmls)
eight.chunked <- chunk_text(eight.album)
eight.tracksplits <- get_track_splits(eight.chunked, eight.htmls)


eight.sentiment <- process_sentiment(eight.album, "bing")


eight.anno <- data.frame(x = find_middle(eight.tracksplits, eight.chunked), 
                        y = rep(c(11, 9), 20)[1:length(eight.tracksplits)], 
                        label = c("Say You Will", "Welcome To Heartbreak",
                                  "Heartless", "Amazing", "Love Lockdown",
                                  "Paranoid", "RoboCop", "Street Lights",
                                  "Bad News", "See You In...",
                                  "Coldest Winter", "Pinocchio Story"))
eight.image <- readPNG("images/808s.png")
p.eight <- 
  plot_sentiment(eight.sentiment, eight.anno, eight.tracksplits, eight.image)
p.eight + labs(title = 
                expression(paste("Sentiment in ", 
                                 italic("808s & Heartbreak"))))

#==============================================================================


# My Beautiful Dark Twisted Fantasy Analysis


mbdtf.htmls <- 
  get_track_htmls(
    "http://genius.com/albums/Kanye-west/My-beautiful-dark-twisted-fantasy",
    "Kanye")
mbdtf.htmls <- mbdtf.htmls[c(1:3,5:14)]
mbdtf.htmls


mbdtf.album <- process_album(mbdtf.htmls)
mbdtf.chunked <- chunk_text(mbdtf.album)
mbdtf.tracksplits <- get_track_splits(mbdtf.chunked, mbdtf.htmls)


mbdtf.sentiment <- process_sentiment(mbdtf.album, "bing")


mbdtf.anno <- data.frame(x = find_middle(mbdtf.tracksplits, mbdtf.chunked), 
                         y = rep(c(11, 9), 20)[1:length(mbdtf.tracksplits)], 
                         label = c("Dark Fantasy", "Gorgeous", "Power",
                                   "All Of The Lights", "Monster",
                                   "So Appalled", "Devil In A..",
                                   "Runaway", "Hell Of A Life", "Blame Game",
                                   "Lost In...", "Who Will...",
                                   "See Me Now"))
mbdtf.image <- readPNG("images/mbdtf.png")
p.mbdtf <- 
  plot_sentiment(mbdtf.sentiment, mbdtf.anno, mbdtf.tracksplits, mbdtf.image)
p.mbdtf + 
  labs(title = expression(paste("Sentiment in ", 
                                italic("My Beautiful Dark Twisted Fantasy"))))

#==============================================================================


# Yeezus Analysis


yeezus.htmls <- get_track_htmls("http://genius.com/albums/Kanye-west/Yeezus",
                              "Kanye")
yeezus.htmls <- yeezus.htmls[1:10]
yeezus.htmls


yeezus.album <- process_album(yeezus.htmls)
yeezus.chunked <- chunk_text(yeezus.album)
yeezus.tracksplits <- get_track_splits(yeezus.chunked, yeezus.htmls)


yeezus.sentiment <- process_sentiment(yeezus.album, "bing")


yeezus.anno <- data.frame(x = find_middle(yeezus.tracksplits, yeezus.chunked),
                        y = rep(c(11, 9), 20)[1:length(yeezus.tracksplits)], 
                        label = c("On Sight", "Black Skinhead",
                                  "I Am A God", "New Slaves",
                                  "Hold My Liquor", "I'm In It",
                                  "Blood On The Leaves", "Guilt Trip",
                                  "Send It Up", "Bound 2"))
yeezus.image <- readPNG("images/yeezus.png")
p.yeezus <- 
  plot_sentiment(yeezus.sentiment, yeezus.anno, 
                 yeezus.tracksplits, yeezus.image)
p.yeezus + labs(title = 
                expression(paste("Sentiment in ", italic("Yeezus"))))

#==============================================================================


# The Life of Pablo Analysis


tlop.htmls <- 
  get_track_htmls("http://genius.com/albums/Kanye-west/The-life-of-pablo",
                  "Kanye")
tlop.htmls <- tlop.htmls[2:20]
tlop.htmls


tlop.album <- process_album(tlop.htmls)
tlop.chunked <- chunk_text(tlop.album)
tlop.tracksplits <- get_track_splits(tlop.chunked, tlop.htmls)

# Minor adjustment needed
tlop.tracksplits[7] <- 35


tlop.sentiment <- process_sentiment(tlop.album, "bing")


tlop.anno <- data.frame(x = find_middle(tlop.tracksplits, tlop.chunked),
                          y = rep(c(11, 9), 20)[1:length(tlop.tracksplits)], 
                          label = c("Ultralight Beams", "Father... Pt. 1",
                                    "Father... Pt. 2", "Famous", "Feedback",
                                    "Low Lights", "Highlights", "Free...",
                                    "I Love Kanye", "Waves", "FML",
                                    "Real Friends", "Wolves", "Frank's Track",
                                    "SS Inter...", "30 Hours", 
                                    "No More Parties In L.A.", "Facts...",
                                    "Fade"))
tlop.image <- readPNG("images/tlop.png")
p.tlop <- 
  plot_sentiment(tlop.sentiment, tlop.anno, 
                 tlop.tracksplits, tlop.image)
p.tlop + labs(title = 
                  expression(paste("Sentiment in ", 
                                   italic("The Life of Pablo"))))

#==============================================================================


# Create a dataframe summarizing the sentiment analyses from Kanye's albums


kanye.titles <- c("The College Dropout", "Late Registration", "Graduation",
                  "808s & Heartbreak", "My Beautiful Dark Twisted Fantasy",
                  "Yeezus", "The Life of Pablo")
kanye.sentiments <- c(cd.sentiment, lr.sentiment, grad.sentiment,
                      eight.sentiment, mbdtf.sentiment, yeezus.sentiment,
                      tlop.sentiment)
means <- sapply((1:7)*2, function(x) mean(kanye.sentiments[[x]]))
var <- sapply((1:7)*2, function(x) var(kanye.sentiments[[x]]))
df.kanye <- data.frame(kanye.titles, round(means, digits = 2), 
                       round(var, digits = 2))
colnames(df.kanye) <- c("Album Title", "Sentiment Mean", "Sentiment Variance")
df.kanye

#==============================================================================


# Acid Rap Analysis


acid.htmls <- 
  get_track_htmls("http://genius.com/albums/Chance-the-rapper/Acid-rap",
                  "Chance")
acid.htmls <- acid.htmls[1:13]
acid.htmls


acid.album <- process_album(acid.htmls)
acid.chunked <- chunk_text(acid.album)
acid.tracksplits <- get_track_splits(acid.chunked, acid.htmls)


acid.sentiment <- process_sentiment(acid.album, "bing")


acid.anno <- data.frame(x = find_middle(acid.tracksplits, acid.chunked),
                        y = rep(c(11, 9), 20)[1:length(acid.tracksplits)], 
                        label = c("Good Ass Intro", "Pusha Man/Paranoia",
                                  "Cocoa Butter Kisses", "JUICE", "Lost",
                                  "Everybody's Something", 
                                  "Interlude (That's Love)", "Favorite Song",
                                  "NaNa", "Smoke Again", "Acid Rain", 
                                  "Chain Smoker", "Everything's Good..."))
acid.image <- readPNG("images/acid.png")
p.acid <- 
  plot_sentiment(acid.sentiment, acid.anno, 
                 acid.tracksplits, acid.image)
p.acid + labs(title = 
                expression(paste("Sentiment in ", 
                                 italic("Acid Rap"))))
