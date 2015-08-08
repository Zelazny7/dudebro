library(tm)
library(lubridate)

Sys.setlocale('LC_ALL','C') 
chats <- read.csv('~/Downloads/dudebro.csv', header=T, stringsAsFactors=F)[,-1]

# clean up belas name
chats$sender[grep('Voort', chats$sender)] <- 'Bela Vander Voort'
chats$sender[grep('Poyas', chats$sender)] <- 'Dave Poyas'

chats$timeStamp <- as.POSIXct(chats$timeStamp, format = "%A, %B %d, %Y at %I:%M%p")
chats <- chats[rev(row.names(chats)),]

for (dude in unique(chats$sender)) {
  f <- chats$sender == dude
  chats$cuml_posts[f] <- seq(1, sum(f))
}

library(scales)
melted <- melt(chats, id.vars = c('timeStamp', 'sender'), measure.vars = 'cuml_posts')
ggplot(melted, aes(x=timeStamp, y=value, color=factor(sender))) + geom_line() +
  scale_x_datetime(breaks = date_breaks("1 month"), labels=date_format("%B")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cumulative Posts by Time")


### summary statistics for time between posts
diff.Date(chats$timeStamp)
for (dude in unique(chats$sender)) {
  f <- chats$sender == dude
  chats$time_between_posts[f] <- diff.POSIXt(chats$timeStamp)
}

time.diffs <- list()
for (dude in unique(chats$sender)) {
  f <- chats$sender == dude
  time.diffs[[dude]] <- data.frame(dude, time=as.numeric(diff.POSIXt(chats$timeStamp[f] + 1) / (60 * 60)))
}

lags <- data.frame(do.call(rbind, time.diffs[-7]), row.names = NULL)

meds <- data.frame(med=sapply(time.diffs[-7], function(x) median(x$time)))
meds$dude <- row.names(meds)


brks <- c(0.16, 0.5, 1, 12, 24, 24*7, 24*30)
lbls <- c('ten mins', 'half hour', 'one hour', 'half day', 'whole day', '1 week', '1 month') 

ggplot(lags, aes(x=dude, y=time, fill=dude)) + geom_boxplot(alpha=0.5) + scale_y_log10(breaks = brks, labels=lbls) +
  ggtitle("Distribution of time between posts -- Log scale")

data.frame(hrs_between_posts=do.call(cbind, time.diffs)[3,-c(5,7:8)])







weekdays(chats$timeStamp)


dudebro <- VCorpus(VectorSource(chats$content))

meta(dudebro, 'sender') <- chats$sender
meta(dudebro, 'date') <- posix.time


### cumulative posts by time
ord <- order(posix.time)

library(ggplot2)
library(reshape2)





inspect(dudebro[1:2])
dudebro <- tm_map(dudebro, stripWhitespace) #whitespace
dudebro <- tm_map(dudebro, content_transformer(tolower)) # lower case
dudebro <- tm_map(dudebro, removeWords, stopwords("english"))

tdm <- TermDocumentMatrix(dudebro)

test <- tm_map(dudebro, stemDocument)
