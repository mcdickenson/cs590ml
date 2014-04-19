setwd('~/github/olympifier/data')

?read.csv
athletes = read.csv('athletes.tsv', sep="\t", header=TRUE, as.is=TRUE)
head(athletes)

athletes[1:20,]

unique(athletes$Event[athletes$Sport=="Volleyball"])

athletes = athletes[which(!is.na(athletes$Sport) & !is.na(athletes$Height..cm) & !is.na(athletes$Weight)), ]

unique(athletes$Sport)
unique(athletes$Event)


want = c("Basketball", "Weightlifting")
subset = athletes[which(athletes$Sport %in% want),]
dim(subset)
subset$pch = subset$Sex
subset$color = ifelse(subset$Sport=="Basketball", "blue", "red")

# length(which(athletes$Sport=="Gymnastics - Artistic"))

head(subset)
tail(subset)

min(subset$Height..cm)

length(which(subset$Sport=="Basketball"))
length(which(subset$Sport!="Basketball"))
nrow(subset)

setwd('~/github/cs590ml/project')
pdf('Basketball-Weightlifting.pdf')
plot(subset$Weight, subset$Height..cm, 
  pch=subset$pch, col=subset$color,
  xlab="Weight (kg)", ylab="Height (cm)",
  xlim=c(45,170))
legend('topleft', legend=c('Male', 'Female', 'Basketball', 'Weightlifting'),
  pch=c(0,1,16,16),
  col=c('black', 'black', 'blue', 'red'))
dev.off()


############################################

setwd('~/github/olympifier')
athletes = read.csv('data/athletes-clean.csv', header=TRUE, as.is=TRUE)
dim(athletes)
sort(unique(athletes$Sport))

colors = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")

wantSports = c("Basketball", "Rowing", "Weightlifting", "Wrestling")
want = c(which(athletes$Sport %in% wantSports))
subset = athletes[want, ]
subset$color = NA 
subset$color = ifelse(subset$Sport=="Basketball", "#e41a1c", subset$color)
subset$color = ifelse(subset$Sport=="Rowing", "#377eb8", subset$color)
subset$color = ifelse(subset$Sport=="Weightlifting", "#4daf4a", subset$color)
subset$color = ifelse(subset$Sport=="Wrestling", "#984ea3", subset$color)
subset$symbol = ifelse(subset$Female==1, 17, 15)
head(subset)

# plot by sport

pdf("graphics/basketball.pdf")
plot(jitter(subset$Weight), jitter(subset$Height),
  col=subset$color,
  pch=subset$symbol,
  xlim=c(40, 160),
  ylim=c(140, 220),
  xlab="Weight (kg)",
  ylab="Height (cm)")
legend('topleft', legend=c('Male', 'Female', wantSports),
  pch=c(15, 17, rep(16, 4)),
  col=c('black', 'black', colors))
dev.off()

wantSports2 = c("Archery", "Swimming", "Handball", "Triathlon")
want2 = c(which(athletes$Sport %in% wantSports2))
subset2 = athletes[want2, ]
dim(subset2)
subset2$color = NA
subset2$color = ifelse(subset2$Sport=="Archery", "#e41a1c", subset2$color)
subset2$color = ifelse(subset2$Sport=="Swimming", "#377eb8", subset2$color)
subset2$color = ifelse(subset2$Sport=="Handball", "#4daf4a", subset2$color)
subset2$color = ifelse(subset2$Sport=="Triathlon", "#984ea3", subset2$color)
subset2$symbol = ifelse(subset2$Female==1, 17, 15)

pdf("graphics/swimming.pdf")
plot(jitter(subset2$Weight), jitter(subset2$Height),
  col=subset2$color,
  pch=subset2$symbol,
  xlim=c(40, 160),
  ylim=c(140, 220),
  xlab="Weight (kg)",
  ylab="Height (cm)")
legend('topleft', legend=c('Male', 'Female', wantSports2),
  pch=c(15, 17, rep(16, 4)),
  col=c('black', 'black', colors))
dev.off()



# plot by event
setwd('~/github/olympifier')
athletes = read.csv('data/athletes-clean.csv', header=TRUE, as.is=TRUE)
athletes = athletes[which(athletes$Sport=="Athletics"),]
sort(unique(athletes$FirstEvent))
myColors = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")

# well-defined clusters
# 100m hurdles
# javelin
# high jump
# hammer throw

athletes$event = NA 
athletes$event = ifelse(athletes$FirstEvent=="Men's 110m Hurdles", "100m Hurdles", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's 100m Hurdles", "100m Hurdles", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Men's Javelin Throw", "Javelin", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's Javelin Throw", "Javelin", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Men's High Jump", "High Jump", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's High Jump", "High Jump", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Men's Hammer Throw", "Hammer Throw", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's Hammer Throw", "Hammer Throw", athletes$event)

sort(unique(athletes$event))

length(which(athletes$FirstEvent=="Men's Javelin Throw"))
length(which(athletes$FirstEvent=="Women's Javelin Throw"))
length(which(athletes$event=="Javelin"))

wantEvents = c("100m Hurdles", "Hammer Throw", "High Jump", "Javelin")
want = c(which(athletes$event %in% wantEvents))
subset = athletes[want, ]
# dim(subset)
# head(subset)
subset$color = NA 
for(i in 1:4){
  subset$color = ifelse(subset$event==wantEvents[i], myColors[i], subset$color)
}
subset$symbol = ifelse(subset$Female==1, 17, 15)
head(subset)

class(subset$Weight)
class(subset$Height)

summary(subset$color)
unique(subset$color)

pdf("graphics/javelin.pdf")
plot(jitter(subset$Weight), jitter(subset$Height),
  col=subset$color,
  pch=subset$symbol,
  xlim=c(40, 160),
  ylim=c(140, 220),
  xlab="Weight (kg)",
  ylab="Height (cm)")
legend('topleft', legend=c('Male', 'Female', wantEvents),
  pch=c(15, 17, rep(16, 4)),
  col=c('black', 'black', myColors))
dev.off()

# poorly defined
# Women's 100m
# Women's Marathon
# Women's 4x400m Relay
# Women's 400m hurdles

athletes$event = ifelse(athletes$FirstEvent=="Men's 100m", "100m", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's 100m", "100m", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Men's Marathon", "Marathon", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's Marathon", "Marathon", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Men's 4 x 400m Relay", "400m Relay", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's 4 x 400m Relay", "400m Relay", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Men's 400m Hurdles", "400m Hurdles", athletes$event)
athletes$event = ifelse(athletes$FirstEvent=="Women's 400m Hurdles", "400m Hurdles", athletes$event)

sort(unique(athletes$event))


wantEvents = c("100m", "400m Hurdles", "400m Relay", "Marathon")
want = c(which(athletes$event %in% wantEvents))
subset = athletes[want, ]
# dim(subset)
# head(subset)
subset$color = NA 
for(i in 1:4){
  subset$color = ifelse(subset$event==wantEvents[i], myColors[i], subset$color)
}
subset$symbol = ifelse(subset$Female==1, 17, 15)


pdf("graphics/marathon.pdf")
plot(jitter(subset$Weight), jitter(subset$Height),
  col=subset$color,
  pch=subset$symbol,
  xlim=c(40, 160),
  ylim=c(140, 220),
  xlab="Weight (kg)",
  ylab="Height (cm)")
legend('topleft', legend=c('Male', 'Female', wantEvents),
  pch=c(15, 17, rep(16, 4)),
  col=c('black', 'black', myColors))
dev.off()


# todo: jitter basketball