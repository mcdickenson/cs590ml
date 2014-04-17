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

pdf("graphics/basketball.pdf")
plot(subset$Weight, subset$Height,
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

dev.off()
