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
