ls(all=TRUE)
rm(list=ls(all=TRUE))
ls()
ls(all=TRUE)

forestfires <- read.csv("C:/Users/fcecursos/Desktop/ML/forestfires.csv",stringsAsFactors = FALSE)
head(forestfires)

sum(is.na(forestfires))

#which function
length(which(forestfires$area == 0))

forestfires$month <- as.numeric((as.factor(forestfires$month)))
forestfires$day <- as.numeric((as.factor(forestfires$day)))

str(forestfires)
