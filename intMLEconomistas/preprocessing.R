#Limpiar escritorio
ls(all=TRUE)
rm(list=ls(all=TRUE))
ls()
ls(all=TRUE)

##Definir escritorio

##Datos

data <- read.csv("C:/Users/fcecursos/Desktop/ML/EjercicioVuelos29Mar22.csv",stringsAsFactors = FALSE)
head(data)

data1 <- data.frame(data$YEAR,data$MONTH,data$ORIGIN,data$ARR_DELAY)
names(data1) = c("año","mes","origen","arrdelay")
head(data1)
colnames(data) = c("year","month","dayofmonth","dayofweek","uniquecarrier","tailnum","origin","dest","crsdeptime","depcelay","arrdelay","cancelled","diverted","distance","carrierdelay","weatherdalay","nasdelay","securitydelay","lateaircraftdelay","x")

#borrar columna
data$x <- NULL

#dimensiones df
dim(data)

#tipos de variables
str(data)

#info detallada de datos
summary(data)

#Cuantas veces se repite cada variable
table(data$dest)

#Ver cantidad de datos vacios en una columna
sum(is.na(data$depcelay))/4955.44

#Eliminar todos los datos nulos en un nuevo df
dataomit = na.omit(data)

boxplot.stats(data$arrdelay)
