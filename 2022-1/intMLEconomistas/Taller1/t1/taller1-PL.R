#Loading data

df <- read.csv("./Documents/UNAL/intMLEconomistas/Taller1/data.csv")
head(df)

#Column names
names(df)

#df dimensions
dim(df)

#types
str(df)

#summary statistics
summary(df)

#na values

sum(is.na(df))
#we have one na value in the entire dataframe

colSums(is.na(df))
# it appears that the na value comes from the region column,lets look deeper:
names(which(colSums((is.na(df)))>0))

which(is.na(df[11])==TRUE)

df[189,]

#With a quick look at google we can see that Benin is from Africa:

df[189,11] = 4

#Lets look again
sum(is.na(df))

#now lets change the fpl_sel column to float type, as it is a percentage:

df$fpl_sel = gsub("%","",df$fpl_sel)
df$fpl_sel = as.numeric((as.factor(df$fpl_sel)))

#lets check:
str(df$fpl_sel) # its numeric now

#Top 5 expensive players
expensive<-df[order(df$market_value,decreasing = TRUE),]
expensive[1:5,1]

#Top 5 popular players
popular<-df[order(df$page_views,decreasing = TRUE),]
popular[1:5,1]

#top 5 most valuable players in Fantasy Premier League
mv<-df[order(df$fpl_value,decreasing = TRUE),]
mv[1:5,1]

library(dplyr)
library(ggplot2)

#Now let's look for the clubs who have the most expensive players:
mkv=summarise(group_by(df,club),clubs_market_value = sum(market_value))
mkv = arrange(mkv,desc(clubs_market_value))
top_five_clubs_market_values_player= head(mkv)
top_five_clubs_market_values_player
#ggplot(mkv,aes(x=club,y=clubs_market_value),geom="bar")+geom_point()+theme(axis.text.x=element_text(angle=90,hjust=1))
 

#Age density
age_d <- ggplot(df, aes(x=age)) + 
  geom_density(color="darkblue", fill="lightblue") + ggtitle("Age density")+theme(plot.title = element_text(hjust = 0.5))
age_d

#Age is normally distributed


#Wikipedia page views distribution

pv_d <- ggplot(df, aes(x=page_views)) + 
  geom_density(color="darkblue", fill="lightblue") + ggtitle("Wikipedia page views density")+theme(plot.title = element_text(hjust = 0.5))
pv_d

#We can see a positive skewness or right skewed distribution for the page views on Wikipedia

#Market value density

mv_d <- ggplot(df, aes(x=market_value)) + 
  geom_density(color="darkblue", fill="lightblue") + ggtitle("Market value density")+theme(plot.title = element_text(hjust = 0.5))
mv_d

#We can see a positive skewness or right skewed distribution for the market value.

#Fantasy premier league value distribution

fp_d <- ggplot(df, aes(x=fpl_value)) + 
  geom_density(color="darkblue", fill="lightblue") + ggtitle("Fantasy Premier League value density")+theme(plot.title = element_text(hjust = 0.5))
fp_d
#We can see a positive skewness or right skewed distribution for FPL value.


#Players by position
pp <- ggplot(df,aes(x=position)) + geom_bar(color="darkblue",fill="red")+ ggtitle("Players by position")+theme(plot.title = element_text(hjust = 0.5))
pp

#Most PL players are CB or central back defenders, center foreward or center middle.

#Players by nationality
pp <- ggplot(df,aes(y=nationality)) + geom_bar(color="darkred",fill="green")+ ggtitle("Players by position")+theme(plot.title = element_text(hjust = 0.5))
pp

#Players by position category
pp <- ggplot(df,aes(x=position_cat)) + geom_bar(fill="orange")+ ggtitle("Players by position category")+theme(plot.title = element_text(hjust = 0.5))
pp

#As we see previously most players are attacker or defenders.

rp <- ggplot(df,aes(x=region)) + geom_bar(fill="red")+ ggtitle("Players by region")+theme(plot.title = element_text(hjust = 0.5))
rp

#Most PL players are from Europe of from England.



#Foreign players
fp <- ggplot(df,aes(x=new_foreign)) + geom_bar(fill="purple")+ ggtitle("new foreign players")+theme(plot.title = element_text(hjust = 0.5))
fp
#There are few foreign new players this season.


#Age vs Market value
ggplot(df,aes(x=market_value,y=age))+ geom_point()+ ggtitle("Age vs Market Value")+theme(plot.title = element_text(hjust = 0.5))

# Wikipedia page views vs market value
ggplot(df,aes(x=market_value,y=page_views))+ geom_point()+ ggtitle("Wikipedia page views vs Market Value")+theme(plot.title = element_text(hjust = 0.5))
#correlated values


#FPL value vs market value
ggplot(df,aes(x=market_value,y=fpl_value))+ geom_point()+ ggtitle("FPL value vs Market Value")+theme(plot.title = element_text(hjust = 0.5))
#correlated values

#FPL sel vs market value
ggplot(df,aes(x=market_value,y=fpl_sel))+ geom_point()+ ggtitle("FPL selection % vs Market Value")+theme(plot.title = element_text(hjust = 0.5))
#correlated values

#FPL points vs market value
ggplot(df,aes(x=market_value,y=fpl_points))+ geom_point()+ ggtitle("FPL points vs Market Value")+theme(plot.title = element_text(hjust = 0.5))
#correlated values


nums <- unlist(lapply(df, is.numeric))  
X = x[ , nums]

#Correlation Matrix
corr_m <- cor(X)
corr_m

#Our goal is to predict the market value, we will choose the predictors by correlation:
which(corr_m[,3]>0.5)

#Our most correlated variables with market_value are page_views, fpl_value, fpl_points, big_club
#From these variables fpl_value,page_views,fpl_points are very correlated with each other, we will keep fpl_value.
#Candidates:

data = df[,c(8,16,6)]
data 

install.packages("caret")
library(caret)
library(ggplot2)
library(lattice)

particionPL<- createDataPartition(data$market_value, p=0.85, list = FALSE)
PLtraining <- data[particionPL,]
PLtesting <- data[-particionPL,]
