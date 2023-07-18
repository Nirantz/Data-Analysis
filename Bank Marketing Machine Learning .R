library(rpart)
library(rpart.plot)
library(caret)
library(corrplot)
library(ggplot2)

bank.df <- read.csv("bank_mark.csv")
View(bank.df)

#bank.df.sampled <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*1)  
#bank.df.sampled <- bank.df.sampled[1:2000 , ]

train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  

train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]
View(train.df)
#----- covert y -> 0 and 1
#train.df$y[train.df$y == "yes"] <- as.numeric(1)
#train.df$y[train.df$y == "no"] <- as.numeric(0)
train.df[train.df$y<-ifelse(train.df$y=="yes",1,0),]
train.df[train.df$default<-ifelse(train.df$default=="yes",1,0),]
train.df[train.df$housing<-ifelse(train.df$housing=="yes",1,0),]
train.df[train.df$loan<-ifelse(train.df$loan=="yes",1,0),]

valid.df[valid.df$y<-ifelse(valid.df$y=="yes",1,0),]
valid.df[valid.df$default<-ifelse(valid.df$default=="yes",1,0),]
valid.df[valid.df$housing<-ifelse(valid.df$housing=="yes",1,0),]
valid.df[valid.df$loan<-ifelse(valid.df$loan=="yes",1,0),]
View(train.df)
#plot(train.df$duration, train.df$y)
#abline(glm(formula = duration ~ y, data = train.df), col = 'blue')
ggplot(train.df, aes(x=duration, y=y)) + geom_point() +
  stat_smooth(method="glm", color="slateblue", se=FALSE,
              method.args = list(family=binomial))
ggplot(train.df, aes(x=balance, y=y)) + geom_point() +
  stat_smooth(method="glm", color="blue", se=FALSE,
              method.args = list(family=binomial))
ggplot(train.df, aes(x=age, y=y)) + geom_point() +
  stat_smooth(method="glm", color="red", se=FALSE,
              method.args = list(family=binomial))
ggplot(train.df, aes(x=previous, y=y)) + geom_point() +
  stat_smooth(method="glm", color="slateblue", se=FALSE,
              method.args = list(family=binomial))

#logistic summary 
summary(glm(formula = y ~ ., family = "binomial", data = train.df))
summary(glm(formula = y ~ duration+previous+age+balance+housing+loan, family = "binomial", data = train.df))


# only for numeric values
#Cor.matrix = cor(cereals.df[,4:16], use="complete.obs")
#corrplot(Cor.matrix, method="color",main="Correlation Matrix")
# correlatio plot for numeric variable in dataset 
Cor.matrix = cor(train.df[ ,c(1,6,7,8,12,13,14,15,17)], use="complete.obs")
corrplot(Cor.matrix, method="color",main="Correlation Matrix")
?corrplot
logit.reg <- glm(y ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)
#
logit.reg <- glm(y ~ +job+marital+education+duration+previous+age+balance+housing+loan, data = train.df, family = "binomial") 
# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

options(scipen=999)
summary(logit.reg.pred)

log.pred.train = predict(logit.reg, train.df, type = "response")
log.pred.train = ifelse(log.pred.train > 0.5, 1, 0)
confusionMatrix(as.factor(log.pred.train), as.factor(train.df$y))

log.pred.test = predict(logit.reg, valid.df, type = "response")
log.pred.test = ifelse(logit.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(log.pred.test), as.factor(valid.df$y))


quantile(bank.df$duration)
#
library(plotrix)
pie3D(p)

p = as.data.frame(table(bank.df$y))
colnames(p) <- c("y", "freq")

pie <- ggplot(p, aes(x = "", y=freq, fill = factor(y))) +
   geom_bar(width = 1, stat = "identity") +
   labs(fill="y", x=NULL, y=NULL, title="Pie Chart")

pie + coord_polar(theta = "y", start=0)



#
ggplot(p, aes(x = "", y = freq, fill = factor(y))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0)  +
  labs(x = "", y = "", title = "Pie Chart",
       fill = "Subscription Status") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10))