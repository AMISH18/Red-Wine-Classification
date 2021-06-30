## ---- warning = FALSE-----------------------------------------------------------------------------------------

red_wine <- read.csv("D:/MS Business Analytics – University of Cincinnati/Classroom Notes and Exercises/Summer 2021/BANA - Statistical Modelling/Final_Take_Home_Assignment/Dataset/winequality-red.csv", sep = ";")

head(red_wine)



## -------------------------------------------------------------------------------------------------------------
summary(red_wine)



## -------------------------------------------------------------------------------------------------------------
str(red_wine)


## -------------------------------------------------------------------------------------------------------------

red_wine$excellent <- ifelse(red_wine$quality >=7,1,0)

str(red_wine$excellent)


## -------------------------------------------------------------------------------------------------------------
sum(duplicated(red_wine))

library(dplyr)

red_wine1 <- red_wine %>% distinct(fixed.acidity, volatile.acidity, citric.acid, residual.sugar,chlorides, free.sulfur.dioxide, total.sulfur.dioxide, density, pH,sulphates,alcohol,quality,excellent, .keep_all = TRUE)



## -------------------------------------------------------------------------------------------------------------

pie_chart <- round(table(red_wine1$excellent)/length(red_wine1$excellent)*100, 1)
labls <- c("Others", "Excellent")
labls <- paste(labls, pie_chart)
labls <- paste(labls, "%", sep = "")
pie(table(red_wine1$excellent), labels = labls, col = rainbow(length(labls)), main = "Pie chart for Showing Proportion of Excellent Red Wines")




## -------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

p1 <- ggplot(red_wine1, aes(x = fixed.acidity, y = excellent)) + geom_point()
p2 <- ggplot(red_wine1, aes(x = volatile.acidity, y = excellent)) + geom_point()
p3 <- ggplot(red_wine1, aes(x = citric.acid, y = excellent)) + geom_point()
p4 <- ggplot(red_wine1, aes(x = residual.sugar, y = excellent)) + geom_point()

grid.arrange(p1, p2,p3, p4, nrow = 2, ncol = 2)


## -------------------------------------------------------------------------------------------------------------
p5 <- ggplot(red_wine1, aes(x = chlorides , y = excellent)) + geom_point()
p6 <- ggplot(red_wine1, aes(x = free.sulfur.dioxide, y = excellent)) + geom_point()
p7 <- ggplot(red_wine1, aes(x = total.sulfur.dioxide, y = excellent)) + geom_point()
p8 <- ggplot(red_wine1, aes(x = density, y = excellent)) + geom_point()

grid.arrange(p5, p6, p7, p8, nrow = 2, ncol = 2)


## -------------------------------------------------------------------------------------------------------------
p9 <- ggplot(red_wine1, aes(x = pH, y = excellent)) + geom_point()
p10 <- ggplot(red_wine1, aes(x = sulphates, y = excellent)) + geom_point()
p11 <- ggplot(red_wine1, aes(x = alcohol, y = excellent)) + geom_point()

grid.arrange(p9 ,p10, p11, nrow = 2, ncol = 2)


## ---- warning = FALSE-----------------------------------------------------------------------------------------
library(ggpubr)

q1 <- ggplot(red_wine1,aes(x=fixed.acidity,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.5)
q2 <- ggplot(red_wine1,aes(x=volatile.acidity,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.01) 
q3 <- ggplot(red_wine1,aes(x=citric.acid,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.01)
q4 <- ggplot(red_wine1,aes(x= residual.sugar,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.5) 

ggarrange(q1, q2,q3, q4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

## -------------------------------------------------------------------------------------------------------------
q5 <- ggplot(red_wine1,aes(x=chlorides,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.01)
q6 <- ggplot(red_wine1,aes(x=free.sulfur.dioxide,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 1) 
q7 <- ggplot(red_wine1,aes(x=total.sulfur.dioxide,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 5)
q8 <- ggplot(red_wine1,aes(x= density,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.0001) 

ggarrange(q5, q6,q7, q8, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")


## -------------------------------------------------------------------------------------------------------------
q9 <- ggplot(red_wine1,aes(x=pH,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.01)
q10 <- ggplot(red_wine1,aes(x=sulphates,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.01) 
q11 <- ggplot(red_wine1,aes(x=alcohol,fill=as.factor(excellent)))+ geom_histogram(position="dodge",binwidth = 0.1)

ggarrange(q9, q10,q11, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")


## -------------------------------------------------------------------------------------------------------------
model1 <- lm(excellent ~ . -quality, data = red_wine1)  
summary(model1)


## -------------------------------------------------------------------------------------------------------------
model2 <- glm(excellent ~ . -quality , data = red_wine1, family = binomial(link = 'logit'))
summary(model2)


## ---- warning= FALSE------------------------------------------------------------------------------------------
library(corrplot)
library(ggcorrplot)

red_winef <- red_wine[,c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar","chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH","sulphates","alcohol","excellent") ]
corr <- round(cor(red_winef), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)



## -------------------------------------------------------------------------------------------------------------
drop1(model2, test="Chi")


## -------------------------------------------------------------------------------------------------------------
model3 <- glm(excellent ~ . -quality -citric.acid , data = red_wine1, family = binomial(link = 'logit'))



## -------------------------------------------------------------------------------------------------------------

drop1(model3, test="Chi")



## -------------------------------------------------------------------------------------------------------------
model4 <- glm(excellent ~ . -quality -citric.acid -pH, data = red_wine1, family = binomial(link = 'logit'))
drop1(model4, test="Chi")


## -------------------------------------------------------------------------------------------------------------
model5 <- glm(excellent ~ . -quality -citric.acid -pH -free.sulfur.dioxide, data = red_wine1, family = binomial(link = 'logit'))
drop1(model5, test="Chi")


## ---- warning = FALSE-----------------------------------------------------------------------------------------
library(MASS)
stepAIC(model2)


## -------------------------------------------------------------------------------------------------------------

linear_pred <- predict(model5)
pred_prob <- predict(model5, type = "response")

# Predicted outcomes using 0.5 probability
pred_out <- ifelse(pred_prob <0.5, "No", "Yes")

red_wine_c <- data.frame(red_wine1, pred_prob, pred_out)
xtabs(~excellent+pred_out, red_wine_c)



## -------------------------------------------------------------------------------------------------------------

specificity <- (1136/ (1136+39))
sensitivity <- (62/(122 + 62))

print(specificity)
print(sensitivity)



## -------------------------------------------------------------------------------------------------------------
threashold <- seq(0.01, 0.5, 0.01)

sensitivity_1 <- specificity_1 <- rep(NA, length(threashold))

for (j in seq(along = threashold)){
  pp <- ifelse(red_wine_c$pred_prob < threashold[j], "No", "Yes")
  xx <- xtabs(~excellent+pp, red_wine_c)
  specificity_1[j] <- xx[1,1]/(xx[1,1] + xx[1,2])
  sensitivity_1[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(threashold, cbind(sensitivity_1, specificity_1), type = "l", xlab = "Threashold", ylab = "Proportion", lty = 1:2)
plot(1-specificity_1, sensitivity_1, type = "l")




## -------------------------------------------------------------------------------------------------------------

# Predicted outcomes using 0.15 probability
pred_out_final <- ifelse(pred_prob <0.15, "No", "Yes")

xtabs(~excellent+pred_out_final, red_wine_c)



## -------------------------------------------------------------------------------------------------------------
library(pROC)
library(ROCR)

pred_1<- predict(model5, data= red_wine1)
pred_c <- prediction(pred_1, red_wine1$excellent)
performance_m <- performance(pred_c, "tpr", "fpr")
plot(performance_m, colorize=TRUE)
unlist(slot(performance(pred_c, "auc"), "y.values"))



## -------------------------------------------------------------------------------------------------------------
predict(model5, red_wine1[1,], type = "link", se = T)
predict(model5, red_wine1[1,], type = "response", se = T)


## ---- warning = FALSE-----------------------------------------------------------------------------------------
library(faraway)
round(ilogit(c(-4.91055 - 1.96*0.3053951,-4.91055 + 1.96*0.3053951 )), 3)
round(c(0.007314538 - 1.96*0.002217485 ,0.007314538 + 1.96*0.002217485 ), 3)


## -------------------------------------------------------------------------------------------------------------
predict(model5, red_wine1[268,], type = "link", se = T)
predict(model5, red_wine1[268,], type = "response", se = T)


## -------------------------------------------------------------------------------------------------------------
round(ilogit(c(-4.694139  - 1.96*0.2750547,-4.694139  + 1.96*0.2750547)), 3)
round(c(0.009065797  - 1.96*0.002470984  ,0.009065797  + 1.96*0.002470984  ), 3)


## -------------------------------------------------------------------------------------------------------------
red_wine1[1,]$excellent
red_wine1[268,]$excellent


## ---- warning = FALSE-----------------------------------------------------------------------------------------
#Probit Model

red_wine_final <- red_wine1[,c(-12,-14)]

model5_logit <- glm(excellent ~ .-citric.acid -pH -free.sulfur.dioxide , data = red_wine_final, family = binomial(link = 'logit'))

model5_probit <- glm(excellent ~ .-citric.acid -pH -free.sulfur.dioxide , data = red_wine_final, family = binomial(link = 'probit'))

model5_loglog <- glm(excellent ~ . -citric.acid -pH -free.sulfur.dioxide, data =red_wine_final, family = binomial(link = 'cloglog'))



## -------------------------------------------------------------------------------------------------------------

summary(model5_logit)
summary(model5_probit)
summary(model5_loglog)



## -------------------------------------------------------------------------------------------------------------
#Logit
pred_1<- predict(model5_logit, data= red_wine_final)
pred_c <- prediction(pred_1, red_wine_final$excellent)
performance_m <- performance(pred_c, "tpr", "fpr")
plot(performance_m, colorize=TRUE)
unlist(slot(performance(pred_c, "auc"), "y.values"))


## -------------------------------------------------------------------------------------------------------------
#Probit
pred_2<- predict(model5_probit, data= red_wine_final)
pred_c1 <- prediction(pred_2, red_wine_final$excellent)
performance_m1 <- performance(pred_c1, "tpr", "fpr")
plot(performance_m1, colorize=TRUE)
unlist(slot(performance(pred_c1, "auc"), "y.values"))


## -------------------------------------------------------------------------------------------------------------
#cloglog
pred_3<- predict(model5_loglog, data= red_wine_final)
pred_c2 <- prediction(pred_3, red_wine_final$excellent)
performance_m2 <- performance(pred_c2, "tpr", "fpr")
plot(performance_m2, colorize=TRUE)
unlist(slot(performance(pred_c2, "auc"), "y.values"))


## -------------------------------------------------------------------------------------------------------------
predval <- sapply(list(model5_logit, model5_probit, model5_loglog), fitted)
round(predval[fitted(model5_logit) > 0.70,],3)


## -------------------------------------------------------------------------------------------------------------
sigma_squared <- sum(residuals(model5_logit, type = "pearson")^2)/(nrow(red_wine_final)-9)

summary(model5_logit, dispersion = sigma_squared)


## -------------------------------------------------------------------------------------------------------------
red_wine_m <- red_wine1[,c(-13,-14,-15)]

red_wine_m$quality_f <- as.factor(red_wine_m$quality)
red_wine_m$quality <- as.numeric(red_wine_m$quality)
levels(red_wine_m$quality_f)
hist(red_wine_m$quality, breaks = 4)



## -------------------------------------------------------------------------------------------------------------
corr1 <- cor(red_wine_m$quality, red_wine_m[, c(-12, -13)], method = c("kendall"))
rank <- corr1[, order(abs(corr1), decreasing = TRUE)]
rank <- data.frame(rank)
rank


## ---- warning = FALSE-----------------------------------------------------------------------------------------
library(VGAM)
model6 <- vglm(quality ~ .  -pH -quality  -quality_f -citric.acid - fixed.acidity - residual.sugar - density - free.sulfur.dioxide, family = cumulative(parallel = TRUE), red_wine_m)

summary(model6)



## -------------------------------------------------------------------------------------------------------------

# 1st bottle
a <- predictvglm(model6, type = "response", newdata = as.data.frame(red_wine_m[1,]))
a
prob_atleast_7_1 <- sum(a[,"7"], a[,"8"])
paste("Probability for 1st bottle is:", round(prob_atleast_7_1,4))

#268th bottle
b <- predictvglm(model6, type = "response", newdata = as.data.frame(red_wine_m[268,]))
b
prob_atleast_7_268 <- sum(a[,"7"], a[,"8"])
paste("Probability for 1st bottle is:", round(prob_atleast_7_268,4))



## -------------------------------------------------------------------------------------------------------------
predict(model5, red_wine1[1,], type = "response")
predict(model5, red_wine1[268,], type = "response")

