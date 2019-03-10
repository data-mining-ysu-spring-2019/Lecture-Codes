library(ggplot2)
library(ggpubr)
housedata<-read.csv("housing.csv")
str(housedata)

#Preparing the data

housedata$waterfront<-factor(housedata$waterfront, levels=c(0,1), labels=c("does_not_look", "looks_at"))
housedata$view<-factor(housedata$view, levels = c(0,1,2,3,4), labels=c("awful", "not_so_good", "fair", "good", "excellent"))

housedata$condition<-factor(housedata$condition, levels=c(1,2,3,4,5), labels=c("need_to_be_repaired", "fair", "average", "good", "excellent"))
housedata$grade<-cut(housedata$grade, breaks=c(0,3,6,7,10,  Inf), labels=c("short", "fair", "average","good", "high" ))
housedata$years_since_building<-2019- housedata$yr_built
str(housedata[,c(9,10,11,12,21)])

#Visualisation of SSE 

set.seed(2708)
sam<-sample(nrow(housedata), floor(nrow(housedata)*0.03))
forvisualisation<-housedata[sam, ]

attach(forvisualisation)
model3 <- lm(price ~ sqft_living)


ggplot(forvisualisation, aes(x=sqft_living, y=price))+
  geom_point(alpha=0.6, col="coral")+
  geom_smooth(method = "lm", se = FALSE)+
  geom_segment(aes(xend = sqft_living, yend = predict(model3)),alpha=0.2) 


#Why we say in avarage

ggplot(forvisualisation, aes (y=price, x=sqft_living))+geom_point()+
  geom_vline(xintercept = mean(forvisualisation$sqft_living))

#Regression without explanatory variables

model0<-lm(price~1, data=housedata)
summary(model0)

#compute coefficient by hand 
#
#
#

#Zero slope model

ggplot(forvisualisation, aes (y=price/1000, x=sqft_living))+geom_point()+
  geom_hline(yintercept = mean(forvisualisation$price/1000), col="red", size=1.5)


attach(housedata)
mod1<-lm(price~sqft_living, data=housedata)
summary(mod1)

min(resid(mod1))
max(resid(mod1))
median(resid(mod1))
quantile(resid(mod1),probs = c(0.25, 0.75) )
sum((resid(mod1)^2))
RSE<-sqrt(sum((resid(mod1)^2))/(dim(housedata)[1]-2))
RSE
summary(mod1)$sigma 
bse<-RSE/sqrt(sum((sqft_living-mean(sqft_living))^2))
bse

#compute t , pvalue, R^2
#
#
#

Fstat<-t^2
Fstat
p.valuef<-dt(Fstat, df = 21613-2)
p.valuef
confint(mod1)
lolim=mod1$coefficients[2] - 1.96*bse
uplim=mod1$coefficients[2] + 1.96*bse
cbind(lolim,uplim)


#Problem of interpretation.
model1.0<-lm(log(price)~bedrooms, data=housedata)
summary(model1.0)

#Prediction
newd <- data.frame(bedrooms=c(5,6,7))
predict(model0, newdata = newd)
head(residuals(model0), 6)
summary(model0)$r.squared

library(readr)
Advertising <- read_csv("Advertising.csv")
ad<-data.frame(Advertising[,-1])

#Multiple Linear Regression
#lm(y~x1+x2+x3)
attach(ad)
mod2 <- lm(Sales ~ Newspaper)
mod2
summary(mod2)
mod3<-lm(Sales ~ Radio)
mod3
summary(mod3)
mod4<-lm(Sales ~., data=ad)
mod4
summary(mod4)

r1=summary(mod1)$r.sq
rse1=summary(mod1)$sigma #RSE

r2=summary(mod2)$r.sq
rse2=summary(mod2)$sigma #RSE

r3=summary(mod3)$r.sq
rse3=summary(mod3)$sigma #RSE

r4=summary(mod4)$r.sq
rse4=summary(mod4)$sigma #RSE

r<-c(r1,r2,r3,r4)
rse<-c(rse1,rse2,rse3,rse4)
data.frame(r,rse)

cor(ad)

#Qualitative Predictors
set.seed(1)
x <- sample( LETTERS[1:2],200, replace=TRUE)
prop.table(table(x))
ad1<-ad
ad1$x<-factor(x)
head(ad1)
mod5<-lm(Sales~x, data=ad1)
summary(mod5)
set.seed(2)
y<-sample( LETTERS[1:3], 200, replace=TRUE, prob=c(0.3, 0.65, 0.05) )
prop.table(table(y))
contrasts(factor(y))
mod6<-lm(Sales~factor(y), data=ad1)
summary(mod6)

#Additive Assumption
modinter<-lm(Sales~TV+Radio+TV*Radio, data=ad)
summary(modinter)
coef(modinter)

#interation term+categorical
mod7=lm(Sales~Radio+x, data=ad1)
summary(mod7)
mod8=lm(Sales~Newspaper+x+Newspaper:x, data=ad1)
summary(mod8)

#Non-linear transfromations of the Predictors 
#I(x^2)+I(x^3)+I(x^4) or poly(x1, 5) -->
modsq <- lm(formula = Sales~TV+I(TV^2), data=ad) 
summary(modsq)
modlog <- lm(formula = Sales~log(TV), data=ad) 
summary(modlog)
modpoly<-lm(formula = Sales~poly(TV,5)) 
summary(modpoly)
