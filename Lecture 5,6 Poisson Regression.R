
library(ggplot2)
library(dplyr)
library(AER)
library(gridExtra)
set.seed(2708)
g1 <- ggplot()+geom_histogram(aes(x=rpois(n=1000, lambda=1))) + labs(x="", title="Histogram 1")
g2 <- ggplot()+geom_histogram(aes(x=rpois(n=1000, lambda=2))) + labs(x="", title="Histogram 2")
g3 <- ggplot()+geom_histogram(aes(x=rpois(n=1000, lambda=10))) + labs(x="", title="Histogram 3")
grid.arrange(g1,g2,g3, nrow=1)
dpois(x=2, lambda=2.7)   
ar <- read.csv("articles.csv")
str(ar)
ar$fem <- factor(ar$fem, levels = c(0,1), labels = c("male", "female"))
ar$mar <- factor(ar$mar, levels = c(0,1), labels = c("else", "married"))
unique(ar$kid5)
unique(ar$art)
ggplot(data = ar, aes(x=art))+geom_histogram()+ labs(x= "# of articles")
ggplot(data = ar, aes(x=fem, y=art))+
  geom_boxplot()+
  labs(x="Gender", y="# of Articles",title=" The Relationship with gender")
ar %>% group_by(fem)%>%
  summarise(mean(art))

ggplot(ar, aes(x=art, fill = fem))+
  geom_histogram(position = "dodge")+
  labs(x="", y="# of Articles",title="By gender")
ggplot(data = ar, aes(x = mar, y = art))+geom_boxplot()+
  labs(x="Marital Status", y="# of Articles",title=" The Relationship with marital status")
ar %>% group_by(mar)%>%
  summarise(mean(art))
ggplot(data = ar, aes(x = factor(kid5), y=art))+
  geom_boxplot()+
  labs(x="Number of kids", y="# of Articles",title=" The Relationship with Kid 5")
ggplot(data = ar, aes(x=phd, y=art))+geom_point()+
  labs(x="Prestige", y="# of Articles",title=" The Relationship with Prestige")
summary(ar$phd)
phddummy <- factor(ifelse(ar$phd>3.1, 1,0))
ggplot(data = ar, aes(x=phddummy, y=art))+geom_boxplot()+
  labs(x="Prestige", y="# of Articles",title=" The Relationship with Prestige")
ggplot(data = ar, aes(x=ment, y=art))+geom_point()+
  labs(x="Mentors' articles", y="# of Articles",title=" The Relationship with MAs")
mod <- glm(art~1, data = ar, family = poisson(link = log))
summary(mod)
min(mod$deviance)
mean(ar$art)
exp(mod$coefficients)
mod1 <- glm(art~., data = ar, family = poisson(link = log))
summary(mod1)
nd <-data.frame(fem = "female", mar = "married", kid5=0, phd=2, ment=0)
predict(mod1, newdata = nd)
lambda <- predict(mod1, newdata = nd, type="response")
dpois(4, lambda = 1.3)
ppois(4, lambda = 1.3, lower.tail = T )
ppois(4, lambda = 1.3, lower.tail = F )
chi_sq <- sum (resid(mod1, type = "pearson")^2)
pchisq(chi_sq, df = df.residual(mod1), lower.tail = F)
dev_chisq <- sum(resid(mod1, type = "deviance")^2)
dev_chisq
pchisq(dev_chisq, df = df.residual(mod), lower.tail = F)
1-mod1$deviance/mod1$null.deviance
var(ar$art)
mean(ar$art)
str(ar)
ar %>% group_by(kid5) %>%
  summarise(var = var(art), mean=mean(art))
mod2<- glm(art~kid5, data=ar, family = poisson(link = log))
summary(mod2)
library(AER)
dispersiontest(mod2, trafo = NULL)
mod_qp <- glm(art~kid5, data = ar, family = quasipoisson(link=log))
library(MASS)
mod_nb <- glm.nb(art~kid5, data = ar)
summary(mod_qp)
summary(mod_nb)
data.frame(coef(mod2),coef(mod_qp),coef(mod_nb))
mod1 <- glm(art~., data = ar, family = poisson(link = log))
summary(mod1)
dispersiontest(mod1)
mod_f_qp <-glm(art~., data = ar, family = quasipoisson(link = log))
summary(mod_f_qp)
mod_f_nb <- glm.nb(art~., data = ar)
summary(mod_f_nb)
deviance(mod1);deviance(mod_f_nb);deviance(mod_f_qp)
pchisq(mod1$deviance, df = df.residual(mod1), lower.tail = F)
pchisq(mod_f_qp$deviance, df = df.residual(mod_f_qp), lower.tail = F)
pchisq(mod_f_nb$deviance, df = df.residual(mod_f_nb), lower.tail = F)
mod1$aic
mod_f_nb$aic
mod_f_qp$aic
mod_f_nb2 <- glm.nb(art~fem+kid5+ment, data = ar)
summary(mod_f_nb2)
exp(coef(mod_f_nb2))