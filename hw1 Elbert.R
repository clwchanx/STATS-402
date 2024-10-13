library(car)
setwd("C:\\Users\\Elbert\\OneDrive - UCLA IT Services\\402")
df <- read.csv("campusclimate.csv")

#Problem One
clean <- df[,c('friendlyenvp','academicenvp')]
clean <- clean[complete.cases(clean),]

#a
plot(clean$friendlyenvp,clean$academicenvp) 

#b need help
scatterplot(friendlyenvp ~ academicenvp,data=clean,span=0.6)

#c 
subset <- clean[(clean$friendlyenvp>=40) & (clean$academicenvp>=10),]

#d
m1 <- lm(academicenvp ~ friendlyenvp, data=clean)
m2 <- lm(academicenvp ~ friendlyenvp, data=subset)
summary(m1)
summary(m2)
#slope - while both models produced statistically significant statistics
#full data set's predicted model has a positive coefficient versus a negative coefficient for the subset.
#R squared increased for the subset, showing a greater explanatory power of friendlyenvp for academicenvp.

#e
#slope: given that friendlyenvp < 40 or academic envp <10. A unit increase in friendlyenvp results in 1.05 unit decrease in academicenvp
#intercept: the theoretical value of academicenvp when friendlyenvp is 0
#R-sqr: 23.19% of the variability of academic envp is explained by friendlyenvp

#f
plot(m2)
hist(subset$friendlyenvp)
hist(subset$academicenvp)

#g
#to get residual vs fitted, just use first plot of plot(m2)
plot(m2)
#to get residual vs friendlyenvp, use resid to get the residual vector then plot
plot(subset$friendlyenvp,resid(m2))

#f
ncvTest(m2)
#p value is .2075. fail to reject null hypothesis

#Problem Two
#1
#It means that at each given independent variable value, the variance of the residual is the same.
#Another way to think of it is, the variance of the residual is independent from the independent variable.

#2
#This means that the regression line is the line that minimizes the sum of squared residuals.

#3
#Y hat is a linear transformation of X, meaning its variability is fully explained by x.
#Thus the two plot demonstrate the same variability, which is the variability of X.
#Mathematically: Cov(Yhat,resid) = Cov((b0+b1*x),resid)=sum((b0+b1*x-b0-b1*xbar)*(resid - residbar))/n-1
#=sum((b1(x-xbar))*(resid-residbar))/n-1 = b1*sum((x-xbar)*(resid-residbar))/n-1 = b1*cov(x,resid)

#4


#Problem Three
#1 No
#Mathematically: Correlation is a better measurement of relationship
#Corr = Cov(X,Y)/(Std(X)*Std(Y)). Just because Cov for that researcher B found is higher, doesn't necessarily mean corr for B is higher
#We can have higher Std for math and physics score in researcher B's sample and potentially result in lower correlation.
#Conceptually: A sample size of 100 is relatively small and variability in sample statistics might be significant.
#Researcher B could have gotten a more polarized group, with higher percentage of high scorer and low scoerer.
#In this scenario, even though the correlation between math score and physics score doesn't change, covariance can be inflated.

#2 Because correlation coefficient ranges from [-1,1] and can be rewritten as sum(z(X)*z(Y)) / (n-1).
#Z scores are standardized measurement of distance from mean, and is not dependent on the units of the original data.
#Thus whatever data is fed into the correlation calculation, gets standardized to z scores and is derived from covariance.

#Problem Four
attach(subset)
#a
#numbers for friendlyenvp
mf = mean(friendlyenvp,na.rm=TRUE)
vf = var(friendlyenvp,na.rm=TRUE)
sf = sd(friendlyenvp,na.rm=TRUE)
#numbers for academicenvp
ma = mean(academicenvp,na.rm=TRUE)
va = var(academicenvp,na.rm=TRUE)
sa = sd(academicenvp,na.rm=TRUE)

#b
covar = cov(friendlyenvp,academicenvp)
corr = cor(friendlyenvp,academicenvp)

#c
data <- c(friendlyenvp,academicenvp)
m <- lm(academicenvp~friendlyenvp)
summary(m)
df=2951+1

SST = va *df
SSX = vf *df
SSR= TSS*(1-corr^2)
sd_slope = sqrt(TSS/(df-1))/sqrt(SSX)
slope = covar/vf
t = slope/sd_slope

F = ((SST-SSR)/1)/(SSR/df-1)

#d
