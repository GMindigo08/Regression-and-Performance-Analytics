#I will start by reading in a data set about parkinsons
parkinsons = read.csv(file.choose(),header=TRUE)


#here we see a list of the names in the data set and the subsequent data tied
#to the names
names(parkinsons)
head(parkinsons)


#Now I am writing a univariate linear model between total_UPDRS and motor_UPDRS.
#We see a clear positive correlation between the two variables
plot(parkinsons$motor_UPDRS,parkinsons$total_UPDRS)


#Viewing residuals and coefficients:
#The Max residuals are pretty high, as are the Min residuals. This shows an 
#offset in the fringes of the data. The median is very close to 0, which shows
#a fairly even distribution (at face value)
#The adjusted R squared is close to a value of 1
modelmvt=lm(total_UPDRS~motor_UPDRS,data=parkinsons)
summary(model)


#Adding the regression line to the plot
abline(coef=coef(modelmvt))


#Generating additional plots to assess the data
#There is clear heteroskedasticity, as there is variation on the horizontal axis
#The Q-Q Residuals plot shows a lack of variability on the fringes and specifically 
#in the 2-3 quartiles of the plot
dev.new()
plot(modelmvt)


#Now I am finding correlation with all other subgroups of data for total and
#motor (multivariate)
#I am omitting the "subject" and "sex" variables from the plot
#You will see that there is no clear correlation between total/motor and the 
#other data. There is quite a bit more variation (heteroskedasticity) in the 
#generated plots. In the summary of these models, I observed an incredibly low R 
#squared value and very large Max and Min values. This model is clearly not as
#informative in predicting total and motor as the previous model was. 

#total_UPDRS v. all
modeltvall=lm(total_UPDRS~age+test_time+Jitter...+Jitter.Abs.+Jitter.RAP+
                Jitter.PPQ5+Jitter.DDP+Shimmer+Shimmer.dB.+
            Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA
            +PPE,data=parkinsons)
dev.new()
plot(modeltvall)
summary(modeltvall)

#motor_UPDRS v. all
modelmvall=lm(motor_UPDRS~age+test_time+Jitter...+Jitter.Abs.+Jitter.RAP+
                Jitter.PPQ5+Jitter.DDP+Shimmer+Shimmer.dB.+
            Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA
            +PPE,data=parkinsons)
dev.new()
plot(modelmvall)
summary(modelmvall)


#Now I am unnecessarily plotting total and motor with every other variable
#individually to check my work. 
#We clearly see that there is no correlation on an individual basis
plot(total_UPDRS~age+test_time+Jitter...+Jitter.Abs.+Jitter.RAP+
       Jitter.PPQ5+Jitter.DDP+Shimmer+Shimmer.dB.+
       Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA
     +PPE,data=parkinsons)
plot(motor_UPDRS~age+test_time+Jitter...+Jitter.Abs.+Jitter.RAP+
       Jitter.PPQ5+Jitter.DDP+Shimmer+Shimmer.dB.+
       Shimmer.APQ3+Shimmer.APQ5+Shimmer.APQ11+Shimmer.DDA+NHR+HNR+RPDE+DFA
     +PPE,data=parkinsons)


#Now I am writing a function that will install a group of packages to produce two
#new models that are much better at comparing the bulk data
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

packages <- c("Hmisc","corrplot","PerformanceAnalytics","dominanceanalysis","pscl")

ipak(packages)

library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(pscl)
library(dominanceanalysis)

#Here is the correlation plot of all the data from "parkinsons" 
#We see in this plot that total_UPDRS and motor_UPDRS have a strong positive 
#correlation with each other (indicated by the large, dark blue circle), but not
#so much with the other variables. This model is useful in the visualization of the 
#data. We can also see how the other variables interact with one another. 
correlationMatrix=cor(parkinsons,method="pearson",use="complete.obs")
corrplot(correlationMatrix,type="upper",order="original",tl.col="black",tl.srt=45)

#Now I will develop a Performance Analytics plot to further explore this data
#Again, total and motor are directly correlated
#The regression lines of total/motor with the other variables is flat in all cases,
#showing us that there is no predictability.
corrplot(correlationMatrix,type="upper",order="original",tl.col="black",tl.srt=45)
chart.Correlation(parkinsons,histogram=TRUE,pch=19)