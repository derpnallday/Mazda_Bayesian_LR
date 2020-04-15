#Warm-Up: Linear Regression Pre-Lab
library(ggplot2)

#This dataset was collected in 1993. Use transform() to append a column with the age of the car to the dataframe.

myData = read.csv("mazdas.csv")
attach(myData)
myData = transform(myData, age = 93-ï..Year)

myData

#Conduct some preliminary data exploration. Are there any transformations that will be necessary? 

par(mfrow=c(1,2))
#How will that impact our model and subsequent analysis?
lattice::xyplot(age~Price, data = myData,xlab = "Price ($)",
                ylab = "age (yr)",
                type = c("p","r"),
                main = "Price by age")



slrFisher = lm(age~Price, data = myData)
summary(slrFisher)
confint(slrFisher)

#looks like a logrithimic curve along price
lattice::xyplot(age~log10(Price), data = myData,xlab = "Price ($)",
                ylab = "age (yr)",
                type = c("p","r"),
                main = "log Price by age")

#log transform looks better

logSlrFisher = lm(age~log10(Price), data = myData)
summary(logSlrFisher)
confint(logSlrFisher)

attach(myData)
myData = transform(myData, logPr = log10(Price))

#look at variable dist

par(mfrow=c(1,3))
plot(density(myData$Price), main = "Price")
plot(density(myData$logPr), main = "log10(Price)")
plot(density(myData$age) , main = "age")
 
#------------------------------------------------------------------------------- 
       
xName="age"
yName="logPr"
fileNameRoot = "Mazdas-Age-SLR-" 
graphFileType = "eps" 

source("Jags-Ymet-Xmet-Mrobust.R")

mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , 
                    numSavedSteps=20000 , saveName=fileNameRoot )


#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , 
          #compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

#Find median prices over year
dep_rate =10^-0.0721
base = 10^4.58

#to store relevant vars
med_prices = integer(23)
ages = integer(23)
round_prices = integer(23)
spacing = integer(23)

#transform all values back to base form
for (i in 0:23){
  med_prices[i+1] = base * (dep_rate)^i
  ages[i+1] = i
  round_prices[i+1] = round(med_prices[i+1], digits = 0)
  spacing[i+1] = 0.8 + 1.2*i
}

#new df of med price against age
df = data.frame("age" = ages,
                "med_price" = med_prices)

par(mfrow=c(1,1))

#plot price by age
barplot(df$age, 
        height = df$med_price,
        ylim=c(0,42000),
        xlim=c(0,30),
        width=0.8,
        space = 0.5,
        xlab="age(yr)",
        ylab="median price($)")

text(spacing,round_prices+3000,labels=as.character(round_prices))