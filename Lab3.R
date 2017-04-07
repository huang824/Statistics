RandomData <- rnorm(100, mean=10, sd=20)
RandomData
mean(RandomData)
sd(RandomData)
title = "RandomData"

library(mosaic)
histogram(~RandomData,type='density', main = title,
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

qqmath(~RandomData, main = title,
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

n = 100
right <- rexp(n,rate=2)
right
histogram(~right,type='density', main = title,
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

qqmath(~right, main = title,
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

left <- rbeta(n,2,0.5,ncp=2)
left
histogram(~left,type='density', main = title,
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

qqmath(~left, main = title,
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

short <- runif(n,min=0,max=2)
short
histogram(~short,type='density', main = title,
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

qqmath(~short, main = title,
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

long <- rcauchy(n,location=0,scale=1)
long
histogram(~long,type='density', main = title,
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

qqmath(~long, main = title,
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

attach(credit_cleaned)
histogram(~BILL2,type='density', main = "BILL2",
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

qqmath(~BILL2, main = "BILL2",
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })
