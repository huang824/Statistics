library(mosaic)
attach(credit_cleaned)
histogram(~BILL,type='density',
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

bwplot(~BILL,  pch = "|")
  panel.mean <- function(x,y,...){
  y <- as.numeric(y)
  y.unique <- sort(unique(y))
  for(Y in y.unique) {
    X <- x[y == Y]
    if (!length(X)) next
    mean.value <- list(x = mean(X), y = Y)
    do.call("lpoints", c(mean.value, pch = 19))
  }
}
bwplot(BILL, pch = "|",
       panel = function(...) {
         panel.bwplot(...)
         panel.mean(...)
       })

qqmath(~BILL, main = 'Normal quantile plot of BILL',
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

logBILL=log(BILL)
histogram(~logBILL,type='density',
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })
bwplot(~logBILL,  pch = "|")
panel.mean <- function(x,y,...){
  y <- as.numeric(y)
  y.unique <- sort(unique(y))
  for(Y in y.unique) {
    X <- x[y == Y]
    if (!length(X)) next
    mean.value <- list(x = mean(X), y = Y)
    do.call("lpoints", c(mean.value, pch = 19))
  }
}
bwplot(logBILL, pch = "|",
       panel = function(...) {
         panel.bwplot(...)
         panel.mean(...)
       })

qqmath(~logBILL, main = 'Normal quantile plot of BILL',
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

mean(logBILL)
sd(logBILL)
sd(logBILL)/sqrt(length(logBILL))

t.test(~logBILL, conf.level=0.95, mu = 11.92, alternative = "greater")
t.test(~logBILL, conf.level=0.95, mu=11.92, alternative="two.sided")
