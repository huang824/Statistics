library(mosaic)
attach(credit_cleaned)
newBill <- subset(credit_cleaned, SEX == "M" | SEX == "F")
logBILL=log(newBill$BILL)

histogram(~logBILL | SEX, layout=c(1,2),type="density",
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

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
5bwplot(~log(studynew$BILL) | SEX, pch = "|", layout = c(1,2),
       panel = function(...) {
         panel.bwplot(...)
         panel.mean(...)
       })

qqmath(~logBILL | SEX, panel = function(x){
  panel.qqmath(x)
  panel.qqmathline(x)
})

t.test(logBILL ~SEX, conf.level-0.95, paired=F, alternative = "two.sided", var.equal=F)

newPay <- subset(credit_cleaned, PAY1 > 0)
dif = newPay$PAY1-newPay$BILL1
histogram(~dif,type="density",
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

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
bwplot(dif, pch = "|",
       panel = function(...) {
         panel.bwplot(...)
         panel.mean(...)
       })

qqmath(~dif , panel = function(x){
  panel.qqmath(x)
  panel.qqmathline(x)
})

t.test(newPay$PAY1, newPay$BILL1,conf.level=0.99, mu=-20000, paired = TRUE, alternative =
         "less")
t.test(dif, conf.level=0.99, mu=-20000, alternative="less")
