credit_cleaned <- read.table(file="credit_clean.txt", header = TRUE)

attach(credit_cleaned)
fivenum(LIMIT)

install.packages("mosaic")
library(mosaic)
fivenum(LIMIT)

bwplot(~LIMIT,  pch = "|")
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
bwplot(LIMIT, pch = "|",
       panel = function(...) {
         panel.bwplot(...)
         panel.mean(...)
       })

histogram(~LIMIT,type='density',
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

mean(LIMIT)
sd(LIMIT)

factor(credit_cleaned$EDUCATION)
B = table(credit_cleaned$EDUCATION)
pie(B, main="Education", col=rainbow(length(B)), labels=c("6", "Graduate","HighSchool","Other","University"))
