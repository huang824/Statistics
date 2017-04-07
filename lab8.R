attach (credit_cleaned)
library(mosaic)

studynew <- subset(credit_cleaned, EDUCATION == "Univ" | EDUCATION == "Grad" | EDUCATION == "High")
studynew <- droplevels(studynew)

histogram(~log(studynew$BILL) | studynew$EDUCATION, layout=c(1,3),type="density",
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
bwplot(~log(studynew$BILL) | studynew$EDUCATION, pch = "|", layout = c(3,1),
       panel = function(...) {
         panel.bwplot(...)
         panel.mean(...)
       })

qqmath(~log(studynew$BILL) | studynew$EDUCATION, panel = function(x){
  panel.qqmath(x)
  panel.qqmathline(x)
})

trace <- rep(1, length(studynew$EDUCATION))
interaction.plot(studynew$EDUCATION, trace, log(studynew$BILL), fun=mean, legend=F)
interaction.plot(studynew$EDUCATION, trace, studynew$BILL, fun=mean, legend=F)
tapply(studynew$BILL, studynew$EDUCATION, length)
tapply(studynew$BILL, studynew$EDUCATION, mean)
tapply(studynew$BILL, studynew$EDUCATION, sd)
tapply(log(studynew$BILL), studynew$EDUCATION, mean)
tapply(log(studynew$BILL), studynew$EDUCATION, sd)

fit <- aov(log(BILL) ~ EDUCATION, data=studynew)
summary(fit)
test.Tukey <- TukeyHSD(fit, conf.level=0.9)
test.Tukey

install.packages("multcomp")
library(multcomp)
Dunnet = glht(fit, linfct = mcp(EDUCATION = "Dunnett"))
summary(Dunnet)
