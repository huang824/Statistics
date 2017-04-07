library(mosaic)
credit <- subset(credit_cleaned, BILL1 > 60000 & BILL1 < 800000)
attach(credit)
xyplot(PAY2 ~ BILL1,
       data = credit,
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       })
cor(BILL1, PAY2)
credit.lm = lm(PAY2 ~ BILL1)
summary(credit.lm)

credit.resid = credit.lm$res
xyplot(credit.resid ~ BILL1,
       data = credit,
       main="Residual plot",
       ylab = "Residual",
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.abline(h = 0)
       })

histogram(~credit.resid,type='density',
          panel=function(x,...)
          {panel.xhistogram(x,density=TRUE)
            panel.densityplot(x,col="red",lwd=2,...)
          })

qqmath(~credit.resid, main = 'Normal quantile plot of Residual',
       panel = function(x){
         panel.qqmath(x)
         panel.qqmathline(x)
       })

confint(credit.lm, level = 0.99)

x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, interval = "prediction")
predict(lm(y ~ x), new, interval = "confidence")

newdata = data.frame(x = 244663)
predict(credit.lm, newdata, interval = "confidence", level = 0.99)
predict(credit.lm, newdata, interval = "predict", level = 0.99)
