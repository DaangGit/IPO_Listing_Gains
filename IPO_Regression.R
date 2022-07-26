ipo <- read.csv("IPOData.csv")
# View(ipo)

#Model

r <- lm(I(RET*100) ~ (QIB)+(NII)+(RII)+(EPS)+(P.E.D)+(NAV), data=ipo)
summary(r)


library(car)
library(lmtest)

#Multicoll
vif(r)

#Heteroskedasticity (If p value < 0.05, not homoscedastic)
bptest(r)

#RESET Test
resettest(r, power = 2, type = "regressor")

#ROBUST STANDARD ERRORS
cov1 <- hccm(r, type="hc1")
r.hc1 <- coeftest(r, vcov.=cov1)
r.hc1

#Testing Exogeneity
mean(resid(r))
cor(resid(r),ipo$QIB)
cor(resid(r),ipo$NII)
cor(resid(r),ipo$RII)
cor(resid(r),ipo$EPS)
cor(resid(r),ipo$P.E.D)
cor(resid(r),ipo$NAV)
cor(resid(r),ipo$RoNW)

#Durbin Watson Test for Corr (p value < 0.05, autocorrelation)
durbinWatsonTest(r)

# # Plots for functional form
# plot(ipo$TOT, ipo$RET, xlab = 'Total Subscription', ylab = "Returns", abline(h=0, v=0))
# plot(ipo$P.E.D, ipo$RET, xlab = 'Price to Earnings', ylab = "Returns", abline(h=0, v=0))
# plot(ipo$RoNW, ipo$RET, xlab = 'Return on Net Worth', ylab = "Returns", abline(h=0, v=0))
# plot(ipo$NAV, ipo$RET, xlab = "Net Asset Value", ylab = "Returns", abline(h=0, v=0))
# plot(ipo$P.B, ipo$RET, xlab = "Price to Book Ratio", ylab = "Returns", abline(h=0, v=0))
# plot(ipo$EPS, ipo$RET, xlab = "Earnings Per Share", ylab = "Returns", abline(h=0, v=0))

# # Desc Stats
library(summarytools)
dfdesc <- data.frame(ipo$RET*100,ipo$QIB,ipo$NII,ipo$RII,ipo$EPS,ipo$P.E.D,ipo$NAV)
colnames(dfdesc) <- c('Returns','QIB','NII','RII','EPS','PE','NAV')
descr(dfdesc, stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr"),transpose = TRUE)