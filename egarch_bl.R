
library(rugarch)
library(quantmod)
library(FinTS)
library(zoo)
library(e1071)
library(stats)
library(tseries)
library(forecast)
library(fBasics)
library(qqplotr)
library(ggplot2)
library(xts)
library(utilities)
setwd('C:/Users/eapst/Desktop/ÊÓĞÑÀ×/data_monthly')


oil.price = read.csv("brent_daily_clean.csv", sep = ",")
oil.price$Date = as.Date(oil.price$Date)
oil.price = oil.price[order(oil.price$Date), ] 
xts_oil = xts(oil.price$Change, order.by = oil.price$Date)


data = read.csv("ALRS_month_13_21_clean.csv")
data = data[order(data$Date), ] 


train_set = subset(data, Date> "2013-01-01" & Date < "2019-01-01")
test_set = subset(data, Date > "2019-01-01")
xts_all_data = xts(x=data$Log_return, order.by=as.Date(data$Date))
xts_afks <- xts(x=train_set$Log_return, order.by=as.Date(train_set$Date))

mean(xts_afks)
sd(xts_afks)
auto.arima(xts_afks)

#model = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
#                   distribution.model = "sstd", 
#                   mean.model=list(armaOrder=c(2,2), archm = TRUE,
#                                   include.mean = TRUE ),
#                    )

model = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)
                                       ),
                  distribution.model = "sstd", 
                  mean.model=list(armaOrder=c(2,2), archm = TRUE, 
                                  include.mean = FALSE
                                  )) 

afks_model_fit = ugarchfit(model, xts_afks, solver = 'hybrid')
afks_model_fit

resid = residuals(afks_model_fit)
write.csv(resid, 'ALRS_resid_2019_2021.csv')

plot(resid)
acf(resid)
qqnorm(resid)
qqline(resid, col = 2) 
plot(afks_model_fit, which = "all")

modelroll=ugarchroll(
  spec=model, data=xts_all_data, n.ahead = 1, forecast.length = 502,n.start = 1508,
  refit.every = 20, refit.window = c("recursive"),solver = "hybrid",
)

mu_forecasted = modelroll@forecast[["density"]][["Mu"]]
sigma_forecasted = modelroll@forecast[["density"]][["Sigma"]]

plot(modelroll)

df = data.frame(cbind(test_set$Date,mu_forecasted,sigma_forecasted))

write.csv(df, 'CHMF_forecast_2019_2021.csv')






