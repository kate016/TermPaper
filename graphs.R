coef_afks = coef(afks_model_fit)
p_value = 
  
  
  coef_matrix_afks = matrix(coef_afks)
mu_afks = coef_matrix_afks[1,1]
ar1_afks = coef_matrix_afks[2,1]
ar2_afks = coef_matrix_afks[3,1]
ma1_afks = coef_matrix_afks[4,1]
ma2_afks = coef_matrix_afks[5,1]
archm_afks = coef_matrix_afks[6,1]
omega_afks = coef_matrix_afks[7,1]
alpha1_afks = coef_matrix_afks[8,1]
beta1_afks = coef_matrix_afks[9,1]
gamma1_afks = coef_matrix_afks[10,1]
skew_afks = coef_matrix_afks[11,1]
shape_afks = coef_matrix_afks[12,1]
resid_afks = residuals(afks_model_fit)

qqnorm(resid_afks)
qqline(residuals, col = 2) 
acf(resid_afks)



resid_afks_matrix = matrix(tail(resid_afks,n=2))
resid_afks_t = resid_afks_matrix[2,1]
resid_afks_t_1_prior = resid_afks_matrix[1,1]

sd_afks = sd(xts_afks)
afks_return_t = matrix(tail(xts_afks,n=2))[2,1]
afks_return_t_1_prior = matrix(tail(xts_afks,n=2))[1,1]

log_sigma_afks_t_1 = omega_afks + log(sd_afks)^2
+alpha1_afks*abs(resid_afks_t/sd_afks)+gamma1_afks*
  (resid_afks_t/sd_afks)
sigma_afks = exp(log_sigma_afks)



r_afks_t_1 = archm_afks*sigma_afks+ar1_afks*
  afks_return_t + ar2_afks*afks_return_t_1_prior +
  ma1_afks*resid_afks_t +ma2_afks*resid_afks_t_1_prior


plot(alrs$Log_return~alrs$Date, type="l",xlab="Дата", ylab="ALRS")
plot(chmf$Log_return~alrs$Date, type="l",xlab="Дата", ylab="CHMF")
plot(gazp$Log_return~alrs$Date, type="l",xlab="Дата", ylab="GAZP")
plot(gmkn$Log_return~alrs$Date, type="l",xlab="Дата", ylab="GMKN")
plot(lkoh$Log_return~alrs$Date, type="l",xlab="Дата", ylab="LKOH")
plot(nlmk$Log_return~alrs$Date, type="l",xlab="Дата", ylab="NLMK")
plot(nvtk$Log_return~alrs$Date, type="l",xlab="Дата", ylab="NVTK")
plot(rosn$Log_return~alrs$Date, type="l",xlab="Дата", ylab="ROSN")
plot(sber$Log_return~alrs$Date, type="l",xlab="Дата", ylab="SBER")
plot(sngs$Log_return~alrs$Date, type="l",xlab="Дата", ylab="SNGS")

par(mfrow=c(3,4))
plot(alrs_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="ALRS")
plot(chmf_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="CHMF")
plot(gazp_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="GAZP")
plot(gmkn_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="GMKN")
plot(lkoh_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="LKOH")
plot(nlmk_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="NLMK")
plot(nvtk_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="NVTK")
plot(rosn_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="ROSN")
plot(sber_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="SBER")
plot(sngs_2013_2018$Log_return~as.Date(alrs_2013_2018$Date), type="l",xlab="Дата", ylab="SNGS")


par(mfrow=c(3,4))
ac_alrs = acf(alrs$Log_return,plot=FALSE)
plot(ac_alrs, main='ALRS лог-доходн.')
ac_chmf = acf(chmf$Log_return,plot=FALSE)
plot(ac_chmf, main='CHMF лог-доходн.')
ac_gazp = acf(gazp$Log_return,plot=FALSE)
plot(ac_gazp, main='GAZP лог-доходн.')
ac_gmkn = acf(gmkn$Log_return,plot=FALSE)
plot(ac_gmkn, main='GMKN лог-доходн.')
ac_lkoh = acf(lkoh$Log_return,plot=FALSE)
plot(ac_lkoh, main='LKOH лог-доходн.')
ac_nlmk= acf(nlmk$Log_return,plot=FALSE)
plot(ac_nlmk, main='NLMK лог-доходн.')
ac_nvtk= acf(nvtk$Log_return,plot=FALSE)
plot(ac_nvtk, main='NVTK лог-доходн.')
ac_rosn= acf(rosn$Log_return,plot=FALSE)
plot(ac_rosn, main='ROSN лог-доходн.')
ac_sber= acf(sber$Log_return,plot=FALSE)
plot(ac_sber, main='SBER лог-доходн.')
ac_sngs= acf(sngs$Log_return,plot=FALSE)
plot(ac_sngs, main='SNGS лог-доходн.')


par(mfrow=c(3,4))
ac_alrs = acf(alrs$Log_return^2,plot=FALSE)
plot(ac_alrs, main='ALRS квадрат лог-доходн.')
ac_chmf = acf(chmf$Log_return^2,plot=FALSE)
plot(ac_chmf, main='CHMF квадрат лог-доходн.')
ac_gazp = acf(gazp$Log_return^2,plot=FALSE)
plot(ac_gazp, main='GAZP квадрат лог-доходн.')
ac_gmkn = acf(gmkn$Log_return^2,plot=FALSE)
plot(ac_gmkn, main='GMKN квадрат лог-доходн.')
ac_lkoh = acf(lkoh$Log_return^2,plot=FALSE)
plot(ac_lkoh, main='LKOH квадрат лог-доходн.')
ac_nlmk= acf(nlmk$Log_return^2,plot=FALSE)
plot(ac_nlmk, main='NLMK квадрат лог-доходн.')
ac_nvtk= acf(nvtk$Log_return^2,plot=FALSE)
plot(ac_nvtk, main='NVTK квадрат лог-доходн.')
ac_rosn= acf(rosn$Log_return^2,plot=FALSE)
plot(ac_rosn, main='ROSN квадрат лог-доходн.')
ac_sber= acf(sber$Log_return^2,plot=FALSE)
plot(ac_sber, main='SBER квадрат лог-доходн.')
ac_sngs= acf(sngs$Log_return^2,plot=FALSE)
plot(ac_sngs, main='SNGS квадрат лог-доходн.')




#data = subset(data, Date> "2013-01-01" & Date < "2019-01-01")


ggtsdisplay(xts_data)
ggtsdisplay(abs(xts_data))
qqnorm(xts_data)
qqline(xts_data)




read_and_clean = function(x){
  data = read.csv(x)
  data$Date = as.Date(data$Date)
  data = data[order(data$Date), ] 
  data = subset(data, Date> "2013-01-01")
  dates <- data$Date
  xts_data <- xts(x=data$Log_return, order.by=dates)
  return (xts_data)
}

alrs = read_and_clean(ticker_list[1]) # arma 2,2 garch 1,3
chmf = read_and_clean(ticker_list[2]) # arma 2,2 garch 1,2
gazp = read_and_clean(ticker_list[3]) # arma 22 garch 03 arma 32 garch 03 arma 32 garch 00
gmkn = read_and_clean(ticker_list[4]) # arma 22 garch 12 arma 12 garch 12
lkoh = read_and_clean(ticker_list[5]) # arma 22 garch 11 arma 03 garch 11
nlmk = read_and_clean(ticker_list[6]) # arma 22 garch 33 arma 00 garch 33
nvtk = read_and_clean(ticker_list[7]) # arma 22 garch 11 arma 33 garch 11 arma 33 garch 02 / arma 32 garch 32 mean
rosn = read_and_clean(ticker_list[8]) # arma 32 garch 11 arma 32 garch 00
sber = read_and_clean(ticker_list[9]) # arma 22 garch 11 arma 11 garch 11 arma 11 garch 33 arma 30 garch 33 arma 30 garch 11 arma 11 
sngs = read_and_clean(ticker_list[10]) #arma 22 garch 21


#Preliminary

nlmk_lr = read.csv(ticker_list[10])
nlmk_lr = nlmk_lr$Log_return/100
basicStats(nlmk_lr)



final.aic = Inf
final.order = c(0,0)

for (m in 0:3) for (n in 0:3){
  spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(2,1)
                                        #variance.targeting =TRUE
  ),
  distribution.model = "sstd", 
  
  fixed.pars=list(),
  mean.model=list(armaOrder=c(m,n), archm = F, 
                  include.mean = F
  )) 
  # fit = ugarchfit(spec, chmf, solver = "hybrid")
  fit = tryCatch(ugarchfit(spec, sngs, solver = "hybrid"), error = function(e) print("Error")) 
  current.aic = tryCatch(infocriteria(fit)[1], error = function(e) print("Error") )
  if (current.aic < final.aic){
    final.aic = current.aic
    final.order = c(m,n)
  }
  
}

final.aic 
final.order




model_sngs = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(2,1)
                                            #variance.targeting =TRUE
), distribution.model = "sstd", 

fixed.pars=list(),
mean.model=list(armaOrder=c(2,2), archm = F, 
                include.mean = F
)) 

fit_alrs = ugarchfit(model_alrs, alrs, solver = "hybrid") # arma 2 2 garch 1 3
fit_chmf = ugarchfit(model_chmf, chmf, solver = "hybrid") # arma 2 2 garch 1 2
fit_gazp = ugarchfit(model_gazp, gazp, solver = "hybrid") # arma 3 2 garch 0 3
fit_gmkn = ugarchfit(model_gmkn, chmf, solver = "hybrid") # arma 1 2 garch 1 2
fit_lkoh = ugarchfit(model_lkoh, chmf, solver = "hybrid") # arma 0 3 garch 1 1
fit_nlmk = ugarchfit(model_nlmk, chmf, solver = "hybrid") # arma 0 0 garch 3 3
fit_nvtk = ugarchfit(model_nvtk, chmf, solver = "hybrid") # arma 3 2 garch 3 2 archm=T
fit_rosn = ugarchfit(model_rosn, chmf, solver = "hybrid") # arma 3 2 garch 0 0
fit_sber = ugarchfit(model_sber, chmf, solver = "hybrid") # arma 1 1 garch 1 1
fit_sngs = ugarchfit(model_sngs, chmf, solver = "hybrid") # arma 2 2 garch 2 1

model_fit_fun = function(x, model){
  data = read.csv(x)
  data$Date = as.Date(data$Date)
  data = data[order(data$Date), ] 
  data = subset(data, Date> "2013-01-01")
  dates <- data$Date
  xts_data <- xts(x=data$Log_return, order.by=dates)
  model_fit = ugarchfit(model,xts_data, solver = 'hybrid')
  return(model_fit)
}

#model_forecast_fun = function(data, model_fit){
#  data = read.csv(data)
#  data$Date = as.Date(data$Date)
#  data = data[order(data$Date), ] 
#  data = subset(data, Date>"2019-01-01")
#  dates <- data$Date
#  xts_data <- xts(x=data$Log_return, order.by=dates)
#  forecast = ugarchforecast(model_fit, n.ahead = 30)
#  return (forecast)
#}

model_fit = model_fit_fun("ALRS_den_2013_2021_clean1.csv", model)

mean = model_fit@fit$fitted.values
vol = model_fit@fit$sigma
resid = model_fit@fit$residuals
checkresiduals(resid)
plot(residuals)
adf.test(resid) #for stationarity
qqnorm(resid)
qqline(resid)
shapiro.test(resid)
hist(resid)
acf(resid^2)
pacf(resid^2)
ts.plot(vol)
forecast_30_ahead = ugarchforecast(model_fit, n.ahead = 30)
boot_30_ahead = ugarchboot(model_fit, n.ahead = 30, method = c('Partial', 'Full')[1])
plot(boot_30_ahead, which=2)



coef_afks = coef(afks_model_fit)
coef_matrix_afks = matrix(coef_afks)
mu_afks = coef_matrix_afks[1,1]
ar1_afks = coef_matrix_afks[2,1]
ar2_afks = coef_matrix_afks[3,1]
ma1_afks = coef_matrix_afks[4,1]
ma2_afks = coef_matrix_afks[5,1]
archm_afks = coef_matrix_afks[6,1]
omega_afks = coef_matrix_afks[7,1]
alpha1_afks = coef_matrix_afks[8,1]
beta1_afks = coef_matrix_afks[9,1]
gamma1_afks = coef_matrix_afks[10,1]
skew_afks = coef_matrix_afks[11,1]
shape_afks = coef_matrix_afks[12,1]
resid_afks = residuals(afks_model_fit)
resid_afks_matrix = matrix(tail(resid_afks,n=2))
resid_afks_t = resid_afks_matrix[2,1]
resid_afks_t_1_prior = resid_afks_matrix[1,1]

sd_afks = sd(xts_afks)
afks_return_t = matrix(tail(xts_afks,n=2))[2,1]
afks_return_t_1_prior = matrix(tail(xts_afks,n=2))[1,1]

log_sigma_afks_t_1 = omega_afks + log(sd_afks)^2
+alpha1_afks*abs(resid_afks_t/sd_afks)+gamma1_afks*
  (resid_afks_t/sd_afks)
sigma_afks = exp(log_sigma_afks)



r_afks_t_1 = archm_afks*sigma_afks+ar1_afks*
  afks_return_t + ar2_afks*afks_return_t_1_prior +
  ma1_afks*resid_afks_t +ma2_afks*resid_afks_t_1_prior
