install.packages('forecast')
install.packages('expsmooth')
install.packages('ggplot2')
install.packages('seasonal')
install.packages('rugarch')
install.packages('xts')                   
install.packages('tsbox')
install.packages('astsa')

library(forecast)
library(expsmooth)
library(ggplot2)
library(seasonal)
library(rugarch)
library(xts)
library(tsbox)
library(astsa)

df=NewField
data = ts(df,start = c(2016,01,01),frequency = 1)
data
df
length_test_set = 24
h = 12
train= ts(df[1:2000,],start = c(2016,1,1))



df$Date<-as.Date(df$Date)  
train_data <- xts(df$DCP,df$Date)
train_data
class(ts1)
r<-xts_to_ts(xts.ts1, frequency = NULL, start = NULL)

nowTS <-ts(ts1)
nowTS


autoplot(train_data) +ggtitle("DCP PRODUCTION") + xlab("Year") + ylab("DCP")


ggsubseriesplot(train_data)  + ylab("New Orders Index") +
  ggtitle("DCP")


pacf(train_data)
ggAcf(train_data)

fit = stl(train_data,t.window=1, robust=TRUE)
autoplot(fit)


autoplot(train_data, series="Data") + 
  autolayer(trendcycle(fit), series="Trend") + 
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("DCP production") +
  scale_colour_manual(values=c("gray","blue","red"),breaks=c("Data","Seasonally Adjusted","Trend"))


model_name = "Naïve"
model = naive
my_forecast_plot = function(model,model_name){
  fit = model(window(train_data, end=2021),h=1)
  autoplot(window(train_data, start = 2016,end=2021)) +
    autolayer(fit, PI=TRUE,alpha = 0.25,color = "red") + 
    autolayer(fit, PI=FALSE, series=model_name,size = 1) + 
    xlab("Year") + ylab("New orders index") +
    ggtitle("Electrical equipment manufacturing (Euro area)") +
    guides(colour=guide_legend(title="Forecast"))
}

my_forecast_plot(model,model_name)

naive_mod <- naive(train_data, h = 50)
summary(naive_mod)
plot(naive_mod)

holt_model <- holt(train_data, h = 100)
summary(holt_model)
plot(holt_model)

holt_model <- holt(train_data, h = 365)
summary(holt_model)

arima_model <- auto.arima(train_data)
summary(arima_model)

fore_arima = forecast::forecast(arima_model, h=100)
fore_arima
plot(fore_arima)
plot(forecast(arima_model))

model_tbats <- tbats(train_data)
summary(model_tbats)
for_tbats <- forecast::forecast(model_tbats, h = 12)
df_tbats = as.data.frame(for_tbats)
dat_test$tbats = df_tbats$`Point Forecast`
mape(dat_test$unemploy, dat_test$tbats) 

plot(forecast(model_tbats))
for_tbats$mean
naive = snaive(train_data, h=12)

ets_model = ets(train_data, allow.multiplicative.trend = TRUE)
summary(ets_model)
ets_forecast = forecast(ets_model, h=120)
ets_forecast$mean
plot(ets_forecast)
dshw_model = dshw(train_data, period1=12, period2 = 360, h=1000)
dshw_model$mean
plot(dshw_model)
summary(dshw_model)
AIC(dshw_model)


plot(train_data)

tbats_model = tbats(train_data)
tbats_forecast = forecast(tbats_model, h=12)




sarima_forecast = sarima.for(training, n.ahead=length(validation),
                             p=0,d=1,q=1,P=1,D=1,Q=0,S=12)


one_step_ahead_sarima = matrix(ncol = 2, nrow = 60)
for (i in 1:60){
  
  training_observed = window(data, start = c(1949,1), end = c(1955,(12+i)), frequency = 12)
  
  forecasted.sarima = sarima.for(training_observed,n.ahead=1,p=0,d=1,q=1,P=1,D=1,Q=0,S=12)
  
  demandforecast = forecasted.sarima$pred
  observed = validation[[i]]
  
  one_step_ahead_sarima[i,1]= observed
  one_step_ahead_sarima[i,2]= demandforecast
}

fit <- auto.arima(train_data)
autoplot(train_data, series="Data") +
  autolayer(simulate(fit, nsim=36), series="Simulated")

















