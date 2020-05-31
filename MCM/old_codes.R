# Load 50-years dataset 
# (from 1970-2019, monthly)
sst = read_csv("scot_sst.csv")
sst = t(sst)
head(sst) # Now, each row is a time, each col is a block. We want our data to be in this format

# Sanity check
plot(sst[,1], type = "l") # seasonal pattern observed, the data is good to go

# Time-Series
sst_ts <- ts(sst, start = c(1970,1), end = c(2019,12), freq = 12)

ts.plot(sst_ts)

forecast(sst_ts, n.ahead = 600)

plot(forecast(HoltWinters(sst_ts[,1]), 120))

tsdisplay(sst_ts[,1])


# ---------- Forecast using monthly data ---------- #
auto = auto.arima(sst_ts[,1])

arima_model1 = Arima(sst_ts[,1], order = c(0, 0, 1), seasonal = c(1, 1, 0))
plot(forecast(arima_model1, 600))

arima_model2 = Arima(sst_ts[,1], order = c(0, 0, 1), seasonal = c(0, 1, 1))
plot(forecast(arima_model2, 600))

accuracy(forecast(arima_model1))

accuracy(forecast(arima_model2))

