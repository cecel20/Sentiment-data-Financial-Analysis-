## stock project 
Sys.setlocale("LC_TIME", "English")

library(data.table)
library(ggplot2)

# data import
setwd('/Users/chenyue/Desktop/R')
dta1 <- fread("total_revise2.csv")
dta1 <- as.data.frame(dta1)
str(dta1)
dta1$symbol <- as.factor(dta1$symbol)
dta1$sector <- as.factor(dta1$sector)
dta1$date <- strptime(dta1$date,"%m/%d/%Y")
dta1$open <- as.numeric(dta1$open)
dta1 <- dta1[!is.na(dta1$open),] 
dta1$high <- as.numeric(dta1$high)
dta1 <- dta1[!is.na(dta1$high),] 
dta1$low <- as.numeric(dta1$low)
dta1 <- dta1[!is.na(dta1$low),] 
dta1$close <- as.numeric(dta1$close)
dta1 <- dta1[!is.na(dta1$close),] 
dta1$volume <- as.numeric(dta1$volume)
dta1 <- dta1[!is.na(dta1$volume),] 

dta1 <- dta1[!is.na(dta1$BULL_SCORED_MESSAGES),] 
dta1 <- dta1[!is.na(dta1$BEAR_SCORED_MESSAGES),] 
dta1 <- dta1[!is.na(dta1$BULL_BEAR_MSG_RATIO),] 
dta1 <- dta1[!is.na(dta1$TOTAL_SCANNED_MESSAGES),] 
dta1 <- dta1[!is.na(dta1$PERCENT_SCORED_MESSAGES),] 
dta1 <- dta1[!is.na(dta1$bullish_advance),]
dta1 <- dta1[!is.na(dta1$BEARISH_advance),]
dta1 <- dta1[!is.na(dta1$BULL_MINUS_BEAR_advance),]
dta1 <- dta1[!is.na(dta1$TOTAL_INTENSITY_advance),]

dta1$year <- as.numeric(format(dta1$date,'%Y'))
dta1$month <- as.numeric(format(dta1$date,'%m'))
dta1$weekday <- weekdays(dta1$date)
dta1$weekday[dta1$weekday == "Monday"] <- 1
dta1$weekday[dta1$weekday == "Tuesday"] <- 2
dta1$weekday[dta1$weekday == "Wednesday"] <- 3
dta1$weekday[dta1$weekday == "Thursday"] <- 4
dta1$weekday[dta1$weekday == "Friday"] <- 5
dta1$weekday <- as.numeric(dta1$weekday)
dta1$date <- as.character(dta1$date)
#dta1 <- dta1[,c(1:17,19:21,23:25,18,22)]
str(dta1)

# view the time trend about close price

avg_close <- aggregate(dta1$close, by=list(dta1$date), FUN=mean)
colnames(avg_close) <- c("date", "close")
####提问1#####
avg_close$date <- as.Date(avg_close$date)

med_close <- aggregate(dta1$close, by=list(dta1$date), FUN=median)
colnames(med_close) <- c("date", "close")
med_close$date <- as.Date(med_close$date)

avg_trend <- ggplot() + geom_line(data=avg_close, aes(x=date, y=close)) + 
  theme_bw() + theme_minimal() + ggtitle("Average Trend of Close Price")
med_trend <- ggplot() + geom_line(data=med_close, aes(x=date, y=close)) + 
  theme_bw() + theme_minimal() + ggtitle("Median Trend of Close Price")
avg_trend
med_trend

symbol_count <- aggregate(dta1$symbol, by=list(dta1$date), FUN=length)
colnames(symbol_count) <- c("date", "symbol")
head(symbol_count,70)

# remove the date before 0018-05-03
dta1 <- dta1[order(dta1$date),]
dta1 <- dta1[dta1$date >= "0018-05-03",]

symbol_count <- aggregate(dta1$symbol, by=list(dta1$date), FUN=length)
colnames(symbol_count) <- c("date", "symbol")
head(symbol_count,10)

# time series model (just close)

library(tseries)
library(forecast)

avg_close <- aggregate(dta1$close, by=list(dta1$date), FUN=mean)
avg_open <- aggregate(dta1$open, by=list(dta1$date), FUN=mean)
avg_high <- aggregate(dta1$high, by=list(dta1$date), FUN=mean)

avg_low <- aggregate(dta1$low, by=list(dta1$date), FUN=mean)
####添加列######
avg_date <- cbind(avg_close,avg_open[,2],avg_high[,2],avg_low[,2])
colnames(avg_date) <- c("date", "close", "open", "high", "low")
avg_date$date <- as.Date(avg_date$date)

ts_data <- ts(avg_date, frequency = 12)
decom_data <- decompose(ts_data[,c("close")], type = "mult")
plot(decom_data)

train_ts <- ts(avg_date[1:200,], frequency = 12)
arima_mod <- auto.arima(train_ts[,c("close")], seasonal = TRUE, test = "adf", ic = "aic")
test_ts <- avg_date[201:247,]

plot(forecast(arima_mod, h=12))

# time series model (include other variables)

ts_regressor <- tslm(close ~ open + high+low ,data = train_ts)

plot(forecast(ts_regressor, newdata = test_ts))
plot(avg_date[1:247,2], type="l")

symbol <- unique(dta1$symbol)
m <- c()

for (i in 1:length(symbol)) {
  test_symbol <- dta1[dta1$symbol==symbol[i],]
  a <- sum((predict(ts_regressor,newdata = test_symbol) - test_symbol$close)^2)/length(test_symbol$close)
  m <- rbind(m,a)
}

symbol <- as.character(symbol)
res_tslm <- as.data.frame(cbind(symbol,m))
colnames(res_tslm) <- c("symbol", "MSE")
res_tslm$MSE <- as.numeric(as.character(res_tslm$MSE))
head(res_tslm,10)

mean(res_tslm$MSE)
median(res_tslm$MSE)
res_tslm[res_tslm$MSE > 100,]
res_tslm_mse<- sum(res_tslm$MSE)
res_tslm_mse
# train and test data

set.seed(1234)
index <- sample(1:2, size = nrow(dta1), replace = TRUE, prob = c(0.75,0.25))
train_data <- dta1[index == 1,]
test_data <- dta1[index == 2,]

# linear regression model (without sentiment data)

mse <- function(m) {
  pred <- predict(object = m, newdata = test_data)
  mse <- sum((pred - test_data$close)^2)/length(pred)
  return(mse)
}

lm_model1 <- lm(close ~ open + high + low, data = train_data)
lm_model2 <- lm(close ~ open + high + low + year + month, data = train_data)
lm_model3 <- lm(close ~ open + high + low + year + month + weekday, data = train_data)
lm_model4 <- lm(close ~ open + high + low + weekday, data = train_data)

pred=predict(lm_model1, newdata = test_data)
AIC(lm_model1)

lm_table <- cbind(c(mse(lm_model1),mse(lm_model2),mse(lm_model3),mse(lm_model4)),
                  c(AIC(lm_model1),AIC(lm_model2),AIC(lm_model3),AIC(lm_model4)))
colnames(lm_table) <- c("MSE","AIC")
rownames(lm_table) <- c("model1","model2","model3","model4")
lm_table

# Linear Regression model

lm_model5 <- lm_model5 <- lm(close ~ open + high + low + volume +
                               BULL_SCORED_MESSAGES + BEAR_SCORED_MESSAGES + 
                               BULL_BEAR_MSG_RATIO + TOTAL_SCANNED_MESSAGES + 
                               PERCENT_SCORED_MESSAGES + bullish_advance + BEARISH_advance + BULL_MINUS_BEAR_advance+ TOTAL_INTENSITY_advance +year + month + weekday
                             , data = train_data)
lm_model5 <- step(lm_model5)
lm_model5$anova
summary(lm_model5)

lm_table <- as.data.frame(rbind(lm_table,c(mse(lm_model5),AIC(lm_model5))))
colnames(lm_table) <- c("MSE","AIC")
rownames(lm_table) <- c("model1","model2","model3","model4","model5")
lm_table

lm_res <- as.data.frame(cbind(mse(lm_model5),1-(sum((predict(lm_model5, newdata = test_data)-test_data$close)^2)/sum((test_data$close-mean(test_data$close))^2)))) 
colnames(lm_res) <- c("MSE", "R-square")
lm_res

# Lasso Regression model (regularization)

library(glmnet)

x <- as.matrix(train_data[,7:22])
y <- as.matrix(train_data[,5])

lasso_fit <- cv.glmnet(x, y, family="gaussian", nlambda=100, alpha=1)
print(lasso_fit)

lasso_model <- as.data.frame(cbind(test_data[,5],predict(lasso_fit, newx=as.matrix(test_data[,7:22]), s=c(lasso_fit$lambda[54],0.2980225))[,1]))  
colnames(lasso_model) <- c("real", "pred")

predict(lasso_fit, type = "coefficients", s=c(lasso_fit$lambda[54],0.2980225))[,1]

lasso_res <- as.data.frame(cbind(sum((lasso_model$pred - lasso_model$real)^2)/length(lasso_model$pred),
                                 1-(sum((lasso_model$pred-lasso_model$real)^2)/sum((lasso_model$real-mean(lasso_model$real))^2)))) 
colnames(lasso_res) <- c("MSE", "R-square")
lasso_res

# Ridge Regression model (regularization)

ridge_fit <- cv.glmnet(x, y, family="gaussian", nlambda=100, alpha=0)
print(ridge_fit)

ridge_model <- as.data.frame(cbind(test_data[,5],predict(ridge_fit, newx=as.matrix(test_data[,7:22]), s=c(ridge_fit$lambda[99],4.529676))[,1]))  
colnames(ridge_model) <- c("real", "pred")

predict(ridge_fit, type = "coefficients", s=c(ridge_fit$lambda[99],4.529676))[,1]

ridge_res <- as.data.frame(cbind(sum((ridge_model$pred - ridge_model$real)^2)/length(ridge_model$pred),
                                 1-(sum((ridge_model$pred-ridge_model$real)^2)/sum((ridge_model$real-mean(ridge_model$real))^2)))) 
colnames(ridge_res) <- c("MSE", "R-square")
ridge_res

# Regression Tree model

library(tree)
tree_model <- tree(close ~ ., data = train_data[,c(5,7:22)], mindev = 0.00002)
rt_res <- as.data.frame(cbind(mse(tree_model),1-(sum((predict(tree_model, newdata = test_data)-test_data$close)^2)/sum((test_data$close-mean(test_data$close))^2)))) 
colnames(rt_res) <- c("MSE", "R-square")
rt_res


# summary

total_table <-as.data.frame(rbind(lm_res,lasso_res,ridge_res,rt_res))
rownames(total_table) <- c("linear regression", "lasso regression", "ridge regression", "regression tree")
total_table

