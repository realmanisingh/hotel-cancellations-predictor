library(ggplot2)
library(data.table)
library(glmnet)
library(dplyr)
library(lubridate)
library(wesanderson)
library(ggcorrplot)
library(rpart) 
library(gbm)
library(caret)




df <- fread('hotel_data.csv')
unique(df$country)
df <- df[!country == '']
View(df)
table(df$country)
colnames(df)
View(df)


# add a column for arrival_date for time series analysis
df[, arrival_date_month := match(arrival_date_month,month.name)]
df[, arrival_date := with(df, ymd(sprintf('%04d%02d%02d', arrival_date_year, arrival_date_month, arrival_date_day_of_month)))]
df[, reservation_status_date := as.Date(reservation_status_date)]
df[, arrival_date_month:= month.name[arrival_date_month]]

unique(df$hotel)
df$city <- sapply(df$hotel, function(x) if(x == 'City Hotel'){1} else {0})
df$resort <- sapply(df$hotel, function(x) if(x == 'Resort Hotel'){1} else {0})
# meal: FB, BB, HB
unique(df$meal)
df$no_meal <- sapply(df$meal, function(x) if(x == 'SC' | x == 'Undefined') {1} else{0})
df$full_board <- sapply(df$meal, function(x) if(x == 'FB') {1} else {0})
df$half_board <- sapply(df$meal, function(x) if(x == 'BB') {1} else {0})
df$bed_breakfast <- sapply(df$meal, function(x) if(x == 'HB') {1} else {0})
#distribution_channel: TA/TO, Direct, Corporate, Undefined, GDS
unique(df$distribution_channel)
df$distribute_TA_TO <- sapply(df$distribution_channel, function(x) if(x == 'TA/TO') {1} else {0})
df$distribute_direct <- sapply(df$distribution_channel, function(x) if(x == 'Direct') {1} else {0})
df$distribute_corporate <- sapply(df$distribution_channel, function(x) if(x == 'Corporate') {1} else {0})
df$distribute_undefined <- sapply(df$distribution_channel, function(x) if(x == 'Undefined') {1} else {0})
df$distribute_gds <- sapply(df$distribution_channel, function(x) if(x == 'GDS') {1} else {0})
# market_segment: Online TA, Offline TA/TO, Direct, Corporate, Groups
df$segment_online <- sapply(df$market_segment, function(x) if(x == 'Online TA') {1} else {0})
df$segment_offline <- sapply(df$market_segment, function(x) if(x == 'Offline TA/TO') {1} else {0})
df$segment_corporate <- sapply(df$market_segment, function(x) if(x == 'Corporate') {1} else {0})
df$segment_direct <- sapply(df$market_segment, function(x) if(x == 'Direct') {1} else {0})
df$segment_groups <- sapply(df$market_segment, function(x) if(x == 'Groups') {1} else {0})
df$segment_complementary <- sapply(df$market_segment, function(x) if(x == 'Complementary') {1} else {0})
df$segment_aviation <- sapply(df$market_segment, function(x) if(x == 'Aviation') {1} else {0})
df$segment_undefined <- sapply(df$market_segment, function(x) if(x == 'Undefined') {1} else {0})
unique(df$market_segment)

# deposit type: Non Refund, Refundable, No Deposit
unique(df$deposit_type)
df$deposit_non_refund <- sapply(df$deposit_type, function(x) if(x == 'Non Refund') {1} else {0})
df$deposit_refundable <- sapply(df$deposit_type, function(x) if(x == 'Refundable') {1} else {0})
df$deposit_no_deposit <- sapply(df$deposit_type, function(x) if(x == 'No Deposit') {1} else {0})

# customer_type: Transient, Transient Party, Contract, Group
unique(df$customer_type)
df$customer_transient <- sapply(df$customer_type, function(x) if(x == 'Transient') {1} else {0})
df$customer_transient_party <- sapply(df$customer_type, function(x) if(x == 'Transient-Party') {1} else {0})
df$customer_contract <- sapply(df$customer_type, function(x) if(x == 'Contract') {1} else {0})
df$customer_group <- sapply(df$customer_type, function(x) if(x == 'Group') {1} else {0})

canceled_row <- nrow(df[is_canceled == 1])
not_canceled <- df[is_canceled == 0]
canceled <- df[is_canceled == 1]
set.seed(5)
cleared_not_canceled <- not_canceled[sample(nrow(not_canceled), canceled_row)]
df <- rbind(cleared_not_canceled , canceled)
table(df$is_canceled)

#plots
df_time <- df[,100*(sum(is_canceled)/.N), by= .(arrival_date_month, hotel)]
ggplot(df_time, aes(x = factor(arrival_date_month, levels= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",  "December")), y = V1, fill= hotel)) + geom_histogram(stat = 'identity', position = 'dodge') + labs(title = "Cancellations per Month", y = "Cancellation %" , x = 'Month') + scale_fill_discrete(name="Hotel") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Hotel", values = c("City Hotel" = "deepskyblue", "Resort Hotel" = "orange"))

df2 <- copy(df)
rmcols <- rev(seq(1,ncol(df2))[!as.logical(sapply(df2, is.numeric))])
for (i in rmcols) df2[[i]] <- NULL
corr <- round(cor(df2), 1)
corr
ggcorrplot(corr, type = "lower") + ggtitle("Correlation Matrix for Numerical Features") + theme(plot.title = element_text(hjust = 0.5))

#join weather data

weather <- fread('weather.csv')
View(weather)
weather$date <- as.Date(weather$date, '%m/%d/%y')
# resort hotel is argrave, city hotel is lisbon
df <- merge(x = df, y = weather, by.x = c('arrival_date', 'city'), by.y = c('date', 'city'), all.x = TRUE)
View(df[country == ''])
View(df)


# correlation
df_time <- df[,100*(sum(is_canceled)/.N), by= .(arrival_date_month, hotel)]
ggplot(df_time, aes(x = factor(arrival_date_month, levels= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",  "December")), y = V1, fill= hotel)) + geom_histogram(stat = 'identity', position = 'dodge') + labs(title = "Cancellations per Month", y = "Cancellation %" , x = 'Month') + scale_fill_discrete(name="Hotel") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Hotel", values = c("City Hotel" = "deepskyblue", "Resort Hotel" = "orange"))


df$precipitation <- NULL

df2 <- copy(df)
rmcols <- rev(seq(1,ncol(df2))[!as.logical(sapply(df2, is.numeric))])
for (i in rmcols) df2[[i]] <- NULL
corr <- round(cor(df2), 1)
View(corr)
ggcorrplot(corr, type = "lower") + ggtitle("Correlation Matrix for Numerical Features") + theme(plot.title = element_text(hjust = 0.5))



names(df)[names(df) == "Km from Portugal"] <- "km"
names(df)[names(df) == "European Union"] <- "eu"

# test & train
# 85 15
test.size <- nrow(df) * 0.15
df[, test:= 0]
df[sample(nrow(df), test.size), test:= 1]

df.test <- df[test == 1]
df.train <- df[test == 0]

y.test <- df.test$is_canceled
y.train <- df.train$is_canceled


# Formula that includes all the predictors with corr coeff >= 0.1 as well as all the weather and country attributes
f1 <- as.formula(is_canceled ~  lead_time + adults + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled + booking_changes + days_in_waiting_list + required_car_parking_spaces
       + total_of_special_requests + km + resort + city + distribute_TA_TO + distribute_direct + distribute_corporate + segment_direct + segment_corporate + segment_groups 
       + deposit_no_deposit + deposit_non_refund + customer_transient_party)

# Baseline Model 
baseline_f1 <- as.formula(is_canceled ~  lead_time + adults + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled 
                          + booking_changes + total_of_special_requests + resort + city + segment_direct 
                          + segment_corporate + segment_groups)
baseline_model <- rpart(baseline_f1, df.train, method="class", control = rpart.control(cp = 0.001))



fit.tree <- rpart(f1, df.train, method="class", control = rpart.control(cp = 0.001))

yhat.tree <- predict(fit.tree, df.train)

mse_train <- mean((y.train - yhat.tree)^2)

yhat.tree.test <- predict(fit.tree, df.test)
mse.test <- mean((y.test - yhat.tree.test)^ 2)

summary(fit.tree)

fit.rndfor <- randomForest(f1, df.train, ntree=100, do.trace=0, importance=TRUE)

plot(fit.rndfor)

importance(fit.rndfor)

yhat.rndfor <- predict(fit.rndfor, df.train)
mse.rndfor <- mean((yhat.rndfor - y.train) ^ 2)

yhat.rndfor.test <- predict(fit.rndfor, df.test)
mse.rndfor.test <- mean((yhat.rndfor.test - y.test) ^ 2)


fit.btree <- gbm(f1, data = df.train, n.trees = 5000, distribution='bernoulli', interaction.depth = 2, shrinkage = 0.001, cv.folds=5)
gbm.perf(fit.btree)

summary(fit.btree)



best.iter = gbm.perf(fit.btree, method="cv")


fitControl = trainControl(method="cv", number=5, returnResamp = "all")

model2 = train(f1, data=df.train, method="gbm",distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))


mPred = predict(model2, df.train, na.action = na.pass)




postResample(mPred, mydata$diabetes)



