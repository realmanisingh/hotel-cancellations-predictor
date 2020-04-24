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
library(randomForest)
library(e1071)



# reading in the data and exploring
df <- fread('hotel_data.csv')
View(df)
df$V1 <- NULL
unique(df$agent)
summary(df == 'NULL')
unique(df$country)
df <- df[!country == '']
table(df$country)
df[hotel == 'City Hotel']

# Changing column names to make it easier to type
names(df)[names(df) == "Km from Portugal"] <- "km"
names(df)[names(df) == "European Union"] <- "eu"
names(df)[names(df) == "country2"] <- "country_name"
names(df)[names(df) == 'resort'] <- 'hotel_resort'
colnames(df)


# add a column for arrival_date for time series analysis
df[, arrival_date_month := match(arrival_date_month,month.name)]
df[, arrival_date := with(df, ymd(sprintf('%04d%02d%02d', arrival_date_year, arrival_date_month, arrival_date_day_of_month)))]
df[, reservation_status_date := as.Date(reservation_status_date)]
df[, arrival_date_month:= month.name[arrival_date_month]]
summary(df[, c('arrival_date_month', 'arrival_date', 'reservation_status_date')])

df$city <- sapply(df$hotel, function(x) if(x == 'City Hotel'){1} else {0})
df$resort <- sapply(df$hotel, function(x) if(x == 'Resort Hotel'){1} else {0})
df$hotel <- NULL
# SC and Undefined are both no meal (NM)
df[meal == 'SC'| meal == 'Undefined', meal:= 'NM']
unique(df$meal)

# assuming those with NA children has none
df[is.na(children), children:= 0]

# set NAs for agent and company to be 0
df[is.na(agent), agent:= 0]
df[is.na(company), company:= 0]

# Storing the reservations that have been canceled and that have not been canceled 
canceled_row <- nrow(df[is_canceled == 1])
not_canceled <- df[is_canceled == 0]
canceled <- df[is_canceled == 1]
set.seed(5)
cleared_not_canceled <- not_canceled[sample(nrow(not_canceled), canceled_row)]
df <- rbind(cleared_not_canceled , canceled)
table(df$is_canceled)



# Joining the weather data with the hotel data
weather <- fread('weather.csv')

weather$date <- as.Date(weather$date, '%m/%d/%y')


# Resort hotel is Algarve, city hotel is Lisbon
df <- merge(x = df, y = weather, by.x = c('arrival_date', 'city'), by.y = c('date', 'city'), all.x = TRUE)
# Removing precipitation column since there is already a rain column
df$precipitation <- NULL
summary(df)

#factorize the categorical variables
df$is_canceled <- factor(df$is_canceled, levels=unique(c(0, 1)))
df$meal <- factor(df$meal, levels=unique(c('BB', 'FB', 'HB', 'NM')))
df$customer_type <- factor(df$customer_type, levels=unique(c('Contract', 'Group', 'Transient', 'Transient-Party')))
df$market_segment <- factor(df$market_segment, levels=unique(c('Online TA', 'Offline TA/TO', 'Groups', 'Direct', 'Corporate', 'Complementary', 'Undefined', 'Aviation')))
df$distribution_channel <- factor(df$distribution_channel, levels=unique(c('Corporate', 'Direct', 'GDS', 'TA/TO', 'Undefined' )))
df$deposit_type <- factor(df$deposit_type, levels=unique(c('No Deposit', 'Non Refund', 'Refundable')))
df$reserved_room_type <- factor(df$reserved_room_type, levels=unique(c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'L', 'P')))
df$arrival_date_month <- factor(df$arrival_date_month, levels=unique(month.name))
df$assigned_room_type <- factor(df$assigned_room_type, levels=unique(c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'P')))
df$reservation_status <- factor(df$reservation_status, levels=unique(c('Check-Out', 'Canceled', 'No-Show')))
df$arrival_date_year <- factor(df$arrival_date_year, levels=unique(c(2015, 2016, 2017)))
df$is_repeated_guest <- factor(df$is_repeated_guest, levels=unique(c(0, 1)))
df$city <- factor(df$city, levels=unique(c(0, 1)))

# Correlation matrix to decide which features to include in formula
#df2 <- copy(df)
#rmcols <- rev(seq(1,ncol(df2))[!as.logical(sapply(df2, is.numeric))])
#for (i in rmcols) df2[[i]] <- NULL
#corr <- round(cor(df), 1)
#View(corr)
#ggcorrplot(corr, type = "lower") + ggtitle("Correlation Matrix for Numerical Features") + theme(plot.title = element_text(hjust = 0.5))


#exclude country & country name, and change date variable to number
cdf <- copy(df)
cdf$country <- NULL
cdf$country_name <- NULL
cdf$day_of_week <- weekdays(cdf$arrival_date)
cdf$day_of_week <- factor(cdf$day_of_week, levels=unique(c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')))
cdf$resort <- NULL
# IMPORTANT: reservation status shouldn't be included as it will give out the answer
cdf$reservation_status <- NULL
cdf$reservation_status_date <- NULL
cdf$arrival_date_week_number <- NULL
cdf$hotel_resort <- NULL
View(cdf)

#normalize numerical values
#leadtime, stays_in_weekend_nights, stays in week nights, adults, children, babies
# previous cancellations, previous bookings not canceled, booking changes, days in waiting list, adr
# car parking spaces, total special requests, km, eu, max_temp, average_temp, min_temp, rain, date_stamp
colnames(cdf)
preproc <- preProcess(cdf[,c(4, 8:12, 17, 18, 21, 25, 27:35)], method=c('range'))
cdf[,c(3, 7:11, 16:17, 20, 24, 26:35)] <- predict(preproc, cdf[,c(3, 7:11, 16:17, 20, 24, 26:35)])


# Creating a training set and test with an 85/15 split
set.seed(5)
test.size <- nrow(cdf) * 0.15
cdf[, test:= 0]
cdf[sample(nrow(cdf), test.size), test:= 1]

df.test <- cdf[test == 1]
df.train <- cdf[test == 0]
df.train.sample.size <- 5000
df.train.sample <- df.train[sample(nrow(df.train), df.train.sample.size)]
y.train.sample <- df.train.sample$is_canceled
y.test <- df.test$is_canceled
y.train <- df.train$is_canceled
set.seed(6)
df.test.sample.size <- 5000
df.test.sample <- df.test[sample(nrow(df.test), df.test.sample.size)]
y.test.sample <- df.test.sample$is_canceled

#variable selection
rf <- randomForest(is_canceled ~., data = df.train.sample)
plot(rf)
variable_selection <- importance(rf)
importance <- data.table(cbind(variable=rownames(variable_selection), variable_selection))
importance$MeanDecreaseGini <- as.numeric(importance$MeanDecreaseGini)
importance[order(-MeanDecreaseGini)]


