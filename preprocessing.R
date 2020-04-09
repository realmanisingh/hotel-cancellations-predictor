library(ggplot2)
library(data.table)
library(glmnet)
library(dplyr)
library(lubridate)
library(wesanderson)
library(ggcorrplot)


df <- fread('hotel_bookings.csv')
View(df[64427])
unique(df$country)
colnames(df)[14] <- 'country_code'
colnames(df)


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


#plots
df_time <- df[,100*(sum(is_canceled)/.N), by= .(arrival_date_month, hotel)]
ggplot(df_time, aes(x = factor(arrival_date_month, levels= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",  "December")), y = V1, fill= hotel)) + geom_histogram(stat = 'identity', position = 'dodge') + labs(title = "Cancellations per Month", y = "Cancellation %" , x = 'Month') + scale_fill_discrete(name="Hotel") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Hotel", values = c("City Hotel" = "deepskyblue", "Resort Hotel" = "orange"))

#df2 <- copy(df)
rmcols <- rev(seq(1,ncol(df2))[!as.logical(sapply(df2, is.numeric))])
for (i in rmcols) df2[[i]] <- NULL
corr <- round(cor(data2), 1)
ggcorrplot(corr, type = "lower") + ggtitle("Correlation Matrix for Numerical Features") + theme(plot.title = element_text(hjust = 0.5))

#join weather and country data
country <- fread('country.csv')
colnames(country) <- c('country_code', 'country_name', 'distance', 'eu')
weather <- fread('weather.csv')
weather$date <- as.Date(weather$date, '%m/%d/%y')
# resort hotel is argrave, city hotel is lisbon
merge(x = df, y = weather, by.x = c('arrival_date', 'city'), by.y = c('date', 'city'), all.x = TRUE)
merge(x = df, y = country, by=c('country_code'), all.x= TRUE) 
which(is.na(newdf$country_name))

View(newdf[64427])



