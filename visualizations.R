
# Cancellations per month based on whether or not the hotel is a city hotel or resort hotel
df_time <- df_country[,100*(sum(cancel_int)/.N), by= .(arrival_date_month, city)]
labels <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ggplot(df_time, aes(x = factor(arrival_date_month, levels= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",  "December")), y=V1, fill=arrival_date_month)) + geom_histogram(stat = 'identity', position = 'dodge') + labs(title = "Cancellations per Month", y = "Cancellation %" , x = 'Month') + theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + scale_x_discrete(labels=labels) #+ scale_fill_manual("Hotel", values = c("City Hotel" = "deepskyblue", "Resort Hotel" = "orange"))



# Total cancellations for top 6 countries
df_country <- df
df_country[, cancel_int := 0] 
df_country[is_canceled==1, cancel_int:=1]
df_temp <- df_country[, 100*(sum(cancel_int)/.N), by=.(country)]

df_temp_sub <- df_temp[country=="PRT" | country=="GBR" | country=="FRA" | country=="ESP" | country=="ITA" | country=="DEU"]

ggplot(df_temp_sub, aes(x=country, y=V1, fill=country)) + geom_bar(stat = "identity") + 
  labs(title = "Cancellation Rates for Top 6 Countries", y="Cancellation Percentage", x="Countries") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


# Correlation Matrix for analyzing relevant features
df2 <- copy(df)
rmcols <- rev(seq(1,ncol(df2))[!as.logical(sapply(df2, is.numeric))])
for (i in rmcols) df2[[i]] <- NULL
corr <- round(cor(df2), 1)
corr
ggcorrplot(corr, type = "lower") + ggtitle("Correlation Matrix for Numerical Features") + theme(plot.title = element_text(hjust = 0.5))

