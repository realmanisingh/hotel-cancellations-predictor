
# Cancellations per month based on whether or not the hotel is a city hotel or resort hotel
df_time <- df[,100*(sum(is_canceled)/.N), by= .(arrival_date_month, hotel)]
ggplot(df_time, aes(x = factor(arrival_date_month, levels= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",  "December")), y = V1, fill= hotel)) + geom_histogram(stat = 'identity', position = 'dodge') + labs(title = "Cancellations per Month", y = "Cancellation %" , x = 'Month') + scale_fill_discrete(name="Hotel") + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_manual("Hotel", values = c("City Hotel" = "deepskyblue", "Resort Hotel" = "orange"))


# Correlation Matrix for analyzing relevant features
df2 <- copy(df)
rmcols <- rev(seq(1,ncol(df2))[!as.logical(sapply(df2, is.numeric))])
for (i in rmcols) df2[[i]] <- NULL
corr <- round(cor(df2), 1)
corr
ggcorrplot(corr, type = "lower") + ggtitle("Correlation Matrix for Numerical Features") + theme(plot.title = element_text(hjust = 0.5))

