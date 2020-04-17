

# Baseline model which is a decision tree that does not include any weather or country data 
baseline_f1 <- as.formula(is_canceled ~  lead_time + adults + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled 
                          + booking_changes + total_of_special_requests + resort + city + segment_direct 
                          + segment_corporate + segment_groups)

baseline_model <- rpart(baseline_f1, df.train, method="class", control = rpart.control(cp = 0.001))





# Formula that includes all the predictors with corr coeff >= 0.1 as well as all the weather and country attributes
f1 <- as.formula(is_canceled ~  lead_time + adults + is_repeated_guest + previous_cancellations + previous_bookings_not_canceled + booking_changes + days_in_waiting_list + required_car_parking_spaces
                 + total_of_special_requests + km + resort + city + distribute_TA_TO + distribute_direct + distribute_corporate + segment_direct + segment_corporate + segment_groups 
                 + deposit_no_deposit + deposit_non_refund + customer_transient_party)

# Decision tree based on formula
fit.tree <- rpart(f1, df.train, method="class", control = rpart.control(cp = 0.001))

yhat.tree <- predict(fit.tree, df.train)
mse_train <- mean((y.train - yhat.tree)^2)
yhat.tree.test <- predict(fit.tree, df.test)
mse.test <- mean((y.test - yhat.tree.test)^ 2)
summary(fit.tree)

# Random forest based on formula
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



