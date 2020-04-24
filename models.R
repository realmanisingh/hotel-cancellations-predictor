

# Baseline model which is a decision tree that does not include any weather or country data 
baseline_f1 <- as.formula(is_canceled ~  deposit_type + 
                            km + lead_time + market_segment + total_of_special_requests + adr + 
                            agent + arrival_date_month + average_temp + day_of_week + customer_type +
                            required_car_parking_spaces + arrival_date_day_of_month + max_temp + assigned_room_type + min_temp + 
                            previous_cancellations + stays_in_week_nights + booking_changes + deposit_type)

baseline_model <- rpart(baseline_f1, df.train, method="class", control = rpart.control(cp = 0.001))
plot(baseline_model)
baseline.test <- predict(baseline_model, df.test.sample, type ='class')
confusionMatrix(table(y.test.sample, baseline.test))

# boosted tree model
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
b.tree <- train(baseline_f1, data = df.train.sample, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
b.tree
btree.test <- predict(b.tree, df.test)
btree.results <- confusionMatrix(table(y.test, btree.test))
# results:
# interaction.depth  n.trees  Accuracy   Kappa    
# 1                   50      0.7561208  0.5101001
# 1                  100      0.7944802  0.5879913
# 1                  150      0.8014011  0.6020481
# 2                   50      0.7978587  0.5949158
# 2                  100      0.8148598  0.6293046
# 2                  150      0.8223988  0.6445237
# 3                   50      0.8075992  0.6146879
# 3                  100      0.8236397  0.6470497
# 3                  150      0.8271587  0.6541081
# The final values used for the model were n.trees = 150, 
# interaction.depth = 3, shrinkage = 0.1
# and n.minobsinnode = 10.

# lasso
lasso <- train(baseline_f1, data = df.train.sample, 
                 method = "glmnet", 
                 family = "binomial",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
lasso
lasso.test <- predict(lasso, df.test)
lasso.results <- confusionMatrix(table(y.test, lasso.test))
# results
# alpha  lambda        Accuracy   Kappa    
# 0.10   0.0004446642  0.7567385  0.5127203
# 0.10   0.0044466421  0.7549583  0.5090571
# 0.10   0.0444664213  0.7522375  0.5032541
# 0.55   0.0004446642  0.7571183  0.5134794
# 0.55   0.0044466421  0.7552583  0.5096049
#0.55   0.0444664213  0.7421790  0.4819657
#1.00   0.0004446642  0.7578183  0.5148759
#1.00   0.0044466421  0.7568380  0.5126856
#1.00   0.0444664213  0.6933184  0.3814665
# The final values used for the model were alpha = 1 and
# lambda = 0.0004446642.


# decision tree
dtree <- train(baseline_f1, data = df.train.sample, 
                      method = "rpart", 
                      trControl = fitControl, 
                      tuneLength = 10
                      ## This last option is actually one
                      ## for gbm() that passes through
                      )
dtree
dtree.test <- predict(dtree, df.test)
dtree.results <- confusionMatrix(table(y.test, dtree.test))
dtree.results
# cp           Accuracy   Kappa    
# 0.004467912  0.7996178  0.5994439
# 0.005144869  0.7942974  0.5889220
# 0.005280260  0.7932174  0.5867241
# 0.005686434  0.7917379  0.5837771
# 0.008529651  0.7897574  0.5798992
# 0.013809911  0.7871566  0.5747565
# 0.022339561  0.7556752  0.5107524
# 0.032629299  0.7383976  0.4754647
# 0.056661251  0.6896382  0.3744908
# 0.329000812  0.5774482  0.1433437
# The final value used for the model was cp = 0.004467912.

# random forest
rf <- train(baseline_f1, data = df.train, 
                   method = "rf", 
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)
rf.test <- predict(rf, df.test)
rf.results <- confusionMatrix(table(y.test, dtree.test))
rf.results


# Naive bayes
bayes <- train(baseline_f1, data = df.train.sample, 
                      method = "nb", 
                      trControl = fitControl, 
                      tuneLength = 10
                      ## This last option is actually one
                      ## for gbm() that passes through
)
bayes


baseline_model <- rpart(baseline_f1, df.train, method="class", control = rpart.control(cp = 0.001))
plot(baseline_model)
baseline.test <- predict(baseline_model, df.test, type ='class')
confusionMatrix(table(y.test, baseline.test))

# Random forest based on formula
fit.rndfor <- randomForest(f1, df.train, ntree=500, do.trace=0, importance=TRUE)
plot(fit.rndfor)
importance(fit.rndfor)
yhat.rndfor <- predict(fit.rndfor, df.train)
mse.rndfor <- mean((yhat.rndfor - y.train) ^ 2)
yhat.rndfor.test <- predict(fit.rndfor, df.test)
mse.rndfor.test <- mean((yhat.rndfor.test - y.test) ^ 2)

conf <- fit.rndfor$confusion

conf



# boosted tree
summary(df.train)
fit.btree <- gbm(baseline_f1, data = df.train, n.trees = 5000, distribution='bernoulli', interaction.depth = 2, shrinkage = 0.001, cv.folds=5)
gbm.perf(fit.btree)

summary(fit.btree)

confusionMatrix(fit.btree)


best.iter = gbm.perf(fit.btree, method="cv")


fitControl = trainControl(method="cv", number=5, returnResamp = "all")

cv.btree <- train(baseline_f1, data=df.train, method="gbm",distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
cv.btree

mPred = predict(model2, df.train, na.action = na.pass)








