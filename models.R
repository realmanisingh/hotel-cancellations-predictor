
library(pROC)
# Baseline model with only logistic regression without country and weather
colnames(df.train)
summary(df.train)
baseline_model <- rpart(is_canceled~., df.train[,c(1:29)], method = 'class', control = rpart.control(cp = 0.001))
plot(baseline_model)
saveRDS(baseline_model, './baseline.rds')
baseline_load <- readRDS('./baseline.rds')

baseline.train <- predict(baseline_model, df.train, type = 'class')
confusionMatrix(table(y.train, baseline.train))
baseline.test <- predict(baseline_model, df.test, type ='class')
confusionMatrix(table(y.test, baseline.test))

#
baseline_f1 <- as.formula(is_canceled ~  deposit_type + 
                            km + lead_time + market_segment + total_of_special_requests + adr + 
                            agent + arrival_date_month + average_temp + day_of_week + customer_type +
                            required_car_parking_spaces + arrival_date_day_of_month + max_temp + assigned_room_type + min_temp + 
                            previous_cancellations + stays_in_week_nights + booking_changes + deposit_type)

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
                 verbose = FALSE, preProc = 'zv')
b.tree

# results:

# interaction.depth  n.trees  Accuracy   Kappa    
# 1                   50      0.7641004  0.5283804
# 1                  100      0.7900646  0.5802152
# 1                  150      0.7998428  0.5997430
# 2                   50      0.7956225  0.5913156
# 2                  100      0.8124813  0.6249873
# 2                  150      0.8177839  0.6355810
# 3                   50      0.8066617  0.6133577
# 3                  100      0.8189819  0.6379758
# 3                  150      0.8217223  0.6434529
# The final values used for the model were n.trees =
# 150, interaction.depth = 3, shrinkage = 0.1
# and n.minobsinnode = 10.
b.tree.table <- data.table(interaction.depth = c(1, 1, 1, 2, 2, 2, 3, 3, 3), 
                              n.trees = c(50, 100, 150, 50, 100, 150, 50, 100, 150),
                                          Accuracy = c(0.7641004, 0.7900646, 0.7998428, 0.7956225, 
                                                      0.8123813, 0.8177839,0.8066617, 0.8189819, 0.8217223),
                                         Kappa = c(0.5283804,0.5802152, 0.5997430, 0.5913156, 0.6249873, 0.6355810,
                                                   0.6133577, 0.6379758,0.6434529) )



df.train$is_canceled <- as.character(df.train$is_canceled)
b.tree<- gbm(baseline_f1, data = df.train, distribution = "bernoulli", n.trees = 150,
                            interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10)
saveRDS(b.tree, './final_model_with_gbm.rds')
gbm.btree.test <- predict(b.tree, df.test, n.trees = 150, type = 'response')

plot(roc(y.test, gbm.btree.test))
# training model
# Accuracy   Kappa    
# 0.8331858  0.6663621
# gbm method ##FINAL MODEL##
df.train$is_canceled <- factor(df.train$is_canceled, levels=unique(c(0, 1)))
                                  
btree.grid <- expand.grid(interaction.depth = 3, n.trees = 150, 
                          shrinkage = 0.1, n.minobsinnode = 10)
#### FINAL MODEL #####
final.btree <- train(baseline_f1, df.train, method = 'gbm', tuneGrid = btree.grid, verbose = FALSE, preProc = 'zv')
saveRDS(final.btree, './final_model.rds')
#train set

final.btree.train <- predict(final.btree, df.train, n.trees = 150)
btree.train.results <- confusionMatrix(table(y.train, final.btree.train))
btree.train.results
# test set
final.btree.test <- predict(final.btree, df.test, n.trees = 150)
plot(roc(y.test, as.numeric(final.btree.test)))



btree.results <- confusionMatrix(table(y.test, final.btree.test))
btree.results

# Accuracy : 0.8358
# final.btree.test
# y.test    0    1
# 0 5575 1022
# 1 1153 5497

# lasso
lasso <- train(baseline_f1, data = df.train.sample, 
                 method = "glmnet", 
                 family = "binomial",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE, preProc = 'zv')
lasso
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

# lasso on test set
# lasso.grid <- expand.grid(alpha = 1, lambda = 0.0004446642)
# lasso.final <- train(baseline_f1, data = df.train, 
#               method = "glmnet", 
#               family = "binomial",
#               verbose = FALSE, tuneGrid = lasso.grid, preProc = 'zv')
# lasso.final.test <- predict(lasso.final, df.test)
# lasso.results <- confusionMatrix(table(y.test, lasso.final.test))
# lasso.results
# Accuracy: 0.7663 

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








