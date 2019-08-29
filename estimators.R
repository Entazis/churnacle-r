
library(caret)

user_next_all3_p2 <- user_next_all3[-(1:79),]
names(user_next_all3_p2) <- gsub("-",".",names(user_next_all3_p2))
row.names(user_next_all3_p2) <- 1:nrow(user_next_all3_p2)
user_next_all3_p2 <- user_next_all3_p2[complete.cases(user_next_all3_p2),]
user_next_all3_p2 <- user_next_all3_p2 %>%
  select(-email) %>%
  select(-endDate) %>%
  drop_na() %>%
  select(paidnext, everything())

set.seed(100)
trainIndex <- createDataPartition(user_next_all3_p2$paidnext, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_tbl <- user_next_all3_p2[ trainIndex,]
test_tbl  <- user_next_all3_p2[-trainIndex,]

colnames(train_tbl) <- c("paidnext", 1:1185)

control <- trainControl(method="cv", number=10, verboseIter = TRUE) 
metric <- "Kappa"

print("training model: treebag_all")
m_treebag_all <- train(paidnext~., data=train_tbl, method="treebag",
                       metric=metric, trControl=control)
print("training model: rpart_all")
m_rpart_all <- train(paidnext~., data=train_tbl, method="rpart",
                      metric=metric, trControl=control)
print("training model: rf_all")
m_rf_all <- train(paidnext~., data=train_tbl, method="rf", 
                   metric=metric, trControl=control)
print("training model: ranger_all")
m_ranger_all <- train(paidnext~., data=train_tbl, method="ranger", 
                       metric=metric, trControl=control)

model_performances <- data.frame(model = NA, Accuracy = NA, Kappa = NA, TP = NA, FP = NA, FN = NA, TN = NA)

print("evaluating: treebag_all")
print("on training data: ")
mtx <- confusionMatrix(predict(m_treebag_all, train_tbl), train_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="treebag_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
print("on test data: ")
mtx <- confusionMatrix(predict(m_treebag_all, test_tbl), test_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="treebag_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))

print("evaluating: rpart_all")
print("on training data: ")
mtx <- confusionMatrix(predict(m_rpart_all, train_tbl), train_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="rpart_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
print("on test data: ")
mtx <- confusionMatrix(predict(m_rpart_all, test_tbl), test_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="rpart_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))

print("evaluating: rf_all")
print("on training data: ")
mtx <- confusionMatrix(predict(m_rf_all, train_tbl), train_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="rf_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
print("on test data: ")
mtx <- confusionMatrix(predict(m_rf_all, test_tbl), test_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="rf_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))

print("evaluating: ranger_all")
print("on training data: ")
mtx <- confusionMatrix(predict(m_ranger_all, train_tbl), train_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="ranger_all_ontraining", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))
print("on test data: ")
mtx <- confusionMatrix(predict(m_ranger_all, test_tbl), test_tbl$paidnext)
print(mtx)
model_performances <- rbind(model_performances, c(model="ranger_all_ontest", Accuracy=mtx$overall["Accuracy"], Kappa=mtx$overall["Kappa"], 
                                                   TP=mtx$table["no","no"], FP=mtx$table["no","yes"], FN=mtx$table["yes","no"], TN=mtx$table["yes","yes"]))


model_performances <- model_performances[complete.cases(model_performances), ]
#model_performances <- model_performances[order(desc(model_performances[,"Kappa"]),desc(model_performances[,"Accuracy"])),]
model_performances$Kappa <- round(as.numeric(model_performances$Kappa),3)
model_performances$Accuracy <- round(as.numeric(model_performances$Accuracy),3)