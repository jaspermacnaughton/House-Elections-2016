library(tidyverse)
library(ISLR)
library(caret)
library(FNN) # KNN
library(MASS)
library(e1071) # SVM
setwd("C:/Users/jaspe/OneDrive/Documents/Classes/Mathematics/318 - Mathematical Analysis of Statistical Software/Final Project/src")

set.seed(10)

load("campaignFunding.rda")
house <- subset(congressional, Office=="H")
modeling_house <- subset(house, select = -c(Office, State, Ind_Total_Con, Total_Con, Numeric_Winner))
#modeling_house <- subset(house, select = -c(Office, State, Ind_Total_Con, Party_Con, Com_Con, Self_Con, Total_Con, Numeric_Winner))

# Just allow republicans and democrats for simplicity - convert all to "Other"
levels(modeling_house$Party)[levels(modeling_house$Party) != "DEM" & levels(modeling_house$Party) != "REP"] <- "Other"


# View Number of zeros
colSums(modeling_house == 0)


# Some meaning could be assigned to Seat_Status
# 0 for CHALLENGER, 1 for OPEN, 2 for INCUMBENT
modeling_house$Seat_Status_numeric <- 0
modeling_house$Seat_Status_numeric[modeling_house$Seat_Status=="OPEN"] <- 1
modeling_house$Seat_Status_numeric[modeling_house$Seat_Status=="INCUMBENT"] <- 2


# Set folds
K = 10
folds <- createFolds(seq(nrow(modeling_house)),k=K)

logistic_err = 0
LDA_err = 0
QDA_err= 0
knn3_err = 0
knn5_err = 0
linear.svm_err = 0
radial.svm_err = 0

# Loop through the folds and compute the k fold error estimates
for(j in seq(K)){
  train.df <- modeling_house[-folds[[j]],]
  test.df <- modeling_house[folds[[j]],]
  
  # Add Logistic Error for this fold
  logistic.train <- glm(Winner~Party + Seat_Status + Ind_Itemized_Con + Ind_Unitemized_Con + Party_Con + Com_Con + Self_Con + Operating_Exp + Net_Con + Total_Loans, train.df, family = binomial)
  logistic.prob <- predict(logistic.train, newdata = test.df, type = "response")
  logistic.pred<- rep(0, nrow(test.df))
  logistic.pred[logistic.prob>0.5] <- 1
  logistic_err <- logistic_err + mean(logistic.pred != test.df$Winner)/K
  
  # Add LDA Error for this fold
  lda.train <- lda(Winner~Party + Seat_Status + Ind_Itemized_Con + Ind_Unitemized_Con + Party_Con + Com_Con + Self_Con + Operating_Exp + Net_Con + Total_Loans, data = train.df)
  lda.pred <- predict(lda.train, test.df)
  LDA_err <- LDA_err + mean(lda.pred$class != test.df$Winner)/K
  
  # Add QDA Error for this fold
  qda.train <- qda(Winner~Party + Seat_Status + Ind_Itemized_Con + Ind_Unitemized_Con + Party_Con + Com_Con + Self_Con + Operating_Exp + Net_Con + Total_Loans, data = train.df)
  qda.pred <- predict(qda.train, test.df)
  QDA_err <- QDA_err + mean(qda.pred$class != test.df$Winner)/K
  
  # Add KNN 3 Error for this fold
  knn3.pred <- knn(as.matrix(subset(train.df, select = -c(Winner, Party, Seat_Status))), as.matrix(subset(test.df, select = -c(Winner, Party, Seat_Status))), as.matrix(train.df$Winner), k=3)
  knn3_err <- knn3_err + mean(knn3.pred!= test.df$Winner)/K
  
  # Add KNN 5 Error for this fold
  knn5.pred <- knn(as.matrix(subset(train.df, select = -c(Winner, Party, Seat_Status))), as.matrix(subset(test.df, select = -c(Winner, Party, Seat_Status))), as.matrix(train.df$Winner), k=5)
  knn5_err <- knn5_err + mean(knn5.pred!= test.df$Winner)/K
  
  # Linear SVM
  linear.svm <- tune(svm, Winner~Party + Seat_Status + Ind_Itemized_Con + Ind_Unitemized_Con + Party_Con + Com_Con + Self_Con + Operating_Exp + Net_Con + Total_Loans,data=train.df, kernel="linear",
                        ranges=list(cost=10^seq(-3,3,length=10)))
  linear.svm_err <- linear.svm_err + mean(predict(linear.svm$best.model, test.df) != test.df$Winner)/K
  
  # Radial SVM
  radial.svm <- tune(svm, Winner~Party + Seat_Status + Ind_Itemized_Con + Ind_Unitemized_Con + Party_Con + Com_Con + Self_Con + Operating_Exp + Net_Con + Total_Loans,data=train.df, kernel="radial",
                     ranges=list(cost=10^seq(-3,3,length=10),
                                 gamma=10^seq(-2,0,length=10)))
  radial.svm_err <- radial.svm_err + mean(predict(radial.svm$best.model, test.df) != test.df$Winner)/K
}

sprintf("Logistic %d-fold Error = %g", K, logistic_err)
sprintf("LDA %d-fold Error = %g", K, LDA_err)
sprintf("QDA %d-fold Error = %g", K, QDA_err)
sprintf("KNN with k=3 %d-fold Error = %g", K, knn3_err)
sprintf("KNN with k=5 %d-fold Error = %g", K, knn5_err)
sprintf("Linear SVM %d-fold Error = %g", K, linear.svm_err)
sprintf("Radial SVM %d-fold Error = %g", K, radial.svm_err)


# Since SVM model performed the best, dig deeper
last.radial.svm.perf <- radial.svm$performances
last.radial.svm.long <- gather(subset(last.radial.svm.perf, select = -c(dispersion)), attribute, value, -error)

radial.svm.hyperparams.plt <- ggplot(data = last.radial.svm.long) +
  geom_point(mapping = aes(x = value, y = error)) +
  facet_wrap(~attribute, ncol = 2, scales = "free") + 
  scale_x_continuous(trans = "log10") +
  labs(title = "Visualization of Radial SVM Hyperparameters", x = "Parameter", y = "Estimated Error Rate")
print(radial.svm.hyperparams.plt)
ggsave("../pictures/Radial_SVM_Hyperparameters_Faceted_Plt.png", radial.svm.hyperparams.plt, height = 10, width = 8)

cost_lower_cutoff <- 10^-1
last.radial.svm.perf.trimmed <- subset(last.radial.svm.perf, last.radial.svm.perf$cost > cost_lower_cutoff)
cost_min <- radial.svm$best.parameters$cost
gamma_min <- radial.svm$best.parameters$gamma
radial.svm.hyperparams.single.plt <- ggplot(data = last.radial.svm.perf.trimmed) +
  geom_point(mapping = aes(x = cost, y = gamma, color = error), size = 4) +
  geom_point(aes(x = cost_min, y = gamma_min), alpha = 0.1, color = "red", size = 4) + 
  #geom_rect(mapping = aes(xmin = cost_lb, xmax = cost_ub, ymin = gamma_lb, ymax = gamma_ub, fill = "red", alpha=0.1), color="black") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(title = "Visualization of Radial SVM Hyperparameters (Single Plot)", x = "Cost", y = "Gamma")
print(radial.svm.hyperparams.single.plt)
ggsave("../pictures/Radial_SVM_Hyperparameters_Single_Plt.png", radial.svm.hyperparams.single.plt, height = 10, width = 8)


final.model.cv <- tune(svm, Winner~Party + Seat_Status + Ind_Itemized_Con + Ind_Unitemized_Con + Party_Con + Com_Con + Self_Con + Operating_Exp + Net_Con + Total_Loans, data = modeling_house,
                    kernel="radial",
                   ranges=list(cost=10^seq(1,4,length=31),
                               gamma=10^seq(-3,-1,length=21)))
final.model <- final.model.cv$best.model


save(final.model, house, modeling_house, file = "final.model.rda")








