library(tidyverse)
library(GGally)

#library(e1071) #SVM
setwd("C:/Users/jaspe/OneDrive/Documents/Classes/Mathematics/318 - Mathematical Analysis of Statistical Software/Final Project/src")

load("campaignFunding.rda")
house = subset(congressional, Office=="H")

p_t <- 0.5
colSums(house == 0)

# Single Predictor Classifier
Total_Con.classifier <- glm(Winner~Total_Con, data = house, family = binomial)
Total_Con.classifier.prob <- predict(Total_Con.classifier, type = "response")
Total_Con.classifier.pred <- rep(0, nrow(house))
Total_Con.classifier.pred[Total_Con.classifier.prob > p_t] <- 1
sprintf("Total Contribution predictive model error rate = %g", mean(Total_Con.classifier.pred != house$Winner))

Total_Con.classifier.betas <- Total_Con.classifier$coefficient
x_t <- log(p_t/(1-p_t))/Total_Con.classifier.betas[2]-Total_Con.classifier.betas[1]/ Total_Con.classifier.betas[2]


Total_Con.classifier.plt <- ggplot(data = house, mapping = aes(x = Total_Con, y = Numeric_Winner)) +
  geom_point(mapping = aes(x = Total_Con, y = Numeric_Winner, color = as.factor(Total_Con.classifier.pred)), show.legend = FALSE) +
  geom_point(mapping = aes(x = Total_Con, y = Total_Con.classifier.prob, color = "blue"), show.legend = FALSE) +
  geom_vline(aes(xintercept=x_t), color="red") +
  xlab("Total Campaign Contributions") + ylab("Election Outcome") +
  ggtitle("1 Predictor Simple Logistic Model Visualization") +
  scale_x_continuous(limits = c(0, 5000000))
print(Total_Con.classifier.plt)

ggsave("../pictures/Simple_Logistic_1d.png", Total_Con.classifier.plt, height = 4, width = 5)


# Simple 2d Logistic Model
simple.logistic.classifier <- glm(Winner~Net_Con+Operating_Exp, data = house, family = binomial)
simple.logistic.prob <- predict(simple.logistic.classifier, newdata = house, type = "response")
simple.logistic.pred <- rep(0, length(simple.logistic.prob))
simple.logistic.pred[simple.logistic.prob>p_t] <- 1
sprintf("2d predictive model error rate = %g", mean(simple.logistic.pred != house$Winner))

# View Prediction Accuracy
print(table(house$Winner, simple.logistic.pred))

simple.logistic.betas <- simple.logistic.classifier$coefficients # extract betas
simple.logistic.intercept <- (log(p_t/(1-p_t)) - simple.logistic.betas[1])/simple.logistic.betas[2]
simple.logistic.slope <- -simple.logistic.betas[3]/simple.logistic.betas[2]

simple.logistic.plt <- ggplot(data = house)+
  geom_point(mapping = aes(x = Operating_Exp, y = Net_Con, color = Winner, alpha=0.5)) + #, shape = as.factor(simple.logistic.pred))
  geom_abline(intercept = simple.logistic.intercept, slope = simple.logistic.slope, color="red") +
  labs(title = "2 Predictor Simple Logistic Model Visualization", x = "Operating Expenditures", y = "Net Contributions") +
  scale_x_continuous(limits = c(0, 5000000)) +
  scale_y_continuous(limits = c(0, 5000000))
print(simple.logistic.plt)
ggsave("../pictures/Simple_Logistic_2d.png", simple.logistic.plt, height = 4, width = 5)





