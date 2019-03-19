library(tidyverse)
library(e1071)
library(ggfortify)
setwd("C:/Users/jaspe/OneDrive/Documents/Classes/Mathematics/318 - Mathematical Analysis of Statistical Software/Final Project/src")


load("final.model.rda")
# colSums(modeling_house == 0)
# 127 Support Vectors

# Overall Error Analysis
final.model.pred <- predict(final.model, modeling_house)
final.model.err <- mean(final.model.pred != modeling_house$Winner)
sprintf("Final RBF SVM Model with tuned hyperparameters cost=%g and gamma=%g, results in prediction error = %g", final.model$cost, final.model$gamma, final.model.err)

# Confusion Matrix
confusion.matrix <- table(house$Winner, final.model.pred)
row.names(confusion.matrix) <- c("Election Loss", "Election Win")
colnames(confusion.matrix) <- c("Predicted Loss", "Predicted Win")
print(confusion.matrix)


modeling_house <- mutate(modeling_house, Winner_Prediction=as.factor(final.model.pred), 
                      Accurate=as.factor(final.model.pred==modeling_house$Winner))

final.model.true.plt <- ggplot(data = modeling_house) +
  geom_point(mapping = aes(x=Operating_Exp, y=Net_Con, color=Winner)) +
  #facet_wrap(~Seat_Status, ncol = 2)+
  labs(title = "True Election Results Over Two Predictors", x = "Operating Expenditures", y = "Net Contributions") +
  scale_x_continuous(limits = c(0, 5000000)) +
  scale_y_continuous(limits = c(0, 5000000)) +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Election Outcome",
                        breaks=c(1, 0),
                        labels=c("Election Win", "Election Loss"))
print(final.model.true.plt)
ggsave("../pictures/final_model_true.png", final.model.true.plt, height = 8, width = 8)

final.model.pred.plt <- ggplot(data = modeling_house) +
  geom_point(mapping = aes(x=Operating_Exp, y=Net_Con, color=Winner_Prediction)) +
  #facet_wrap(~Seat_Status, ncol = 2)+
  labs(title = "Radial Basis Kernel SVM Prediction Results Over Two Predictors", x = "Operating Expenditures", y = "Net Contributions") +
  scale_x_continuous(limits = c(0, 5000000)) +
  scale_y_continuous(limits = c(0, 5000000)) +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Election Prediction",
                        breaks=c(1, 0),
                        labels=c("Predicted Win", "Predicted Loss"))
print(final.model.pred.plt)
ggsave("../pictures/final_model_pred.png", final.model.pred.plt, height = 8, width = 8)

final.model.err.plt <- ggplot(data = modeling_house) +
  geom_point(mapping = aes(x=Operating_Exp, y=Net_Con, color=Accurate, shape = Winner_Prediction)) +
  #facet_wrap(~Seat_Status, ncol = 2)+
  labs(title = "Radial Basis Kernel SVM Prediction Results Over Two Predictors", x = "Operating Expenditures", y = "Net Contributions") +
  scale_x_continuous(limits = c(0, 5000000)) +
  scale_y_continuous(limits = c(0, 5000000)) +
  #scale_x_continuous(trans = "log10") +
  #scale_y_continuous(trans = "log10") +
  scale_shape_manual(name = "Prediction", breaks=c(1, 0), labels=c("Predicted Win", "Predicted Loss"), values = c(1, 8)) +
  scale_colour_discrete(name ="Prediction Accuracy", breaks=c(TRUE, FALSE), labels=c("Correct Prediction", "Incorrect Prediction"))
print(final.model.err.plt)
ggsave("../pictures/final_model_err.png", final.model.err.plt, height = 10, width = 12)


final.model.sv.plt <- ggplot(data = modeling_house[final.model$index,]) +
  geom_point(mapping = aes(x=Operating_Exp, y=Net_Con, color=final.model$coefs)) +
  labs(title = "Support Vectors of Radial Basis Kernel SVM Model", x = "Operating Expenditures", y = "Net Contributions") +
  scale_x_continuous(limits = c(0, 5000000)) +
  scale_y_continuous(limits = c(0, 5000000)) +
  scale_colour_continuous(name ="Support Vector\nCoefficient")
print(final.model.sv.plt)
ggsave("../pictures/final_model_sv.png", final.model.sv.plt, height = 4, width = 10)


mean.df <- data_frame(Party = factor(),
                      Seat_Status = factor(),
                      Ind_Itemized_Con = double(),
                      Ind_Unitemized_Con = double(),
                      Party_Con = double(),
                      Com_Con = double(),
                      Self_Con = double(),
                      Operating_Exp = double(),
                      Total_Loans = double(),
                      Net_Con = double())
for (p in c("REP", "DEM", "Other")) {
  for (s in c("INCUMBENT", "OPEN", "CHALLENGER")) {
    mean.df <- rbind(mean.df, data_frame(Party = p,
                              Seat_Status = s,
                              Ind_Itemized_Con = mean(modeling_house$Ind_Itemized_Con),
                              Ind_Unitemized_Con = mean(modeling_house$Ind_Unitemized_Con),
                              Party_Con = mean(modeling_house$Party_Con),
                              Com_Con = mean(modeling_house$Com_Con),
                              Self_Con = mean(modeling_house$Self_Con),
                              Operating_Exp = mean(modeling_house$Operating_Exp),
                              Total_Loans = mean(modeling_house$Total_Loans),
                              Net_Con = seq(0, max(modeling_house$Net_Con), by = 1000)))
  }
}
mean.df$Party <- as.factor(mean.df$Party)
mean.df$Seat_Status <- as.factor(mean.df$Seat_Status)

# Need to construct new model that produces probabilities instead of just classes
# Use cross validated optimal gamma and cost values from prior training
final.model.prob <- svm(Winner~Party + Seat_Status + Ind_Itemized_Con + Ind_Unitemized_Con + Party_Con + Com_Con + Self_Con + Operating_Exp + Net_Con + Total_Loans,
                                    data = modeling_house,
                                    kernel = "radial",
                                    probability = TRUE,
                                    cost=final.model$cost,
                                    gamma = final.model$gamma)


mean.pred.probs <- predict(final.model.prob, mean.df, probability = TRUE)
win.pred.probs <- attr(mean.pred.probs, "probabilities")[,1]
win.pred.probs.df <- data_frame(Win_Probability = win.pred.probs)

mean.pred_plt <- ggplot(data = mean.df)+
  geom_point(mapping = aes(x = Net_Con, y = win.pred.probs.df$Win_Probability, color = Party)) +
  facet_wrap(~Seat_Status, ncol = 3) +
  #facet_grid(Party ~ Seat_Status) +
  labs(title = "Model Predictions with respect to Net Contributions", subtitle = "Other Variables held constant at their means", x = "Net Contributions", y = "Predicted Win Probability") +
  scale_x_continuous(limits = c(0, 1000000), breaks = seq(0, 1000000, 500000), labels = c("$0","Half Mil", "$1 M")) +
  scale_color_manual(values=c("blue", "gray50", "red"))
print(mean.pred_plt)
ggsave("../pictures/final_model_predicted_values.png", mean.pred_plt, height = 4, width = 12)

mean.pred_plt_party <- ggplot(data = mean.df)+
  geom_point(mapping = aes(x = Net_Con, y = win.pred.probs.df$Win_Probability, color = Seat_Status)) +
  facet_wrap(~Party, ncol = 3) +
  #facet_grid(Party ~ Seat_Status) +
  labs(title = "Model Predictions with respect to Net Contributions", subtitle = "Other Variables held constant at their means", x = "Net Contributions", y = "Predicted Win Probability") +
  scale_x_continuous(limits = c(0, 1000000), breaks = seq(0, 1000000, 500000), labels = c("$0","Half Mil", "$1 M")) +
  scale_color_manual(values=c("red", "blue", "darkgreen"))
print(mean.pred_plt_party)
ggsave("../pictures/final_model_predicted_values_party.png", mean.pred_plt_party, height = 4, width = 12)

# PCA Attempt
#supportVector_indices <- final.model$index
#pca.support_vectors <- prcomp(select_if(modeling_house, is.numeric)[supportVector_indices,], scale=TRUE)
#positive_coefs <- data_frame(SVCoefs = as.vector(final.model$coefs - min(final.model$coefs[,1])))
#pca.sv.plt <- autoplot(pca.support_vectors, data = modeling_house[supportVector_indices,], colour = Winner) +
#  ggtitle("PCA of Campaign Modeling Data")
#print(pca.sv.plt)