
'Models Comparison'

Logistic_Regression <- c(0.8088, 0.9044, 0.5446, 0.8459, 0.6733)
Decision_Tree <- c(0.7965, 0.9516, 0.3679, 0.8062, 0.7331)
Random_Forest <- c(0.8060, 0.9283, 0.4679, 0.8282, 0.7024)


comparison <- t(data.frame(Logistic_Regression, 
                           Decision_Tree,
                           Random_Forest))


colnames(comparison) <-  c("Accuracy", "Sensitivity", "Specificity",
                           "Pos Pred Value",
                           "Neg Pred Value")

print("Models Comparison"); comparison

 #########
Logistic_Regression <- c(0.8088, 0.9044, 0.5446)
Decision_Tree <- c(0.7965, 0.9516, 0.3679)
Random_Forest <- c(0.8060, 0.9283, 0.4679)


comparison <- t(data.frame(Logistic_Regression, 
                           Decision_Tree,
                           Random_Forest))


colnames(comparison) <-  c("Accuracy", "Sensitivity", "Specificity")

print("Models Comparison"); comparison


