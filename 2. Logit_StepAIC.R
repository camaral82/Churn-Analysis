'Model Specification
The stepAIC() function gives back a reduced model
MASS library'
#Build the new model
logitModelNew  <- stepAIC(LogModel,trace = FALSE) 

#Look at the model
summary(logitModelNew) 

# Save the formula of the new model (it will be needed for the out-of-sample part) 
formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit

#creating a model with the selected variables
LogModel_new <- glm(formulaLogit,
                   data = training,
                   family=binomial(link="logit"))


#Pseudo R2
LogRegR2(LogModel_new)



# Take a look at the model
summary(LogModel_new)


###Odds Ratio
exp(cbind(OR=coef(LogModel_new), confint(LogModel)))


# Predict on test: p
pred2 <- predict(LogModel_new, 
                 testing, 
                 type <- "response")


'Calculate a confusion matrix'
# If p exceeds threshold of 0.5, Churn or not Churn
fitted_results2 <- as.factor(ifelse(pred2 > 0.5, "Yes", "No"))


confusionMatrix(fitted_results2, testing$Churn)
