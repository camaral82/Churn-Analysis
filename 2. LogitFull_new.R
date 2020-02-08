'LOGISTIC REGRESSION'
x <- 10
plot(function(x) exp(1+x)/(1+exp(1+x)), 
     from=-10,to=10)


'Fitting the Logistic Regression Model:'
###Logistic model
set.seed(2020)
LogModel <- glm(Churn ~ .,
                family=binomial(link="logit"),
                data=training)
print(summary(LogModel))


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
print("Pseudo R2"); LogRegR2(LogModel_new)


# Take a look at the model
summary(LogModel_new)


###Odds Ratio
exp(cbind(OR=coef(LogModel_new), confint(LogModel)))


'Feature Analysis:
The top three most-relevant features include 
Contract, tenure_group and PaperlessBilling.'

anova(LogModel_new, test="Chisq")
'Analyzing the deviance table we can see the drop in deviance 
when adding each variable one at a time. 
Adding InternetService, Contract and tenure_group significantly 
reduces the residual deviance. 
The other variables such as PaymentMethod and Dependents 
seem to improve the model less even though they all have low p-values.'

# Predict on test: p
pred2 <- predict(LogModel_new, 
                 testing, 
                 type <- "response")


'Calculate a confusion matrix'
# If p exceeds threshold of 0.5, Churn or not Churn
fitted_results2 <- as.factor(ifelse(pred2 > 0.5, "Yes", "No"))


confusionMatrix(fitted_results2, testing$Churn)


#### ROC and AIC
library(ROSE)

roc.curve(testing$Churn, fitted_results2, 
          col=2, lwd=2, main = "ROC Curve - Logistic Regression")


library(ROCit)
x <- as.numeric(fitted_results2)
y <- as.numeric(testing$Churn)
plot(rocit(x, y))
