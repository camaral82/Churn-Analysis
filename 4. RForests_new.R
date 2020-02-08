'Random Forest
Random Forest Initial Model'
library(caret)

set.seed(2020)
rfModel <- randomForest(Churn ~., data = training)
print(rfModel)


'The error rate is relatively low when predicting "No", 
and the error rate is much higher when predicting "Yes".'

#Random Forest Prediction and Confusion Matrix
pred_rf <- predict(rfModel, testing)

confusionMatrix(pred_rf, testing$Churn)



#Random Forest Error Rate
plot(rfModel)
'We use this plot to help us determine the number of trees.
As the number of trees increases, the OOB error rate decreases, 
and then becomes almost constant. 
We are not able to decrease the OOB error rate after 
about 100 to 200 trees.
'

#Tune Random Forest Model
t <- tuneRF(training[, -18], 
            training[, 18], 
            stepFactor = 0.5, 
            plot = TRUE, 
            ntreeTry = 200, 
            trace = TRUE, 
            improve = 0.05)

'We use this plot to give us some ideas on the number of 
mtry to choose. 
OOB error rate is at the lowest when mtry is 2. 
Therefore, we choose mtry=2.
'

#Fit the Random Forest Model After Tuning
rfModel_new <- randomForest(Churn ~., 
                            data = training, 
                            ntree = 200, 
                            mtry = 2, 
                            importance = TRUE, 
                            proximity = TRUE)
print(rfModel_new)

'OOB error rate decreased to 20.57% from 21.04% on Figure 14.'

pred_rf_new <- predict(rfModel_new, testing)

confusionMatrix(pred_rf_new, testing$Churn)
'Both accuracy and sensitivity are improved, compare with Figure 15.'


roc.curve(testing$Churn, pred_rf_new, 
          col=2, lwd=2, main = "ROC Curve - Random Forest")


###Random Forest Feature Importance
varImpPlot(rfModel_new, sort=T, n.var = 10, 
           main = 'Top 10 Feature Importance')


'Summary
From the above example, we can see that Logistic Regression, Decision Tree 
and Random Forest can be used for customer churn analysis 
for this particular dataset equally fine.

Throughout the analysis, I have learned several important things:
Features such as tenure_group, Contract, PaperlessBilling, MonthlyCharges 
and InternetService appear to play a role in customer churn.

There does not seem to be a relationship between gender and churn.

Customers in a month-to-month contract, 
with PaperlessBilling and are within 12 months tenure, 
are more likely to churn; On the other hand, 
customers with one or two year contract, with longer than 12 months tenure, 
that are not using PaperlessBilling, are less likely to churn.
'