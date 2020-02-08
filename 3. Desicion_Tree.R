'Decision Tree'

'Decision Tree visualization
For illustration purpose, we are going to use only three variables 
for plotting Decision Trees, they are 
"Contract", "tenure_group" and "PaperlessBilling".'
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)


set.seed(2020)
####Decision tree using RPART
tree <- rpart(Churn ~ .,
               data = training,
               method = "class") 


rpart.plot(tree,
           type = 2, extra = "auto",
           under = FALSE, fallen.leaves = TRUE,
           digits = 2, varlen = 0, faclen = 0, roundint = TRUE,
           cex = NULL, tweak = 1.5,
           clip.facs = FALSE, clip.right.labs = TRUE,
           snip = FALSE,
           box.palette = "auto", shadow.col = 0)


fancyRpartPlot(tree)


####Examine the complexity plot
plotcp(tree)
?plotcp

#### Prune the tree
tree_pruned <- prune(tree, cp = 0.026)

rpart.plot(tree_pruned, cex = NULL, tweak = 1.4)




pred_tree <- predict(tree_pruned,
                      newdata = testing,
                      type = "class")


?confusionMatrix
?predict
confusionMatrix(pred_tree, testing$Churn)

### ROC.CUrve - AUC
roc.curve(testing$Churn, pred_tree, 
          col=2, lwd=2, main = "ROC Curve - Decision Tree")


x <- as.numeric(pred_tree)
y <- as.numeric(testing$Churn)
plot(rocit(x, y), label = "ROC Curve Decision Tree")

roc.curve(y, x, 
          col=2, lwd=2, main = "ROC Curve - Decision Tree")


'1. Out of three variables we use, Contract is the most important 
variable to predict customer churn or not churn.
2. If a customer in a one-year or two-year contract, 
no matter he (she) has PapelessBilling or not, 
he (she) is less likely to churn.
3. On the other hand, if a customer is in a month-to-month contract, 
and in the tenure group of 0-12 month, and using PaperlessBilling, 
then this customer is more likely to churn.
'

'The accuracy for Decision Tree has hardly improved. 
Lets see if we can do better using Random Forest.'
