library(tidyverse)
library(lubridate)
library(dbplyr)
library(stringr)
library(class)
library(reader)
library(tidytext)
library(tidyr)
library(ROCR)
library(pROC)
library(ranger)
library(cvms)

set.seed(24601)

NPBD <- read_csv("NPDB.csv") #Clean data set from end of cleaning script

## Fitting a logistic regression
# Making sure certain variables are factors/numeric...
NPBD$OUTCOME <- as.numeric(NPBD$OUTCOME)

NPBD$PRACTAGE <- as.factor(NPBD$PRACTAGE)
NPBD$GRAD <- as.factor(NPBD$GRAD)
NPBD$STATE <- as.factor(NPBD$STATE)
NPBD$PTAGE <- as.factor(NPBD$PTAGE)
NPBD$PTGENDER <- as.factor(NPBD$PTGENDER)
NPBD$PTTYPE <- as.factor(NPBD$PTTYPE)
NPBD$AA_INDICATOR <- as.factor(NPBD$AA_INDICATOR)
NPBD$SEVERITY <- as.factor(NPBD$SEVERITY)
NPBD$ADDT_ALLEGNT <- as.numeric(NPBD$ADDT_ALLEGNT)  

# Creating train and test set
clean.NPBD <- na.omit(NPBD)
train.NA <- clean.NPBD %>% filter(ORIGYEAR >= 2010 & ORIGYEAR <= 2018)
test.NA <- clean.NPBD %>% filter(ORIGYEAR > 2018)

# Fit a logistic regression on train predicting outcome with all plausible predictors in dataset
log.fit <- glm(OUTCOME ~ SEVERITY + ALGNNATR + PREVIOUS_CASES + AA_INDICATOR + LCNFIELD_GRP+ 
                  STATE + PTGENDER + PTTYPE + ADDT_ALLEGNT + PRACTAGE + PTAGE + NUMBPRSN, 
               data = train.NA,                                                                         
               family = "binomial")
summary(log.fit)

# computing the AUC score of the fitted model on the test set
test.NA$predicted.prob <- predict(log.fit, test.NA, type = 'response')
aucLog <- auc(test.NA$OUTCOME, test.NA$predicted.prob)
round(aucLog,2)



#### Using RF for Feature Selection ####
## Fitting a random forest model ( Had to switch over to ranger package, randomForest was taking so long!)
feature.fit <- ranger(OUTCOME ~ SEVERITY + ALGNNATR + PREVIOUS_CASES + AA_INDICATOR + LCNFIELD_GRP+ 
                   STATE + PTGENDER + PTTYPE + ADDT_ALLEGNT + PRACTAGE + PTAGE + NUMBPRSN,
                 data = train.NA, 
                 num.trees = 1000,
                 importance = "permutation",
                 respect.unordered.factors = T,
                 probability = T )


tib <- tibble(feature.fit$variable.importance)
v <- as.vector(tib$`feature.fit$variable.importance`)
v <- round(v,7)
w <-(as.vector(names(feature.fit$variable.importance)))
df <-cbind(w,v)
df <-as.data.frame(df)
df <- df %>% arrange(v)
df$w <- factor(df$w, levels = df$w)
 

# Feature plot based on importance
p <- ggplot(df, aes(x = w, y = v))
p <- p + geom_bar(stat="identity", color='skyblue',fill='steelblue')
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p <- p + ggtitle("Information Value Summary") + xlab("Predictor") + ylab("Importance") + coord_flip()
p 
####


## Creating a model with the top six (half of original number of predictors) most important features
# Creating train and test set
log.fit2 <- glm(OUTCOME ~ SEVERITY + STATE + PTAGE + LCNFIELD_GRP + ALGNNATR + PTTYPE, 
               data = train.NA, 
               family = "binomial")
summary(log.fit2)

# computing the AUC score of the fitted model on the test set
test.NA$predicted.prob2 <- predict(log.fit2, test.NA, type = 'response')
aucLog2 <- auc(test.NA$OUTCOME, test.NA$predicted.prob2)
round(aucLog2,2)

AIC(log.fit) # 73368.35
BIC(log.fit) # 74490.73

AIC(log.fit2) # 73594.18
BIC(log.fit2) # 74591.85
# Its very close but overall our first model fits better according to AIC and BIC...


# Using ranger on best fit model
ranger_model <- ranger(OUTCOME ~ SEVERITY + ALGNNATR + PREVIOUS_CASES + AA_INDICATOR + LCNFIELD_GRP+ 
                         STATE + PTGENDER + PTTYPE + ADDT_ALLEGNT + PRACTAGE + PTAGE + NUMBPRSN,
                       data = train.NA,
                       num.trees = 1000,
                       respect.unordered.factors = "order")
ranger_model

# Computing the AUC score of the fitted model on the test set
pred.test <- predict(ranger_model, data = test.NA)
rocRF <- roc(as.numeric(test.NA$OUTCOME), pred.test$predictions)
aucRF <- pROC::auc(rocRF)
round(aucRF, 2)





## Checking Model Comparison using Precision Plots
# Computing the AUC score of the fitted model on the test set
test.NA$predicted.prob <- predict(log.fit, test.NA, type = 'response')
aucLog <- auc(test.NA$OUTCOME, test.NA$predicted.prob)
round(aucLog,2) #[1] 0.82 Still slighlty greater than RF AUC

# Creating Precision-at-k plot data
test.NA$rf.preds <-pred.test$predictions

log.plotdata <- test.NA %>% as.data.frame() %>% arrange(desc(predicted.prob)) %>%
     mutate(rank = row_number(),
                       model = "Logistic Regression",
                       precision = cumsum(OUTCOME)/rank)


rf.plotdata <-  test.NA  %>% as.data.frame() %>% arrange(desc(rf.preds)) %>%
     mutate(rank = row_number(),
                       model = "Random Forest",
                       precision = cumsum(OUTCOME)/rank)
plot.data <- rbind(log.plotdata, rf.plotdata)

# Generate plot
p <- ggplot(data = plot.data, aes(x = rank, y = precision, color = model))
p <- p + geom_line()
p <- p + scale_x_continuous('Number of Malpractice Payment Reports')
p <- p + scale_y_continuous("Precision")
p <- p + labs(title = "Precision-at-k Curves")
p






## Creating Confusion matrices based on a 0.50 threshold
# RF
test.NA$rf.outcome <- ifelse(test.NA$rf.preds >= 0.50, 1, 0)

eval <- evaluate(
  data = test.NA,
  target_col = "OUTCOME",
  prediction_cols = "rf.outcome",
  type = "binomial"
)

plot_confusion_matrix(
  eval[["Confusion Matrix"]][[1]],
  palette = "Greens",
  theme_fn = ggplot2::theme_light
) + labs(x = "Outcome", y = "Random Forest Prediction", title = "Random Forest Performance") 


# Logistic Regression Model Confusion Matrix
test.NA$log.outcome <- ifelse(test.NA$predicted.prob >= 0.50, 1, 0)

eval2 <- evaluate(
  data = test.NA,
  target_col = "OUTCOME",
  prediction_cols = "log.outcome",
  type = "binomial"
)

plot_confusion_matrix(
  eval2[["Confusion Matrix"]][[1]],
  palette = "Blues",
  theme_fn = ggplot2::theme_light
) + labs(x = "Outcome", y = "Logistic Regression Prediction", title = "Logistic Regression Performance") 
