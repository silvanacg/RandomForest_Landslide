library(dplyr)
library(randomForest)
library(caret)
library(pROC)
library(tibble)
library(knitr)

path = "/Users/silvanacastillo/Documents/UBC/2018TERM2/TOPICSGEOMHIDROL/PROJECT/variables.csv"
data = read.csv(path)
sum(data$MM_Inv == 1)

set.seed(300)
landslidespx = subset(data, data$MM_Inv == 1)
nonlandslidespx = subset(data, data$MM_Inv== 0)

landslide_sample = sample_n(landslidespx, 948)
landslidetst_sample = setdiff(landslidespx, landslide_sample)
nonlandslide_sample = sample_n(nonlandslidespx, 948)
nonlandslidetst_sample = sample_n(nonlandslidespx, 407)
train_dataf = rbind(landslide_sample,nonlandslide_sample)
train_dataf = train_dataf[sample(nrow(train_dataf)), ]
test_dataf = rbind(landslidetst_sample,nonlandslidetst_sample)
test_dataf = test_dataf[sample(nrow(test_dataf)), ]

train_dataf = dplyr::select(train_dataf, -OBJECTID,-Value, -Count)
test_dataf = dplyr::select(test_dataf, -OBJECTID,-Value, -Count)

correlationMatrix <- cor(train_dataf[,1:9])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

train_dataf = dplyr::select(train_dataf, -ends_with("VR"))
test_dataf = dplyr::select(test_dataf, -ends_with("VR"))

train_dataf$MM_Inv = as.factor(train_dataf$MM_Inv)

model_trial <- train(MM_Inv ~ r_vias + r_pendient + r_geom + r_geologia + 
                       r_fallas + r_drenajes + r_curvatur + r_cobertur,  data = train_dataf, 
                     method = 'rf')
pred = predict.train(model_trial, newdata = test_dataf, type ='prob')
# pred = as.numeric(pred) - 1
# test_dataf$MM_Inv = as.factor(test_dataf$MM_Inv)
# confussion = confusionMatrix(pred, test_dataf$MM_Inv)

pred = dplyr::select(pred, -ends_with("0"))
pred = as.numeric(unlist(pred))
roc_value = roc(test_dataf$MM_Inv, pred)
plot.roc(roc_value, col = "black", lty = 1, print.auc = TRUE, legacy.axes = TRUE, xlim = c(1,-0.2))
ciobj = ci.se(roc_value)
plot(ciobj, type = 'shape', col = 'lightyellow')
plot(ci(roc_value, of = 'thresholds', thresholds = 'best'))
legend("bottomright", legend = c("AUC - RF Model", "Confidence intervals (0.74 - 0.80)"), bty = "n", 
       lty = c(1, NA), col = c("black", NA), density = c(0,100), fill = c(NA, "lightyellow"),
       border = c(NA, "black"), x.intersp = c(2, 0.5))

data_pred = data[4:13]

pred = predict.train(model_trial, newdata = data_pred, type = 'prob')
pred = dplyr::select(pred, -ends_with("0"))
pred = as.numeric(unlist(pred))
roc_value = roc(data$MM_Inv, pred)
write.csv(pred, file = '/Users/silvanacastillo/Documents/UBC/2018TERM2/TOPICSGEOMHIDROL/PROJECT/pred_new.csv')

plot(1:500, model_trial$finalModel$err.rate[1:500], col= "blue", type= "l", lwd =2,
     xlab = "Number of trees", ylab = "Error Rate")


#Logistic Regression

train_dataf$MM_Inv = as.numeric(train_dataf$MM_Inv)
logistic = lm(MM_Inv ~ r_vias + r_pendient + r_relievr + r_geom + r_geologia + 
                 r_fallas + r_drenajes + r_curvatur + r_cobertur, data = train_dataf)
summary(logistic)
logistic = glm(MM_Inv ~ r_vias  + r_geom + r_geologia + r_drenajes + r_cobertur, family = 'binomial', 
               data = train_dataf)
pred_glm = predict(logistic, newdata = test_dataf)
roc_value_glm = roc(test_dataf$MM_Inv, pred_glm)
plot.roc(roc_value_glm, col = "red", lty = 1, legacy.axes = TRUE, print.auc = TRUE)
legend("bottomright", legend = c("GLM Model"), lty = 1, col = ("red"), bty = "n")

data_logistic = dplyr::select(data, -MM_Inv)
data_logistic = data_logistic[4:12]

data_logistic$UNO = 1
data_logistic = dplyr::select(data_logistic, UNO, everything())
coeff= logistic$coefficients  
z= t(t(data_logistic)*coeff) %>%
  rowSums(t(t(data_logistic)*coeff))
prob = exp(z)/(1+exp(z))%>%
  as.data.frame()
write.csv(prob, file = '/Users/silvanacastillo/Documents/UBC/2018TERM2/TOPICSGEOMHIDROL/PROJECT/prob.csv')


error = test_dataf$INVENTARIO.MOVIMIENTOS - pred
mbe = mean(error)
rmse = sqrt(mean(error^2))
mae = mean(abs(error))



