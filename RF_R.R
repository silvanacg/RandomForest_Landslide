library(dplyr)
library(randomForest)
library(caret)
library(pROC)
library(tibble)


path = "/Users/silvanacastillo/Downloads/Datos_SP.csv"
data = read.csv(path)
data$INVENTARIO.MOVIMIENTOS = as.factor(data$INVENTARIO.MOVIMIENTOS)
data = dplyr::select(data, -ends_with("PIXELES"))
sum(data$INVENTARIO.MOVIMIENTOS == 1)

landslidespx = subset(data, data$INVENTARIO.MOVIMIENTOS == 1)
nonlandslidespx = subset(data, data$INVENTARIO.MOVIMIENTOS == 0)

set.seed(300)
landslide_sample = sample_n(landslidespx, 8400)
nonlandslide_sample = sample_n(nonlandslidespx, 8400)
train_dataf = rbind(landslide_sample,nonlandslide_sample)
train_dataf = train_dataf[sample(nrow(train_dataf)), ]

landslidetst_sample = sample_n(landslidespx, 3604)
nonlandslidetst_sample = sample_n(nonlandslidespx, 3604)
test_dataf = rbind(landslidetst_sample,nonlandslidetst_sample)
test_dataf = test_dataf[sample(nrow(test_dataf)), ]

correlationMatrix <- cor(train_dataf[,1:9])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

train_dataf = dplyr::select(train_dataf, -ends_with("VR"))
test_dataf = dplyr::select(test_dataf, -ends_with("VR"))

#model
model_trial <- train(INVENTARIO.MOVIMIENTOS ~ R_VIAS + R_PENDIENTE + R_GEOM + R_GEOLOGIA + 
                       R_FALLAS + R_DRENAJES + R_CURVATURA + R_COBERTUR,  data = train_dataf, 
                     method = 'rf')
pred_= predict.train(model_trial, newdata = test_dataf)


#factors table

variable_dataset = c("Geomorphology", "Geology", "Slope", "Relief", "Land cover","Distance to drainages", 
                     "Curvature", "Distance to faults", " Distance to roads")
scales = c( "1:25.000","1:10.000","", "", "1:10.000", "", "","1:10.000","1:10.000")
source= c("Dem, Field survey", "Geological maps, Field survey", "DEM", "DEM", "CORANTIOQUIA, Field survey",
          "Topographic maps, DEM","DEM", "Geological maps, Field survey", "Topographic maps" )
range = c("","","<5° - >45°","0 - 52.23m","","10 - >30m","-648.94 - 674","10 - >50m","10 - >40m")

data.factors = data.frame(variable_dataset, scales, source,range)
colnames(data.factors) = c("Factor", "Scale", "Source", "Range")

#plot error
plot(1:500, model_trial$finalModel$err.rate[1:500], col= "blue", type= "l", lwd =2,
     xlab = "Number of trees", ylab = "Error Rate")

#table importance

variable = c("Geomorphology", "Geology", "Slope", "Land cover","Distance to drainages", 
             "Curvature", "Distance to faults", " Distance to roads")
importance = c(1566.9, 1066.5, 938.5, 885.3, 869.3, 763.7, 653.7, 606.4)
weight = c( importance[1]/sum(importance), importance[2]/sum(importance), importance[3]/sum(importance),
            importance[4]/sum(importance), importance[5]/sum(importance), importance[6]/sum(importance), 
            importance[7]/sum(importance), importance[8]/sum(importance))
importance.df = data.frame(variable, importance, weight)
colnames(importance.df) = c("Factor", "Importance", "Weight")

# Confussion matrix

confussion = data.frame(model_trial$finalModel$confusion) %>%
  dplyr::select(X0, X1)
colnames(confussion) = c("0","1")


#LSI Values

#LSI =  data$R_VIAS * weight[8] + data$R_PENDIENTE * weight [3] + data$R_GEOM * weight [1] + 
  data$R_GEOLOGIA * weight [2] + data$R_FALLAS * + weight [7] + data$R_DRENAJES * weight [5] + 
  data$R_CURVATURA * weight [6] + data$R_COBERTUR * weight [4]


#LSI = data.frame(LSI)
#write.csv(LSI, file = "/Users/silvanacastillo/Documents/UBC/2018TERM2/TOPICSGEOMHIDROL/PROJECT/LSI.csv")

pred_ = as.numeric(pred_)
roc_value = roc(test_dataf$INVENTARIO.MOVIMIENTOS, pred_)
roc_value$specificities
roc_value$sensitivities

plot.roc(roc_value, col = "red", lty = 1, legacy.axes = TRUE, print.auc = TRUE)
legend("bottomright", legend = c("RF Model"), lty = 1, col = ("red"), bty = "n")

#Probability

data = rownames_to_column(data,var = "Value")
votes = (model_trial$finalModel$votes[16801:33600])
traindata_results = cbind(train_dataf, votes) %>%
 rownames_to_column(var = "Value") %>%
  dplyr::select(starts_with("v"))

probability = full_join(data, traindata_results) %>%
 dplyr::select("votes")


probability = data.frame(probability)
probability[is.na(probability)] = 10
write.csv(probability, file = "/Users/silvanacastillo/Documents/UBC/2018TERM2/TOPICSGEOMHIDROL/PROJECT/probability_10.csv")





