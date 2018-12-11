
normalize <- function (x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Limpia y prepara los datos para el test de KNN
# Devuelve dos data frames de data y test respectivamente
prepareKNNTestData <- function(df) {
  df$malware = with(df, type != "BENIGN")
  df <- df[-c(1,2,7,8,9,10,11,13,14,19,20,23,24,27,28,31,32)]
  
  totalRows <- nrow(df)
  totalCols <- ncol(df)
  df <- df[c(totalCols, 1:totalCols - 1)]
  df_n <- as.data.frame(lapply(df[2:totalCols], normalize))
  
  print(head(df_n))

  split <- floor(totalRows * 0.65)
  trainingData <- df[1:split,]
  testData <- df[split:totalRows,]
  
  labels <- as.data.frame(trainingData)
  labels <- as.data.frame(labels)[,1, drop = TRUE]
  
  retValue <- list(as.data.frame(trainingData), as.data.frame(testData), labels)
  return(retValue)
}

ret <- prepareKNNTestData(df)
test_pred <- class::knn(train = as.data.frame(ret[1]), test = as.data.frame(ret[2]), cl = as.data.frame(ret[3])[,1, drop = TRUE], k = 10)
test_actual <- as.data.frame(ret[2])$malware
test_actual <- as.factor(test_actual)
