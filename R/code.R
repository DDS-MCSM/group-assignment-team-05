#' Lee y transforma los datos recopilados sobre las aplicacions de Android
read_and_clean_apk_data <- function() {
  apks <- utils::read.csv("data.csv", stringsAsFactors = F)
  apks$type <- as.factor(apks$type)

  keep_cols <- c("type", "apk_size", "download_size", "n_features",
                 "n_files", "n_permissions", "n_dex", "n_references",
                 "n_resources_packages", "n_defined_packages",
                 "n_referenced_packages", "n_defined_classes",
                 "n_referenced_classes", "n_defined_methods",
                 "n_referenced_methods", "n_defined_fields",
                 "n_referenced_fields", "total_packages_size",
                 "total_classes_size", "total_methods_size", "total_fields_size",
                 "max_depth")
  apks <- apks[keep_cols]
  apks$malware = as.factor(with(apks, type != "BENIGN"))

  return(apks)
}

#' Reorganiza los datos de un data frame aleatoriamente
randomize_data_frame <- function(data_frame) {
  return (data_frame[sample(nrow(data_frame)), ])
}

#' Normaliza los datos de una columna
normalize <- function (x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#' Divide un data frame en dos distintos, uno para datos de entrenamiento
#' y otro para datos de testeo.
#'
#' @param df: El data frame a dividir
#' @param split: Valor entre 0 y 1 que indica el porcentaje de datos que
#' incluir en el data frame de entrenamiento
#'
split_data_frame <- function(df, split) {
  total_rows <- nrow(df)
  training_length <- floor(total_rows * split)

  training_data <- df[1:training_length]
  test_data <- df[training_length + 1:total_rows]

  return (list(training_data, test_data))
}

#' Implementa un metodo de KNN
#'
#' @param data_frame: Data frame con los datos. La primera columna debe
#' ser el resultado y las demas los parametros a considerar
#' @param training_percentage: Valor entre 0 y 1 correspondiente al porcentaje
#' de registros a usar para los datos de entrenamiento
#' @param k: El parametro del test KNN
doKNN <- function(data_frame, training_percentage, k) {

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
