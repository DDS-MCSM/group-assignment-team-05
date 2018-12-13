#' Lee y transforma los datos recopilados sobre las aplicacions de Android
read_and_clean_apk_data <- function() {
  apks <- utils::read.csv("data.csv")

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

  return(apks)
}

#' Modifica la columna 'type' del data_frame por una columna de 'malware'
get_data_frame_with_malware <- function(data_frame) {
  new_data_frame <- dplyr::rename(data_frame, malware = "type")
  new_data_frame$malware <- with(new_data_frame, malware != "BENIGN")
  return (new_data_frame)
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

  training_data <- df[1:training_length,]
  test_data <- df[(training_length + 1):total_rows,]

  return (list(training_data, test_data))
}

#' Implementa un metodo de KNN
#'
#' @param data_frame: Data frame con los datos. La primera columna debe
#' ser el resultado y las demas los parametros a considerar
#' @param training_percentage: Valor entre 0 y 1 correspondiente al porcentaje
#' de registros a usar para los datos de entrenamiento
#' @param k: El parametro del test KNN
doKNN <- function(data_frame, training_percentage, ks) {
  res <- split_data_frame(data_frame, training_percentage)
  labels <- res[[1]][,1]

  training_data <- as.data.frame(lapply(
    res[[1]], normalize
  ))[-1]
  test_data <- as.data.frame(lapply(
    res[[2]], normalize
  ))[-1]

  test_expected <- class::knn(train = training_data,
                              test = test_data,
                              cl = labels,
                              k = ks)
  test_actual <- as.factor(res[[2]]$malware)

  return (list(test_expected, test_actual))
}

doSVM <- function(data_frame, training_percentage) {
  res <- split_data_frame(data_frame, training_percentage)

  training_data <- res[[1]][-1]
  test_data <- res[[2]][-1]
  labels <- res[[1]][,1]

  model <- e1071::svm(x = training_data,
                      y = labels,
                      cost = 10,
                      scale = FALSE)
}

#' Comprueba el nivel de acierto de nuestros tests
check_results <- function(expected, actual) {
  ok <- 0
  for (i in 1:length(expected)) {
    if (expected[i] == actual[i]) {
      ok <- ok + 1
    }
  }

  return (ok / length(expected))
}

#' Devuelve una tabla con los valores del parametro k y su porcentaje de
#' acierto
get_k_value_accuracy_table <- function(data_frame) {
  accuracy_data_frame <- data.frame(0, 0)
  names(accuracy_data_frame) <- c("k", "accuracy")

  for (k in 1:20) {
    res <- doKNN(data_frame, 0.65, k)
    accuracy <- check_results(res[[1]], res[[2]])

    accuracy_data_frame <- rbind(accuracy_data_frame, list(k, accuracy))
  }

  return (accuracy_data_frame)
}


#' Realiza tests KNN con diversos valores de k y devuelve una lista con el valor
#' de k mas optimo y su porcentaje de acierto.
get_best_k_value <- function(data_frame) {
  best_k <- 1
  best_accuracy <- 0

  for (k in 1:20) {
    res <- doKNN(data_frame, 0.65, k)
    accuracy <- check_results(res[[1]], res[[2]])

    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_k <- k
    }
  }

  return (list(best_k, best_accuracy))
}



