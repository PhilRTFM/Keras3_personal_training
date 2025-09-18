# -- R/supervised_mod_multilabel.R -------------------------------------------

#' Construit un MLP multi-label
#'
#' @param input_dim integer Nombre de variables explicatives
#' @param num_classes integer Nombre de classes (>=2)
#' @param hidden_units integer[] Tailles des couches cachées
#' @param activation character Fonction d'activation des couches cachées
#' @param dropout numeric Taux de dropout
#' @param lr numeric Learning rate
#' @param verbose logical Messages
#'
#' @return keras_model compilé
#' @export
mlp_build_multilabel <- function(input_dim,
                                 num_classes,
                                 hidden_units = c(32L, 16L),
                                 activation = "relu",
                                 dropout = 0,
                                 lr = 1e-3,
                                 verbose = FALSE) {
  inp <- keras3::layer_input(shape = input_dim)
  x <- inp
  for (u in hidden_units) {
    x <- keras3::layer_dense(x, units = u, activation = activation)
    if (dropout > 0) x <- keras3::layer_dropout(x, rate = dropout)
  }
  out <- keras3::layer_dense(x, units = as.integer(num_classes), activation = "sigmoid")
  model <- keras3::keras_model(inputs = inp, outputs = out, name = "mlp_multilabel")
  
  opt <- keras3::optimizer_adam(learning_rate = lr)
  keras3::compile(model,
                  loss = "binary_crossentropy",
                  optimizer = opt,
                  metrics = list("accuracy", keras3::metric_auc(name = "AUC")))
  if (verbose) base::message("[build] MLP multilabel avec ", length(hidden_units), " couches cachées")
  return(model)
}

#' Entraîne un classifieur MLP multi-label
#'
#' @param X_train matrix|df Données d'entraînement (n x p), numériques
#' @param Y_train_bin matrix Matrice binaire (n x K) — labels multi-label
#' @param X_test matrix|df Données de test (m x p)
#' @param Y_test_bin matrix Matrice binaire (m x K)
#' @param build_args list Arguments pour mlp_build_multilabel()
#' @param fit_args list Arguments passés à keras3::fit() (epochs, batch_size, ...)
#' @param verbose logical Messages (FALSE)
#'
#' @return list(history, preds_prob, model, eval_metrics)
#' @export
train_mlp_multilabel_classifier <- function(X_train, Y_train_bin,
                                            X_test, Y_test_bin,
                                            build_args = list(),
                                            fit_args = list(epochs = 50L, batch_size = 16L,
                                                            validation_split = 0.2, verbose = 0),
                                            verbose = FALSE) {
  X_train <- base::as.matrix(X_train); X_test <- base::as.matrix(X_test)
  Y_train_bin <- base::as.matrix(Y_train_bin); Y_test_bin <- base::as.matrix(Y_test_bin)
  input_dim <- base::ncol(X_train); num_classes <- base::ncol(Y_train_bin)
  
  args <- utils::modifyList(list(input_dim = input_dim, num_classes = num_classes), build_args)
  model <- do.call(mlp_build_multilabel, args)
  
  if (verbose) base::message("[fit] epochs=", fit_args$epochs, ", batch=", fit_args$batch_size)
  hist <- do.call(keras3::fit,
                  utils::modifyList(list(object = model, x = X_train, y = Y_train_bin), fit_args))
  
  preds_prob <- model$predict(X_test, verbose = 0)
  eval <- model$evaluate(X_test, Y_test_bin, verbose = 0)
  out <- list(history = hist, preds_prob = preds_prob, model = model, eval_metrics = eval)
  return(out)
}

#' Prédiction binaire multi-label avec seuil
#'
#' @param model keras_model Modèle entraîné
#' @param X matrix|df Données (n x p)
#' @param threshold numeric Seuil d'activation (0.5)
#' @param verbose logical Messages (FALSE)
#'
#' @return matrix Matrice binaire (n x K)
#' @export
mlp_predict_multilabel <- function(model, X, threshold = 0.5, verbose = FALSE) {
  X <- base::as.matrix(X)
  probs <- model$predict(X, verbose = 0)
  if (!base::is.numeric(threshold) || base::length(threshold) != 1L) {
    stop("threshold must be a single numeric value")
  }
  pred_bin <- base::ifelse(probs >= threshold, 1L, 0L)
  storage.mode(pred_bin) <- "integer"
  if (verbose) base::message("[predict] threshold=", threshold)
  return(pred_bin)
}