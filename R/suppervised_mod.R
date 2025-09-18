# -- R/suppervised_mod.R — Approche supervisée : MLP --------------------------

#' Construction d'un MLP multi-classes
#'
#' @param input_dim integer Nombre de variables explicatives (>0)
#' @param hidden_units integer[] Tailles des couches cachées (ex: c(16, 8))
#' @param activation character Fonction d'activation des couches cachées
#' @param num_classes integer Nombre de classes cibles (>=2)
#' @param lr numeric Taux d'apprentissage (optimiseur Adam)
#' @param verbose logical Affiche des infos (FALSE)
#'
#' @return keras3::keras_model Modèle compilé
#' @examples
#' model <- mlp_build(input_dim = 4, hidden_units = c(16, 8),
#'                    activation = "relu", num_classes = 3)
#' @export
mlp_build <- function(input_dim,
                      hidden_units = c(16L, 8L),
                      activation = "relu",
                      num_classes,
                      lr = 1e-3,
                      verbose = FALSE) {
  input <- keras3::layer_input(shape = input_dim)
  x <- input
  for (u in hidden_units) {
    x <- keras3::layer_dense(x, units = u, activation = activation)
  }
  output <- keras3::layer_dense(x, units = num_classes, activation = "softmax")
  model <- keras3::keras_model(inputs = input, outputs = output)
  
  keras3::compile(model,
                  optimizer = keras3::optimizer_adam(learning_rate = lr),
                  loss = "sparse_categorical_crossentropy",
                  metrics = "accuracy")
  if (verbose) base::message("MLP construit avec ", base::length(hidden_units), " couches cachées")
  return(model)
}

#' Prédictions discrètes (étiquettes) d'un modèle MLP
#'
#' @param model keras3::keras_model entraîné
#' @param x matrix Données en entrée
#' @param class_levels character[] Niveaux de classes
#' @param verbose logical Affiche des infos
#'
#' @return factor Prédictions discrètes
#' @export
mlp_predict_labels <- function(model,
                               x,
                               class_levels,
                               verbose = FALSE) {
  # ✅ appel direct à la méthode $predict
  probs <- model$predict(x, verbose = 0)
  pred_idx <- base::max.col(probs)  # index max par ligne
  preds <- base::factor(class_levels[pred_idx], levels = class_levels)
  if (verbose) base::message("Prédictions générées pour ", base::nrow(x), " instances")
  return(preds)
}


#' Entraînement complet d'un MLP (classification)
#'
#' @param X_tr matrix Données train
#' @param y_tr_int integer[] Labels train (indices 0:(k-1))
#' @param X_te matrix Données test
#' @param y_te_fac factor Labels test
#' @param class_levels character[] Niveaux de classes
#' @param hidden_units integer[] Taille des couches cachées
#' @param activation character Fonction d'activation
#' @param lr numeric Learning rate
#' @param epochs integer Nombre d'époques
#' @param batch_size integer Taille de batch
#' @param verbose logical
#'
#' @return list avec : history (keras_training_history),
#'   preds (factor), confusion_plot (patchwork)
#' @examples
#' \dontrun{
#' split <- preprocess_data(iris, "Species")
#' res <- train_mlp_classifier(split$X_train, split$y_train_int,
#'                             split$X_test, split$y_test_fac,
#'                             split$class_levels)
#' }
#' @export
train_mlp_classifier <- function(X_tr,
                                 y_tr_int,
                                 X_te,
                                 y_te_fac,
                                 class_levels,
                                 hidden_units = c(16L, 8L),
                                 activation = "relu",
                                 lr = 1e-3,
                                 epochs = 50L,
                                 batch_size = 16L,
                                 verbose = FALSE) {
  model <- mlp_build(input_dim = base::ncol(X_tr),
                     hidden_units = hidden_units,
                     activation = activation,
                     num_classes = base::length(class_levels),
                     lr = lr,
                     verbose = verbose)
  
  history <- keras3::fit(model,
                         x = X_tr,
                         y = y_tr_int,
                         epochs = epochs,
                         batch_size = batch_size,
                         validation_split = 0.2,
                         verbose = ifelse(verbose, 1, 0))
  
  preds <- mlp_predict_labels(model, X_te, class_levels = class_levels, verbose = verbose)
  
  # matrice de confusion
  conf_plot <- viz_confusion_panel_from_predictions(
    y_true = y_te_fac,
    y_pred = preds,
    labels = class_levels,
    title = "Confusion Matrix (MLP)",
    verbose = verbose
  )
  
  return(list(history = history,
              preds = preds,
              confusion_plot = conf_plot,
              model = model))
}
