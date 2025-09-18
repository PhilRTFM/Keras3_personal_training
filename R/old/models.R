# -- R/models.R — Modèles supervisé (MLP) et non-supervisé (Autoencodeur) ----
# Conventions : tout namespacé, pas de library()/install.*, verbose en DERNIER (par défaut FALSE)

# -- Supervisé : Modèle MLP (classification multi-classes) ---------------------

#' Construit et compile un MLP pour classification multi-classes
#'
#' @param input_dim integer Nombre de variables explicatives (> 0)
#' @param hidden_units integer Vecteur de tailles des couches cachées, ex. c(16, 8)
#' @param activation character Fonction d'activation des couches cachées (ex. "relu")
#' @param num_classes integer Nombre de classes en sortie (>= 2) — sortie softmax
#' @param lr numeric Taux d'apprentissage (learning rate) de l'optimiseur Adam (par défaut 1e-3)
#' @param verbose logical Affiche des messages (FALSE)
#' @return keras3::keras_model Modèle compilé, prêt pour keras3::fit()
#' @details
#' Entrées pour l'entraînement :
#'  - x : matrice/tableau (n_obs x input_dim) numérique
#'  - y : entiers dans [0 .. num_classes-1] (loss = sparse_categorical_crossentropy)


mlp_build <- function(input_dim,
                      hidden_units = c(16L, 8L),
                      activation = "relu",
                      num_classes = 3L,
                      lr = 1e-3,
                      verbose = FALSE) {
  # Vérification des paramètres d'entrée
  stopifnot(is.numeric(input_dim), input_dim >= 1, length(hidden_units) >= 1, num_classes >= 2)

  if (isTRUE(verbose)) base::message("[mlp_build] input_dim=", input_dim,
                                     ", layers=", base::paste(hidden_units, collapse = ","),
                                     ", classes=", num_classes)

  # Définition des couches du modèle
  inp <- keras3::layer_input(shape = list(base::as.integer(input_dim)))
  x <- inp
  # Première couche dense
  x <- keras3::layer_dense(x, units = base::as.integer(hidden_units[1]), activation = activation)
  # Ajout des couches cachées supplémentaires si spécifiées
  if (length(hidden_units) > 1) {
    for (u in hidden_units[-1]) {
      x <- keras3::layer_dense(x, units = base::as.integer(u), activation = activation)
    }
  }
  # Couche de sortie avec activation softmax
  out <- keras3::layer_dense(x, units = base::as.integer(num_classes), activation = "softmax")

  # Compilation du modèle
  model <- keras3::keras_model(inp, out)
  keras3::compile(
    object = model,
    optimizer = keras3::optimizer_adam(learning_rate = lr),
    loss = "sparse_categorical_crossentropy",
    metrics = list("sparse_categorical_accuracy")
  )
  return(model)
}

#' Prédit les étiquettes (factor) à partir d'un modèle MLP
#'
#' @param model keras3::keras_model Modèle MLP entraîné
#' @param x matrix|array Données de prédiction (n_obs x input_dim)
#' @param class_levels character Niveaux de classes (longueur K) ordre du softmax
#' @param verbose logical Messages (FALSE)
#' @return factor Vecteur factor des prédictions, niveaux = class_levels
mlp_predict_labels <- function(model, x, class_levels, verbose = FALSE) {
  # Vérification des paramètres d'entrée
  stopifnot(!is.null(model), !is.null(x), !is.null(class_levels))

  # 1) Exécuter le modèle pour obtenir les probabilités
  y <- model(x, training = FALSE)

  # 2) Convertir proprement en matrice R
  if ("reticulate.python.builtin.object" %in% class(y) || "py_object" %in% class(y)) {
    y <- reticulate::py_to_r(y)
  }
  if (inherits(y, "tensorflow.tensor")) {
    y <- base::as.array(y)
  }
  if (!base::is.matrix(y)) {
    y <- tryCatch(base::as.matrix(y),
                  error = function(e) base::as.matrix(base::as.array(y)))
  }

  # 3) Trouver les indices des classes prédominantes (argmax)
  stopifnot(ncol(y) == length(class_levels))
  idx   <- base::max.col(y, ties.method = "first")
  preds <- base::factor(class_levels[idx], levels = class_levels)

  if (isTRUE(verbose)) {
    base::message(sprintf("[mlp_predict_labels] shape probs = %d x %d", nrow(y), ncol(y)))
  }
  return(preds)
}



# -- Non-supervisé : Autoencodeur (reconstruction X -> X') ---------------------

#' Construit un autoencodeur symétrique et expose encoder/decoder
#'
#' @param input_dim integer Dimension d'entrée (> 0)
#' @param latent_dim integer Taille de l'espace latent (>= 1)
#' @param units integer Vecteur de tailles des couches cachées de l'encodeur
#' @param activation character Fonction d'activation des couches cachées (ex. "relu")
#' @param latent_activation character Fonction d'activation de la couche latente (souvent "linear")
#' @param dropout numeric Taux de dropout (0 = inactif)
#' @param batchnorm logical Batch-normalization après chaque couche cachée
#' @param l2 numeric Coefficient de pénalisation L2 (0 = inactif)
#' @param lr numeric Taux d'apprentissage Adam
#' @param verbose logical Messages (TRUE)
#' @return list Liste : autoencoder (keras_model), encoder (keras_model), decoder (keras_model)
#' @details
#' Modèle reconstruit X -> X' avec loss = MSE et métrique MAE. Décodeur symétrique.

ae_build <- function(input_dim,
                     latent_dim = 2L,
                     units = c(16L, 8L),
                     activation = "relu",
                     latent_activation = "linear",
                     dropout = 0,
                     batchnorm = FALSE,
                     l2 = 0,
                     lr = 1e-3,
                     verbose = TRUE) {
  # Vérification des paramètres d'entrée
  stopifnot(input_dim >= 1, latent_dim >= 1, length(units) >= 1)
  if (isTRUE(verbose)) base::message("[ae_build] input_dim=", input_dim,
                                     ", latent_dim=", latent_dim,
                                     ", units=", base::paste(units, collapse = ","))

  # Définir la régularisation L2 si spécifiée
  reg <- if (l2 > 0) keras3::regularizer_l2(l2) else NULL

  # --- Encodeur ---
  inp <- keras3::layer_input(shape = list(base::as.integer(input_dim)))
  x <- inp
  for (u in units) {
    x <- keras3::layer_dense(x, units = base::as.integer(u), activation = activation, kernel_regularizer = reg)
    if (isTRUE(batchnorm)) x <- keras3::layer_batch_normalization(x)
    if (dropout > 0) x <- keras3::layer_dropout(x, rate = dropout)
  }
  z <- keras3::layer_dense(x, units = base::as.integer(latent_dim), activation = latent_activation, name = "latent")

  # --- Décodeur (symétrique) ---
  y <- z
  for (u in rev(units)) {
    y <- keras3::layer_dense(y, units = base::as.integer(u), activation = activation, kernel_regularizer = reg)
    if (isTRUE(batchnorm)) y <- keras3::layer_batch_normalization(y)
    if (dropout > 0) y <- keras3::layer_dropout(y, rate = dropout)
  }
  out <- keras3::layer_dense(y, units = base::as.integer(input_dim), activation = "linear", name = "recon")

  # Création des modèles autoencodeur, encodeur et décodeur
  ae  <- keras3::keras_model(inp, out, name = "autoencoder")
  enc <- keras3::keras_model(inp, z,   name = "encoder")

  # --- Décodeur autonome (Z -> X') ---
  latent_inp <- keras3::layer_input(shape = list(base::as.integer(latent_dim)))
  yd <- latent_inp
  for (u in rev(units)) {
    yd <- keras3::layer_dense(yd, units = base::as.integer(u), activation = activation, kernel_regularizer = reg)
  }
  dec_out <- keras3::layer_dense(yd, units = base::as.integer(input_dim), activation = "linear")
  dec <- keras3::keras_model(latent_inp, dec_out, name = "decoder")

  # Compilation de l'autoencodeur
  keras3::compile(
    object = ae,
    optimizer = keras3::optimizer_adam(learning_rate = lr),
    loss = "mse",
    metrics = list("mae")
  )

  res <- base::list(autoencoder = ae, encoder = enc, decoder = dec)
  return(res)
}
