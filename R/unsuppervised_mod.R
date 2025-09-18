# -- R/unsuppervised_mod.R — Approche non supervisée : Autoencodeur -----------

#' Construction d'un Autoencodeur
#'
#' @param input_dim integer Nombre de variables explicatives (>0)
#' @param latent_dim integer Taille de l'espace latent (par défaut 2)
#' @param units integer[] Tailles des couches cachées de l'encodeur
#' @param activation character Fonction d'activation des couches cachées
#' @param latent_activation character Fonction d'activation de la couche latente
#' @param dropout numeric taux de dropout (0 = aucun)
#' @param batchnorm logical Appliquer une normalisation de batch
#' @param l2 numeric coefficient de régularisation L2
#' @param lr numeric learning rate (Adam)
#' @param verbose logical Affiche des infos
#'
#' @return list(autoencoder, encoder, decoder) modèles keras3
#' @examples
#' ae <- ae_build(input_dim = 4, latent_dim = 2)
#' @export
ae_build <- function(input_dim,
                     latent_dim = 2L,
                     units = c(16L, 8L),
                     activation = "relu",
                     latent_activation = "linear",
                     dropout = 0,
                     batchnorm = FALSE,
                     l2 = 0,
                     lr = 1e-3,
                     verbose = FALSE) {
  input <- keras3::layer_input(shape = input_dim)
  x <- input
  for (u in units) {
    x <- keras3::layer_dense(x, units = u, activation = activation,
                             kernel_regularizer = if (l2 > 0) keras3::regularizer_l2(l2) else NULL)
    if (batchnorm) x <- keras3::layer_batch_normalization(x)
    if (dropout > 0) x <- keras3::layer_dropout(x, rate = dropout)
  }
  latent <- keras3::layer_dense(x, units = latent_dim, activation = latent_activation)
  x <- latent
  for (u in rev(units)) {
    x <- keras3::layer_dense(x, units = u, activation = activation)
  }
  output <- keras3::layer_dense(x, units = input_dim, activation = "linear")
  
  autoencoder <- keras3::keras_model(inputs = input, outputs = output)
  encoder <- keras3::keras_model(inputs = input, outputs = latent)
  
  # decoder : relie latent -> sortie
  latent_input <- keras3::layer_input(shape = latent_dim)
  x_dec <- latent_input
  for (u in rev(units)) {
    x_dec <- keras3::layer_dense(x_dec, units = u, activation = activation)
  }
  dec_output <- keras3::layer_dense(x_dec, units = input_dim, activation = "linear")
  decoder <- keras3::keras_model(inputs = latent_input, outputs = dec_output)
  
  keras3::compile(autoencoder,
                  optimizer = keras3::optimizer_adam(learning_rate = lr),
                  loss = "mse")
  
  if (verbose) base::message("Autoencodeur construit (", input_dim, " -> ", latent_dim, ")")
  return(list(autoencoder = autoencoder, encoder = encoder, decoder = decoder))
}

#' Entraîne un autoencodeur simple + évalue par centroïdes latents
#'
#' @param X_tr matrix Données train
#' @param X_te matrix Données test
#' @param y_tr_fac factor Labels train
#' @param y_te_fac factor Labels test
#' @param class_levels character[] Niveaux de classes
#' @param encoding_dim integer Taille couche latente
#' @param epochs integer Nb d’époques
#' @param batch_size integer Taille batch
#' @param verbose logical
#'
#' @return list Résultats entraînement + plots + erreurs reconstruction
#' @export
train_autoencoder <- function(X_tr, X_te,
                              y_tr_fac, y_te_fac,
                              class_levels,
                              encoding_dim = 2L,
                              epochs = 50L,
                              batch_size = 16L,
                              verbose = FALSE) {
  input_dim <- ncol(X_tr)
  
  # --- Définition modèle AE ---
  input <- keras3::layer_input(shape = input_dim)
  encoded <- keras3::layer_dense(input, units = encoding_dim, activation = "relu")
  decoded <- keras3::layer_dense(encoded, units = input_dim, activation = "linear")
  
  autoencoder <- keras3::keras_model(input, decoded)
  encoder <- keras3::keras_model(input, encoded)
  
  encoded_input <- keras3::layer_input(shape = encoding_dim)
  decoder_layer <- autoencoder$layers[[length(autoencoder$layers)]]
  decoder <- keras3::keras_model(encoded_input, decoder_layer(encoded_input))
  
  autoencoder$compile(
    loss = "mse",
    optimizer = keras3::optimizer_adam(learning_rate = 1e-3)
  )
  
  # --- Entraînement ---
  history <- autoencoder$fit(
    X_tr, X_tr,
    epochs = epochs,
    batch_size = batch_size,
    shuffle = TRUE,
    validation_data = list(X_te, X_te),
    verbose = if (verbose) 1 else 0
  )
  
  # --- Reconstruction & erreurs ---
  X_recon <- autoencoder$predict(X_te, verbose = 0)
  recon_errors <- list(
    mse  = mean((X_te - X_recon)^2),
    rmse = sqrt(mean((X_te - X_recon)^2)),
    mae  = mean(abs(X_te - X_recon))
  )
  # erreurs par individu
  errors_indiv <- rowMeans((X_te - X_recon)^2)
  
  # --- Latent space ---
  z_tr <- encoder$predict(X_tr, verbose = 0)
  z_te <- encoder$predict(X_te, verbose = 0)
  
  # Prédictions par centroïdes
  preds <- predict_by_centroids(z_train = z_tr,
                                y_train = y_tr_fac,
                                z_test = z_te,
                                class_levels = class_levels,
                                verbose = verbose)
  
  conf_plot <- viz_confusion_panel_from_predictions(y_te_fac, preds,
                                                    labels = class_levels,
                                                    title = "Confusion AE latent")
  
  # --- PCA & MDS sur reconstructions ---
  pca_recon <- pca_plot(X_recon, group_factor = y_te_fac, verbose = verbose)
  mds_recon <- mds_plot(X_recon, group_factor = y_te_fac, verbose = verbose)
  
  if (verbose) {
    message("[AE] Reconstruction : MSE=", round(recon_errors$mse, 4),
            " | RMSE=", round(recon_errors$rmse, 4),
            " | MAE=", round(recon_errors$mae, 4))
  }
  
  return(list(
    history       = history,
    preds         = preds,
    confusion_plot = conf_plot,
    recon_errors  = recon_errors,
    errors_indiv  = errors_indiv,   # ✅ ajout
    X_recon       = X_recon,        # ✅ ajout
    latent_coords = list(train = z_tr, test = z_te),
    pca_plot      = pca_recon,
    mds_plot      = mds_recon,
    autoencoder   = autoencoder,
    encoder       = encoder,
    decoder       = decoder
  ))
}
