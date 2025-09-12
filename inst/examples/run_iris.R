# -- run_iris.R — Démonstration complète sur iris ----
# Suppose l'existence de R/models.R, R/analysis.R et R/metrics.R
# Tâches :
#  - ACP (PCA) et MDS sur iris brut
#  - Prétraitement + entraînement MLP, matrices de confusion + courbes
#  - Entraînement Autoencodeur, matrices de confusion (par centroïdes latents)
#  - Reconstruction via AE puis ACP & MDS sur X_reconstruit
# Conventions : roxygen, pkg::fun(), verbose en DERNIER (FALSE), return() partout

# --- Chargement des fonctions du projet --------------------------------------
# Chargement des scripts nécessaires pour les modèles, analyses et métriques
base::source("R/models.R",   chdir = TRUE)
base::source("R/analysis.R", chdir = TRUE)
base::source("R/metrics.R",  chdir = TRUE)

# --- Helpers -----------------------------------------------------------
#' Découpage stratifié train/test (indices)
#' @param y (factor) étiquettes
#' @param prop (numeric) proportion train (0<prop<1)
#' @return (list) idx_train, idx_test
# Fonction pour diviser les données en ensembles d'entraînement et de test
stratified_split <- function(y, prop = 0.7) {
  y <- base::as.factor(y)
  classes <- base::levels(y)
  idx_train <- idx_test <- integer(0)
  for (cl in classes) {
    idx <- base::which(y == cl)
    n_tr <- base::max(1L, base::floor(base::length(idx) * prop))
    set <- base::sample(idx, size = n_tr)
    idx_train <- base::c(idx_train, set)
    idx_test  <- base::c(idx_test,  setdiff(idx, set))
  }
  return(list(idx_train = idx_train, idx_test = idx_test))
}

#' Mise à l'échelle standard (centrage-réduction) avec application sur train/test
#' @param x (matrix|data.frame) complet
#' @param idx_train (int) indices train
#' @param idx_test (int) indices test
#' @return (list) x_train, x_test, center, scale
# Fonction pour normaliser les données d'entraînement et de test
standardize_train_test <- function(x, idx_train, idx_test) {
  x <- base::as.matrix(x)
  mu <- base::colMeans(x[idx_train, , drop = FALSE])
  sd <- base::apply(x[idx_train, , drop = FALSE], 2L, stats::sd)
  sd[sd == 0] <- 1
  x_tr <- base::scale(x[idx_train, , drop = FALSE], center = mu, scale = sd)
  x_te <- base::scale(x[idx_test,  , drop = FALSE], center = mu, scale = sd)
  return(list(x_train = base::unclass(x_tr), x_test = base::unclass(x_te),
              center = mu, scale = sd))
}

#' Prédiction par centroïdes dans l'espace latent (AE)
#' @param z_train (matrix) codes latents train (n_train x d)
#' @param y_train (factor) étiquettes train
#' @param z_test (matrix) codes latents test (n_test x d)
#' @param class_levels (character) niveaux ordonnés
#' @return (factor) prédictions pour z_test
# Fonction pour prédire les classes en utilisant les centroïdes dans l'espace latent
predict_by_centroids <- function(z_train, y_train, z_test, class_levels) {
  y_train <- base::as.factor(y_train)
  lvls <- base::levels(y_train)
  cent <- base::sapply(seq_along(lvls), function(i) {
    cls <- lvls[i]
    base::colMeans(z_train[y_train == cls, , drop = FALSE])
  })
  cent <- base::t(cent) # K x d
  # Calcul des distances euclidiennes entre les données test et les centroïdes
  assign <- base::apply(z_test, 1L, function(row) {
    d <- base::rowSums((cent - base::matrix(row, nrow = base::nrow(cent), ncol = base::ncol(cent), byrow = TRUE))^2)
    which.min(d)
  })
  preds <- base::factor(class_levels[assign], levels = class_levels)
  return(preds)
}

# --- Script principal ---------------------------------------------------------
#' Exécute l'ensemble de la démo sur iris
#' @param seed (integer) graine aléatoire
#' @param verbose (logical) messages
#' @return (invisible) list de sorties principales
# Fonction principale pour exécuter la démonstration complète sur le jeu de données iris
run_iris_demo <- function(seed = 123L, verbose = FALSE) {
  base::set.seed(seed)
  if (verbose) base::message("[Info] Chargement iris…")

  # Chargement des données brutes
  X_raw <- base::as.matrix(iris[, 1:4])
  y_fac <- base::factor(iris$Species)
  class_levels <- base::levels(y_fac)

  # -- ACP & MDS sur iris brut -------------------------------------------------
  if (verbose) base::message("[Info] PCA & MDS sur iris brut…")
  # Analyse en composantes principales (ACP) et Scaling multidimensionnel (MDS)
  p_pca_raw <- pca_plot(X_raw, group_factor = y_fac, scale_unit = TRUE,
                        ellipse = TRUE, ellipse_level = 0.95, verbose = verbose)
  p_mds_raw <- mds_plot(
    x = X_raw, method = "euclidean", k = 2,
    labels = base::rownames(iris), group_factor = y_fac,
    title = "Iris — MDS (brut)", verbose = verbose)
  
  base::print(p_pca_raw)
  base::print(p_mds_raw)

  # -- Split + standardisation pour modèles -----------------------------------
  sp <- stratified_split(y_fac, prop = 0.7)
  pp <- standardize_train_test(X_raw, sp$idx_train, sp$idx_test)
  X_tr <- pp$x_train; X_te <- pp$x_test
  y_tr_fac <- y_fac[sp$idx_train]; y_te_fac <- y_fac[sp$idx_test]
  y_tr_int <- base::as.integer(y_tr_fac) - 1L
  y_te_int <- base::as.integer(y_te_fac) - 1L

  # ========================= MLP (supervisé) ==================================
  if (verbose) base::message("[Info] Entraînement MLP…")
  # Construction et entraînement du modèle MLP (Perceptron multicouche)
  mlp <- mlp_build(input_dim = base::ncol(X_tr), hidden_units = c(32L, 16L),
                   activation = "relu", num_classes = base::length(class_levels),
                   lr = 1e-3, verbose = verbose)
  hist_mlp <- keras3::fit(
    object = mlp,
    x = X_tr, y = y_tr_int,
    validation_split = 0.2,
    epochs = 100L, batch_size = 16L,
    verbose = if (verbose) 1L else 0L
  )
  p_hist_mlp <- history_curves_plot(history = hist_mlp,
                                    title = "Training curves — MLP",
                                    metrics = c("loss", "accuracy"),
                                    smooth = FALSE, verbose = verbose)
  base::print(p_hist_mlp)

  # Prédictions et matrice de confusion pour le MLP
  y_pred_mlp <- mlp_predict_labels(mlp, X_te, class_levels, verbose = verbose)
  p_cm_mlp <- viz_confusion_panel_from_predictions(
    y_true = y_te_fac, y_pred = y_pred_mlp, labels = class_levels,
    title = "Iris — Confusion matrix (MLP, test)", digits = 2L,
    show_bottomright = FALSE, verbose = verbose
  )
  base::print(p_cm_mlp)

  # ====================== Autoencodeur (non-supervisé) ========================
  if (verbose) base::message("[Info] Entraînement Autoencodeur…")
  # Construction et entraînement de l'autoencodeur
  ae <- ae_build(input_dim = base::ncol(X_tr), latent_dim = 2L,
                 units = c(16L, 8L), activation = "relu",
                 latent_activation = "linear", dropout = 0,
                 batchnorm = FALSE, l2 = 0, lr = 1e-3, verbose = verbose)

  hist_ae <- keras3::fit(
    object = ae$autoencoder,
    x = X_tr, y = X_tr,
    validation_split = 0.2,
    epochs = 150L, batch_size = 16L,
    verbose = if (verbose) 1L else 0L
  )
  p_hist_ae <- history_curves_plot(history = hist_ae,
                                   title = "Training curves — Autoencoder",
                                   metrics = c("loss"),
                                   smooth = FALSE, verbose = verbose)
  base::print(p_hist_ae)

  # --- Codes latents & prédiction par centroïdes ------------------------------
  # Extraction des codes latents pour les ensembles d'entraînement et de test
  z_tr <- base::as.matrix(base::`dim<-`(base::as.array(ae$encoder(X_tr)),
                                        c(base::nrow(X_tr), 2L)))
  z_te <- base::as.matrix(base::`dim<-`(base::as.array(ae$encoder(X_te)),
                                        c(base::nrow(X_te), 2L)))

  # Prédictions basées sur les centroïdes dans l'espace latent
  y_pred_ae <- predict_by_centroids(z_train = z_tr, y_train = y_tr_fac,
                                    z_test = z_te, class_levels = class_levels)

  p_cm_ae <- viz_confusion_panel_from_predictions(
    y_true = y_te_fac, y_pred = y_pred_ae, labels = class_levels,
    title = "Iris — Confusion matrix (AE latent-centroids, test)", digits = 2L,
    show_bottomright = FALSE, verbose = verbose
  )
  base::print(p_cm_ae)

  # --- Reconstruction & analyses (PCA/MDS) ------------------------------------
  # Reconstruction des données originales via l'autoencodeur
  X_recon_all <- base::as.matrix(base::`dim<-`(base::as.array(ae$autoencoder(X_raw)),
                                               c(base::nrow(X_raw), base::ncol(X_raw))))

  # Analyse en composantes principales (ACP) et Scaling multidimensionnel (MDS) sur les données reconstruites
  p_pca_recon <- pca_plot(X_recon_all, group_factor = y_fac, scale_unit = TRUE,
                          ellipse = TRUE, ellipse_level = 0.95, verbose = verbose)
  p_mds_recon <- mds_plot(
    x = X_recon_all, method = "euclidean", k = 2,
    labels = base::rownames(iris), group_factor = y_fac,
    title = "Iris — MDS (reconstruit par AE)", verbose = verbose
  )
  
  base::print(p_pca_recon)
  base::print(p_mds_recon)

  # Afficher quelques statistiques sur la reconstruction
  cat("Premières lignes des données originales :\n")
  print(head(X_raw))
  cat("\nPremières lignes des données reconstruites :\n")
  print(head(X_recon_all))
  cat("\nErreur moyenne de reconstruction :", mean((X_raw - X_recon_all)^2))

  if (verbose) base::message("[OK] Démo terminée.")
  return(invisible(list(
    pca_raw = p_pca_raw, mds_raw = p_mds_raw,
    history_mlp = hist_mlp, cm_mlp = p_cm_mlp,
    history_ae = hist_ae, cm_ae = p_cm_ae,
    pca_recon = p_pca_recon, mds_recon = p_mds_recon
  )))
}

# --- Exécution directe si appelé comme script --------------------------------
# Exécute la démonstration si le script est appelé directement
if (sys.nframe() == 0L) {
  run_iris_demo(seed = 123L, verbose = TRUE)
}
