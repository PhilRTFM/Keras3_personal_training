# --- Refactored iris demo pipeline -------------------------------------------

# Source project modules
base::source("R/models.R",   chdir = TRUE)
base::source("R/analysis.R", chdir = TRUE)
base::source("R/metrics.R",  chdir = TRUE)
base::source("R/utils.R",    chdir = TRUE)  # contient preprocess_data(), save_outputs(), etc.

# === Data Prep ===
prepare_iris_data <- function(target = "Species", prop = 0.7, seed = 123L, verbose = FALSE) {
  if (!base::exists("preprocess_data")) {
    base::stop("La fonction 'preprocess_data()' doit être définie dans utils.R")
  }
  dat <- datasets::iris
  preprocess_data(data = dat, target = target, prop = prop, seed = seed, verbose = verbose)
}

# === PCA and MDS Visualization ===
visualize_raw_data <- function(X_raw, y_fac, verbose = FALSE) {
  p_pca <- pca_plot(
    x = X_raw, group_factor = y_fac, scale_unit = TRUE,
    ellipse = TRUE, ellipse_level = 0.95, verbose = verbose
  )
  
  p_mds <- mds_plot(
    x = X_raw, method = "euclidean", k = 2,
    labels = base::rownames(X_raw), group_factor = y_fac,
    title = "iris — MDS (brut)", verbose = verbose
  )
  
  base::list(pca = p_pca, mds = p_mds)
}

# === Train MLP ===
train_mlp_classifier <- function(X_tr, y_tr_int, X_te, y_te_fac, class_levels, verbose = FALSE) {
  mlp <- mlp_build(
    input_dim = base::ncol(X_tr), hidden_units = c(32L, 16L),
    activation = "relu", num_classes = base::length(class_levels),
    lr = 1e-3, verbose = verbose
  )
  
  hist <- keras3::fit(
    object = mlp, x = X_tr, y = y_tr_int,
    validation_split = 0.2, epochs = 100L, batch_size = 16L,
    verbose = if (verbose) 1L else 0L
  )
  
  y_pred <- mlp_predict_labels(mlp, X_te, class_levels, verbose = verbose)
  
  p_cm <- viz_confusion_panel_from_predictions(
    y_true = y_te_fac, y_pred = y_pred, labels = class_levels,
    title = "iris — Confusion matrix (MLP, test)", digits = 2L,
    show_bottomright = FALSE, verbose = verbose
  )
  
  base::list(history = hist, confusion_plot = p_cm)
}

# === Train Autoencoder ===
train_autoencoder <- function(X_tr, X_te, y_tr_fac, y_te_fac, class_levels, X_raw, y_fac, verbose = FALSE) {
  ae <- ae_build(
    input_dim = base::ncol(X_tr), latent_dim = 2L,
    units = c(16L, 8L), activation = "relu",
    latent_activation = "linear", dropout = 0,
    batchnorm = FALSE, l2 = 0, lr = 1e-3, verbose = verbose
  )
  
  hist <- keras3::fit(
    object = ae$autoencoder, x = X_tr, y = X_tr,
    validation_split = 0.2, epochs = 150L, batch_size = 16L,
    verbose = if (verbose) 1L else 0L
  )
  
  z_tr <- base::as.matrix(base::`dim<-`(base::as.array(ae$encoder(X_tr)), c(base::nrow(X_tr), 2L)))
  z_te <- base::as.matrix(base::`dim<-`(base::as.array(ae$encoder(X_te)), c(base::nrow(X_te), 2L)))
  
  y_pred <- predict_by_centroids(
    z_train = z_tr, y_train = y_tr_fac,
    z_test = z_te, class_levels = class_levels
  )
  
  p_cm <- viz_confusion_panel_from_predictions(
    y_true = y_te_fac, y_pred = y_pred, labels = class_levels,
    title = "iris — Confusion matrix (AE latent-centroids, test)", digits = 2L,
    show_bottomright = FALSE, verbose = verbose
  )
  
  X_recon <- base::as.matrix(base::`dim<-`(base::as.array(ae$autoencoder(X_raw)),
                                           c(base::nrow(X_raw), base::ncol(X_raw))))
  
  p_pca <- pca_plot(
    x = X_recon, group_factor = y_fac, scale_unit = TRUE,
    ellipse = TRUE, ellipse_level = 0.95, verbose = verbose
  )
  
  p_mds <- mds_plot(
    x = X_recon, method = "euclidean", k = 2,
    labels = base::rownames(X_raw), group_factor = y_fac,
    title = "iris — MDS (reconstruit par AE)", verbose = verbose
  )
  
  p_latent <- viz_latent_space(z_tr, y_tr_fac, z_te, y_te_fac,
                               title = "AE — Espace latent", verbose = verbose)
  
  base::list(
    history = hist,
    confusion_plot = p_cm,
    pca = p_pca,
    mds = p_mds,
    latent_plot = p_latent,   # <--- ajout
    recon_error = base::mean((X_raw - X_recon)^2)
  )
}

# === Main wrapper ===
run_iris_demo <- function(seed = 123L, target = "Species", verbose = FALSE, auto_save = TRUE) {
  base::set.seed(seed)
  
  pp <- prepare_iris_data(target = target, prop = 0.7, seed = seed, verbose = verbose)
  vis_raw <- visualize_raw_data(pp$X_raw, pp$y_fac, verbose)
  mlp_res <- train_mlp_classifier(pp$X_train, pp$y_train_int, pp$X_test, pp$y_test_fac,
                                  pp$class_levels, verbose)
  ae_res <- train_autoencoder(pp$X_train, pp$X_test, pp$y_train_fac, pp$y_test_fac,
                              pp$class_levels, pp$X_raw, pp$y_fac, verbose)
  
  base::cat("Erreur moyenne de reconstruction :", ae_res$recon_error, "\n")
  if (verbose) base::message("[OK] Démo terminée.")
  
  # Génération des courbes d’historique
  p_hist_mlp <- plot_training_history(mlp_res$history, metrics = c("loss"), smooth = FALSE)
  p_hist_ae  <- plot_training_history(ae_res$history,  metrics = c("loss"), smooth = FALSE)
  
  results <- base::list(
    pca_raw           = vis_raw$pca,
    mds_raw           = vis_raw$mds,
    history_mlp_obj   = mlp_res$history,
    history_mlp_plot  = p_hist_mlp,
    cm_mlp            = mlp_res$confusion_plot,
    history_ae_obj    = ae_res$history,
    history_ae_plot   = p_hist_ae,
    cm_ae             = ae_res$confusion_plot,
    pca_recon         = ae_res$pca,
    mds_recon         = ae_res$mds
  )
  
  if (auto_save) save_outputs(results, verbose = verbose)
  
  base::invisible(results)
}

# Run if standalone
if (base::sys.nframe() == 0L) {
  run_iris_demo(seed = 123L, target = "Species", verbose = TRUE
  )
}
