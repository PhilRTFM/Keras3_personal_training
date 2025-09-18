# -- inst/examples/iris_unsuppervised.R — Analyse non supervisée IRIS ---------

# --- Chargement des dépendances ----------------------------------------------
base::source("R/utils.R",            chdir = TRUE)
base::source("R/unsuppervised_mod.R",chdir = TRUE)
base::source("R/visualizations.R",   chdir = TRUE)
base::source("R/eval_unsup.R",       chdir = TRUE)

# --- Fixer la graine globale -------------------------------------------------
set_seed_global(123L)

# --- Helpers internes --------------------------------------------------------

#' Prépare les données iris
#' @param target character Colonne cible
#' @param prop numeric proportion train
#' @param seed integer graine
#' @param verbose logical
#' @return list (voir preprocess_data)
prepare_iris_data <- function(target = "Species",
                              prop = 0.7,
                              seed = 123L,
                              verbose = FALSE) {
  return(preprocess_data(iris, target = target, prop = prop,
                         seed = seed, verbose = verbose))
}

# ... (reste du script inchangé) ...

#' Tableau descriptif IRIS
#' @param prep list sortie preprocess_data
#' @return ggplotify::as.ggplot tableau descriptif
iris_description_table <- function(prep) {
  n_vars  <- ncol(prep$X_raw)
  n_indiv <- nrow(prep$X_raw)
  n_train <- length(prep$idx_train)
  n_test  <- length(prep$idx_test)
  df <- data.frame(
    Variables = n_vars,
    Individus = n_indiv,
    Train = n_train,
    Test = n_test
  )
  tbl <- gridExtra::tableGrob(df, rows = NULL,
                              theme = gridExtra::ttheme_minimal(
                                core = list(fg_params = list(fontface = 1)),
                                colhead = list(fg_params = list(fontface = 2),
                                               bg_params = list(fill = "#e6e6e6"))))
  return(ggplotify::as.ggplot(tbl))
}

# --- Pipeline complet --------------------------------------------------------

#' Analyse non supervisée IRIS (Autoencodeur)
#'
#' @param seed integer graine
#' @param target character Colonne cible
#' @param verbose logical
#' @param auto_save logical Exporter PDF automatiquement
#' @return invisible(list(...)) Résultats
#' @export
run_iris_unsuppervised <- function(seed = 123L,
                                   target = "Species",
                                   verbose = FALSE,
                                   auto_save = TRUE) {
  # Prétraitement
  prep <- prepare_iris_data(target = target, seed = seed, verbose = verbose)
  
  # Page 1 : descriptif
  p_desc <- iris_description_table(prep)
  
  # Page 2 : MDS (brut)
  p_mds_raw <- mds_plot(prep$X_raw, group_factor = prep$y_fac, verbose = verbose)
  
  # Page 3 : PCA (brut)
  p_pca_raw <- pca_plot(prep$X_raw, group_factor = prep$y_fac, verbose = verbose)
  
  # Entraînement Autoencodeur (sur split standardisé)
  res_ae <- train_autoencoder(
    prep$X_train, prep$X_test,
    prep$y_train_fac, prep$y_test_fac,
    prep$class_levels,
    verbose = verbose
  )
  
  # Page 4 : Courbes d'entraînement AE
  p_history_ae <- history_curves_plot(res_ae$history,
                                      title = "Courbes entraînement AE",
                                      metrics = c("loss"),
                                      verbose = verbose)
  
  # Page 5 : Shepard plot — distances originales (X_test) vs distances en 2D (MDS sur X_recon)
  # NB: shepard_plot attend (dist_orig, coords_2D). On projette X_recon en 2D via cmdscale.
  d_orig <- stats::dist(prep$X_test)
  mds_recon <- stats::cmdscale(stats::dist(res_ae$X_recon), k = 2, eig = TRUE)
  coords_recon_2d <- as.matrix(mds_recon$points)
  colnames(coords_recon_2d) <- c("Dim1","Dim2")
  p_shepard <- shepard_plot(d_orig, coords_recon_2d, method = "pearson", verbose = verbose)
  
  # Page 6 : MDS sur X_recon (plot déjà fourni dans res_ae)
  p_mds_recon <- res_ae$mds_plot
  
  # Page 7 : PCA sur X_recon (plot déjà fourni dans res_ae)
  p_pca_recon <- res_ae$pca_plot
  
  # Préparer tous les plots dans l'ordre voulu
  all_plots <- list(
    p_desc,        # 1
    p_mds_raw,     # 2
    p_pca_raw,     # 3
    p_history_ae,  # 4
    p_shepard,     # 5
    p_mds_recon,   # 6
    p_pca_recon    # 7
  )
  
  # Export PDF 16:9 unique
  if (auto_save) {
    out_dir <- "outputs_iris"
    export_pdf_16x9(all_plots, out_dir, base_name = "iris_unsuppervised", verbose = verbose)
  }
  
  return(invisible(list(
    prep = prep,
    ae   = res_ae,
    plots = all_plots
  )))
}

# --- Exécution si lancé directement -----------------------------------------
if (base::sys.nframe() == 0L) {
  run_iris_unsuppervised(verbose = TRUE, auto_save = TRUE)
}
