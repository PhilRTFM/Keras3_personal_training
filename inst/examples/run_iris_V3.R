# -- inst/examples/run_iris.R — Démonstration complète sur iris ---------------

# --- Chargement des dépendances ----------------------------------------------
base::source("R/utils.R",            chdir = TRUE)
base::source("R/suppervised_mod.R",  chdir = TRUE)
base::source("R/unsuppervised_mod.R",chdir = TRUE)
base::source("R/visualizations.R",   chdir = TRUE)
base::source("R/eval_sup.R",         chdir = TRUE)
base::source("R/eval_unsup.R",       chdir = TRUE)

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

#' Visualisation brute iris (PCA + MDS)
#' @param X_raw matrix
#' @param y_fac factor
#' @param verbose logical
#' @return list(pca, mds)
visualize_raw_data <- function(X_raw,
                               y_fac,
                               verbose = FALSE) {
  p_pca <- pca_plot(X_raw, group_factor = y_fac, verbose = verbose)
  p_mds <- mds_plot(X_raw, group_factor = y_fac, verbose = verbose)
  return(list(pca = p_pca, mds = p_mds))
}

# --- Pipeline complet --------------------------------------------------------

#' Démonstration complète sur iris
#'
#' @param seed integer Graine
#' @param target character Colonne cible
#' @param verbose logical
#' @param auto_save logical Exporter PDF automatiquement
#'
#' @return invisible(list(...)) Résultats de la démo
#' @export
run_iris_demo <- function(seed = 123L,
                          target = "Species",
                          verbose = FALSE,
                          auto_save = TRUE) {
  
  # Prétraitement
  prep <- prepare_iris_data(target = target, seed = seed, verbose = verbose)
  
  # Visualisations brutes
  vis_raw <- visualize_raw_data(prep$X_raw, prep$y_fac, verbose = verbose)
  
  # Entraînement MLP
  res_mlp <- train_mlp_classifier(
    prep$X_train, prep$y_train_int,
    prep$X_test, prep$y_test_fac,
    prep$class_levels,
    verbose = verbose
  )
  
  # Courbes MLP
  p_history_mlp <- history_curves_plot(res_mlp$history,
                                       title = "Courbes entraînement MLP",
                                       verbose = verbose)
  
  # Entraînement Autoencodeur
  res_ae <- train_autoencoder(
    prep$X_train, prep$X_test,
    prep$y_train_fac, prep$y_test_fac,
    prep$class_levels,
    verbose = verbose
  )
  
  # Courbes AE
  p_history_ae <- history_curves_plot(res_ae$history,
                                      title = "Courbes entraînement AE",
                                      verbose = verbose)
  
  # Distribution des erreurs reconstruction
  p_err <- error_distributions(res_ae$errors_indiv,
                               title = "Distribution erreurs reconstruction")
  
  # Shepard plot original vs reconstruit
  p_shep <- shepard_plot(stats::dist(prep$X_test),
                         stats::dist(res_ae$X_recon),
                         method = "pearson",
                         verbose = verbose)
  
  # Préparer tous les plots
  all_plots <- list(
    vis_raw$pca,            # PCA brut
    vis_raw$mds,            # MDS brut
    p_history_mlp,          # courbes entraînement MLP
    res_mlp$confusion_plot, # confusion MLP
    p_history_ae,           # courbes entraînement AE
    res_ae$confusion_plot,  # confusion AE (latents)
    p_err,                  # distribution erreurs AE
    p_shep,                 # Shepard plot AE
    res_ae$pca_plot,        # PCA reconstruit
    res_ae$mds_plot         # MDS reconstruit
  )
  
  # Export PDF 16:9 unique
  if (auto_save) {
    out_dir <- "outputs_iris"
    export_pdf_16x9(all_plots, out_dir, base_name = "iris_demo", verbose = verbose)
  }
  
  return(invisible(list(
    prep = prep,
    vis_raw = vis_raw,
    mlp = res_mlp,
    ae = res_ae
  )))
}

# --- Exécution si lancé directement -----------------------------------------
if (base::sys.nframe() == 0L) {
  run_iris_demo(verbose = TRUE, auto_save = TRUE)
}
