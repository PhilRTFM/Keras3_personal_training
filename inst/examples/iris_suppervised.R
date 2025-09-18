# -- inst/examples/iris_suppervised.R — Analyse supervisée IRIS ----------------


# --- Chargement des dépendances ----------------------------------------------
base::source("R/utils.R", chdir = TRUE)
base::source("R/suppervised_mod.R", chdir = TRUE)
base::source("R/visualizations.R", chdir = TRUE)
base::source("R/eval_sup.R", chdir = TRUE)


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

#' Tableau descriptif IRIS
#' @param prep list sortie preprocess_data
#' @return ggplotify::as.ggplot tableau descriptif
iris_description_table <- function(prep) {
  n_vars <- ncol(prep$X_raw)
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

#' Analyse supervisée IRIS
#'
#' @param seed integer graine
#' @param target character Colonne cible
#' @param verbose logical
#' @param auto_save logical Exporter PDF automatiquement
#' @return invisible(list(...)) Résultats
#' @export
run_iris_suppervised <- function(seed = 123L,
                                 target = "Species",
                                 verbose = FALSE,
                                 auto_save = TRUE) {
  
  # Prétraitement
  prep <- prepare_iris_data(target = target, seed = seed, verbose = verbose)
  
  # Page 1 : descriptif
  p_desc <- iris_description_table(prep)
  
  # Page 2 : MDS
  p_mds <- mds_plot(prep$X_raw, group_factor = prep$y_fac, verbose = verbose)
  
  # Page 3 : PCA
  p_pca <- pca_plot(prep$X_raw, group_factor = prep$y_fac, verbose = verbose)
  
  # Page 4-6 : Entraînement MLP
  res_mlp <- train_mlp_classifier(
    prep$X_train, prep$y_train_int,
    prep$X_test, prep$y_test_fac,
    prep$class_levels,
    verbose = verbose
  )
  
  p_history_mlp <- history_curves_plot(res_mlp$history,
                                       title = "Courbes entraînement MLP",
                                       verbose = verbose)
  
  # Confusion train
  preds_train <- mlp_predict_labels(res_mlp$model, prep$X_train,
                                    class_levels = prep$class_levels)
  conf_train <- viz_confusion_panel_from_predictions(prep$y_train_fac,
                                                     preds_train,
                                                     labels = prep$class_levels,
                                                     title = "Confusion Matrix (Train)")
  # Confusion test (déjà dans res_mlp)
  conf_test <- res_mlp$confusion_plot
  
  # Préparer tous les plots
  all_plots <- list(
    p_desc,         # Page 1
    p_mds,          # Page 2
    p_pca,          # Page 3
    p_history_mlp,  # Page 4
    conf_train,     # Page 5
    conf_test       # Page 6
  )
  
  # Export PDF 16:9 unique
  if (auto_save) {
    out_dir <- "outputs_iris"
    export_pdf_16x9(all_plots, out_dir, base_name = "iris_suppervised", verbose = verbose)
  }
  
  return(invisible(list(
    prep = prep,
    mlp = res_mlp,
    plots = all_plots
  )))
}

# --- Exécution si lancé directement -----------------------------------------
if (base::sys.nframe() == 0L) {
  run_iris_suppervised(verbose = TRUE, auto_save = TRUE)
}