# -- inst/examples/iris_supervised_multilabel.R -------------------------------

# --- Chargement des dépendances ----------------------------------------------
base::source("R/utils.R",                     chdir = TRUE)
base::source("R/supervised_mod_multilabel.R", chdir = TRUE)
base::source("R/visualizations.R",            chdir = TRUE)
base::source("R/eval_multi_label.R",          chdir = TRUE)

# --- Fixer la graine globale -------------------------------------------------
set_seed_global(123L)

# --- Helpers internes --------------------------------------------------------

#' Prépare les données iris multi-label
prepare_iris_multilabel <- function(prop = 0.7,
                                    seed = 123L,
                                    verbose = FALSE) {
  set.seed(seed)
  data <- iris
  class_names <- levels(data$Species)
  
  # Créer un dataset altéré multi-label : ajouter aléatoirement une 2ème étiquette
  n <- nrow(data)
  multi_idx <- sample(seq_len(n), size = floor(0.1 * n))
  Y_main <- as.character(data$Species)
  Y_second <- rep(NA_character_, n)
  Y_second[multi_idx] <- sapply(multi_idx, function(i) sample(setdiff(class_names, Y_main[i]), 1))
  
  # Binarisation multi-label
  Y_bin <- multi_label_binarizer(Y_main, Y_second, class_names)
  
  # Split train/test + standardisation
  idx <- stratified_split(factor(iris$Species), prop = prop, seed = seed)
  X <- as.matrix(iris[, 1:4])
  std <- standardize_train_test(X, idx$idx_train, idx$idx_test)
  
  return(list(
    X_raw = X,
    Y_bin = Y_bin,
    class_names = class_names,
    idx_train = idx$idx_train,
    idx_test = idx$idx_test,
    X_train = std$x_train,
    X_test  = std$x_test,
    Y_train_bin = Y_bin[idx$idx_train, , drop = FALSE],
    Y_test_bin  = Y_bin[idx$idx_test, , drop = FALSE]
  ))
}

#' Tableau descriptif IRIS multi-label
iris_multilabel_description <- function(prep) {
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

#' Courbes d'entraînement keras (robuste)
plot_history_simple <- function(history,
                                title = "Courbes entraînement MLP multi-label") {
  # --- Extraction ---
  if (!is.null(history$history)) {
    hist_df <- as.data.frame(history$history, stringsAsFactors = FALSE)
  } else if (is.list(history) && !is.null(history$metrics)) {
    hist_df <- as.data.frame(history$metrics, stringsAsFactors = FALSE)
  } else if (is.data.frame(history)) {
    hist_df <- history
  } else {
    stop("Format d'historique non reconnu : ", paste(class(history), collapse = ","))
  }
  
  # Toujours ajouter epoch
  if (!"epoch" %in% names(hist_df)) {
    hist_df$epoch <- seq_len(nrow(hist_df))
  }
  
  # Ne garder que colonnes numériques
  num_cols <- names(hist_df)[vapply(hist_df, is.numeric, logical(1))]
  df_long <- tidyr::pivot_longer(
    hist_df,
    cols = tidyselect::all_of(num_cols),
    names_to = "metric",
    values_to = "value"
  )
  
  # --- Plot ---
  p <- ggplot2::ggplot(df_long,
                       ggplot2::aes(x = epoch, y = value,
                                    color = metric, group = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = title, x = "Epoch", y = "Valeur") +
    ggplot2::theme_minimal()
  
  return(p)
}

# --- Pipeline complet --------------------------------------------------------

#' Analyse supervisée multi-label IRIS
run_iris_supervised_multilabel <- function(seed = 123L,
                                           verbose = FALSE,
                                           auto_save = TRUE) {
  # Prétraitement
  prep <- prepare_iris_multilabel(seed = seed, verbose = verbose)
  
  # Page 1 : descriptif + heatmap co-occurrence
  p_desc <- iris_multilabel_description(prep)
  cooc <- crossprod(prep$Y_bin)
  cooc_df <- as.data.frame(as.table(cooc))
  cooc_df$Var1 <- factor(cooc_df$Var1, levels = prep$class_names)
  cooc_df$Var2 <- factor(cooc_df$Var2, levels = prep$class_names)
  p_cooc <- ggplot2::ggplot(cooc_df, ggplot2::aes(x = Var1, y = Var2, fill = Freq)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = Freq)) +
    ggplot2::scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Co-occurrence des classes (IRIS alt)", x = NULL, y = NULL)
  
  # Page 2 : MDS
  p_mds <- mds_plot(prep$X_raw, group_factor = factor(iris$Species), verbose = verbose)
  
  # Page 3 : PCA
  p_pca <- pca_plot(prep$X_raw, group_factor = factor(iris$Species), verbose = verbose)
  
  # Entraînement MLP multi-label
  res_mlp <- train_mlp_multilabel_classifier(
    prep$X_train, prep$Y_train_bin,
    prep$X_test, prep$Y_test_bin,
    build_args = list(),
    fit_args = list(epochs = 50L, batch_size = 16L, validation_split = 0.2,
                    verbose = ifelse(verbose, 1, 0)),
    verbose = verbose
  )
  
  # Page 4 : Courbes d'entraînement (AUC, accuracy, loss)
  hist_df <- NULL
  if (!is.null(res_mlp$history$history)) {
    # keras3 style
    hist_df <- as.data.frame(res_mlp$history$history)
  } else if (!is.null(res_mlp$history$metrics)) {
    # ancien keras
    hist_df <- as.data.frame(res_mlp$history$metrics)
  }
  if (!is.null(hist_df)) {
    if (!"epoch" %in% names(hist_df)) hist_df$epoch <- seq_len(nrow(hist_df))
    df_long <- tidyr::pivot_longer(
      hist_df,
      cols = tidyselect::all_of(intersect(
        c("loss", "val_loss", "accuracy", "val_accuracy", "AUC", "val_AUC"),
        names(hist_df)
      )),
      names_to = "metric",
      values_to = "value"
    )
    p_history <- ggplot2::ggplot(df_long,
                                 ggplot2::aes(x = epoch, y = value,
                                              color = metric, group = metric)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 1) +
      ggplot2::labs(title = "Courbes entraînement MLP multi-label",
                    x = "Epoch", y = "Valeur") +
      ggplot2::theme_minimal()
  } else {
    p_history <- ggplot2::ggplot() +
      ggplot2::labs(title = "Courbes entraînement indisponibles") +
      ggplot2::theme_void()
  }
  
  
  # Page 5 : Confusions multi-label
  preds_bin <- mlp_predict_multilabel(res_mlp$model, prep$X_test)
  p_conf <- multilabel_confusion_heatmap(prep$Y_test_bin, preds_bin, prep$class_names)
  
  # Page 6 : Tableau métriques
  metrics <- multilabel_metrics(prep$Y_test_bin, preds_bin)
  p_tbl <- metrics_table_multilabel(metrics)
  
  # Préparer tous les plots
  all_plots <- list(
    p_desc,    # 1 descriptif
    p_cooc,    # 2 co-occurrence
    p_mds,     # 3 MDS
    p_pca,     # 4 PCA
    p_history, # 5 courbes entraînement
    p_conf,    # 6 confusions multi-label
    p_tbl      # 7 métriques
  )
  
  # Export PDF
  if (auto_save) {
    out_dir <- "outputs_iris"
    export_pdf_16x9(all_plots, out_dir, base_name = "iris_supervised_multilabel", verbose = verbose)
  }
  
  return(invisible(list(
    prep = prep,
    mlp  = res_mlp,
    plots = all_plots,
    metrics = metrics
  )))
}

# --- Exécution directe -------------------------------------------------------
if (base::sys.nframe() == 0L) {
  run_iris_supervised_multilabel(verbose = TRUE, auto_save = TRUE)
}
