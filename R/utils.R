# -- R/utils.R — Preprocessing, helpers, sauvegarde & export PDF --------------

#' Découpage stratifié train/test (indices)
#'
#' @param y factor Étiquettes de classes
#' @param prop numeric Proportion train (0 < prop < 1)
#' @param seed integer graine de reproductibilité
#'
#' @return list avec vecteurs idx_train et idx_test
#' @examples
#' idx <- stratified_split(iris$Species, prop = 0.7)
#' @export
stratified_split <- function(y, prop = 0.7, seed = 123L) {
  base::set.seed(seed)
  y <- base::as.factor(y)
  classes <- base::levels(y)
  idx_train <- idx_test <- integer(0)
  for (cl in classes) {
    idx <- base::which(y == cl)
    n_tr <- base::max(1L, base::floor(base::length(idx) * prop))
    set <- base::sample(idx, size = n_tr)
    idx_train <- base::c(idx_train, set)
    idx_test  <- base::c(idx_test,  base::setdiff(idx, set))
  }
  return(list(idx_train = idx_train, idx_test = idx_test))
}

#' Standardisation train/test (centrage-réduction)
#'
#' @param X matrix|data.frame Features
#' @param idx_train integer[] indices train
#' @param idx_test integer[] indices test
#'
#' @return list avec x_train, x_test, mean, sd
#' @examples
#' split <- stratified_split(iris$Species)
#' std <- standardize_train_test(iris[,1:4], split$idx_train, split$idx_test)
#' @export
standardize_train_test <- function(X, idx_train, idx_test) {
  X <- base::as.matrix(X)
  mu <- base::apply(X[idx_train, , drop = FALSE], 2, base::mean, na.rm = TRUE)
  sigma <- base::apply(X[idx_train, , drop = FALSE], 2, stats::sd, na.rm = TRUE)
  sigma[sigma == 0] <- 1
  X_train <- base::scale(X[idx_train, , drop = FALSE], center = mu, scale = sigma)
  X_test  <- base::scale(X[idx_test, , drop = FALSE],  center = mu, scale = sigma)
  return(list(
    x_train = base::as.matrix(X_train),
    x_test  = base::as.matrix(X_test),
    mean    = mu,
    sd      = sigma
  ))
}

#' Prédiction par centroïdes (classification latente)
#'
#' @param z_train matrix Coordonnées latentes (train)
#' @param y_train factor Labels train
#' @param z_test matrix Coordonnées latentes (test)
#' @param class_levels character[] Niveaux de classes
#' @param verbose logical
#'
#' @return factor Prédictions factorielles sur z_test
#' @examples
#' \dontrun{
#' preds <- predict_by_centroids(z_train, y_train, z_test,
#'                               class_levels = levels(y_train))
#' }
#' @export
predict_by_centroids <- function(z_train,
                                 y_train,
                                 z_test,
                                 class_levels,
                                 verbose = FALSE) {
  # calcul des centroïdes par classe
  centroids <- lapply(class_levels, function(cl) {
    rows <- which(y_train == cl)
    if (length(rows) == 0) return(rep(NA_real_, ncol(z_train)))
    colMeans(z_train[rows, , drop = FALSE], na.rm = TRUE)
  })
  centroids <- do.call(rbind, centroids)
  rownames(centroids) <- class_levels
  
  # distances test vs centroïdes
  dists <- as.matrix(stats::dist(rbind(z_test, centroids)))
  n_test <- nrow(z_test)
  d_mat <- dists[1:n_test, (n_test + 1):(n_test + length(class_levels)), drop = FALSE]
  
  # assignation à la classe du centroïde le plus proche
  nearest <- apply(d_mat, 1, which.min)
  preds <- factor(class_levels[nearest], levels = class_levels)
  
  if (verbose) message("Prédictions par centroïdes réalisées sur ", n_test, " instances")
  return(preds)
}


#' Prétraitement Iris (wrapper)
#'
#' @param data data.frame Jeu de données
#' @param target character Nom de la colonne cible
#' @param prop numeric proportion train
#' @param seed integer graine de reproductibilité
#' @param verbose logical Affiche des infos
#'
#' @return list (X_raw, y_fac, class_levels, idx_train, idx_test,
#'   X_train, X_test, y_train_fac, y_test_fac, y_train_int, y_test_int)
#' @examples
#' prep <- preprocess_data(iris, "Species")
#' @export
preprocess_data <- function(data,
                            target = "Species",
                            prop = 0.7,
                            seed = 123L,
                            verbose = FALSE) {
  # Cible
  y <- base::as.factor(data[[target]])
  
  # Retirer la cible et garder uniquement les colonnes numériques
  X <- data[, base::setdiff(base::colnames(data), target), drop = FALSE]
  X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
  X <- base::as.matrix(X)
  
  # Split stratifié + standardisation
  split <- stratified_split(y, prop, seed)
  std <- standardize_train_test(X, split$idx_train, split$idx_test)
  
  # Labels train/test
  y_train_fac <- y[split$idx_train]
  y_test_fac  <- y[split$idx_test]
  y_train_int <- base::as.integer(y_train_fac) - 1L
  y_test_int  <- base::as.integer(y_test_fac) - 1L
  
  if (verbose) base::message("Classes : ", base::paste(base::levels(y), collapse = ", "))
  
  return(list(
    X_raw       = X,
    y_fac       = y,
    class_levels = base::levels(y),
    idx_train   = split$idx_train,
    idx_test    = split$idx_test,
    X_train     = std$x_train,
    X_test      = std$x_test,
    y_train_fac = y_train_fac,
    y_test_fac  = y_test_fac,
    y_train_int = y_train_int,
    y_test_int  = y_test_int
  ))
}



#' Prétraitement msleep (avec variables numériques + cible catégorielle)
#'
#' @param data data.frame msleep
#' @param target character variable cible (ex: "vore")
#' @param prop numeric proportion train
#' @param seed integer graine
#' @param verbose logical
#'
#' @return list similaire à preprocess_data
#' @export
preprocess_data_msleep <- function(data,
                                   target = "vore",
                                   prop = 0.7,
                                   seed = 123L,
                                   verbose = FALSE) {
  num_vars <- base::c("sleep_total","sleep_rem","sleep_cycle","awake","brainwt","bodywt")
  data_num <- data[, num_vars]
  y <- base::as.factor(data[[target]])
  split <- stratified_split(y, prop, seed)
  std <- standardize_train_test(data_num, split$idx_train, split$idx_test)
  y_train_fac <- y[split$idx_train]
  y_test_fac  <- y[split$idx_test]
  y_train_int <- base::as.integer(y_train_fac) - 1L
  y_test_int  <- base::as.integer(y_test_fac) - 1L
  if (verbose) base::message("Classes : ", base::paste(base::levels(y), collapse = ", "))
  return(list(
    X_raw       = base::as.matrix(data_num),
    y_fac       = y,
    class_levels = base::levels(y),
    idx_train   = split$idx_train,
    idx_test    = split$idx_test,
    X_train     = std$x_train,
    X_test      = std$x_test,
    y_train_fac = y_train_fac,
    y_test_fac  = y_test_fac,
    y_train_int = y_train_int,
    y_test_int  = y_test_int
  ))
}

#' Wrapper simple msleep
#'
#' @param target character variable cible
#' @param prop numeric proportion train
#' @param seed integer graine
#' @param verbose logical
#'
#' @return list résultat de preprocess_data_msleep
#' @export
prepare_msleep_data <- function(target = "vore",
                                prop = 0.7,
                                seed = 123L,
                                verbose = FALSE) {
  return(preprocess_data_msleep(ggplot2::msleep,
                                target = target,
                                prop = prop,
                                seed = seed,
                                verbose = verbose))
}

#' ISO date (YYYY-MM-DD)
#' @return character Date au format ISO
#' @examples iso_date()
#' @export
iso_date <- function() {
  return(base::format(base::Sys.Date(), "%Y-%m-%d"))
}

#' Ouvre un device PDF 16:9
#'
#' @param path character chemin du fichier
#' @param width_in numeric largeur en pouces
#' @param height_in numeric hauteur en pouces
#'
#' @return NULL (ouvre un device PDF)
#' @examples
#' \dontrun{ open_pdf_16x9("figures.pdf") }
#' @export
open_pdf_16x9 <- function(path, width_in = 16, height_in = 9) {
  grDevices::pdf(file = path, width = width_in, height = height_in, onefile = TRUE)
  return(invisible(NULL))
}

#' Exporter une liste de plots en PDF 16:9
#'
#' @param plots list de ggplot
#' @param out_dir character dossier de sortie
#' @param base_name character nom de base du fichier
#' @param verbose logical
#'
#' @return character chemin du PDF créé
#' @examples
#' \dontrun{
#' export_pdf_16x9(list(p1,p2), "outputs", "iris_demo")
#' }
#' @export
export_pdf_16x9 <- function(plots,
                            out_dir,
                            base_name,
                            verbose = FALSE) {
  if (!base::dir.exists(out_dir)) base::dir.create(out_dir, recursive = TRUE)
  file <- base::file.path(out_dir, base::paste0(base_name, "_", iso_date(), ".pdf"))
  if (verbose) base::message("Export PDF : ", file)
  open_pdf_16x9(file)
  for (p in plots) {
    base::print(p)
  }
  grDevices::dev.off()
  return(file)
}

#' Sauvegarde générique des résultats (RDS, PNG)
#'
#' @param res list résultats
#' @param output_dir character dossier
#' @param verbose logical
#'
#' @return invisible(NULL)
#' @export
save_outputs <- function(res, output_dir = NULL, verbose = FALSE) {
  if (base::is.null(output_dir)) return(invisible(NULL))
  if (!base::dir.exists(output_dir)) base::dir.create(output_dir, recursive = TRUE)
  rds_path <- base::file.path(output_dir, "results.rds")
  base::saveRDS(res, rds_path)
  if (verbose) base::message("Résultats sauvegardés : ", rds_path)
  return(invisible(NULL))
}

#' Trace les courbes d'entraînement (loss / accuracy)
#'
#' Compatible keras3 (keras.src.callbacks.history.History) et data.frame/list.
#'
#' @param history keras History | list | data.frame
#' @param title character Titre du graphique
#' @param metrics character[] Noms de métriques à tracer
#' @param verbose logical
#'
#' @return ggplot2::ggplot
#' @export
history_curves_plot <- function(history,
                                title = "Courbes entraînement",
                                metrics = c("accuracy", "loss"),
                                verbose = FALSE) {
  # --- Extraction en data.frame ---
  if (inherits(history, "keras.src.callbacks.history.History")) {
    # keras3 → utiliser $history
    hist_list <- history$history
    hist_df <- as.data.frame(hist_list, stringsAsFactors = FALSE)
    hist_df$epoch <- seq_len(nrow(hist_df))
  } else if (is.list(history) && !is.null(history$metrics)) {
    # ancien keras
    hist_df <- as.data.frame(history$metrics)
    hist_df$epoch <- seq_len(nrow(hist_df))
  } else if (is.data.frame(history)) {
    hist_df <- history
    if (!"epoch" %in% names(hist_df)) {
      hist_df$epoch <- seq_len(nrow(hist_df))
    }
  } else {
    stop("Format d'historique non supporté (classe = ",
         paste(class(history), collapse = ","), ")")
  }
  
  if (verbose) {
    message("[history_curves_plot] Colonnes disponibles : ",
            paste(names(hist_df), collapse = ", "))
  }
  
  # --- Colonnes à tracer ---
  keep <- names(hist_df)[names(hist_df) %in% c(metrics, paste0("val_", metrics))]
  if (length(keep) == 0) {
    stop("Aucune métrique trouvée dans l'historique.")
  }
  
  df_long <- tidyr::pivot_longer(
    hist_df,
    cols = dplyr::all_of(keep),
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
