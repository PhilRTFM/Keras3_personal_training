# =====================================================================
# R/analysis.R
# Conventions : roxygen, pkg::fun(), verbose en dernier (par défaut FALSE), return() partout
# =====================================================================

# =====================================================================
# inst/examples/run_iris.R — Pipeline IRIS (MLP + PCA/MDS + AE)
# Mode standalone : source uniquement R/models.R, R/analysis.R, R/metrics.R
# Conventions : tout namespacé pour les packages externes, pas de library()/install.*
# Dépendances (du repo) : keras3, FactoMineR, ggplot2, MASS, stats, tibble, dplyr, tidyr, patchwork
# Sorties : figures PNG et CSV dans inst/examples/outputs/
# =====================================================================


# -----------------------------------------------------------------------------
# Démonstration complète sur iris :
#  - Pré-traitement pour MLP (supervisé) et AE (non-supervisé)
#  - Entraînement MLP (+ prédictions & matrices de confusion)
#  - Entraînement Autoencodeur (+ extraction des codes latents)
#  - PCA & MDS sur iris (brut) et sur l'espace latent de l'AE
#  - Affichage des courbes d'entraînement (history) et des panneaux de confusion
# -----------------------------------------------------------------------------


#' Exécute une PCA (ACP) via FactoMineR (calcul seul)
#' @param x matrix|data.frame, données numériques (lignes = individus, colonnes = variables)
#' @param scale_unit logical, standardiser en interne (TRUE recommandé pour le cercle de corrélations)
#' @param graph logical, laisser FactoMineR dessiner ses graphes (FALSE par défaut)
#' @param verbose logical, afficher des messages d'étapes (FALSE par défaut)
#' @return FactoMineR::PCA, l'objet PCA complet
pca_run <- function(x, scale_unit = TRUE, graph = FALSE, verbose = FALSE) {
  if (isTRUE(verbose)) base::message("[PCA] run …")
  pca <- FactoMineR::PCA(base::as.data.frame(x), scale.unit = scale_unit, graph = graph)
  return(pca)
}

#' Construit la table des individus pour ggplot
#' @param pca FactoMineR::PCA, objet PCA
#' @param dims integer, dimensions à tracer (longueur 2)
#' @param group_factor factor|NULL, groupe par individu pour la couleur
#' @param verbose logical, messages (FALSE)
#' @return tibble, colonnes = Dim.1, Dim.2, cos2, contrib, group (optionnel), label
pca_build_ind_df <- function(pca, dims = c(1, 2), group_factor = NULL, verbose = FALSE) {
  coords <- pca$ind$coord[, dims, drop = FALSE]
  cos2  <- pca$ind$cos2[, dims, drop = FALSE]
  contrib <- pca$ind$contrib[, dims, drop = FALSE]
  
  df <- tibble::tibble(
    label = base::rownames(coords),
    Dim.1 = coords[, 1],
    Dim.2 = coords[, 2],
    cos2  = base::rowSums(cos2),
    contrib = base::rowSums(contrib)
  )
  if (!base::is.null(group_factor)) df$group <- base::as.factor(group_factor)
  if (isTRUE(verbose)) base::message("[PCA] ind_df: ", nrow(df), " individus")
  return(df)
}

#' Construit la table des variables pour ggplot (cercle des corrélations)
#' @param pca FactoMineR::PCA, objet PCA
#' @param dims integer, dimensions à tracer (longueur 2)
#' @param verbose logical, messages (FALSE)
#' @return tibble, colonnes = Dim.1, Dim.2, cos2, contrib, var
pca_build_var_df <- function(pca, dims = c(1, 2), verbose = FALSE) {
  coords <- pca$var$coord[, dims, drop = FALSE]
  cos2   <- pca$var$cos2[, dims, drop = FALSE]
  contrib <- pca$var$contrib[, dims, drop = FALSE]
  
  df <- tibble::tibble(
    var   = base::rownames(coords),
    Dim.1 = coords[, 1],
    Dim.2 = coords[, 2],
    cos2  = base::rowSums(cos2),
    contrib = base::rowSums(contrib)
  )
  if (isTRUE(verbose)) base::message("[PCA] var_df: ", nrow(df), " variables")
  return(df)
}

#' Scatter des individus (ggplot), ellipses optionnelles
#' @param ind_df tibble, issu de pca_build_ind_df()
#' @param ellipse logical, tracer des ellipses (FALSE)
#' @param ellipse_level numeric, niveau de confiance des ellipses (0.95)
#' @param verbose logical, messages (FALSE)
#' @return ggplot, graphique individus
pca_ind_plot <- function(ind_df, ellipse = FALSE, ellipse_level = 0.95, verbose = FALSE) {
  p <- ggplot2::ggplot(ind_df, ggplot2::aes(x = .data$Dim.1, y = .data$Dim.2, color = .data$group)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), hjust = 0, vjust = 1.2, size = 3, show.legend = FALSE) +
    ggplot2::labs(title = "PCA — individus", x = "Dim 1", y = "Dim 2") +
    ggplot2::theme_minimal(base_size = 12)
  
  if (ellipse && "group" %in% base::colnames(ind_df)) {
    p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = .data$group), level = ellipse_level)
  }
  if (isTRUE(verbose)) base::message("[PCA] ind_plot prêt")
  return(p)
}

#' Cercle des corrélations (ggplot)
#' @param var_df tibble, issu de pca_build_var_df()
#' @param circle_radius numeric, rayon du cercle unité (1)
#' @param verbose logical, messages (FALSE)
#' @return ggplot, graphique cercle des corrélations
pca_var_circle_plot <- function(var_df, circle_radius = 1, verbose = FALSE) {
  circle <- base::within(base::data.frame(t = base::seq(0, 2 * base::pi, length.out = 200)), {
    x <- circle_radius * base::cos(t)
    y <- circle_radius * base::sin(t)
  })
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = circle, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_segment(
      data = var_df,
      ggplot2::aes(x = 0, y = 0, xend = .data$Dim.1, yend = .data$Dim.2),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc"))
    ) +
    ggplot2::geom_text(
      data = var_df,
      ggplot2::aes(x = .data$Dim.1, y = .data$Dim.2, label = .data$var),
      hjust = 0, vjust = -0.5, size = 3
    ) +
    ggplot2::labs(title = "PCA — cercle des corrélations", x = "Dim 1", y = "Dim 2") +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 12)
  if (isTRUE(verbose)) base::message("[PCA] var_circle_plot prêt")
  return(p)
}

#' Barplot % variance expliquée
#' @param pca FactoMineR::PCA, objet PCA
#' @param top_n integer, nombre de premières dimensions à afficher (10)
#' @param verbose logical, messages (FALSE)
#' @return ggplot, barplot de la variance expliquée
pca_variance_barplot <- function(pca, top_n = 10, verbose = FALSE) {
  eig <- base::as.data.frame(pca$eig)
  eig$Dim <- base::paste0("Dim ", base::seq_len(nrow(eig)))
  eig <- eig[seq_len(base::min(top_n, nrow(eig))), , drop = FALSE]
  
  p <- ggplot2::ggplot(eig, ggplot2::aes(x = .data$Dim, y = .data$`percentage of variance`)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = base::paste0(base::round(.data$`percentage of variance`, 1), "%")),
                       vjust = -0.25, size = 3) +
    ggplot2::labs(title = "PCA — variance expliquée", x = "Dimension", y = "%") +
    ggplot2::theme_minimal(base_size = 12)
  if (isTRUE(verbose)) base::message("[PCA] variance barplot prêt")
  return(p)
}

#' Wrapper 3 graphes PCA (individus, cercle, variance)
#' @param pca FactoMineR::PCA, objet PCA
#' @param group_factor factor|NULL, groupes pour individus
#' @param dims integer, dimensions (longueur 2)
#' @param ellipse logical, ellipses sur individus (FALSE)
#' @param ellipse_level numeric, niveau d'ellipse (0.95)
#' @param top_n integer, barplot des top_n dimensions (10)
#' @param verbose logical, messages (FALSE)
#' @return list, ind_plot, var_circle_plot, var_prop_plot
pca_three_plots_custom <- function(pca, group_factor = NULL, dims = c(1, 2),
                                   ellipse = FALSE, ellipse_level = 0.95,
                                   top_n = 10, verbose = FALSE) {
  ind_df <- pca_build_ind_df(pca, dims = dims, group_factor = group_factor, verbose = verbose)
  var_df <- pca_build_var_df(pca, dims = dims, verbose = verbose)
  p1 <- pca_ind_plot(ind_df, ellipse = ellipse, ellipse_level = ellipse_level, verbose = verbose)
  p2 <- pca_var_circle_plot(var_df, verbose = verbose)
  p3 <- pca_variance_barplot(pca, top_n = top_n, verbose = verbose)
  res <- base::list(ind_plot = p1, var_circle_plot = p2, var_prop_plot = p3)
  return(res)
}

#' Calcule une matrice de distances (stats::dist) avec plusieurs méthodes
#' @param x matrix|data.frame, données numériques (lignes = objets)
#' @param method character, "euclidean","manhattan","canberra","binary","minkowski"
#' @param p numeric, paramètre p pour minkowski (2 par défaut)
#' @param verbose logical, messages (FALSE)
#' @return dist, objet distance
mds_compute_distance <- function(x, method = "euclidean", p = 2, verbose = FALSE) {
  if (isTRUE(verbose)) base::message("[MDS] distance: method=", method)
  d <- stats::dist(x = x, method = method, p = p)
  return(d)
}

#' MDS métrique (cmdscale)
#' @param dist_obj dist, objet distance
#' @param k integer, dimension projetée (2)
#' @param verbose logical, messages (FALSE)
#' @return list, df (coords tibble), eig (valeurs propres), fit (matrice brute)
mds_metric_run <- function(dist_obj, k = 2, verbose = FALSE) {
  fit <- stats::cmdscale(d = dist_obj, k = k, eig = TRUE)
  coords <- tibble::as_tibble(fit$points, .name_repair = "minimal")
  base::colnames(coords) <- base::paste0("Dim.", base::seq_len(ncol(coords)))
  if (isTRUE(verbose)) base::message("[MDS] metric: dims=", k)
  res <- base::list(df = coords, eig = fit$eig, fit = fit$points)
  return(res)
}

#' MDS non-métrique (Kruskal, MASS::isoMDS)
#' @param dist_obj dist, objet distance
#' @param k integer, dimension projetée (2)
#' @param verbose logical, messages (FALSE)
#' @return list, df (coords tibble), stress (numeric), fit (objet isoMDS)
mds_nonmetric_run <- function(dist_obj, k = 2, verbose = FALSE) {
  fit <- MASS::isoMDS(d = dist_obj, k = k)
  coords <- tibble::as_tibble(fit$points, .name_repair = "minimal")
  base::colnames(coords) <- base::paste0("Dim.", base::seq_len(ncol(coords)))
  if (isTRUE(verbose)) base::message("[MDS] non-metric: stress=", fit$stress)
  res <- base::list(df = coords, stress = fit$stress, fit = fit)
  return(res)
}

#' Scatter ggplot des coordonnées MDS
#' @param coords_df tibble, colonnes Dim.1, Dim.2
#' @param labels character|NULL, étiquettes des points
#' @param title character|NULL, titre
#' @param verbose logical, messages (FALSE)
#' @return ggplot, nuage de points
mds_scatter_plot <- function(coords_df, labels = NULL, title = NULL, verbose = FALSE) {
  p <- ggplot2::ggplot(coords_df, ggplot2::aes(x = .data$Dim.1, y = .data$Dim.2)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = labels), hjust = 0, vjust = -0.5, size = 3, na.rm = TRUE) +
    ggplot2::labs(title = base::ifelse(base::is.null(title), "MDS — scatter", title),
                  x = "Dim 1", y = "Dim 2") +
    ggplot2::theme_minimal(base_size = 12)
  if (isTRUE(verbose)) base::message("[MDS] scatter prêt")
  return(p)
}

#' Shepard plot pour diagnostic MDS (distances originales vs projetées)
#' @param dist_obj dist, distances originales (espace d'entrée)
#' @param coords_df tibble|data.frame, coordonnées MDS projetées (colonnes Dim.1, Dim.2, ...)
#' @param verbose logical, messages (FALSE)
#' @return ggplot, nuage des paires (d_orig, d_proj) avec diagonale y=x et lissage loess
#' @details
#' Compare visuellement les distances originales (axe x) aux distances dans l'espace
#' MDS (axe y). Un bon ajustement donne des points proches de la diagonale.
#' Utilise base::startsWith("Dim.") pour éviter les problèmes d'échappement regex.
mds_shepard_plot <- function(dist_obj, coords_df, verbose = FALSE) {
  if (!inherits(dist_obj, "dist")) base::stop("dist_obj doit être un objet 'dist'.")
  cn <- base::colnames(coords_df)
  dim_cols <- cn[base::startsWith(cn, "Dim.")]
  if (length(dim_cols) < 2) base::stop("coords_df doit contenir au moins deux colonnes débutant par 'Dim.' (ex: Dim.1, Dim.2).")
  
  d_orig <- base::as.numeric(dist_obj)
  d_proj <- base::as.numeric(stats::dist(coords_df[, dim_cols, drop = FALSE]))
  
  df <- tibble::tibble(orig = d_orig, proj = d_proj)
  rmse <- base::sqrt(base::mean((df$proj - df$orig)^2))
  r <- stats::cor(df$orig, df$proj, method = "pearson")
  ttl <- base::sprintf("MDS — Shepard plot (RMSE=%.3f, r=%.3f)", rmse, r)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$orig, y = .data$proj)) +
    ggplot2::geom_point(alpha = 0.5, size = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = ttl, x = "Distance originale", y = "Distance projetée") +
    ggplot2::theme_minimal(base_size = 12)
  
  if (isTRUE(verbose)) base::message("[MDS] Shepard plot prêt (n=", nrow(df), ")")
  return(p)
}


# -- MDS avec stats::cmdscale et MASS::isoMDS ----

#' Calcule une matrice de distances (stats::dist) avec plusieurs méthodes
#' @param x matrix|data.frame, données numériques (lignes = objets)
#' @param method character, "euclidean","manhattan","canberra","binary","minkowski"
#' @param p numeric, paramètre p pour minkowski (2 par défaut)
#' @param verbose logical, messages (FALSE)
#' @return dist, objet distance

mds_compute_distance <- function(x, method = "euclidean", p = 2, verbose = FALSE) {
  if (isTRUE(verbose)) base::message("[MDS] distance: method=", method)
  d <- stats::dist(x = x, method = method, p = p)
  return(d)
}

#' MDS métrique (cmdscale)
#' @param dist_obj dist, objet distance
#' @param k integer, dimension projetée (2)
#' @param verbose logical, messages (FALSE)
#' @return list, df (coords tibble), eig (valeurs propres), fit (matrice brute)
mds_metric_run <- function(dist_obj, k = 2, verbose = FALSE) {
  fit <- stats::cmdscale(d = dist_obj, k = k, eig = TRUE)
  coords <- tibble::as_tibble(fit$points, .name_repair = "minimal")
  base::colnames(coords) <- base::paste0("Dim.", base::seq_len(ncol(coords)))
  if (isTRUE(verbose)) base::message("[MDS] metric: dims=", k)
  res <- base::list(df = coords, eig = fit$eig, fit = fit$points)
  return(res)
}

#' MDS non-métrique (Kruskal, MASS::isoMDS)
#' @param dist_obj dist, objet distance
#' @param k integer, dimension projetée (2)
#' @param verbose logical, messages (FALSE)
#' @return list, df (coords tibble), stress (numeric), fit (objet isoMDS)
mds_nonmetric_run <- function(dist_obj, k = 2, verbose = FALSE) {
  fit <- MASS::isoMDS(d = dist_obj, k = k)
  coords <- tibble::as_tibble(fit$points, .name_repair = "minimal")
  base::colnames(coords) <- base::paste0("Dim.", base::seq_len(ncol(coords)))
  if (isTRUE(verbose)) base::message("[MDS] non-metric: stress=", fit$stress)
  res <- base::list(df = coords, stress = fit$stress, fit = fit)
  return(res)
}

#' Scatter ggplot des coordonnées MDS
#' @param coords_df tibble, colonnes Dim.1, Dim.2
#' @param labels character|NULL, étiquettes des points
#' @param title character|NULL, titre
#' @param verbose logical, messages (FALSE)
#' @return ggplot, nuage de points
mds_scatter_plot <- function(coords_df, labels = NULL, title = NULL, verbose = FALSE) {
  p <- ggplot2::ggplot(coords_df, ggplot2::aes(x = .data$Dim.1, y = .data$Dim.2)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = labels), hjust = 0, vjust = -0.5, size = 3, na.rm = TRUE) +
    ggplot2::labs(title = base::ifelse(base::is.null(title), "MDS — scatter", title),
                  x = "Dim 1", y = "Dim 2") +
    ggplot2::theme_minimal(base_size = 12)
  if (isTRUE(verbose)) base::message("[MDS] scatter prêt")
  return(p)
}

#' Shepard plot pour diagnostic MDS (distances originales vs projetées)
#' @param dist_obj dist, distances originales (espace d'entrée)
#' @param coords_df tibble|data.frame, coordonnées MDS projetées (colonnes Dim.1, Dim.2, ...)
#' @param verbose logical, messages (FALSE)
#' @return ggplot, nuage des paires (d_orig, d_proj) avec diagonale y=x et lissage loess
#' @details
#' Compare visuellement les distances originales (axe x) aux distances dans l'espace
#' MDS (axe y). Un bon ajustement donne des points proches de la diagonale.
#' Utilise base::startsWith("Dim.") pour éviter les problèmes d'échappement regex.
mds_shepard_plot <- function(dist_obj, coords_df, verbose = FALSE) {
  if (!inherits(dist_obj, "dist")) base::stop("dist_obj doit être un objet 'dist'.")
  cn <- base::colnames(coords_df)
  dim_cols <- cn[base::startsWith(cn, "Dim.")]
  if (length(dim_cols) < 2) base::stop("coords_df doit contenir au moins deux colonnes débutant par 'Dim.' (ex: Dim.1, Dim.2).")
  
  d_orig <- base::as.numeric(dist_obj)
  d_proj <- base::as.numeric(stats::dist(coords_df[, dim_cols, drop = FALSE]))
  
  df <- tibble::tibble(orig = d_orig, proj = d_proj)
  rmse <- base::sqrt(base::mean((df$proj - df$orig)^2))
  r <- stats::cor(df$orig, df$proj, method = "pearson")
  ttl <- base::sprintf("MDS — Shepard plot (RMSE=%.3f, r=%.3f)", rmse, r)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$orig, y = .data$proj)) +
    ggplot2::geom_point(alpha = 0.5, size = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::labs(title = ttl, x = "Distance originale", y = "Distance projetée") +
    ggplot2::theme_minimal(base_size = 12)
  
  if (isTRUE(verbose)) base::message("[MDS] Shepard plot prêt (n=", nrow(df), ")")
  return(p)
}



-# -- R/metrics_plots.R — Matrice de confusion ----
# + tableau de métriques + courbes d'entraînement
# Conventions : Roxygen, tout namespacé (pkg::fun), PAS de library()/install.*
# verbose en DERNIER (FALSE), toutes les fonctions se terminent par return(...)

# 1) Métriques à partir d'une matrice de confusion (sans collision de nom)

#' Construit des métriques (accuracy, precision/recall/F1 par classe et macro)
#' @param cm table|matrix Matrice de confusion KxK (lignes = vérité, colonnes = prédiction)
#' @param verbose logical Affiche des messages (FALSE)
#' @return list Liste : accuracy (numeric), per_class_tbl (tibble), macro_tbl (tibble 1 ligne)
#' @details
#' Ne redéfinit pas `metrics_from_cm()` pour éviter les collisions ; cette
#' fonction de construction retourne un format compatible avec `metrics_table_plot()`.
metrics_build_from_cm <- function(cm, verbose = FALSE) {
  cm <- base::as.matrix(cm)
  if (is.null(base::rownames(cm)) || is.null(base::colnames(cm))) base::stop("cm doit avoir des dimnames.")
  if (!base::identical(base::rownames(cm), base::colnames(cm))) cm <- cm[, base::rownames(cm), drop = FALSE]
  
  classes <- base::rownames(cm)
  total <- base::sum(cm)
  acc <- base::sum(base::diag(cm)) / total
  
  per <- lapply(base::seq_along(classes), function(i) {
    TP <- cm[i, i]
    FP <- base::sum(cm[, i]) - TP
    FN <- base::sum(cm[i, ]) - TP
    TN <- total - TP - FP - FN
    precision   <- base::ifelse((TP + FP) == 0, base::NA_real_, TP / (TP + FP))
    recall      <- base::ifelse((TP + FN) == 0, base::NA_real_, TP / (TP + FN))
    specificity <- base::ifelse((TN + FP) == 0, base::NA_real_, TN / (TN + FP))
    denom <- precision + recall
    f1 <- if (base::is.na(precision) || base::is.na(recall) || base::isTRUE(denom == 0)) base::NA_real_ else 2 * precision * recall / denom
    tibble::tibble(class = classes[i], TP = TP, FP = FP, FN = FN, TN = TN,
                   precision = precision, recall = recall, specificity = specificity, f1 = f1)
  }) |> dplyr::bind_rows()
  
  macro_tbl <- tibble::tibble(
    macro_precision = base::mean(per$precision, na.rm = TRUE),
    macro_recall    = base::mean(per$recall,    na.rm = TRUE),
    macro_f1        = base::mean(per$f1,        na.rm = TRUE),
    weighted_f1     = stats::weighted.mean(per$f1, w = per$TP + per$FN, na.rm = TRUE)
  )
  
  if (isTRUE(verbose)) base::message(base::sprintf("[metrics] acc=%.4f, macro_f1=%.4f", acc, macro_tbl$macro_f1))
  out <- base::list(accuracy = acc, per_class_tbl = per, macro_tbl = macro_tbl)
  return(out)
}

# ---------------------------------------------------------------------
# 2) Heatmap avec ligne/colonne "sum" + double gradient SANS ggnewscale
# ---------------------------------------------------------------------

#' Étend une matrice de confusion en ajoutant ligne/colonne "sum" (format long)
#' @param cm table|matrix Matrice de confusion KxK
#' @param show_bottomright logical TRUE pour afficher la somme globale en (sum,sum)
#' @param verbose logical Messages (FALSE)
#' @return tibble Colonnes : Truth, Pred, Freq, is_sum (logical)
cm_with_sums_long <- function(cm, show_bottomright = FALSE, verbose = FALSE) {
  cm <- base::as.matrix(cm)
  classes <- base::colnames(cm)
  if (base::is.null(classes)) base::stop("cm doit avoir des colnames.")
  x_levels <- base::c(classes, "sum"); y_levels <- base::c(classes, "sum")
  
  core <- base::as.data.frame(stats::xtabs(Freq ~ Truth + Pred,
                                           data = stats::setNames(base::as.data.frame(base::as.table(cm)), c("Truth","Pred","Freq"))),
                              stringsAsFactors = FALSE)
  
  row_sum <- stats::aggregate(Freq ~ Truth, core, sum)
  row_sum$Pred <- "sum"; row_sum <- row_sum[, c("Truth","Pred","Freq")]
  
  col_sum <- stats::aggregate(Freq ~ Pred, core, sum)
  col_sum$Truth <- "sum"; col_sum <- col_sum[, c("Truth","Pred","Freq")]
  
  corner <- base::data.frame(Truth = "sum", Pred = "sum",
                             Freq = if (isTRUE(show_bottomright)) base::sum(cm) else base::NA_real_)
  
  df <- dplyr::bind_rows(core, row_sum, col_sum, corner)
  df <- tidyr::complete(df, Truth = y_levels, Pred = x_levels, fill = base::list(Freq = base::NA_real_))
  df$Truth <- base::factor(df$Truth, levels = y_levels)
  df$Pred  <- base::factor(df$Pred,  levels = x_levels)
  df$is_sum <- (df$Truth == "sum") | (df$Pred == "sum")
  if (isTRUE(verbose)) base::message("[cm] long: ", nrow(df), " lignes")
  return(tibble::as_tibble(df))
}

#' Heatmap double gradient (centre bleu, bords vert) sans ggnewscale
#' @param df_long tibble Sortie de cm_with_sums_long()
#' @param title character Titre
#' @param y_lab character Libellé axe Y
#' @param core_pal character(2) Palette centre (hex)
#' @param sum_pal character(2) Palette sums (hex)
#' @param verbose logical Messages (FALSE)
#' @return ggplot Graphique heatmap
viz_confusion_heatmap_sums <- function(df_long,
                                       title = "Matrice de confusion",
                                       y_lab  = "prédiction du modèle",
                                       core_pal = c("#cfe5f2", "#2b8cbe"),
                                       sum_pal  = c("#a1d99b", "#31a354"),
                                       verbose = FALSE) {
  core_df <- dplyr::filter(df_long, !.data$is_sum)
  sum_df  <- dplyr::filter(df_long,  .data$is_sum)
  
  # Générateurs de couleurs
  col_fun_core <- scales::col_numeric(core_pal, domain = c(0, 1), na.color = "#ffffff")
  col_fun_sum  <- scales::col_numeric(sum_pal,  domain = c(0, 1), na.color = "#ffffff")
  
  # Normalisation locale (par groupe)
  norm01 <- function(x) {
    rng <- base::range(x, na.rm = TRUE)
    if (!base::is.finite(rng[1]) || !base::is.finite(rng[2]) || rng[1] == rng[2]) return(base::ifelse(base::is.na(x), base::NA_real_, 1))
    return(scales::rescale(x, to = c(0, 1), from = rng))
  }
  
  if (nrow(core_df) > 0) core_df$fill_hex <- base::ifelse(base::is.na(core_df$Freq), "#ffffff", col_fun_core(norm01(core_df$Freq)))
  if (nrow(sum_df)  > 0) sum_df$fill_hex  <- base::ifelse(base::is.na(sum_df$Freq),  "#ffffff", col_fun_sum(norm01(sum_df$Freq)))
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = core_df, ggplot2::aes(x = .data$Pred, y = .data$Truth, fill = .data$fill_hex), color = "white", linewidth = 0.6) +
    ggplot2::geom_text(data = core_df, ggplot2::aes(x = .data$Pred, y = .data$Truth, label = base::ifelse(base::is.na(.data$Freq), "", .data$Freq)), size = 4.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_tile(data = sum_df,  ggplot2::aes(x = .data$Pred, y = .data$Truth, fill = .data$fill_hex), color = "white", linewidth = 0.6) +
    ggplot2::geom_text(data = sum_df,  ggplot2::aes(x = .data$Pred, y = .data$Truth, label = base::ifelse(base::is.na(.data$Freq), "", .data$Freq)), size = 4.2) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, x = NULL, y = y_lab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none", panel.grid = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(vjust = 0))
  
  if (isTRUE(verbose)) base::message("[cm] heatmap (double gradient) prête.")
  return(p)
}

# 3) Tableau des métriques + wrapper d'assemblage

#' Petit tableau ggplot des métriques globales (macro)
#' @param metrics_list list Sortie de metrics_build_from_cm() ou de metrics_from_cm()
#' @param digits integer Nombre de décimales
#' @param verbose logical Messages (FALSE)
#' @return ggplot Tableau sous forme de ggplot (via ggplotify)
metrics_table_plot <- function(metrics_list, digits = 2, verbose = FALSE) {
  # Rendre compatible avec deux schémas de noms possibles
  macro <- metrics_list$macro_tbl %||% metrics_list$macro
  acc   <- metrics_list$accuracy
  if (base::is.null(macro) || base::is.null(acc)) base::stop("metrics_list doit contenir 'accuracy' et macro(_tbl).")
  
  # Colonnes attendues (avec fallback sur différents noms)
  spec  <- macro$specificity %||% macro$Specificity %||% base::NA_real_
  rec   <- macro$macro_recall %||% macro$Recall %||% base::NA_real_
  f1    <- macro$macro_f1 %||% macro$F1 %||% base::NA_real_
  
  df_wide <- tibble::tibble(
    Specificity = spec,
    Sensitivity = rec,
    Accuracy    = acc,
    `F1 score`  = f1
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ base::sprintf(base::paste0("%.", digits, "f"), .x)))
  
  tbl <- gridExtra::tableGrob(
    df_wide, rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core   = base::list(fg_params = base::list(fontface = 1, just = "center")),
      colhead= base::list(fg_params = base::list(fontface = 2), bg_params = base::list(fill = "#e6e6e6", col = NA))
    )
  )
  # Colonnes largeur égale
  tbl$widths <- grid::unit(rep(1 / base::ncol(df_wide), base::ncol(df_wide)), "npc")
  p <- ggplotify::as.ggplot(tbl)
  if (isTRUE(verbose)) base::message("[cm] tableau métriques prêt.")
  return(p)
}

#' Assemble la heatmap et le tableau des métriques (sans opérateurs patchwork)
#' @param cm table|matrix Matrice de confusion KxK
#' @param metrics_list list Optionnel, sinon calculé via metrics_build_from_cm()
#' @param title character Titre du panneau
#' @param verbose logical Messages (FALSE)
#' @return patchwork Plot patchwork (via wrap_plots)

viz_confusion_with_sums_and_table <- function(cm, metrics_list = NULL, title = "Confusion Matrix (Test)", verbose = FALSE) {
  if (base::is.null(metrics_list)) metrics_list <- metrics_build_from_cm(cm, verbose = verbose)
  df_long <- cm_with_sums_long(cm, show_bottomright = FALSE, verbose = verbose)
  p_cm  <- viz_confusion_heatmap_sums(df_long, title = title, verbose = verbose)
  p_tbl <- metrics_table_plot(metrics_list, digits = 2, verbose = verbose)
  p <- patchwork::wrap_plots(p_cm, p_tbl, ncol = 1, heights = c(3, 1))
  return(p)
}

# -- 4) Historique Keras → data.frame + courbes d'entraînement ----

#' Convertit un objet history (keras3/reticulate) en data.frame
#' @param history list|object Historique de keras3::fit(...) ou objet python (reticulate)
#' @param verbose logical Messages (FALSE)
#' @return data.frame Tableau long des métriques par epoch
history_to_dataframe <- function(history, verbose = FALSE) {
  df <- NULL
  # Tentative reticulate si disponible et structure python
  if (base::requireNamespace("reticulate", quietly = TRUE)) {
    h <- base::try(reticulate::py_to_r(history$history), silent = TRUE)
    if (!inherits(h, "try-error") && base::is.list(h) && length(h) > 0) {
      df <- base::as.data.frame(h, optional = TRUE, check.names = FALSE)
      n  <- if (!base::is.null(df$loss)) base::length(df$loss) else base::max(base::lengths(h))
      df$epoch <- base::seq_len(n)
    }
  }
  # Fallbacks génériques
  if (base::is.null(df)) df <- base::try(base::as.data.frame(history), silent = TRUE)
  if (inherits(df, "try-error") || base::is.null(df) || base::ncol(df) == 0) base::stop("Aucune métrique trouvée dans 'history'.")
  if (!("epoch" %in% base::tolower(base::names(df)))) df$epoch <- base::seq_len(base::nrow(df))
  
  # Alias utiles
  if ("sparse_categorical_accuracy" %in% base::names(df) && !("accuracy" %in% base::names(df))) df$accuracy <- df$sparse_categorical_accuracy
  if ("val_sparse_categorical_accuracy" %in% base::names(df) && !("val_accuracy" %in% base::names(df))) df$val_accuracy <- df$val_sparse_categorical_accuracy
  
  # Prévenir conflits de noms
  base::names(df) <- base::make.unique(base::names(df), sep = "_")
  return(df)
}

#' Courbes d'entraînement à partir d'un objet history
#' @param history list|object Historique Keras (keras3::fit) ou Python
#' @param title character Titre du graphique
#' @param verbose logical Messages (FALSE)
#' @return ggplot Courbes (train/val) pour toutes les métriques numériques
history_plot <- function(history, title = "Training curves", verbose = FALSE) {
  df <- history_to_dataframe(history, verbose = verbose)
  num_cols <- base::names(df)[vapply(df, is.numeric, logical(1))]
  metric_cols <- base::setdiff(num_cols, "epoch")
  if (length(metric_cols) == 0L) base::stop("Aucune métrique numérique à tracer.")
  
  long <- tidyr::pivot_longer(df, cols = dplyr::all_of(metric_cols), names_to = "metric_name", values_to = "metric_value") |>
    tidyr::drop_na(metric_value)
  
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$epoch, y = .data$metric_value,
                                          linetype = base::grepl("^val", .data$metric_name), group = .data$metric_name)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = "Epoch", y = "Value", linetype = "Validation") +
    ggplot2::theme_minimal(base_size = 13)
  return(p)
}

