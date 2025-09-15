# -- R/analysis.R : PCA | MDS -------------------------------------------------
# Conventions : roxygen, pkg::fun(), verbose en dernier (FALSE), return() partout

# ================================ PCA ========================================

#' PCA : deux plots côte à côte (individus + cercle des corrélations)
#'
#' @param x data.frame|matrix, données numériques (lignes = individus)
#' @param dims integer, dimensions à tracer (longueur 2)
#' @param group_factor factor|NULL, couleur des individus (facultatif)
#' @param scale_unit logical, standardiser (TRUE recommandé)
#' @param ellipse logical, ellipses de groupes (FALSE)
#' @param ellipse_level numeric, niveau des ellipses (0.95)
#' @param verbose logical, messages (FALSE)
#'
#' @return patchwork (wrap_plots), deux plots côte à côte
pca_plot <- function(x, dims = c(1, 2), group_factor = NULL,
                     scale_unit = TRUE, ellipse = FALSE, ellipse_level = 0.95,
                     verbose = FALSE) {
  if (isTRUE(verbose)) base::message("[PCA] Calcul de l'ACP…")
  
  pca <- FactoMineR::PCA(base::as.data.frame(x), scale.unit = scale_unit, graph = FALSE)
  
  # --- Individus --------------------------------------------------------------
  ind_coords <- pca$ind$coord[, dims, drop = FALSE]
  df_ind <- tibble::tibble(
    label = base::rownames(ind_coords),
    Dim.1 = ind_coords[, 1],
    Dim.2 = ind_coords[, 2]
  )
  if (!base::is.null(group_factor)) df_ind$group <- base::as.factor(group_factor)
  
  # mapping de base (sans couleur)
  p_ind <- ggplot2::ggplot(df_ind, ggplot2::aes(x = .data$Dim.1, y = .data$Dim.2)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), hjust = 0, vjust = 1.2,
                       size = 3, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "PCA — individus",
                  x = base::paste0("Dim ", dims[1]), y = base::paste0("Dim ", dims[2])) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::coord_equal()
  
  # couleur par groupe si présent
  if ("group" %in% base::colnames(df_ind)) {
    p_ind <- p_ind + ggplot2::aes(color = .data$group)
    if (isTRUE(ellipse)) {
      p_ind <- p_ind + ggplot2::stat_ellipse(ggplot2::aes(group = .data$group),
                                             level = ellipse_level)
    }
  }
  
  # --- Variables --------------------------------------------------------------
  var_coords <- pca$var$coord[, dims, drop = FALSE]
  df_var <- tibble::tibble(
    var = base::rownames(var_coords),
    Dim.1 = var_coords[, 1],
    Dim.2 = var_coords[, 2]
  )
  
  circle <- tibble::tibble(
    t = base::seq(0, 2 * base::pi, length.out = 200),
    x = base::cos(t),
    y = base::sin(t)
  )
  
  p_var <- ggplot2::ggplot() +
    ggplot2::geom_path(data = circle, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_segment(data = df_var,
                          ggplot2::aes(x = 0, y = 0, xend = .data$Dim.1, yend = .data$Dim.2),
                          arrow = ggplot2::arrow(length = grid::unit(0.02, "npc"))) +
    ggplot2::geom_text(data = df_var,
                       ggplot2::aes(x = .data$Dim.1, y = .data$Dim.2, label = .data$var),
                       hjust = 0, vjust = -0.5, size = 3) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "PCA — cercle des corrélations",
                  x = base::paste0("Dim ", dims[1]), y = base::paste0("Dim ", dims[2])) +
    ggplot2::coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    ggplot2::theme_minimal(base_size = 12)
  
  # --- Combinaison SANS charger patchwork dans le search path -----------------
  # Utilise wrap_plots pour éviter l'opérateur '+' de patchwork.
  p_combined <- patchwork::wrap_plots(p_ind, p_var, ncol = 2, widths = c(1, 1))
  
  if (isTRUE(verbose)) base::message("[PCA] Plot généré")
  return(p_combined)
}

# ================================ MDS ========================================

#' Analyse MDS : configuration + diagramme de Shepard (RMSE & r)
#'
#' @param x matrix|data.frame, données numériques (lignes = objets)
#' @param method character, méthode de distance ("euclidean", etc.)
#' @param metric logical, MDS métrique (TRUE) ou non-métrique (FALSE)
#' @param k integer, nombre de dimensions projetées (2)
#' @param labels character|NULL, étiquettes des points (sinon rownames)
#' @param group_factor factor|NULL, groupe pour coloration des points
#' @param title character|NULL, titre principal
#' @param verbose logical, messages (FALSE)
#'
#' @return patchwork (wrap_plots) : MDS + Shepard
mds_plot <- function(x, method = "euclidean", k = 2,
                                 labels = NULL, group_factor = NULL,
                                 title = NULL, verbose = FALSE) {
  if (isTRUE(verbose)) message("[MDS] Calcul des distances : ", method)
  dist_obj <- stats::dist(x = x, method = method)
  
  if (isTRUE(verbose)) message("[MDS] MDS métrique (cmdscale)")
  fit <- stats::cmdscale(d = dist_obj, k = k, eig = TRUE)
  coords <- tibble::as_tibble(fit$points, .name_repair = "minimal")
  colnames(coords) <- paste0("Dim.", seq_len(ncol(coords)))
  
  if (is.null(labels)) labels <- rownames(x)
  coords$label <- labels
  if (!is.null(group_factor)) coords$group <- as.factor(group_factor)
  
  # Calcul du "stress de Kruskal" (1 - r^2)
  d_orig <- as.numeric(dist_obj)
  d_proj <- as.numeric(dist(coords[, startsWith(names(coords), "Dim.")]))
  r <- cor(d_orig, d_proj)
  stress <- 1 - r^2
  
  if (verbose) message("[MDS] Stress de Kruskal : ", round(stress, 4))
  
  title_plot <- if (is.null(title)) {
    "MDS métrique"
  } else {
    paste("MDS —", title)
  }
  
  # Texte annoté
  stress_label <- sprintf("Stress = %.3f", stress)
  
  # Plot
  p <- ggplot2::ggplot(coords, ggplot2::aes(x = .data$Dim.1, y = .data$Dim.2)) +
    ggplot2::geom_point(ggplot2::aes(color = group)) +
    ggplot2::geom_text(ggplot2::aes(label = label), hjust = 0, vjust = -0.5, size = 3, na.rm = TRUE, show.legend = FALSE) +
    ggplot2::labs(title = title_plot, x = "Dim 1", y = "Dim 2", color = "Groupe") +
    ggplot2::annotate("text",
                      x = max(coords$Dim.1, na.rm = TRUE),
                      y = min(coords$Dim.2, na.rm = TRUE),
                      hjust = 1, vjust = 0,
                      label = stress_label,
                      size = 8) +
    ggplot2::theme_minimal(base_size = 12)
  
  
  return(p)
}

#' Scatter plot comparant les distances entre deux matrices
#'
#' @param x_ref (matrix) matrice de référence (originale)
#' @param x_test (matrix) matrice test à comparer (ex: reconstruite)
#' @param title (character) titre du graphique
#' @return (ggplot) scatter plot avec diagonale idéale et métriques
scatter_distances <- function(x_ref, x_test, title = "Comparaison des distances") {
  d_ref  <- as.numeric(stats::dist(x_ref))
  d_test <- as.numeric(stats::dist(x_test))
  
  rmse <- sqrt(mean((d_test - d_ref)^2))
  r <- cor(d_ref, d_test)
  
  df <- tibble::tibble(dist_ref = d_ref, dist_test = d_test)
  
  ggplot2::ggplot(df, ggplot2::aes(x = dist_ref, y = dist_test)) +
    ggplot2::geom_point(alpha = 0.6, size = 0.8) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6) +
    ggplot2::annotate("text",
                      x = max(d_ref, na.rm = TRUE),
                      y = min(d_test, na.rm = TRUE),
                      hjust = 1, vjust = 0,
                      label = sprintf("RMSE = %.3f\nr = %.3f", rmse, r),
                      size = 3) +
    ggplot2::labs(
      title = title,
      x = "Distance — IRIS brut",
      y = "Distance — IRIS reconstruit"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
