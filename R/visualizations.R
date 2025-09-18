# -- R/visualizations.R — PCA, MDS et comparaisons ----------------------------

#' Analyse en composantes principales (PCA) avec cercle des corrélations
#'
#' @param x data.frame|matrix Données numériques
#' @param dims integer[2] Dimensions à tracer
#' @param group_factor factor|NULL Groupes (couleurs)
#' @param scale_unit logical Standardiser
#' @param ellipse logical Ajouter ellipses
#' @param ellipse_level numeric Niveau de confiance des ellipses
#' @param verbose logical
#'
#' @return patchwork::wrap_plots individus + cercle corrélations
#' @export
pca_plot <- function(x,
                     dims = c(1, 2),
                     group_factor = NULL,
                     scale_unit = TRUE,
                     ellipse = FALSE,
                     ellipse_level = 0.95,
                     verbose = FALSE) {
  res <- FactoMineR::PCA(x, scale.unit = scale_unit, graph = FALSE)
  eig <- res$eig
  coords <- base::as.data.frame(res$ind$coord)
  coords$group <- if (!is.null(group_factor)) base::as.factor(group_factor) else "All"
  
  # Plot individus
  p_ind <- ggplot2::ggplot(coords,
                           ggplot2::aes(x = .data[[paste0("Dim.", dims[1])]],
                                        y = .data[[paste0("Dim.", dims[2])]],
                                        color = group)) +
    ggplot2::geom_point(size = 2) +
    {if (ellipse && !is.null(group_factor))
      ggplot2::stat_ellipse(level = ellipse_level)} +
    ggplot2::labs(title = "PCA — individus",
                  x = paste0("Dim ", dims[1], " (", round(eig[dims[1], 2], 1), "%)"),
                  y = paste0("Dim ", dims[2], " (", round(eig[dims[2], 2], 1), "%)")) +
    ggplot2::theme_minimal()
  
  # Coordonnées des variables
  var_coords <- base::as.data.frame(res$var$coord)
  
  if (nrow(var_coords) == 0) {
    p_var <- ggplot2::ggplot() +
      ggplot2::labs(title = "Cercle des corrélations (aucune variable)") +
      ggplot2::theme_void()
  } else {
    # Cercle unité (x² + y² = 1)
    circle <- data.frame(x = cos(seq(0, 2*pi, length.out = 200)),
                         y = sin(seq(0, 2*pi, length.out = 200)))
    
    p_var <- ggplot2::ggplot() +
      # Cercle unité
      ggplot2::geom_path(data = circle, ggplot2::aes(x = x, y = y),
                         color = "gray50", linetype = "dashed") +
      # Flèches (pointe sur les coordonnées des variables)
      ggplot2::geom_segment(data = var_coords,
                            ggplot2::aes(x = 0, y = 0,
                                         xend = .data[[paste0("Dim.", dims[1])]],
                                         yend = .data[[paste0("Dim.", dims[2])]]),
                            arrow = ggplot2::arrow(length = grid::unit(0.25, "cm"),
                                                   type = "closed"),
                            color = "black") +
      # Labels variables
      ggplot2::geom_text(data = var_coords,
                         ggplot2::aes(x = .data[[paste0("Dim.", dims[1])]],
                                      y = .data[[paste0("Dim.", dims[2])]],
                                      label = rownames(var_coords)),
                         hjust = 0.5, vjust = -0.8, color = "black") +
      ggplot2::labs(title = "Cercle des corrélations") +
      ggplot2::coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +  # cercle 1:1
      ggplot2::theme_minimal()
  }
  
  
  return(patchwork::wrap_plots(p_ind, p_var))
}



#' MultiDimensional Scaling (MDS) avec stress de Kruskal
#'
#' @param x matrix|data.frame Données ou matrice de distances
#' @param method character Méthode de distance (par défaut "euclidean")
#' @param k integer Dimension réduite (2 recommandé)
#' @param group_factor factor|NULL Groupes pour colorier
#' @param title character|NULL Titre du graphique
#' @param verbose logical Affiche des infos
#'
#' @return ggplot2::ggplot scatter plot avec stress
#' @references Kruskal JB, Wish M (1978). Multidimensional Scaling. Sage Publications.
#'   Voir aussi: https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-019-2780-y
#' @examples
#' p <- mds_plot(iris[,1:4], group_factor = iris$Species)
#' @export
mds_plot <- function(x,
                     method = "euclidean",
                     k = 2,
                     group_factor = NULL,
                     title = NULL,
                     verbose = FALSE) {
  if (base::inherits(x, "dist")) {
    dist_mat <- x
  } else {
    dist_mat <- stats::dist(x, method = method)
  }
  
  # ✅ Correction : extraire uniquement $points
  mds_res <- stats::cmdscale(dist_mat, k = k, eig = TRUE)
  coords  <- base::as.data.frame(mds_res$points)
  colnames(coords)[1:2] <- c("Dim1", "Dim2")
  coords$group <- if (!base::is.null(group_factor)) base::as.factor(group_factor) else "All"
  
  # Stress (Kruskal)
  MASS <- utils::getFromNamespace("isoMDS", "MASS")
  stress_val <- tryCatch({
    MASS(dist_mat, y = coords[, 1:2], k = k)$stress
  }, error = function(e) NA_real_)
  stress_label <- if (!is.na(stress_val)) sprintf("Stress = %.4g", stress_val) else NULL
  
  p <- ggplot2::ggplot(coords,
                       ggplot2::aes(x = Dim1, y = Dim2, color = group)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = ifelse(is.null(title), "MDS", title),
                  subtitle = stress_label) +
    ggplot2::theme_minimal()
  
  if (verbose) message("MDS terminé avec ", nrow(coords), " points ; ", stress_label)
  return(p)
}


#' Shepard plot (distances originales vs réduites)
#'
#' @param dist_orig dist|matrix Distances originales
#' @param coords matrix Coordonnées réduites (2D)
#' @param method character Méthode de corrélation ("pearson","spearman")
#' @param verbose logical
#'
#' @return ggplot2::ggplot
#' @examples
#' \dontrun{
#' d <- dist(iris[,1:4])
#' coords <- cmdscale(d, k=2)
#' p <- shepard_plot(d, coords)
#' }
#' @export
shepard_plot <- function(dist_orig,
                         coords,
                         method = "pearson",
                         verbose = FALSE) {
  if (base::inherits(dist_orig, "dist")) {
    d_o <- base::as.vector(dist_orig)
  } else {
    d_o <- base::as.vector(stats::dist(dist_orig))
  }
  d_r <- base::as.vector(stats::dist(coords))
  df <- tibble::tibble(orig = d_o, reduced = d_r)
  corr_val <- stats::cor(df$orig, df$reduced, method = method, use = "pairwise.complete.obs")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = orig, y = reduced)) +
    ggplot2::geom_point(alpha = 0.6, size = 1) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggplot2::labs(title = "Shepard plot",
                  subtitle = base::paste0("Correlation (", method, ") = ", round(corr_val, 3)),
                  x = "Distance originale",
                  y = "Distance réduite") +
    ggplot2::theme_minimal()
  if (verbose) base::message("Corrélation Shepard : ", round(corr_val, 3))
  return(p)
}