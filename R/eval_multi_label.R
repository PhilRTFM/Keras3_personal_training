# -- R/eval_multi_label.R ------------------------------------------------------

#' Heatmap de confusion multilabel
#'
#' @param y_true matrix Vrais labels binarisés (n x k)
#' @param y_pred matrix Prédictions binarisées (n x k)
#' @param class_names character[] noms des classes
#'
#' @return ggplot2::ggplot heatmap
#' @export
multilabel_confusion_heatmap <- function(y_true, y_pred, class_names) {
  cm <- colSums((y_true == 1 & y_pred == 1))
  fn <- colSums((y_true == 1 & y_pred == 0))
  fp <- colSums((y_true == 0 & y_pred == 1))
  tn <- colSums((y_true == 0 & y_pred == 0))
  
  df <- data.frame(
    Class = rep(class_names, each = 4),
    Metric = rep(c("TP", "FN", "FP", "TN"), times = length(class_names)),
    Value = c(cm, fn, fp, tn)
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Class, y = Metric, fill = Value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = Value)) +
    ggplot2::scale_fill_gradient(low = "#fee0d2", high = "#de2d26") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.ticks  = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = "Matrice de confusion multilabel")
  return(p)
}

#' Calcul métriques multilabel (macro, micro)
#'
#' @param y_true matrix binarisée
#' @param y_pred matrix binarisée
#'
#' @return data.frame avec précision, rappel, F1 (micro/macro)
#' @export
multilabel_metrics <- function(y_true, y_pred) {
  tp <- sum(y_true == 1 & y_pred == 1)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)
  tn <- sum(y_true == 0 & y_pred == 0)
  
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1        <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
  
  data.frame(
    Metric = c("Precision", "Recall", "F1"),
    Micro  = c(precision, recall, f1),
    Macro  = c(precision, recall, f1) # version simplifiée
  )
}

#' Affichage tableau métriques multilabel
#'
#' @param metrics data.frame sortie multilabel_metrics
#'
#' @return ggplotify::as.ggplot
#' @export
metrics_table_multilabel <- function(metrics) {
  tbl <- gridExtra::tableGrob(
    metrics,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core = list(fg_params = list(fontface = 1)),
      colhead = list(fg_params = list(fontface = 2),
                     bg_params = list(fill = "#e6e6e6"))
    )
  )
  return(ggplotify::as.ggplot(tbl))
}
