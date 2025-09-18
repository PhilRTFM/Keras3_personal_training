# -- R/eval_sup.R — Évaluation des modèles supervisés -------------------------

#' Heatmap double gradient (centre bleu, marges vertes)
#'
#' @param df_long tibble Colonnes Truth, Pred, Freq, is_sum
#' @param title character Titre
#' @param y_lab character Libellé Y
#' @param core_pal character(2) Palette cœur (min->max)
#' @param sum_pal character(2) Palette marges (min->max)
#' @param verbose logical
#'
#' @return ggplot2::ggplot
#' @export
viz_confusion_heatmap_sums <- function(df_long,
                                       title = "Matrice de confusion",
                                       y_lab = "prédiction du modèle",
                                       core_pal = c("#cfe5f2", "#2b8cbe"),
                                       sum_pal = c("#a1d99b", "#31a354"),
                                       verbose = FALSE) {
  core_df <- dplyr::filter(df_long, !is_sum)
  sum_df  <- dplyr::filter(df_long, is_sum)
  
  col_fun_core <- scales::col_numeric(core_pal, domain = c(0, 1), na.color = "#ffffff")
  col_fun_sum  <- scales::col_numeric(sum_pal,  domain = c(0, 1), na.color = "#ffffff")
  
  norm01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
      return(ifelse(is.na(x), NA_real_, 1))
    }
    scales::rescale(x, to = c(0, 1), from = rng)
  }
  
  if (nrow(core_df) > 0) {
    core_df$fill_hex <- ifelse(is.na(core_df$Freq), "#ffffff", col_fun_core(norm01(core_df$Freq)))
  }
  if (nrow(sum_df) > 0) {
    sum_df$fill_hex  <- ifelse(is.na(sum_df$Freq), "#ffffff", col_fun_sum(norm01(sum_df$Freq)))
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = core_df,
                       ggplot2::aes(x = Pred, y = Truth, fill = fill_hex),
                       color = "white", linewidth = 0.6) +
    ggplot2::geom_text(data = core_df,
                       ggplot2::aes(x = Pred, y = Truth, label = ifelse(is.na(Freq), "", Freq)),
                       size = 4.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_tile(data = sum_df,
                       ggplot2::aes(x = Pred, y = Truth, fill = fill_hex),
                       color = "white", linewidth = 0.6) +
    ggplot2::geom_text(data = sum_df,
                       ggplot2::aes(x = Pred, y = Truth, label = ifelse(is.na(Freq), "", Freq)),
                       size = 4.2) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, x = NULL, y = y_lab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none",
                   panel.grid = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(vjust = 0))
  
  if (isTRUE(verbose)) message("[cm] heatmap (double gradient) prête.")
  return(p)
}

#' Tableau ggplot des métriques globales (macro)
#'
#' @param metrics_list list Contient accuracy + macro_tbl
#' @param digits integer Décimales
#' @param verbose logical
#' @return ggplot2::ggplot
#' @export
metrics_table_plot <- function(metrics_list, digits = 2, verbose = FALSE) {
  macro <- metrics_list$macro_tbl %||% metrics_list$macro
  acc   <- metrics_list$accuracy
  
  spec <- macro$macro_specificity %||% macro$specificity %||% macro$Specificity %||% NA_real_
  rec  <- macro$macro_recall      %||% macro$recall      %||% macro$Recall      %||% NA_real_
  f1   <- macro$macro_f1          %||% macro$F1          %||% NA_real_
  
  df_wide <- tibble::tibble(
    Specificity = spec,
    Sensitivity = rec,
    Accuracy    = acc,
    `F1 score`  = f1
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~ sprintf(paste0("%.", digits, "f"), .x)))
  
  tbl <- gridExtra::tableGrob(
    df_wide, rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core = list(fg_params = list(fontface = 1, just = "center")),
      colhead = list(fg_params = list(fontface = 2),
                     bg_params = list(fill = "#e6e6e6", col = NA))
    )
  )
  tbl$widths <- grid::unit(rep(1 / ncol(df_wide), ncol(df_wide)), "npc")
  p <- ggplotify::as.ggplot(tbl)
  
  if (isTRUE(verbose)) message("[metrics_table_plot] OK")
  return(p)
}

#' Panel confusion + tableau des métriques
#'
#' @param y_true factor|character|integer Vérités terrain
#' @param y_pred factor|character|integer Prédictions
#' @param labels character Ordre des classes
#' @param title character Titre du panneau
#' @param digits integer Décimales
#' @param show_bottomright logical Affiche la somme globale
#' @param verbose logical
#'
#' @return patchwork::patchwork Panel combiné
#' @export
viz_confusion_panel_from_predictions <- function(y_true,
                                                 y_pred,
                                                 labels = NULL,
                                                 title = "Confusion Matrix (Test)",
                                                 digits = 2L,
                                                 show_bottomright = FALSE,
                                                 verbose = FALSE) {
  stopifnot(length(y_true) == length(y_pred))
  
  labs <- if (is.null(labels)) {
    sort(unique(c(as.character(y_true), as.character(y_pred))))
  } else {
    as.character(labels)
  }
  
  y_true_f <- factor(as.character(y_true), levels = labs)
  y_pred_f <- factor(as.character(y_pred), levels = labs)
  cm <- table(Truth = y_true_f, Pred = y_pred_f) |> as.matrix()
  total <- sum(cm)
  acc   <- if (total > 0) sum(diag(cm)) / total else NA_real_
  
  per <- lapply(seq_along(rownames(cm)), function(i) {
    TP <- cm[i, i]; FP <- sum(cm[, i]) - TP
    FN <- sum(cm[i, ]) - TP; TN <- total - TP - FP - FN
    precision   <- ifelse((TP + FP) == 0, NA_real_, TP / (TP + FP))
    recall      <- ifelse((TP + FN) == 0, NA_real_, TP / (TP + FN))
    specificity <- ifelse((TN + FP) == 0, NA_real_, TN / (TN + FP))
    f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else NA_real_
    tibble::tibble(class = rownames(cm)[i],
                   TP = TP, FP = FP, FN = FN, TN = TN,
                   precision = precision, recall = recall,
                   specificity = specificity, f1 = f1)
  }) |> dplyr::bind_rows()
  
  macro_tbl <- tibble::tibble(
    macro_precision   = mean(per$precision,   na.rm = TRUE),
    macro_recall      = mean(per$recall,      na.rm = TRUE),
    macro_specificity = mean(per$specificity, na.rm = TRUE),
    macro_f1          = mean(per$f1,          na.rm = TRUE),
    weighted_f1       = stats::weighted.mean(per$f1, w = per$TP + per$FN, na.rm = TRUE)
  )
  
  metrics_list <- list(accuracy = acc, per_class_tbl = per, macro_tbl = macro_tbl)
  
  classes <- colnames(cm)
  x_levels <- c(classes, "sum")
  y_levels <- c(classes, "sum")
  
  core <- as.data.frame(as.table(cm))
  names(core) <- c("Truth", "Pred", "Freq")
  
  row_sum <- aggregate(Freq ~ Truth, core, sum)
  row_sum$Pred <- "sum"
  
  col_sum <- aggregate(Freq ~ Pred, core, sum)
  col_sum$Truth <- "sum"
  
  corner <- data.frame(Truth = "sum", Pred = "sum",
                       Freq = if (isTRUE(show_bottomright)) sum(cm) else NA_real_)
  
  df_long <- dplyr::bind_rows(core, row_sum, col_sum, corner) |>
    tidyr::complete(Truth = y_levels, Pred = x_levels, fill = list(Freq = NA_real_)) |>
    dplyr::mutate(Truth = factor(Truth, levels = y_levels),
                  Pred  = factor(Pred,  levels = x_levels),
                  is_sum = Truth == "sum" | Pred == "sum")
  
  if (isTRUE(verbose)) message("[cm-panel] df_long: ", nrow(df_long), " lignes")
  
  p_cm  <- viz_confusion_heatmap_sums(df_long, title = title, verbose = verbose)
  p_tbl <- metrics_table_plot(metrics_list, digits = digits, verbose = verbose)
  p     <- patchwork::wrap_plots(p_cm, p_tbl, ncol = 1, heights = c(3, 1))
  return(p)
}