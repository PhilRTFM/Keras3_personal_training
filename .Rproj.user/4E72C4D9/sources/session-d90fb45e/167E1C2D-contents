# -- R/metrics.R — Matrice de confusion + métriques & courbes d'entraînement ----
# Conventions : roxygen, tout namespacé (pkg::fun), PAS de library()/install.*
# verbose en DERNIER (FALSE), toutes les fonctions se terminent par return(...)

# Dépendances suggérées : ggplot2, dplyr, tidyr, tibble, scales, gridExtra,
# ggplotify, patchwork. Aucune n'est attachée via library().


#--  1) Heatmap double gradient (centre vs marges) ----

#' Heatmap double gradient (centre bleu, bords vert) sans ggnewscale
#'
#' @param df_long tibble Sortie d'une fonction interne cm_with_sums_long(),
#'   colonnes attendues : Truth, Pred, Freq, is_sum (logique)
#' @param title character Titre du graphique ("Matrice de confusion")
#' @param y_lab character Libellé de l'axe Y ("prédiction du modèle")
#' @param core_pal character(2) Palette hex du coeur (min->max)
#' @param sum_pal character(2) Palette hex des marges (min->max)
#' @param verbose logical Messages (FALSE)
#' @return ggplot2::ggplot
viz_confusion_heatmap_sums <- function(df_long,
                                       title = "Matrice de confusion",
                                       y_lab = "prédiction du modèle",
                                       core_pal = c("#cfe5f2", "#2b8cbe"),
                                       sum_pal = c("#a1d99b", "#31a354"),
                                       verbose = FALSE) {
  core_df <- dplyr::filter(df_long, !is_sum)
  sum_df <- dplyr::filter(df_long, is_sum)
  
  col_fun_core <- scales::col_numeric(core_pal, domain = c(0, 1), na.color = "#ffffff")
  col_fun_sum <- scales::col_numeric(sum_pal, domain = c(0, 1), na.color = "#ffffff")
  
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
    sum_df$fill_hex <- ifelse(is.na(sum_df$Freq), "#ffffff", col_fun_sum(norm01(sum_df$Freq)))
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = core_df,
      ggplot2::aes(x = Pred, y = Truth, fill = fill_hex),
      color = "white", linewidth = 0.6
    ) +
    ggplot2::geom_text(
      data = core_df,
      ggplot2::aes(x = Pred, y = Truth, label = ifelse(is.na(Freq), "", Freq)),
      size = 4.2
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_tile(
      data = sum_df,
      ggplot2::aes(x = Pred, y = Truth, fill = fill_hex),
      color = "white", linewidth = 0.6
    ) +
    ggplot2::geom_text(
      data = sum_df,
      ggplot2::aes(x = Pred, y = Truth, label = ifelse(is.na(Freq), "", Freq)),
      size = 4.2
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, x = NULL, y = y_lab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(vjust = 0)
    )
  
  if (isTRUE(verbose)) message("[cm] heatmap (double gradient) prête.")
  
  return(p)
}

# --  2) Tableau des métriques (macro) ----

#' Tableau ggplot des métriques globales (macro)
#'
#' @param metrics_list list Contient `accuracy` et `macro_tbl` ou `macro`
#' @param digits integer Nombre de décimales (2)
#' @param verbose logical Messages (FALSE)
#' @return ggplot2::ggplot
metrics_table_plot <- function(metrics_list, digits = 2, verbose = FALSE) {
  stopifnot(!is.null(metrics_list))
  
  macro <- metrics_list$macro_tbl
  if (is.null(macro)) macro <- metrics_list$macro
  acc <- metrics_list$accuracy
  
  if (is.null(macro) || is.null(acc)) {
    stop("metrics_list doit contenir 'accuracy' et macro(_tbl).")
  }
  
  # Récupérations tolérantes (collent aux noms utilisés dans tes autres funcs)
  spec <- if (!is.null(macro$macro_specificity)) {
    macro$macro_specificity
  } else if (!is.null(macro$specificity)) {
    macro$specificity
  } else if (!is.null(macro$Specificity)) {
    macro$Specificity
  } else {
    NA_real_
  }
  
  rec <- if (!is.null(macro$macro_recall)) {
    macro$macro_recall
  } else if (!is.null(macro$recall)) {
    macro$recall
  } else if (!is.null(macro$Recall)) {
    macro$Recall
  } else {
    NA_real_
  }
  
  f1 <- if (!is.null(macro$macro_f1)) {
    macro$macro_f1
  } else if (!is.null(macro$F1)) {
    macro$F1
  } else {
    NA_real_
  }
  
  df_wide <- tibble::tibble(
    Specificity = spec,
    Sensitivity = rec,
    Accuracy    = acc,
    `F1 score`  = f1
  )
  # formatage
  df_wide <- dplyr::mutate(df_wide,
                           dplyr::across(dplyr::everything(), ~ sprintf(paste0("%.", digits, "f"), .x))
  )
  
  if (!requireNamespace("gridExtra", quietly = TRUE) ||
      !requireNamespace("ggplotify", quietly = TRUE)) {
    stop("Installez 'gridExtra' et 'ggplotify' (install.packages(c('gridExtra','ggplotify'))).")
  }
  
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
  
  if (isTRUE(verbose)) base::message("[metrics_table_plot] OK")
  return(p)
}


# --  3) Panel confusion + métriques ----

#' Panneau complet : matrice de confusion + tableau des métriques
#'
#' @param y_true factor|character|integer Vérités terrain
#' @param y_pred factor|character|integer Prédictions
#' @param labels character(NULL) Ordre des classes
#' @param title character Titre du panneau
#' @param digits integer Décimales
#' @param show_bottomright logical Affiche la somme globale en (sum,sum)
#' @param verbose logical Messages (FALSE)
#' @return patchwork::wrap_plots( heatmap, tableau )
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
  acc <- if (total > 0) sum(diag(cm)) / total else NA_real_
  
  per <- lapply(seq_along(rownames(cm)), function(i) {
    TP <- cm[i, i]
    FP <- sum(cm[, i]) - TP
    FN <- sum(cm[i, ]) - TP
    TN <- total - TP - FP - FN
    
    precision <- ifelse((TP + FP) == 0, NA_real_, TP / (TP + FP))
    recall <- ifelse((TP + FN) == 0, NA_real_, TP / (TP + FN))
    specificity <- ifelse((TN + FP) == 0, NA_real_, TN / (TN + FP))
    
    denom <- precision + recall
    f1 <- if (is.na(precision) || is.na(recall) || isTRUE(denom == 0)) {
      NA_real_
    } else {
      2 * precision * recall / denom
    }
    
    tibble::tibble(
      class = rownames(cm)[i],
      TP = TP, FP = FP, FN = FN, TN = TN,
      precision = precision, recall = recall,
      specificity = specificity, f1 = f1
    )
  }) |> dplyr::bind_rows()
  
  macro_tbl <- tibble::tibble(
    macro_precision = mean(per$precision, na.rm = TRUE),
    macro_recall = mean(per$recall, na.rm = TRUE),
    macro_f1 = mean(per$f1, na.rm = TRUE),
    weighted_f1 = stats::weighted.mean(per$f1, w = per$TP + per$FN, na.rm = TRUE)
  )
  
  metrics_list <- list(accuracy = acc, per_class_tbl = per, macro_tbl = macro_tbl)
  
  classes <- colnames(cm)
  x_levels <- c(classes, "sum")
  y_levels <- c(classes, "sum")
  
  core <- as.data.frame(
    xtabs(Freq ~ Truth + Pred, data = setNames(as.data.frame(as.table(cm)), c("Truth", "Pred", "Freq"))),
    stringsAsFactors = FALSE
  )
  
  row_sum <- aggregate(Freq ~ Truth, core, sum)
  row_sum$Pred <- "sum"
  row_sum <- row_sum[, c("Truth", "Pred", "Freq")]
  
  col_sum <- aggregate(Freq ~ Pred, core, sum)
  col_sum$Truth <- "sum"
  col_sum <- col_sum[, c("Truth", "Pred", "Freq")]
  
  corner <- data.frame(
    Truth = "sum", Pred = "sum",
    Freq = if (isTRUE(show_bottomright)) sum(cm) else NA_real_
  )
  
  df_long <- dplyr::bind_rows(core, row_sum, col_sum, corner) |>
    tidyr::complete(Truth = y_levels, Pred = x_levels, fill = list(Freq = NA_real_)) |>
    dplyr::mutate(
      Truth = factor(Truth, levels = y_levels),
      Pred = factor(Pred, levels = x_levels),
      is_sum = Truth == "sum" | Pred == "sum"
    )
  
  if (isTRUE(verbose)) message("[cm-panel] df_long: ", nrow(df_long), " lignes")
  
  p_cm <- viz_confusion_heatmap_sums(df_long, title = title, verbose = verbose)
  p_tbl <- metrics_table_plot(metrics_list, digits = digits, verbose = verbose)
  p <- patchwork::wrap_plots(p_cm, p_tbl, ncol = 1, heights = c(3, 1))
  
  return(p)
}

# --  4) Courbes d'entraînement (history Keras) ----

#' Courbes d'entraînement (loss & accuracy) depuis un objet history keras
#'
#' @param history list|python object, typiquement keras3::fit()
#' @param title character Titre du graphique
#' @param metrics character  Métriques c("loss", "accuracy")
#' @param smooth logical  Lissage LOESS (FALSE)
#' @param verbose logical Messages (FALSE)
#' @return ggplot2::ggplot
history_curves_plot <- function(history,
                                title   = "Training curves",
                                metrics = c("loss", "accuracy"),
                                smooth  = FALSE,
                                verbose = FALSE) {
  # 1) Extraire un objet list-like
  h <- NULL
  if ("reticulate.python.builtin.object" %in% class(history) || "py_object" %in% class(history)) {
    if (reticulate::py_has_attr(history, "history")) {
      h <- reticulate::py_to_r(history$history)
    } else if (reticulate::py_has_attr(history, "metrics")) {
      h <- reticulate::py_to_r(history$metrics)
    } else {
      h <- reticulate::py_to_r(history)
    }
  } else if (is.list(history)) {
    h <- history$history
    if (is.null(h)) h <- history$metrics
    if (is.null(h)) h <- history
  }
  if (is.null(h)) stop("[history_curves_plot] Historique illisible.")
  
  # 2) Construire un data.frame long à partir des clés “connues”
  keys_known <- c("loss","val_loss","accuracy","val_accuracy","mae","val_mae")
  df <- NULL
  if (any(names(h) %in% keys_known)) {
    keep <- intersect(names(h), keys_known)
    df <- purrr::map_dfr(keep, function(k) {
      vv <- h[[k]]
      tibble::tibble(
        epoch  = seq_along(vv),
        metric = sub("^val_", "", k),
        set    = if (startsWith(k, "val_")) "val" else "train",
        value  = as.numeric(vv)
      )
    })
  } else if (!is.null(h$names) && !is.null(h$history)) {
    # format "names/history"
    nms <- unlist(h$names)
    df <- purrr::map_dfr(seq_along(nms), function(i) {
      k  <- as.character(nms[[i]])
      vv <- as.numeric(h$history[[i]])
      tibble::tibble(
        epoch  = seq_along(vv),
        metric = sub("^val_", "", k),
        set    = if (startsWith(k, "val_")) "val" else "train",
        value  = vv
      )
    })
  } else if (is.list(h) && all(vapply(h, is.numeric, TRUE))) {
    # liste nommée de vecteurs
    df <- purrr::map_dfr(names(h), function(k) {
      vv <- h[[k]]
      tibble::tibble(
        epoch  = seq_along(vv),
        metric = sub("^val_", "", k),
        set    = if (startsWith(k, "val_")) "val" else "train",
        value  = as.numeric(vv)
      )
    })
  }
  if (is.null(df) || nrow(df) == 0L) {
    stop("Aucune des colonnes attendues ('loss', 'val_loss', 'accuracy', 'val_accuracy') n'a été trouvée.")
  }
  
  # 3) Normaliser les noms (“sparse_categorical_accuracy” → “accuracy”)
  df$metric <- gsub("^sparse_categorical_accuracy$", "accuracy", df$metric)
  df$metric <- gsub("^categorical_accuracy$",        "accuracy", df$metric)
  df$metric <- gsub("^mean_absolute_error$",         "mae",      df$metric)
  
  # 4) Filtrage métriques demandées
  keep <- unique(df$metric)
  keep <- intersect(keep, metrics)
  df   <- dplyr::filter(df, .data$metric %in% keep)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$epoch, y = .data$value, linetype = .data$set)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .data$metric, scales = "free_y") +
    ggplot2::labs(title = title, x = "Epoch", y = "Value", linetype = NULL) +
    ggplot2::theme_minimal(base_size = 11)
  
  if (isTRUE(smooth)) {
    p <- p + ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x)
  }
  if (isTRUE(verbose)) base::message("[history_curves_plot] OK")
  return(p)
}
