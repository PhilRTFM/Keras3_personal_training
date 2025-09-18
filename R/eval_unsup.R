# -- R/eval_unsup.R — Évaluation des modèles non supervisés -------------------

#' Calcul des erreurs de reconstruction
#'
#' @param X matrix Données originales
#' @param X_recon matrix Données reconstruites par l'autoencodeur
#'
#' @return list(mse, rmse, mae)
#' @examples
#' X <- matrix(rnorm(100), nrow = 20)
#' X_recon <- X + rnorm(100, sd = 0.1)
#' reconstruction_errors(X, X_recon)
#' @export
reconstruction_errors <- function(X, X_recon) {
  errors <- X - X_recon
  mse <- base::mean(errors^2)
  rmse <- base::sqrt(mse)
  mae <- base::mean(base::abs(errors))
  return(list(mse = mse, rmse = rmse, mae = mae))
}

#' Distribution des erreurs de reconstruction
#'
#' @param errors numeric[] Vecteur d'erreurs (par ex. MSE par individu)
#' @param title character Titre du graphique
#' @param verbose logical Affiche des infos
#'
#' @return ggplot2::ggplot Histogramme/densité des erreurs
#' @examples
#' errs <- rnorm(100, mean = 0.1, sd = 0.05)
#' p <- error_distributions(errs)
#' @export
error_distributions <- function(errors,
                                title = "Distribution des erreurs",
                                verbose = FALSE) {
  df <- tibble::tibble(error = errors)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = error)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                            bins = 30, fill = "skyblue", color = "white", alpha = 0.7) +
    ggplot2::geom_density(color = "red", size = 1, alpha = 0.6) +
    ggplot2::labs(title = title, x = "Erreur", y = "Densité") +
    ggplot2::theme_minimal()
  if (verbose) {
    base::message("Résumé erreurs : ",
                  "moyenne=", round(base::mean(errors), 4),
                  " écart-type=", round(stats::sd(errors), 4))
  }
  return(p)
}

#' Matrice de confusion à partir des centroïdes latents
#'
#' Utilise predict_by_centroids() (défini dans utils.R) et
#' viz_confusion_panel_from_predictions() (défini dans eval_sup.R).
#'
#' @param z_train matrix Coordonnées latentes (train)
#' @param y_train factor Labels train
#' @param z_test matrix Coordonnées latentes (test)
#' @param y_test factor Labels test
#' @param class_levels character[] Niveaux de classes
#' @param verbose logical
#'
#' @return patchwork::patchwork Confusion panel
#' @examples
#' \dontrun{
#' preds <- confusion_from_latent(z_train, y_train, z_test, y_test, class_levels)
#' }
#' @export
confusion_from_latent <- function(z_train,
                                  y_train,
                                  z_test,
                                  y_test,
                                  class_levels,
                                  verbose = FALSE) {
  preds <- predict_by_centroids(z_train = z_train,
                                y_train = y_train,
                                z_test = z_test,
                                class_levels = class_levels,
                                verbose = verbose)
  conf_plot <- viz_confusion_panel_from_predictions(
    y_true = y_test,
    y_pred = preds,
    labels = class_levels,
    title = "Confusion Matrix (AE latent)",
    verbose = verbose
  )
  return(conf_plot)
}
