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
