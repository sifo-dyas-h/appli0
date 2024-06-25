# fonctions utilisées dans les programmes

#' Décennie à partir de l'année
#'
#' @param annee 
#'
#' @return la décennie à laquelle appartient l'année
#' @examples decennie_a_partir_annee(2012) renvoie #2010#
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' fonction de stat agregee
#'
#' @param a : la variable qu'on cherche à résumer statistiquement
#' @param b : l'opérateur qu'on veut appliquer à la variable
#' @param ... 
#'
#' @return un indicateur statistique de la variable d'entrée
#'         exemple : moyenne, écart-type, variance
#' @export
#'
#' @examples fonction_de_stat_agregee(c(1,2,3,4)) renvoie 2.5
fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = TRUE, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = TRUE, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = TRUE, ...)
  }
  return(x)
}

