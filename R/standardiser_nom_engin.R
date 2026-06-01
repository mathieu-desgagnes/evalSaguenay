#' Standardise l'orthographe des noms d'engins
#'
#' @param engins vector() des engins à identifier et valider
#'
#' @return un vector() de noms standardisés
#'
#'
standardiser_nom_engin <- function(engins) {
  if (FALSE) {
    table(engins, useNA = 'ifany')
  }
  engins[engins %in% c('ligbne', 'ligne', 'Ligne', 'LIgne')] <- 'ligne'
  engins[engins %in% c('brimbale', 'Brimbale')] <- 'brimbale'
  engins[engins %in% c('rouleau', 'Rouleau')] <- 'rouleau'
  engins[engins %in% c(NA)] <- 'inconnu'
  engins[engins %in% c('ligne/bri.', 'ligne/roul', 'ligne/roul.')] <- 'multiple'
  print(sort(table(engins, useNA = 'always'), decreasing = TRUE))
  ##
  return(engins)
}
