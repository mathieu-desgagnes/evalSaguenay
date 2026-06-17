#' Ajouter les données récentes aux données historiques de données biologiques.
#'
#' Les nouvelles données sont présentent dans le dossier "input_2021_et_plus_dir", chaque nouvelle années dans un dossier avec comme nom "Saison AAAA"
#'
#' @param donnees_2020_et_moins `data.frame` contenant les données historique consolidées
#' @param input_2021_et_plus_fichier chemin du fichier contenant les données récentes
#'
#' @importFrom readxl read_excel
#'
#' @return le data.frame() consolidé
#'
ajout_donnees_bio <- function(
  donnees_2020_et_moins = NULL,
  input_2021_et_plus_fichier
) {
  ##
  db.init <- donnees_2020_et_moins
  if (file.exists(input_2021_et_plus_fichier)) {
    db.new <- as.data.frame(readxl::read_excel(
      path = input_2021_et_plus_fichier,
      sheet = 'Formulaire de saisie',
      na = c('', 'NA')
    ))
    names(db.new)[match(
      c('site', 'longueur_mm', 'poids_g'),
      names(db.new)
    )] <- c('nomSite', 'longueur', 'poids')
    db.new$anneeGestion <- db.new$annee
    db <- merge(db.init, db.new, all = TRUE)
  } else {
    db <- db.init
  }
  ## dim(db.init); dim(db.new); dim(db)

  db$date <- as.POSIXct(
    paste(db$annee, db$mois, db$jour, sep = '-'),
    format = '%Y-%m-%d'
  )
  db$nomSite <- standardiser_nom_site(sites = db$nomSite)[, 'sites']
  db$espece <- standardiser_nom_espece(espece = db$espece)
  ##
  return(db)
}
