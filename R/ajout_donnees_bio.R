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
      sheet = 2,
      na = 'NA'
    ))
  }

  db <- merge(db.init, db.new, all = TRUE)
  ## nrow(db.init); nrow(db.new); nrow(db)

  db$date <- as.POSIXct(
    paste(db$annee, db$mois, db$jour, sep = '-'),
    format = '%Y-%m-%d'
  )
  db$site <- standardiser_nom_site(sites = db$site)[, 'sites']
  db$espece <- standardiser_nom_espece(espece = db$espece)
  db$annee <- db$anneeGestion
  ##
  return(db)
}
