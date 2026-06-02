#' Ajouter les données récentes aux données historiques de données biologiques.
#'
#' Les nouvelles données sont présentent dans le dossier "input_2021_et_plus_dir", chaque nouvelle années dans un dossier avec comme nom "Saison AAAA"
#'
#' @param donnees_2020_et_moins `data.frame` contenant les données historique consolidées
#' @param input_2021_et_plus_dir chemin du dossier contenant des dossier "Saison 20xx" avec les nouvelles données
#'
#' @importFrom readxl read_excel
#'
#' @return le data.frame() consolidé
#'
ajout_donnees_bio <- function(
  donnees_2020_et_moins = NULL,
  input_2021_et_plus_dir
) {
  ##
  db.init <- donnees_2020_et_moins
  temp <- dir(input_2021_et_plus_dir)
  annee <- substr(temp[which(substr(temp, 1, 7) == 'Saison ')], 8, 11)
  db.temp <- as.data.frame(array(
    NA,
    dim = c(0, 12),
    dimnames = list(
      NULL,
      c(
        'partenaire',
        'nomSite',
        'echantillonneur',
        'annee',
        'mois',
        'jour',
        'espece',
        'longueur',
        'poids',
        'commentaire',
        'anneeGestion',
        'date'
      )
    )
  ))
  for (i.an in annee) {
    print(i.an)
    if(file.exists(file.path(
      input_2021_et_plus_dir,
      paste0('Saison ', i.an),
      paste0('donneeBiologique_', i.an, '.xlsx'))){
      db.new <- as.data.frame(readxl::read_excel(
      path = file.path(
        input_2021_et_plus_dir,
        paste0('Saison ', i.an),
        paste0('donneeBiologique_', i.an, '.xlsx')
      ),
      sheet = 1,
      na = 'NA'
    ))
      names(db.new) <- c(
      'partenaire',
      'nomSite',
      'echantillonneur',
      'annee',
      'mois',
      'jour',
      'espece',
      'longueur',
      'poids',
      'commentaire'
      )
      if (nrow(db.new) > 0) {
      db.new$anneeGestion <- as.numeric(i.an)
      db.new$date <- as.POSIXct(
        paste(db.new$annee, db.new$mois, db.new$jour, sep = '-'),
        format = '%Y-%m-%d'
      )
      db.new$nomSite <- standardiser_nom_site(sites = db.new$nomSite)[, 'sites']
      db.new$espece <- standardiser_nom_espece(espece = db.new$espece)
      db.temp <- merge(db.temp, db.new, all = TRUE)
    }
  }}
  ##
  ## nrow(db.init); nrow(db.temp)
  db <- merge(db.init, db.temp, all = TRUE)
  ## nrow(db)
  ##
  return(db)
}
