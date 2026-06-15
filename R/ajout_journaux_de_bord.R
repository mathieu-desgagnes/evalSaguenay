#' Ajouter les données récentes aux données historiques de journaux de bords.
#'
#' Les nouvelles données sont présentent dans le dossier "input_2021_et_plus_dir", chaque nouvelle années dans un dossier avec comme nom "Saison AAAA"
#'
#' @param donnees_2020_et_moins `data.frame` contenant les données historique consolidées
#' @param input_2021_et_plus_fichier chemin du fichier contenant les données récentes
#'
#' @importFrom readxl read_excel
#' @import lubridate
#'
#' @return le data.frame() consolidé
#'
ajout_journaux_de_bord <- function(
  donnees_2020_et_moins = NULL,
  input_2021_et_plus_fichier
) {
  ##
  jb.init <- donnees_2020_et_moins
  if (file.exists(input_2021_et_plus_fichier)) {
    jb.new <- as.data.frame(readxl::read_excel(
      path = input_2021_et_plus_fichier,
      sheet = 'Formulaire de saisie',
      na = c('', 'NA')
    ))
    jb.new$site <- standardiser_nom_site(sites = jb.new$site)[, 'sites']
    jb <- merge(jb.init, jb.new, all = TRUE)
  } else {
    jb <- jb.init
  }
  ## nrow(db.init); nrow(db.new); nrow(db)

  return(jb)
}
