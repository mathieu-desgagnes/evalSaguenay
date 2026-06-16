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
    ##
    names(jb.new)[match(
      c(
        'nom',
        'nb_totalHH',
        'nb_totalMM',
        'site',
        'montante',
        'descendante',
        'sonar_1_0',
        'prof_maxSite_m',
        'prof_min_m',
        'typeCanne',
        'typeAppat',
        'nb_ligne',
        'ng_hamecon',
        'espece_recherche',
        'nbSebaste',
        'notes'
      ),
      names(jb.new)
    )] <- c(
      'echantillonneur',
      'nbHeureImmersion',
      'nbMinuteImmersion',
      'nomSite',
      'mareeMontante',
      'mareeDescendante',
      'echosondeur',
      'profSite',
      'profPeche',
      'enginTypeCanne',
      'appatType',
      'nbLignes',
      'nbHamecons',
      'especeVisee',
      'nbSebastes',
      'remarques'
    )
    jb.new$anneeGestion <- jb.new$annee
    jb.new$date <- as.POSIXct(
      with(jb.new, sprintf("%04d-%02d-%02d", annee, mois, jour)),
      format = "%Y-%m-%d",
      tz = "UTC"
    )

    jb.new$nomSite <- standardiser_nom_site(sites = jb.new$nomSite)[, 'sites']
    jb <- merge(jb.init, jb.new, all = TRUE)
  } else {
    jb <- jb.init
  }
  ## dim(jb.init); dim(jb.new); dim(jb)

  return(jb)
}
