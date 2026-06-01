#' Ajouter les données récentes aux données historiques des journaux de bord.
#'
#' Les nouvelles données sont présentent dans le dossier "input_2021_et_plus_dir", chaque nouvelle années dans un dossier avec comme nom "Saison AAAA"
#'
#' @param donnees_2020_et_moins `data.frame` contenant les données historique consolidées
#' @param input_2021_et_plus_dir chemin du dossier contenant des dossier "Saison 20xx" avec les nouvelles données
#'
#' @importFrom readxl read_excel
#' @import lubridate
#'
#' @return le data.frame() consolidé
#'
ajout_echantillonneurs <- function(
  donnees_2020_et_moins = NULL,
  input_2021_et_plus_dir
) {
  ##
  ech.init <- donnees_2020_et_moins
  temp <- dir(input_2021_et_plus_dir)
  annee <- substr(temp[which(substr(temp, 1, 7) == 'Saison ')], 8, 11)
  for (i.an in annee) {
    print(i.an)
    ech.new <- as.data.frame(readxl::read_excel(
      path = file.path(
        input_2021_et_plus_dir,
        paste0('Saison ', i.an),
        paste0('echantillonneur_', i.an, '.xlsx')
      ),
      sheet = 1,
      na = 'NA'
    ))
    names(ech.new) <- c(
      'affiliationEchantillonneur',
      'echantillonneur',
      'annee',
      'mois',
      'jour',
      'heure',
      'sfs',
      'nomSite',
      'nbPecheursFond',
      'noPecheur',
      'secteur',
      'engin',
      'nbLignes',
      'nbHamecons',
      'nbHeures',
      'echosondeur',
      'prof',
      'nbMorue',
      'nbOgac',
      'nbSebastes',
      'nbTurbot',
      'nbCrabe',
      'nbLycode',
      'nbRaie',
      'nbMerluche',
      'nbEperlan',
      'nbFletan',
      'nbSaida',
      'commentaires'
    )
    if (nrow(ech.new) > 0) {
      ech.new$anneeGestion <- i.an
      ech.new$date <- as.POSIXct(
        paste(ech.new$annee, ech.new$mois, ech.new$jour, sep = '-'),
        format = '%Y-%m-%d'
      )
      ech.new$nomSite <- standardiser_nom_site(site = ech.new$nomSite)[,
        'sites'
      ]
      ech.new$annee <- lubridate::year(ech.new$date)
      ech.new$mois <- lubridate::month(ech.new$date)
      ech.new$jour <- lubridate::day(ech.new$date)
      ech.new$jourSemaine <- lubridate::wday(ech.new$date) #1=dimanche
      ech.new$lundiAuVendredi <- ech.new$jourSemaine %in% 2:6
      ##
      ## Nettoyer les données
      ## table(ech.new$echosondeur, useNA='ifany')
      ## table(ech.new$nbSebastes, useNA='ifany')
      ## table(ech.new$nbMorue, useNA='ifany')
      ## table(ech.new$nbOgac, useNA='ifany')
      ## table(ech.new$nbTurbot, useNA='ifany')
      ## table(ech.new$nbAutre, useNA='ifany') # pas sur quoi faire avec ca!
      ##
      ## les nombres == NA transformés en 0
      temp <- names(ech.new)[which(substr(names(ech.new), 1, 2) == 'nb')]
      especes <- temp[which(
        !temp %in%
          c(
            "nbPecheursPelag",
            "nbPecheursFond",
            "nbLignes",
            "nbHamecons",
            "nbHeures",
            "nbPecheursTot",
            "nbCabanesOccFond",
            "nbPecheursFondSScabane",
            "nbCabanesOccPelag",
            "nbPecheursPelagSScabane",
            "nbPecheursSecteur",
            "nbPecheursMorue",
            "nbPecheursSebastes"
          )
      )]
      for (i.esp in especes) {
        ech.new[is.na(ech.new[i.esp]), i.esp] <- 0
      }
      ##
      ech.init <- merge(ech.init, ech.new, all = TRUE)
    }
  }
  ##
  ## nrow(ech.init)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ## table(ech.init$echosondeur)
  ech.init[
    ech.init$echosondeur %in% c('O', 'o', 'oui', 'OUI'),
    'echosondeur'
  ] <- TRUE
  ech.init[
    ech.init$echosondeur %in% c('N', 'n', 'non', 'NON'),
    'echosondeur'
  ] <- FALSE
  ech.init[
    ech.init$echosondeur %in% c('nd', 'O/N', 'P', '?', '17', '2', '3'),
    'echosondeur'
  ] <- NA
  ech <- ech.init
  ##
  return(ech)
}
