#' Ajouter les données récentes aux données historiques de journaux de bords.
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
ajout_journaux_de_bord <- function(
  donnees_2020_et_moins = NULL,
  input_2021_et_plus_dir
) {
  ##
  jb.init <- donnees_2020_et_moins
  temp <- dir(input_2021_et_plus_dir)
  annee <- substr(temp[which(substr(temp, 1, 7) == 'Saison ')], 8, 11)
  jb.temp <- as.data.frame(array(
    NA,
    dim = c(0, 38),
    dimnames = list(
      NULL,
      c(
        'annee',
        'partenaire',
        'echantillonneur',
        'dateTemporaireTravail',
        'mois',
        'jour',
        'heureDebut',
        'minuteDebut',
        'heureFin',
        'minuteFin',
        'nbHeureImmersion',
        'nbMinuteImmersion',
        'nomSite',
        'village',
        'horsVillage',
        'mareeMontante',
        'mareeDescendante',
        'mareeAutre',
        'echosondeur',
        'profMin',
        'profMax',
        'engin',
        'enginTypeCanne',
        'appatType',
        'nbLignes',
        'nbHamecons',
        'especeVisee',
        'nbSebastes',
        'nbMorue',
        'nbOgac',
        'nbTurbot',
        'nbRaie',
        'nbLycode',
        'nbCrabe',
        'nbFletan',
        'nbSaida',
        'nbEperlan',
        'commentaire'
      )
    )
  ))
  for (i.an in annee) {
    print(i.an)
    jb.new <- as.data.frame(readxl::read_excel(
      path = file.path(
        input_2021_et_plus_dir,
        paste0('Saison ', i.an),
        paste0('journalBord_', i.an, '.xlsx')
      ),
      sheet = 1,
      na = 'NA'
    ))
    names(jb.new) <- c(
      'annee',
      'partenaire',
      'echantillonneur',
      'dateTemporaireTravail',
      'mois',
      'jour',
      'heureDebut',
      'minuteDebut',
      'heureFin',
      'minuteFin',
      'nbHeureImmersion',
      'nbMinuteImmersion',
      'nomSite',
      'village',
      'horsVillage',
      'mareeMontante',
      'mareeDescendante',
      'mareeAutre',
      'echosondeur',
      'profMin',
      'profMax',
      'engin',
      'enginTypeCanne',
      'appatType',
      'nbLignes',
      'nbHamecons',
      'especeVisee',
      'nbSebastes',
      'nbMorue',
      'nbOgac',
      'nbTurbot',
      'nbRaie',
      'nbLycode',
      'nbCrabe',
      'nbFletan',
      'nbSaida',
      'nbEperlan',
      'commentaire'
    )
    if (nrow(jb.new) > 0) {
      jb.new$anneeGestion <- as.numeric(i.an)
      jb.new$date <- as.POSIXct(
        paste(jb.new$annee, jb.new$mois, jb.new$jour, sep = '-'),
        format = '%Y-%m-%d'
      )
      jb.new$nomSite <- standardiser_nom_site(site = jb.new$nomSite)[,
        'sites'
      ]
      jb.new$espece <- standardiser_nom_espece(espece = jb.new$espece)
      jb.new$annee <- lubridate::year(jb.new$date)
      jb.new$mois <- lubridate::month(jb.new$date)
      jb.new$jour <- lubridate::day(jb.new$date)
      jb.new$jourSemaine <- lubridate::wday(jb.new$date) #1=dimanche
      jb.new$lundiAuVendredi <- jb.new$jourSemaine %in% 2:6
      ##
      ## Nettoyer les données
      ## table(jb.new$echosondeur, useNA='ifany')
      jb.new[jb.new$echosondeur %in% c('O', 'o', 'oui'), 'echosondeur'] <- TRUE
      jb.new[jb.new$echosondeur %in% c('N', 'n', 'non'), 'echosondeur'] <- FALSE
      ## table(jb.new$village, useNA='ifany')
      jb.new[jb.new$village %in% c('O', 'o', 'oui', 1), 'village'] <- TRUE
      jb.new[jb.new$village %in% c('N', 'n', 'non', 0), 'village'] <- FALSE
      jb.new[, 'village'] <- as.logical(jb.new[, 'village'])
      ## table(jb.new$nbSebastes, useNA='ifany')
      jb.new[which(is.na(jb.new$nbSebastes)), 'nbSebastes'] <- 0
      ## table(jb.new$nbMorue, useNA='ifany')
      jb.new[which(is.na(jb.new$nbMorue)), 'nbMorue'] <- 0
      ## table(jb.new$nbOgac, useNA='ifany')
      jb.new[which(is.na(jb.new$nbOgac)), 'nbOgac'] <- 0
      ## table(jb.new$nbTurbot, useNA='ifany')
      jb.new[which(is.na(jb.new$nbTurbot)), 'nbTurbot'] <- 0
      ## table(jb.new$nbAutre, useNA='ifany') # pas sur quoi faire avec ca!
      jb.new$nbTot <- apply(
        jb.new[, c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')],
        1,
        sum,
        na.rm = TRUE
      )
      ##
      jb.temp <- merge(jb.temp, jb.new, all = TRUE)
    }
  }
  ##
  ## nrow(jb.init); nrow(jb.temp)
  jb <- merge(jb.init, jb.temp, all = TRUE)
  ## nrow(jb)
  ##
  return(jb)
}
