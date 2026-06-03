#' Lit les différents fichiers de journaux de bord utilisés jusqu'en 2021 dans l'évaluation de stock,
#' standardise les noms de site et d'espèce, consolide le tout dans un data.frame().
#'
#'
#' @param input_dir Chemin vers le dossier où sont les fichiers de données brutes
#'
#' @importFrom readxl read_excel
#' @import lubridate
#'
#' @return le data.frame() consolidé
#'
lire_journaux_de_bord_pre2021 <- function(
  input_dir = file.path('S:', 'Saguenay', 'Pêche hivernale', 'JB')
) {
  print(2015)
  jb.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0('Sag_journal_', 2015, '.xlsx'))
  ))
  names(jb.init) <- c(
    'echantillonneur',
    'telephone1',
    'telephone2',
    'date',
    'heureDebut',
    'heureFin',
    'immersion',
    'nomSite',
    'village',
    'horsVillage',
    'latitude',
    'longitude',
    'mareeMontante',
    'mareeDescendante',
    'mareeAutre',
    'echosondeur',
    'profSite',
    'profPeche',
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
    'autreEspece',
    'nbAutre',
    'remarques'
  )
  jb.init$anneeGestion <- 2015
  for (i.an in 2016:2017) {
    print(i.an)
    temp <- as.data.frame(readxl::read_excel(
      path = file.path(input_dir, paste0('Sag_journal_', i.an, '.xlsx'))
    ))
    names(temp) <- c(
      'echantillonneur',
      'telephone1',
      'telephone2',
      'date',
      'heureDebut',
      'heureFin',
      'immersion',
      'nomSite',
      'village',
      'horsVillage',
      'latitude',
      'longitude',
      'mareeMontante',
      'mareeDescendante',
      'mareeAutre',
      'echosondeur',
      'profSite',
      'profPeche',
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
      'autreEspece',
      'nbAutre',
      'remarques'
    )
    temp$anneeGestion <- i.an
    jb.init <- merge(jb.init, temp, all = TRUE)
  }
  i.an <- 2018
  print(i.an)
  temp <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0('Sag_journal_', i.an, '.xlsx'))
  )[, 1:31])
  names(temp) <- c(
    'echantillonneur',
    'telephone1',
    'telephone2',
    'date',
    'heureDebut',
    'heureFin',
    'immersion',
    'nomSite',
    'village',
    'horsVillage',
    'latitude',
    'longitude',
    'mareeMontante',
    'mareeDescendante',
    'mareeAutre',
    'echosondeur',
    'profSite',
    'profPeche',
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
    'autreEspece',
    'nbAutre',
    'remarques'
  )
  temp$anneeGestion <- 2018
  jb.init <- merge(jb.init, temp, all = TRUE)
  for (i.an in 2019:2020) {
    print(i.an)
    temp <- as.data.frame(readxl::read_excel(
      path = file.path(input_dir, paste0('Sag_journal_', i.an, '.xlsx'))
    ))
    names(temp) <- c(
      'echantillonneur',
      'telephone1',
      'telephone2',
      'date',
      'heureDebut',
      'heureFin',
      'immersion',
      'nomSite',
      'village',
      'horsVillage',
      'mareeMontante',
      'mareeDescendante',
      'mareeAutre',
      'echosondeur',
      'profSite',
      'profPeche',
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
      'autreEspece',
      'nbAutre',
      'remarques'
    )
    temp$anneeGestion <- i.an
    jb.init <- merge(jb.init, temp, all = TRUE)
  }
  jb.init$annee <- lubridate::year(jb.init$date)
  jb.init$mois <- lubridate::month(jb.init$date)
  jb.init$jour <- lubridate::day(jb.init$date)
  jb.init$jourSemaine <- lubridate::wday(jb.init$date) #1=dimanche
  jb.init$lundiAuVendredi <- jb.init$jourSemaine %in% 2:6
  ##
  ## Nettoyer les données
  ## table(jb.init$echosondeur, useNA='ifany')
  jb.init[jb.init$echosondeur %in% c('O', 'o', 'oui'), 'echosondeur'] <- TRUE
  jb.init[jb.init$echosondeur %in% c('N', 'n', 'non'), 'echosondeur'] <- FALSE
  ## table(jb.init$village, useNA='ifany')
  jb.init[jb.init$village %in% c('O', 'o', 'oui', '1'), 'village'] <- TRUE
  jb.init[jb.init$village %in% c('N', 'n', 'non', '0'), 'village'] <- FALSE
  ## table(jb.init$nbSebastes, useNA='ifany')
  jb.init[which(is.na(jb.init$nbSebastes)), 'nbSebastes'] <- 0
  ## table(jb.init$nbMorue, useNA='ifany')
  jb.init[which(is.na(jb.init$nbMorue)), 'nbMorue'] <- 0
  ## table(jb.init$nbOgac, useNA='ifany')
  jb.init[which(is.na(jb.init$nbOgac)), 'nbOgac'] <- 0
  ## table(jb.init$nbTurbot, useNA='ifany')
  jb.init[which(is.na(jb.init$nbTurbot)), 'nbTurbot'] <- 0
  ## table(jb.init$nbAutre, useNA='ifany') # pas sur quoi faire avec ca!
  jb.init$nbTot <- apply(
    jb.init[, c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')],
    1,
    sum
  )
  ##
  jb.init$nbHeureImmersion <- floor(jb.init$immersion)
  jb.init$nbMinuteImmersion <- (jb.init$immersion - floor(jb.init$immersion)) *
    100
  jb.init[which(jb.init$nbMinuteImmersion == 25), 'nbMinuteImmersion'] <- 15
  jb.init[which(jb.init$nbMinuteImmersion == 50), 'nbMinuteImmersion'] <- 30
  jb.init[which(jb.init$nbMinuteImmersion == 75), 'nbMinuteImmersion'] <- 45
  ## table(jb.init$site,useNA='ifany')
  ##
  ##
  ## ajuster les noms utilisés
  jb <- jb.init
  jb$nomSite <- standardiser_nom_site(jb$nomSite)[, 'sites']
  jb$espece <- standardiser_nom_espece(jb$espece)
  ##
  return(jb)
}
