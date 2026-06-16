#' Lit les différents fichiers des échantillonneurs utilisés jusqu'en 2020 dans l'évaluation de stock,
#' standardise les noms de site, consolide le tout dans un data.frame().
#'
#' Aucune données en 2021 pour cause de pandémie.
#'
#' @param input_dir Chemin vers le dossier où sont les fichiers de données brutes
#'
#' @importFrom readxl read_excel
#' @import lubridate
#'
#' @return une list() avec le data.frame() consolidé ($ech) et le data.frame() incluant des données mise de côté pour l'évaluation mais potentiellement
#' d'intérêt pour d'autres travaux
#'
lire_pre2021_echantillonneurs <- function(input_dir) {
  ##
  i.an <- 1995
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    col_types = c(rep('guess', 6), rep('numeric', 6), rep('guess', 21))
  ))
  names(ech.init) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'date',
    'sfs',
    'nbPecheursFond',
    'nbCabanesOccFond',
    'nbPecheursFondSScabane',
    'nbPecheursPelag',
    'nbCabanesOccPelag',
    'nbPecheursPelagSScabane',
    'secteur',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'nomAutre',
    'nbAutre',
    'reValide',
    'inconnu2',
    'nValideNbPecheursPelag',
    'nValideNbPecheursFond',
    'echantillonneur',
    'nValideNbTotPecheurs',
    'nValideNbPecheursMorue',
    'nValideNbPecheursSebaste'
  )
  ## ajuster le nombre de pêcheurs arbitrairement (non documenté)
  if (FALSE) {
    table(ech.init$nbPecheursPelag, useNA = 'ifany')
    table(ech.init$nombrePecheurFond, useNA = 'ifany')
    table(ech.init$nbCabanesOccPelag, useNA = 'ifany')
    table(ech.init$nbCabanesOccFond, useNA = 'ifany')
    table(ech.init$nbPecheursPelagSScabane, useNA = 'ifany')
    table(ech.init$nbPecheursFondSScabane, useNA = 'ifany')
  }
  lesquels <- which(
    is.na(ech.init$nbPecheursPelag) & is.na(ech.init$nombrePecheurFond)
  ) #aucune info sur nombre pour chaque secteur
  ech.init[lesquels, 'nbPecheursPelag'] <- ech.init[
    lesquels,
    'nbCabanesOccPelag'
  ] *
    2.5 +
    ech.init[lesquels, 'nbPecheursPelagSScabane']
  ech.init[lesquels, 'nombrePecheurFond'] <- ech.init[
    lesquels,
    'nbCabanesOccFond'
  ] *
    2.5 +
    ech.init[lesquels, 'nbPecheursFondSScabane']
  lesquels <- which(
    is.na(ech.init$nbPecheursPelag) & is.na(ech.init$nombrePecheurFond)
  ) #aucune info sur nombre pour chaque secteur
  ech.init[lesquels, 'nbPecheursPelag'] <- ech.init[
    lesquels,
    'nbCabanesOccPelag'
  ] *
    2.5
  ech.init[lesquels, 'nombrePecheurFond'] <- ech.init[
    lesquels,
    'nbCabanesOccFond'
  ] *
    2.5
  lesquels <- which(
    is.na(ech.init$nbPecheursPelag) & is.na(ech.init$nombrePecheurFond)
  ) #aucune info sur nombre pour chaque secteur
  ech.init[lesquels, 'nbPecheursPelag'] <- ech.init[
    lesquels,
    'nbPecheursPelagSScabane'
  ]
  ech.init[lesquels, 'nombrePecheurFond'] <- ech.init[
    lesquels,
    'nbPecheursFondSScabane'
  ]
  lesquels <- which(
    is.na(ech.init$nbPecheursPelag) & is.na(ech.init$nombrePecheurFond)
  ) # utilise des données non-validées, source inconnue
  ech.init[lesquels, 'nbPecheursPelag'] <- round(ech.init[
    lesquels,
    'nValideNbPecheursPelag'
  ])
  ech.init[lesquels, 'nombrePecheurFond'] <- round(ech.init[
    lesquels,
    'nValideNbPecheursFond'
  ])
  ech.init$anneeGestion <- 1995
  ech <- ech.init[, c(
    'annee',
    'anneeGestion',
    'date',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'echantillonneur'
  )]
  ##
  i.an <- 1996
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    col_types = c(rep('guess', 6), rep('numeric', 7), rep('guess', 19))
  ))
  names(ech.init) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'date',
    'sfs',
    'nbPecheursTot',
    'nbPecheursFond',
    'nbCabanesOccFond',
    'nbPecheursFondSScabane',
    'nbPecheursPelag',
    'nbCabanesOccPelag',
    'nbPecheursPelagSScabane',
    'secteur',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'nomAutre2',
    'nbAutre2',
    'reValide',
    'echantillonneur',
    'nValideNbPecheursPelag',
    'nValideNbPecheursFond',
    'nValideNbTotPecheurs'
  )
  ## ajuster le nombre de pêcheurs arbitrairement (non documenté)
  if (FALSE) {
    table(ech.init$nbPecheursPelag, useNA = 'ifany')
    table(ech.init$nombrePecheurFond, useNA = 'ifany')
    table(ech.init$nbCabanesOccPelag, useNA = 'ifany')
    table(ech.init$nbCabanesOccFond, useNA = 'ifany')
    table(ech.init$nbPecheursPelagSScabane, useNA = 'ifany')
    table(ech.init$nbPecheursFondSScabane, useNA = 'ifany')
  }
  ech.init[
    which(is.na(ech.init$nbPecheursPelag)),
    'nbPecheursPelag'
  ] <- ech.init[which(is.na(ech.init$nbPecheursPelag)), 'nbCabanesOccPelag'] *
    2.5 +
    ech.init[which(is.na(ech.init$nbPecheursPelag)), 'nbPecheursPelagSScabane']
  ech.init[
    which(is.na(ech.init$nbPecheursPelag)),
    'nbPecheursPelag'
  ] <- round(ech.init[
    which(is.na(ech.init$nbPecheursPelag)),
    'nValideNbPecheursPelag'
  ])
  ech.init[
    which(is.na(ech.init$nombrePecheurFond)),
    'nombrePecheurFond'
  ] <- ech.init[
    which(is.na(ech.init$nombrePecheurFond)),
    'nbCabanesOccFond'
  ] *
    2.5 +
    ech.init[which(is.na(ech.init$nombrePecheurFond)), 'nbPecheursFondSScabane']
  ech.init[
    which(is.na(ech.init$nombrePecheurFond)),
    'nombrePecheurFond'
  ] <- round(ech.init[
    which(is.na(ech.init$nombrePecheurFond)),
    'nValideNbPecheursFond'
  ])
  ech.init$anneeGestion <- 1996
  ##
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech)
  ##
  i.an <- 1997
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'date',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbPecheursFond',
    'nbPecheursPelag',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'echantillonneur'
  )
  ech.init$anneeGestion <- 1997
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech)
  ##
  i.an <- 1998
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'date',
    'echantillonneur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'especeVisee',
    'reValide',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbSole',
    'nbMerlucheEcureuil'
  )
  ech.init$anneeGestion <- 1998
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech$date)
  ##
  i.an <- 1999
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1:AL1371'
  ))
  names(ech.init) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbMerlucheEcureuil',
    'nbAnguille',
    'reValide',
    'nbPecheursSecteur',
    'inconnu2',
    'inconnu3',
    'nbPecheursMorue',
    'nbPecheursSebastes',
    'especeVisee'
  )

  ech.init2 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1371:AL1431',
    col_types = c(rep('guess', 5), 'text', rep('guess', 32))
  )) #en prenant une ligne de plus, pour les titres
  names(ech.init2) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbMerlucheEcureuil',
    'nbAnguille',
    'reValide',
    'nbPecheursSecteur',
    'inconnu2',
    'inconnu3',
    'nbPecheursMorue',
    'nbPecheursSebastes',
    'especeVisee'
  )
  ech.init2[, 'date'] <- as.POSIXct(paste(
    1999,
    '02',
    substr(ech.init2[, 'date'], 1, 2),
    sep = '-'
  ))
  ech.init3 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1431:AL1477',
    col_types = c(rep('guess', 5), 'date', rep('guess', 32))
  ))
  names(ech.init3) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbMerlucheEcureuil',
    'nbAnguille',
    'reValide',
    'nbPecheursSecteur',
    'inconnu2',
    'inconnu3',
    'nbPecheursMorue',
    'nbPecheursSebastes',
    'especeVisee'
  )
  ech.init4 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1477:AL1479',
    col_types = c(rep('guess', 5), 'text', rep('guess', 32))
  ))
  names(ech.init4) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbMerlucheEcureuil',
    'nbAnguille',
    'reValide',
    'nbPecheursSecteur',
    'inconnu2',
    'inconnu3',
    'nbPecheursMorue',
    'nbPecheursSebastes',
    'especeVisee'
  )
  ech.init4[, 'date'] <- as.POSIXct(paste(
    1999,
    '02',
    substr(ech.init4[, 'date'], 1, 2),
    sep = '-'
  ))
  ech.init5 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1479:AL1825',
    col_types = c(rep('guess', 5), 'date', rep('guess', 32))
  ))
  names(ech.init5) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbMerlucheEcureuil',
    'nbAnguille',
    'reValide',
    'nbPecheursSecteur',
    'inconnu2',
    'inconnu3',
    'nbPecheursMorue',
    'nbPecheursSebastes',
    'especeVisee'
  )
  temp <- rbind(ech.init, ech.init2, ech.init3, ech.init4, ech.init5)
  temp$anneeGestion <- 1999
  ech <- merge(ech, temp, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech$date)
  ##
  i.an <- 2000
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'date',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbPecheursFond',
    'nbPecheursPelag',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'nbCapelan',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbMerlucheEcureuil',
    'nbAnguille',
    'nbOursin',
    'echantillonneur'
  )
  ech.init$anneeGestion <- 2000
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech)
  ##
  i.an <- 2001
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'date',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbPecheursFond',
    'nbPecheursPelag',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nbCapelan',
    'nbFletan',
    'nbLycode',
    'nbLimace',
    'nbTruite',
    'nbSaida',
    'nbRaie',
    'nbPoulamon',
    'nbPlie',
    'nbGoberge',
    'nbHareng',
    'nbMerlucheEcureuil',
    'nbAnguille',
    'nbOursin',
    'echantillonneur'
  )
  ech.init$anneeGestion <- 2001
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech)
  ##
  i.an <- 2002
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'especeVisee',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'nomAutre2',
    'nbAutre2',
    'reValide',
    'inconnu2',
    'inconnu3',
    'inconnu4_gr_tal',
    'noCabane'
  )
  ech.init <- head(ech.init, -2) #les deux dernières lignes du fichier original semblent est des sommation de colonnes, sans date ou site
  ech.init$anneeGestion <- 2002
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech$date)
  ##
  i.an <- 2003
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1:T2288'
  ))
  names(ech.init) <- c(
    'annee',
    'date',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbPecheursFond',
    'nbPecheursPelag',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'echantillonneur',
    'reValide'
  )
  ech.init$anneeGestion <- 2003
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech$date)
  ##
  i.an <- 2004
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1:V2019'
  ))
  names(ech.init) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'date',
    'echantillonneur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'reValide'
  )
  ech.init$anneeGestion <- 2004
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2005
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1:U555'
  ))
  names(ech.init) <- c(
    'annee',
    'noSite',
    'echantillonneur',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'reValide'
  )
  ech.init2 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A555:U562'
  ))
  names(ech.init2) <- c(
    'annee',
    'noSite',
    'echantillonneur',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'reValide'
  )
  ech.init2[, 'date'] <- as.POSIXct(paste(
    2005,
    lubridate::month(ech.init2[, 'date']),
    lubridate::day(ech.init2[, 'date']),
    sep = '-'
  ))
  ech.init3 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A562:U585'
  ))
  names(ech.init3) <- c(
    'annee',
    'noSite',
    'echantillonneur',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'reValide'
  )
  ech.init4 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A585:U591'
  ))
  names(ech.init4) <- c(
    'annee',
    'noSite',
    'echantillonneur',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'reValide'
  )
  ech.init4[, 'date'] <- as.POSIXct(paste('2005', '03', '01', sep = '-'))
  ech.init5 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A591:U1690'
  ))
  names(ech.init5) <- c(
    'annee',
    'noSite',
    'echantillonneur',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'reValide'
  )
  ech.init6 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1690:U1697'
  ))
  names(ech.init6) <- c(
    'annee',
    'noSite',
    'echantillonneur',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'reValide'
  )
  ech.init6[, 'date'] <- as.POSIXct(paste('2005', '03', '01', sep = '-'))
  ech.init7 <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    range = 'A1697:U1933'
  ))
  names(ech.init7) <- c(
    'annee',
    'noSite',
    'echantillonneur',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'reValide'
  )
  ech.init <- rbind(
    ech.init,
    ech.init2,
    ech.init3,
    ech.init4,
    ech.init5,
    ech.init6,
    ech.init7
  )
  ech.init$anneeGestion <- 2005
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2006
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'echantillonneur',
    'date',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'nomAutre2',
    'nbAutre2',
    'reValide'
  )
  ech.init$anneeGestion <- 2006
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2007
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'date',
    'noSite',
    'nomSite',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'echantillonneur',
    'reValide'
  )
  ech.init$anneeGestion <- 2007
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2008
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'noSite',
    'nomSite',
    'date',
    'reValide',
    'echantillonneur',
    'sfs',
    'nbPecheursFond',
    'nbPecheursPelag',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'affiliationEchantillonneur'
  )
  ech.init$anneeGestion <- 2008
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2009
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'inconnu',
    'annee',
    'noSite',
    'nomSite',
    'date',
    'sfs',
    'secteur',
    'nbPecheursPelag',
    'nbPecheursFond',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'echantillonneur',
    'reValide'
  )
  ech.init$anneeGestion <- 2009
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2010
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'nomSite',
    'date',
    'echantillonneur',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'noPecheur',
    'heureEch'
  )
  ech.init$anneeGestion <- 2010
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2011
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    col_types = c(rep('guess', 19), 'text', 'text', 'guess')
  ))
  names(ech.init) <- c(
    'annee',
    'nomSite',
    'date',
    'echantillonneur',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'noPecheur',
    'heureEch',
    'reValide'
  )
  ech.init$anneeGestion <- 2011
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2012
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    col_types = c(rep('guess', 19), 'text', 'text')
  ))
  names(ech.init) <- c(
    'annee',
    'nomSite',
    'date',
    'echantillonneur',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'noPecheur',
    'reValide'
  )
  ech.init$anneeGestion <- 2012
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2013
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls')),
    col_types = c(rep('guess', 19), 'text', 'text')
  ))
  names(ech.init) <- c(
    'annee',
    'nomSite',
    'date',
    'echantillonneur',
    'sfs',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'heureEch'
  )
  ech.init$anneeGestion <- 2013
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2014
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'echantillonneur',
    'date',
    'heure',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'pecheurDedie',
    'commentaires'
  )
  ech.init$anneeGestion <- 2014
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2015
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'echantillonneur',
    'date',
    'heure',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'engin',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'profondeur',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'commentaires'
  )
  ## table(ech.init$secteur); table(ech.init$engin)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ech.init$anneeGestion <- 2015
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2016
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xls'))
  ))
  names(ech.init) <- c(
    'annee',
    'echantillonneur',
    'date',
    'heure',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'engin',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'profondeur',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'commentaires'
  )
  ## table(ech.init$secteur); table(ech.init$engin)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ech.init$anneeGestion <- 2016
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2017
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xlsx'))
  ))
  names(ech.init) <- c(
    'annee',
    'echantillonneur',
    'date',
    'heure',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'engin',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'profondeur',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'commentaires'
  )
  ## table(ech.init$secteur); table(ech.init$engin)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ech.init$anneeGestion <- 2017
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2018
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xlsx'))
  ))
  names(ech.init) <- c(
    'annee',
    'echantillonneur',
    'date',
    'heure',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'engin',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'profondeur',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'commentaires'
  )
  ## table(ech.init$secteur); table(ech.init$engin)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ech.init$anneeGestion <- 2018
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2019
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xlsx'))
  ))
  names(ech.init) <- c(
    'annee',
    'echantillonneur',
    'date',
    'heure',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'engin',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'profondeur',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'commentaires'
  )
  ## table(ech.init$secteur); table(ech.init$engin)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ech.init$anneeGestion <- 2019
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  i.an <- 2020
  print(i.an)
  ech.init <- as.data.frame(readxl::read_excel(
    path = file.path(input_dir, paste0(i.an, 'PUE.xlsx'))
  ))
  names(ech.init) <- c(
    'annee',
    'echantillonneur',
    'date',
    'heure',
    'sfs',
    'nomSite',
    'nbPecheursPelag',
    'nbPecheursFond',
    'noPecheur',
    'secteur',
    'engin',
    'nbLignes',
    'nbHamecons',
    'nbHeures',
    'echosondeur',
    'nbEperlan',
    'profondeur',
    'nbMorue',
    'nbOgac',
    'nbSebastes',
    'nbTurbot',
    'nomAutre',
    'nbAutre',
    'commentaires'
  )
  ## table(ech.init$secteur); table(ech.init$engin)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ech.init$anneeGestion <- 2020
  ech <- merge(ech, ech.init, all = TRUE)
  ## nrow(ech.init); nrow(ech); plot(ech.init$date)
  ##
  ## ajuster les données
  ech$annee <- lubridate::year(ech$date)
  ech$mois <- lubridate::month(ech$date)
  ech$jour <- lubridate::day(ech$date)
  ech$jourSemaine <- lubridate::wday(ech$date) #1=dimanche
  ech$lundiAuVendredi <- ech$jourSemaine %in% 2:6
  ## table(ech.init$semaine(0)_FinDeSemaine(1))
  ##
  ##
  ## standardiser les noms des sites
  ## table(ech$site)
  temp <- standardiser_nom_site(ech$nomSite)
  ech$nomSite <- temp[, 'sites']
  ech$nomSite.ori <- temp[, 'sites.ori']
  ##
  ## attribuer les noSite selon les site
  ## table(ech$noSite, ech$site, useNA='ifany')
  ech[is.na(ech$noSite), 'noSite'] <- match(
    ech[is.na(ech$noSite), 'nomSite'],
    c(
      'StFulgence',
      'AnseBenjamin',
      'GrandeBaie',
      'LesBattures',
      'SteRose',
      'StFelix',
      'RiviereEternite',
      'AnseStJean'
    )
  )
  ## table(ech$noSite, ech$site, useNA='ifany')
  ##
  ## standardiser les noms de 'secteur'
  ## table(ech$secteur)
  ech[ech$secteur %in% c('bimbale', 'Brimbale'), 'secteur'] <- 'brimbale'
  ech[
    ech$secteur %in%
      c('éperlan', 'Éperlan', 'pelag', 'pélag', 'Pélag', 'pélagique'),
    'secteur'
  ] <- 'pelag'
  ech[ech$secteur %in% c('fond', 'Fond'), 'secteur'] <- 'fond'
  ech[ech$secteur %in% c('rouleau'), 'secteur'] <- 'rouleau'
  ##
  ## les nombres == NA transformés en 0
  temp <- names(ech)[which(substr(names(ech), 1, 2) == 'nb')]
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
    ech[is.na(ech[i.esp]), i.esp] <- 0
  }
  ##
  ## ajuster les secteurs selon l'espèce capturée (éprelan ou poisson de fond)
  lesquels <- which(
    ech$secteur == 'pelag' &
      apply(
        ech[, c('sebaste', 'morue', 'turbot', 'ogac')],
        1,
        sum,
        na.rm = TRUE
      ) >
        ech$eperlan
  )
  if (FALSE) {
    ech[lesquels, c('annee', 'eperlan', 'sebaste')]
  }
  ech[lesquels, 'secteur'] <- 'fond'
  ##
  lesquels <- which(
    ech$secteur %in%
      c('fond', 'rouleau', 'brimbale') &
      ech$eperlan >
        apply(
          ech[, c('sebaste', 'morue', 'turbot', 'ogac')],
          1,
          sum,
          na.rm = TRUE
        )
  )
  if (FALSE) {
    ech[lesquels, c('annee', 'eperlan', 'sebaste', 'morue', 'turbot')]
  }
  ech[lesquels, 'secteur'] <- 'pelag'
  ##
  ## Dans le fichier Lect_PUE_SAG_96-2020.sas, il est suggéré d'enlever les lignes suivantes
  ## Enlève des données qui proviennet de calendrier et non d'échanillonneur
  ech.prime <- ech
  ech <- ech[
    !ech$echantillonneur %in%
      c(
        'Anonyme',
        'Alexandre Tremblay',
        'Antoine Claveau',
        'Florent Boivin',
        'Jean Demers',
        'Jean-Claude Tremblay',
        'Line Claveau',
        'Richard Perron',
        'Romuald Tremblay',
        'Tony Franklin',
        'Camil Corneau',
        'Denis Durand',
        'Denis Roy',
        'Diane Voyer',
        'Jean-Yves Corneau',
        'Jessi Bouchard',
        'Marc Berni',
        'Michael Bouchard',
        'Monique Gagnon',
        'Noel Jean',
        'Rosaire Boies',
        'Benoît Bouchard',
        'anonyme',
        'Lucie Duchesne'
      ),
  ]
  ## ces activités proviennent probablement des calendriers, on ne les conserve pas ne sont pas semblables à celles receuillies par les échantillonneurs
  ## Lucie Duchesne capture régulièrement plus de 5 poissons de fond par jour en 2009 et 2011
  ##
  ## pêcheurs 'dédié' à partir de 2014 sont pêcheurs-repères, ne pas inclure...
  ech <- ech[is.na(ech$pecheurDedie), ]
  ##
  return(list(ech = ech, ech.prime = ech.prime))
}
