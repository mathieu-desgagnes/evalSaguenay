#' Ajouter les données récentes aux données historiques des journaux de bord.
#'
#' Les nouvelles données sont présentent dans le fichier "input_2021_et_plus_fichier"
#'
#' @param donnees_2020_et_moins `data.frame` contenant les données historique consolidées
#' @param fichier_2021_et_plus_fichier chemin le fichier contenant les données récentes
#'
#' @importFrom readxl read_excel
#' @import lubridate
#'
#' @return le data.frame() consolidé
#'
ajout_echantillonneurs <- function(
  donnees_2020_et_moins = NULL,
  input_2021_et_plus_fichier
) {
  ##
  ech.init <- donnees_2020_et_moins
  if (file.exists(input_2021_et_plus_fichier)) {
    ech.new <- as.data.frame(readxl::read_excel(
      path = input_2021_et_plus_fichier,
      sheet = 'Formulaire de saisie',
      na = c('', 'NA')
    ))
    ##
    names(ech.new)[match(
      c(
        'partenaire',
        'semaine(0)_FinDeSemaine(1)',
        'site',
        'idPecheur',
        'nb_ligne',
        'nb_hamecon',
        'nb_heure',
        'sonar',
        'profondeur_m',
        'morue',
        'ogac',
        'sebaste',
        'turbot',
        'crabe',
        'lycode',
        'raie',
        'merluche',
        'eperlan',
        'fletan',
        'saida',
        'notes'
      ),
      names(ech.new)
    )] <- c(
      'affiliationEchantillonneur',
      'sfs',
      'nomSite',
      'noPecheur',
      'nbLignes',
      'nbHamecons',
      'nbHeures',
      'echosondeur',
      'profondeur',
      'nbMorue',
      'nbOgac',
      'nbSebastes',
      'nbTurbot',
      'nbCrabe',
      'nbLycode',
      'nbRaie',
      'nbMerlucheEcureuil',
      'nbEperlan',
      'nbFletan',
      'nbSaida',
      'commentaires'
    )
    ech.new$date <- as.POSIXct(
      paste(ech.new$annee, ech.new$mois, ech.new$jour, sep = '-'),
      format = '%Y-%m-%d'
    )
    ech.new$nomSite <- standardiser_nom_site(site = ech.new$nomSite)[, 'sites']
    ech.new$jourSemaine <- lubridate::wday(ech.new$date) #1=dimanche
    ech.new$lundiAuVendredi <- ech.new$jourSemaine %in% 2:6
    ##
    ech.new$anneeGestion <- ech.new$annee
    ##
    ech <- merge(ech.init, ech.new, all = TRUE)
  } else {
    ech <- ech.init
  }

  ## nrow(ech)
  ech$engin <- standardiser_nom_engin(ech$engin)
  ## table(ech$echosondeur)
  ech[
    ech$echosondeur %in% c('O', 'o', 'oui', 'OUI', TRUE),
    'echosondeur'
  ] <- TRUE
  ech[
    ech$echosondeur %in% c('N', 'n', 'non', 'NON', FALSE),
    'echosondeur'
  ] <- FALSE
  ech[
    ech$echosondeur %in% c('nd', 'O/N', 'P', '?', '17', '2', '3'),
    'echosondeur'
  ] <- NA
  ##
  ##
  ech$anneeGestion <- as.numeric(ech$anneeGestion)
  # plot(ech$annee, ech$anneeGestion)
  ech$nbHeures[which(ech$nbHeures < 0.25)] <- 0.25 #si le nombre d'heure est inférieur à 0.25 alors on met 0.25
  ech$nbGadide <- ech$nbMorue + ech$nbOgac
  ech[which(ech$anneeGestion < 2001), c('nbMorue', 'nbOgac')] <- NA
  ech$nbHeures <- as.numeric(ech$nbHeures)
  ech$ue <- ech$nbLignes * ech$nbHamecons * ech$nbHeures #unite d'effort
  ech$logUE <- log(ech$ue)
  ech$sfs.ori <- ech$sfs
  ech$sfs <- as.numeric(wday(ech$date) %in% c(1, 7)) #semaine=0, fds=1
  ## as.numeric(wday(as.POSIXct('2023-11-01')) %in% c(1,7))
  ## as.numeric(wday(as.POSIXct('2023-10-29')) %in% c(1,7))
  ## which(ech$sfs.ori != ech$sfs)
  ##plot(wday(ech[ech$annee==2022,'date']), ech[ech$annee==2022,'sfs.ori'])
  if (FALSE) {
    #exploration du nombre d'heures de pêche
    nrow(ech[which(ech$secteur == "fond"), ])
    nrow(ech[
      which(ech$secteur == "fond" & ech$nbHeures <= 12),
    ])
    nrow(ech[which(ech$secteur == "fond" & ech$nbHeures < 12), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nbHeures <= 8), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nbHeures < 8), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nbHeures <= 6), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nbHeures < 6), ])
    ##
    hist(
      ech[, 'nbHeures'],
      breaks = seq(-0.25, 200, by = 1),
      xlim = c(0, 30),
      ylim = c(0, 0.2),
      main = 'Nombre heures de pêche',
      freq = FALSE
    )
    x <- hist(
      ech[ech$annee %in% c(1994:1998), 'nbHeures'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 1)
    x <- hist(
      ech[ech$annee %in% c(1999:2003), 'nbHeures'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 2)
    x <- hist(
      ech[ech$annee %in% c(2004:2008), 'nbHeures'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 3)
    x <- hist(
      ech[ech$annee %in% c(2009:2013), 'nbHeures'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 4)
    x <- hist(
      ech[ech$annee %in% c(2014:2018), 'nbHeures'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 5)
    x <- hist(
      ech[ech$annee %in% c(2019:2023), 'nbHeures'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 6)
  }
  table(ech$annee, ech$secteur, useNA = 'ifany')
  ech <- ech[which(ech$secteur %in% c('fond')), ]

  ##
  return(ech)
}
