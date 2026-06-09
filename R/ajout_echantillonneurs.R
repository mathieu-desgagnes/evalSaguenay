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
      sheet = 1,
      na = 'NA'
    ))
  }

  # names(ech.new) <- c(
  #   'affiliationEchantillonneur',
  #   'echantillonneur',
  #   'annee',
  #   'mois',
  #   'jour',
  #   'heure',
  #   'sfs',
  #   'site',
  #   'nbPecheursFond',
  #   'noPecheur',
  #   'secteur',
  #   'engin',
  #   'nbLignes',
  #   'nbHamecons',
  #   'nb_heure',
  #   'sonar',
  #   'prof',
  #   'nbMorue',
  #   'nbOgac',
  #   'nbSebastes',
  #   'nbTurbot',
  #   'nbCrabe',
  #   'nbLycode',
  #   'nbRaie',
  #   'nbMerluche',
  #   'nbEperlan',
  #   'nbFletan',
  #   'nbSaida',
  #   'commentaires'
  # )
  ech.new$date <- as.POSIXct(
    paste(ech.new$annee, ech.new$mois, ech.new$jour, sep = '-'),
    format = '%Y-%m-%d'
  )
  ech.new$site <- standardiser_nom_site(site = ech.new$site)[,
    'sites'
  ]
  ech.new$jourSemaine <- lubridate::wday(ech.new$date) #1=dimanche
  ech.new$lundiAuVendredi <- ech.new$jourSemaine %in% 2:6
  ##
  ech.new$anneeGestion <- ech.new$annee
  ##

  ech.init <- merge(ech.init, ech.new, all = TRUE)
  ## nrow(ech.init); nrow(ech.new); nrow(ech)

  ##
  ## nrow(ech.init)
  ech.init$engin <- standardiser_nom_engin(ech.init$engin)
  ## table(ech.init$sonar)
  ech.init[
    ech.init$sonar %in% c('O', 'o', 'oui', 'OUI', TRUE),
    'sonar'
  ] <- 1
  ech.init[
    ech.init$sonar %in% c('N', 'n', 'non', 'NON', FALSE),
    'sonar'
  ] <- 0
  ech.init[
    ech.init$sonar %in% c('nd', 'O/N', 'P', '?', '17', '2', '3'),
    'sonar'
  ] <- NA
  ##
  ##
  ech.init$anneeGestion <- as.numeric(ech.init$anneeGestion)
  # plot(ech.init$annee, ech.init$anneeGestion)
  ech.init$nb_heure <- as.numeric(ech.init$nb_heure)
  ech.init$nb_heure[which(ech.init$nb_heure < 0.25)] <- 0.25 #si le nombre d'heure est inférieur à 0.25 alors on met 0.25
  ech.init$gadide <- ech.init$morue + ech.init$ogac
  ech.init[which(ech.init$anneeGestion < 2001), c('morue', 'ogac')] <- NA
  ech.init$ue <- ech.init$nb_ligne * ech.init$nb_hamecon * ech.init$nb_heure #unite d'effort
  ech.init$logUE <- log(ech.init$ue)
  ech.init$sfs <- ech.init[, 'semaine(0)_FinDeSemaine(1)']
  ech.init$sfs.ori <- ech.init$sfs
  ech.init$sfs <- as.numeric(wday(ech.init$date) %in% c(1, 7)) #semaine=0, fds=1
  ## as.numeric(wday(as.POSIXct('2023-11-01')) %in% c(1,7))
  ## as.numeric(wday(as.POSIXct('2023-10-29')) %in% c(1,7))
  ## which(ech.init$sfs.ori != ech.init$sfs)
  if (FALSE) {
    #exploration du nombre d'heures de pêche
    nrow(ech.init[which(ech.init$secteur == "fond"), ])
    nrow(ech.init[
      which(ech.init$secteur == "fond" & ech.init$nb_heure <= 12),
    ])
    nrow(ech.init[which(ech.init$secteur == "fond" & ech.init$nb_heure < 12), ])
    nrow(ech.init[which(ech.init$secteur == "fond" & ech.init$nb_heure <= 8), ])
    nrow(ech.init[which(ech.init$secteur == "fond" & ech.init$nb_heure < 8), ])
    nrow(ech.init[which(ech.init$secteur == "fond" & ech.init$nb_heure <= 6), ])
    nrow(ech.init[which(ech.init$secteur == "fond" & ech.init$nb_heure < 6), ])
    ##
    hist(
      ech.init[, 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      xlim = c(0, 30),
      ylim = c(0, 0.2),
      main = 'Nombre heures de pêche',
      freq = FALSE
    )
    x <- hist(
      ech.init[ech.init$annee %in% c(1994:1998), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 1)
    x <- hist(
      ech.init[ech.init$annee %in% c(1999:2003), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 2)
    x <- hist(
      ech.init[ech.init$annee %in% c(2004:2008), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 3)
    x <- hist(
      ech.init[ech.init$annee %in% c(2009:2013), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 4)
    x <- hist(
      ech.init[ech.init$annee %in% c(2014:2018), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 5)
    x <- hist(
      ech.init[ech.init$annee %in% c(2019:2023), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 6)
  }
  table(ech.init$annee, ech.init$secteur, useNA = 'ifany')
  ech <- ech.init[which(ech.init$secteur %in% c('fond')), ]

  ##
  return(ech)
}
