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
    ech.new$date <- as.POSIXct(
      paste(ech.new$annee, ech.new$mois, ech.new$jour, sep = '-'),
      format = '%Y-%m-%d'
    )
    ech.new$site <- standardiser_nom_site(site = ech.new$site)[, 'sites']
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
  ## table(ech$sonar)
  ech[
    ech$sonar %in% c('O', 'o', 'oui', 'OUI', TRUE),
    'sonar'
  ] <- 1
  ech[
    ech$sonar %in% c('N', 'n', 'non', 'NON', FALSE),
    'sonar'
  ] <- 0
  ech[
    ech$sonar %in% c('nd', 'O/N', 'P', '?', '17', '2', '3'),
    'sonar'
  ] <- NA
  ##
  ##
  ech$anneeGestion <- as.numeric(ech$anneeGestion)
  # plot(ech$annee, ech$anneeGestion)
  ech$nb_heure <- as.numeric(ech$nb_heure)
  ech$nb_heure[which(ech$nb_heure < 0.25)] <- 0.25 #si le nombre d'heure est inférieur à 0.25 alors on met 0.25
  ech$gadide <- ech$morue + ech$ogac
  ech[which(ech$anneeGestion < 2001), c('morue', 'ogac')] <- NA
  ech$ue <- ech$nb_ligne * ech$nb_hamecon * ech$nb_heure #unite d'effort
  ech$logUE <- log(ech$ue)
  ech$sfs <- ech[, 'semaine(0)_FinDeSemaine(1)']
  ech$sfs.ori <- ech$sfs
  ech$sfs <- as.numeric(wday(ech$date) %in% c(1, 7)) #semaine=0, fds=1
  ## as.numeric(wday(as.POSIXct('2023-11-01')) %in% c(1,7))
  ## as.numeric(wday(as.POSIXct('2023-10-29')) %in% c(1,7))
  ## which(ech$sfs.ori != ech$sfs)
  if (FALSE) {
    #exploration du nombre d'heures de pêche
    nrow(ech[which(ech$secteur == "fond"), ])
    nrow(ech[
      which(ech$secteur == "fond" & ech$nb_heure <= 12),
    ])
    nrow(ech[which(ech$secteur == "fond" & ech$nb_heure < 12), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nb_heure <= 8), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nb_heure < 8), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nb_heure <= 6), ])
    nrow(ech[which(ech$secteur == "fond" & ech$nb_heure < 6), ])
    ##
    hist(
      ech[, 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      xlim = c(0, 30),
      ylim = c(0, 0.2),
      main = 'Nombre heures de pêche',
      freq = FALSE
    )
    x <- hist(
      ech[ech$annee %in% c(1994:1998), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 1)
    x <- hist(
      ech[ech$annee %in% c(1999:2003), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 2)
    x <- hist(
      ech[ech$annee %in% c(2004:2008), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 3)
    x <- hist(
      ech[ech$annee %in% c(2009:2013), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 4)
    x <- hist(
      ech[ech$annee %in% c(2014:2018), 'nb_heure'],
      breaks = seq(-0.25, 200, by = 1),
      plot = FALSE
    )
    lines(x$mids, x$density, col = 5)
    x <- hist(
      ech[ech$annee %in% c(2019:2023), 'nb_heure'],
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
