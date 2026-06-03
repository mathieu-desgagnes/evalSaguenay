#' Calcul l'effort de pêche sur le Saguenay et produit les graphiques correspondants.
#'
#' Note à valider:
#' Certaines figures limites l'effort aux activités de moins de 12 heures...
#'
#' @param ech données récolétées dans le volet 1 du projet (échantillonneurs).
#' @param dates_officielles_fichier Chemin vers le fichier de format .csv contenant les dates officielles
#' d'ouverture et de fermeture des sites de pêche
#' @param output_dir chemin vers le répertoire où sont enregistrés les graphiques et données correspondantes.
#' Doit contenir les dossiers 'fr','en','bil'et'csv'
#'
#' @returns Aucun objet est retourné mais des fichiers sont créés (images et données).
#' @export
#'
#' @examples ##À venir
effort_de_peche <- function(ech, dates_officielles_fichier, output_dir) {
  ech$anneeGestion <- as.numeric(ech$anneeGestion)
  # plot(ech$annee, ech$anneeGestion)
  annees <- sort(unique(ech$anneeGestion))
  # ech <- ech[ech$anneeGestion %in% annees, ]
  sites <- c(
    'AnseBenjamin',
    'AnseStJean',
    'GrandeBaie',
    'LesBattures',
    'RiviereEternite',
    'SteRose',
    'StFelix',
    'StFulgence'
  )

  ## préparation des données
  ##
  ## echantillons
  ech$nbHeures <- as.numeric(ech$nbHeures)
  ech$nbHeures[which(ech$nbHeures < 0.25)] <- 0.25 #si le nombre d'heure est inférieur à 0.25 alors on met 0.25
  ech$nbGadide <- ech$nbMorue + ech$nbOgac
  ech[ech$anneeGestion < 2001, c('nbMorue', 'nbOgac')] <- NA
  ech$ue <- ech$nbLignes * ech$nbHamecons * ech$nbHeures #unite d'effort
  ech$logUE <- log(ech$ue)
  ech$sfs.ori <- ech$sfs
  ech$sfs <- as.numeric(wday(ech$date) %in% c(1, 7)) #semaine=0, fds=1
  ## as.numeric(wday(as.POSIXct('2023-11-01')) %in% c(1,7))
  ## as.numeric(wday(as.POSIXct('2023-10-29')) %in% c(1,7))
  ## which(ech$sfs.ori != ech$sfs)
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
  ech <- ech[which(ech$secteur %in% c('fond', 'Fond')), ]

  ## visites
  ## lorsqu'il y a plusieurs échantillons (visites) d'un même site une même journée,
  ## faire la moyenne des nombres de pêcheurs
  ## modif: ne pas tenir compte du nombre de pêcheurs Pelag -> retire 7 cas seulement
  a.enlever <- vector() #lignes de "fausses" visite à retirer de la liste après traitement
  visites.ori <- ech[
    !duplicated(ech[, c(
      "anneeGestion",
      "date",
      "nomSite",
      "sfs",
      "nbPecheursFond",
      "echantillonneur"
    )]),
  ]
  visites.temp <- visites.ori[
    order(visites.ori[, 'date'], visites.ori[, 'nomSite']),
  ]
  visites.temp$rowNb <- 1:nrow(visites.temp)
  for (i.vis in 1:(length(visites.temp$rowNb) - 1)) {
    j <- i.vis + 1
    if (
      visites.temp$date[i.vis] == visites.temp$date[i.vis + 1] &
        visites.temp$nomSite[i.vis] == visites.temp$nomSite[i.vis + 1]
    ) {
      while (
        j != nrow(visites.temp) &
          visites.temp$date[j] == visites.temp$date[j + 1] &
          visites.temp$nomSite[j] == visites.temp$nomSite[j + 1]
      ) {
        j <- j + 1
      }
      if (!is.na(mean(visites.temp$nbPecheursFond[i.vis:j], na.rm = TRUE))) {
        visites.temp$nbPecheursFond[i.vis] <- mean(
          visites.temp$nbPecheursFond[i.vis:j],
          na.rm = TRUE
        )
      }
      if (!is.na(mean(visites.temp$nbPecheursPelag[i.vis:j], na.rm = TRUE))) {
        visites.temp$nbPecheursPelag[i.vis] <- mean(
          visites.temp$nbPecheursPelag[i.vis:j],
          na.rm = TRUE
        )
      }
      a.enlever <- c(a.enlever, (i.vis + 1):j)
    }
  }
  visites.init <- visites.temp[
    -a.enlever,
    c(
      'anneeGestion',
      'date',
      'nomSite',
      'sfs',
      'secteur',
      'nbPecheursFond',
      'echantillonneur',
      'annee',
      'engin',
      'affiliationEchantillonneur',
      'mois',
      'jour',
      'jourSemaine',
      'lundiAuVendredi',
      'nbPecheursPelag',
      'noSite',
      'especeVisee',
      'nbPecheursTot',
      'nbCabanesOccFond',
      'nbPecheursFondSScabane',
      'nbCabanesOccPelag',
      'nbPecheursPelagSScabane',
      'nValideNbPecheursPelag',
      'nValideNbPecheursFond',
      'nValideNbTotPecheurs',
      'nbPecheursSecteur',
      'nbPecheursMorue',
      'nbPecheursSebastes',
      'pecheurDedie',
      'nomSite.ori'
    )
  ]
  ##
  ## nrow(visites.temp); nrow(visites.init)
  ## étude des données de visites
  if (FALSE) {
    ## à plusieurs endroit, il semble y avoir "visite multiple" simplement parce que la deuxième ligne est "NA" pour
    ## le nombre de pêcheurs
    ## l'approche de faire la moyenne gere ces cas sans difficulté
    visites.ori <- ech[
      !duplicated(ech[, c(
        "anneeGestion",
        "date",
        "nomSite",
        "sfs",
        "nbPecheursFond",
        "echantillonneur"
      )]),
    ]
    visites.temp <- visites.ori[
      order(visites.ori[, 'date'], visites.ori[, 'nomSite']),
    ]
    visites.temp$rowNb <- 1:nrow(visites.temp)
    temp <- NULL
    for (i.vis in 1:(length(visites.temp$rowNb) - 1)) {
      j <- i.vis + 1
      if (
        visites.temp$date[i.vis] == visites.temp$date[i.vis + 1] &
          visites.temp$nomSite[i.vis] == visites.temp$nomSite[i.vis + 1]
      ) {
        while (
          j != nrow(visites.temp) &
            visites.temp$date[j] == visites.temp$date[j + 1] &
            visites.temp$nomSite[j] == visites.temp$nomSite[j + 1]
        ) {
          j <- j + 1
        }
        temp <- rbind(temp, visites.temp[i.vis:j, ])
      }
    }
    write.csv2(temp, file = file.path(output_dir, 'exploDonneesVisite.csv'))
  }

  aggregate(
    visites.init$nbPecheursFond,
    visites.init['anneeGestion'],
    FUN = sum,
    na.rm = TRUE
  )

  ## nombre de visites par année
  plot(aggregate(
    visites.init$anneeGestion,
    visites.init['anneeGestion'],
    FUN = length
  ))
  ## nombre de pêcheurs interrogés par année
  plot(aggregate(ech$anneeGestion, ech['anneeGestion'], FUN = length))
  abline(
    h = mean(aggregate(ech$anneeGestion, ech['anneeGestion'], FUN = length)$x)
  )
  mean(tail(
    aggregate(ech$anneeGestion, ech['anneeGestion'], FUN = length)$x,
    15
  ))
  # ## nombre de poisson mesurés
  # plot(aggregate(db$anneeGestion, db['anneeGestion'], FUN = length))
  # temp <- aggregate(db$anneeGestion, db['anneeGestion'], FUN = length)
  # mean(tail(temp$x, 10))
  # ## nombre de journaux de bord
  # plot(aggregate(jb$anneeGestion, jb['anneeGestion'], FUN = length))
  # mean(aggregate(jb$anneeGestion, jb['anneeGestion'], FUN = length)$x)
  # mean(table(
  #   aggregate(
  #     jb$anneeGestion,
  #     jb[, c('echantillonneur', 'anneeGestion')],
  #     FUN = length
  #   )$anneeGestion
  # ))

  ## nbespeces autres
  ## dimnames(ech)[[2]][which(substring(dimnames(ech)[[2]], 1, 2)=='nb')]
  ech$nbAutre <- as.numeric(ech$nbAutre)
  ech$nbToutAutre <- apply(
    ech[, c(
      "nbFletan",
      "nbLycode",
      "nbSaida",
      "nbRaie",
      "nbMerluche",
      "nbAutre",
      "nbAutre2",
      "nbLimace",
      "nbPlie",
      "nbGoberge",
      "nbHareng",
      "nbMerlucheEcureuil"
    )],
    1,
    sum,
    na.rm = TRUE
  )
  ## aggregate(ech$nbToutAutre, ech['anneeGestion'], FUN=sum, na.rm=TRUE)

  ##
  ## clacul de nombre de jour de pêche par site par année
  ## déterminer le nombre de jours entre premier et dernier échantillon d'une année (de fin de semaine et de semaine)
  sfs.nbJour.an <- array(
    dim = c(length(annees), 2),
    dimnames = list(annee = annees, c('semaine', 'fds'))
  )
  for (i.an in seq_along(annees)) {
    cetteAnnee <- visites.init[
      which(visites.init$anneeGestion == annees[i.an]),
    ]
    if (nrow(cetteAnnee) > 0) {
      temp <- wday(seq(
        min(cetteAnnee$date, na.rm = TRUE),
        max(cetteAnnee$date, na.rm = TRUE),
        by = 'day'
      ))
      temp2 <- month(seq(
        min(cetteAnnee$date, na.rm = TRUE),
        max(cetteAnnee$date, na.rm = TRUE),
        by = 'day'
      ))
      if (annees[i.an] %in% c(2015:2016)) {
        #semaine, mesure de gestion interdisant la pêche les mardi durant ces années
        sfs.nbJour.an[i.an, 'semaine'] <- length(which(
          temp %in% c(1, 3:5) & temp2 %in% c(1:2)
        )) +
          length(which(temp %in% c(1:5) & temp2 %in% c(3)))
      } else {
        sfs.nbJour.an[i.an, 'semaine'] <- length(which(temp %in% 1:5))
      }
      sfs.nbJour.an[i.an, 'fds'] <- length(which(temp %in% c(6:7))) #fin de semaine
    } else {
      sfs.nbJour.an[i.an, ] <- c(0, 0)
    }
  }

  ##
  ## dates d'échantillonnage
  ## lire les dates officielles d'ouverture et de fermeture
  datesOfficielles <- read.csv2(dates_officielles_fichier) # dates officielles
  ## nbJourJDL <- read.csv2("longueurSaisonJDL.csv",header=TRUE)  # longueurs de saisons utilisées par JDL (avant 2010) (non-utilisé depuis)
  datesOfficielles$ouverture <- as.POSIXct(paste(
    datesOfficielles$anneeOuverture,
    datesOfficielles$moisOuverture,
    datesOfficielles$jourOuverture,
    sep = '-'
  ))
  datesOfficielles$fermeture <- as.POSIXct(paste(
    datesOfficielles$anneeFermeture,
    datesOfficielles$moisFermeture,
    datesOfficielles$jourFermeture,
    sep = '-'
  ))
  nomPng <- 'date_ech'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 9,
      units = 'in',
      res = 300
    )
    temp <- graph_dates_ech(
      ech = ech,
      visites = visites.init,
      ouvertureOfficielle = datesOfficielles[, c(
        'anneeGestion',
        'ouverture',
        'fermeture'
      )],
      langue = i.langue
    )
    dev.off()
  }
  ## write.csv2(temp, file=file.path(dir.output,'csv',paste0(nomPng,'.csv')))

  ########
  ## Attention ici: on restreint aux efforts de moins de 12 heures...
  ########

  ##
  ## effort de pêche par site
  ##
  nomPng <- 'effort_peche_par_site'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 9,
      units = 'in',
      res = 300
    )
    # temp <- graph_effort_peche_par_site(
    #   ech = subset(ech, nbHeures < 12),
    #   visites = visites.init,
    #   langue = i.langue
    # )
    temp <- graph_effort_peche_par_site(
      ech = ech,
      visites = visites.init,
      langue = i.langue
    )
    dev.off()
  }
  nbPecheursMoy.site.sfs <- temp
  nbPecheursTot.site <- array(
    dim = dim(nbPecheursMoy.site.sfs)[1:2],
    dimnames = dimnames(nbPecheursMoy.site.sfs)[1:2]
  )
  for (i.an in seq_along(annees)) {
    for (i.site in seq_along(sites)) {
      nbPecheursTot.site[sites[i.site], as.character(annees[i.an])] <-
        sum(
          nbPecheursMoy.site.sfs[
            sites[i.site],
            as.character(annees[i.an]),
            c('semaine', 'fds'),
            'moyenne'
          ] *
            sfs.nbJour.an[as.character(annees[i.an]), c('semaine', 'fds')]
        )
    }
  }
  write.csv2(
    nbPecheursTot.site,
    file = file.path(output_dir, 'csv', paste0(nomPng, '.csv'))
  )
  save(
    nbPecheursTot.site,
    file = file.path(output_dir, 'csv', 'nbPecheursTot_site.RData')
  )
  write.csv2(
    nbPecheursMoy.site.sfs,
    file = file.path(output_dir, 'csv', 'nbPecheursMoy_strate.csv')
  )
  save(
    nbPecheursMoy.site.sfs,
    file = file.path(output_dir, 'csv', 'nbPecheursMoy_strate.RData')
  )

  load(
    file = file.path(output_dir, 'csv', 'nbPecheursTot_site.RData'),
    verbose = 1
  )
  load(
    file = file.path(output_dir, 'csv', 'nbPecheursMoy_strate.RData'),
    verbose = 1
  )

  ##
  ## effort de pêche total
  ##
  nomPng <- 'effort_peche'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 5,
      width = 8,
      units = 'in',
      res = 300
    )
    temp <- graph_effort_peche(
      nbPecheurs = nbPecheursMoy.site.sfs,
      nbJours = sfs.nbJour.an,
      langue = i.langue
    )
    dev.off()
  }
  nbPecheursTot.secteur <- temp
  write.csv2(temp, file = file.path(output_dir, 'csv', paste0(nomPng, '.csv')))
  save(
    nbPecheursTot.secteur,
    file = file.path(output_dir, 'csv', paste0(nomPng, '.RData'))
  )

  load(
    file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')),
    verbose = 1
  )

  ##
  ## effort de pêche total, en barplot, 1x2
  ##
  nomPng <- 'effort_peche_barplot'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 4.5,
      width = 9,
      units = 'in',
      res = 300
    )
    temp <- graph_effort_peche_barplot(
      nbPecheursTot = nbPecheursTot.secteur,
      langue = i.langue
    )
    dev.off()
  }
  write.csv2(temp, file = file.path(output_dir, 'csv', paste0(nomPng, '.csv')))

  effort <- list(
    nb_pecheurs_moy_par_strate = nbPecheursMoy.site.sfs,
    nb_jours_par_strate = sfs.nbJour.an
  )
}
