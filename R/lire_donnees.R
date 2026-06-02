lire_donnees <- function(
  dir_input,
  dir_output
) {
  ##
  ## vérifier si les données biologiques pré-2021 existent et les charger
  if (
    file.exists(file.path(dir_input, 'donneeBiologique_1995-2020.RData')) &&
      file.exists(file.path(dir_input, 'journauxBords_2015-2020.RData')) &&
      file.exists(file.path(dir_input, 'echantillonneurs_1995-2020.RData'))
  ) {
    data <- list()
    load(
      file = file.path(dir_input, 'donneeBiologique_1995-2020.RData'),
      verbose = TRUE
    )
    data$db <- db.2020etMoins
    load(
      file = file.path(dir_input, 'journauxBords_2015-2020.RData'),
      verbose = TRUE
    )
    data$jb <- jb.2020etMoins
    load(
      file = file.path(dir_input, 'echantillonneurs_1995-2020.RData'),
      verbose = TRUE
    )
    data$ech <- ech.2020etMoins
  } else {
    stop('Vérifier la disponibilité des données pré-2021.')
  }

  ##
  ##
  ## données biologiques: ajouter les données de 2021 et plus aux données historiques
  db <- ajout_donnees_bio(
    donnees_2020_et_moins = data$db,
    input_2021_et_plus_dir = dir_input
  )
  ## sauvegarder la base de donnée consolidée
  save(db, file = file.path(dir_input, 'donnees_DB.RData'))
  write.csv2(db, file = file.path(dir_input, 'donnees_DB.csv'))

  ######
  ##
  ## PROBLÈME ICI AVEC LA LECTURE DES NOUVEAUX JOURNAUX DE BORD
  ## Le fichier semble changer de format à 2023
  ##
  ######

  ##
  ##
  ## journaux de bord: ajouter les données de 2021 et plus aux données historiques
  jb <- ajout_journaux_de_bord(
    donnees_2020_et_moins = data$jb,
    input_2021_et_plus_dir = dir_input
  )
  ## sauvegarder la base de donnée consolidée
  save(jb, file = file.path(dir_input, 'donnees_JB.RData'))
  write.csv2(jb, file = file.path(dir_input, 'donnees_JB.csv'))

  ##
  ##
  ## échantillonneurs: ajouter les données de 2021 et plus aux données historiques
  ech <- ajout_echantillonneurs(
    donnees_2020_et_moins = data$ech,
    input_2021_et_plus_dir = dir_input
  )
  ## sauvegarder la base de donnée consolidée
  save(ech, file = file.path(dir_input, 'donnees_ech.RData'))
  write.csv2(ech, file = file.path(dir_input, 'donnees_ech.csv'))

  ##
  ##
  ## application Glaces du Fjord: lire les données à partir de 2025
  gdf <- lire_glaces_du_fjord(
    input_2021_et_plus_dir = dir_input
  )
  ## sauvegarder la base de donnée consolidée
  save(ech, file = file.path(dir_input, 'donnees_ech.RData'))
  write.csv2(ech, file = file.path(dir_input, 'donnees_ech.csv'))

  ##
  ## Exploration des résultats
  if (FALSE) {
    jb$pue <- jb$nbTot / (jb$nbHeureImmersion * 60 + jb$nbMinuteImmersion)
    ech$nbTot <- apply(
      ech[, c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')],
      1,
      sum,
      na.rm = TRUE
    )
    hist(ech$nbTot, breaks = seq(0, 650), xlim = c(0, 15))
    ech$pue <- apply(
      ech[, c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')],
      1,
      sum,
      na.rm = TRUE
    ) /
      (as.numeric(ech$nbHeures) * 60)
    ##
    hist(
      apply(
        ech[
          ech$annee == 2019,
          c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')
        ],
        1,
        sum,
        na.rm = TRUE
      ),
      breaks = seq(0, 650),
      xlim = c(0, 15),
      main = 2019
    )
    hist(
      apply(
        ech[
          ech$annee == 2022,
          c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')
        ],
        1,
        sum,
        na.rm = TRUE
      ),
      breaks = seq(0, 650),
      xlim = c(0, 15),
      main = 2022
    )
    hist(
      apply(
        ech[
          ech$annee == 2023,
          c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')
        ],
        1,
        sum,
        na.rm = TRUE
      ),
      breaks = seq(0, 650),
      xlim = c(0, 15),
      main = 2023
    )
    hist(
      apply(
        ech[
          ech$annee == 2024,
          c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')
        ],
        1,
        sum,
        na.rm = TRUE
      ),
      breaks = seq(0, 650),
      xlim = c(0, 15),
      main = 2024
    )
    hist(
      apply(
        ech[
          ech$annee == 2025,
          c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')
        ],
        1,
        sum,
        na.rm = TRUE
      ),
      breaks = seq(0, 650),
      xlim = c(0, 15),
      main = 2025
    )
    hist(
      apply(
        ech[
          ech$annee == 2026,
          c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')
        ],
        1,
        sum,
        na.rm = TRUE
      ),
      breaks = seq(0, 650),
      xlim = c(0, 15),
      main = 2026
    )
    ##
    par(mfrow = c(1, 2))
    boxplot(pue ~ annee, data = jb, ylim = c(0, 0.05))
    boxplot(
      pue ~ annee,
      data = subset(ech, secteur == 'fond' & annee %in% 2015:2026),
      ylim = c(0, 0.05)
    )
    ##

    pdf(file.path('dev', 'exploration.pdf'), height = 8.5, width = 14)
    # vioplot(pue~annee, data=na.omit(subset(ech,secteur=='fond'&is.finite(pue))[,c('annee','pue')]), ylim=c(0,0.03))
    # for(i.an in 1995:2023){
    #   par(mfrow=c(1,3))
    #   temp <- subset(ech, annee==i.an & secteur=='fond')
    #   if(nrow(temp)>0){
    #       hist(temp$pue, main=i.an, breaks=seq(0,10,by=0.002), xlim=c(0,0.1), freq=FALSE, ylim=c(0,100))
    #       ## boxplot(pue~annee, data=subset(ech,secteur=='fond'), ylim=c(0,0.05))
    #   }else{
    #       frame()
    #   }
    #   temp <- subset(jb, annee==i.an)
    #   if(nrow(temp)>0){
    #       hist(temp$pue, main=i.an, breaks=seq(0,10,by=0.002), xlim=c(0,0.1), freq=FALSE, ylim=c(0,100))
    #       ## boxplot(pue~annee, data=subset(ech,secteur=='fond'), ylim=c(0,0.05))
    #       hist(temp$nbTot, main=i.an, breaks=seq(0,100,by=1), xlim=c(0,10))
    #   }
  }
}
