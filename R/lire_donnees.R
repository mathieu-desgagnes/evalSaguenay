lire_donnees <- function(
  dir_input,
  dir_output
) {
  ##
  ## vérifier si les données biologiques pré-2021 existent et les charger
  if (
    file.exists(file.path(dir_input, 'donnees_ech.RData')) &&
      file.exists(file.path(dir_input, 'donnees_DB.RData')) &&
      file.exists(file.path(dir_input, 'donnees_JB.RData'))
  ) {
    data <- list()
    load(file = file.path(dir_input, 'donnees_ech.RData'), verbose = TRUE)
    data$ech <- ech.2020etMoins
    load(file = file.path(dir_input, 'donnees_JB.RData'), verbose = TRUE)
    data$jb <- jb.2020etMoins
    load(file = file.path(dir_input, 'donnees_DB.RData'), verbose = TRUE)
    data$db <- db.2020etMoins
  } else {
    stop('Vérifier la disponibilité des données pré-2021.')
  }

  ##
  ##
  ## ajouter les données de 2021 et plus
  db <- ajout_donnees_bio(
    donnees_2020_et_moins = data$db,
    input_2021_et_plus_dir = dir_input
  )
  ## sauvegarder la base de donnée consolidée
  save(db, file = file.path(dir_input, 'donnees_DB.RData'))
  write.csv2(db, file = file.path(dir_input, 'donnees_DB.csv'))

  jb <- ajoutJournauxBords(
    fichier2020etMoins = file.path(
      dirSag,
      'Données',
      'journauxBords_2015-2020.RData'
    ),
    dirInput2021etPlus = file.path(dirSag, 'Données')
  )
  ## sauvegarder la base de donnée consolidée
  save(jb, file = file.path(dirSag, 'Données', 'donnees_JB.RData'))
  write.csv2(jb, file = file.path(dirSag, 'Données', 'donnees_JB.csv'))

  ech <- ajoutEchantillonneurs(
    fichier2020etMoins = file.path(
      dirSag,
      'Données',
      'echantillonneurs_1995-2020.RData'
    ),
    dirInput2021etPlus = file.path(dirSag, 'Données')
  )
  ## sauvegarder la base de donnée consolidée
  save(ech, file = file.path(dirSag, 'Données', 'donnees_ech.RData'))
  write.csv2(ech, file = file.path(dirSag, 'Données', 'donnees_ech.csv'))
  ## load(file.path(dirSag,'Données','donnees_ech.RData'), verbose=1)
}
