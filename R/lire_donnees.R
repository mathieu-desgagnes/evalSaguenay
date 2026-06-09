#' Lire les données disponibles pour l'évaluation
#'
#' Quatres sources sont combinées dans la liste retournée, soit:
#'  - Les enquêtes des échantillonneurs
#'  - Les journaux de bord distribués à certains pêcheurs
#'  - Les données biologiques liées aux mesures de poissons individuels
#'  - Les informations collectées par l'application "Glaces du fjord"
#'
#' @param dir_input chemin vers le dossier où sont situées les donnée
#' @param donnees_2020_et_moins_dir chemin du répertoire des données récentes
#' @param dir_output chemin vers le dossier où sont enregistrés les fichiers d'info consolidés
#'
#' @returns une liste des données de chacune des sources
#' @export
#'
#' @examples ##À venir
lire_donnees <- function(
  donnees_2020_et_moins_dir = NULL,
  dir_input,
  dir_output
) {
  ##
  ## vérifier si les données biologiques pré-2021 existent et les charger
  if (
    file.exists(file.path(
      donnees_2020_et_moins_dir,
      'donneeBiologique_1995-2020.RData'
    )) &&
      file.exists(file.path(
        donnees_2020_et_moins_dir,
        'journauxBord_2015-2020.RData'
      )) &&
      file.exists(file.path(
        donnees_2020_et_moins_dir,
        'echantillonneurs_1995-2020.RData'
      ))
  ) {
    data <- list()
    load(
      file = file.path(
        donnees_2020_et_moins_dir,
        'donneeBiologique_1995-2020.RData'
      ),
      verbose = TRUE
    )
    data$db <- db.2020etMoins
    load(
      file = file.path(
        donnees_2020_et_moins_dir,
        'journauxBord_2015-2020.RData'
      ),
      verbose = TRUE
    )
    ##file.info( file.path(donnees_2020_et_moins_dir,'journauxBords_2015-2020.RData'))
    data$jb <- jb.2020etMoins
    load(
      file = file.path(
        donnees_2020_et_moins_dir,
        'echantillonneurs_1995-2020.RData'
      ),
      verbose = TRUE
    )
    data$ech <- ech.2020etMoins
  } else {
    stop('Vérifier la disponibilité des données pré-2021.')
  }

  ##
  ## données biologiques: ajouter les données de 2021 et plus aux données historiques
  db <- ajout_donnees_bio(
    donnees_2020_et_moins = data$db,
    input_2021_et_plus_fichier = file.path(
      dir_input,
      'Donnees_Biologiques.xlsx'
    )
  )
  ## sauvegarder la base de donnée consolidée
  save(db, file = file.path(dir_input, 'donnees_DB.RData'))
  write.csv2(db, file = file.path(dir_input, 'donnees_DB.csv'))

  ###
  ##
  ## Problème dnas les années couvertes par les deux sources, historiques et récentes, a valider
  ##
  #####
  ## journaux de bord: ajouter les données de 2021 et plus aux données historiques
  jb <- ajout_journaux_de_bord(
    donnees_2020_et_moins = data$jb,
    input_2021_et_plus_fichier = file.path(
      dir_input,
      'Donnees_Journaux_de_bord.xlsx'
    )
  )
  ## sauvegarder la base de donnée consolidée
  save(jb, file = file.path(dir_input, 'donnees_JB.RData'))
  write.csv2(jb, file = file.path(dir_input, 'donnees_JB.csv'))

  ##
  ##
  ## échantillonneurs: ajouter les données de 2021 et plus aux données historiques
  ech <- ajout_echantillonneurs(
    donnees_2020_et_moins = data$ech,
    input_2021_et_plus_fichier = file.path(
      dir_input,
      'Donnees_Echantillonneur.xlsx'
    )
  )
  ## sauvegarder la base de donnée consolidée
  save(ech, file = file.path(dir_output, 'donnees_ech.RData'))
  write.csv2(ech, file = file.path(dir_output, 'donnees_ech.csv'))

  ##
  ##
  ## application Glaces du Fjord: lire les données à partir de 2025
  gdf <- lire_glaces_du_fjord(
    input_2021_et_plus_fichier = file.path(
      dir_input,
      'Donnees_Glaces_du_fjord.xlsx'
    )
  )
  ## sauvegarder la base de donnée consolidée
  save(gdf, file = file.path(dir_input, 'donnees_gdf.RData'))
  write.csv2(gdf, file = file.path(dir_input, 'donnees_gdf.csv'))

  list(echant = ech, journaux = jb, dbio = db, glace_du_fjord = gdf)
}
