#' Créer les fichier .csv consolidant données historiques de chacune des trois sources (pré-2021)
#'
#' Cette fonction lit les fichiers de données dans un dossier source,
#' applique les transformations nécessaires, puis sauvegarde en format .csv dans le dossier destination
#'
#' Ce code n'a pas à être exécuté pour réaliser l'évaluation, mais permet le suivi du traitement des données anciennes.
#' Les trois sources de données consolidées ici sont:
#'  - Les données biologiques
#'  - Les journaux de bord
#'  - les échantillonneurs
#'
#' @param input_dir Chemin vers le dossier contenant les fichiers de données brutes
#' @param output_dir Chemin vers le dossier où sauvegarder les fichier .csv produits
#'
#' @returns Aucun objet n'est retourné.
#' Les fichiers générés sont sauvegardés dans le dossier spécifié par \code{output_dir}
#' @export
#'
#' @examples
lire_donnees_pre2021 <- function(input_dir, output_dir) {
  ## données biologiques
  db.2020etMoins <- consoliderDonneesBio(dirInput = file.path(input_dir, 'DB'))
  save(
    db.2020etMoins,
    file = file.path(dirSag, 'Données', 'donneeBiologique_1995-2020.RData')
  )
  write.csv2(
    db.2020etMoins,
    file = file.path(dirSag, 'Données', 'donneeBiologique_1995-2020.csv')
  )

  ## journaux de bord
  jb.2020etMoins <- consoliderJournauxBords(
    dirInput = file.path(dirSag, 'Pêche hivernale', 'JB')
  )
  save(
    jb.2020etMoins,
    file = file.path(dirSag, 'Données', 'journauxBords_2015-2020.RData')
  )
  write.csv2(
    jb.2020etMoins,
    file = file.path(dirSag, 'Données', 'journauxBords_2015-2020.csv')
  )

  ## échantillonneurs
  ech.2020etMoins.init <- consoliderEchantillonneurs(
    dirInput = file.path(dirSag, 'Pêche hivernale')
  )
  ech.2020etMoins <- ech.2020etMoins.init[['ech']]
  ech.2020etMoins.complete <- ech.2020etMoins.init[['ech.prime']]
  save(
    ech.2020etMoins,
    file = file.path(dirSag, 'Données', 'echantillonneurs_1995-2020.RData')
  )
  write.csv2(
    ech.2020etMoins,
    file = file.path(dirSag, 'Données', 'echantillonneurs_1995-2020.csv')
  )
  save(
    ech.2020etMoins.complete,
    file = file.path(
      dirSag,
      'Données',
      'echantillonneurs_1995-2020_donneesCompletes.RData'
    )
  )
  write.csv2(
    ech.2020etMoins.complete,
    file = file.path(
      dirSag,
      'Données',
      'echantillonneurs_1995-2020_donneesCompletes.csv'
    )
  )
}
