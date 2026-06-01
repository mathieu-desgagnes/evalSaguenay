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
#' @param dir_input Chemin vers le dossier contenant les fichiers de données brutes
#' @param dir_output Chemin vers le dossier où sauvegarder les fichier .csv produits
#'
#' @returns Aucun objet n'est retourné.
#' Les fichiers générés sont sauvegardés dans le dossier spécifié par \code{output_dir}
#' @export
#'
#' @examples
lire_donnees_pre2021 <- function(dir_input, dir_output) {
  ## données biologiques
  db.2020etMoins <- consolider_donnees_bio(
    input_dir = file.path(dir_input, 'DB')
  )
  write.csv2(
    db.2020etMoins,
    file = file.path(dir_output, 'donneeBiologique_1995-2020.csv')
  )
  save(db.2020etMoins, file = file.path(dir_output, 'donnees_db.RData'))

  ## journaux de bord
  jb.2020etMoins <- consolider_journaux_de_bords(
    input_dir = file.path(dir_input, 'JB')
  )
  write.csv2(
    jb.2020etMoins,
    file = file.path(dir_output, 'journauxBords_2015-2020.csv')
  )
  save(jb.2020etMoins, file = file.path(dir_output, 'donnees_jb.RData'))

  ## échantillonneurs
  ech.2020etMoins.init <- consolider_echantillonneurs(
    input_dir = dir_input
  )
  ech.2020etMoins <- ech.2020etMoins.init[['ech']]
  ech.2020etMoins.complete <- ech.2020etMoins.init[['ech.prime']]
  write.csv2(
    ech.2020etMoins,
    file = file.path(dir_output, 'echantillonneurs_1995-2020.csv')
  )
  save(ech.2020etMoins, file = file.path(dir_output, 'donnees_ech.RData'))
  write.csv2(
    ech.2020etMoins.complete,
    file = file.path(
      dir_output,
      'echantillonneurs_1995-2020_donneesCompletes.csv'
    )
  )
  save(
    ech.2020etMoins.complete,
    file = file.path(dir_output, 'donnees_ech_complet.RData')
  )
}
