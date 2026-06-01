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
  ## ajouter les données post-2021
}
