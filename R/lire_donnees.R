lire_donnees <- function() {
  dir_input <- file.path('S:', 'Saguenay', 'Données')
  dir_output <- file.path(
    'C:',
    'Users',
    'DesgagnesMa',
    'Documents',
    'evalSaguenay2026',
    'output'
  )
  if (!dir.exists(file.path(dir_output))) {
    dir.create(file.path(dir.output))
  } # créer le dossier s'il n'existe pas
  if (!dir.exists(file.path(dir_output, 'fr'))) {
    dir.create(file.path(dir_output, 'fr'))
  } # créer le dossier s'il n'existe pas
  if (!dir.exists(file.path(dir_output, 'en'))) {
    dir.create(file.path(dir_output, 'en'))
  } # créer le dossier s'il n'existe pas
  if (!dir.exists(file.path(dir_output, 'bil'))) {
    dir.create(file.path(dir_output, 'bil'))
  } # créer le dossier s'il n'existe pas
  if (!dir.exists(file.path(dir_output, 'csv'))) {
    dir.create(file.path(dir_output, 'csv'))
  } # créer le dossier s'il n'existe pas

  load(file = file.path(dir.input, 'donnees_ech.RData'), verbose = TRUE) #pour reprendre une session précédente sans refaire tous les calculs
  file.info(file.path(dir.input, 'donnees_ech.RData'))$mtime
  ## ech.init <- read.table(file=file.path(dir.input, 'donnees_ech.csv'), sep=';',dec=, na='', header=T, as.is=TRUE)
  ech.init <- ech
  load(file = file.path(dir.input, 'donnees_jb.RData'), verbose = TRUE) #pour reprendre une session précédente sans refaire tous les calculs
  load(file = file.path(dir.input, 'donnees_db.RData'), verbose = TRUE) #pour reprendre une session précédente sans refaire tous les calculs
}
