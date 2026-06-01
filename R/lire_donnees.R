lire_donnees <- function(
  dir_input = file.path('S:', 'Saguenay', 'Données'),
  dir_output = file.path(
    'C:',
    'Users',
    'DesgagnesMa',
    'Documents',
    'evalSaguenay2026',
    'output'
  )
) {
  ## vérifier si les données biologiques pré-2021 existent, les recalculer sinon
  if (NULL) {
    load(file = file.path(dir.input, 'donnees_ech.RData'), verbose = TRUE)
  } #pour reprendre une session précédente sans refaire tous les calculs
  file.info(file.path(dir.input, 'donnees_ech.RData'))$mtime
  ## ech.init <- read.table(file=file.path(dir.input, 'donnees_ech.csv'), sep=';',dec=, na='', header=T, as.is=TRUE)
  ech.init <- ech
  load(file = file.path(dir.input, 'donnees_jb.RData'), verbose = TRUE) #pour reprendre une session précédente sans refaire tous les calculs
  load(file = file.path(dir.input, 'donnees_db.RData'), verbose = TRUE) #pour reprendre une session précédente sans refaire tous les calculs
}
