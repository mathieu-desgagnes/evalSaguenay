#' Production des graphiques liés à la capture.
#'
#' @param ech tableau des données d'échantillonneurs
#' @param nb_pecheurs_moy_par_strate effort de pêche en nombre de pêcheurs
#' @param nb_jours_par_strate nombre de jours de pêche
#' @param output_dir chemin où enregistre les graphiques
#'
#' @returns
#' @export
#'
#' @examples ##à compléter
capture <- function(
  ech,
  nb_pecheurs_moy_par_strate,
  nb_jours_par_strate,
  output_dir
) {
  ##
  ## captures total, ajusté selon les strates annee-site-sfs
  ##
  nomPng <- 'capt_tot'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 10,
      units = 'in',
      res = 300
    )
    temp <- graph_capture_totale(
      capt.pecheur = ech[, c(
        'anneeGestion',
        'nomSite',
        'sfs',
        'nbSebastes',
        'nbMorue',
        'nbOgac',
        'nbTurbot',
        'nbGadide'
      )],
      nbPecheursMoy.strate = nb_pecheurs_moy_par_strate[,,, 'moyenne'],
      nbJour.strate = nb_jours_par_strate,
      langue = i.langue
    )
    dev.off()
  }
  captTot.site.sfs <- temp
  write.csv2(temp, file = file.path(output_dir, 'csv', paste0(nomPng, '.csv')))
  save(
    captTot.site.sfs,
    file = file.path(output_dir, 'csv', 'captTot.site.sfs.RData')
  )

  load(
    file = file.path(output_dir, 'csv', 'captTot.site.sfs.RData'),
    verbose = 1
  )

  ##
  ## captures total, ajusté selon les strates annee-site
  ##
  nomPng <- 'captTot2'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 10,
      units = 'in',
      res = 300
    )
    temp <- graph_capture_totale2(
      capt.pecheur = ech[, c(
        'anneeGestion',
        'nomSite',
        'nbSebastes',
        'nbMorue',
        'nbOgac',
        'nbTurbot'
      )],
      nbPecheursMoy.strate = nbPecheursTot.site,
      langue = i.langue
    )
    dev.off()
  }
  write.csv2(temp, file = file.path(output_dir, 'csv', paste0(nomPng, '.csv')))
  ##

  ##
  ## captures total, brute par annee
  ##
  nomPng <- 'captTot3'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 10,
      units = 'in',
      res = 300
    )
    temp <- captureTotale3(
      capt.pecheur = ech[, c(
        'anneeGestion',
        'nomSite',
        'nbSebastes',
        'nbMorue',
        'nbOgac',
        'nbTurbot'
      )],
      nbPecheursMoy.strate = apply(nbPecheursTot.site, 2, sum, na.rm = TRUE),
      langue = i.langue
    )
    dev.off()
  }
  captTot <- temp
  write.csv2(temp, file = file.path(output_dir, 'csv', paste0(nomPng, '.csv')))
  ##

  ##
  ## nbTot et proportion des espèces capturées, 1x2
  ##
  nomPng <- 'propEspCapt'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 4.5,
      width = 10,
      units = 'in',
      res = 300
    )
    par(mfrow = c(1, 2))
    temp <- captureTot(capt = captTot.site.sfs, langue = i.langue)
    temp <- propEspeceCapt(
      capt = captTot.site.sfs,
      titre = 'B',
      langue = i.langue
    )
    dev.off()
  }
  write.csv2(temp, file = file.path(output_dir, 'csv', paste0(nomPng, '.csv')))
  ##
}
