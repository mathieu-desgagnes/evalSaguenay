#' Traite les données d'échantillonneurs pour obtenir une nue standardisée
#'
#' @param ech données d'échantillonneurs
#'
#' @importFrom MASS glm.nb
#'
#' @returns
#' @export
#'
#' @examples ## à venir
std_nue <- function(ech, jb, output_dir) {
  ##ajustement échantillonneurs
  ajustement <- std_nue_ech(ech = ech, uniteEffort = 'ue')
  # ajustement <- std_nue_ech(ech = subset(ech,nbLignes<=2), uniteEffort = 'nbHeures')
  ##ajustement jb
  jb$ue <- jb$nbLignes *
    jb$nbHamecons *
    (jb$nbHeureImmersion + jb$nbMinuteImmersion / 60) #unite d'effort
  jb$immersion <- jb$nbHeureImmersion + jb$nbMinuteImmersion / 60 #temps d'immersion
  # ajustement.jb <- std_nue_jb(
  #   donnees = jb,
  #   uniteEffort = 'ue'
  # )
  ajustement.jb <- std_nue_jb(
    donnees = jb,
    uniteEffort = 'immersion'
  )
  # save(ajustement.jb, file = file.path(output_dir, 'csv', 'nueStdJB.RData'))

  ##
  ##
  annees <- 1996:max(ech$anneeGestion, na.rm = TRUE)
  nomPng <- 'stdNue'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 10,
      units = 'in',
      res = 300
    )
    temp <- graph_std_nue_ech(
      fit = ajustement$fit,
      dat = ajustement$dat,
      annees = annees,
      nueBrutes = FALSE,
      langue = i.langue
    )
    dev.off()
  }
  nueStd <- temp
  # save(nueStd, file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))
  # for (i.esp in seq_along(temp)) {
  #   write.csv2(
  #     temp[[i.esp]],
  #     file = file.path(
  #       output_dir,
  #       'csv',
  #       paste0(nomPng, '_', names(temp)[i.esp], '.csv')
  #     )
  #   )
  # }
  # load(file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))

  ##
  ## à faire: ajouter 'nombre heures pêchées et nombre de poissons capturés"
  ##

  ##journaux de bord
  ## à faire: corriger "différents" noms d'un même pêcheur
  ##
  if (FALSE) {
    # table(jb$anneeGestion, jb$nomSite, useNA = 'ifany')
    # table(jb$echantillonneur, jb$nomSite, useNA = 'ifany')
    table(jb$echantillonneur, jb$anneeGestion, useNA = 'ifany')
    # table(jb$nomSite)
  }
  ##
  ## proportion des espèces capturées
  ##
  nomPng <- 'propCaptJB'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 4.5,
      width = 10,
      units = 'in',
      res = 300
    )
    par(mfrow = c(1, 2))
    temp <- graph_capt_par_activite_jb(donnees = jb, langue = i.langue)
    temp <- graph_prop_par_activite_jb(
      donnees = jb,
      titre = 'B',
      langue = i.langue
    )
    dev.off()
  }
  ##
  ##
  nomPng <- 'stdNueJB'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 10,
      units = 'in',
      res = 300
    )
    temp <- graph_std_nue_jb(
      fit = ajustement.jb$fit,
      dat = ajustement.jb$dat,
      anneesFit = 2015:2026,
      limitesX = c(1996, 2026),
      nueBrutes = FALSE,
      langue = i.langue
    )
    dev.off()
  }
  nueStd.jb <- temp
  # save(nueStd.jb, file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))
  # for (i.esp in seq_along(temp)) {
  #   write.csv2(
  #     temp[[i.esp]],
  #     file = file.path(
  #       output_dir,
  #       'csv',
  #       paste0(nomPng, '_', names(temp)[i.esp], '.csv')
  #     )
  #   )
  # }
  # load(
  #   file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')),
  #   verbose = 1
  # )

  ##
  ## standardisation des ech et des jb sur même graphique
  nomPng <- 'stdNue2sources'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
      height = 8,
      width = 10,
      units = 'in',
      res = 300
    )
    par(mfrow = c(2, 2))
    ##
    j.text <- 1
    for (i.esp in c('sebastes', 'morue', 'ogac', 'turbot')) {
      graph_std_nue_2sources(
        nue.ech = nueStd[[i.esp]],
        nue.jb = nueStd.jb[[i.esp]],
        limitesX = 1995:2026,
        main.txt = '',
        abcd = c('A', 'B', 'C', 'D')[j.text],
        legende = c(TRUE, rep(FALSE, 3))[j.text],
        langue = i.langue
      )
      j.text <- j.text + 1
    }
    dev.off()
  }
}
