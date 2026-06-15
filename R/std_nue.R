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
  ##échantillonneurs
  ajustement <- std_nue_ech(ech = ech, uniteEffort = 'ue')
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
  save(nueStd, file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))
  for (i.esp in seq_along(temp)) {
    write.csv2(
      temp[[i.esp]],
      file = file.path(
        output_dir,
        'csv',
        paste0(nomPng, '_', names(temp)[i.esp], '.csv')
      )
    )
  }
  load(file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))

  ##
  ## à faire: ajouter 'nombre heures pêchées et nombre de poissons capturés"
  ##

  ##journaux de bord
  ## à faire: corriger "différents" noms d'un même pêcheur
  ##
  if (FALSE) {
    table(jb$anneeGestion, jb$nomSite, useNA = 'ifany')
    table(jb$echantillonneur, jb$nomSite, useNA = 'ifany')
    table(jb$echantillonneur, jb$anneeGestion, useNA = 'ifany')
    table(jb$nomSite)
  }
  ##
  ## proportion des espèces capturées
  ##
  nomPng <- 'propCaptJB'
  for (i.langue in c('fr', 'en', 'bil')) {
    png(
      file = file.path(dir.output, i.langue, paste0(nomPng, '.png')),
      height = 4.5,
      width = 10,
      units = 'in',
      res = 300
    )
    par(mfrow = c(1, 2))
    temp <- captParActJB(donnees = jb, langue = i.langue)
    temp <- propCaptJB(donnees = jb, titre = 'B', langue = i.langue)
    dev.off()
  }
}
