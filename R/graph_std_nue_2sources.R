#' Gaphique de la standardisation des NUE obtenues de deux sources, soit les journaux de bord et le sondage par les échantillonneurs pour une espèce donnée
#'
#' Les strates sont définies spécifiquement selon la source.
#'
#' @param nue.ech les valeurs annuelles de la moyenne standardisée, et l'intervale de confiance inférieur et supérieur pour les données de sondage par les
#' échantillonneurs
#' @param nue.jb les valeurs annuelles de la moyenne standardisée, et l'intervale de confiance inférieur et supérieur pour les données de journaux de bord
#' @param anneesFit vector() des années à utiliser
#' @param limitesX valeurs minimal et maximal des années présentées dans le graphique
#' @param main.txt le texte à utiliser en titre de graphique
#' @param abcd texte (généralement la lettre pour référence dans la légende de la publication) à afficher dans le graphique, en haut au centre.
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return list() de 2 data.frame() des données des deux graphiques
#'
graph_std_nue_2sources <- function(
  nue.ech,
  nue.jb,
  anneesFit,
  limitesX = c(1996, 2023),
  main.txt = '',
  abcd = '',
  legende = FALSE,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'NUE (prises/hameçon-heure)'
      leg.volet <- 'volet'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'NUE (catch/hook-hour)'
      leg.volet <- 'component'
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'NUE'
      leg.volet <- 'volet/component'
    }
  )
  op <- par(mar = c(4, 4, 1, 3) + 0.1)
  ##
  ratio <- mean(nue.ech[
    'valeur',
    which(dimnames(nue.ech)[[2]] %in% dimnames(nue.jb)[[2]])
  ]) /
    mean(nue.jb['valeur', ])

  ##
  ## échantillonneurs
  ## ajouter un point vide pour 2021
  nue.ech <- cbind(
    nue.ech[, as.character(min(dimnames(nue.ech)[[2]]):2020)],
    rep(NA, 3),
    nue.ech[, as.character(2022:max(dimnames(nue.ech)[[2]]))]
  )
  dimnames(nue.ech)[[2]] <- min(
    as.numeric(dimnames(nue.ech)[[2]]),
    na.rm = TRUE
  ):max(
    as.numeric(dimnames(nue.ech)[[
      2
    ]]),
    na.rm = TRUE
  )
  plot(
    as.numeric(dimnames(nue.ech)[[2]]),
    nue.ech['valeur', dimnames(nue.ech)[[2]]],
    type = 'o',
    pch = 16,
    yaxs = 'i',
    xlim = range(limitesX),
    ylim = c(
      0,
      max(nue.ech['valeur', ], nue.jb['valeur', ] * ratio, na.rm = TRUE) * 1.05
    ),
    xlab = xlab1,
    ylab = ylab1,
    main = main.txt
  )
  ##
  ## journaux de bord
  lines(
    as.numeric(dimnames(nue.jb)[[2]]),
    nue.jb['valeur', ] * ratio,
    type = 'o',
    pch = 17,
    col = 2
  )
  axis(
    4,
    at = pretty(par('usr')[3:4] / ratio) * ratio,
    labels = pretty(par('usr')[3:4] / ratio),
    col = 2,
    col.axis = 2
  )
  text(
    x = mean(par('usr')[1:2]),
    y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
    labels = abcd,
    cex = 1.5
  )
  if (legende) {
    legend(
      'topright',
      inset = 0.03,
      legend = paste(leg.volet, c(1, 3)),
      title = 'Source',
      pch = c(16, 17),
      lty = 1,
      col = c(1, 2)
    )
  }
}
