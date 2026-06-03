#' Calcule et produit la fréquentation annuelle total de l'activité de pêche réacréative hivernale sur le Saguenay.
#'
#'
#' @param nbPecheurs data.frame() contenant le nombre moyen de pêcheurs par site, année et moment de la semaine
#' @param nbJours data.frame() détaillant le nombre de jours de pêche selon l'année et le moment de la semaine
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return data.frame() du nombre total de pêcheurs-jour par année, et pour le secteur de La Baie et le PMSSL
#'
graph_effort_peche <- function(
  nbPecheurs,
  nbJours,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'Nombre de pêcheurs-jours (milliers)'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'Number of fishermen - days (thousands)'
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'Pêcheurs-jour / Fishermen-day'
    }
  )
  op <- par(mar = c(4, 4, 1, 1) + 0.1)
  annees <- min(as.numeric(dimnames(nbJours)[[1]])):max(
    (as.numeric(dimnames(nbJours)[[1]]))
  )
  val <- array(
    dim = c(3, length(annees)),
    dimnames = list(site = c('Tout', 'LaBaie', 'PMSSL'), annee = annees)
  )
  for (i.an in seq_along(annees)) {
    if (
      annees[i.an] %in%
        dimnames(nbJours)[[1]] &&
        sum(nbJours[as.character(annees[i.an]), ]) != 0
    ) {
      ## Tout
      val['Tout', as.character(annees[i.an])] <- sum(
        nbPecheurs[, as.character(annees[i.an]), 'semaine', 'moyenne'] *
          nbJours[as.character(annees[i.an]), 'semaine'],
        na.rm = TRUE
      ) +
        sum(
          nbPecheurs[, as.character(annees[i.an]), 'fds', 'moyenne'] *
            nbJours[as.character(annees[i.an]), 'fds'],
          na.rm = TRUE
        )
      ## LaBaie
      val['LaBaie', as.character(annees[i.an])] <- sum(
        nbPecheurs[
          c('AnseBenjamin', 'GrandeBaie', 'LesBattures'),
          as.character(annees[i.an]),
          'semaine',
          'moyenne'
        ] *
          nbJours[as.character(annees[i.an]), 'semaine'],
        na.rm = TRUE
      ) +
        sum(
          nbPecheurs[
            c('AnseBenjamin', 'GrandeBaie', 'LesBattures'),
            as.character(annees[i.an]),
            'fds',
            'moyenne'
          ] *
            nbJours[as.character(annees[i.an]), 'fds'],
          na.rm = TRUE
        )
      ## PMSSL
      val['PMSSL', as.character(annees[i.an])] <- sum(
        nbPecheurs[
          c('AnseStJean', 'RiviereEternite', 'SteRose', 'StFelix'),
          as.character(annees[i.an]),
          'semaine',
          'moyenne'
        ] *
          nbJours[as.character(annees[i.an]), 'semaine'],
        na.rm = TRUE
      ) +
        sum(
          nbPecheurs[
            c('AnseStJean', 'RiviereEternite', 'SteRose', 'StFelix'),
            as.character(annees[i.an]),
            'fds',
            'moyenne'
          ] *
            nbJours[as.character(annees[i.an]), 'fds'],
          na.rm = TRUE
        )
    }
  }
  ##
  plot(
    annees,
    val['Tout', ] / 1000,
    type = 'o',
    ylim = c(0, max(val['Tout', ] / 1000, na.rm = TRUE) * 1.05),
    xlab = xlab1,
    ylab = ylab1,
    pch = 16,
    yaxs = 'i'
  )
  lines(annees, val['LaBaie', ] / 1000, type = 'o', pch = 15, col = 4)
  lines(annees, val['PMSSL', ] / 1000, type = 'o', pch = 17, col = 3)
  legend(
    'topright',
    legend = c('Saguenay', 'La Baie', 'PMSSL'),
    col = c(1, 4, 3),
    pch = c(16, 15, 17),
    lty = 1
  )
  lines(
    2023 + c(-14, 0),
    rep(mean(tail(val['Tout', ], 15) / 1000, na.rm = TRUE), 2),
    lwd = 2
  )
  lines(
    2023 + c(-14, 0),
    rep(mean(tail(val['LaBaie', ], 15) / 1000, na.rm = TRUE), 2),
    col = 4,
    lwd = 2
  )
  lines(
    2023 + c(-14, 0),
    rep(mean(tail(val['PMSSL', ], 15) / 1000, na.rm = TRUE), 2),
    col = 3,
    lwd = 2
  )
  return(as.data.frame(t(val)))
}
