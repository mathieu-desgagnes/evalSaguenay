#' Calcule la plage de dates où des échantillons sont récoltés pour chaque année et produit le graphique correspondant
#'
#' @param ech data.frame() contenant les données récoltées par les échantillonneurs
#' @param visites data.frame() contenant les données récoltées par les échantillonneurs, une ligne par visite
#' @param ouvertureOfficielle dataframe() contenant les années, mois et jours d'ouverture et
#'  de fermeture de la pêche blanche pour chaque année de gestion
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return
#'
graph_dates_ech <- function(
  ech,
  visites,
  ouvertureOfficielle,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Jours depuis 1 janvier'
      ylab1 <- 'Année'
      legend.semaine <- 'Semaine'
      legend.fds <- 'Fin de semaine'
    },
    'en' = {
      xlab1 <- 'Days since January 1st'
      ylab1 <- 'Year'
      legend.semaine <- 'Week days'
      legend.fds <- 'Weekend days'
    },
    'bil' = {
      xlab1 <- 'Depuis 1er janvier / Since January 1st'
      ylab1 <- 'Année / Year'
      legend.semaine <- 'Semaine / Week day'
      legend.fds <- 'Fin de semaine / Weekend'
    }
  )
  op <- par(mfrow = c(1, 1), mar = c(4, 4, 1, 1) + 0.1)
  annees <- sort(unique(visites$anneeGestion))
  ## main_date_echAnnee <- "Dates d'échantillonage et ouverture de la pêche"
  ## ylab_nPecheurEch_SiteAn <- "Nombre de pêcheurs"
  plot(
    0,
    0,
    type = 'n',
    ylim = c(annees[1], tail(annees, 1)),
    xlim = c(
      -31,
      max(yday(ouvertureOfficielle$fermeture) * 1.05, na.rm = TRUE)
    ),
    xlab = xlab1,
    ylab = ylab1
  )
  for (i.vis in 1:length(visites$date)) {
    if (year(visites[i.vis, 'date']) %in% annees) {
      points(
        yday(visites[i.vis, 'date']),
        visites[i.vis, 'anneeGestion'],
        pch = 3,
        col = c(rep('green', 5), rep('black', 2))[wday(visites[i.vis, 'date'])]
      )
    }
  }
  for (i.an in 1:nrow(ouvertureOfficielle)) {
    if (ouvertureOfficielle$anneeGestion[i.an] %in% annees) {
      lines(
        c(
          yday(ouvertureOfficielle[i.an, 'ouverture']) +
            365 *
              (year(ouvertureOfficielle[i.an, 'ouverture']) -
                ouvertureOfficielle[i.an, 'anneeGestion']),
          yday(ouvertureOfficielle[i.an, 'fermeture'])
        ),
        rep(ouvertureOfficielle[i.an, 'anneeGestion'], 2)
      )
      points(
        c(
          yday(ouvertureOfficielle[i.an, 'ouverture']) +
            365 *
              (year(ouvertureOfficielle[i.an, 'ouverture']) -
                ouvertureOfficielle[i.an, 'anneeGestion']),
          yday(ouvertureOfficielle[i.an, 'fermeture'])
        ),
        rep(ouvertureOfficielle[i.an, 'anneeGestion'], 2)
      )
    }
  }
  abline(v = 0)
  legend(
    'topright',
    inset = 0.03,
    c(legend.semaine, legend.fds),
    pch = 3,
    col = c('green', 'black')
  )
}
