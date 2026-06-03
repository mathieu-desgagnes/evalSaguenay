#' Calcule la fréquentation annuelle des différents sites de pêche, selon les vistes des échantillonneurs.
#'
#' Les sites sont c("AnseBenjamin", "AnseStJean", "GrandeBaie","LesBattures", "RiviereEternite", "SteRose",
#'                  "StFelix", "StFulgence")
#'
#' @param ech data.frame() contenant les données récoltées par les échantillonneurs
#' @param visites data.frame() contenant les visites réalisées par les échantillonneurs
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return un array() de 4 dimension (site, année, sfs et statistique) fournissant les statistiques de moyenne, de sd et de n.
#'
graph_effort_peche_par_site <- function(
  ech,
  visites,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'Nombre de pêcheurs'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'Number of fishermen'
    },
    'bil' = {
      xlab1 <- 'Année / Year'
      ylab1 <- 'Nombre de pêcheurs / Number of fishermen'
    }
  )
  op <- par(mfrow = c(4, 2), mar = c(2, 3, 1.5, 2))
  annees <- min(visites$anneeGestion):max(visites$anneeGestion)
  ordreSite <- by(
    visites$nbPecheursFond,
    visites['nomSite'],
    FUN = max,
    na.rm = TRUE
  )
  site <- names(ordreSite)[order(ordreSite, decreasing = TRUE)]
  siteNom <- c(
    "Anse-à-Benjamin",
    "Anse-Saint-Jean",
    "Grande Baie",
    "Les Battures",
    "Rivière Éternité",
    "Sainte-Rose-du-Nord",
    "Saint-Félix-d'Otis",
    "Saint-Fulgence"
  )[match(
    c(
      "AnseBenjamin",
      "AnseStJean",
      "GrandeBaie",
      "LesBattures",
      "RiviereEternite",
      "SteRose",
      "StFelix",
      "StFulgence"
    ),
    names(ordreSite)
  )]
  siteNom <- siteNom[order(ordreSite, decreasing = TRUE)]
  val <- array(
    dim = c(length(siteNom), length(annees), 2, 3),
    dimnames = list(
      site = site,
      annee = annees,
      sfs = c('semaine', 'fds'),
      mesure = c('moyenne', 'sd', 'n')
    )
  )
  for (i.site in seq_along(site)) {
    sem.init <- na.omit(visites[
      which(visites$nomSite == site[i.site] & visites$sfs == 0),
      c('anneeGestion', 'nbPecheursFond')
    ])
    sem <- as.data.frame(cbind(
      moyenne = by(
        sem.init$nbPecheursFond,
        sem.init['anneeGestion'],
        FUN = mean,
        na.rm = TRUE
      ),
      sd = by(
        sem.init$nbPecheursFond,
        sem.init['anneeGestion'],
        FUN = sd,
        na.rm = TRUE
      ),
      n = by(sem.init$nbPecheursFond, sem.init['anneeGestion'], FUN = length)
    ))
    sem$annee <- as.numeric(dimnames(sem)[[1]])
    val[i.site, as.character(sem$annee), 'semaine', 'moyenne'] <- sem[,
      'moyenne'
    ]
    val[i.site, as.character(sem$annee), 'semaine', 'sd'] <- sem[, 'sd']
    val[i.site, as.character(sem$annee), 'semaine', 'n'] <- sem[, 'n']
    fsem.init <- na.omit(visites[
      which(visites$nomSite == site[i.site] & visites$sfs == 1),
      c('anneeGestion', 'nbPecheursFond')
    ])
    fsem <- as.data.frame(cbind(
      moyenne = by(
        fsem.init$nbPecheursFond,
        fsem.init['anneeGestion'],
        FUN = mean,
        na.rm = TRUE
      ),
      sd = by(
        fsem.init$nbPecheursFond,
        fsem.init['anneeGestion'],
        FUN = sd,
        na.rm = TRUE
      ),
      n = by(fsem.init$nbPecheursFond, fsem.init['anneeGestion'], FUN = length)
    ))
    fsem$annee <- as.numeric(dimnames(fsem)[[1]])
    val[i.site, as.character(fsem$annee), 'fds', 'moyenne'] <- fsem[, 'moyenne']
    val[i.site, as.character(fsem$annee), 'fds', 'sd'] <- fsem[, 'sd']
    val[i.site, as.character(fsem$annee), 'fds', 'n'] <- fsem[, 'n']
    plot(
      fsem$annee,
      fsem$moyenne,
      type = "n",
      col = 'black',
      xlim = c(annees[1], tail(annees, 1)),
      ylim = c(
        0,
        max(
          c(sem.init$nbPecheursFond, fsem.init$nbPecheursFond),
          na.rm = TRUE
        ) *
          1.05
      ),
      yaxs = "i",
      xlab = xlab1,
      ylab = ylab1,
      main = siteNom[i.site]
    )
    abline(h = seq(0, 2000, by = 100), col = 'grey80')
    points(
      sem.init$anneeGestion - 0.05,
      sem.init$nbPecheursFond,
      pch = 16,
      cex = 0.5,
      col = 'green'
    )
    points(
      fsem.init$annee + 0.05,
      fsem.init$nbPecheursFond,
      pch = 16,
      cex = 0.5
    )
    lines(
      moyenne ~ annee,
      data = merge(
        as.data.frame(list(annee = min(annees):max(annees))),
        fsem,
        all = TRUE
      )
    )
    lines(
      moyenne ~ annee,
      data = merge(
        as.data.frame(list(annee = min(annees):max(annees))),
        sem,
        all = TRUE
      ),
      col = 'green'
    )
  }
  return(val)
}
