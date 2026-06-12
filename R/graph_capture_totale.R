#' Estime la capture anuelle totale pour chacune des espèces sébastes, morue, ogac et turbot et produit un graphique de ces captures.
#'
#' Pour chaque strate année/site/semaineFinSemaine, un nombre moyen de captures par pêcheur est calculé.
#' Ce nombre est multiplié par le nombre de moyen de pêcheurs pour la strate et par le nombre de jour calculé pour une saison complète de la strate.
#'
#' @param capt.pecheur un data.frame() de pêcheurs avec leur capture par espece
#' @param nbPecheurMoy.strate moyenne du nombre de pêcheurs estimé par strate
#' @param nbJours.strate nombre de jour que dure la saison, par strate
#' @param legende dans quel(s) graphique doit apparaitre la légende
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return data.frame() des captures totales par strate
#'
captureTotale <- function(
  capt.pecheur,
  nbPecheursMoy.strate,
  nbJour.strate,
  legende = c(FALSE, TRUE, FALSE, FALSE),
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'Nombre (milliers)'
      espSeb = 'Sébastes'
      espMor = 'Morue'
      espOgac = 'Ogac'
      espTur = 'Turbot'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'Number (thousands)'
      espSeb = 'Sebastes'
      espMor = 'Cod'
      espOgac = 'Ogac'
      espTur = 'Turbot'
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'Nombre (milliers) / Number (thousands)'
      espSeb = 'Sebastes'
      espMor = 'Morue/Cod'
      espOgac = 'Ogac'
      espTur = 'Turbot'
    }
  )
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 1, 1) + 0.1)
  ##
  annees <- as.numeric(dimnames(nbPecheursMoy.strate)$annee)
  sites <- dimnames(nbPecheursMoy.strate)$site
  especes <- c('sebastes', 'morue', 'ogac', 'turbot', 'gadide')
  sfs <- dimnames(nbPecheursMoy.strate)$sfs
  ##
  capt <- array(
    dim = c(length(annees), length(sites), length(especes), length(sfs)),
    dimnames = list(annee = annees, site = sites, espece = especes, sfs = sfs)
  )
  nbEch <- array(
    0,
    dim = c(length(annees), length(sites), length(especes), length(sfs)),
    dimnames = list(annee = annees, site = sites, espece = especes, sfs = sfs)
  )
  ## pueBrute <- array(dim=c(length(annees), length(sites), length(especes), length(sfs)),
  ##                   dimnames=list(annee=annees, site=sites, espece=especes, sfs=sfs))
  pueEch <- array(
    dim = c(length(annees), length(sites), length(especes), length(sfs)),
    dimnames = list(annee = annees, site = sites, espece = especes, sfs = sfs)
  )
  ##
  ## calcul du nombre total de chaque espèce pêchée par strate
  for (i.site in seq_along(sites)) {
    ceSite <- subset(ech, nomSite == sites[i.site])
    for (i.an in seq_along(annees)) {
      cetteAnnee <- subset(ceSite, anneeGestion == annees[i.an])
      if (nrow(cetteAnnee) > 0) {
        for (i.esp in seq_along(especes)) {
          for (i.sfs in seq_along(sfs)) {
            nomColEsp <- paste0(
              'nb',
              c('Sebastes', 'Morue', 'Ogac', 'Turbot', 'Gadide')[match(
                especes[i.esp],
                c('sebastes', 'morue', 'ogac', 'turbot', 'gadide')
              )]
            )
            capt[
              as.character(annees[i.an]),
              sites[i.site],
              especes[i.esp],
              sfs[i.sfs]
            ] <-
              sum(
                cetteAnnee[which(cetteAnnee$sfs == i.sfs - 1), nomColEsp],
                na.rm = TRUE
              )
            nbEch[
              as.character(annees[i.an]),
              sites[i.site],
              especes[i.esp],
              sfs[i.sfs]
            ] <-
              nrow(cetteAnnee[which(cetteAnnee$sfs == i.sfs - 1), ])
            ## pueBrute[as.character(annees[i.an]), sites[i.site], especes[i.esp], sfs[i.sfs]] <-
            ##     capt[as.character(annees[i.an]), sites[i.site], especes[i.esp], sfs[i.sfs]] /
            ##     heures[as.character(annees[i.an]), sites[i.site], especes[i.esp], sfs[i.sfs]]
            pueEch[
              as.character(annees[i.an]),
              sites[i.site],
              especes[i.esp],
              sfs[i.sfs]
            ] <-
              capt[
                as.character(annees[i.an]),
                sites[i.site],
                especes[i.esp],
                sfs[i.sfs]
              ] /
              nbEch[
                as.character(annees[i.an]),
                sites[i.site],
                especes[i.esp],
                sfs[i.sfs]
              ] *
              nbPecheursMoy.strate[
                sites[i.site],
                as.character(annees[i.an]),
                sfs[i.sfs]
              ] *
              nbJour.strate[as.character(annees[i.an]), sfs[i.sfs]]
          }
        }
      }
    }
  }
  ##
  ## production des graphiques
  ## sébastes
  val <- apply(pueEch[,, 'sebastes', ], c(1, 2), sum, na.rm = TRUE) / 1000
  x <- barplot(
    t(val),
    col = 2:(ncol(val) + 1),
    legend = legende[1], #xlab=xlab1,
    ylab = ylab1,
    main = espSeb, #paste(espSeb, ', total:',round(sum(val))),
    args.legend = list(
      x = 'bottomleft',
      inset = 0.03,
      bg = "white",
      title = 'Site'
    )
  )
  j.text <- 1
  text(
    x = mean(par('usr')[1:2]),
    y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
    labels = c('A', 'B', 'C', 'D')[j.text],
    cex = 1.5
  )
  j.text = j.text + 1
  print(paste(
    espSeb,
    ', total 1996 à 2020:',
    round(sum(val[as.character(1996:2020), ])) * 1000
  ))
  ## morue
  val <- apply(pueEch[,, 'morue', ], c(1, 2), sum, na.rm = TRUE) / 1000
  x <- barplot(
    t(val),
    col = 2:(ncol(val) + 1),
    legend = legende[2], #xlab=xlab1, ylab=ylab1,
    main = espMor, #paste(espMor, ', total:', round(sum(val))),
    args.legend = list(
      x = 'topright',
      inset = 0.03,
      bg = "white",
      title = 'Site'
    )
  )
  text(
    x = mean(par('usr')[1:2]),
    y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
    labels = c('A', 'B', 'C', 'D')[j.text],
    cex = 1.5
  )
  j.text = j.text + 1
  ## ogac
  val <- apply(pueEch[,, 'ogac', ], c(1, 2), sum, na.rm = TRUE) / 1000
  x <- barplot(
    t(val),
    col = 2:(ncol(val) + 1),
    legend = legende[3],
    xlab = xlab1,
    ylab = ylab1,
    main = espOgac, #paste(espOgac,', total:',round(sum(val))),
    args.legend = list(
      x = 'topright',
      inset = 0.03,
      bg = "white",
      title = 'Site'
    )
  )
  text(
    x = mean(par('usr')[1:2]),
    y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
    labels = c('A', 'B', 'C', 'D')[j.text],
    cex = 1.5
  )
  j.text = j.text + 1
  ## turbot
  val <- apply(pueEch[,, 'turbot', ], c(1, 2), sum, na.rm = TRUE) / 1000
  x <- barplot(
    t(val),
    col = 2:(ncol(val) + 1),
    legend = legende[4],
    xlab = xlab1, #ylab=ylab1,
    main = espTur, #paste(espTur,', total:',round(sum(val))),
    args.legend = list(
      x = 'topright',
      inset = 0.03,
      bg = "white",
      title = 'Site'
    )
  )
  text(
    x = mean(par('usr')[1:2]),
    y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
    labels = c('A', 'B', 'C', 'D')[j.text],
    cex = 1.5
  )
  j.text = j.text + 1
  ##
  return(pueEch)
}
