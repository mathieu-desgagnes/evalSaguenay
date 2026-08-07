#' Produit le graphique des proportion d'especes dans les captures rapportées dans les journaux de bord
#'
#' @param donnees data.frame() contenant les données de captures des journaux de bord
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return
#'
graph_par_activite_jb <- function(
  donnees,
  proportion = TRUE,
  titre = 'A',
  legende = FALSE,
  max.y = NULL,
  autre_esp = TRUE,
  range.x = NULL,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'Nombre par activité'
      legende1 = 'Espèce'
      espSeb = 'Sébastes'
      espMor = 'Morue'
      espOgac = 'Ogac'
      espGad = 'Gadide'
      espTur = 'Turbot'
      espAutre = 'Autre'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'Number per activity'
      legende1 = 'Species'
      espSeb = 'Sebastes'
      espMor = 'Cod'
      espOgac = 'Ogac'
      espGad = 'Gadide'
      espTur = 'Turbot'
      espAutre = 'Other'
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'Nombre apr activity/Number per activity'
      legende1 = 'Espèce/Species'
      espSeb = 'Sebastes'
      espMor = 'Morue/Cod'
      espOgac = 'Ogac'
      espGad = 'Gadide'
      espTur = 'Turbot'
      espAutre = 'Autre/Other'
    }
  )
  # op <- par(mar = c(4, 4, 1, 1) + 0.1)
  ##
  donnees$nbToutAutre <- apply(
    donnees[, c('nbSaida', 'nbFletan', 'nbCrabe', 'nbLycode', 'nbRaie')],
    1,
    sum,
    na.rm = TRUE
  )
  if (autre_esp) {
    temp <- donnees[, c(
      'nbSebastes',
      'nbMorue',
      'nbOgac',
      'nbTurbot',
      'nbToutAutre'
    )]
    nom_legende <- rev(c(espSeb, espMor, espOgac, espTur, espAutre))
  } else {
    temp <- donnees[, c(
      'nbSebastes',
      'nbMorue',
      'nbOgac',
      'nbTurbot'
    )]
    nom_legende <- rev(c(espSeb, espMor, espOgac, espTur))
  }
  esp.an <- aggregate(temp, donnees['anneeGestion'], FUN = sum)
  dimnames(esp.an)[[1]] <- esp.an$anneeGestion
  ## esp.an <- merge(esp.an, list(anneeGestion=annees), all=TRUE)
  nb.an <- aggregate(temp[, 1], donnees['anneeGestion'], FUN = length)

  if (proportion) {
    val <- t(esp.an[, -1] / apply(esp.an[, -1], 1, sum))
    max.y <- 1
  } else {
    val <- t(esp.an[, -1] / nb.an$x)
    if (is.null(max.y)) {
      max.y <- max(apply(t(esp.an[, -1] / nb.an$x), 2, sum))
    }
  }

  if (is.null(range.x)) {
    range.x <- range(donnees$annee)
  }
  val2 <- array(
    0,
    dim = c(nrow(val), diff(range.x) + 1),
    dimnames = list(
      espece = unique(rownames(val)),
      annee = range.x[1]:range.x[2]
    )
  )
  val2[, na.omit(match(colnames(val), colnames(val2)))] <- val[, na.omit(match(
    colnames(val2),
    colnames(val)
  ))]

  if (legende) {
    x <- barplot(
      val2,
      col = c(2, 3, 4, 6, 8),
      xlab = xlab1,
      ylab = 'Proportion',
      ylim = c(0, max.y),
      legend.text = TRUE,
      args.legend = list(
        x = 'bottomright',
        inset = 0.03,
        bg = "white",
        title = legende1,
        legend = nom_legende,
        fill = rev(c(2, 3, 4, 6, 8)[1:length(nom_legende)])
      )
    )
  } else {
    x <- barplot(
      val2,
      col = c(2, 3, 4, 6, 8),
      xlab = xlab1,
      ylab = 'Proportion',
      ylim = c(0, max.y)
    )
  }
  ## axis(1, at=x)
  ## text(x=mean(par('usr')[1:2]), y=diff(par('usr')[3:4])*0.9+par('usr')[3], labels=c('A','B','C','D')[j.text], cex=1.5); j.text=j.text+1
  mtext(side = 3, text = titre)
  ##
  par('usr')[4]
}
