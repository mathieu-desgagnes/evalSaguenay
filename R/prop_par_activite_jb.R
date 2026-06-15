#' Produit le graphique des proportion d'especes dans les captures rapportées dans les journaux de bord
#'
#' @param donnees data.frame() contenant les données de captures des journaux de bord
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return
#'
prop_par_activite_jb <- function(
  donnees,
  titre = 'A',
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
    }
  )
  op <- par(mar = c(4, 4, 1, 1) + 0.1)
  temp <- donnees[, c('nbSebastes', 'nbMorue', 'nbOgac', 'nbTurbot')]
  esp.an <- aggregate(temp, donnees['anneeGestion'], FUN = sum)
  dimnames(esp.an)[[1]] <- esp.an$anneeGestion
  ## esp.an <- merge(esp.an, list(anneeGestion=annees), all=TRUE)
  nb.an <- aggregate(temp[, 1], donnees['anneeGestion'], FUN = length)
  x <- barplot(
    t(esp.an[, -1] / apply(esp.an[, -1], 1, sum)),
    col = c(2, 3, 4, 6),
    xlab = xlab1,
    ylab = 'Proportion'
  )
  ## axis(1, at=x)
  ## text(x=mean(par('usr')[1:2]), y=diff(par('usr')[3:4])*0.9+par('usr')[3], labels=c('A','B','C','D')[j.text], cex=1.5); j.text=j.text+1
  mtext(side = 3, text = titre)
  ##
  return(merge(esp.an, nb.an))
}
