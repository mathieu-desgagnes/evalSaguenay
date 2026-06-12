#' Produit le graphique des proportion d'especes dans les captures rapportées
#'
#' @param capt data.frame() contenant la captures totale estimée pour chaque espèce, par strate année/site/sfs
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return list() de 2 data.frame() des données des deux graphiques
#'
graph_capture_prop_esp2 <- function(
  capt,
  titre = 'A',
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'Nombre'
      legende1 = 'Espèce'
      espSeb = 'Sébastes'
      espMor = 'Morue'
      espOgac = 'Ogac'
      espGad = 'Gadide'
      espTur = 'Turbot'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'Number'
      legende1 = 'Species'
      espSeb = 'Sebastes'
      espMor = 'Cod'
      espOgac = 'Ogac'
      espGad = 'Gadide'
      espTur = 'Turbot'
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'Nombre/Number'
      legende1 = 'Espèce/Species'
      espSeb = 'Sebastes'
      espMor = 'Morue/Cod'
      espOgac = 'Ogac'
      espGad = 'Gadide'
      espTur = 'Turbot'
    }
  )
  op <- par(mar = c(4, 4, 1, 1) + 0.1)
  temp <- apply(capt, c(1, 3), sum, na.rm = TRUE)[, c(
    'sebastes',
    'morue',
    'ogac',
    'gadide',
    'turbot'
  )]
  ## ajuster 'gadide'
  temp[as.character(2001:2023), 'gadide'] <- 0
  temp[as.character(1996:2000), c('morue', 'ogac')] <- 0
  x <- barplot(
    t(temp / apply(temp, 1, sum)),
    col = c(2, 3, 4, 5, 6),
    legend = TRUE,
    xlab = xlab1,
    ylab = 'Proportion',
    args.legend = list(
      x = 'bottomleft',
      inset = 0.03,
      bg = "white",
      title = legende1,
      legend = rev(c(
        espSeb,
        espMor,
        espOgac,
        paste0(espMor, '+', espOgac),
        espTur
      )),
      fill = rev(c(2, 3, 4, 5, 6))
    )
  )
  mtext(side = 3, text = titre)
  ## text(x=mean(par('usr')[1:2]), y=diff(par('usr')[3:4])*0.9+par('usr')[3], labels=c('A','B','C','D')[j.text], cex=1.5); j.text=j.text+1
  ##
  return(temp)
}
