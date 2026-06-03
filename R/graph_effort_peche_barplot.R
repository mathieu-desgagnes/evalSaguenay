#' Calcule la fréquentation annuelle total
#'
#'
#' @param nbPecheursTot data.frame() contenant le nombre annuel total de pêcheurs-jour pour tour le saguenay et
#' par secteur (La Baie et PMSSL)
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return data.frame() du nombre total de pêcheurs-jour par année, et pour le secteur de La Baie et le PMSSL
#'
graph_effort_peche_barplot <- function(
  nbPecheursTot,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'Nombre de pêcheurs-jours (milliers)'
      ylab2 <- "Proportion de l'effort"
      legend1 <- c('PMSSL', 'La Baie', 'Autre')
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'Number of fishermen - days (thousands)'
      ylab2 <- "Proportion of effort"
      legend1 <- c('PMSSL', 'La Baie', 'Other')
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'Pêcheurs-jour / Fishermen-day'
      ylab2 <- "Proportion"
      legend1 <- c('PMSSL', 'La Baie', 'Autre/Other')
    }
  )
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 1, 1) + 0.1)
  nbPec <- nbPecheursTot
  nbPec$Tout <- round(nbPec$Tout - (nbPec$LaBaie + nbPec$PMSSL), 2)
  x <- barplot(
    t(nbPec) / 1000,
    col = 1 + 1:ncol(nbPec),
    ## legend=TRUE, args.legend=list(x='bottomright', inset=0.03, legend=legend1),
    legend = FALSE,
    xlab = xlab1,
    ylab = ylab1
  )
  ## text(x=mean(par('usr')[1:2]), y=diff(par('usr')[3:4])*0.9+par('usr')[3], labels='A', cex=1.5)
  mtext(side = 3, text = 'A')
  barplot(
    t(nbPec / apply(nbPec, 1, sum)),
    col = 1 + 1:ncol(nbPec),
    legend = TRUE,
    args.legend = list(
      x = 'bottomright',
      inset = 0.03,
      bg = "white",
      legend = legend1
    ),
    ## legend=FALSE, args.legend=list(x='bottomright',inset=0.03,bg="white",legend=legend1),
    xlab = xlab1,
    ylab = ylab2
  )
  ## text(x=mean(par('usr')[1:2]), y=diff(par('usr')[3:4])*0.9+par('usr')[3], labels='B', cex=1.5)
  mtext(side = 3, text = 'B')
}
