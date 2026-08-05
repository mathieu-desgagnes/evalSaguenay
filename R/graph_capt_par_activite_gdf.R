#' Produit le graphique des proportion d'especes dans les captures rapportées par l'application "Glaces du Fjord"
#'
#' @param donnees data.frame() contenant les données de captures de l'application "Glaces du Fjord"
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return
#'
graph_capt_par_activite_gdf <- function(
  donnees,
  titre = 'A',
  max.y = NULL,
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
  op <- par(mar = c(4, 4, 1, 1) + 0.1)
  donnees$quantite_par_pecheur <- donnees$quantite / donnees$nombre_pecheurs
  donnees$quantite_par_ligne <- donnees$quantite / donnees$nombre_lignes_actives
  donnees2 <- donnees
  donnees2[
    !donnees2$espece %in% c('Morue franche', 'Ogac', 'Turbot', 'Sébaste'),
    'espece'
  ] <- 'Autre'
  temp1 <- aggregate(
    donnees2$quantite_par_pecheur,
    donnees2[, c('annee', 'espece')],
    FUN = sum
  )
  temp1 <- merge(
    expand.grid(
      annee = 2015:max(donnees$annee),
      espece = unique(donnees2$espece)
    ),
    temp1,
    by = c('annee', 'espece'),
    all.x = TRUE
  )
  temp1$x[is.na(temp1$x)] <- 0
  # temp2 <- aggregate(
  #   donnees2$quantite_par_ligne,
  #   donnees2[, c('identifiant_activite', 'annee', 'espece')],
  #   FUN = sum
  # )
  ##regrouper par activité de pêche
  temp2 <- aggregate(
    temp1$x,
    temp1[c('annee')],
    FUN = sum
  )
  test <- unique(donnees2[, c('identifiant_activite', 'annee')])
  temp3 <- aggregate(
    test$identifiant_activite,
    test[c('annee')],
    FUN = length
  )
  temp1$x_par_activite <- temp1$x / temp3$x
  temp1$prop <- temp1$x / temp2$x

  # Calculer les totaux par année et espèce
  totaux <- aggregate(x_par_activite ~ annee + espece, data = temp1, sum)
  # Ordre des espèces pour la légende
  especes <- unique(temp1$espece)
  # Couleurs pour les espèces
  couleurs <- c(
    "Sébaste" = 2,
    "Autre" = 8,
    "Morue franche" = 3,
    "Ogac" = 4,
    "Turbot" = 6
  )
  # Convertir en matrice pour le graphique
  matrice <- matrix(
    totaux$x,
    nrow = length(unique(temp1$annee)),
    ncol = length(unique(temp1$espece))
  )
  colnames(matrice) <- unique(temp1$espece)
  rownames(matrice) <- unique(temp1$annee)
  matrice <- t(matrice)[
    c('Sébaste', 'Morue franche', 'Ogac', 'Turbot', 'Autre'),
  ]

  if (is.null(max.y)) {
    max.y <- max(apply(matrice, 2, sum))
  }
  x <- barplot(
    matrice,
    col = couleurs[rownames(matrice)],
    legend.text = TRUE,
    xlab = xlab1,
    ylab = ylab1,
    ylim = c(0, max.y),
    args.legend = list(
      x = 'bottomright',
      inset = 0.03,
      bg = "white",
      title = legende1,
      legend = rev(c(espSeb, espMor, espOgac, espTur, espAutre)),
      fill = rev(couleurs[rownames(matrice)])
    )
  )
  ## axis(3, at=x, labels=nb.an$x, cex.axis=0.8, tick=FALSE, line=-1, las=1)
  ## j.text <- 1; text(x=mean(par('usr')[1:2]), y=diff(par('usr')[3:4])*0.9+par('usr')[3], labels=c('A','B','C','D')[j.text], cex=1.5); j.text=j.text+1
  mtext(side = 3, text = titre)
  ##
  return(NA)
}
