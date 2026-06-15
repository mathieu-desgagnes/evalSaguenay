#' Standardisation des NUE récoltées par les échantillonneurs, pour l'ensemble du Saguenay
#'
#' Les strates sont définies selon l'année, le site et le jour de capture, soit semaine ou fin de semaine.
#' Les années standardisées varie selon l'espècee.
#' Les données de gadidés n'ont pas été séparés entre 'morue' et 'ogac' avant 2001.
#' Pour l'ensemble des espèces, il n'y a aucune données utilisable avant 1996 et en 2021.
#' Cette version du code ne produit que les valeurs pour l'ensemble du Saguenay, laissant de côté
#' les valeurs pour le secteur La Baie et PMSSL, tel que fait dans le passé.
#'
#' @param ech l'ensemble des données d'échantillonneurs, nécessaitant l'années de gestion (anneeGestion),
#' le site (nomSite), le jour de semaine sous forme semaine/fin de semaine (sfs),
#' les captures de chaque espèce et l'unité d'effort
#' @param uniteEffort le nom de la colonne contenant l'unité d'effort à utiliser,
#' celle-ci ne doit pas avoir déjà été pré-transformée (log en particulier)
#'
#' @importFrom MASS glm.nb
#'
#' @return list() de 2 data.frame() des données des deux graphiques
#'
std_nue_ech <- function(ech, uniteEffort) {
  op <- par(mfrow = c(1, 1), mar = c(4, 4, 1, 1) + 0.1)
  annees <- 1996:max(ech$anneeGestion, na.rm = TRUE)
  annee.espece <- list(
    'sebastes' = annees[-which(annees == 2021)],
    'morue' = c(2001:2020, 2022:max(annees)),
    'ogac' = c(2001:2020, 2022:max(annees)),
    'turbot' = annees[-which(annees == 2021)]
  )
  especes <- names(annee.espece)
  ## secteurs <- c('saguenay','laBaie','pmssl')
  ## retirer les unités d'effort non finies
  dat <- ech[, c(
    'nbSebastes',
    'nbMorue',
    'nbOgac',
    'nbTurbot',
    'anneeGestion',
    'nomSite',
    'sfs'
  )]
  dat$uniteEffort <- log(ech[, uniteEffort])
  temp <- nrow(dat)
  dat <- dat[-which(!is.finite(dat$uniteEffort)), ]
  if (nrow(dat) < temp) {
    print(paste(
      temp - nrow(dat),
      'enregistrements sur',
      temp,
      'ont été retiré pour effort invalide'
    ))
  }
  dat[which(dat$nbMorue == 0.5), 'nbMorue'] <- 1 #données de décompte
  dat$anneeGestion <- as.factor(dat$anneeGestion)
  dat$nomSite <- as.factor(dat$nomSite)
  dat$sfs <- as.factor(dat$sfs)
  ##
  ## formules de standardisation selon les trois facteurs et l'unité d'effort
  formule <- list()
  formule[[especes[1]]] <- nbSebastes ~ offset(uniteEffort) +
    anneeGestion +
    nomSite +
    sfs
  formule[[especes[2]]] <- nbMorue ~ offset(uniteEffort) +
    anneeGestion +
    nomSite +
    sfs
  formule[[especes[3]]] <- nbOgac ~ offset(uniteEffort) +
    anneeGestion +
    nomSite +
    sfs
  formule[[especes[4]]] <- nbTurbot ~ offset(uniteEffort) +
    anneeGestion +
    nomSite +
    sfs
  ##
  ##
  ## ajustement du modèle
  fit <- list()
  for (i.esp in especes) {
    print(i.esp)
    formule.temp <- formule[[i.esp]]
    csum <- c("contr.sum", "contr.sum")
    names(csum) <- c("factor", "ordered")
    options(contrasts = csum)
    ## fit[[i.esp]] <- list()
    ## fit[[i.esp]][[secteurs[1]]] <- glm.nb(formule.temp, data=subset(dat, anneeGestion%in%annee.espece[[i.esp]]))
    ## fit[[i.esp]][[secteurs[2]]] <- glm.nb(formule.temp, data=subset(dat, anneeGestion%in%annee.espece[[i.esp]] &
    ##                                                                      nomSite%in%c('AnseBenjamin','GrandeBaie','LesBattures')))
    ## fit[[i.esp]][[secteurs[3]]] <- glm.nb(formule.temp, data=subset(dat, anneeGestion%in%annee.espece[[i.esp]] &
    ##                                                                      nomSite%in%c('SteRose','AnseStJean','StFelix','RiviereEternite')))
    fit[[i.esp]] <- MASS::glm.nb(
      formule.temp,
      data = subset(dat, anneeGestion %in% annee.espece[[i.esp]])
    )
  }
  ##
  ##
  return(list(
    fit = fit,
    dat = dat[, c(
      'nbSebastes',
      'nbMorue',
      'nbOgac',
      'nbTurbot',
      'anneeGestion',
      'nomSite',
      'sfs',
      'uniteEffort'
    )]
  ))
}
