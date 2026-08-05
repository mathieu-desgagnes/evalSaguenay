#' Standardisation des NUE obtenu par l'application "glace du fjord"
#'
#' Les strates sont définies selon l'année et l'identifiant pêcheur
#'
#' @param donnees l'ensemble des données de journaux de bord, nécessaitant l'années de gestion (anneeGestion), le site (nomSite),
#' le jour de semaine sous forme semaine/fin de semaine (sfs), les captures de chaque espèce et l'unité d'effort
#' @param uniteEffort le nom de la colonne contenant l'unité d'effort à utiliser, celle-ci ne doit pas avoir déjà été pré-transformée (log en particulier)
#' @param annees annees étudiées
#'
#' @importFrom MASS glm.nb
#'
#' @return list() de 2 data.frame() des données des deux graphiques
#'
std_nue_gdf <- function(donnees, uniteEffort, anneesFit = NULL) {
  if (is.null(anneesFit)) {
    anneesFit <- min(donnees$annee, na.rm = TRUE):max(
      donnees$annee,
      na.rm = TRUE
    )
  }
  especes <- c('sebastes', 'morue', 'ogac', 'turbot')
  ## retirer les unités d'effort non finies
  dat <- donnees[, c(
    'nbSebastes',
    'nbMorue',
    'nbOgac',
    'nbTurbot',
    'annee',
    'identifiant_utilisateur'
  )]
  dat$uniteEffort <- log(donnees[, uniteEffort])
  # nrow(dat)
  # hist(donnees[,uniteEffort] , breaks=seq(0,25,by=0.05))
  # hist(dat$uniteEffort)
  dat <- subset(dat, uniteEffort >= log(0.2))
  # hist(exp(dat$uniteEffort), breaks=seq(0,25,by=0.05))
  # nrow(dat)
  # temp <- nrow(dat)
  # dat <- dat[-which(!is.finite(dat$uniteEffort)), ]
  # if (nrow(dat) < temp) {
  #   print(paste(
  #     temp - nrow(dat),
  #     'enregistrements sur',
  #     temp,
  #     'ont été retiré pour effort invalide'
  #   ))
  # }
  dat$anneeGestion <- as.factor(dat$annee)
  dat$identifiant_utilisateur <- as.factor(dat$identifiant_utilisateur)
  table_ech <- table(dat$identifiant_utilisateur)
  # names(table_ech)[3] <- 'nb_heures_tot'
  # temp2 <- apply(table_ech, 1, function(x) {
  #   sum(x > 0)
  # })
  dat <- subset(
    dat,
    identifiant_utilisateur %in% names(table_ech)[which(table_ech > 1)]
  )
  ## nrow(dat)
  ## formules de standardisation selon les trois facteurs et l'unité d'effort
  formule <- list()
  formule[[especes[1]]] <- nbSebastes ~ offset(uniteEffort) +
    anneeGestion +
    identifiant_utilisateur
  formule[[especes[2]]] <- nbMorue ~ offset(uniteEffort) +
    anneeGestion +
    identifiant_utilisateur
  formule[[especes[3]]] <- nbOgac ~ offset(uniteEffort) +
    anneeGestion +
    identifiant_utilisateur
  formule[[especes[4]]] <- nbTurbot ~ offset(uniteEffort) +
    anneeGestion +
    identifiant_utilisateur
  # formule[[especes[1]]] <- nbSebastes ~ offset(uniteEffort) +
  #   anneeGestion +
  #   nomSite +
  #   sfs +
  #   village +
  #   echosondeur
  # formule[[especes[2]]] <- nbMorue ~ offset(uniteEffort) +
  #   anneeGestion +
  #   nomSite +
  #   sfs +
  #   village +
  #   echosondeur
  # formule[[especes[3]]] <- nbOgac ~ offset(uniteEffort) +
  #   anneeGestion +
  #   nomSite +
  #   sfs +
  #   village +
  #   echosondeur
  # formule[[especes[4]]] <- nbTurbot ~ offset(uniteEffort) +
  #   anneeGestion +
  #   nomSite +
  #   sfs +
  #   village +
  #   echosondeur
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
      data = subset(dat, anneeGestion %in% anneesFit)
    )
  }
  ##
  table_ech <- aggregate(
    dat$uniteEffort,
    dat[, c('anneeGestion', 'identifiant_utilisateur')],
    FUN = sum
  )
  ##
  return(list(
    fit = fit,
    dat = dat[, c(
      'nbSebastes',
      'nbMorue',
      'nbOgac',
      'nbTurbot',
      'anneeGestion',
      'identifiant_utilisateur',
      'uniteEffort'
    )],
    echantillonneurs = table_ech
  ))
}
