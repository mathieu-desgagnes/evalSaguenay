#' Gaphiques de la standardisation des NUE obtenues des journaux de bord, pour l'ensemble du Saguenay
#'
#' Les strates sont définies selon l'année, le site et le jour de capture, soit semaine ou fin de semaine.
#' Cette version du code ne produit que les valeurs pour l'ensemble du Saguenay, laissant de côté les valeurs pour le secteur La Baie et PMSSL,
#' tel que fait dans le passé.
#'
#' @param fit list() par espèces de l'ajustement des modèles
#' @param dat les données nettoyées utilisée pour l'ajustement
#' @param anneesFit vector() des années à utiliser
#' @param nueBrute si TRUE, ajoute au graphique la moyenne non standardisée des nue
#' @param langue trois choix de langue du texte dans le graphique, soit français ('fr'), anglais('en') ou bilingue('bil')
#'
#' @return list() de 2 data.frame() des données des deux graphiques
#'
graph_std_nue_jb <- function(
  fit,
  dat,
  anneesFit,
  nueBrutes = FALSE,
  limitesX = 1996:2023,
  especes = c('sebastes', 'morue', 'ogac', 'turbot'),
  main.txt = '',
  abcd = TRUE,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'NUE (prises/hameçon-heure)'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'NUE (catch/hook-hour)'
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'NUE'
    }
  )
  op <- par(mar = c(4, 4, 1, 3) + 0.1)
  ##
  if (FALSE) {
    ## travail exploratoire pour inclure valeurs brutes
    i.esp <- 1
    dat.esp <- subset(dat, anneeGestion %in% anneesFit)
    dat.esp$anneeGestion <- as.numeric(as.character(dat.esp$anneeGestion))

    pred <- predict(fit[[especes[i.esp]]], type = 'response', se = TRUE)
    ## plot(fit[[especes[i.esp]]]$model[,1], pred, cex=0.8, xlab='observé', ylab='predit')
    ## abline(a=0,b=1, col=2)
    ##
    yr.ref <- 2023
    mod.variab <- c("anneeGestion", "echantillonneur")
    pred.grid <- dat.esp[dat.esp$anneeGestion == yr.ref, mod.variab]
    pred.grid <- dat.esp[, mod.variab]
    tmp <- rep(
      unique(dat.esp$anneeGestion),
      rep(dim(pred.grid)[1], length(unique(dat.esp$anneeGestion)))
    )
    for (i in 1:(length(unique(dat.esp$anneeGestion)) - 1)) {
      pred.grid <- rbind(pred.grid, dat.esp[, mod.variab])
    }
    ##
    pred.grid$anneeGestion <- tmp
    pred <- predict(
      fit[[especes[i.esp]]],
      pred.grid,
      type = "response",
      se = TRUE
    )
    pred <- predict(
      fit[[especes[i.esp]]],
      newdata = list(
        anneeGestion = as.factor(c(2015:2026)),
        echantillonneur = as.factor('Alain Gagnon'),
      ),
      type = "response",
      se = TRUE
    )
    ##
    ## dev.new()
    ## png(file=paste0(dirOutput,'pueComm.png'), height=6, width=8, units='in', res=300)
    ## par(mfrow=c(1,1))
    obs.yr <- aggregate(
      fit[[especes[i.esp]]]$model[, 1] / exp(fit[[especes[i.esp]]]$model[, 2]),
      fit[[especes[i.esp]]]$model['anneeGestion'],
      FUN = mean,
      na.rm = TRUE
    )
    pred.yr <- aggregate(
      pred$fit / exp(fit[[especes[i.esp]]]$model[, 2]),
      list(pred.grid$anneeGestion),
      FUN = mean
    )
    plot(obs.yr, yaxs = 'i', ylim = c(0, max(obs.yr$x, pred.yr$x) * 1.05))
    points(pred.yr, col = 2)

    coefficients(fit[[especes[i.esp]]])

    pred.yr.se.up <- aggregate(
      pred$fit + 1.96 * pred$se.fit,
      list(pred.grid$anneeGestion),
      mean
    )
    pred.yr.se.lo <- aggregate(
      pred$fit - 1.96 * pred$se.fit,
      list(pred.grid$anneeGestion),
      mean
    )
    se.yr <- aggregate(pred$se.fit, list(pred.grid$anneeGestion), mean)
    plot(obs.yr[, 1], exp(obs.yr$x), type = "p", pch = 16, col = 4)
    abline(h = 0, col = 'grey70')
    lines(pred.yr[, 1], pred.yr$fit, lty = 1)
    lines(pred.yr.se.up[, 1], exp(pred.yr.se.up$x), lty = 2)
    lines(pred.yr.se.lo[, 1], exp(pred.yr.se.lo$x), lty = 2)
  }
  ##
  ## fonction qui retourne une list() des coefficients du modèles
  coeffModel <- function(fit.temp, x, data) {
    coeffs <- fit.temp$coefficients
    stderr <- summary(fit.temp)$coeff[, "Std. Error"]
    covar <- summary(fit.temp)$cov.unscaled
    fact <- x
    z <- is.element(substring(names(coeffs), 1, nchar(fact)), fact)
    fcoef <- coeffs[z]
    fcont <- contrasts(as.factor(as.character(data[[x]])))
    fcoef <- fcont %*% fcoef
    fcoef <- as.vector(fcoef)
    fserr <- stderr[z]
    fcov <- covar[z, z]
    errZ <- sqrt(sum(fcov))
    fserr <- c(fserr, errZ)
    return(list(
      coeff = fcoef,
      errstd = fserr,
      intercept = coeffs["(Intercept)"]
    ))
  }
  ##
  ##
  val <- list()
  if (length(especes) > 1) {
    par(mfrow = c(2, 2))
  }
  j.text <- 1
  for (i.esp in seq_along(especes)) {
    data.secteur <- subset(dat, anneeGestion %in% anneesFit)
    coeffGraph <- list()
    if (!is.null(fit[[especes[i.esp]]])) {
      coeffGraph$anneeGestion <- coeffModel(
        fit.temp = fit[[especes[i.esp]]],
        x = "anneeGestion",
        data = data.secteur
      )
      coeffGraph$echantillonneur <- coeffModel(
        fit[[especes[i.esp]]],
        "echantillonneur",
        data.secteur
      )
    }
    if (nueBrutes) {
      ## calcul des nue brutes
      nomColEsp <- paste0(
        'nb',
        c('Sebastes', 'Morue', 'Ogac', 'Turbot')[match(
          especes[i.esp],
          c('sebastes', 'morue', 'ogac', 'turbot')
        )]
      )
      brut <- aggregate(
        data.secteur[, nomColEsp] / exp(data.secteur$uniteEffort),
        data.secteur[c('anneeGestion')],
        FUN = mean,
        na.rm = TRUE
      )
      dimnames(brut)[[2]] <- c(head(dimnames(brut)[[2]], -1), 'moy')
      ##
      brut.temp <- aggregate(
        data.secteur[, nomColEsp] / exp(data.secteur$uniteEffort),
        data.secteur[c('anneeGestion')],
        FUN = length
      )
      brut$n <- brut.temp[, 'x']
      brut.temp <- aggregate(
        data.secteur[, nomColEsp] / exp(data.secteur$uniteEffort),
        data.secteur[c('anneeGestion')],
        FUN = sd,
        na.rm = TRUE
      )
      brut$sd <- brut.temp[, 'x']
      brut$se <- brut$sd / sqrt(brut$n)
      ##
      ratio <- mean(exp(coeffGraph$annee$coeff + coeffGraph$annee$intercept)) /
        mean(brut$moy)
      ##
      maxx1 <- max(
        max(ifelse(
          exp(max(
            coeffGraph$anneeGestion$coeff +
              coeffGraph$anneeGestion$intercept +
              coeffGraph$anneeGestion$errstd * 1.96
          )) <
            10,
          exp(max(
            coeffGraph$anneeGestion$coeff +
              coeffGraph$anneeGestion$intercept +
              coeffGraph$anneeGestion$errstd * 1.96
          )) *
            1.05,
          exp(max(
            coeffGraph$anneeGestion$coeff + coeffGraph$anneeGestion$intercept
          )) *
            1.05
        )),
        max((brut$moy + brut$se * 1.96) * 1.05) * ratio
      )
      ##
    } else {
      maxx1 <- max(ifelse(
        exp(max(
          coeffGraph$anneeGestion$coeff +
            coeffGraph$anneeGestion$intercept +
            coeffGraph$anneeGestion$errstd * 1.96
        )) <
          10,
        exp(max(
          coeffGraph$anneeGestion$coeff +
            coeffGraph$anneeGestion$intercept +
            coeffGraph$anneeGestion$errstd * 1.96
        )) *
          1.05,
        exp(max(
          coeffGraph$anneeGestion$coeff + coeffGraph$anneeGestion$intercept
        )) *
          1.05
      ))
    }
    ## maxx2 <- max(ifelse(exp(max(coeffGraph$nomSite$coeff + coeffGraph$nomSite$errstd*1.96))<10,
    ##                     exp(max(coeffGraph$nomSite$coeff + coeffGraph$nomSite$errstd*1.96))*1.05,
    ##                     exp(max(coeffGraph$nomSite$coeff))*1.05))
    ## maxx3 <- max(ifelse(exp(max(coeffGraph$sfs$coeff + coeffGraph$sfs$errstd*1.96))<10,
    ##                     exp(max(coeffGraph$sfs$coeff + coeffGraph$sfs$errstd*1.96))*1.05,
    ##                     exp(max(coeffGraph$sfs$coeff))*1.05))
    ##
    cg <- coeffGraph
    ##
    ## anneeGestion
    temp <- rbind(
      exp(cg$anneeGestion$coeff + cg$anneeGestion$intercept),
      exp(
        cg$anneeGestion$coeff +
          cg$anneeGestion$intercept -
          cg$anneeGestion$errstd * 1.96
      ),
      exp(
        cg$anneeGestion$coeff +
          cg$anneeGestion$intercept +
          cg$anneeGestion$errstd * 1.96
      )
    )
    dimnames(temp) <- list(c('valeur', 'IC95-', 'IC95+'), anneesFit)
    val[[especes[i.esp]]] <- temp
    temp <- cbind(annee = anneesFit, exp(cg$annee$coeff + cg$annee$intercept))
    temp <- merge(temp, list(annee = anneesFit), all = TRUE, by = 'annee')
    plot(
      temp,
      type = "o",
      xlim = limitesX,
      ylim = c(
        0,
        maxx1 #ifelse(exp(max(cg$annee$coeff+cg$annee$intercept + cg$annee$errstd*1.96))<10,
        #  exp(max(cg$annee$coeff+cg$annee$intercept + cg$annee$errstd*1.96))*1.05,
        # exp(max(cg$annee$coeff+cg$annee$intercept))*1.05)
      ),
      yaxs = "i",
      xlab = xlab1,
      ylab = ylab1,
      main = main.txt
    )
    for (i.an in seq_along(cg$anneeGestion$coeff)) {
      lines(
        rep(anneesFit[i.an], 2),
        exp(c(
          cg$anneeGestion$coeff[i.an] +
            cg$anneeGestion$intercept -
            cg$anneeGestion$errstd[i.an] * 1.96,
          cg$anneeGestion$coeff[i.an] +
            cg$anneeGestion$intercept +
            cg$anneeGestion$errstd[i.an] * 1.96
        )),
        col = "blue"
      )
    }
    ##
    if (nueBrutes) {
      for (i.an in 1:nrow(brut)) {
        points(
          as.numeric(as.character(brut[i.an, 'anneeGestion'])) + 0.1,
          brut[i.an, 'moy'] * ratio,
          col = 2
        )
        lines(
          rep(as.numeric(as.character(brut[i.an, 'anneeGestion'])) + 0.1, 2),
          (brut[i.an, 'moy'] + 1.96 * c(-brut[i.an, 'se'], brut[i.an, 'se'])) *
            ratio,
          col = 2
        )
      }
      axis(
        4,
        at = pretty(par('usr')[3:4] / ratio) * ratio,
        labels = pretty(par('usr')[3:4] / ratio),
        col = 2,
        col.axis = 2
      )
    }
    ##
    if (abcd) {
      text(
        x = mean(par('usr')[1:2]),
        y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
        labels = c('A', 'B', 'C', 'D')[j.text],
        cex = 1.5
      )
    }
    j.text = j.text + 1
    ##
    if (FALSE) {
      ## nomSite
      temp <- rbind(
        exp(cg$nomSite$coeff),
        exp(cg$nomSite$coeff - cg$nomSite$errstd * 1.96),
        exp(cg$nomSite$coeff + cg$nomSite$errstd * 1.96)
      )
      dimnames(temp) <- list(
        c('valeur', 'IC95-', 'IC95+'),
        c(
          'AnseBenjamin',
          'AnseStJean',
          'GrandeBaie',
          'LesBattures',
          'RiviereEternite',
          'SteRose',
          'StFelix',
          'StFulgence'
        )
      )
      plot(
        1:length(cg$nomSite$coeff),
        exp(cg$nomSite$coeff),
        ylim = c(
          0,
          maxx2 #exp(max(cg$site$coeff + cg$site$errstd*1.96))*1.05
        ),
        yaxs = "i",
        xlab = "Site",
        ylab = "Effet sur NUE",
        xaxt = "n"
      )
      for (i.site in 1:length(cg$nomSite$coeff)) {
        lines(
          rep(i.site, 2),
          exp(c(
            cg$nomSite$coeff[i.site] - cg$nomSite$errstd[i.site] * 1.96,
            cg$nomSite$coeff[i.site] + cg$nomSite$errstd[i.site] * 1.96
          )),
          col = "blue"
        )
      }
      axis(1, 1:length(axes.site[[i.cg]]), axes.site[[i.cg]])
      abline(h = 1)
      ## sfs
      temp <- rbind(
        exp(cg$sfs$coeff),
        exp(cg$sfs$coeff - cg$sfs$errstd * 1.96),
        exp(cg$sfs$coeff + cg$sfs$errstd * 1.96)
      )
      dimnames(temp) <- list(c('valeur', 'IC95-', 'IC95+'), c("S", "FS"))
      plot(
        1:length(cg$sfs$coeff),
        exp(cg$sfs$coeff),
        ylim = c(
          0,
          maxx3 #exp(max(cg$sfs$coeff + cg$sfs$errstd*1.96))*1.05
        ),
        xlim = c(0, 3),
        yaxs = "i",
        xlab = "Semaine / Fin de semaine",
        ylab = "Effet sur NUE",
        xaxt = "n"
      )
      axis(1, 1:2, c("S", "FS"))
      for (i.sfs in 1:length(cg$sfs$coeff)) {
        lines(
          rep(i.sfs, 2),
          exp(c(
            cg$sfs$coeff[i.sfs] - cg$sfs$errstd[i.sfs] * 1.96,
            cg$sfs$coeff[i.sfs] + cg$sfs$errstd[i.sfs] * 1.96
          )),
          col = "blue"
        )
      }
      abline(h = 1)
    }
  }
  ##
  return(val)
}
