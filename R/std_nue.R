#' Traite les données d'échantillonneurs pour obtenir une nue standardisée
#'
#' @param ech données d'échantillonneurs
#'
#' @importFrom MASS glm.nb
#'
#' @returns
#' @export
#'
#' @examples ## à venir
std_nue <- function(ech, jb, gdf, output_dir, brut = FALSE) {
  if (!brut) {
    ##ajustement échantillonneurs
    if (FALSE) {
      #par défaut
      ech$ue <- ech$nbLignes * ech$nbHamecons * ech$nbHeures #unite d'effort
      ech$ue <- ech$nbLignes * ech$nbHeures #unite d'effort
      ech$ue <- ech$nbHeures #unite d'effort
    }
    ech$ue <- ech$nbHeures #unite d'effort
    ajustement <- std_nue_ech(ech = ech, uniteEffort = 'ue')
    # ajustement <- std_nue_ech(ech = subset(ech,nbLignes<=2), uniteEffort = 'nbHeures')
    ##ajustement jb
    # jb$ue <- jb$nbLignes *
    #   jb$nbHamecons *
    #   (jb$nbHeureImmersion + jb$nbMinuteImmersion / 60) #unite d'effort
    # ajustement.jb <- std_nue_jb(
    #   donnees = jb,
    #   uniteEffort = 'ue'
    # )
    jb$immersion <- jb$nbHeureImmersion + jb$nbMinuteImmersion / 60 #temps d'immersion
    ajustement.jb <- std_nue_jb(
      donnees = jb,
      uniteEffort = 'immersion'
    )
    # save(ajustement.jb, file = file.path(output_dir, 'csv', 'nueStdJB.RData'))
    ##
    ##ajustement gdf
    gdf$ue <- gdf$nombre_lignes_actives *
      gdf$nombre_hamecons *
      gdf$duree_minutes #unite d'effort
    # ajustement.gdf <- std_nue_gdf(
    #   donnees = gdf,
    #   uniteEffort = 'ue'
    # )
    gdf$duree_heures <- gdf$duree_minutes / 60
    ajustement.gdf <- std_nue_gdf(
      donnees = gdf,
      uniteEffort = 'duree_heures'
    )
    # save(ajustement.jb, file = file.path(output_dir, 'csv', 'nueStdJB.RData'))

    ##
    ##
    annees <- 1996:max(ech$anneeGestion, na.rm = TRUE)
    nomPng <- 'stdNue'
    for (i.langue in c('fr', 'en', 'bil')) {
      png(
        file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
        height = 8,
        width = 10,
        units = 'in',
        res = 300
      )
      temp <- graph_std_nue_ech(
        fit = ajustement$fit,
        dat = ajustement$dat,
        annees = annees,
        nueBrutes = FALSE,
        langue = i.langue
      )
      dev.off()
    }
    nueStd <- temp
    # save(nueStd, file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))
    # for (i.esp in seq_along(temp)) {
    #   write.csv2(
    #     temp[[i.esp]],
    #     file = file.path(
    #       output_dir,
    #       'csv',
    #       paste0(nomPng, '_', names(temp)[i.esp], '.csv')
    #     )
    #   )
    # }
    # load(file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))

    ##
    ## à faire: ajouter 'nombre heures pêchées et nombre de poissons capturés"
    ##

    ##journaux de bord
    ## à faire: corriger "différents" noms d'un même pêcheur
    ##
    if (FALSE) {
      # table(jb$anneeGestion, jb$nomSite, useNA = 'ifany')
      # table(jb$echantillonneur, jb$nomSite, useNA = 'ifany')
      table(jb$echantillonneur, jb$anneeGestion, useNA = 'ifany')
      # table(jb$nomSite)
    }
    ##
    ## proportion des espèces capturées
    ##
    nomPng <- 'propCaptJB'
    for (i.langue in c('fr', 'en', 'bil')) {
      png(
        file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
        height = 4.5,
        width = 10,
        units = 'in',
        res = 300
      )
      par(mfrow = c(1, 2))
      temp <- graph_capt_par_activite_jb(donnees = jb, langue = i.langue)
      temp <- graph_prop_par_activite_jb(
        donnees = jb,
        titre = 'B',
        langue = i.langue
      )
      dev.off()
    }
    ##
    ##
    nomPng <- 'stdNueJB'
    for (i.langue in c('fr', 'en', 'bil')) {
      png(
        file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
        height = 8,
        width = 10,
        units = 'in',
        res = 300
      )
      temp <- graph_std_nue_jb(
        fit = ajustement.jb$fit,
        dat = ajustement.jb$dat,
        anneesFit = 2015:2026,
        limitesX = c(1996, 2026),
        nueBrutes = FALSE,
        langue = i.langue
      )
      dev.off()
    }
    nueStd.jb <- temp
    # save(nueStd.jb, file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))
    # for (i.esp in seq_along(temp)) {
    #   write.csv2(
    #     temp[[i.esp]],
    #     file = file.path(
    #       output_dir,
    #       'csv',
    #       paste0(nomPng, '_', names(temp)[i.esp], '.csv')
    #     )
    #   )
    # }
    # load(
    #   file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')),
    #   verbose = 1
    # )
    #
    nomPng <- 'stdNueGDF'
    for (i.langue in c('fr', 'en', 'bil')) {
      png(
        file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
        height = 8,
        width = 10,
        units = 'in',
        res = 300
      )
      temp <- graph_std_nue_gdf(
        fit = ajustement.gdf$fit,
        dat = ajustement.gdf$dat,
        anneesFit = 2025:2026,
        limitesX = c(1996, 2026),
        nueBrutes = FALSE,
        langue = i.langue
      )
      dev.off()
    }
    nueStd.jb <- temp
    # save(nueStd.jb, file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')))
    # for (i.esp in seq_along(temp)) {
    #   write.csv2(
    #     temp[[i.esp]],
    #     file = file.path(
    #       output_dir,
    #       'csv',
    #       paste0(nomPng, '_', names(temp)[i.esp], '.csv')
    #     )
    #   )
    # }
    # load(
    #   file = file.path(output_dir, 'csv', paste0(nomPng, '.RData')),
    #   verbose = 1
    # )

    ##
    ## standardisation des ech et des jb sur même graphique
    nomPng <- 'stdNue2sources'
    for (i.langue in c('fr', 'en', 'bil')) {
      png(
        file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
        height = 8,
        width = 10,
        units = 'in',
        res = 300
      )
      par(mfrow = c(2, 2))
      ##
      j.text <- 1
      for (i.esp in c('sebastes', 'morue', 'ogac', 'turbot')) {
        graph_std_nue_2sources(
          nue.ech = nueStd[[i.esp]],
          nue.jb = nueStd.jb[[i.esp]],
          limitesX = 1995:2026,
          main.txt = '',
          abcd = c('A', 'B', 'C', 'D')[j.text],
          legende = c(TRUE, rep(FALSE, 3))[j.text],
          langue = i.langue
        )
        j.text <- j.text + 1
      }
      dev.off()
    }
  } else {
    nomPng <- 'nueBrut'
    for (i.langue in c('fr', 'en', 'bil')) {
      png(
        file = file.path(output_dir, i.langue, paste0(nomPng, '.png')),
        height = 8,
        width = 10,
        units = 'in',
        res = 300
      )
      par(mfrow = c(2, 2))
      ##
      ### rendements bruts selon les sources et par espèce
      ## sébaste
      nue <- aggregate(
        ech$nbSebastes / ech$nbHeures,
        ech['anneeGestion'],
        FUN = mean,
        na.rm = TRUE
      )
      names(nue) <- c('annee', 'ech')
      ##
      jb.temp <- subset(jb, immersion >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          jb.temp$nbSebastes / jb.temp$immersion,
          jb.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'jb')
      # ## jb village
      # table(jb$village, useNA = 'ifany')
      # jb.temp <- subset(jb, immersion >= 0.2 & jb$village)
      # nue <- merge(
      #   nue,
      #   aggregate(
      #     jb.temp$nbSebastes / jb.temp$immersion,
      #     jb.temp['annee'],
      #     FUN = mean,
      #     na.rm = TRUE
      #   ),
      #   by = 'annee',
      #   all = TRUE
      # )
      # names(nue) <- c(head(names(nue), -1), 'jb.village')
      # ## jb non-village
      # jb.temp <- subset(jb, immersion >= 0.2 & !jb$village)
      # nue <- merge(
      #   nue,
      #   aggregate(
      #     jb.temp$nbSebastes / jb.temp$immersion,
      #     jb.temp['annee'],
      #     FUN = mean,
      #     na.rm = TRUE
      #   ),
      #   by = 'annee',
      #   all = TRUE
      # )
      # names(nue) <- c(head(names(nue), -1), 'jb.non_village')
      ## gdf
      gdf.temp <- subset(gdf, duree_minutes / 60 >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          gdf.temp$nbSebastes / (gdf.temp$duree_minutes / 60),
          gdf.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'gdf')

      temp <- apply(na.omit(nue[, c('ech', 'jb')]), 2, mean)
      ratio1 <- temp[1] / temp[2]
      temp <- apply(na.omit(nue[, c('ech', 'gdf')]), 2, mean)
      ratio2 <- temp[1] / temp[2]
      # temp <- apply(na.omit(nue[, c('ech', 'jb.village')]), 2, mean)
      # ratio3 <- temp[1] / temp[2]

      # Valeur absolue
      #
      # plot(
      #   nue[, c('annee', 'ech')],
      #   type = 'o',
      #   ylim = c(0, 1.4),
      #   yaxs = 'i',
      #   col = 2,
      #   xlab = 'Année',
      #   ylab = 'Nombre par heure de pêche'
      # )
      # lines(nue$annee, nue$jb, type = 'o', col = 3)
      # # lines(nue$annee, nue$jb.village, type = 'o', col = 3, lty=3)
      # # lines(nue$annee, nue$jb.non_village, type = 'o', col = 3, lty=3)
      # lines(nue[, c('annee', 'gdf')], type = 'o', col = 4)
      # legend(
      #   'bottomleft',
      #   inset = 0.03,
      #   legend = c('Echant.', 'Journaux', 'Appli.'),
      #   pch = 1,
      #   lty = 1,
      #   col = c(2, 3, 4),
      #   title = 'Source'
      # )

      #ratio de mise à l'échelle
      plot(
        nue[, c('annee', 'ech')],
        type = 'o',
        ylim = c(0, max(nue$ech, na.rm = TRUE) * 1.05),
        yaxs = 'i',
        col = 2,
        xlab = 'Année',
        ylab = 'Nombre par heure de pêche'
      )
      lines(nue$annee, nue$jb * ratio1, type = 'o', col = 3)
      lines(nue$annee, nue$gdf * ratio2, type = 'o', col = 4)
      lines(
        smooth.spline(
          na.omit(cbind(
            annee = rep(nue$annee, 3),
            val = c(nue$ech, nue$jb * ratio1, nue$gdf * ratio2)
          )),
          df = 8
        ),
        lwd = 2
      )
      text(
        x = mean(par('usr')[1:2]),
        y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
        labels = 'A',
        cex = 1.5
      )
      legend(
        'topright',
        inset = 0.03,
        legend = paste(
          c('Ech =', 'JB =', 'Appli ='),
          round(1 / c(1, ratio1, ratio2), 2)
        ),
        pch = 1,
        lty = 1,
        col = c(2, 3, 4),
        title = 'Multiplicateur'
      )

      ##
      ##
      ##
      ## morue
      nue <- aggregate(
        ech$nbMorue / ech$nbHeures,
        ech['anneeGestion'],
        FUN = mean,
        na.rm = TRUE
      )
      names(nue) <- c('annee', 'ech')
      jb.temp <- subset(jb, immersion >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          jb.temp$nbMorue / jb.temp$immersion,
          jb.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'jb')
      gdf.temp <- subset(gdf, duree_minutes / 60 >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          gdf.temp$nbMorue / (gdf.temp$duree_minutes / 60),
          gdf.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'gdf')

      temp <- apply(na.omit(nue[, c('ech', 'jb')]), 2, mean)
      ratio1 <- temp[1] / temp[2]
      temp <- apply(na.omit(nue[, c('ech', 'gdf')]), 2, mean)
      ratio2 <- temp[1] / temp[2]

      #valeur absolue
      # plot(
      #   nue[, c('annee', 'ech')],
      #   type = 'o',
      #   ylim = c(0, .09),
      #   yaxs = 'i',
      #   col = 2,
      #   xlab = 'Année',
      #   ylab = 'Nombre par heure de pêche'
      # )
      # lines(nue$annee, nue$jb, type = 'o', col = 3)
      # lines(nue[, c('annee', 'gdf')], type = 'o', col = 4)
      # legend(
      #   'topleft',
      #   inset = 0.03,
      #   legend = c('Echant.', 'Journaux', 'Appli.'),
      #   pch = 1,
      #   lty = 1,
      #   col = c(2, 3, 4),
      #   title = 'Source'
      # )

      #ratio de mise à l'échelle
      plot(
        nue[, c('annee', 'ech')],
        type = 'o',
        ylim = c(0, max(nue$ech, na.rm = TRUE) * 1.15),
        yaxs = 'i',
        col = 2,
        xlab = 'Année',
        ylab = 'Nombre par heure de pêche'
      )
      lines(nue$annee, nue$jb * ratio1, type = 'o', col = 3)
      lines(nue$annee, nue$gdf * ratio2, type = 'o', col = 4)
      lines(
        smooth.spline(
          na.omit(cbind(
            annee = rep(nue$annee, 3),
            val = c(nue$ech, nue$jb * ratio1, nue$gdf * ratio2)
          )),
          df = 8
        ),
        lwd = 2
      )
      text(
        x = mean(par('usr')[1:2]),
        y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
        labels = 'B',
        cex = 1.5
      )
      legend(
        'bottomleft',
        inset = 0.03,
        legend = paste(
          c('Ech =', 'JB =', 'Appli ='),
          round(1 / c(1, ratio1, ratio2), 2)
        ),
        pch = 1,
        lty = 1,
        col = c(2, 3, 4),
        title = 'Multiplicateur'
      )

      ##
      ##
      ##
      ## ogac
      nue <- aggregate(
        ech$nbOgac / ech$nbHeures,
        ech['anneeGestion'],
        FUN = mean,
        na.rm = TRUE
      )
      names(nue) <- c('annee', 'ech')
      jb.temp <- subset(jb, immersion >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          jb.temp$nbOgac / jb.temp$immersion,
          jb.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'jb')
      gdf.temp <- subset(gdf, duree_minutes / 60 >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          gdf.temp$nbOgac / (gdf.temp$duree_minutes / 60),
          gdf.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'gdf')

      temp <- apply(na.omit(nue[, c('ech', 'jb')]), 2, mean)
      ratio1 <- temp[1] / temp[2]
      temp <- apply(na.omit(nue[, c('ech', 'gdf')]), 2, mean)
      ratio2 <- temp[1] / temp[2]

      #ratio de mise à l'échelle
      plot(
        nue[, c('annee', 'ech')],
        type = 'o',
        ylim = c(0, max(nue$ech, na.rm = TRUE) * 1.15),
        yaxs = 'i',
        col = 2,
        xlab = 'Année',
        ylab = 'Nombre par heure de pêche'
      )
      lines(nue$annee, nue$jb * ratio1, type = 'o', col = 3)
      lines(nue$annee, nue$gdf * ratio2, type = 'o', col = 4)
      lines(
        smooth.spline(
          na.omit(cbind(
            annee = rep(nue$annee, 3),
            val = c(nue$ech, nue$jb * ratio1, nue$gdf * ratio2)
          )),
          df = 8
        ),
        lwd = 2
      )
      text(
        x = mean(par('usr')[1:2]),
        y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
        labels = 'C',
        cex = 1.5
      )
      legend(
        'topright',
        inset = 0.03,
        legend = paste(
          c('Ech =', 'JB =', 'Appli ='),
          round(1 / c(1, ratio1, ratio2), 2)
        ),
        pch = 1,
        lty = 1,
        col = c(2, 3, 4),
        title = 'Multiplicateur'
      )

      ##
      ##
      ##
      ## turbot
      nue <- aggregate(
        ech$nbTurbot / ech$nbHeures,
        ech['anneeGestion'],
        FUN = mean,
        na.rm = TRUE
      )
      names(nue) <- c('annee', 'ech')
      jb.temp <- subset(jb, immersion >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          jb.temp$nbTurbot / jb.temp$immersion,
          jb.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'jb')
      gdf.temp <- subset(gdf, duree_minutes / 60 >= 0.2)
      nue <- merge(
        nue,
        aggregate(
          gdf.temp$nbTurbot / (gdf.temp$duree_minutes / 60),
          gdf.temp['annee'],
          FUN = mean,
          na.rm = TRUE
        ),
        by = 'annee',
        all = TRUE
      )
      names(nue) <- c(head(names(nue), -1), 'gdf')

      temp <- apply(na.omit(nue[, c('ech', 'jb')]), 2, mean)
      ratio1 <- temp[1] / temp[2]
      temp <- apply(na.omit(nue[, c('ech', 'gdf')]), 2, mean)
      ratio2 <- temp[1] / temp[2]

      #ratio de mise à l'échelle
      plot(
        nue[, c('annee', 'ech')],
        type = 'o',
        ylim = c(0, max(nue$ech, na.rm = TRUE) * 1.15),
        yaxs = 'i',
        col = 2,
        xlab = 'Année',
        ylab = 'Nombre par heure de pêche'
      )
      lines(nue$annee, nue$jb * ratio1, type = 'o', col = 3)
      lines(nue$annee, nue$gdf * ratio2, type = 'o', col = 4)
      lines(
        smooth.spline(
          na.omit(cbind(
            annee = rep(nue$annee, 3),
            val = c(nue$ech, nue$jb * ratio1, nue$gdf * ratio2)
          )),
          df = 8
        ),
        lwd = 2
      )
      text(
        x = mean(par('usr')[1:2]),
        y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
        labels = 'D',
        cex = 1.5
      )
      legend(
        'topleft',
        inset = 0.03,
        legend = paste(
          c('Ech =', 'JB =', 'Appli ='),
          round(1 / c(1, ratio1, ratio2), 2)
        ),
        pch = 1,
        lty = 1,
        col = c(2, 3, 4),
        title = 'Multiplicateur'
      )

      dev.off()
    }
  }
}
