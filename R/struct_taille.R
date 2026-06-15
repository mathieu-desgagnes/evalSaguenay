#' Gaphiques des structures de tailles provenant des données biologiques
#'
#' valeurs de croissance du sébaste dans le golfe (Senay, C., Rousseau, S., Brule, C., Chavarria, C., Isabel, L., Parent, G.J., Chabot, D., and Duplisea, D. 2023. Unit 1 Redfish (Sebastes mentella and S. fasciatus) stock status in 2021. DFO Can. Sci. Advis. Sec. Res. Doc. 2023/036. xi + 125.:
#' Data          Linf constraint Linfinity k      t0   Curve
#' 1980          42-50 cm        42        0.086 -1.57 Black
#' 2011          42-50 cm        42        0.079 -1.81 Blue
#' 1980 and 2011 42-50 cm        42        0.085 -1.52 Orange
#' 1980          Unconstrained   37        0.153  0.07 Black dotted
#' 2011          Unconstrained   28        0.200 -0.17 Blue dotted
#' 1980 and 2011 Unconstrained   37        0.132 -0.24 Orange dotted
#'
#' @param db données
#'
#' @import vioplot
#'
#' @return
#'
struct_taille <- function(
  donnees,
  donnees_gdf = NULL,
  langue = c('fr', 'en', 'bil')
) {
  switch(
    langue,
    'fr' = {
      xlab1 <- 'Année'
      ylab1 <- 'Longueur (mm)'
      legend.morue <- 'Morue franche'
      legende.gadide <- 'Gadidé non-spécifié'
    },
    'en' = {
      xlab1 <- 'Year'
      ylab1 <- 'Length (mm)'
      legend.morue <- 'Atlantic cod'
      legende.gadide <- 'Unspecified Gadidae'
    },
    'bil' = {
      xlab1 <- 'Année/Year'
      ylab1 <- 'Longueur/Length (mm)'
      legend.morue <- 'Morue franche/Atlantic cod'
      legende.gadide <- 'Gadidae, non-spécifié/unspecified '
    }
  )
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 1, 1) + 0.1)
  ##
  ## Sébastes
  if (FALSE) {
    #sans Talbot1991
    temp <- subset(donnees, espece == 'sebastes')
    x <- vioplot::vioplot(
      longueur ~ anneeGestion,
      data = temp,
      h = 8,
      at = sort(unique(temp$anneeGestion)),
      main = 'Sébaste, Saguenay',
      xlab = 'Année',
      ylab = 'Longueur (mm)',
      ylim = c(0, 500)
    )
    axis(
      3,
      at = sort(unique(temp$anneeGestion)),
      labels = table(temp$anneeGestion),
      cex.axis = 0.8,
      tick = FALSE,
      line = -1,
      las = 2
    )
    ##
    x <- 0:60
    y <- 33 * (1 - exp(-0.11 * (x - (0)))) * 10
    lines(x + 1980, y, lwd = 3, lty = 2, col = 3)
    lines(x + 2011, y, lwd = 3, lty = 2, col = 3)
    legend(
      'bottomleft',
      inset = 0.03,
      legend = paste(c('Linf', 'K', 't0'), ':', c(33, 0.105, 0))
    )
  }
  ##
  ## en incluant (hard coded) les données reprises de talbot 1991
  db.sebaste <- subset(donnees, espece == 'sebastes')
  talbot <- c(
    16,
    rep(17, 2),
    rep(18, 2),
    rep(19, 29),
    rep(20, 95),
    rep(21, 132),
    rep(22, 130),
    rep(23, 189),
    rep(24, 167),
    rep(25, 179),
    rep(26, 142),
    rep(27, 83),
    rep(28, 24),
    rep(29, 11),
    30,
    31
  ) *
    10
  test <- as.data.frame(cbind(
    anneeGestion = rep(1991, length(talbot)),
    longueur = talbot,
    espece = 'sebastes'
  ))
  test$longueur <- as.numeric(test$longueur)
  test$anneeGestion <- as.numeric(test$anneeGestion)
  db.sebaste <- merge(db.sebaste, test, all = TRUE)
  x <- vioplot::vioplot(
    longueur ~ anneeGestion,
    data = db.sebaste,
    h = 8,
    at = sort(unique(db.sebaste$anneeGestion)),
    col = 3,
    lineCol = NA,
    ## main='Sébaste, Saguenay',
    xlab = xlab1,
    ylab = ylab1,
    xlim = c(1981, max(db.sebaste$anneeGestion, na.rm = TRUE) + 1),
    ylim = c(0, 500),
    xaxt = 'n'
  )
  ## abline(h=seq(0,500,by=100), col='grey70')
  ## x <- vioplot(longueur~anneeGestion, data=db.sebaste, h=8, at=sort(unique(db.sebaste$anneeGestion)), col=3, lineCol=NA,
  ##              ## main='Sébaste, Saguenay',
  ##              xlab='Année', ylab='Longueur (mm)', xlim=c(1981, 2024), ylim=c(0,500), xaxt='n', add=TRUE)
  axis(1)
  axis(
    3,
    at = sort(unique(db.sebaste$anneeGestion)),
    labels = table(db.sebaste$anneeGestion),
    cex.axis = 0.4,
    tick = FALSE,
    line = -0.8,
    las = 2
  )
  ##
  if (!is.null(donnees_gdf)) {
    # donnees_gdf$taille <- donnees_gdf$taille*10
    temp <- subset(donnees_gdf, espece == 'Sébaste' & taille < 100)
    x <- vioplot::vioplot(
      10 * taille ~ annee,
      data = temp,
      # h = 8,
      at = sort(unique(donnees_gdf$annee)) + 0.3,
      col = 6,
      lineCol = NA,
      ## main='Sébaste, Saguenay',
      add = TRUE
    )
    axis(
      3,
      at = sort(unique(temp$annee)),
      labels = table(temp$annee),
      cex.axis = 0.4,
      tick = FALSE,
      line = -1.8,
      las = 2,
      col.axis = 6
    )
    x <- vioplot::vioplot(
      longueur ~ anneeGestion,
      data = db.sebaste,
      h = 8,
      at = sort(unique(db.sebaste$anneeGestion)),
      col = 3,
      lineCol = NA,
      ## main='Sébaste, Saguenay',
      add = TRUE
    )
  }
  x <- 0:60
  y <- 33 * (1 - exp(-0.11 * (x - (0)))) * 10 #; cbind(x,y)
  lines(x + 1980, y, lwd = 3, lty = 2)
  lines(x + 2011, y, lwd = 3, lty = 2)
  if (FALSE) {
    #données du GSL
    x <- 0:60 #1980, non-contraint
    y <- 37 * (1 - exp(-0.153 * (x - (0.07)))) * 10 #; cbind(x,y)
    lines(x + 1980, y, lwd = 3, lty = 3, col = 2)
    x <- 0:60 #2011, non-contraint
    y <- 28 * (1 - exp(-0.2 * (x - (-0.17)))) * 10 #; cbind(x,y)
    lines(x + 2011, y, lwd = 3, lty = 3, col = 2)
    x <- 0:60 #1980, contraint
    y <- 42 * (1 - exp(-0.086 * (x - (-1.57)))) * 10 #; cbind(x,y)
    lines(x + 1980, y, lwd = 3, lty = 3, col = 4)
  }
  j.text <- 1
  text(
    x = mean(par('usr')[1:2]),
    y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
    labels = c('A', 'B', 'C', 'D')[j.text],
    cex = 1.5
  )
  j.text = j.text + 1
  legend(
    'topleft',
    inset = 0.03,
    legend = paste(c('Linf', 'K', 't0'), ':', c(33, 0.11, 0)),
    bg = 'white'
  )
  ##
  ## distribution frequences de taille
  if (FALSE) {
    ##dfl traditionnelle
    plot(
      0,
      0,
      type = 'n',
      xlim = c(100, 400),
      ylim = range(db.sebaste$anneeGestion, na.rm = TRUE) + c(-1, 1),
      xlab = 'Taille (mm)',
      ylab = 'Année'
    )
    largeurClasse <- 10
    for (i.an in sort(unique(db.sebaste$anneeGestion))) {
      temp2 <- subset(db.sebaste, anneeGestion == i.an)
      x <- hist(
        temp2$longueur,
        breaks = seq(-5.5, 550, by = largeurClasse),
        plot = FALSE
      )
      polygon(
        x = x$mids[min(which(x$counts > 0)):max(which(x$counts > 0))],
        y = i.an +
          x$counts[min(which(x$counts > 0)):max(which(x$counts > 0))] /
            max(x$counts) *
            0.9,
        col = rgb(1, 0, 0, alpha = 0.3),
        border = NA
      )
      lines(
        x = x$mids[min(which(x$counts > 0)):max(which(x$counts > 0))],
        y = i.an +
          x$counts[min(which(x$counts > 0)):max(which(x$counts > 0))] /
            max(x$counts) *
            0.9,
        col = 'grey70'
      )
    }
  }
  ##
  ## gadidés
  if (FALSE) {
    #ogac et morue supperposé
    temp <- subset(donnees, espece %in% 'ogac')
    x <- vioplot::vioplot(
      longueur ~ anneeGestion,
      data = temp,
      h = 20,
      ylim = c(0, 1000),
      at = sort(unique(temp$anneeGestion)),
      xlim = c(1995, max(temp$anneeGestion, na.rm = TRUE) + 1),
      col = 7,
      ## main='Morue, Saguenay',
      xlab = 'Année',
      ylab = 'Longueur (mm)'
    )
    axis(
      3,
      at = sort(unique(temp$anneeGestion)),
      labels = table(temp$anneeGestion),
      cex.axis = 0.4,
      tick = FALSE,
      line = -0.8,
      col.axis = 7
    )
    ## ajouter morueSp
    temp <- subset(donnees, espece == 'morueSp')
    vioplot::vioplot(
      longueur ~ anneeGestion,
      data = temp,
      h = 20,
      at = sort(unique(temp$anneeGestion)),
      add = TRUE,
      col = 2
    )
    axis(
      3,
      at = sort(unique(temp$anneeGestion)),
      labels = table(temp$anneeGestion),
      cex.axis = 0.4,
      tick = FALSE,
      line = -0.8,
      col.axis = 2
    )
    ## ajouter franche
    temp <- subset(donnees, espece == 'morue')
    vioplot::vioplot(
      longueur ~ anneeGestion,
      data = temp,
      h = 20,
      at = sort(unique(temp$anneeGestion)) + 0.2,
      add = TRUE,
      col = 4
    )
    axis(
      3,
      at = sort(unique(temp$anneeGestion)),
      labels = table(temp$anneeGestion),
      cex.axis = 0.6,
      tick = FALSE,
      line = 0,
      col.axis = 4
    )
    ##
    x <- 0:13
    y <- 70 * (1 - exp(-0.2 * (x - (0)))) * 10
    ## y <- 132*(1-exp(-0.1*(x-(-0.44))))*10
    ## y <- 101*(1-exp(-0.17*(x-(0))))*10
    lines(x + 1987, y, lwd = 2, lty = 2)
    lines(x + 1994, y, lwd = 2, lty = 2)
    ## lines(x+2001, y)
    lines(x + 2004, y, lwd = 2, lty = 2)
    lines(x + 2011, y, lwd = 2, lty = 2)
    lines(x + 2018, y, lwd = 2, lty = 2)
    legend(
      'bottomleft',
      inset = 0.03,
      legend = c('Morue franche', 'Ogac', 'Gadidé non-spécifié'),
      fill = c(4, 7, 2)
    )
    legend(
      'bottomright',
      inset = 0.03,
      legend = paste(c('Linf', 'K', 't0'), ':', c(70, 0.2, 0))
    )
  }
  ##
  temp <- subset(donnees, espece %in% 'morue')
  x <- vioplot::vioplot(
    longueur ~ anneeGestion,
    data = temp,
    h = 20,
    ylim = c(0, 1200),
    at = sort(unique(temp$anneeGestion)),
    xlim = c(1981, max(temp$anneeGestion, na.rm = TRUE) + 1),
    col = 4,
    lineCol = NA,
    ## main='Morue, Saguenay',
    xlab = xlab1,
    ylab = ylab1,
    xaxt = 'n'
  )
  axis(
    3,
    at = as.numeric(names(table(temp$anneeGestion))),
    labels = table(temp$anneeGestion),
    cex.axis = 0.4,
    tick = FALSE,
    line = -0.8,
    las = 2,
    col.axis = 4
  )
  ## ajouter morueSp
  temp <- subset(donnees, espece == 'morueSp')
  vioplot::vioplot(
    longueur ~ anneeGestion,
    data = temp,
    h = 20,
    at = sort(unique(temp$anneeGestion)),
    add = TRUE,
    col = 2,
    lineCol = NA
  )
  axis(1)
  axis(
    3,
    at = sort(unique(temp$anneeGestion)),
    labels = table(temp$anneeGestion),
    cex.axis = 0.4,
    tick = FALSE,
    line = -0.8,
    las = 2,
    col.axis = 2
  )
  legend(
    'topleft',
    inset = 0.03,
    legend = paste(c('Linf', 'K', 't0'), ':', c(70, 0.2, 0))
  )
  ##
  ##
  x <- 0:13
  y <- 70 * (1 - exp(-0.2 * (x - (0)))) * 10
  ## lines(x+1987, y, lwd=2, lty=2)
  ## lines(x+1994, y, lwd=2, lty=2)
  lines(x + 2004, y, lwd = 2, lty = 2)
  lines(x + 2011, y, lwd = 2, lty = 2)
  lines(x + 2018, y, lwd = 2, lty = 2)
  legend(
    'bottomleft',
    inset = 0.03,
    legend = c(legend.morue, legende.gadide),
    fill = c(4, 2)
  )
  text(
    x = mean(par('usr')[1:2]),
    y = diff(par('usr')[3:4]) * 0.9 + par('usr')[3],
    labels = c('A', 'B', 'C', 'D')[j.text],
    cex = 1.5
  )
  j.text = j.text + 1
  ##
  if (FALSE) {
    ## turbot
    temp <- subset(donnees, espece == 'turbot')
    vioplot::vioplot(
      longueur ~ anneeGestion,
      data = temp,
      at = sort(unique(temp$anneeGestion)),
      main = 'Turbot, Saguenay',
      xlab = 'Année',
      ylab = 'Longueur (mm)',
      ylim = c(0, 700)
    )
    axis(
      3,
      at = sort(unique(temp$anneeGestion)),
      labels = table(temp$anneeGestion),
      cex.axis = 0.8,
      tick = FALSE,
      line = -1,
      las = 2
    )
    abline(h = c(0, 440))
    legend(
      'bottomright',
      inset = 0.03,
      legend = paste(c('Linf', 'K', 't0'), ':', c(53, 0.25, 0.5)),
      bg = 'white'
    )
    x <- 0:18
    y <- 53 * (1 - exp(-0.25 * (x - (-0.5)))) * 10
    lines(x + 1982, y)
    x <- 0:13
    y <- 53 * (1 - exp(-0.25 * (x - (-0.5)))) * 10
    lines(x + 1989, y)
    lines(x + 1995, y)
    lines(x + 1997, y)
    ## lines(x+1999, y)
    lines(x + 2004, y)
    points(2004 + 1:3, c(160, 250, 310))
    ## lines(x+2010, y)
    lines(x + 2012, y)
    points(2012 + 1:3, c(160, 250, 310))
    lines(x + 2018, y)
    points(2018 + 1:3, c(160, 250, 310))
  }
}
