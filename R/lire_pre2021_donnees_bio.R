#' Lit les différents fichiers de données biologiques utilisés jusqu'en 2021 dans l'évaluation de stock,
#' standardise les noms de site et d'espèce, consolide le tout dans un data.frame().
#'
#' Avant 2001, les morues franches et ogac sont confondus.
#' À partir de 2001, les échantillonneurs sont formés à faire la différence.
#' Aucune données en 2021 pour cause de pandémie.
#' Commentaires supplémentaires: - mauvaises dates en 2000
#'                               - les dates sont à valder 2002, 1 janier par défaut
#'                               - différents turbot sans date et site en 2007
#'                               - problèmes de dates en 2009, sort bien avec excel, mais pas ici
#'                               - deux formats de dates à gérer en 2010, 2015, 2016, 2017, 2018
#'                               - certaines date au début 2014 dans les fichiers de 2015 et 2016
#'                               - certaines date en 2108 dans le fichier 2018
#'
#' @param input_dir Chemin vers le dossier où sont les fichiers de données brutes
#'
#' @importFrom readxl read_excel
#' @import lubridate
#'
#' @return le data.frame() consolidé
#'
lire_pre2021_donnees_bio <- function(input_dir) {
  donnees.init <- readxl::read_excel(path = file.path(input_dir, '1995DB.xls'))
  print(1995)
  donnees.init <- as.data.frame(donnees.init)
  names(donnees.init) <- c(
    'annee',
    'noSite',
    'nomSite',
    'date',
    'espece',
    'longueur',
    'poids',
    'echGonade',
    'echEstomac',
    'echantillonneur'
  )
  donnees.init[which(donnees.init$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.init$annee <- 1900 + floor(donnees.init$date / 10000)
  donnees.init$mois <- floor(donnees.init$date / 100) -
    floor(donnees.init$date / 10000) * 100
  donnees.init$jour <- donnees.init$date - floor(donnees.init$date / 100) * 100
  ligneErreur <- which(donnees.init$longueur == '3.05 M')
  donnees.init$longueur <- as.numeric(donnees.init$longueur)
  donnees.init$poids <- as.numeric(donnees.init$poids)
  donnees.init[ligneErreur, 'longueur'] <- 305
  donnees.init[ligneErreur, 'poids'] <- 272000
  donnees.init$anneeGestion <- 1995
  ## 1996
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '1996DB.xls'))
  print(1996)
  names(donnees.temp) <- c(
    'annee',
    'noSite',
    'nomSite',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 1996
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 1997
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '1997DB.xls'))
  print(1997)
  names(donnees.temp) <- c(
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 1997
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 1998
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '1998DB.xls'))
  print(1998)
  names(donnees.temp) <- c(
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'dateAlt',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp$annee <- 1900 + floor(donnees.temp$date / 10000)
  donnees.temp$mois <- floor(donnees.temp$date / 100) -
    floor(donnees.temp$date / 10000) * 100
  donnees.temp$jour <- donnees.temp$date - floor(donnees.temp$date / 100) * 100
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.temp$anneeGestion <- 1998
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 1999
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '1999DB.xls'))
  print(1999)
  names(donnees.temp) <- c(
    'nomSite',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp$annee <- 1900 + floor(donnees.temp$date / 10000)
  donnees.temp$mois <- floor(donnees.temp$date / 100) -
    floor(donnees.temp$date / 10000) * 100
  donnees.temp$jour <- donnees.temp$date - floor(donnees.temp$date / 100) * 100
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.temp$anneeGestion <- 1999
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2000
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2000DB.xls'))
  print(2000)
  names(donnees.temp) <- c(
    'espece',
    'nomSite',
    'date',
    'longueur',
    'poids',
    'espece2'
  )
  donnees.temp$longueur <- donnees.temp$longueur * 10 #de cm à mm
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  temp <- donnees.temp[which(lubridate::year(donnees.temp$date) < 1999), ]$date
  donnees.temp[
    which(lubridate::year(donnees.temp$date) < 1999),
    'date'
  ] <- as.POSIXct(paste(
    lubridate::year(temp) + 100,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2000
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2001
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2001DB.xls'))
  print(2001)
  names(donnees.temp) <- c(
    'nomSite',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  temp <- donnees.temp[which(lubridate::year(donnees.temp$date) < 2000), ]$date
  donnees.temp[
    which(lubridate::year(donnees.temp$date) < 2000),
    'date'
  ] <- as.POSIXct(paste(
    lubridate::year(temp) + 101,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp[is.na(donnees.temp$annee), 'annee'] <- 2001
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2001
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2002
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2002DB.xls'))
  print(2002)
  names(donnees.temp) <- c(
    'nomSite',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids',
    'remarques'
  )
  donnees.temp[which(lubridate::year(donnees.temp$date) > 2002), 'date'] <- NA #quelques données le 13 octobre 2073
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2002
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2003
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2003DB.xls'))
  print(2003)
  names(donnees.temp) <- c(
    'noSite',
    'nomSite',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  temp <- donnees.temp[which(lubridate::year(donnees.temp$date) < 2002), ]$date #
  donnees.temp[
    which(lubridate::year(donnees.temp$date) < 2002),
    'date'
  ] <- as.POSIXct(paste(
    lubridate::year(temp) + 103,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2003
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2004
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2004DB.xls'))
  print(2004)
  names(donnees.temp) <- c(
    'annee',
    'noSite',
    'nomSite',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2004
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2005
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2005DB.xls'))
  print(2005)
  names(donnees.temp) <- c(
    'annee',
    'noSite',
    'nomSite',
    'espece',
    'longueur',
    'poids',
    'echantillonneur',
    'date'
  )
  temp <- donnees.temp[which(lubridate::year(donnees.temp$date) < 2005), ]$date
  donnees.temp[
    which(lubridate::year(donnees.temp$date) < 2005),
    'date'
  ] <- as.POSIXct(paste(
    lubridate::year(temp) + 4,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2005
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2006
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2006DB.xls'))
  print(2006)
  names(donnees.temp) <- c(
    'echantillonneur',
    'date',
    'noSite',
    'nomSite',
    'espece',
    'longueur',
    'poids'
  )
  temp <- donnees.temp[which(lubridate::year(donnees.temp$date) < 2005), ]$date
  donnees.temp[
    which(lubridate::year(donnees.temp$date) < 2005),
    'date'
  ] <- as.POSIXct(paste(
    lubridate::year(temp) + 6,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2006
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2007
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2007DB.xls'))
  print(2007)
  names(donnees.temp) <- c(
    'nomSite',
    'date',
    'echantillonneur',
    'espece',
    'poids',
    'longueur'
  )
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2007
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2008
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2008DB.xls'))
  print(2008)
  names(donnees.temp) <- c(
    'noSite',
    'nomSite',
    'date',
    'echantillonneur',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2008
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2009
  donnees.temp <- readxl::read_excel(
    path = file.path(input_dir, '2009DB.xls'),
    col_types = c('date', rep('guess', 15))
  )
  print(2009)
  names(donnees.temp) <- c(
    'date',
    'echantillonneur',
    'annee',
    'noSite',
    'nomSite',
    'espece',
    'longueur',
    'poids',
    'indiceCondition',
    'inutile1',
    'inutile2',
    'inutile3',
    'inutile4',
    'inutile5',
    'inutile6',
    'inutile7'
  )
  donnees.temp$longueur <- donnees.temp$longueur * 10 #de cm à mm
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2009
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2010
  donnees.temp <- readxl::read_excel(
    path = file.path(input_dir, '2010DB.xls'),
    range = 'A1:F110',
    col_types = c('guess', 'date', rep('guess', 4))
  )
  print(2010)
  names(donnees.temp) <- c(
    'nomSite',
    'date',
    'echantillonneur',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2010
  donnees.temp2 <- readxl::read_excel(
    path = file.path(input_dir, '2010DB.xls'),
    skip = 110,
    col_names = c(
      'nomSite',
      'date',
      'echantillonneur',
      'espece',
      'longueur',
      'poids'
    )
  )
  donnees.temp2$date <- as.POSIXct(paste(
    2010,
    2,
    substr(donnees.temp2$date, 1, 2),
    sep = '-'
  )) #toutes en février 2010
  donnees.temp2$annee <- lubridate::year(donnees.temp2$date)
  donnees.temp2$mois <- lubridate::month(donnees.temp2$date)
  donnees.temp2$jour <- lubridate::day(donnees.temp2$date)
  donnees.temp2$anneeGestion <- 2010
  donnees.init <- merge(
    donnees.init,
    rbind(donnees.temp, donnees.temp2),
    all = TRUE
  )
  ## 2011 à 2013
  for (i.an in 2011:2013) {
    fichier <- paste0(i.an, 'DB.xls')
    donnees.temp <- readxl::read_excel(
      path = file.path(input_dir, fichier),
      col_types = c('guess', 'date', rep('guess', 4))
    )
    print(i.an)
    names(donnees.temp) <- c(
      'nomSite',
      'date',
      'echantillonneur',
      'espece',
      'longueur',
      'poids'
    )
    donnees.temp$annee <- lubridate::year(donnees.temp$date)
    donnees.temp$mois <- lubridate::month(donnees.temp$date)
    donnees.temp$jour <- lubridate::day(donnees.temp$date)
    donnees.temp$anneeGestion <- i.an
    donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  }
  ## 2014
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2014DB.xls'))
  print(2014)
  names(donnees.temp) <- c(
    'nomSite',
    'date',
    'echantillonneur',
    'espece',
    'longueur',
    'poids',
    'remarques'
  )
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2014
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2015
  donnees.temp <- readxl::read_excel(
    path = file.path(input_dir, '2015DB.xlsx'),
    range = 'A2:G393',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  print(2015)
  donnees.temp2 <- readxl::read_excel(
    path = file.path(input_dir, '2015DB.xlsx'),
    range = 'A501:G534',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  donnees.temp <- rbind(donnees.temp, donnees.temp2)
  donnees.temp$date <- as.POSIXct(donnees.temp$date, format = '%d-%m-%Y')
  donnees.temp3 <- readxl::read_excel(
    path = file.path(input_dir, '2015DB.xlsx'),
    range = 'A394:G500',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  donnees.temp4 <- readxl::read_excel(
    path = file.path(input_dir, '2015DB.xlsx'),
    skip = 534,
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  test <- rbind(donnees.temp, donnees.temp3, donnees.temp4)
  temp <- test[which(lubridate::year(test$date) < 2015), ]$date
  test[which(lubridate::year(test$date) < 2015), 'date'] <- as.POSIXct(paste(
    lubridate::year(temp) + 1,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  test$annee <- lubridate::year(test$date)
  test$mois <- lubridate::month(test$date)
  test$jour <- lubridate::day(test$date)
  test$anneeGestion <- 2015
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2016
  donnees.temp <- readxl::read_excel(
    path = file.path(input_dir, '2016DB.xlsx'),
    range = 'A2:G350',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  print(2016)
  donnees.temp$date <- as.POSIXct(donnees.temp$date, format = '%d/%m/%Y')
  donnees.temp2 <- readxl::read_excel(
    path = file.path(input_dir, '2016DB.xlsx'),
    skip = 350,
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  test <- rbind(donnees.temp, donnees.temp2)
  temp <- test[which(lubridate::year(test$date) < 2016), ]$date
  test[which(lubridate::year(test$date) < 2016), 'date'] <- as.POSIXct(paste(
    lubridate::year(temp) + 1,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  test$annee <- lubridate::year(test$date)
  test$mois <- lubridate::month(test$date)
  test$jour <- lubridate::day(test$date)
  test$anneeGestion <- 2016
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2017
  donnees.temp <- readxl::read_excel(
    path = file.path(input_dir, '2017DB.xlsx'),
    range = 'A2:G121',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  print(2017)
  donnees.temp2 <- readxl::read_excel(
    path = file.path(input_dir, '2017DB.xlsx'),
    range = 'A125:G196',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  donnees.temp <- rbind(donnees.temp, donnees.temp2)
  donnees.temp2 <- readxl::read_excel(
    path = file.path(input_dir, '2017DB.xlsx'),
    range = 'A122:G124',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  donnees.temp2$date <- as.POSIXct('2017-02-09', format = '%Y-%m-%d')
  donnees.temp3 <- readxl::read_excel(
    path = file.path(input_dir, '2017DB.xlsx'),
    skip = 196,
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  donnees.temp3$date <- as.POSIXct(donnees.temp3$date, format = '%d/%m/%Y')
  test <- rbind(donnees.temp, donnees.temp2, donnees.temp3)
  test$annee <- lubridate::year(test$date)
  test$mois <- lubridate::month(test$date)
  test$jour <- lubridate::day(test$date)
  test$anneeGestion <- 2017
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2018
  donnees.temp <- readxl::read_excel(
    path = file.path(input_dir, '2018DB.xlsx'),
    range = 'A2:G747',
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  print(2018)
  donnees.temp$date <- as.POSIXct(donnees.temp$date, format = '%d/%m/%Y')
  donnees.temp2 <- readxl::read_excel(
    path = file.path(input_dir, '2018DB.xlsx'),
    skip = 747,
    col_names = c(
      'nomSite',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'remarques'
    )
  )
  test <- rbind(donnees.temp, donnees.temp2)
  temp <- test[which(lubridate::year(test$date) > 2018), ]$date
  test[which(lubridate::year(test$date) > 2018), 'date'] <- as.POSIXct(paste(
    2018,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  temp <- test[which(lubridate::year(test$date) < 2018), ]$date
  test[which(lubridate::year(test$date) < 2018), 'date'] <- as.POSIXct(paste(
    2018,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  test$annee <- lubridate::year(test$date)
  test$mois <- lubridate::month(test$date)
  test$jour <- lubridate::day(test$date)
  test$anneeGestion <- 2018
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2019
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2019DB.xlsx'))
  print(2019)
  names(donnees.temp) <- c(
    'nomSite',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids',
    'remarques'
  )
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2019
  donnees.init <- merge(donnees.init, rbind(donnees.temp), all = TRUE)
  ## 2020
  donnees.temp <- readxl::read_excel(path = file.path(input_dir, '2020DB.xlsx'))
  print(2020)
  names(donnees.temp) <- c(
    'nomSite',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids',
    'remarques',
    'inutile1',
    'inutile2',
    'inutile3',
    'inutile4'
  )
  donnees.temp$annee <- lubridate::year(donnees.temp$date)
  donnees.temp$mois <- lubridate::month(donnees.temp$date)
  donnees.temp$jour <- lubridate::day(donnees.temp$date)
  donnees.temp$anneeGestion <- 2020
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ##
  ##
  ## ajuster les noms utilisés
  donnees.init$date <- as.POSIXct(
    paste(donnees.init$annee, donnees.init$mois, donnees.init$jour, sep = '-'),
    format = '%Y-%m-%d'
  )
  donnees <- donnees.init
  donnees$nomSite <- standardiser_nom_site(donnees$nomSite)[, 'sites']
  donnees$espece <- standardiser_nom_espece(donnees$espece)
  ##
  return(donnees)
}
