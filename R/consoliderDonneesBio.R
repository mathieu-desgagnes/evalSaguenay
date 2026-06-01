#' Lit et consolide les différents fichiers de données biologiques utilisés jusqu'en 2021 dans l'évaluation de stock,
#' standardise les noms de site et d'espèce, consolide le tout dans un data.frame().
#'
#' Enregistre ce data.frame() dans un fichier .csv.
#' Il n'est par conséquent pas nécessaire de rouler cette fonction plus d'une fois.
#'
#' Avant 2001, les morues franches et ogac sont confondus.
#' À partir de 2001, les échantillonneurs sont formés à faire la différence.
#' Aucune données en 2021 pour cause de pandémie.
#' Commentaires supplémentaires: - mauvaises dates en 2000
#'                               - les dates sont à valder 2002, 1 janvier par défaut
#'                               - différents turbot sans date et site en 2007
#'                               - problèmes de dates en 2009, sort bien avec excel, mais pas ici
#'                               - deux formats de dates à gérer en 2010, 2015, 2016, 2017, 2018
#'                               - certaines date au début 2014 dans les fichiers de 2015 et 2016
#'                               - certaines date en 2108 dans le fichier 2018
#'
#' @param dir_input répertoire où les fichiers originaux sont présent
#' @param dir_output répertoire où le fichier consolidé est enregistré
#'
#' @importFrom readxl readxl::read_excel
#' @importFrom lubridate year month day
#'
#' @return le data.frame() consolidé
#'
consoliderDonneesBio <- function(
  dir_input = file.path(
    '//dcqcimlna01a',
    'Projets',
    'Saguenay',
    'Pêche hivernale',
    'DB'
  ),
  dir_output = file.path(
    'S:',
    'Saguenay',
    'evaluation2023',
    'manipulationDonnees'
  )
) {
  donnees.init <- readxl::read_excel(path = file.path(dir_input, '1995DB.xls'))
  print(1995)
  names(donnees.init) <- c(
    'annee',
    'noSite',
    'site',
    'date',
    'espece',
    'longueur',
    'poids',
    'echGonade',
    'echEstomac',
    'echantillonneur'
  )
  donnees.init[which(donnees.init$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.init$date <- as.POSIXct(paste(
    1900 + floor(donnees.init$date / 10000),
    floor(donnees.init$date / 100) - floor(donnees.init$date / 10000) * 100,
    donnees.init$date - floor(donnees.init$date / 100) * 100,
    sep = '-'
  ))
  ligneErreur <- which(donnees.init$longueur == '3.05 M')
  donnees.init$longueur <- as.numeric(donnees.init$longueur)
  donnees.init$poids <- as.numeric(donnees.init$poids)
  donnees.init[ligneErreur, 'longueur'] <- 305
  donnees.init[ligneErreur, 'poids'] <- 272000
  ## 1996
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '1996DB.xls'))
  print(1996)
  names(donnees.temp) <- c(
    'annee',
    'noSite',
    'site',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 1997
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '1997DB.xls'))
  print(1997)
  names(donnees.temp) <- c(
    'noSite',
    'site',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 1998
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '1998DB.xls'))
  print(1998)
  names(donnees.temp) <- c(
    'noSite',
    'site',
    'echantillonneur',
    'date',
    'dateAlt',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp$date <- as.POSIXct(paste(
    1900 + floor(donnees.temp$date / 10000),
    floor(donnees.temp$date / 100) - floor(donnees.temp$date / 10000) * 100,
    donnees.temp$date - floor(donnees.temp$date / 100) * 100,
    sep = '-'
  ))
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 1999
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '1999DB.xls'))
  print(1999)
  names(donnees.temp) <- c(
    'site',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp$date <- as.POSIXct(paste(
    1900 + floor(donnees.temp$date / 10000),
    floor(donnees.temp$date / 100) - floor(donnees.temp$date / 10000) * 100,
    donnees.temp$date - floor(donnees.temp$date / 100) * 100,
    sep = '-'
  ))
  donnees.temp[which(donnees.temp$espece == 'morue'), 'espece'] <- 'morueSp' #voir "Note identification des morues.txt"
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2000
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2000DB.xls'))
  print(2000)
  names(donnees.temp) <- c(
    'espece',
    'site',
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
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2001
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2001DB.xls'))
  print(2001)
  names(donnees.temp) <- c(
    'site',
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
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2002
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2002DB.xls'))
  print(2002)
  names(donnees.temp) <- c(
    'site',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids',
    'commentaire'
  )
  temp <- donnees.temp[which(lubridate::year(donnees.temp$date) > 2002), ]$date
  donnees.temp[
    which(lubridate::year(donnees.temp$date) > 2002),
    'date'
  ] <- as.POSIXct(paste(2002, 01, 01, sep = '-'))
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2003
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2003DB.xls'))
  print(2003)
  names(donnees.temp) <- c(
    'noSite',
    'site',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  temp <- donnees.temp[which(lubridate::year(donnees.temp$date) < 2002), ]$date
  donnees.temp[
    which(lubridate::year(donnees.temp$date) < 2002),
    'date'
  ] <- as.POSIXct(paste(
    lubridate::year(temp) + 103,
    lubridate::month(temp),
    lubridate::day(temp),
    sep = '-'
  ))
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2004
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2004DB.xls'))
  print(2004)
  names(donnees.temp) <- c(
    'annee',
    'noSite',
    'site',
    'date',
    'espece',
    'longueur',
    'poids'
  )
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2005
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2005DB.xls'))
  print(2005)
  names(donnees.temp) <- c(
    'annee',
    'noSite',
    'site',
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
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2006
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2006DB.xls'))
  print(2006)
  names(donnees.temp) <- c(
    'echantillonneur',
    'date',
    'noSite',
    'site',
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
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2007
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2007DB.xls'))
  print(2007)
  names(donnees.temp) <- c(
    'site',
    'date',
    'echantillonneur',
    'espece',
    'poids',
    'longueur'
  )
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2008
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2008DB.xls'))
  print(2008)
  names(donnees.temp) <- c(
    'noSite',
    'site',
    'date',
    'echantillonneur',
    'espece',
    'longueur',
    'poids'
  )
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2009
  donnees.temp <- readxl::read_excel(
    path = file.path(dir_input, '2009DB.xls'),
    col_types = c('date', rep('guess', 15))
  )
  print(2009)
  names(donnees.temp) <- c(
    'date',
    'echantillonneur',
    'annee',
    'noSite',
    'site',
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
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2010
  donnees.temp <- readxl::read_excel(
    path = file.path(dir_input, '2010DB.xls'),
    range = 'A1:F110',
    col_types = c('guess', 'date', rep('guess', 4))
  )
  print(2010)
  names(donnees.temp) <- c(
    'site',
    'date',
    'echantillonneur',
    'espece',
    'longueur',
    'poids'
  )
  donnees.temp2 <- readxl::read_excel(
    path = file.path(dir_input, '2010DB.xls'),
    skip = 110,
    col_names = c(
      'site',
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
  ))
  donnees.init <- merge(
    donnees.init,
    rbind(donnees.temp, donnees.temp2),
    all = TRUE
  )
  ## 2011 à 2013
  for (i.an in 2011:2013) {
    fichier <- paste0(i.an, 'DB.xls')
    donnees.temp <- readxl::read_excel(
      path = file.path(dir_input, fichier),
      col_types = c('guess', 'date', rep('guess', 4))
    )
    print(i.an)
    names(donnees.temp) <- c(
      'site',
      'date',
      'echantillonneur',
      'espece',
      'longueur',
      'poids'
    )
    donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  }
  ## 2014
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2014DB.xls'))
  print(2014)
  names(donnees.temp) <- c(
    'site',
    'date',
    'echantillonneur',
    'espece',
    'longueur',
    'poids',
    'commentaire'
  )
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ## 2015
  donnees.temp <- readxl::read_excel(
    path = file.path(dir_input, '2015DB.xlsx'),
    range = 'A2:G393',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  print(2015)
  donnees.temp2 <- readxl::read_excel(
    path = file.path(dir_input, '2015DB.xlsx'),
    range = 'A501:G534',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  donnees.temp <- rbind(donnees.temp, donnees.temp2)
  donnees.temp$date <- as.POSIXct(donnees.temp$date, format = '%d-%m-%Y')
  donnees.temp3 <- readxl::read_excel(
    path = file.path(dir_input, '2015DB.xlsx'),
    range = 'A394:G500',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  donnees.temp4 <- readxl::read_excel(
    path = file.path(dir_input, '2015DB.xlsx'),
    skip = 534,
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
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
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2016
  donnees.temp <- readxl::read_excel(
    path = file.path(dir_input, '2016DB.xlsx'),
    range = 'A2:G350',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  print(2016)
  donnees.temp$date <- as.POSIXct(donnees.temp$date, format = '%d/%m/%Y')
  donnees.temp2 <- readxl::read_excel(
    path = file.path(dir_input, '2016DB.xlsx'),
    skip = 350,
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
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
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2017
  donnees.temp <- readxl::read_excel(
    path = file.path(dir_input, '2017DB.xlsx'),
    range = 'A2:G121',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  print(2017)
  donnees.temp2 <- readxl::read_excel(
    path = file.path(dir_input, '2017DB.xlsx'),
    range = 'A125:G196',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  donnees.temp <- rbind(donnees.temp, donnees.temp2)
  donnees.temp2 <- readxl::read_excel(
    path = file.path(dir_input, '2017DB.xlsx'),
    range = 'A122:G124',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  donnees.temp2$date <- as.POSIXct('2017-02-09', format = '%Y-%m-%d')
  donnees.temp3 <- readxl::read_excel(
    path = file.path(dir_input, '2017DB.xlsx'),
    skip = 196,
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  donnees.temp3$date <- as.POSIXct(donnees.temp3$date, format = '%d/%m/%Y')
  test <- rbind(donnees.temp, donnees.temp2, donnees.temp3)
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2018
  donnees.temp <- readxl::read_excel(
    path = file.path(dir_input, '2018DB.xlsx'),
    range = 'A2:G747',
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
    )
  )
  print(2018)
  donnees.temp$date <- as.POSIXct(donnees.temp$date, format = '%d/%m/%Y')
  donnees.temp2 <- readxl::read_excel(
    path = file.path(dir_input, '2018DB.xlsx'),
    skip = 747,
    col_names = c(
      'site',
      'echantillonneur',
      'date',
      'espece',
      'longueur',
      'poids',
      'commentaire'
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
  donnees.init <- merge(donnees.init, test, all = TRUE)
  ## 2019
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2019DB.xlsx'))
  print(2019)
  names(donnees.temp) <- c(
    'site',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids',
    'commentaire'
  )
  donnees.init <- merge(donnees.init, rbind(donnees.temp), all = TRUE)
  ## 2020
  donnees.temp <- readxl::read_excel(path = file.path(dir_input, '2020DB.xlsx'))
  print(2020)
  names(donnees.temp) <- c(
    'site',
    'echantillonneur',
    'date',
    'espece',
    'longueur',
    'poids',
    'commentaire',
    'inutile1',
    'inutile2',
    'inutile3',
    'inutile4'
  )
  donnees.init <- merge(donnees.init, donnees.temp, all = TRUE)
  ##
  ##
  donnees <- donnees.init
  ## validation des entrées
  if (FALSE) {
    table(donnees$site, useNA = 'ifany')
  }
  donnees[
    donnees$site %in%
      c(
        'AnsBen',
        'Anse-à-ben.',
        'Anse-à-Benjamin',
        'Anse-À-Benjamin',
        'Anse-à-Philippe',
        'Anse à Benjamin',
        'Anse à Philippe',
        'AnseBenjamin',
        'AnsePhilippe',
        'Anse à Philippe et à Benjamin'
      ),
    'site'
  ] <- 'AnseBenjamin'
  donnees[
    donnees$site %in%
      c(
        'Anse-St-Jean',
        'Anse Saint-Jean',
        'Anse st-Jean',
        'Anse St-Jean',
        'Anse st-jean repère',
        'AnseStJean',
        'AnsJean',
        'ASJ',
        'Anse-Saint-Jean'
      ),
    'site'
  ] <- 'AnseStJean'
  donnees[
    donnees$site %in%
      c(
        'Batture',
        'Battures',
        'Battures Baie des Ha Ha',
        'battures baie des haha',
        'Battures baie des haha',
        'Battures Baie des haha',
        'Les Battures',
        'LesBatt',
        'LesBattures'
      ),
    'site'
  ] <- 'LesBattures'
  donnees[
    donnees$site %in% c('Grande-Baie', 'Grande Baie', 'GrandeBaie', 'GrBaie'),
    'site'
  ] <- 'GrandeBaie'
  donnees[
    donnees$site %in%
      c(
        'Baie-Éternité',
        'Baie Éternité',
        'BaieEter',
        'BÉ',
        'Riv. Éternité',
        'Riv.Éternité',
        'RivÉter',
        'Rivière-Éternité',
        'Rivière Éternité',
        'RiviereEternite'
      ),
    'site'
  ] <- 'RiviereEternite'
  donnees[
    donnees$site %in%
      c(
        'cap jaseux',
        'Cap Jaseux',
        'St-Fulgence',
        'SteFulg',
        'StFulg',
        'StFulgence',
        'StFulgence (Anse-à-Pelletier)'
      ),
    'site'
  ] <- 'StFulgence'
  donnees[
    donnees$site %in%
      c(
        'anse aux billots',
        'Anse aux billots',
        'St-Félix',
        "St-Félix d'Otis (Anse aux érables)",
        'StFélix'
      ),
    'site'
  ] <- 'StFelix'
  donnees[
    donnees$site %in%
      c(
        'Sainte-Rose-du-Nord',
        'Ste-Rose',
        'SteRos',
        'SteRose',
        'Ste-Rose_Quai'
      ),
    'site'
  ] <- 'SteRose'
  donnees[
    donnees$site %in%
      c('Anse à chaine', 'anse chaine', 'variés', 'Anse à Chaîne'),
    'site'
  ] <- 'Indetermine'
  donnees[donnees$site %in% c('', NA), 'site'] <- 'Indetermine'
  if (FALSE) {
    table(donnees$site)
  }
  ##
  donnees[donnees$site == 'StFulgence', 'noSite'] <- 1
  donnees[donnees$site == 'AnseBenjamin', 'noSite'] <- 2
  donnees[donnees$site == 'GrandeBaie', 'noSite'] <- 3
  donnees[donnees$site == 'LesBattures', 'noSite'] <- 4
  donnees[donnees$site == 'SteRose', 'noSite'] <- 5
  donnees[donnees$site == 'StFelix', 'noSite'] <- 6
  donnees[donnees$site == 'RiviereEternite', 'noSite'] <- 7
  donnees[donnees$site == 'AnseStJean', 'noSite'] <- 8
  donnees[donnees$site == 'Indetermine', 'noSite'] <- 99
  ##
  if (FALSE) {
    table(donnees$espece, useNA = 'ifany')
  }
  donnees[
    donnees$espece %in%
      c(
        'Fl. atlantique',
        'Flétan Atlantique',
        'Flétan Atlantiq',
        'FletanAtl',
        'flétan atlantique'
      ),
    'espece'
  ] <- 'fletan'
  donnees[
    donnees$espece %in%
      c(
        'Flétan Groenlan',
        'turbo',
        'turbot',
        'Turbot',
        'Flétan Gr',
        'Flétan Groenland'
      ),
    'espece'
  ] <- 'turbot'
  ## table(donnees$espece2, useNA='ifany')
  ## donnees[donnees$espece2 %in% c('franche'), 'espece'] <- 'morue' #invalide en 2000, serait tout des morueSp
  donnees[
    donnees$espece %in%
      c(
        'Franche',
        'Morue franche',
        'franche',
        'morue fran.',
        'morue franche',
        'Morue Fra',
        'Morue fra',
        'morue fra',
        'MorueFran',
        'Morue Franche'
      ),
    'espece'
  ] <- 'morue'
  donnees[
    donnees$espece %in%
      c(
        'Morue Ogac',
        'Morue ogac',
        'Ogac',
        'morue ogac',
        'ogac',
        'Morue Oga',
        'Morue oga',
        'Ogac',
        'morue oga',
        'ogac'
      ),
    'espece'
  ] <- 'ogac'
  donnees[
    donnees$espece %in% c('Sébaste', 'sebaste', 'sébaste'),
    'espece'
  ] <- 'sebastes'
  donnees[
    donnees$espece %in%
      c(
        'ANGUILLE',
        'Anguille',
        'Anquille',
        'anguille',
        'Anguille de roc',
        'Anquille de roc',
        'anguille de roc',
        'Anguille de roche',
        'Anquille de roche',
        'anguille de roche'
      ),
    'espece'
  ] <- 'anguille'
  donnees[donnees$espece %in% c('GOB', 'Goberge'), 'espece'] <- 'goberge'
  donnees[
    donnees$espece %in%
      c(
        'Petite limace de mer',
        'LIM',
        'Limace',
        'Limace de',
        'limace de',
        'petite limace de mer',
        'petite li',
        'Limace de mer',
        'limace de mer',
        'petite limace d',
        'Petite limace d',
        'Jello-fish',
        'limace sp',
        'limace de Reinhard'
      ),
    'espece'
  ] <- 'limaces'
  donnees[
    donnees$espece %in%
      c(
        'lycode atlantique',
        'lycode de vahl',
        'LYC',
        'LYCODE',
        'Lycode',
        'Lycode ?',
        'Lycode La',
        'Lycode de',
        'Lycode ou',
        'lycode de',
        'Lycode de Laval',
        'lycode de laval',
        'Lycode Laval',
        'Lycode de laval',
        'lycode',
        'lycode de Laval',
        'Lycode de laval (ou de Vahl)'
      ),
    'espece'
  ] <- 'lycodes'
  donnees[
    donnees$espece %in%
      c(
        'merluche écureuil',
        'merluche blanche',
        'Merluche',
        'merluche',
        'Merluche blanche',
        'Merluche blanch',
        'Merluche écureu',
        'merluche blanch',
        'merluche écureu',
        'Merluche écureuil'
      ),
    'espece'
  ] <- 'merluche'
  donnees[
    donnees$espece %in%
      c(
        'truite arc-en-ciel',
        'Omble de',
        'TRUITE',
        'Truite de',
        'truite',
        'truite ar',
        'Truite de mer',
        'truite de mer',
        'truite arc-en-c',
        'Omble de fontai',
        'Truite',
        'Omble de fontaine'
      ),
    'espece'
  ] <- 'truite'
  donnees[
    donnees$espece %in%
      c(
        'PLIE CAN.',
        'Plie',
        'plie canadienne',
        'plie cana',
        'PLIE CANADIENNE',
        'SOLE'
      ),
    'espece'
  ] <- 'plie'
  donnees[
    donnees$espece %in% c('POUL', 'Poulamon', 'poulamon'),
    'espece'
  ] <- 'poulamon'
  donnees[
    donnees$espece %in%
      c(
        'RAIE',
        'RAIE ÉPIN',
        'Raie',
        'Raie épin',
        'raie épin',
        'Raie épineuse',
        'Raie Épineuse',
        'raie épineuse',
        'raie',
        'raie epineuse'
      ),
    'espece'
  ] <- 'raies'
  donnees[
    donnees$espece %in%
      c(
        'SAI',
        'Saida',
        'Saïda',
        'saida',
        'saïda',
        'Boreogadus saida',
        'Saĩda',
        'Boreogadus said'
      ),
    'espece'
  ] <- 'saïda'
  donnees[donnees$espece %in% c('LOQUETTE'), 'espece'] <- 'loquette'
  donnees[donnees$espece %in% c('Capelan'), 'espece'] <- 'capelan'
  donnees[donnees$espece %in% c('Hareng'), 'espece'] <- 'hareng'
  donnees[donnees$espece %in% c('REQUIN'), 'espece'] <- 'requin'
  donnees[donnees$espece %in% c('Lycode ou Limace'), 'espece'] <- 'inconnu'
  donnees[donnees$espece %in% c('SAUMONEAU'), 'espece'] <- 'saumon'
  if (FALSE) {
    table(donnees$espece, useNA = 'ifany')
  }
  ##
  ##
  ## sauvegarder la base de donnée consolidée
  save(donnees, file = file.path(dir_output, 'donnees_DB_1995-2021.RData'))
  write.csv2(donnees, file = file.path(dir_output, 'donnees_DB_1995-2021.csv'))
  ##
  return(donnees)
}


#' Ajoute les nouvelles données saisies aux données historiques.
#' Enregistre un data.frame() dans un fichier .csv
#'
#' @param dir_input répertoire où le fichier historique consolidé est présent
#' @param dir_inputNew répertoire des nouvelles données
#' @param dir_output répertoire où le fichier consolidé est enregistré
#'
#' @return le data.frame() consolidé
#'
ajoutDonneesBio <- function(
  dir_input = file.path(
    'S:',
    'Saguenay',
    'evaluation2023',
    'manipulationDonnees'
  ),
  dir_inputNew = file.path('S:', 'Saguenay', 'Données'),
  dir_output = file.path(
    'S:',
    'Saguenay',
    'evaluation2023',
    'manipulationDonnees'
  )
) {
  require('readxl') #lire les .xls(x)
  load(file = file.path(dir_input, 'donnees_DB_1995-2021.RData'), verbose = 1)
  donnees.new <- readxl::read_excel(
    path = file.path(dir_inputNew, 'Donnees_Biologiques_2022-2023.xlsx'),
    sheet = 1
  )
  names(donnees.new) <- c(
    'annee',
    'partenaire',
    'site',
    'echantillonneur',
    'annee2',
    'mois',
    'jour',
    'espece',
    'longueur',
    'poids',
    'commentaire'
  )
  lesquels <- which(!is.na(as.numeric(donnees.new$mois)))
  donnees.new[lesquels, 'date'] <- as.POSIXct(paste(
    unlist(donnees.new[lesquels, 'annee']),
    as.numeric(unlist(donnees.new[lesquels, 'mois'])),
    as.numeric(unlist(donnees.new[lesquels, 'jour'])),
    sep = '-'
  ))
  donnees <- merge(donnees, donnees.new, all = TRUE)
  ##
  ##
  ## sauvegarder la base de donnée consolidée
  save(donnees, file = file.path(dir_output, 'donnees_DB_1995-2023.RData'))
  write.csv2(donnees, file = file.path(dir_output, 'donnees_DB_1995-2023.csv'))
  ##
  return(donnees)
}
