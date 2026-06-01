#' Standardise l'orthographe des noms d'espèce
#'
#' Avant 2001, les morues franches et ogac sont confondus.
#' À partir de 2001, les échantillonneurs sont formés à faire la différence.
#'
#' @param espece vector() des espèces à identifier et valider
#'
#' @return un vector() de noms standardisés
#'
#'
standardiser_nom_espece <- function(espece) {
  if (FALSE) {
    table(espece, useNA = 'ifany')
  }
  espece[
    espece %in%
      c(
        'Fl. atlantique',
        'Flétan Atlantique',
        'Flétan Atlantiq',
        'FletanAtl',
        'flétan atlantique'
      )
  ] <- 'fletan'
  espece[
    espece %in%
      c(
        'Flétan Groenlan',
        'turbo',
        'turbot',
        'Turbot',
        'Flétan Gr',
        'Flétan Groenland',
        'flétan du Groenland',
        'Flétan du Groenland',
        'trubot',
        'utbot',
        'flétan Groenland'
      )
  ] <- 'turbot'
  ## table(espece2, useNA='ifany')
  ## espece[espece2 %in% c('franche')] <- 'morue' #invalide en 2000, serait tout des morueSp
  espece[
    espece %in%
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
        'Morue Franche',
        'morue Atlantique'
      )
  ] <- 'morue'
  espece[
    espece %in%
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
      )
  ] <- 'ogac'
  espece[espece %in% c('Sébaste', 'sebaste', 'sébaste')] <- 'sebastes'
  espece[
    espece %in%
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
      )
  ] <- 'anguille'
  espece[espece %in% c('GOB', 'Goberge')] <- 'goberge'
  espece[
    espece %in%
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
      )
  ] <- 'limaces'
  espece[
    espece %in%
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
      )
  ] <- 'lycodes'
  espece[
    espece %in%
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
      )
  ] <- 'merluche'
  espece[
    espece %in%
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
      )
  ] <- 'truite'
  espece[
    espece %in%
      c(
        'PLIE CAN.',
        'Plie',
        'plie canadienne',
        'plie cana',
        'PLIE CANADIENNE',
        'SOLE',
        'plie du Canada'
      )
  ] <- 'plie'
  espece[espece %in% c('POUL', 'Poulamon', 'poulamon')] <- 'poulamon'
  espece[
    espece %in%
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
      )
  ] <- 'raies'
  espece[
    espece %in%
      c(
        'SAI',
        'Saida',
        'Saïda',
        'saida',
        'saïda',
        'Boreogadus saida',
        'Saĩda',
        'Boreogadus said'
      )
  ] <- 'saïda'
  espece[espece %in% c('LOQUETTE')] <- 'loquette'
  espece[espece %in% c('Capelan')] <- 'capelan'
  espece[espece %in% c('Hareng')] <- 'hareng'
  espece[espece %in% c('REQUIN')] <- 'requin'
  espece[espece %in% c('Lycode ou Limace', '/', 'nd', NA)] <- 'inconnu'
  espece[espece %in% c('SAUMONEAU')] <- 'saumon'
  espece[
    espece %in%
      c(
        'toutes',
        'sébaste/morue',
        'sébaste, morue',
        'sébaste, morue, ogac, turbot',
        'sébaste, morue, turbot',
        'Toutes',
        'sébaste-morues',
        'sébaste/morue Atl.',
        'Sébaste - morue',
        'sébaste-morue',
        'Sébaste et Morue',
        'séba/morue/turbot',
        'morues ogac+Atl.',
        'sébaste et morue',
        'sébaste/ogac',
        'flétan morue,',
        'morue, flétan'
      )
  ] <- 'multiple'
  print(sort(table(espece, useNA = 'always'), decreasing = TRUE))
  ##
  ##
  return(espece)
}
