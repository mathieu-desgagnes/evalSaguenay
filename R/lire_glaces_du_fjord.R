#' Lire les données issues de l'application Glaces du Fjord.
#'
#' Les données sont produites par Contact Nature et sont obtenues sur demande à ceux-ci
#' Les données sont présentent dans le dossier "input_2021_et_plus_dir", chaque nouvelle années dans un dossier avec comme nom "Saison AAAA"
#'
#' Note à développer: associer à des sites ou non?
#'
#' @param input_2021_et_plus_dir chemin du dossier contenant des dossier "Saison 20xx" avec les nouvelles données
#'
#' @importFrom stringr str_match
#' @import lubridate
#'
#' @return le data.frame() consolidé
#'
lire_glaces_du_fjord <- function(
  input_2021_et_plus_dir
) {
  ##
  gdf.init <- as.data.frame(array(
    NA,
    dim = c(0, 25),
    dimnames = list(
      NULL,
      c(
        "Date",
        "Heure.de.début",
        "Heure.de.fin",
        "Durée..minutes.",
        "Identifiant.de.la.pêche",
        "Identifiant.de.l.utilisateur",
        "Espèce",
        "Quantité",
        "Poids",
        "Taille",
        "Type.d.appât",
        "Utilisation.de.sonar",
        "Nombre.d.hameçons",
        "Latitude",
        "Longitude",
        "Nombre.de.pêcheurs",
        "Nombre.de.lignes.actives",
        "Nombre.de.lignes.dormantes",
        "DateHeure.de.début",
        "DateHeure.de.fin",
        "annee",
        "mois",
        "jour",
        "jourSemaine",
        "lundiAuVendredi"
      )
    )
  ))
  temp <- dir(input_2021_et_plus_dir)
  annee <- substr(temp[which(substr(temp, 1, 7) == 'Saison ')], 8, 11)
  for (i.an in annee) {
    print(i.an)
    fichiers <- list.files(
      path = file.path(input_2021_et_plus_dir, paste0('Saison ', i.an)),
      pattern = '^prises.*\\.csv$',
      full.names = FALSE
    )
    if (length(fichiers) > 0) {
      info <- stringr::str_match(
        fichiers,
        "prises-(\\d{4})-(\\d+)-(\\d+)-(\\d{4})-(\\d+)-(\\d+)\\.csv"
      )
      df <- data.frame(
        fichier = fichiers,
        annee_debut = as.integer(info[, 2]),
        mois_debut = as.integer(info[, 3]),
        jour_debut = as.integer(info[, 4]),
        annee_fin = as.integer(info[, 5]),
        mois_fin = as.integer(info[, 6]),
        jour_fin = as.integer(info[, 7])
      )
      df$date_debut <- as.Date(sprintf(
        "%d-%02d-%02d",
        df$annee_debut,
        df$mois_debut,
        df$jour_debut
      ))
      df$date_fin <- as.Date(sprintf(
        "%d-%02d-%02d",
        df$annee_fin,
        df$mois_fin,
        df$jour_fin
      ))
      ##
      gdf.new <- read.csv(
        file = file.path(
          input_2021_et_plus_dir,
          paste0('Saison ', i.an),
          fichiers[which.max(df$date_fin)]
        ),
        na.strings = ''
      )
      if (nrow(gdf.new) > 0) {
        gdf.init <- merge(gdf.init, gdf.new, all = TRUE)
      }
    }
  }
  nouveaux.noms <-
    c(
      "date",
      "heure_debut",
      "heure_fin",
      "duree_minutes",
      "identifiant_peche",
      "identifiant_utilisateur",
      "espece",
      "quantite",
      "poids",
      "taille",
      "appat",
      "echosondeur",
      "nombre_hamecons",
      "latitude",
      "longitude",
      "nombre_pecheurs",
      "nombre_lignes_actives",
      "nombre_lignes_dormantes",
      "dateHeure_debut",
      "dateHeure_fin",
      "annee",
      "mois",
      "jour",
      "jour_de_la_semaine",
      "lundi_au_vendredi"
    )
  ##
  names(gdf.init) <- nouveaux.noms[match(
    names(gdf.init),
    c(
      "Date",
      "Heure.de.début",
      "Heure.de.fin",
      "Durée..minutes.",
      "Identifiant.de.la.pêche",
      "Identifiant.de.l.utilisateur",
      "Espèce",
      "Quantité",
      "Poids",
      "Taille",
      "Type.d.appât",
      "Utilisation.de.sonar",
      "Nombre.d.hameçons",
      "Latitude",
      "Longitude",
      "Nombre.de.pêcheurs",
      "Nombre.de.lignes.actives",
      "Nombre.de.lignes.dormantes",
      "DateHeure.de.début",
      "DateHeure.de.fin",
      "annee",
      "mois",
      "jour",
      "jourSemaine",
      "lundiAuVendredi"
    )
  )]
  ##
  gdf.init$date <- as.Date(gdf.init$date)
  gdf.init$dateHeure_debut = as.POSIXct(
    paste(gdf.init$date, gsub(" h ", ":", gdf.init$heure_debut)),
    format = "%Y-%m-%d %H:%M"
  )
  gdf.init$dateHeure_fin = as.POSIXct(
    paste(gdf.init$date, gsub(" h ", ":", gdf.init$heure_fin)),
    format = "%Y-%m-%d %H:%M"
  )
  gdf.init$annee <- lubridate::year(gdf.init$date)
  gdf.init$mois <- lubridate::month(gdf.init$date)
  gdf.init$jour <- lubridate::day(gdf.init$date)
  gdf.init$jour_de_la_semaine <- lubridate::wday(gdf.init$date) #1=dimanche
  gdf.init$lundi_au_vendredi <- gdf.init$jour_de_la_semaine %in% 2:6
  ##
  gdf.init$taille_po <- gdf.init$taille
  gdf.init$taille <- gdf.init$taille_po * 2.54
  ##
  gdf <- gdf.init[gdf.init$espece != 'Éperlan', ]
  ##
  return(gdf)
}
