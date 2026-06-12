#' Lire les données issues de l'application Glaces du Fjord.
#'
#' Les données sont produites par Contact Nature et sont obtenues sur demande à ceux-ci
#' Les données sont présentent dans le dossier "input_2021_et_plus_dir", chaque nouvelle années dans un dossier avec comme nom "Saison AAAA"
#'
#' Note à développer: associer à des sites ou non?
#'
#' @param input_2021_et_plus_dossier chemin le dossier contenant le ou les fichiers de données
#'
#' @import lubridate
#'
#' @return le data.frame() consolidé
#'
lire_glaces_du_fjord <- function(
  input_2021_et_plus_dossier
) {
  fichiers <- list.files(
    path = input_2021_et_plus_dossier,
    pattern = "^prises.*\\.csv$",
    full.names = TRUE
  )
  liste_df <- lapply(fichiers, read.csv)
  gdf <- do.call(rbind, liste_df)

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
      "nombre_lignes_dormantes"
    )
  ##

  names(gdf) <- nouveaux.noms

  ##
  gdf$date <- as.Date(gdf$date)
  gdf$dateHeure_debut = as.POSIXct(
    paste(gdf$date, gsub(" h ", ":", gdf$heure_debut)),
    format = "%Y-%m-%d %H:%M"
  )
  gdf$dateHeure_fin = as.POSIXct(
    paste(gdf$date, gsub(" h ", ":", gdf$heure_fin)),
    format = "%Y-%m-%d %H:%M"
  )
  gdf$annee <- lubridate::year(gdf$date)
  gdf$mois <- lubridate::month(gdf$date)
  gdf$jour <- lubridate::day(gdf$date)
  gdf$jour_de_la_semaine <- lubridate::wday(gdf$date) #1=dimanche
  gdf$lundi_au_vendredi <- gdf$jour_de_la_semaine %in% 2:6
  ##
  gdf$taille_po <- gdf$taille
  gdf$taille <- gdf$taille_po * 2.54
  ##
  gdf <- gdf[gdf$espece != 'Éperlan', ]
  ##
  return(gdf)
}
