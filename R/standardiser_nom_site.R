#' Standardise l'orthographe des appellations de lieux.
#'
#' StFulgence = noSite(1)
#' AnseBenjamin = noSite(2)
#' GrandeBaie = noSite(3)
#' LesBattures = noSite(4)
#' SteRose = noSite(5)
#' StFelix = noSite(6)
#' RiviereEternite = noSite(7)
#' AnseStJean = noSite(8)
#' Indetermine= noSite(99)
#'
#' @param sites Vector() des sites à identifier et valider
#'
#' @return Une liste de 2 éléments, l'orthographe des noms originaux standardisés ($site.ori) et les noms regroupés en 8 villages de pêche ($site)
#'
standardiser_nom_site <- function(sites) {
  sites.ori <- sites
  if (FALSE) {
    table(sites, useNA = 'ifany')
  }
  ## conserver les noms originaux, mais écrire de manière similaire
  sites.ori[
    sites.ori %in%
      c(
        'AnsBen',
        'Anse-à-ben.',
        'Anse-à-Benjamin',
        'Anse-À-Benjamin',
        'Anse à Benjamin',
        'anaben',
        'Ans-Ben',
        'Ansben',
        'Anse àBenjamin',
        'AnseBenjamin',
        'Aqnse-à-Benjamin',
        'AnseBenjamin',
        'Ans.Benjamin',
        'anse à Benjamin',
        'Anse à Benjamin',
        'Anse è Benjamin'
      )
  ] <- 'AnseBenjamin'
  sites.ori[
    sites.ori %in%
      c(
        'Anse-à-Philippe',
        'Anse à Philippe',
        'AnsePhilippe',
        'Ans.philippe',
        'Ans.Philippe',
        'anse à Philippe',
        'AnsePhil',
        'Anse à philippe',
        'Anse à Philippe',
        'Anse è Philippe',
        'anse à Phlippe',
        'A. à Philippe seulement'
      )
  ] <- 'AnsePhilippe'
  sites.ori[
    sites.ori %in%
      c(
        'Anse à Philippe et Benjamin',
        'Anse à Philippe/Benjamin',
        'Benj/Phil',
        'Benj/Philip',
        'Benj/Philippe',
        'Benjamin et Philippe',
        'Anse à Philippe et à Benjamin'
      )
  ] <- 'AnseBenjaminPhilippe'
  sites.ori[
    sites.ori %in% c('anse à Poulette', 'Anse à Poulette')
  ] <- 'AnsePoulette'
  sites.ori[sites.ori %in% c('Bagotville', 'Bagot')] <- 'Bagotville'
  sites.ori[
    sites.ori %in% c('Anse à Benjamin/écorceur', 'Benja+écorceur', 'Écorceur')
  ] <- 'Ecorceur'
  sites.ori[
    sites.ori %in%
      c(
        'Anse-St-Jean',
        'Anse Saint-Jean',
        'Anse st-Jean',
        'Anse St-Jean',
        'Anse st-jean repère',
        'AnseStJean',
        'AnsJean',
        'ASJ',
        'AnseStJean',
        'ansjean',
        'Anse-Saint-Jean',
        'Anse-St-Jean',
        'Anse St-Jean',
        'Anse-Saint-jean',
        'anse St-Jean',
        'anse st jean',
        'Ans-Jean',
        'Anse-st-jean'
      )
  ] <- 'AnseStJean'
  sites.ori[
    sites.ori %in%
      c(
        'Batture',
        'Battures',
        'Battures Baie des Ha Ha',
        'battures baie des haha',
        'Battures baie des haha',
        'Battûres',
        'lesbatt',
        'Battures Baie des haha',
        'Les Battures',
        'LesBatt',
        'LesBattures'
      )
  ] <- 'LesBattures'
  sites.ori[
    sites.ori %in%
      c(
        'anse aux billots',
        'Anse aux billots',
        'Anse à Billot',
        'Anse à billots',
        'Anse à Billots',
        'anse à billot',
        'Anse aux billots',
        'anse à Billots'
      )
  ] <- 'AnseBillots'
  sites.ori[
    sites.ori %in%
      c(
        'Grande-Baie',
        'Grande Baie',
        'GrandeBaie',
        'GrBaie',
        'Grande baie',
        'Gr-Baie',
        'granbaie',
        'grande Baie'
      )
  ] <- 'GrandeBaie'
  sites.ori[
    sites.ori %in%
      c(
        'Baie-Éternité',
        'Baie Éternité',
        'BaieEter',
        'BÉ',
        'Riv. Éternité',
        'Riv.Éternité',
        'RivÉter',
        'Rivière-Éternité',
        'BaieEternite',
        'Riv-Éter',
        'riveter',
        'Rivière Éternité',
        'R-É',
        'RiviereEternite',
        'rivière éternité',
        'rivière Éternité',
        'baie eternitée'
      )
  ] <- 'RiviereEternite'
  sites.ori[sites.ori %in% c('cap jaseux', 'Cap Jaseux')] <- 'CapJaseux'
  sites.ori[
    sites.ori %in%
      c(
        'St-Fulgence',
        'SteFulg',
        'StFulg',
        'StFulgence',
        'Saint-Fulgence',
        'St-Fulg',
        'stful'
      )
  ] <- 'StFulgence'
  sites.ori[
    sites.ori %in%
      c('StFulgence (Anse-à-Pelletier)', 'anse à Pelletier', 'Anse à Pelletier')
  ] <- 'AnsePelletier'
  sites.ori[
    sites.ori %in%
      c('St-Félix', 'StFélix', "St-Félix D'Otis", "St-Félix-d'Otis", 'stfelix')
  ] <- 'StFelix'
  sites.ori[
    sites.ori %in%
      c(
        "St-Félix d'Otis (Anse aux érables)",
        'Anse aux érables',
        'anse aux érables'
      )
  ] <- 'AnseErables'
  sites.ori[
    sites.ori %in%
      c(
        'Anse à chaine',
        'anse chaine',
        'Anse à Chaîne',
        'anse à chaîne',
        'Anse à chaîne',
        'anse à Chaîne'
      )
  ] <- 'AnseChaine'
  sites.ori[
    sites.ori %in%
      c(
        'Sainte-Rose-du-Nord',
        'Ste-Rose',
        'SteRos',
        'SteRose',
        'Ste-Rose_Quai',
        "Sainte-Rose-du-Nord (Anse d'en bas)",
        'Sainte-Rose du Nord',
        'Ste-Rose-du-Nord',
        'Ste-Rose-Nord',
        'Ste_Rose',
        'Ste-rose du Nord',
        'sterosnor',
        'St-Rose'
      )
  ] <- 'SteRose'
  sites.ori[sites.ori %in% c('', NA, '??', 'nd')] <- 'Indetermine'
  print(table(sites.ori, useNA = 'always'))
  print('----')
  ##
  sites[
    sites.ori %in%
      c(
        'AnseBenjamin',
        'AnsePhilippe',
        'AnsePoulette',
        'Ecorceur',
        'Bagotville',
        'AnseBenjaminPhilippe',
        'à la croix',
        "Quai d'escale",
        'Anse à la Croix'
      )
  ] <- 'AnseBenjamin'
  sites[sites.ori %in% c('AnseStJean')] <- 'AnseStJean'
  sites[sites.ori %in% c('LesBattures', 'AnseBillots')] <- 'LesBattures'
  sites[sites.ori %in% c('GrandeBaie')] <- 'GrandeBaie'
  sites[sites.ori %in% c('RiviereEternite')] <- 'RiviereEternite'
  sites[
    sites.ori %in% c('CapJaseux', 'StFulgence', 'AnsePelletier')
  ] <- 'StFulgence'
  sites[
    sites.ori %in% c('StFelix', 'AnseChaine', 'AnseErables', 'Robe noire')
  ] <- 'StFelix'
  sites[sites.ori %in% c('SteRose', 'SteRose_Quai')] <- 'SteRose'
  sites[
    sites.ori %in%
      c(
        'variés',
        'La Baie',
        'Tableau',
        "Cap au l'est",
        'Indetermine',
        'Anse à platte'
      )
  ] <- 'Indetermine'
  ##
  ##
  return(cbind(sites.ori, sites))
}
