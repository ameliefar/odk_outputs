library(ruODK)
library(filesstrings)
library(reshape2)
library(data.table)
library(jsonlite)
library(tidyverse)


#-----------------------------------------------------#
# Récupération du formulaires OF sur le serveur ODK #
#-----------------------------------------------------#
projet <- "CEFE"

formulaire_poussins <- "Hérault - Poussins"


dossier_poussins <- here::here("formulaires", "poussins")

# Vérifier que le dossier existe
if (!dir.exists(dossier_poussins)) {
  stop(paste("Le dossier", dossier_poussins, "n'existe pas."))
}



#source le code des différents scripts R utilisés

source(here::here("Script_ruODK", "constantes.r"))				# Script contenant les variables / constantes
source(here::here("Script_ruODK", "informations_connexion.r"))	# Constantes confidentielles de login à la plateforme
source(here::here("Script_ruODK", "fonctions.r"))				# Script des fonctions utilisées

connexion_odkcentral(serveur = url_odk_central,
                     username = login_odk,
                     password = mot_de_passe)

android <- submission_export(
  local_dir = dossier_poussins,
  overwrite = TRUE,
  media = FALSE,
  repeats = TRUE,
  deleted_fields = FALSE,
  pid = pid_projet(projet),
  fid = fid_formulaire(projet, formulaire_poussins),
  url = "https://odk.gedeop.inrae.fr",
  un = login_odk,
  pw =  mot_de_passe,
  pp = get_default_pp(),
  retries = get_retries(),
  odkc_version = get_default_odkc_version(),
  verbose = get_ru_verbose()
)
t_android <-  dossier_poussins
unzip(android, exdir = t_android)
unlink(android)

  
  #----------------------------------------------------#
  # RECUPERATION FICHIERS DES FORMULAIRES SOUS ANDROID #
  #----------------------------------------------------#
  
  
setwd(dossier_poussins)
filenames <-  gsub("\\.csv$","", list.files(pattern="\\.csv$"))

for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}
csv_android <- poussins_mtp %>% 
  select(6:8, 13, 15, 16, 20, 27) %>% 
  rename_with(~str_remove(.x, "info_gen.")) %>% 
  janitor::clean_names() %>% 
  left_join(`poussins_mtp-poussin` %>% 
              select(6, 15) %>% 
              rename_with(~str_remove(.x, "info_chick.")) %>% 
              janitor::clean_names(),
            by = c("key" = "parent_key"),
            relationship = "one-to-many")

print(paste("Le fichier csv android génère", nrow(csv_android), "lignes"))
#----------------------------------------------#
# Mise en forme des données poussins récoltées #
#----------------------------------------------#

#' Pour la rouvière

pous_rouv <- csv_android %>%
  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA"))) %>% 
  dplyr::filter(lieu %in% c("rou")) %>% 

  dplyr::mutate(date_mesure = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(heure, format = "%H:%M:%OS"), format = "%H:%M"),
                npul = poussin_count,
                obs = stringr::str_trim(stringr::str_to_upper(obs))) %>%
  dplyr::group_by(key) %>%
  dplyr::arrange(bague) %>%
  dplyr::mutate(ordre_passage = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(as.Date(date_mesure, format = "%d/%m/%Y"), obs, suppressWarnings(as.numeric(nic)), ordre_passage) %>%
  dplyr::select(date_mesure, nic, heure, obs, espece, ordre_passage, bague)



#' Pour la ville
pous_ville <- csv_android %>% 
  dplyr::filter(lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "mtmr")) %>% 
  
  dplyr::mutate(date_mesure = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(heure, format = "%H:%M:%OS"), format = "%H:%M"),
                npul = poussin_count,
                obs = stringr::str_trim(stringr::str_to_upper(obs))) %>% 
  dplyr::group_by(key) %>% 
  dplyr::arrange(bague) %>% 
  dplyr::mutate(ordre_passage = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(lieu, suppressWarnings(as.numeric(nic)), as.Date(date_mesure, format = "%d/%m/%Y"), obs, ordre_passage) %>% 
  dplyr::select(date_mesure, lieu, nic, heure, obs, espece, ordre_passage, bague)

#--------------------#
# Sauvegarde des csv #
#--------------------#

write.csv(pous_rouv, paste0(here::here("outputs"), "/pous_rouv.csv"), na = "", row.names = FALSE)
write.csv(pous_ville, paste0(here::here("outputs"), "/pous_ville.csv"), na = "", row.names = FALSE)



# Supprimer TOUT le contenu (fichiers + sous-dossiers)
unlink(dossier_poussins, recursive = TRUE)

# Recréer le dossier vide (optionnel, si tu veux le garder)
dir.create(dossier_poussins)

message(paste("Le dossier", dossier_poussins, "a été complètement vidé."))


