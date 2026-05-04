library(ruODK)
library(filesstrings)
library(reshape2)
library(data.table)
library(jsonlite)
library(rjson)
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

# Supprimer TOUT le contenu (fichiers + sous-dossiers)
unlink(dossier_poussins, recursive = TRUE)

# Recréer le dossier vide (optionnel, si tu veux le garder)
dir.create(dossier_poussins)

message(paste("Le dossier", dossier_poussins, "a été complètement vidé."))

#source le code des différents scripts R utilisés

source(here::here("Script_ruODK", "constantes.r"))				# Script contenant les variables / constantes
source(here::here("Script_ruODK", "informations_connexion.r"))	# Constantes confidentielles de login à la plateforme
source(here::here("Script_ruODK", "fonctions.r"))				# Script des fonctions utilisées

connexion_odkcentral(serveur = url_odk_central,
                     username = login_odk,
                     password = mot_de_passe)

#Utilise l'archive zip pour récupérer un ensemble de fichiers CSV des soumissions (uniquement pour les formulaires avec répétition)

recupere_soumission(nom_du_projet = projet,
                    nom_du_formulaire = formulaire_poussins,
                    "ZIP",
                    "",
                    "MULTICSV",
                    dossier_poussins,
                    FALSE)




#----------------------------------------------#
# Mise en forme des données poussins récoltées #
#----------------------------------------------#



pous <- read.csv(paste0(dossier_poussins, "/poussins_mtp.csv"), sep = ";", na = "") %>% 
  dplyr::filter(!is.na(uuid)) %>% 
  # dplyr::mutate(uuid = dplyr::case_when(stringr::str_detect(uuid, "POUS") ~ poussin_count, #bug bizarre avec ce formulaire
  #                                                TRUE ~ uuid)) %>% 
  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA"))) %>% 
  dplyr::select(uuid, date_mesure, lieu, nic, heure, espece, poussin_count, obs) %>% 
  dplyr::right_join(read.csv(paste0(dossier_poussins, "/poussins_mtp-poussin.csv"), sep = ";", na = "") %>% 
                      dplyr::mutate(dplyr::across(where(is.character),
                                                  ~dplyr::na_if(., "NA"))) %>% 
                      dplyr::select(uuid_parent, action, bague, sex, age_plume, tarsed, poids, collect, meas_com, id_img),
                    by = c("uuid" = "uuid_parent"),
                    relationship = "one-to-many")



#' Pour la rouvière

pous_rouv <- pous %>%
  dplyr::filter(lieu %in% c("rou")) %>% ###############CHANGER LA DATE POUR RECUPERER LES DONNEES POUR UNE PERIODE DONNEE

  dplyr::mutate(date_mesure = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(heure, format = "%H:%M:%OS"), format = "%H:%M"),
                npul = poussin_count,
                obs = stringr::str_trim(stringr::str_to_upper(obs))) %>%
  dplyr::group_by(uuid) %>%
  dplyr::arrange(bague) %>%
  dplyr::mutate(ordre_passage = 1:n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(as.Date(date_mesure, format = "%d/%m/%Y"), obs, as.numeric(nic), ordre_passage) %>%
  dplyr::select(date_mesure, nic, heure, obs, espece, ordre_passage, bague)



#' Pour la ville
pous_ville <- pous %>% 
  dplyr::filter(lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "mtmr")) %>% ###############CHANGER LA DATE POUR RECUPERER LES DONNEES POUR UNE PERIODE DONNEE
  
  dplyr::mutate(date_mesure = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(heure, format = "%H:%M:%OS"), format = "%H:%M"),
                npul = poussin_count,
                obs = stringr::str_trim(stringr::str_to_upper(obs))) %>% 
  dplyr::group_by(uuid) %>% 
  dplyr::arrange(bague) %>% 
  dplyr::mutate(ordre_passage = 1:n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(lieu, as.numeric(nic), as.Date(date_mesure, format = "%d/%m/%Y"), obs, ordre_passage) %>% 
  dplyr::select(date_mesure, lieu, nic, heure, obs, espece, ordre_passage, bague)

#--------------------#
# Sauvegarde des csv #
#--------------------#

write.csv(pous_rouv, paste0(here::here("outputs"), "/pous_rouv.csv"), na = "", row.names = FALSE)
write.csv(pous_ville, paste0(here::here("outputs"), "/pous_ville.csv"), na = "", row.names = FALSE)


#'Les arrangements en cas de bug
#'dplyr::mutate(poussin_count = dplyr::case_when(stringr::str_detect(uuid, "uuid") ~ uuid, #bug bizarre avec ce formulaire
#'date_soumission <= "2025-05-01" ~ instanceName, #à cause du changement de place de obs_meas
#'pop == "font" & lieu == "18" ~ instanceName, #à cause d'une version pas à jour
#'TRUE ~ poussin_count)) %>% 
#'  dplyr::select(date_soumission, uuid = "poussin_count", 
#'                date = "end", 
#'                pop = "date",
#'                lieu = "pop",
#'                nic = "lieu",
#'               heure = "nic",
#'               species = "nic_hs_Accuracy",
#'                species_autre = "species",
#'                obs_meas = "species_autre",
#'                nest_com = "obs_meas",
#'                count = "count") 
