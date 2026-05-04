#Si besoin d'installer le package ruODK (enlever les "#'" devant le bout de script ci-dessous avant d'exécuter)

# options(repos = c(
# ropensci = "https://ropensci.r-universe.dev",
# CRAN = "https://cloud.r-project.org"
# ))
# install.packages("ruODK")




library(ruODK)
library(filesstrings)
library(reshape2)
library(data.table)
library(jsonlite)
library(rjson)
library(tidyverse)
library(here)


#------------------------------------------------------------------#
# Récupération du formulaires Adultes (Android) sur le serveur ODK #
#------------------------------------------------------------------#
projet <- "CEFE"

formulaire_android <- "Hérault - Adultes"



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#' Adapter les chemins selon l'ordinateur
#' Attention pour "chemin_dossier", s'assurer que le dossier est créé et vide (ça évite d'écraser une ancienne version)
# chemin_scripts <- source(here::here("Script_ruODK"))
# chemin_dossier <- ) #Je dois trouver un moyen de vider le dossier
dossier_android <- here::here("formulaires", "morph_android")

fichiers_android <- list.files(path = dossier_android, full.names = TRUE)
if (length(fichiers_android) > 0) {
  unlink(fichiers_android)
  message(paste("Tous les fichiers du dossier", dossier_android, "ont été supprimés."))
} else {
  message(paste("Le dossier", dossier_android, "est déjà vide."))
}

# setwd(chemin_scripts)	#positionnement dans le dossier contenant les scripts - Ne pas modifier

#source le code des différents scripts R utilisés
source(here::here("Script_ruODK", "constantes.r"))				# Script contenant les variables / constantes
source(here::here("Script_ruODK", "informations_connexion.r"))	# Constantes confidentielles de login à la plateforme
source(here::here("Script_ruODK", "fonctions.r"))				# Script des fonctions utilisées

connexion_odkcentral(serveur = url_odk_central,
                     username = login_odk,
                     password = mot_de_passe)

#Utilise l'archive zip pour récupérer un fichier CSV fusionné des soumissions

recupere_soumission(nom_du_projet = projet,
                    nom_du_formulaire = formulaire_android,
                    "API",
                    "",
                    "CSV",
                    dossier_android,
                    FALSE)




#---------------------------------#
# Récupération des données morpho #
#---------------------------------#


#'Pour la Rouvière

morph_rou_android <- read.csv(paste0(dossier_android, "/tmp/Submissions.csv"), sep = ";", na = "") %>%  
  dplyr::filter(info_nest_lieu == "rou") %>% ###############CHANGER LA DATE POUR RECOLTER LES DONNEES POUR UNE PERIODE DONNEE
  #'Exemple : les données des OF de Muro en mars 2025 ont été récupérées, on peut indiquer qu'on veut récupérer les données après le 01 avril 2025 ("2025-04-01")
  
  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA")),
                lieu = info_nest_lieu,
                date_mesure = format(as.Date(info_nest_date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(capture_heure, format = "%H:%M:%OS"), format = "%H:%M"),
                espece = capture_espece,
                action = id_adult_action,
                obs = str_trim(str_to_upper(id_measure_obs)),
                sexe = dplyr::case_when(info_adult_sex == "1" ~ "M",
                                        info_adult_sex == "2" ~ "F",
                                        TRUE ~ "?"),
                nic = as.numeric(info_nest_nic),
                bague = id_adult_bague,
                age = info_adult_age) %>% 
  dplyr::arrange(as.Date(date_mesure, format = "%d/%m/%Y"), obs, nic, sexe) %>% 
  dplyr::select(date_mesure, nic, heure, obs, espece, action, bague, sexe,  age)


#' Pour la ville
morph_ville_android <- read.csv(paste0(dossier_android, "/tmp/Submissions.csv"), sep = ";", na = "") %>%  
  dplyr::filter(info_nest_lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "mtmr")) %>% ###############CHANGER LA DATE POUR RECOLTER LES DONNEES POUR UNE PERIODE DONNEE
  #'Exemple : les données des OF de Muro en mars 2025 ont été récupérées, on peut indiquer qu'on veut récupérer les données après le 01 avril 2025 ("2025-04-01")
  
  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA")),
                lieu = info_nest_lieu,
                date_mesure = format(as.Date(info_nest_date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(capture_heure, format = "%H:%M:%OS"), format = "%H:%M"),
                espece = capture_espece,
                action = id_adult_action,
                obs = str_trim(str_to_upper(id_measure_obs)),
                sexe = dplyr::case_when(info_adult_sex == "1" ~ "M",
                                        info_adult_sex == "2" ~ "F",
                                        TRUE ~ "?"),
                nic = as.numeric(info_nest_nic),
                bague = id_adult_bague,
                age = info_adult_age) %>% 
  dplyr::arrange(lieu, nic, as.Date(date_mesure, format = "%d/%m/%Y"), sexe) %>% 
  dplyr::select(date_mesure, lieu, nic, heure, obs, espece, action, bague, sexe,  age)



#------------------------------------------------------------------#
# Récupération du formulaires Adultes (Android) sur le serveur ODK #
#------------------------------------------------------------------#

#' projet <- "CEFE"
#' 
#' formulaire_ios <- "Hérault - iOS - Adultes"
#' 
#' 
#' 
#' #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#' #' Adapter les chemins selon l'ordinateur
#' #' Attention pour "chemin_dossier", s'assurer que le dossier est créé et vide (ça évite d'écraser une ancienne version)
#' # chemin_scripts <- source(here::here("Script_ruODK"))
#' # chemin_dossier <- ) #Je dois trouver un moyen de vider le dossier
#' dossier_ios <- here::here("formulaires", "morph_ios")
#' 
#' fichiers_ios <- list.files(path = dossier_ios, full.names = TRUE)
#' if (length(fichiers_ios) > 0) {
#'   unlink(fichiers_ios)
#'   message(paste("Tous les fichiers du dossier", dossier_ios, "ont été supprimés."))
#' } else {
#'   message(paste("Le dossier", dossier_ios, "est déjà vide."))
#' }
#' 
#' # setwd(chemin_scripts)	#positionnement dans le dossier contenant les scripts - Ne pas modifier
#' 
#' #source le code des différents scripts R utilisés
#' source(here::here("Script_ruODK", "constantes.r"))				# Script contenant les variables / constantes
#' source(here::here("Script_ruODK", "informations_connexion.r"))	# Constantes confidentielles de login à la plateforme
#' source(here::here("Script_ruODK", "fonctions.r"))				# Script des fonctions utilisées
#' 
#' connexion_odkcentral(serveur = url_odk_central,
#'                      username = login_odk,
#'                      password = mot_de_passe)
#' 
#' #Utilise l'archive zip pour récupérer un fichier CSV fusionné des soumissions
#' 
#' recupere_soumission(nom_du_projet = projet,
#'                     nom_du_formulaire = formulaire_ios,
#'                     "API",
#'                     "",
#'                     "CSV",
#'                     dossier_ios,
#'                     FALSE)


#--------------------#
# Sauvegarde des csv #
#--------------------#

write.csv(morph_rou_android, paste0(here::here("outputs"), "/morph_rou.csv"), na = "", row.names = FALSE)
write.csv(morph_ville_android, paste0(here::here("outputs"), "/morph_ville.csv"), na = "", row.names = FALSE)
