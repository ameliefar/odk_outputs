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
library(tidyverse)
library(here)


#------------------------------------------------------------------#
# Récupération du formulaires Adultes (Android) sur le serveur ODK #
#------------------------------------------------------------------#
projet <- "CEFE"

formulaire_android <- "Hérault - Adultes"



dossier_android <- here::here("formulaires", "morph_android")

# Vérifier que le dossier existe
if (!dir.exists(dossier_android)) {
  stop(paste("Le dossier", dossier_android, "n'existe pas."))
}


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

morph_rou_android <- read.csv(paste0(dossier_android, "/tmp/Submissions.csv"), sep = "\t", na = "") %>%  
  dplyr::filter(info_nest_lieu == "rou") %>% 
  
  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA")),
                date_mesure = format(as.Date(info_nest_date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(capture_heure, format = "%H:%M:%OS"), format = "%H:%M"),
                obs = str_trim(str_to_upper(id_measure_obs)),
                sexe = dplyr::case_when(info_adult_sex == "1" ~ "M",
                                        info_adult_sex == "2" ~ "F",
                                        TRUE ~ "?"),
                nic = as.numeric(info_nest_nic)) %>% 
  dplyr::arrange(as.Date(date_mesure, format = "%d/%m/%Y"), obs, nic, sexe) %>% 
  dplyr::select(date_mesure, nic, heure, obs, espece = "capture_espece", action = "id_adult_action", bague = "id_adult_bague", sexe,  age = "info_adult_age")


#' Pour la ville
morph_ville_android <- read.csv(paste0(dossier_android, "/tmp/Submissions.csv"), sep = "\t", na = "") %>%  
  dplyr::filter(info_nest_lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "mtmr")) %>%

  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA")),
                date_mesure = format(as.Date(info_nest_date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(capture_heure, format = "%H:%M:%OS"), format = "%H:%M"),
                obs = str_trim(str_to_upper(id_measure_obs)),
                sexe = dplyr::case_when(info_adult_sex == "1" ~ "M",
                                        info_adult_sex == "2" ~ "F",
                                        TRUE ~ "?"),
                nic = as.numeric(info_nest_nic),
                lieu = info_nest_lieu) %>% 
  dplyr::arrange(lieu, nic, as.Date(date_mesure, format = "%d/%m/%Y"), sexe) %>% 
  dplyr::select(date_mesure, lieu, nic, heure, obs, espece = "capture_espece", action = "id_adult_action", bague = "id_adult_bague", sexe,  age = "info_adult_age")



#------------------------------------------------------------------#
# Récupération du formulaires Adultes (Android) sur le serveur ODK #
#------------------------------------------------------------------#

#' projet <- "CEFE"
#' 
#' formulaire_ios <- "Hérault - iOS - Adultes"
#' 
#' 
#' dossier_ios <- here::here("formulaires", "morph_ios")
#' 
#' # Vérifier que le dossier existe
#' if (!dir.exists(dossier_ios)) {
#'   stop(paste("Le dossier", dossier_ios, "n'existe pas."))
#' }
#' 
#' 
#' #' #source le code des différents scripts R utilisés
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
#'                     "ZIP",
#'                     "",
#'                     "CSV",
#'                     dossier_ios,
#'                     FALSE)
#'
#'
#' #'
#' #' 
#' #' #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#' #' Attention pour "chemin_dossier", s'assurer que le dossier est créé et vide (ça évite d'écraser une ancienne version)
#' # chemin_scripts <- source(here::here("Script_ruODK"))
#' # chemin_dossier <- ) #Je dois trouver un moyen de vider le dossier
#' dossier_ios <- here::here("formulaires", "morph_ios")

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

# Supprimer TOUT le contenu (fichiers + sous-dossiers)
unlink(dossier_android, recursive = TRUE)
#unlink(dossier_ios, recursive = TRUE)

# Recréer le dossier vide (optionnel, si tu veux le garder)
dir.create(dossier_android)
message(paste("Le dossier", dossier_android, "a été complètement vidé."))

#dir.create(dossier_ios)
#message(paste("Le dossier", dossier_ios, "a été complètement vidé."))
