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


android <- odata_submission_get(
  table = "Submissions",
  skip = NULL,
  top = NULL,
  count = FALSE,
  wkt = FALSE,
  expand = FALSE,
  filter = NULL,
  parse = TRUE,
  download = TRUE,
  orders = get_default_orders(),
  local_dir = paste0(dossier_android, "/media/"),
  pid = pid_projet(projet),
  fid = fid_formulaire(projet, formulaire_android),
  url = "https://odk.gedeop.inrae.fr",
  un = login_odk,
  pw = mot_de_passe,
  odkc_version = get_default_odkc_version(),
  tz = tz_paris,
  retries = get_retries(),
  verbose = get_ru_verbose()
)

morph_android <- android %>% 
  select(5:7, 9, 11, 24, 28, 35:37, 42) %>% 
  rename_with(~str_remove(.x, "info_nest_")) %>% 
  rename_with(~str_remove(.x, "capture_")) %>%
  rename_with(~str_remove(.x, "id_adult_")) %>%
  rename_with(~str_remove(.x, "info_adult_")) %>%
  rename_with(~str_remove(.x, "id_measure_")) %>%
  janitor::clean_names()

#---------------------------------#
# Récupération des données morpho #
#---------------------------------#


#'Pour la Rouvière

morph_rou_android <- morph_android %>%  
  dplyr::filter(lieu == "rou") %>% 
  
  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA")),
                date_mesure = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(heure, format = "%H:%M:%OS"), format = "%H:%M"),
                obs = str_trim(str_to_upper(obs)),
                sexe = dplyr::case_when(sex == "1" ~ "M",
                                        sex == "2" ~ "F",
                                        TRUE ~ "?"),
                bague_couleur = str_trim(str_to_upper(bague_coul))) %>% 
  dplyr::arrange(as.Date(date_mesure, format = "%d/%m/%Y"), obs, as.numeric(nic), sexe) %>% 
  dplyr::select(date_mesure, nic, heure, obs, espece, action, bague, bague_couleur, sexe,  age)


#' Pour la ville
morph_ville_android <- morph_android %>%  
  dplyr::filter(lieu %in% c("bot", "cef", "fac", "font", "gram", "mas", "mos", "zoo", "mtmr")) %>%

  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA")),
                date_mesure = format(as.Date(date_mesure, format = "%d/%m/%Y"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(heure, format = "%H:%M:%OS"), format = "%H:%M"),
                obs = str_trim(str_to_upper(obs)),
                sexe = dplyr::case_when(sex == "1" ~ "M",
                                        sex == "2" ~ "F",
                                        TRUE ~ "?"),
                bague_couleur = str_trim(str_to_upper(bague_coul))) %>% 
  dplyr::arrange(lieu, nic, as.Date(date_mesure, format = "%d/%m/%Y"), sexe) %>% 
  dplyr::select(date_mesure, lieu, nic, heure, obs, espece, action, bague, bague_couleur, sexe,  age)


#--------------------#
# Sauvegarde des csv #
#--------------------#

write.csv(morph_rou_android, paste0(here::here("outputs"), "/morph_rou.csv"), na = "", row.names = FALSE)
write.csv(morph_ville_android, paste0(here::here("outputs"), "/morph_ville.csv"), na = "", row.names = FALSE)

# Supprimer TOUT le contenu (fichiers + sous-dossiers)
unlink(dossier_android, recursive = TRUE)

# Recréer le dossier vide 
dir.create(dossier_android)
message(paste("Le dossier", dossier_android, "a été complètement vidé."))

