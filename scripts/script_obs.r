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


#-----------------------------------------------------------------#
# Récupération du formulaires Lecture de bague sur le serveur ODK #
#-----------------------------------------------------------------#
projet <- "CEFE"

formulaire_obs <- "Lecture de bague"



dossier_obs <- here::here("formulaires", "lecture_bague")

# Vérifier que le dossier existe
if (!dir.exists(dossier_obs)) {
  stop(paste("Le dossier", dossier_obs, "n'existe pas."))
}


#source le code des différents scripts R utilisés
source(here::here("Script_ruODK", "constantes.r"))				# Script contenant les variables / constantes
source(here::here("Script_ruODK", "informations_connexion.r"))	# Constantes confidentielles de login à la plateforme
source(here::here("Script_ruODK", "fonctions.r"))				# Script des fonctions utilisées

connexion_odkcentral(serveur = url_odk_central,
                     username = login_odk,
                     password = mot_de_passe)

obs <- odata_submission_get(
  table = "Submissions",
  skip = NULL,
  top = NULL,
  count = FALSE,
  wkt = FALSE,
  expand = FALSE,
  filter = NULL,
  parse = TRUE,
  download = FALSE,
  orders = get_default_orders(),
  #local_dir = paste0(dossier_android, "/media/"),
  pid = pid_projet(projet),
  fid = fid_formulaire(projet, formulaire_obs),
  url = "https://odk.gedeop.inrae.fr",
  un = login_odk,
  pw = mot_de_passe,
  odkc_version = get_default_odkc_version(),
  tz = tz_paris,
  retries = get_retries(),
  verbose = get_ru_verbose()
)

obs_short <- obs %>% 
  select(4:6, 12:13, 15:19) %>% 
  rename_with(~str_remove(.x, "general_")) %>% 
  rename_with(~str_remove(.x, "observation_")) %>%
  janitor::clean_names() %>% 
  
  dplyr::mutate(dplyr::across(where(is.character),
                              ~dplyr::na_if(., "NA")),
                date_obs = format(as.Date(date_brute, format = "%Y-%m-%d"), format = "%d/%m/%Y"),
                heure = format(as.POSIXct(heure, format = "%H:%M:%OS"), format = "%H:%M"),
                type_obs = case_when(lecture == "mor" ~ "mort",
                                     lecture %in% c("jum", "nid") ~ "vu",
                                     TRUE ~ "autre"),
                darvic = str_trim(str_to_upper(bague_coul)),
                sex = case_when(sex == "1" ~ "M",
                                sex == "2" ~ "F",
                                sex == "4" ~ "M?",
                                sex == "5" ~ "F?",
                                TRUE ~ NA_character_),
                age = case_when(age == "P" ~ "poussin",
                                TRUE ~ "adulte")) %>% 
  dplyr::arrange(desc(age), type_obs, lieu, as.numeric(nic)) %>% 
  dplyr::select(age, type_obs, date_obs, lieu, nic, bague, darvic, sex)



#-------------------#
# Sauvegarde en csv #
#-------------------#

write.csv(obs_short, paste0(here::here("outputs"), "/observations.csv"), na = "", row.names = FALSE)

# Supprimer TOUT le contenu (fichiers + sous-dossiers)
unlink(dossier_obs, recursive = TRUE)

# Recréer le dossier vide 
dir.create(dossier_obs)
message(paste("Le dossier", dossier_obs, "a été complètement vidé."))

