#Elements concernant la plateforme. A modifier pour une autre plateforme ou en cas d'évolution
version_api=1
version_odk_central="1.5"	
tz_paris="Europe/Paris"
url_odk_central="https://odk.gedeop.inrae.fr"

#Constantes liées au fonctionnement des scripts R. Généralement la modification relève de la responsabilité
#des développeurs.
const_mode_api="API"
const_mode_zip="ZIP"
const_mode_extraction=c(const_mode_api,const_mode_zip)
const_mode_csv="CSV"
const_mode_multicsv="MULTICSV"
const_mode_json="JSON"
const_mode_sql="SQL"
const_mode_sortie=c(const_mode_csv,const_mode_multicsv,const_mode_json,const_mode_sql)
const_sous_dossier_tmp="tmp"
const_type_structure="structure"
const_type_repeat="repeat"
const_type_geopoint="geopoint"
const_type_colonne_dataframe_list="list"
const_instance_id="instanceID"
const_dossier_media="media"
const_submissions="Submissions"
const_nom_fichier_submissions=paste0(const_submissions,".csv")
const_colonne_odata="odata_context"
const_separateur_tableau_json=","
const_tabulation_json=2

#Libellé des colonnes obligatoirement conservées en mode ZIP et correspondance des noms en sortie (fichier principal de données et fichiers de répétitions)
df_obligatoire_zip=data.frame(colonne_origine=c("SubmissionDate","KEY"), colonne_sortie=c("date_soumission","uuid"))
df_obligatoire_zip_repeat=data.frame(colonne_origine=c("PARENT_KEY","KEY"), colonne_sortie=c("uuid_parent","uuid")) #(fichier secondaire de données : boucle repeat)

#Libellé des colonnes obligatoirement conservées en mode API et correspondance des noms en sortie (fichier principal de données)
df_obligatoire_api=data.frame(colonne_origine=c("system_submission_date","id"), colonne_sortie=c("date_soumission","uuid"))

#Libellé des colonnes générées par le système pour des champs de type pointgeo
champ_type_geom_zip=c("Latitude" , "Longitude", "Altitude", "Accuracy")
champ_type_geom_api=c("longitude" , "latitude", "altitude", "accuracy")