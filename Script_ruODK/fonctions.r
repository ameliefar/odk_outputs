### Ce script comporte les fonctions qui ont été développées pour la récupération des données via RUODK

###############################
deplacer_dossier<- function(dossier_source,dossier_cible){
# Cette fonction déplace un dossier vers une autre cible.
# Elle vérifie que la source existe en tant que dossier et que la cible n'existe pas déjà
# puis effectue une copie récursive.
# 
# Auteur : Alain Benard
# Valeur de retour : Booleen qui indique si le traitement a aboutit (TRUE) ou bien si une erreur est survenue (FALSE)
# Paramètres :
#	- dossier_source	: Chemin complet du dossier à déplacer
#	- dossier_cible		: Chemin complet du dossier après déplacement
# Dernière modification 27/10/2022
#		Création fonction
	dossier_source=normalizePath(file.path(path=dossier_source),mustWork=FALSE,winslash = "\\")
	dossier_cible=normalizePath(file.path(path=dossier_cible),mustWork=FALSE,winslash = "\\")
	
	if ( !(dir.exists(dossier_source)) | (dir.exists(dossier_cible)) ) {
		print("La commande deplacer_dossier est en erreur - Dossier source inexistant ou dossier cible déjà présent")
		return(FALSE)
	}
	#Création dossier cible
	dir.create(dossier_cible)
	listing=list.files(path=dossier_source,full.names=TRUE,no..=TRUE)
	for (fic in listing) {
		if (file.info(fic)$isdir) {
			#Appel recursif
			if (
				!deplacer_dossier(
					dossier_source=normalizePath(file.path(path=fic),mustWork=FALSE,winslash = "\\"),
					dossier_cible=normalizePath(file.path(path=dossier_cible,basename(fic)),mustWork=FALSE,winslash = "\\"))
				) {
					return(FALSE)
				}
		} else {
			file.move(fic,dossier_cible)
		}
	}
	#Suppression du dossier
	unlink(dossier_source,recursive=TRUE)
	return(TRUE)
}

###############################
enleve_dernier_caractere<-function(chaine){
#Cette fonction supprime le dernier carcatère de la chaine passée en paramètre
# Auteur : Alain Benard
# Valeur de retour : chaine modifiée
# Paramètres :
#	- chaine				: chaone de laquelle enlever le dernier caractère
	return(substr(chaine,1,nchar(chaine)-1))
}

###############################
connexion_odkcentral<- function(serveur,username,password,timezone=tz_paris){
# Cette fonction effectue le positionnement du contexte de travail avec le serveur ODK central
# 
# Auteur : Alain Benard
# Valeur de retour : aucune
# Paramètres :
#	- url			: url de la plateforme ODK Central sans caractère slash de terminaison
#	- username		: login utilisateur de la plateforme
#	- password		: mot de passe associé au login
#	- timezone		: time/zone à utiliser (valeur par défaut positionnée)
# Dernière modification 11/10/2022
#		Création fonction
	
	#ru_setup(url =paste0(serveur,"/#/"),un=username,pw=password,tz=timezone) #l'ajout du dièse en fin d'url engendre une non récupération des fichiers média
	ru_setup(url =serveur,un=username,pw=password,tz=timezone) 
}

###############################
projet_existe<-function(nom_du_projet){
# Cette fonction retourne vrai ou faux selon que le projet passé en paramètre existe sur la plateforme ODK
# Auteur : Alain Benard
# Valeur de retour : Vrai si le projet existe et faux dans le cas contraire
# Paramètres :
#	- nom_projet	: nom du projet à rechercher

# Dernière modification 11/10/2022
#		Création fonction	
	if (nrow(subset(project_list(),name==nom_du_projet))==1) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

###############################
pid_projet<-function(nom_du_projet){
# Cette fonction retourne l'id du projet passé en paramètre ou 0 si le projet n'est pas trouvé
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : Id du projet ou 0
# Paramètres :
#	- nom_du_projet	: nom du projet à rechercher

# Dernière modification 11/10/2022
#		Création fonction
	if (projet_existe(nom_du_projet)) {
		return (subset(project_list(),name==nom_du_projet)$id)
	} else {
		return(0)
	}
	
}

###############################
formulaire_existe<-function(nom_du_projet,nom_du_formulaire){
# Cette fonction retourne vrai ou faux selon que le formulaire passé en paramètre existe au sein du projet 
# passé lui aussi en paramètre
# Auteur : Alain Benard
# Valeur de retour : Vrai si le formulaire existe et faux dans le cas contraire
# Paramètres :
#	- nom_du_projet		: nom du projet au sein duquel rechercher le formulaire
#	- nom_du_formulaire	: nom du formulaire à rechercher

# Dernière modification 11/10/2022
#		Création fonction
	if (projet_existe(nom_du_projet)) {
		#ru_setup(pid=pid_projet(nom_du_projet))
		if (nrow(subset(form_list(pid=pid_projet(nom_du_projet)),name==nom_du_formulaire))==1) {
			return(TRUE)
		} else {
			print(paste("formulaire inexistant :",nom_du_formulaire))
			return(FALSE)
		}
	} else {
		return(FALSE)
	}
}

###############################
fid_formulaire<-function(nom_du_projet,nom_du_formulaire){
# Cette fonction retourne l'id (fid) du formulaire passé en paramètre ou une chaine vide si le formulaire n'est pas trouvé
# au sein du projet passé lui aussi en paramètre
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : fid du formulaire ou chaine vide
# Paramètres :
#	- nom_du_projet		: nom du projet au sein duquel rechercher le formulaire
#	- nom_du_formulaire	: nom du formulaire à rechercher

# Dernière modification 11/10/2022
#		Création fonction
	if (formulaire_existe(nom_du_projet,nom_du_formulaire)) {
		return (subset(form_list(pid=pid_projet(nom_du_projet)),name==nom_du_formulaire)$fid)
	} else {
		return(0)
	}

}

###############################
last_submission_isna<-function(nom_du_projet,nom_du_formulaire){
# Cette fonction retourne TRUE si la dernière submission vaut NA pour le formulaire (le service ne peut être utilisé)
# au sein du projet passé lui aussi en paramètre et FALSE dans le cas contraire
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : TRUE ou FALSE
# Paramètres :
#	- nom_du_projet		: nom du projet au sein duquel rechercher le formulaire
#	- nom_du_formulaire	: nom du formulaire à rechercher

# Dernière modification 11/10/2022
#		Création fonction
	if (formulaire_existe(nom_du_projet,nom_du_formulaire)) {
		if (is.na(subset(form_list(pid=pid_projet(nom_du_projet)),name==nom_du_formulaire)$last_submission)) {
			return (TRUE)
		} else {
			return (FALSE)
			}
	} else { #Le formulaire n'existe pas
		return (FALSE)
		}
}

###############################
doublon_nom_champ_formulaire<-function(nom_du_projet,nom_du_formulaire){
# Cette fonction retourne TRUE si au moins un champ du formulaire est dupliqué 
# au sein du formaulaire (passé en paramètre) du projet passé lui aussi en paramètre et FALSE dans le cas contraire.
# L'appelant pourra ainsi choisir de ne pas utiliser le nom des champs de formulaire comme entête de colonne s'il y a des 
# doublons et plutôt utiliser le ruodk_name qui contient la structure (groupe) en plus du nom de champ (un même
# champ ne pouvant être dupliqué au sein d'une même structure.
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : TRUE ou FALSE
# Paramètres :
#	- nom_du_projet		: nom du projet au sein duquel rechercher le formulaire
#	- nom_du_formulaire	: nom du formulaire à rechercher

# Dernière modification 11/10/2022
#		Création fonction
	if (formulaire_existe(nom_du_projet,nom_du_formulaire)) {
		if (anyDuplicated(form_schema()$name) != 0) {
			return (TRUE)
		} else {
			return (FALSE)
			}
	} else { #Le formulaire n'existe pas
		return (NA)
		}
}

###############################
initialise_service_recuperation<-function(nom_du_projet,nom_du_formulaire){
# Cette fonction vérifie que le formulaire passé en paramètre existe bien au sein du projet passé lui 
# aussi en paramètre et positionne le service (paramètre svc de ru_setup) de manière à pouvoir ensuite
# utiliser les fonctions de récupération des soumissions de ce formulaire. L'utilisation de la fonction
# last_submission_isna permet de traiter le cas où aucune submission n'est accessible car l'initialisation
# du service dans ce cas ne sert à rien.
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : vrai si l'opération n'a pas rencontré d'obstacle et faux dans le cas contraire
# Paramètres :
#	- nom_du_projet		: nom du projet au sein duquel rechercher le formulaire
#	- nom_du_formulaire	: nom du formulaire à rechercher

# Dernière modification 11/10/2022
#		Création fonction
	
	if (formulaire_existe(nom_du_projet,nom_du_formulaire)) {
		if (last_submission_isna(nom_du_projet,nom_du_formulaire)){
			print("formulaire sans sousmissions")
			return(FALSE)
		} else {
			#La ligne ci-dessous ne fonctionne plus avec ODK en version 1.5.3 et laisse pid avec la valeur 'projects' et fid avec 'forms'
			#ru_setup(svc=paste0(url_odk_central,"/v",version_api,"/projects/",pid_projet(nom_du_projet),"/forms/",fid_formulaire(nom_du_projet,nom_du_formulaire))) 
			ru_setup(pid=pid_projet(nom_du_projet),fid=fid_formulaire(nom_du_projet,nom_du_formulaire), odkc_version= version_odk_central)
			if (doublon_nom_champ_formulaire(nom_du_projet=nom_du_projet,nom_du_formulaire=nom_du_formulaire)){
				print("Le formulaire comporte des champs en doublons et ne peut être traité avec ce code")
				return(FALSE)
			}  else {
				return(TRUE)
			}
		}
	} else {
		return(FALSE)
	}
	
}

###############################
schema_formulaire<-function(mode_extraction,info_geom = NULL){
# Cette fonction récupère le schéma du formulaire défini dans l'environnement (pid et fid positionnés), lui enlève les éléments
# de structure (Section begin group du formulaire) et alimente des nouvelles colonnes représentant 
#	nom_colonne_fichier : le nom de la colonne qui sera disponible dans le fichier recupéré en zip ou via l'api et les commandes odata_submission_get()
#	nom_colonne_df		: le nom de la colonne qui sera disponible dans l'entête du dataframe constuit à partir du fichier
#	niveau				: entier représentant le niveau d'imbrication dans la structure du formulaire, 0 représentant le champ dans aucune structure de répétition
#	nom_fichier 		: nom du fichier dans lequel sera présente la données
#	parent				: chemin du parent (boucle de répétition, / pour la racine ...)
#	fichier_parent		: nom du fichier parent pour faciliter le travail de plusieurs fonctions
# 
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : dataframe du schéma du formulaire modifié tel que décrit ci-dessus
# Paramètres :
#	- mode_extraction	: 2 valeurs possibles (cf constante mode_extraction)
#		- ZIP (utilisation de l'export sous forme d'archve zip : pas de filtre possible)
#		- API (utilisation de fonctions odata_submission_get() avec un filtre possible)
#	- info_geom 		: dataframe à 2 colonnes (nom_champ_geom,renseigne [booleen]) contenant la liste des champs de type geopoint

# Dernière modification 08/11/2022
#	Fusion des fonctions schema_zip et schema_api en une seule avec ajout des 2 colonnes
#	25/10/2022		Création fonction

	nom_formulaire_traite=ru_settings()$fid
	if (mode_extraction==const_mode_zip){
		separateur_groupe="-"
		nom_fichier_principal=paste0(nom_formulaire_traite,".csv")
	}	else {
			separateur_groupe="_"
			nom_fichier_principal=const_nom_fichier_submissions
		}
	#purge des champs de structure et instance_id
	df_schema = (subset(form_schema(),type!=const_type_structure & name!= const_instance_id))
	#Initialisation des colonnes supplémentaires
	df_schema=cbind(df_schema,nom_colonne_fichier="",nom_colonne_df="",niveau=0,nom_fichier="",parent="/",fichier_parent="")
	#Insère une ligne au début pour la racine
	df_schema=rbind("",df_schema)
	df_schema[1,]$path="/"
	df_schema[1,]$niveau=0
	df_schema[1,]$nom_fichier=nom_fichier_principal
	df_schema[1,]$fichier_parent=""
	
	for(i in 2:nrow(df_schema)) {
		#Initialisation avec le parent 'racine'
		niveau_courant=0
		fichier_courant=nom_fichier_principal
		parent_courant="/"
		fichier_parent_courant=""
		ligne = df_schema[i,] #récupération de la ligne suivante du schema

		#cherche parent :
		for (j in (i-1):1){ #Remonte le tableau du schema pour trouver le parent
			#Un parent est celui qui dispose d'un path  qui se trouve au début du path enfant (/arbre est parent de /arbre/taxon)
			#Il faut toutefois se méfier des cas où le nommage peut amener de la confusion par exemple 
			#	/identification/observateurs représente le début de /identification/observateurs_supplementaires mais les 2 champs observateurs et observateurs_supplementaires
			#	sont au même niveau.
			
			if ((startsWith(ligne$path,paste0(df_schema[j,]$path,"/"))) || (j==1)){
				niveau_courant=as.numeric(df_schema[j,]$niveau)
				fichier_courant=c(df_schema[j,]$nom_fichier)
				parent_courant=c(df_schema[j,]$path)
				fichier_parent_courant=c(df_schema[j,]$fichier_parent)
				if (endsWith(parent_courant,"/")) { #Excepté sur la première ligne pour la racine la colonne path ne se termine pas par un "/"
					#et les fonctions utilisées pour manipuler les chaînes ci-dessus doivent décaler d'un caractère supplémentaire
					decalage=1
				} 	else {
						decalage=2
					}
				# colonne fichier = (path - parent) avec subsitution des / par des -
				nom_colonne_fichier_courant=gsub("/",separateur_groupe,substring(ligne$path,nchar(parent_courant)+decalage))
				nom_colonne_df_courant=gsub("/",".",substring(ligne$path,nchar(parent_courant)+decalage))
				break
			}
		}
		if (ligne$type=="repeat"){
			niveau_courant=niveau_courant+1 #Pour une ligne repeat on incrémente de 1 par rapport au parent
			if (mode_extraction==const_mode_zip){
				fichier_courant=paste0(nom_formulaire_traite,"-",ligne$name,".csv")
			}	else {
					fichier_courant=paste0(const_submissions,".",gsub("/",".",substring(ligne$path,2)),".csv")
				}
			nom_colonne_fichier_courant=""
			nom_colonne_df_courant=""
			fichier_parent_courant=c(df_schema[j,]$nom_fichier)
		}
		df_schema[i,]$nom_colonne_fichier=nom_colonne_fichier_courant
		df_schema[i,]$nom_colonne_df = nom_colonne_df_courant
		df_schema[i,]$niveau=niveau_courant
		df_schema[i,]$nom_fichier=fichier_courant
		df_schema[i,]$parent=parent_courant
		df_schema[i,]$fichier_parent=fichier_parent_courant
	}
	#S'il y a des champs de type geopoint le schéma des données récupérées est connu en mode zip (en mode API il dépend aussi du contenu des tables/fichiers)
	if (nrow(subset(df_schema, type==const_type_geopoint))>0){
		if (mode_extraction==const_mode_zip) {
			#Les colonnes de type geopoint sont à remplacer par 4 colonnes portant le nom habituel du champ suffixé par -Latitude , -Longitude, -Altitude et Accuracy
			df_tmp=df_schema[1,] #Initialise la variable temporaire avec la première ligne qui forcément n'est pas de type geopoint (voir ci-dessus)
			for (k in 2:nrow(df_schema)) {
				ligne = df_schema[k,] #récupération de la ligne suivante du schema
				if (ligne$type==const_type_geopoint){
					#On ajoute 4 lignes
					for (suffixe in champ_type_geom_zip) {
						ligne$name=paste0(df_schema[k,]$name,"_",suffixe)
						ligne$type="decimal"
						ligne$ruodk_name=paste0(df_schema[k,]$ruodk_name,"_",suffixe)
						ligne$nom_colonne_fichier=paste0(df_schema[k,]$nom_colonne_fichier,"-",suffixe)
						ligne$nom_colonne_df=paste0(df_schema[k,]$nom_colonne_df,".",suffixe)
						df_tmp=rbind(df_tmp,ligne)
					}
					
				}	else {	#On reprend telle quel la ligne en cours
						df_tmp=rbind(df_tmp,ligne)
					}
			}
		} 	else { #Mode api
				#Les colonnes de type geopoint sont à remplacer par 4 colonnes portant le nom habituel du champ suffixé par _longitude , _latitude, _altitude et _accuracy
				#Seulement si le champ geopoint a été renseigné au moins une fois ce qui est indiqué dans le dataframe info_geom
				df_tmp=df_schema[1,] #Initialise la variable temporaire avec la première ligne qui forcément n'est pas de type geopoint (voir ci-dessus)
				for (k in 2:nrow(df_schema)) {
					ligne = df_schema[k,] #récupération de la ligne suivante du schema
					if (ligne$type==const_type_geopoint){
						#Teste si le champ a été renseigné au moins une fois via le parametre info_geom
						if(subset(info_geom, nom_champ_geom== ligne$ruodk_name)$renseigne == TRUE) {
							#On ajoute 4 lignes
							for (suffixe in champ_type_geom_api) {
								ligne$name=paste0(df_schema[k,]$name,"_",suffixe)
								ligne$type="decimal"
								ligne$ruodk_name=paste0(df_schema[k,]$ruodk_name,"_",suffixe)
								ligne$nom_colonne_fichier=paste0(df_schema[k,]$nom_colonne_fichier,"_",suffixe)
								ligne$nom_colonne_df=paste0(df_schema[k,]$nom_colonne_df,"_",suffixe)
								df_tmp=rbind(df_tmp,ligne)
							}						
						}	else {	#On reprend telle quel la ligne en cours
								df_tmp=rbind(df_tmp,ligne)
							}
						
						
					}	else {	#On reprend telle quel la ligne en cours
							df_tmp=rbind(df_tmp,ligne)
						}
				}
			}
	}

	if (exists("df_tmp")) {
		return (df_tmp)
	}  else {
		return(df_schema)
	}
	
}

##############################
normalise_fichier<-function(df_fichier,dossier_sortie,mode_extraction,mode_full,df_colonnes){
# Cette fonction est appelée par la fonction finalise. Elle a pour objectif d'harmoniser l'ordre et les noms de
# colonne du fichier listé dans le dataframe d'une seule ligne df_fichier (paramètre) présent dans le sous-dossier représenté par const_sous_dossier_tmp en filtrant ce 
# qui doit l'être selon que le mode full est est actif ou pas. Le fichier standardisé sera mis en place dans le dossier de sortie passé en paramètre.
# Le nom des colonnes de formulaire dans le fichier de sortie prendra ses valeurs dans la colonne name du schéma de formulaire. La date de soumission, l'id  de l'élément et
# l'id de l'élément parent sont standardisés. Les autres colonnes sytèmes présentes si le mode_full est actif gardent le même nom que celui fourni par l'API.

# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : TRUE s'il n'y a ps d'erreur et FALSE dans le cas contraire
# Paramètres :
#	- df_nom_fichier : information du fichier à standardiser sous la forme d'un dataframe d'une seule ligne contenant le nom le niveau et le nom du fichier parent
#	- dossier_sortie: chemin qui a été utilisé comme paramètre à l'appel de recupere_soumission qui contiendra 
#	- mode_extraction	: 2 valeurs possibles (cf constante mode_extraction)
#		- ZIP (utilisation de l'export sous forme d'archve zip : pas de filtre possible)
#		- API (utilisation de fonctions odata_submission_get() avec un filtre possible)
#	- mode_full		: booléen qui précise si l'on conserve toutes les colonnes des fichiers récupérés d'ODK Central ou bien
#						si l'on conserve seulement certaines colonnes système et la totalité des colonnes de données.
#	- df_colonnes	: extrait du schéma du formulaire concernant uniquement le fichier à traiter
# Dernière modification 02/12/2022
#		Création fonction

tryCatch(
{	
  #ligne_fin : Lors de la construction de la liste des colonnes à extraire il ne faut pas prendre la  première ligne de df_colonne et prendre en compte qu'il peut n'y avoir qu'une seule 
  # ligne pour le fichier principal (aucun champ en dehors de la boucle principale de saisie)
  ligne_fin = max(2,nrow(df_colonnes))
  
	#Ouverture du fichier (en mode zip le système fournit des fichiers avec un séparateur qui est la virgule tandis qu'en mode API
	#notre code a enregistré les tables avec un séparateur ';'
	if (mode_extraction == const_mode_api){
		separateur=";"
	}  else {
		separateur=","
	}
	df_data = read.csv(normalizePath(file.path(dossier_sortie,const_sous_dossier_tmp,df_fichier$nom_fichier),mustWork=FALSE,winslash = "\\"),sep=separateur,quote=NULL)
	
	#Etablissement de la liste (nom et ordre) des colonnes à conserver
	if (df_fichier$niveau == 0){ #Cas du fichier principal
		if (mode_extraction==const_mode_zip){ #Fichier principal en mode zip
		  if (ligne_fin == 2) {
		    colonne_a_extraire = c(df_obligatoire_zip$colonne_origine) #pas de champs en dehors de df_obligatoire
		  } else {
		    colonne_a_extraire = c(df_obligatoire_zip$colonne_origine,df_colonnes[2:ligne_fin,]$nom_colonne_df) #il ne faut pas prendre la première ligne
		    print(colonne_a_extraire)  
		  }
			
		}  else { #fichier principal en mode API
		  if (ligne_fin == 2) {
		    colonne_a_extraire = c(df_obligatoire_api$colonne_origine) #pas de champs en dehors de df_obligatoire
		  } else {
		    colonne_a_extraire = c(df_obligatoire_api$colonne_origine,df_colonnes[2:ligne_fin,]$ruodk_name) 	#il ne faut pas prendre la première ligne  
		  }
		}
	} else { #Fichier représentant une boucle de répétitions
		if (mode_extraction==const_mode_zip){ #Fichier boucle en mode zip
			colonne_a_extraire = c(df_obligatoire_zip_repeat$colonne_origine,df_colonnes[2:nrow(df_colonnes),]$nom_colonne_df) #il ne faut pas prendre la première ligne
		}  else { #fichier boucle en mode API
			#Le lien avec le fichier parent se fait via une colonne dépendant du nom du fichier parent (passage en minsucule et remplacement des '.' par des '-' 
			#ainsi que remplacement du '.csv' en fin de nom par '_id'
			cle_etrangere=str_to_lower(paste0(gsub('.{4}$','', gsub('[.]',"_",df_fichier$fichier_parent)),"_id"))
			df_obligatoire_api_repeat= data.frame(colonne_origine=c(cle_etrangere,"id"), colonne_sortie=c("uuid_parent","uuid"))
			colonne_a_extraire = c(df_obligatoire_api_repeat$colonne_origine,df_colonnes[2:nrow(df_colonnes),]$nom_colonne_fichier) 
		}
	
	}	

	if (mode_full){ #Analyse la nécessité de récupérer les autres colonnes systèmes que celles obligatoires déjà listées. Ces colonnes seront placées en fin de dataframe
		autres_colonnes_system = colnames(df_data)[!(colnames(df_data) %in% colonne_a_extraire )]
		df_data= subset(df_data,select = c(colonne_a_extraire,autres_colonnes_system))	
	} else { #Mode simplifié
		df_data= subset(df_data,select = colonne_a_extraire)
	}
		
	#Renommage des colonnes - l'extraction ci-dessus garantit que les colonnes systèmes obligatoires sont les premières du dataframe.
	if (mode_extraction==const_mode_zip){
		if (df_fichier$niveau == 0) {	#Fichier principal
			for (col in colnames(df_data)){
				if (col %in% df_obligatoire_zip$colonne_origine) { #traitement des colonnes systèmes obligatoires
					if (exists("entete_sortie")) { #Utile pour la première colonne car entete_sortie n'existe pas à ce moment - En mode zip cette colonne est SubmissionDate
						entete_sortie=c(entete_sortie,subset(df_obligatoire_zip,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
					} else  {
						entete_sortie=c(subset(df_obligatoire_zip,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
					}
				} else { #traitement des colonnes représentant les champs de formulaire.
					if (col %in% df_colonnes$nom_colonne_df) { #Traitement des colonnes du formulaire						
						entete_sortie=c(entete_sortie,subset(df_colonnes,nom_colonne_df==col,select = name)$name)	
						#Traitement particulier pour les colonnes de type media
					} else { #traitement des autres colonnes s'il y a lieu
						entete_sortie=c(entete_sortie,col)
					}
				}
			}
		} else { #Niveau > 0 - Le fichier comporte déjà PARENT_KEY et KEY puis les colonnes formulaires . Le fichier représente une boucle de répétitions.
			for (col in colnames(df_data)){
				if (col %in% df_obligatoire_zip_repeat$colonne_origine) { #traitement des colonnes systèmes obligatoires (PARENT_KEY et KEY
					if (exists("entete_sortie")) { #Utile pour la première colonne car entete_sortie n'existe pas à ce moment 
							entete_sortie=c(entete_sortie,subset(df_obligatoire_zip_repeat,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
						} else  {
							entete_sortie=c(subset(df_obligatoire_zip_repeat,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
						}
					
				} else { #traitement des colonnes représentant les champs de formulaire.
					if (col %in% df_colonnes$nom_colonne_df) { #Traitement des colonnes du formulaire
						entete_sortie=c(entete_sortie,subset(df_colonnes,nom_colonne_df==col,select = name)$name)
						#Traitement particulier pour les colonnes de type media
					} else { #traitement des autres colonnes s'il y a lieu
						entete_sortie=c(entete_sortie,col)
					}
				}
			}
		}
	} else {#pas mode zip 
		if (df_fichier$niveau == 0) {	#Fichier principal
			for (col in colnames(df_data)){
				if (col %in% df_obligatoire_api$colonne_origine) { #traitement des colonnes systèmes obligatoires
					if (exists("entete_sortie")) { #Utile pour la première colonne car entete_sortie n'existe pas à ce moment - 
						entete_sortie=c(entete_sortie,subset(df_obligatoire_api,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
					} else  {
						entete_sortie=c(subset(df_obligatoire_api,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
					}
				} else { #traitement des colonnes représentant les champs de formulaire.
					if (col %in% df_colonnes$ruodk_name) { #Traitement des colonnes du formulaire
						entete_sortie=c(entete_sortie,subset(df_colonnes,ruodk_name==col,select = name)$name)			
						#Traitement particulier pour les colonnes de type media
					} else { #traitement des autres colonnes s'il y a lieu
						entete_sortie=c(entete_sortie,col)
					}
				}
			}
		} else { #Niveau > 0 - Le fichier comporte une colonne id et une autre pour la clé étrangère (lien au fichier parent) avec une syntaxe spécifique. Le fichier représente une boucle de répétitions.
			for (col in colnames(df_data)){
				if (col %in% df_obligatoire_api_repeat$colonne_origine) { #traitement des colonnes systèmes obligatoires (PAREN?T_KEY et KEY
					if (exists("entete_sortie")) { #Utile pour la première colonne car entete_sortie n'existe pas à ce moment 
							entete_sortie=c(entete_sortie,subset(df_obligatoire_api_repeat,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
						} else  {
							entete_sortie=c(subset(df_obligatoire_api_repeat,colonne_origine==col,select = colonne_sortie)$colonne_sortie)
						}
					
				} else { #traitement des colonnes représentant les champs de formulaire. !!! remplacer nom_colnne_df par nom_colonne_fichier ?????
					if (col %in% df_colonnes$nom_colonne_fichier) { #Traitement des colonnes du formulaire
						entete_sortie=c(entete_sortie,subset(df_colonnes,nom_colonne_fichier==col,select = name)$name)
						#Traitement particulier pour les colonnes de type media
					} else { #traitement des autres colonnes s'il y a lieu
						entete_sortie=c(entete_sortie,col)
					}
				}
			}
		
			
		}	
	}

	colnames(df_data)=entete_sortie
	write.table(df_data,normalizePath(file.path(dossier_sortie,df_fichier$nom_fichier),mustWork=FALSE,winslash = "\\"),row.names = FALSE,quote=FALSE,sep=";",na="")
	
	#Suppression fichier traité
	unlink(normalizePath(file.path(dossier_sortie,const_sous_dossier_tmp,df_fichier$nom_fichier)),recursive = FALSE)
	return(TRUE)
},
	warning = function(e){
		print("warning function normalise_fichier")
		print(e)
		return(FALSE)
	},
	error = function(e){
		print("Erreur function normalise_fichier")
		print(e)
		return(FALSE)
	}		
)

}

##############################
valide_colonne<-function(df_fichier,dossier_sortie,df_colonnes,mode_sortie){
# Cette fonction est appelée par la fonctions finalise. Elle a pour objectif d'effectuer des validations sur le contenu du fichier qui a été précedemment 
# normalisé par la fonction normalise_fichier.
# Les contrôles effectués :
#	- pour les champs de type media : existence du fichier (audio, image, vidéo ...) dans le dossier média. Seul le nom du fichier est conservé dans les données,
#	  le chemin complet du dossier étant supprimé.
#	- pour les champs multivalués, une normalisation est réalisée (Si N valeur alors N lignes sauf si mode json : tableau JSON)
# Une version éventuellement modifiée du fichier analysé écrasera l'original en cours de traitement.

# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : TRUE s'il n'y a ps d'erreur et FALSE dans le cas contraire
# Paramètres :
#	- df_nom_fichier : information du fichier à standardiser sous la forme d'un dataframe d'une seule ligne contenant le nom le niveau et le nom du fichier parent
#	- dossier_sortie: chemin qui a été utilisé comme paramètre à l'appel de recupere_soumission qui contiendra les fichiers de données et le dossier media
#	- df_colonnes	: extrait du schéma du formumlaire concernant uniquement le fichier à traiter
#	- mode_sortie :  4 valeurs possibles (cf constante mode_sortie)
#		- CSV		: seule valeur acceptable pour un formulaire simple (sans répétitions) : 1 seul fichier au format
#					  CSV avec éventuellement des répétitions en lignes pour les répétitions.
#		- MULTICSV	: ensemble de fichiers CSV
#		- JSON		: 1 fichier json avec l'arborescence des répétitions reconstruite
#		- SQL		: 1 base de données SQL Light
# Dernière modification 09/12/2022
#		Création fonction

tryCatch(
{
	#Ouverture du fichier
	
	df_data = read.csv(normalizePath(file.path(dossier_sortie,df_fichier$nom_fichier),mustWork=FALSE,winslash = "\\"),sep=";",quote=NULL)
	#print(paste("valide_colonne :",df_fichier$nom_fichier))
	nom_colonne_origine=(colnames(df_data))
	erreur= FALSE
	position_colonne=0
	for (col in nom_colonne_origine){ #Pour chaque colonne du fichier normalisé
		#print(paste("colonne",col))
		position_colonne = position_colonne+1
		#Traitement uniquement sur les colonnes de formulaire => pas de vérification sur les colonnes systèmes
		if (col %in% df_colonnes$name) {
			
			type_colonne=subset(df_colonnes,name==col,select=type)$type

			if (type_colonne == "binary"){#Traitement particulier pour les colonnes de type media => binary
				for(i in 1:nrow(df_data)) { #Vérifie l'existence des fichiers listés dans la colonne entière
					df_media_courant=df_data[i,col]
					if (!(is.na(df_media_courant))) { #La colonne est renseignée (un fichier a normalement été uploadé
						chemin_media=normalizePath(file.path(dossier_sortie,const_dossier_media,basename(df_media_courant)),mustWork=FALSE,winslash = "\\") #nécessaire car le dossier media a été remonté dans dossier_sortie
						#Vérifie si le ficheir existe
						if (file.exists(chemin_media)) {
							df_data[i,col]=basename(df_media_courant) #on ne conserve que le nom du fichier et pas le chemin complet, permettant un déplacement de l'ensemble plus simple
						}  else {
							print(paste("Fichier media non trouve",df_fichier$nom_fichier,chemin_media))
							erreur=TRUE
						}
						
					}
				}
				#ecrase le fichier avec le dataframe modifié
				write.table(df_data,normalizePath(file.path(dossier_sortie,df_fichier$nom_fichier),mustWork=FALSE),row.names = FALSE,quote=FALSE,sep=";")
			} else {
				colonne_mutivaluee=subset(df_colonnes,name==col,select=selectMultiple)$selectMultiple
				if ((!is.na(colonne_mutivaluee)) & (colonne_mutivaluee == TRUE)){#Traitement particulier pour les colonnes multivaluee => selectMultiple = TRUE
					if (mode_sortie==const_mode_json){ #On transforme la colonne multivaluée au format tableau json
						for(i in 1:nrow(df_data)) {
							df_data[i,position_colonne]=tableau_json(df_data[i,position_colonne]," ")							
						}
					}  else {
						#Pour normaliser le fichier (par exemple 2 valeurs dans le champs) il faut construire un dataframe avec le champ id et le passage des 2 valeurs sur 2 lignes puis 
						#ensuite effectuer une jointure entre le dtaframe d'origine et celui fabriqué. Il faudra au passage supprimer la colonne d'origine multivaluée et renommer celle standardisée
						#récupération d'un mini dataset avec l'id et le champ multivalué
						df_temporaire = subset(df_data,select =c("uuid",col))
						#Renomme la col multivaluée pour pouvoir la manipuler avec un nom fixe
						colnames(df_temporaire)=c("uuid","multivalue")
						nb_max_valeur=max(str_count(df_temporaire$multivalue," ")) + 1 #on compte le nombre d'occurences du séparateur (espace)
						#Eclater la colonne multivaluée en plusieurs : uuid;multivaluee;var1;var2 ...
						setDT(df_temporaire)[, paste("var", 1:nb_max_valeur) := tstrsplit(df_temporaire$multivalue, " ")]
						#suppression de la colonne multivaluée (en 2° position)
						df_temporaire<-df_temporaire[,-2 ]
						#transposition en ligne
						liste_colonne <- colnames( 
									 as.data.frame(df_temporaire)[grepl("^var",
									 colnames(df_temporaire))])
						#transpose en supprimant les valeurs NA et la colonne N°2 inutile
						df_temporaire_transpose<- melt(df_temporaire, 
							  id.vars = "uuid",
							  measure.vars = liste_colonne,
							  na.rm=TRUE)[,-2]
						colnames(df_temporaire_transpose)=c("uuid",col)
						df_data=df_data[,-position_colonne]
						#Jointure du dataframe d'origine avec le dataframe de travail transpose et conservation de l'ordre des colonnes original
						df_data= subset(merge(x=df_data,y=df_temporaire_transpose,by="uuid",all.x=TRUE),select = nom_colonne_origine)
					}
					#ecrase le fichier avec le dataframe modifié
					write.table(df_data,normalizePath(file.path(dossier_sortie,df_fichier$nom_fichier),mustWork=FALSE),row.names = FALSE,quote=FALSE,sep=";")
				}
			
			}
		} 
	}
	return(erreur)
},
	warning = function(e){
		print(paste("Warning function valide_colonne. Fichier : ",df_fichier$nom_fichier," - colonne :",col))
		print(e)
		#return(FALSE)
	},
	error = function(e){
		print(paste("Erreur function valide_colonne. Fichier : ",df_fichier$nom_fichier," - colonne :",col))
		print(e)
		return(FALSE)
	}	

)
}

##############################
finalise<-function(mode_extraction,mode_sortie,dossier_sortie,mode_full,schema_formulaire_courant){
# Cette fonction est appelée par recupere_soumission qui a récupéré et décompressé l'archive zip ou bien utilisé l'API pour récupérer fichiers et médias 
# en utilisant le dossier passé ici en paramètre et le sous-dossier de la constante const_sous_dossier_tmp. 
# Son travail consiste :
# 	- Elle récupère le schéma enrichi du formulaire (API + ajout de colonnes utiles par la  fonction schema_formulaire dédiée à cet usage)
# 	- Elle remonte le dossier media du sous-dossier tmp vers le dossier de sorite attendu.
# 	- Elle construit le listing des fichiers concernés par la récupération des soumissions pour ensuite boucler dessus.
# 	- Pour chaque fichier elle ordonnance les fonctions :
#		- de normalisation des colonnes (ordre et nom)
#		- de vérification des médias et normalisation des champs mulitvalués (fonction valide_colonne)
# 
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : TRUE s'il n'y a ps d'erreur et FALSE dans le cas contraire
# Paramètres :
#	- mode_extraction	: 2 valeurs possibles (cf constante mode_extraction)
#		- ZIP (utilisation de l'export sous forme d'archve zip : pas de filtre possible)
#		- API (utilisation de fonctions odata_submission_get() avec un filtre possible)
#	- mode_sortie :  4 valeurs possibles (cf constante mode_sortie)
#		- CSV		: seule valeur acceptable pour un formulaire simple (sans répétitions) : 1 seul fichier au format
#					  CSV avec éventuellement des répétitions en lignes pour les répétitions.
#		- MULTICSV	: ensemble de fichiers CSV
#		- JSON		: 1 fichier json avec l'arborescence des répétitions reconstruite
#		- SQL		: 1 base de données SQL Light
#	- dossier_sortie: chemin qui a été utilisé comme paramètre à l'appel de recupere_soumission qui contiendra 
#	- mode_full		: booléen qui précise si l'on conserve toutes les colonnes des fichiers récupérés d'ODK Central ou bien
#						si l'on conserve seulement certaines colonnes système et la totalité des colonnes de données.
#	- schema_formulaire_courant : schema du formulaire construit par l'appelant via la fonction schema_formulaire

# Dernière modification 25/10/2022
#		Création fonction

tryCatch(
{
	# Désormais passé en paramètre schema_formulaire_courant = schema_formulaire(mode_extraction=mode_extraction) #recuperation des colonnes correspondant aux champs de saisie du formulaire
	chemin_media= normalizePath(file.path(dossier_sortie,const_sous_dossier_tmp,const_dossier_media),mustWork=FALSE,winslash = "\\")

	if (dir.exists(chemin_media)) { #remonte le dossier media depuis le sous-dossier tmp
		if (!deplacer_dossier(chemin_media,normalizePath(file.path(dossier_sortie,const_dossier_media),mustWork=FALSE,winslash = "\\"))) {
			print(paste("Le dossier ",const_dossier_media,"n'a pu être déplacé"))
			return(FALSE)
		}
	}
	#listing des fichiers récupérés d'après le schéma du formulaire
	doublons=which(duplicated(subset(schema_formulaire_courant,select =c("nom_fichier","niveau","fichier_parent")))) #Liste des doublons (sur 3 colonnes)
	listing_fichier=subset(schema_formulaire_courant,select =c("nom_fichier","niveau","fichier_parent"))[-doublons,] #Sans les doublons

	for(i in 1:nrow(listing_fichier)) {
	  
		df_fichier_courant=listing_fichier[i,]
		df_colonnes_formulaire_courant=subset(schema_formulaire_courant,nom_fichier == df_fichier_courant$nom_fichier)
		
		normalise_fichier(df_fichier=df_fichier_courant,
				dossier_sortie=dossier_sortie,
				mode_extraction=mode_extraction,
				mode_full=mode_full,
				df_colonnes=df_colonnes_formulaire_courant)
		valide_colonne(df_fichier=df_fichier_courant, dossier_sortie=dossier_sortie,df_colonnes=df_colonnes_formulaire_courant,mode_sortie=mode_sortie)
							
	}

	#Suppression du dossier temporaire
	unlink(normalizePath(file.path(dossier_sortie,const_sous_dossier_tmp)),recursive = TRUE)
	return(TRUE)
	
},
	warning = function(e){
		print("Warning function finalise.")
		print(e)
		return(TRUE)
		#return(FALSE)
	},
	error = function(e){
		print("Erreur function finalise.")
		print(e)
		return(FALSE)
	}	

)
}

fusionne_csv<-function(mode_extraction,mode_sortie,dossier_sortie,schema_formulaire_courant){
# Cette fonction est appelée par recupere_soumission qui a récupéré, vérifié et normalisé les fichiers csv. 
# Elle ne doit être appelée que lorsqu'il y a des répétitions dans le formulaire, cette vérification étant du ressort de l'appelant.
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : TRUE s'il n'y a ps d'erreur et FALSE dans le cas contraire
# Paramètres :
#	- mode_extraction	: 2 valeurs possibles (cf constante mode_extraction)
#		- ZIP (utilisation de l'export sous forme d'archve zip : pas de filtre possible)
#		- API (utilisation de fonctions odata_submission_get() avec un filtre possible)
#	- mode_sortie :  4 valeurs possibles (cf constante mode_sortie)
#		- CSV		: seule valeur acceptable pour un formulaire simple (sans répétitions) : 1 seul fichier au format
#					  CSV avec éventuellement des répétitions en lignes pour les répétitions.
#		- MULTICSV	: ensemble de fichiers CSV
#		- JSON		: 1 fichier json avec l'arborescence des répétitions reconstruite
#		- SQL		: 1 base de données SQL Light
#	- dossier_sortie: chemin qui a été utilisé comme paramètre à l'appel de recupere_soumission qui contiendra 
#	- schema_formulaire_courant : schema du formulaire construit par l'appelant via la fonction schema_formulaire
# Dernière modification 15/12/2022
#		Création fonction
tryCatch(
{
	# Désormais passé en paramètre schema_formulaire_courant = schema_formulaire(mode_extraction=mode_extraction) #recuperation des colonnes correspondant aux champs de saisie du formulaire
	doublons=which(duplicated(subset(schema_formulaire_courant,select =c("nom_fichier","niveau","fichier_parent")))) #Liste des doublons (sur 3 colonnes)
	listing_fichier=subset(schema_formulaire_courant,select =c("nom_fichier","niveau","fichier_parent"))[-doublons,] #Sans les doublons
	#Ordonne le listing par niveau / fichier_parent, ordre dans lequel seront réalisées les fusions.
	listing_fichier=listing_fichier[order(listing_fichier$niveau,listing_fichier$fichier_parent,decreasing = TRUE), ]
	
	for(i in 1:nrow(listing_fichier)){
		if (as.numeric(listing_fichier[i,]$niveau)>0){
			#chemins des fichiers enfant et parent à compléter avec le chemin du dossier de sortie.
			fichier_enfant= normalizePath(file.path(dossier_sortie,listing_fichier[i,]$nom_fichier),mustWork=TRUE,winslash = "\\")
			fichier_parent= normalizePath(file.path(dossier_sortie,listing_fichier[i,]$fichier_parent),mustWork=TRUE,winslash = "\\")
			
			#Fichier enfant et informations associées
			df_enfant=read.csv(fichier_enfant,sep=";",quote=NULL)

			#Liste des colonnes du fichier enfant sauf les 2 premières
			colonne_data_enfant=colnames(df_enfant)[-1] #On enlève la première
			colonne_data_enfant=colonne_data_enfant[-1]
			
			#renommage des colonnes 1 [uuid_parent] et 2 [uuid] pour ne pas avoir de conflit/confusion entre les 2 dataframes (parent et enfant)
			colnames(df_enfant)[1] <- "uuid_parent_enfant"
			colnames(df_enfant)[2] <- "uuid_enfant"
			
			#Fichier parent
			df_parent=read.csv(fichier_parent,sep=";",quote=NULL)
			colonne_sortie=c(colnames(df_parent),colonne_data_enfant) #Concaténation liste des colonnes parent + sous-ensemble des colonnes du fichier enfant
			
			df_data= subset(merge(x=df_parent,y=df_enfant,by.x="uuid",by.y="uuid_parent_enfant",all.x=TRUE),select = colonne_sortie)
			write.table(df_data,fichier_parent,row.names = FALSE,quote=FALSE,sep=";")
			#write.json(df_data,fichier_parent,row.names = FALSE)
			unlink(fichier_enfant)
		}
	}
	
	#réordonne les colonnes
	liste_colonnes_formulaire_courant=subset(schema_formulaire_courant,(type != const_type_repeat) & (name !=""))$name
	liste_colonnes_sortie=c(df_obligatoire_api$colonne_sortie,liste_colonnes_formulaire_courant)

	fichier_final= normalizePath(file.path(dossier_sortie,listing_fichier[i,]$nom_fichier),mustWork=TRUE,winslash = "\\")
	df_final=read.csv(fichier_final,sep=";",quote=NULL)
	df_final<-subset(df_final,select=liste_colonnes_sortie)

	write.table(df_final,fichier_final,row.names = FALSE,quote=FALSE,sep=";")
	return(TRUE)
	
},
	warning = function(e){
		print(paste("Warning function fusionne_csv. Fichier : ",listing_fichier[i,]$nom_fichier))
		print(e)
		return(TRUE)
		#return(FALSE)
	},
	error = function(e){
		print(paste("Erreur function fusionne_csv. Fichier : ",listing_fichier[i,]$nom_fichier))
		print(e)
		return(FALSE)
	}	

)
}

###############################
recupere_soumission<-function(nom_du_projet,nom_du_formulaire,mode_extraction,filtre_date,mode_sortie,dossier_sortie,mode_full){
# Cette fonction Initialise le service de récupération et s'il n'y a pas de problème rencontré va extraire
# tout ou partie des soumissions pour le formulaire (paramètre) du projet concerné (paramètre) en s'appuyant sur 
# le mode zip ou l'API (selon le paramètre mode_extraction), avec un éventuel filtre sur la date de soumission avec une 
# sortie définie par le paramètre sortie.
# Il est supposé que l'environnement a été positionné (voir fonction connexion_odkcentral ci-dessus)
# Auteur : Alain Benard
# Valeur de retour : vrai si l'opération n'a pas rencontré d'obstacle et faux dans le cas contraire
# Paramètres :
#	- nom_du_projet		: nom du projet au sein duquel rechercher le formulaire
#	- nom_du_formulaire	: nom du formulaire à rechercher
#	- mode_extraction	: 2 valeurs possibles (cf constante mode_extraction)
#		- ZIP (utilisation de l'export sous forme d'archve zip : pas de filtre possible)
#		- API (utilisation de fonctions odata_submission_get() avec un filtre possible)
#	- filtre : valeur du filtre à appliquer sur la date de soumissions
#	- mode_sortie :  4 valeurs possibles (cf constante mode_sortie)
#		- CSV		: seule valeur acceptable pour un formulaire simple (sans répétitions) : 1 seul fichier au format
#					  CSV avec éventuellement des répétitions en lignes pour les répétitions.
#		- MULTICSV	: ensemble de fichiers CSV
#		- JSON		: 1 fichier json avec l'arborescence des répétitions reconstruite
#		- SQL		: 1 base de données SQL Light
#	- dossier_sortie : chemin complet du dossier où seront créés les fichiers de sortie. ce dossier doit exister et être vide.
#	- mode_full		: booléen qui précise si l'on conserve toutes les colonnes des fichiers récupérés d'ODK Central ou bien
#						si l'on conserve seulement certaines colonnes système et la totalité des colonnes de données.

# Dernière modification 18/10/2022
#		Création fonction

	#Vérification des paramètres
	if (!mode_extraction %in% const_mode_extraction){
		print(paste0("mode extraction incorrect : ", mode_extraction))
		return(FALSE)
	}
	if (!mode_sortie %in% const_mode_sortie){
		print(paste0("mode sortie incorrect : ", mode_sortie))
		return(FALSE)
	}
	if ((mode_full==TRUE)& (mode_sortie!=const_mode_multicsv )){ #Le mode full n'est compatible qu'avec le mode de sortie multicsv
		print("Le mode full ne peut être associé qu'au mode de sortie multicsv")
		return(FALSE)
		
	}
	#Existence dossier vide
	if (dir.exists(dossier_sortie)){
		if(length(list.files(dossier_sortie))>0){ #Dossier non vide
			print(paste0("dossier non vide : ", dossier_sortie))
			return(FALSE)
		}
	} else {
		print(paste0("dossier inexistant : ", dossier_sortie))
		return(FALSE)
		}
		
	#Initialise l'extraction
	if (!initialise_service_recuperation(nom_du_projet,nom_du_formulaire)) {
		print(paste0("problème initialisation extraction : ", nom_du_projet," - ",nom_du_formulaire))
		ru_settings() #Affiche les informations en lien avec la plateforme
		return(FALSE)
	} 
	
	listing_tables <- odata_service_get()
	if((nrow(listing_tables)==1)) {
		formulaire_simple=TRUE 
		print("Formulaire simple")
	} else {
		print("Formulaire avec répétitions")
		formulaire_simple=FALSE
		}
		
	if (formulaire_simple) {
		if ((mode_sortie!=const_mode_csv) && (mode_sortie!=const_mode_json)) {
			print(paste("Seul les modes",const_mode_csv,"et",const_mode_json,"sont supportés pour un formulaire simple :",mode_sortie))
			return (FALSE)
		}
	}
	
	if (mode_extraction==const_mode_zip){ #Récupération archive zip et décompression
		if (filtre_date!=""){
				print("Filtre date ignoré pour le mode zip.")
			}
		nom_archive=submission_export(local_dir=dossier_sortie)
		#listing_archive=unzip(nom_archive, files = NULL, list = TRUE)
		#décompression puis suppression de l'archive :
		unzip(nom_archive, files = NULL, exdir=normalizePath(file.path(dossier_sortie,const_sous_dossier_tmp),mustWork=FALSE))
		unlink(nom_archive)
		
	 } else { #Mode API
			#Ici prévoir vérification du filtre date
			#Initialisation de la liste des champs de type geopoint
			if (exists("infos_champ_geo")){
				rm(infos_champ_geo)
			}
			for (soumission in listing_tables$name){
				data=odata_submission_get(table=soumission,filter=filtre_date,download=TRUE,
					local_dir=normalizePath(file.path(dossier_sortie,const_sous_dossier_tmp,"media"),mustWork=FALSE)) 
				liste_champ_geom=subset(form_schema(), type==const_type_geopoint) #Liste des champs de type geopoint
				for (colonne in colnames(data)){
					for (champ_geom in liste_champ_geom$ruodk_name) {
						if (colonne == champ_geom) {
							if (class(data[[colonne]]) == "list"){ #Le champ geopoint a été rensiegné au moins une fois
								#Suppression de la colonne de type list - Les 4 colonnes supplémentaires contiennent déjà 
								#l'information (lat,lon,alt et accuracy) et la sauvegarde d'une liste via write.table pose problème ci-après
								data[[colonne]]=NULL
								#mémorisation du constat pour alimenter un dataframe qui sera exploité par la fonction schema_formulaire
								if (exists("infos_champ_geo")) {
									#On ajout une ligne au listing des champs geom
									infos_champ_geo=rbind(infos_champ_geo,data.frame(nom_champ_geom=colonne,renseigne=TRUE))
									
								}	else {
										#On créé le listing
										infos_champ_geo=data.frame(nom_champ_geom=colonne,renseigne=TRUE)
								}
							}  else {#La colonne n'est pas de type list ce qui signifie qu'elle n'a jamais été renseignée et que la table récupérée
									 #ne contiendra pas de colonnes supplémentaires.  Ici aussi mémorisation  pour exploitation via schema_formulaire
									if (exists("infos_champ_geo")) {
										#On ajoute une ligne au listing des champs geom
										infos_champ_geo=rbind(infos_champ_geo,data.frame(nom_champ_geom=colonne,renseigne=FALSE))
										
									}	else {
											#On créé le listing
											infos_champ_geo=data.frame(nom_champ_geom=colonne,renseigne=FALSE)
									}									 
								}
							
						}
					}
				}
				write.table(data,normalizePath(file.path(dossier_sortie,const_sous_dossier_tmp,paste0(soumission,".csv")),mustWork=FALSE),row.names = FALSE,quote=FALSE,sep=";",na="")
			}
		}
		
	#Récupération du schéma du formulaire
	if (exists("infos_champ_geo")) {
		schema_du_formulaire = schema_formulaire(mode_extraction=mode_extraction,info_geom = infos_champ_geo ) 
	}  else {
		schema_du_formulaire = schema_formulaire(mode_extraction=mode_extraction)
	}
	
	if (finalise(mode_extraction=mode_extraction,mode_sortie=mode_sortie,dossier_sortie=dossier_sortie,mode_full=mode_full,schema_formulaire_courant=schema_du_formulaire)	== FALSE){
		print("Erreur pendant la fonction finalise")
		return(FALSE)
	}
	
	#A ce stade les fichiers sont normalisés (nom et ordre des colonnes) dans le dossier représenté par le paramètre dossier_sortie quelque soit le 
	#mode d'extraction utilisé. Des vérifications concernant les fichiers médias ont été réalisées et les champs multivalués ont fait l'objet 
	#de normalisation ou ont été convertis en tableau json.
	#Selon le mode de sortie retenu il faut maintenant actionner des traitements spécifiques

	#Cas de sortie en JSON
	if (mode_sortie==const_mode_json){
		if (mode_extraction == const_mode_zip) { #En mode zip il reste un fichier csv qui porte le nom du formulaire
			fichier_unique=normalizePath(file.path(dossier_sortie,paste0(fid_formulaire(nom_du_projet,nom_du_formulaire),".csv")),mustWork=TRUE,winslash = "\\")
		}  else { #Mode API l'unique fichier porte un nom fixe
			fichier_unique= normalizePath(file.path(dossier_sortie,const_nom_fichier_submissions),mustWork=TRUE,winslash = "\\")
		}	
		if (formulaire_simple) {#Simple conversion de l'unique fichier de sortie en JSON
			transform_file_json(fichier_unique,pretty=TRUE,purge=TRUE)
			print(paste("Traitement achevé - les résultats sont disponibles dans le dosssier",dossier_sortie))
			return(TRUE)
		}  else {
			fusionne_json(mode_extraction=mode_extraction,mode_sortie=mode_sortie,dossier_sortie=dossier_sortie,fichier_principal=basename(fichier_unique),schema_formulaire_courant=schema_du_formulaire)
			print(paste("Traitement achevé - les résultats sont disponibles dans le dosssier",dossier_sortie))
			return(TRUE)
		}
	}
	
	#Cas de sortie en sql lite
	if (mode_sortie==const_mode_sql){
		print("Je dois encore générer une base SQL lite. Traitement non implémentée")
		return(FALSE)
	}
	
	if ((mode_sortie==const_mode_multicsv) | (formulaire_simple))  {
		#Le traitement est achevé à ce stade :
		print(paste("Traitement achevé - les résultats sont disponibles dans le dosssier",dossier_sortie))
		return(TRUE)
	} 
	if (mode_sortie==const_mode_csv){ #Au vu du test précédent on est forcement en présence d'un formulaire avec repetitions => fusion
		fusionne_csv(mode_extraction=mode_extraction,mode_sortie=mode_sortie,dossier_sortie=dossier_sortie,schema_formulaire_courant=schema_du_formulaire)
		print(paste("Traitement achevé - les résultats sont disponibles dans le dosssier",dossier_sortie))
		return(TRUE)
	} 
	
	
}






