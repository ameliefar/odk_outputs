###############################
extension<-function(nomfichier){
# Cette fonction renvoie une chaine correspondant à l'extension du fichier passé en paramètre (avec le caractère.) 
# ou une chaine vide s'il n'y a pas d'extension
# Auteur : Alain Benard
# Valeur de retour : chaine vide ou extension du fichier (exple ".csv"
# Paramètres :
#	- nomfichier			: nom du fichier à analyser
# Dernière modification 22/12/2022
#		Création fonction
	debut_extension = max(grep(".",strsplit(nomfichier,NULL)[[1]],fixed=TRUE))
	longueur_nom_fichier=nchar(nomfichier)
	return(substr(nomfichier,debut_extension,longueur_nom_fichier))
}
###############################
valeur_json<-function(nomcle,valeur){
# Cette fonction renvoie une chaine correspondant à la chaine json qui assemble la clé et la valeur. Si la valeur est détectée comme tableau [...] alors
# elle ne sera pas entourée de guillemets. Cette fonction serait un bon candidat pour gérer plus de type de données (tableau ou string jusqu'ici)
# Auteur : Alain Benard
# Valeur de retour : chaine json
# Paramètres :
#	- nomcle				: nom de la cle au sens json (élément de structure)
#	- valeur				: valeur à associer à la clé
# Dernière modification 21/12/2022
#		Création fonction
	#Prise en compte de la structure
	retour = paste0('"',nomcle,'":')
	if (is.na(valeur)) { #La valeur est considérée comme une chaine vide
		#retour = paste0(retour,"\"","","\",")
		retour = paste0(retour,'"',"",'",')
	}  else {
		if ((substr(valeur,1,1)=="[") & (substr(valeur,nchar(valeur),nchar(valeur))=="]")) { #Premier caractère '[' et dernier caractère ']' ==> tableau
			retour = paste0(retour,valeur,",")
		}  else {
			retour = paste0(retour,'"',valeur,'",')
		}	
	}
	return(retour)
}

transform_file_json<-function(fichier,pretty = FALSE,purge=FALSE){
# Cette fonction convertit au format json le fichier passé en paramètre.
# L'utilisation de la fonction write_json fournit un document complet au format json (pas de conversion ligne à ligne)
# Auteur : Alain Benard
# Valeur de retour : TRUE ou FALSE selon que tout s'est bien passé ou pas
# Paramètres :
#	- fichier				: Chemin complet du fichier à convertir (fichier tabulaire)
#	- pretty				: booleen indiquant si le fichier de sortie doit bénéficier d'une présentation lisible par un humain
#	 						  sinon le document sera en une seule ligne.
#	- purge					: booleen indiquant si le ficheir d'origine doit être supprimé après la conversion en json
# Dernière modification 22/12/2022
#		Création fonction
	if (file.exists(fichier)) {
		dossier_fichier=dirname(fichier)
		nom_fichier=basename(fichier)
		extension_fichier=extension(nom_fichier)
		fichier_tmp=normalizePath(file.path(dossier_fichier,paste0(substr(nom_fichier,1,nchar(nom_fichier) - nchar(extension_fichier)),".tmp")),mustWork=FALSE)
		fichier_json=normalizePath(file.path(dossier_fichier,paste0(substr(nom_fichier,1,nchar(nom_fichier) - nchar(extension_fichier)),".json")),mustWork=FALSE)
		df_original = read.csv(fichier,sep=";",quote=NULL)
		write_json(df_original,fichier_tmp,pretty=pretty)
		if (purge==TRUE){
			unlink(fichier)
		}
		#Le fichier temporaire issu de write_json comporte encore des soucis de quote que l'on règle en remplaçant :
		#	"[ 		par		[
		#	]"		par 	]
		#	/"		par 	"
		json_bad=readLines(fichier_tmp)
		for (i in 1:length(json_bad)) {
			cat(gsub('\\"','"',gsub(']"',']',gsub('"[','[',json_bad[i],fixed=TRUE),fixed=TRUE),fixed=TRUE),file=fichier_json,sep="\n",append=TRUE)
		}
		unlink(fichier_tmp)
		
		#La manipulation des chaines de caractère laisse toutefois encore des tableaux lorsqu'il y a un seul élément et 
		#le coté pretty n'est pas appliqué pour les éléments répétés même si la chaine JSON reste correcte
		contenu_json=toJSON(fromJSON(file=fichier_json))
		if (pretty==TRUE) {
			write(prettify(contenu_json, indent=const_tabulation_json),fichier_json)
		}  else {
			write(contenu_json,fichier_json)
		}
		
	}  else {
		print(paste("conversion json - fichier",fichier,"inexistant."))
		return(FALSE)
	}
}

###############################
tableau_json<-function(chaine_multi,separateur){
# Cette fonction renvoie une chaine correspondant au tableau json construit à partir de la chaine multivaluée 
# passée en paramètre
# Auteur : Alain Benard
# Valeur de retour : tableau json
# Paramètres :
#	- chaine_multi				: chaine de caractères contenant les valeurs à utiliser
#	- separateur				: séparateur entre les valeurs dans la chaine originale
# Dernière modification 22/12/2022
#		Création fonction
	if ((is.na(chaine_multi)) || (chaine_multi=="")) {
		retour="[]"
	}  else {
		retour="["
		for (val in (unlist(str_split(chaine_multi,pattern=separateur)))) {
			retour = paste0(retour,'"',val,'"',const_separateur_tableau_json)
		}
		retour = paste0(enleve_dernier_caractere(retour),"]")
	}
	return(retour)
}
df2json<-function(df_data,nom_colonne_a_conserver,nom_colonne_json,fusion_ligne){
# Cette fonction prend un dataframe, supprime certaines_colonne, en conserve d'autres et fusionne les restantes en une chaine de type json {"cle":"valeur" ...}
# Auteur : Alain Benard
# Valeur de retour : dataframe modifié
# Paramètres :
#	- df_data				: dataframe complet sur lequel effectuer le traitement. Il doit êtrre trié préalablement sur la colonne clé (nom_colonne_a_conserver)
#	- nom_colonne_a_conserver	: nom de la colonne à conserver (clé)
#	- nom_colonne_json		: nom de la colonne qui contiendra la chaine type json
#	- fusion_ligne			: booleen précisant si les lignes identique au niveau de la clé (nom_colonne_a_conserver) doivent être fusionnées au niveau de la chaine json
# Dernière modification 20/12/2022
#		Création fonction
	
	nom_colonne_origine=colnames(df_data)
	nb_colonnes = length(nom_colonne_origine)+1
	#ajout colonne vide
	df_data$tmpjson=""
	
	for(i in 1:nrow(df_data)) {
		df_data[i,]$tmpjson="{"
		num_col=0 # Initialise le numéro de colonne
		for(col in colnames(df_data)){
			num_col=num_col+1
			if ((col !=nom_colonne_a_conserver ) & (col !="tmpjson" )) {
				#df_data[i,]$tmpjson=paste0(df_data[i,]$tmpjson,"\"",col,"\":","\"",df_data[i,num_col],"\",")
				df_data[i,]$tmpjson=paste0(df_data[i,]$tmpjson,valeur_json(col,df_data[i,num_col]))
			}
		}
		#df_data[i,]$tmpjson=paste0(substr(df_data[i,]$tmpjson,1,nchar(df_data[i,]$tmpjson)-1),"}") #supprime la dernière virgule et femre l'acolade
		df_data[i,]$tmpjson=paste0(enleve_dernier_caractere(df_data[i,]$tmpjson),"}") #supprime la dernière virgule et femre l'acolade
	}
	df_data=subset(df_data,select=c(nom_colonne_a_conserver,"tmpjson"))
	#colnames(df_data)=c(nom_colonne_a_conserver,nom_colonne_json)
	if (fusion_ligne== TRUE){
		#Il est nécessaire que le dataframe soit trié selon la première colonne our que l'algorithme qui suit fonctione
		df_data=df_data[order(df_data[,1]),]
		#Initialise la valeur de la colonne cle et celle du contenu
		uuid=""
		valeur=""
		df_sortie<- as.data.frame(setNames(replicate(2,numeric(0), simplify = F),c("id","json") ))
		for(i in 1:nrow(df_data)) {
			if (!(df_data[i,1]==uuid)){ #On change d'identifiant ligne
				if (valeur=="") { #La valeur ne contient pas encore de valeurs assemblées en tableau
					valeur=paste0("[",df_data[i,2],",")	#initialise le tableau
					uuid=df_data[i,1] #mémorise la nouvelle identification de ligne
				}  else {		  # Un tableau est déjà en cours de construction 
					#écriture de la ligne de l'ancienne identification de ligne
					ligne <- data.frame(id=c(uuid), json=c(paste0(enleve_dernier_caractere(valeur),"]")))
					df_sortie<-rbind(df_sortie,ligne) #Ajoute une ligne à la sortie en enlevant la dernière virgule et en ajoutant la fermeture du tableau
					valeur=paste0("[",df_data[i,2],",")	#initialise le tableau
					uuid=df_data[i,1] #mémorise la nouvelle identification de ligne	
				}
			}  else { #L'identifiant de ligne ne change pas
				valeur=paste0(valeur,df_data[i,2],",")				
			}
		}
		#Gestion du dernier enregistrement
		df_sortie<-rbind(df_sortie,c(uuid,paste0(enleve_dernier_caractere(valeur),"]")))
		
	}
	colnames(df_sortie)=c(nom_colonne_a_conserver,nom_colonne_json)
	return(df_sortie)
}

fusionne_json<-function(mode_extraction,mode_sortie,dossier_sortie,fichier_principal,schema_formulaire_courant=NULL){
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
#	- fichier_principal : nom du dernier fichier dans la fusion (sans le chemin).
#	- schema_formulaire_courant : schema du formulaire construit par l'appelant via la fonction schema_formulaire

# Dernière modification 15/12/2022
#		Création fonction

	# Désormais passé en paramètre  schema_formulaire_courant = schema_formulaire(mode_extraction=mode_extraction) #recuperation des colonnes correspondant aux champs de saisie du formulaire
	doublons=which(duplicated(subset(schema_formulaire_courant,select =c("nom_fichier","niveau","fichier_parent")))) #Liste des doublons (sur 3 colonnes)
	listing_fichier=subset(schema_formulaire_courant,select =c("nom_fichier","niveau","fichier_parent"))[-doublons,] #Sans les doublons
	#Ordonne le listing par niveau / fichier_parent, ordre dans lequel seront réalisées les fusions.
	listing_fichier=listing_fichier[order(listing_fichier$niveau,listing_fichier$fichier_parent,decreasing = TRUE), ]
	
	for(i in 1:nrow(listing_fichier)){
		if (as.numeric(listing_fichier[i,]$niveau)>0){
			#Le fichier de la forme "uuid_parent","uuid","autres colonnes,..." doit être converti en "uuid_parent_enfant","colonnejson"
			#avant d'être fusionné avec le fichier parent.
			
			#chemins des fichiers enfant et parent à compléter avec le chemin du dossier de sortie.
			fichier_enfant= normalizePath(file.path(dossier_sortie,listing_fichier[i,]$nom_fichier),mustWork=TRUE,winslash = "\\")
			fichier_parent= normalizePath(file.path(dossier_sortie,listing_fichier[i,]$fichier_parent),mustWork=TRUE,winslash = "\\")
			
			#la première ligne du schéma de formulaire qui contient le nom du fichier dans la colonne nom_fichier est de type repeat
			#et comporte en colonne name le nom que prendra la colonne json assemblant les différentes colonnes en un seul objet
			ligne_repeat=subset(schema_formulaire_courant,nom_fichier==listing_fichier[i,]$nom_fichier & type == "repeat",select=c("name","type"))
			if (nrow(ligne_repeat)!=1){
				print(paste("Erreur fonction fusionne_json - impossible de choisir le nom de la colonne json pour",listing_fichier[i,]$nom_fichier))
				return(FALSE)
			} else {
				nom_colonne_json=ligne_repeat[1,]$name
				#print(paste(listing_fichier[i,]$nom_fichier,nom_colonne_json))
			}
			
			#Ouverture fichier enfant
			df_enfant=read.csv(fichier_enfant,sep=";",quote=NULL)
			df_enfant_json=df2json(df_enfant[,-2],"uuid_parent",nom_colonne_json,TRUE) #Au passage on supprime la 2° colonne (UUID)
			colnames(df_enfant_json)[1] <- "uuid_parent_enfant"

			#Ouverture fichier parent
			df_parent=read.csv(fichier_parent,sep=";",quote=NULL)
			colonne_sortie=c(colnames(df_parent),nom_colonne_json) #Concaténation liste des colonnes parent + la colonne json du fichier enfant
			
			df_data= subset(merge(x=df_parent,y=df_enfant_json,by.x="uuid",by.y="uuid_parent_enfant",all.x=TRUE),select = colonne_sortie)
			write.table(df_data,fichier_parent,row.names = FALSE,quote=FALSE,sep=";")
			
			unlink(fichier_enfant)
		}
	}
	
	#Reste à convertir le fichier principal en json.
	chemin_complet_fichier_principal=normalizePath(file.path(dossier_sortie,fichier_principal),mustWork=TRUE,winslash = "\\")
	transform_file_json(chemin_complet_fichier_principal,pretty=TRUE,purge=TRUE)
		
	
}
