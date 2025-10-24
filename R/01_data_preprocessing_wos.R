# Data preprocessing
#
# The data is provided by Web of Science (WoS):
# 
# Query: "Trust Game" (Topic) OR "Investment Game" (Topic)
# Refined by Publication Years: 1995–2024
#
# In accordance with Web of Science data sharing policies, 
# only the columns necessary for the analysis are included in this project. 
# The full dataset is not publicly shared.

library(stringr)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)


document_types <- c("ARTICLE", 
                    "ARTICLE; PROCEEDINGS PAPER", 
                    "ARTICLE; EARLY ACCESS", 
                    "ARTICLE; BOOK CHAPTER", 
                    "ARTICLE; DATA PAPER")

nodes <- database_WOS %>%
  tibble::rownames_to_column("Noeud") %>%
  filter(
    DT %in% document_types,
    AU != "[ANONYMOUS]",
    !is.na(PY) & PY >= 1998 & PY <= 2024, #1998-2024 de base
    !is.na(CR) & CR != ""
  )

bibliographical_ref <- str_split(nodes$CR, ";", simplify = TRUE) #Split each references and store the bibliographic references in a separate file
bibliographical_ref <- data.frame(bibliographical_ref) #Transforms the produced matrix into data.frame
bibliographical_ref <- cbind.data.frame(nodes,bibliographical_ref) #Paste the bibliographic references to the nodes file

citations <- bibliographical_ref %>%
				mutate(across(everything(), \(x) trimws(x, which = "both"))) %>% 
				
				# Garde "Noeud" et les colonnes dont le nom commence par X suivi d'un ou plusieurs chiffres
				select(Noeud, matches("^X\\d+$")) %>% 
				
				# Pivote toutes les colonnes sauf "Noeud"
				pivot_longer(-Noeud, names_to = "Observation", values_to = "Citation") %>% 
				
				# Supprime les lignes vides et non désirées
				filter(Citation != "" & Citation != "NO TITLE CAPTURED")

				
references <- str_split(citations$Citation, ",", simplify = TRUE) #Split each element of Citation in a separate matrix
references <- data.frame(references) #Transforms the produced matrix into data.frame
citations <- cbind.data.frame(citations,references) #Paste separate elements of Citations to the citations file
citations <- mutate(citations, DOI_raw = str_extract(citations$Citation, "DOI .*")) #Isolates DOIs in a column


DOI <- str_split(citations$DOI_raw, ",", simplify = TRUE) #Separate DOI column in a separate file
DOI <- DOI %>% 
		data.frame() %>% #Transform this file into data.base
		rename(DOI = X1) %>% #Rename the first column to DOI
		subset(select = c(1)) %>% #Keep only the first column
		mutate(DOI = str_replace(DOI, "^DOI DOI", "DOI"))

citations <- cbind.data.frame(citations, DOI) #Add the DOI column to the citations file
citations <- citations %>%
							mutate(Volume = str_extract(Citation, "V[:digit:]{1,}")) %>% #Extract the Volume data and put it in a new column
							mutate(Pages = str_extract(Citation, "P[:digit:]{1,}")) %>% #Extract the Pages data and put it in a new column
							mutate(Year1 = str_extract(X2, "[:digit:]{4}")) #Extract a 4 digits number from the column the year date is supposed to be and put it in a new column

citations <- citations %>%
	mutate(Year = ifelse(is.na(Year1), str_extract(Citation, "\\d{4}"), Year1)) %>% # Crée la colonne 'Year' en extrayant 4 chiffres si 'Year1' est NA
	tibble::rownames_to_column("IDcites") %>% # Assigne un ID unique pour chaque citation
	select(IDcites, Noeud, Observation, Citation, X1, X3, DOI, Volume, Pages, Year) %>% # Garde les colonnes pertinentes (remplace subset par select pour la cohérence)
	rename(Author = X1, Revue = X3) %>% # Renomme les colonnes
	mutate(
    across(everything(), \(x) trimws(x, which = "both")),
    across(everything(), \(x) na_if(x, "")),
    Author = gsub("[.]", "", Author),
	Full_author = Author,
	Author = gsub("\\s'", "'", Author),
	Author = stringr::word(Author, 1)
  )

#In order to make matches between two identical references we will create new data for each references that combine the available ones:
	# - Author x Year x Volume (AYV)
	# - Author x Year x Pages (AYP)
	# - Author x Revue x Volume (ARV)
	# - Author x Revue x Pages (ARP)
  	# - Author x Year x Revue (AYR)

citations <- citations %>%
						mutate(AYP = ifelse(!is.na(Author) & !is.na(Year) & !is.na(Pages), paste(Author, Year, Pages, sep=" "), NA )) %>% #Add the AYP column
						mutate(AYV = ifelse(!is.na(Author) & !is.na(Year) & !is.na(Volume), paste(Author, Year, Volume, sep=" "), NA )) %>% #Add the AYV column
						mutate(ARV = ifelse(!is.na(Author) & !is.na(Revue) & !is.na(Volume), paste(Author, Revue, Volume, sep=" "), NA )) %>% #Add the ARV column
						mutate(ARP = ifelse(!is.na(Author) & !is.na(Revue) & !is.na(Pages), paste(Author, Revue, Pages, sep=" "), NA )) %>% #Add the ARP column
  						mutate(AYR = ifelse(!is.na(Author) & !is.na(Year) & !is.na(Revue), paste(Author, Year, Revue, sep=" "), NA )) #Add the ARP column

#lignes sans citations
citations_vides <- citations[is.na(citations$AYP) & is.na(citations$AYV) &  is.na(citations$ARV) & is.na(citations$ARP) & is.na(citations$AYR), ]

#From now on, the goal is to associate each reference with an ID that can be associated with each appearance of the reference in the citations files.

#We start with the DOI because it is the data with which there are less possible errors, in other words, it is the data that is less likely to associate two different articles with the same ID.


#Once each DOI has been associated with a unique ID, we associate this ID with the constructed data when it can be linked to a DOI.

# Fonction pour éviter les correspondances multiples
make_id_pair_unique <- function(data, col_key, col_value, id_table) {
  col_key <- rlang::ensym(col_key)      # Clé de jointure (ex: DOI)
  col_value <- rlang::ensym(col_value)  # Valeur à associer (ex: AYP)
  
  # Le nom de la colonne de comptage est dérivé du nom de la colonne clé
  # ex: si col_key est "DOI", la colonne de comptage sera "n_ID_DOI"
  count_col_name <- paste0("n_", rlang::as_name(col_key))
  count_col <- rlang::sym(count_col_name)

  data %>%
    # 1. Obtenir les paires uniques de clé/valeur
    distinct(!!col_key, !!col_value) %>%
    
    # 2. Filtrer les valeurs vides ou non valides
    filter(!is.na(!!col_key), !!col_key != "",
           !is.na(!!col_value), !!col_value != "") %>%
    
    # 3. Joindre avec la table d'ID pour récupérer le nombre d'occurrences (n)
    inner_join(id_table, by = rlang::as_name(col_key)) %>%
    
    # 4. grouper par valeur
    group_by(!!col_value) %>% # Pour chaque valeur (ex: chaque AYP)...
    
    # 5. ...on ne garde que la ligne où le compte est le plus élevé.
    # slice_max ordonne par le compte et prend la première ligne.
    # with_ties = FALSE garantit qu'on ne garde qu'une seule ligne même en cas d'égalité.
    slice_max(order_by = !!count_col, n = 1, with_ties = FALSE) %>%
    
    # 6. Dégrouper pour les opérations futures
    ungroup()
}

ID_DOI <- citations %>% 
  filter(!is.na(DOI), DOI != "") %>% 
  count(DOI, name = "n_DOI") %>%  # compte le nombre d'occurrences de chaque DOI
  mutate(ID_DOI = row_number())


DOI_AYP <- make_id_pair_unique(citations, DOI, AYP, ID_DOI)
DOI_AYV <- make_id_pair_unique(citations, DOI, AYV, ID_DOI)
DOI_ARV <- make_id_pair_unique(citations, DOI, ARV, ID_DOI)
DOI_ARP <- make_id_pair_unique(citations, DOI, ARP, ID_DOI)
DOI_AYR <- make_id_pair_unique(citations, DOI, AYR, ID_DOI)

max_ID <- max(ID_DOI$ID_DOI) #set the max_ID

#This step is repeated for the other constructed data, in order: AYP, AYV, ARV, ARP, AYR.

ID_AYP <- citations %>%
  filter(!is.na(AYP), AYP != "") %>% 
  count(AYP, name = "n_AYP") %>%
  left_join(DOI_AYP, by = "AYP") %>%        # 3. merge pour récupérer ID_DOI si existant
  mutate(ID_AYP = ifelse(is.na(ID_DOI), row_number() + max_ID, ID_DOI)) %>% # 4. ID unique 
  select(AYP, ID_AYP, n_AYP)

AYP_AYV <- make_id_pair_unique(citations, AYP, AYV, ID_AYP)
AYP_ARV <- make_id_pair_unique(citations, AYP, ARV, ID_AYP)
AYP_ARP <- make_id_pair_unique(citations, AYP, ARP, ID_AYP)
AYP_AYR <- make_id_pair_unique(citations, AYP, AYR, ID_AYP)


#The third step of the matching consists in associating to each unique AYV an ID

max_ID <- max(ID_AYP$ID_AYP) #set the max_ID

ID_AYV <- citations %>% 
  filter(!is.na(AYV), AYV != "") %>% 
  count(AYV, name = "n_AYV") %>% 
  left_join(DOI_AYV, by = "AYV") %>%    
  left_join(AYP_AYV, by = "AYV") %>% 
  mutate(
    ID_AYV = coalesce(ID_DOI, ID_AYP)
  ) %>% 
  mutate(
    ID_AYV = if_else(
      is.na(ID_AYV),
      row_number() + max_ID,
      ID_AYV
    )
  ) %>%
  select(AYV, ID_AYV, n_AYV)

AYV_ARV <- make_id_pair_unique(citations, AYV, ARV, ID_AYV)
AYV_ARP <- make_id_pair_unique(citations, AYV, ARP, ID_AYV)
AYV_AYR <- make_id_pair_unique(citations, AYV, AYR, ID_AYV)

########################################################################################

#The fourth step of the matching consists in associating to each unique ARV an ID

max_ID <- max(ID_AYV$ID_AYV) #set the max_ID

ID_ARV <- citations %>% 
  filter(!is.na(ARV), ARV != "") %>% 
  count(ARV, name = "n_ARV") %>% 
  left_join(DOI_ARV, by = "ARV") %>%    
  left_join(AYP_ARV, by = "ARV") %>% 
  left_join(AYV_ARV, by = "ARV") %>% 
  mutate(
    ID_ARV = coalesce(ID_DOI, ID_AYP, ID_AYV)
  ) %>% 
  mutate(
    ID_ARV = if_else(
      is.na(ID_ARV),
      row_number() + max_ID,
      ID_ARV
    )
  ) %>%
  select(ARV, ID_ARV, n_ARV)

ARV_ARP <- make_id_pair_unique(citations, ARV, ARP, ID_ARV)
ARV_AYR <- make_id_pair_unique(citations, ARV, AYR, ID_ARV)

max_ID <- max(ID_ARV$ID_ARV) #set the max_ID

ID_ARP <- citations %>% 
  filter(!is.na(ARP), ARP != "") %>% 
  count(ARP, name = "n_ARP") %>% 
  left_join(DOI_ARP, by = "ARP") %>%    
  left_join(AYP_ARP, by = "ARP") %>% 
  left_join(AYV_ARP, by = "ARP") %>% 
  left_join(ARV_ARP, by = "ARP") %>% 
  mutate(
    ID_ARP = coalesce(ID_DOI, ID_AYP, ID_AYV, ID_ARV)
  ) %>% 
  mutate(
    ID_ARP = if_else(
      is.na(ID_ARP),
      row_number() + max_ID,
      ID_ARP
    )
  ) %>%
  select(ARP, ID_ARP, n_ARP)


ARP_AYR <- make_id_pair_unique(citations, ARP, AYR, ID_ARP)

max_ID <- max(ID_ARP$ID_ARP) #set the max_ID


ID_AYR <- citations %>% 
  filter(!is.na(AYR), AYR != "") %>% 
  count(AYR, name = "n_AYR") %>% 
  left_join(DOI_AYR, by = "AYR") %>%    
  left_join(AYP_AYR, by = "AYR") %>% 
  left_join(AYV_AYR, by = "AYR") %>% 
  left_join(ARV_AYR, by = "AYR") %>% 
  left_join(ARP_AYR, by = "AYR") %>% 
  mutate(
    ID_AYR = coalesce(ID_DOI, ID_AYP, ID_AYV, ID_ARV, ID_ARP)
  ) %>% 
  mutate(
    ID_AYR = if_else(
      is.na(ID_AYR),
      row_number() + max_ID,
      ID_AYR
    )
  ) %>%
  select(AYR, ID_AYR, n_AYR)


id_tables <- list(ID_DOI, ID_AYP, ID_AYV, ID_ARV, ID_ARP)

Edges <- reduce(id_tables, ~ merge(.x, .y, all = TRUE), .init = citations) %>%
  transmute(Noeud, Observation,
            ID_citation = coalesce(ID_DOI, ID_AYP, ID_AYV, ID_ARV, ID_ARP)) %>%
  distinct(Noeud, ID_citation, .keep_all = FALSE) %>%   # équivalent de unique()
  filter(!is.na(ID_citation) & ID_citation != "")

Edges_before_coupling <- Edges