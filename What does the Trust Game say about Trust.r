#The data is provided by WebofScience :

#Query : "Trust Game" (Topic) or "Investment Game" (Topic) 
#Refined by : Document Types: Article and Publication Years: 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008

library(bibliometrix)
library(stringr)
library(tibble)
library(readxl)
library(dplyr)
library(tidyr)
library(biblionetwork)
library("writexl")
library("readxl")
library(networkflow)
library(magrittr)
library(dplyr)
library(tidygraph)

nodes <- database_WOS %>% 
				tibble::rownames_to_column("Noeud") %>% #Assigns an ID to each article
				subset(DT == "ARTICLE" | DT == "ARTICLE; PROCEEDINGS PAPER" | DT == "ARTICLE; EARLY ACCESS" | DT == "ARTICLE; BOOK CHAPTER" | DT == "ARTICLE; DATA PAPER") %>% #Selects the references labeled as article
				subset(AU!="[ANONYMOUS]") %>% #Delete "Anonymous" author articles
				subset(PY!=NA | PY!="") %>% #Delete articles with no publication years
				subset(CR!=NA | CR!="") #Delete articles with no bibliographical data

bibliographical_ref <- str_split(nodes$CR, ";", simplify = TRUE) #Store the bibliographic references in a separate file
bibliographical_ref <- data.frame(bibliographical_ref) #Transforms the produced matrix into data.frame
bibliographical_ref <- cbind.data.frame(nodes,bibliographical_ref) #Paste the bibliographic references to the nodes file

citations <- bibliographical_ref %>%
				mutate(across(everything(),  trimws, which = "both")) %>% #Removes spaces at the beginning and end of text
				subset(select = -c(AU, REF, CR, DT, PY, SC, PT, SO, TI, SR, Z9, WC, AB)) %>% #Keep only the IDs and bibliographic references
				pivot_longer( 2:ncol(.), names_to = "Observation", values_to = "Citation") %>% #Pivot the table to obtain a file with one line for each reference
				subset(Citation!="") %>% #Delete empty row 
				subset(Citation!= "NO TITLE CAPTURED") #Delete row with "NO TITLE CAPTURED" as references
				
references <- str_split(citations$Citation, ",", simplify = TRUE) #Split each element of Citation in a separate matrix
references <- data.frame(references) #Transforms the produced matrix into data.frame
citations <- cbind.data.frame(citations,references) #Paste separate elements of Citations to the citations file
citations <- mutate(citations, DOI_raw = str_extract(citations$Citation, "DOI .*")) #Isolates DOIs in a column


DOI <- str_split(citations$DOI_raw, ",", simplify = TRUE) #Separate DOI column in a separate file
DOI <- DOI %>% 
		data.frame() %>% #Transform this file into data.base
		rename(DOI = X1) %>% #Rename the first column to DOI
		subset(select = c(1)) #Keep only the first column

citations <- cbind.data.frame(citations, DOI) #Add the DOI column to the citations file
citations <- citations %>%
							mutate(Volume = str_extract(Citation, "V[:digit:]{1,}")) %>% #Extract the Volume data and put it in a new column
							mutate(Pages = str_extract(Citation, "P[:digit:]{1,}")) %>% #Extract the Pages data and put it in a new column
							mutate(Year1 = str_extract(X2, "[:digit:]{4}")) #Extract a 4 digits number from the column the year date is supposed to be and put it in a new column

citations <-  citations %>%
							mutate(Year2 = ifelse(is.na(Year1), str_extract(Citation, "[:digit:]{4}"), Year1)) #Extract a 4 digits number frome the Citation column and label it as the Year data if the first method did not work
							
citations <-  mutate(citations, Year = ifelse(is.na(citations$Year1), str_extract(citations$Citation, "[:digit:]{4}"), citations$Year1)) %>% #Extract a 4 digits number frome the Citation column and label it as the Year data if the first method did not work
							tibble::rownames_to_column("IDcites") %>% #Assigns a unique ID for each citation
							subset(select = c('IDcites','Noeud','Observation','Citation','X1','X3','DOI','Volume','Pages','Year')) %>% #Keep the revelant column
							rename(Author = X1, Revue = X3) %>% #Rename the columns with Author and Revue data 
							mutate(across(everything(),  trimws, which = "both")) %>% #Removes unnecessary spaces before and after the text
							mutate_all(list(~na_if(.,""))) %>% #Replace empty cells with NA
							mutate(Author = gsub("[.]","",Author)) #Delete the dots in the name column

#In order to make matches between two identical references we will create new data that combine the available ones:
	# - Author x Year x Volume (AYV)
	# - Author x Year x Pages (AYP)
	# - Author x Revue x Volume (ARV)
	# - Author x Revue x Pages (ARP) 

citations <- citations %>%
						mutate(AYP = ifelse(!is.na(Author) & !is.na(Year) & !is.na(Pages), paste(Author, Year, Pages, sep=" "), NA )) %>% #Add the AYP column
						mutate(AYV = ifelse(!is.na(Author) & !is.na(Year) & !is.na(Volume), paste(Author, Year, Volume, sep=" "), NA )) %>% #Add the AYV column
						mutate(ARV = ifelse(!is.na(Author) & !is.na(Revue) & !is.na(Volume), paste(Author, Revue, Volume, sep=" "), NA )) %>% #Add the ARV column
						mutate(ARP = ifelse(!is.na(Author) & !is.na(Revue) & !is.na(Pages), paste(Author, Revue, Pages, sep=" "), NA )) #Add the ARP column

#From now on, the goal is to associate each reference with an ID that can be associated with each appearance of the reference in the citations files.

#We start with the DOI because it is the data with which there are less possible errors, in other words, it is the data that is less likely to associate two different articles with the same ID.

ID_DOI <- unique(citations$DOI) #Groups all unique DOIs
ID_DOI <- ID_DOI %>% 
								data.frame() %>% #Transforms the file into data.frame
								tibble::rownames_to_column("ID") %>% #Assigns an ID to each DOI
								rename(DOI = ".") %>% #Rename the column in DOI
								subset(DOI!="" | DOI!=NA) %>%  #Erase the DOI NA line
								rename(ID_DOI = ID) #Rename the ID column

ID_DOI <- unique(ID_DOI[c("ID_DOI", "DOI")]) #Keeps unique pairs associating IDs and DOIs

#Once each DOI has been associated with a unique ID, we associate this ID with the constructed data when it can be linked to a DOI.

DOI_AYP <- unique(citations[c("DOI", "AYP")]) #Selects unique pairs of DOI and AYP							
DOI_AYP <- DOI_AYP %>%
						subset(DOI!=NA | DOI!="") %>% #Remove lines without DOI
						subset(AYP!=NA | AYP!="")  %>% #Remove lines without AYP
						merge(ID_DOI, by = "DOI") %>% #merge with ID_DOI to get the ID of the AYP 
						subset(select = c(2,3)) #Only keep AYP with ID


DOI_AYV <- unique(citations[c("DOI", "AYV")]) #Selects unique pairs of DOI and AYV							
DOI_AYV <- DOI_AYV %>%
						subset(DOI!=NA | DOI!="") %>% #Remove lines without DOI
						subset(AYV!=NA | AYV!="")  %>% #Remove lines without AYV
						merge(ID_DOI, by = "DOI") %>% #merge with ID_DOI to get the ID of the AYV 
						subset(select = c(2,3)) #Only keep AYV with ID


DOI_ARV <- unique(citations[c("DOI", "ARV")]) #Selects unique pairs of DOI and ARV							
DOI_ARV <- DOI_ARV %>%
						subset(DOI!=NA | DOI!="") %>% #Remove lines without DOI
						subset(ARV!=NA | ARV!="")  %>% #Remove lines without ARV
						merge(ID_DOI, by = "DOI") %>% #merge with ID_DOI to get the ID of the ARV 
						subset(select = c(2,3)) #Only keep ARV with ID


DOI_ARP <- unique(citations[c("DOI", "ARP")]) #Selects unique pairs of DOI and ARP							
DOI_ARP <- DOI_ARP %>%
						subset(DOI!=NA | DOI!="") %>% #Remove lines without DOI
						subset(ARP!=NA | ARP!="")  %>% #Remove lines without ARP
						merge(ID_DOI, by = "DOI") %>% #merge with ID_DOI to get the ID of the ARP 
						subset(select = c(2,3)) #Only keep ARP with ID



#This step is repeated for the other constructed data, in order: AYP, AYV, ARV, ARP.

ID_AYP <- unique(citations$AYP) #Groups all unique AYPs
ID_AYP <- ID_AYP %>% 
								data.frame() %>% #Transforms the file into data.frame
								rename(AYP = ".") %>% #Rename the column in AYP
								subset(AYP!="" | AYP!=NA) %>%  #Erase the AYP NA line
								merge(DOI_AYP, by = "AYP", all=T) %>%
								tibble::rownames_to_column("ID_temp")  #Assigns an temporary ID to each AYP


								
ID_AYP$ID_temp <- as.numeric(ID_AYP$ID_temp) #Consider ID_temp as a number and not as a word
ID_DOI$ID_DOI <- as.numeric(ID_DOI$ID_DOI) #Consider ID_DOI as a number and not as a word
max_ID <- max(ID_DOI$ID_DOI) #set the max_ID

ID_AYP <- ID_AYP %>%
					mutate(ID_AYP = ifelse(is.na(ID_DOI), ID_temp + max_ID, ID_DOI)) %>% #If the AYP has already been assigned an ID previously, use this ID. If not, it is assigned an ID that is the sum of the temporary ID and the max ID in order to avoid having duplicate IDs.
					subset(select = c(2,4)) #Keeps only the AYPs and their IDs

ID_AYP <- unique(ID_AYP[c("ID_AYP", "AYP")]) #Keeps unique pairs associating IDs and AYPs			


AYP_AYV <- unique(citations[c("AYP", "AYV")]) #Selects unique pairs of AYP and AYV	
AYP_AYV <- AYP_AYV %>%
						subset(AYP!=NA | AYP!="") %>% #Remove lines without AYP
						subset(AYV!=NA | AYV!="")  %>% #Remove lines without AYV
						merge(ID_AYP, by = "AYP") %>% #merge with ID_AYP to get the ID of the AYP 
						subset(select = c(2,3)) #Only keep AYV with ID

AYP_ARV <- unique(citations[c("AYP", "ARV")]) #Selects unique pairs of AYP and ARV
AYP_ARV <- AYP_ARV %>%
						subset(AYP!=NA | AYP!="") %>% #Remove lines without AYP
						subset(ARV!=NA | ARV!="")  %>% #Remove lines without ARV
						merge(ID_AYP, by = "AYP") %>% #merge with ID_AYP to get the ID of the AYP 
						subset(select = c(2,3)) #Only keep ARV with ID

AYP_ARP <- unique(citations[c("AYP", "ARP")]) #Selects unique pairs of AYP and ARP	
AYP_ARP <- AYP_ARP %>%
						subset(AYP!=NA | AYP!="") %>% #Remove lines without AYP
						subset(ARP!=NA | ARP!="")  %>% #Remove lines without ARP
						merge(ID_AYP, by = "AYP") %>% #merge with ID_AYP to get the ID of the AYP 
						subset(select = c(2,3)) #Only keep ARP with ID

#The third step of the matching consists in associating to each unique AYV an ID

ID_AYV <- unique(citations$AYV) #Select unique AYVs
ID_AYV <- ID_AYV %>%
								data.frame() %>% #Transforms the file into data.frame
								rename(AYV = ".") %>% #Rename the column in AYV
								subset(AYV!="" | AYV!=NA) %>%  #Erase the AYV NA line
								merge(DOI_AYV, by = "AYV", all=T) %>% #merge AYV with AYV already associate in the DOI step
								merge(AYP_AYV, by = "AYV", all=T) %>% #merge AYV with AYV already associate in the AYV step
								tibble::rownames_to_column("ID_temp")  #Assigns an temporary ID to each AYP



ID_AYV$ID_temp <- as.numeric(ID_AYV$ID_temp) #Consider ID_temp as a number and not as a word
ID_AYP$ID_AYP <- as.numeric(ID_AYP$ID_AYP) #Consider ID_AYP as a number and not as a word
max_ID <- max(ID_AYP$ID_AYP) #re-set the max_ID

ID_AYV <- ID_AYV %>%
					mutate(ID_AYV_step_1 = ifelse(is.na(ID_DOI), ID_AYP, ID_DOI)) %>% #If the AYV has already been assigned an ID previously, use this ID. If not, it is assigned an ID that is the sum of the temporary ID and the max ID in order to avoid having duplicate IDs.
					mutate(ID_AYV = ifelse(is.na(ID_AYV_step_1), ID_AYV$ID_temp + max_ID, ID_AYV_step_1)) %>%
					subset(select = c(2,6))   

ID_AYV <- unique(ID_AYV[c("ID_AYV", "AYV")])

AYV_ARV <- unique(citations[c("AYV", "ARV")]) 
AYV_ARV <- AYV_ARV %>%
						subset(AYV!=NA | AYV!="") %>% #Remove lines without AYV
						subset(ARV!=NA | ARV!="")  %>% #Remove lines without ARV
						merge(ID_AYV, by = "AYV") %>% 
						subset(select = c(2,3)) #Only keep ARV with ID

AYV_ARP <- unique(citations[c("AYV", "ARP")]) 
AYV_ARP <- AYV_ARP %>%
						subset(AYV!=NA | AYV!="") %>% #Remove lines without AYV
						subset(ARP!=NA | ARP!="")  %>% #Remove lines without ARP
						merge(ID_AYV, by = "AYV") %>% 
						subset(select = c(2,3)) #Only keep ARP with ID

#The fourth step of the matching consists in associating to each unique ARV an ID

ID_ARV <- unique(citations$ARV)
ID_ARV <- ID_ARV %>%
								data.frame() %>% #Transforms the file into data.frame
								rename(ARV = ".") %>% #Rename the column in ARV
								subset(ARV!="" | ARV!=NA) %>%  #Erase the ARV NA line
								merge(DOI_ARV, by = "ARV", all=T) %>%
								merge(AYP_ARV, by = "ARV", all=T) %>%
								merge(AYV_ARV, by = "ARV", all=T) %>%
								tibble::rownames_to_column("ID_temp")  #Assigns an temporary ID to each AYRV

ID_ARV$ID_temp <- as.numeric(ID_ARV$ID_temp)
ID_AYV$ID_AYV <- as.numeric(ID_AYV$ID_AYV)
max_ID <- max(ID_AYV$ID_AYV)

ID_ARV <- ID_ARV %>%
					mutate(ID_ARV_step_1 = ifelse(is.na(ID_DOI), ID_AYP, ID_DOI)) %>%
					mutate(ID_ARV_step_2 = ifelse(is.na(ID_ARV_step_1), ID_AYV, ID_ARV_step_1)) %>%
					mutate(ID_ARV = ifelse(is.na(ID_ARV_step_2), ID_ARV$ID_temp + max_ID, ID_ARV_step_2)) %>%
					subset(select = c(2,8))

ID_ARV <- unique(ID_ARV[c("ID_ARV", "ARV")])
 

ARV_ARP <- unique(citations[c("ARV", "ARP")]) 
ARV_ARP <- ARV_ARP %>%
						subset(ARV!=NA | ARV!="") %>% #Remove lines without ARV
						subset(ARP!=NA | ARP!="")  %>% #Remove lines without ARP
						merge(ID_ARV, by = "ARV") %>% 
						subset(select = c(2,3)) #Only keep ARP with ID


#The fifth step of the matching consists in associating to each unique ARP an ID

ID_ARP <- unique(citations$ARP)
ID_ARP <- ID_ARP %>%
								data.frame() %>% #Transforms the file into data.frame
								rename(ARP = ".") %>% #Rename the column in ARP
								subset(ARP!="" | ARP!=NA) %>%  #Erase the ARP NA line
								merge(DOI_ARP, by = "ARP", all=T) %>%
								merge(AYP_ARP, by = "ARP", all=T) %>%
								merge(AYV_ARP, by = "ARP", all=T) %>%
								merge(ARV_ARP, by = "ARP", all=T) %>%
								tibble::rownames_to_column("ID_temp")  #Assigns an temporary ID to each ARP

ID_ARP$ID_temp <- as.numeric(ID_ARP$ID_temp)
ID_ARV$ID_ARV <- as.numeric(ID_ARV$ID_ARV)
max_ID <- max(ID_ARV$ID_ARV)

ID_ARP <- ID_ARP %>%
					mutate(ID_ARP_step_1 = ifelse(is.na(ID_DOI), ID_AYP, ID_DOI)) %>%
					mutate(ID_ARP_step_2 = ifelse(is.na(ID_ARP_step_1), ID_AYV, ID_ARP_step_1)) %>%
					mutate(ID_ARP_step_3 = ifelse(is.na(ID_ARP_step_2), ID_ARV, ID_ARP_step_2)) %>%
					mutate(ID_ARP = ifelse(is.na(ID_ARP_step_3), ID_ARP$ID_temp + max_ID, ID_ARP_step_3)) %>%
					subset(select = c(2,10))

ID_ARP <- unique(ID_ARP[c("ID_ARP", "ARP")])


Edges <- citations %>%
					merge(ID_DOI, by = "DOI", all=T) %>% #Associates DOIs with their ID in the citation file
					merge(ID_AYP, by = "AYP", all=T) %>% #Associates AYPs with their ID in the citation file
					merge(ID_AYV, by = "AYV", all=T) %>% #Associates AYVs with their ID in the citation file
					merge(ID_ARV, by = "ARV", all=T) %>% #Associates ARVs with their ID in the citation file
					merge(ID_ARP, by = "ARP", all=T) %>% #Associates ARPs with their ID in the citation file
					subset(select = c('Noeud','Observation','ID_DOI','ID_AYP','ID_AYV','ID_ARV','ID_ARP')) %>% #Selects only the ID of the nodes, the cited reference and the different IDs found for this reference.
					mutate(ID_step_1 = ifelse(is.na(ID_DOI), ID_AYP, ID_DOI)) %>% #Assigns an ID_citation to each references
					mutate(ID_step_2 = ifelse(is.na(ID_step_1), ID_AYV, ID_step_1)) %>%
					mutate(ID_step_3 = ifelse(is.na(ID_step_2), ID_ARV, ID_step_2)) %>%
					mutate(ID_citation = ifelse(is.na(ID_step_3), ID_ARP, ID_step_3)) %>%
					subset(select = c('Noeud','Observation','ID_citation')) 

Edges <- unique(Edges[c("Noeud", "Observation", "ID_citation")]) 
Edges <- Edges %>%
				subset(select = c('Noeud','ID_citation')) %>%
				subset(ID_citation!="" | ID_citation!=NA)
Edges <- biblio_coupling(Edges, "Noeud", "ID_citation", weight_threshold = 3) #Use of the biblio_coupling function to obtain the edges file
Edges <- Edges %>%
				subset(select = c('Source','Target','weight')) 

#In order to keep in the node file only those items that have a link with another article in the network we produce a List of nodes
List_of_nodes <- Edges %>%
				subset(select = c('Source','Target')) %>%
				pivot_longer( 1:2, names_to = "x", values_to = "ID") %>%
				subset(select = c('ID'))
List_of_nodes <- unique(List_of_nodes$ID)
List_of_nodes <- List_of_nodes %>%
									data.frame() %>%
									rename(ID = ".")

#In order to label the nodes we produce a list of names that we paste with the Publication Year in the next step
List_of_names <- sub(" .*", "", nodes$AU)
List_of_names <- data.frame(List_of_names) 


nodes <- nodes %>%
				cbind(List_of_names) %>%
				mutate(Label = paste(List_of_names, PY, sep=", ")) %>%
				subset(select = c('Noeud','Label','AU','PY','TI','SO','Z9','SC','WC', 'AB')) %>% 
				rename(ID = "Noeud") %>% 
				merge(List_of_nodes, by = "ID")

write_xlsx(nodes,"C:/Users/ncami/OneDrive/Bureau//nodes_trust_games_th3.xlsx")
write_xlsx(Edges,"C:/Users/ncami/OneDrive/Bureau//edges_trust_games_th3.xlsx")
