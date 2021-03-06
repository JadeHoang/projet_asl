####install et charger les packages####
install.packages("xml2")
install.packages("stringr")
install.packages("svMisc")
install.packages("udpipe")
install.packages("quanteda")
# install.packages("dplyr")
library(udpipe)
library(dplyr)
library(xml2)
library(stringr)
library(quanteda)
####import xml file####

#import version de Yang Yang avec rootNode
# test <- read_xml(x = as(rootNode,"character"))

#importer avec xml2
test <- read_xml("GENIA_term_3.02/GENIAcorpus3.02.xml")

####extraire tous les lex et sem####
#initialise sem et lex comme vecteurs vides
sem <- c()
lex <- c()

#initialise path comme chemin pour trouver tous les balises <cons></cons>
path <- "//sentence/cons"

#trouver tous les balises <cons></cons> avec xml2
att <- xml_find_all(test, xpath = path)

#boucle tant qu'on trouve encore de balises <cons></cons>
while (length(att) > 0){
  
  #ajouter dans vecteur sem les valeurs de l'attribute sem
  sem <- c(sem,xml_attr(att,"sem"))
  
  #ajouter dans vecteur lex les valeurs de l'attribute lex
  lex <- c(lex,xml_attr(att,"lex"))
  
  #modifier le path en ajoutant encore un autre node cons 
  path <- paste0(path,"/cons")

  #retrouver tous les balises <cons></cons> embriquées par une autre balise <cons></cons>
  att <- xml_find_all(test, xpath = path)
}

####un dataframe avec 2 colonnes lex et sem####
df_test <- as.data.frame(cbind(lex,sem),stringsAsFactors = FALSE )
  
#enlever tous les lignes qui ont des valeurs NA (les termes sans sem)
df_test <- df_test[complete.cases(df_test),]

#garder que des lignes uniques
df_test <- unique(df_test)

####les fonctions pour couper les lex et sem compliqués (ceux avec AND OR ...)####

#fonction split_all
split_all <- function(str){
  # str <- gsub("[][()]","",str)
  # supprimer les "(" , ")" , "*" 
  # str <- gsubfn(".", list("(" = "", ")" = "","*"=""), str)
  str <- str_replace_all(str,"\\(|\\)|\\*","")
  #couper les termes qui sont separées par " " 
  return(unlist(strsplit(x = str," ")))
}

#fonction find_intersect
find_intersect <- function(df){
  
  #creer une copie de dataframe
  df_temp <- df
  
  #creer un dataframe vide mais a les memes colonnes que df
  df_empt <- df[0,]
  
  #initialise un vecteur vide pour stoker les index des lignes a enlever
  index <- c()
  
  #boucle pour aller tout au long du dataframe
  for (i in 1:nrow(df)){
    #afficher ieme iteration
    # print(i)
    require(svMisc)
    progress(i, max.value = nrow(df))
    # Sys.sleep(0.01)
    
    #utiliser split_all pour ieme valeur de la colonne lex de dataframe
    lex <- split_all(df[i,1])
    #utiliser split_all pour ieme valeur de la colonne sem de dataframe
    sem <- split_all(df[i,2])
    
    #trouver le mot en commun de ces 2 valeurs
    common <- intersect(lex,sem)
    
    #s'il existe common
    if(length(common) > 0){
      #on a trouvé
      # print("found!")
      
      #stoker i dans index
      index <- c(index, i )
      
      #supprimer le(s) mot(s) commun(s) de lex et sem 
      lex <- setdiff(lex,common)
      sem <- setdiff(sem,common)
      
      #creer un data frame en combinant en colonne lex et sem
      mat <- as.data.frame(cbind(lex,sem),stringsAsFactors = FALSE)
      
      #ajouter chaque ligne de mat vers le dataframe vide
      df_empt <- rbind(df_empt,mat)
    }
  
  }
  
  #supprimer les index des lignes complexe dans la copie
  df_temp <- df_temp[-index,]
  
  #combiner df_temp et df_empt
  df_temp <- rbind(df_temp, df_empt)
  
  return(df_temp)
}

####creer un nouveau dataframe sans les lignes de lex et sem complexes####

df_new <- find_intersect(df_test) #il prend quelques moments car on a plus de 30000 lignes
df_new <- unique(df_new)
nrow(df_new)
# [1] 35508

####ensemble des articles####
# id <- xml_find_all(test, "//bibliomisc")
# vals.id <- trimws(xml_text(id))
# vals.id <- 1:2000

#fonction de concatener les balises de sentences
concat_text <- function(xmlnode){
  #converti xmlnode 
  xml.node <- read_xml(as.character(xmlnode))
  
  #extraire seulement les phrases
  xml.sentence <- xml_find_all(xml.node,"./sentence")
  
  #concatener les phrases
  xml.full <- paste(xml_text(xml.sentence),collapse = " ")
  
  return(xml.full)
}

#id des docs
vals.id <- 1:2000
#stocker les balises de titre
title <- xml_find_all(test, "//article/title")
#extraire les titres et les stocker dans un vecteur
vals.title <- sapply(title, function(x) {concat_text(x)})

#stocker les balises d'abstract
abstract <- xml_find_all(test, "//article/abstract")
# extraire les resumes et les stocker dans un vecteur
vals.abstract <- sapply(abstract, function(x) {concat_text(x)})

#combiner les titres et les abstracts
vals.article <- paste(vals.title,vals.abstract)

#data frame qui stock les articles
data_article <- data.frame(cbind(doc_id = vals.id, doc = vals.article),
                          stringsAsFactors = FALSE)

####creer un dictionnaire par rapport a df_new####
#frequence des classes d'entites
sort(table(df_new$sem),decreasing = TRUE)

#Choix de classe d'entité
#trouver tous les sem qui contiennent ces 5 entités
##DNA
ent_dna <- grep(pattern = "DNA",x = unique(df_new$sem), value = TRUE)
##PROTEIN
ent_protein <- grep(pattern = "protein",x = unique(df_new$sem), value = TRUE)
##RNA
ent_rna <- grep(pattern = "RNA",x = unique(df_new$sem), value = TRUE)
##CELL_TYPE
ent_cell_type <- grep(pattern = "cell_type",x = unique(df_new$sem), value = TRUE)
##CELL_LINE
ent_cell_line<- grep(pattern = "cell_line",x = unique(df_new$sem), value = TRUE)

#garder que des lignes qui ppartiennent a ces 5 entités
df_5_ent <- df_new[which(df_new$sem %in% c(ent_dna,ent_protein,ent_rna,ent_cell_type,ent_cell_line)),]

#reconstruire un dictionnaire de ces 5 entités
labelling <- function(cellule){
  
  if(cellule %in% ent_dna) cellule <- "DNA"
  else if (cellule %in% ent_protein) cellule <- "PROTEIN"
  else if (cellule %in% ent_rna) cellule <- "RNA"
  else if (cellule %in% ent_cell_type) cellule <- "CTYPE"
  else cellule <- "CLIEN"
  
  return(cellule)
}
#relibeller les entites
df_5_ent$sem <- sapply(df_5_ent$sem,function(x) {labelling(x)})
#garger seulement les lignes uniques
df_5_ent <- unique(df_5_ent)
# length(unique(df_5_ent$lex))

#replacer les espaces en underscores
df_5_ent <- as.data.frame(apply(df_5_ent, 2, function(x){gsub(" ","_",x)}),stringsAsFactors = FALSE)

#garder que des lex uniques (probleme avec les annotations du corpus)
df_5_ent <- df_5_ent[!duplicated(df_5_ent["lex"]),]

####IOB tagging####
#les lex uniques qui designent DNA,PROTEIN,RNA,CTYPE,CLINE
term.dna <- unique(df_5_ent$lex[which(df_5_ent$sem=="DNA")])
term.protein <- unique(df_5_ent$lex[which(df_5_ent$sem=="PROTEIN")])
term.rna <- unique(df_5_ent$lex[which(df_5_ent$sem=="RNA")])
term.ctype <- unique(df_5_ent$lex[which(df_5_ent$sem=="CTYPE")])
term.cline <- unique(df_5_ent$lex[which(df_5_ent$sem=="CLINE")])

#remplacer _ par espace
term.dna <- as.character(sapply(term.dna, function(x) {gsub("_"," ",x)}))
term.protein <- as.character(sapply(term.protein, function(x) {gsub("_"," ",x)}))
term.rna <- as.character(sapply(term.rna, function(x) {gsub("_"," ",x)}))
term.ctype <- as.character(sapply(term.ctype, function(x) {gsub("_"," ",x)}))
term.cline <- as.character(sapply(term.cline, function(x) {gsub("_"," ",x)}))

#creer un dictionaire avec package quanteda
dict <- dictionary(list(DNA=term.dna,
                        PROTEIN=term.protein
                        ,RNA=term.rna
                        ,CTYPE=term.ctype,
                         CLINE=term.cline
                        ))

#tokeniser les abtracts (tokeniser les mots composés)
#les mots qui se ressemblent et les remplacer dans la liste des tokens 
infle <- c("cell","cells") #les mots qui se ressemblent
lemma <- rep("cell", length(infle))
toks_replace <- tokens_replace(tokens(data_article$doc), infle, lemma) #remplacer les mots
toks_compound <- tokens_compound(toks_replace,dict,join = FALSE)

#enregistrer les id des documents dans doc_id
doc_id <- c()
for (i in 1:length(docnames(toks_compound))){

  svMisc::progress(i, max.value = length(docnames(toks_compound)))
  
  doc_id <- c(doc_id,rep(i,length(as.character(toks_compound[i]))))
}

#data frame pour les tokens et leurs documents id
data_toks <- data.frame(doc_id, toks = as.character(toks_compound), stringsAsFactors = FALSE)

#fonction pour libeller en IOB/BOI les tokens 
iob_tag <- function(word,semantic){
  #separer le mot par "_"
  if (grepl(pattern = "_", word,fixed=TRUE)){
    word.vec <- unlist(strsplit(word,"_"))
  
    label.vec <- ifelse(word.vec == word.vec[1], 
                        paste("B",semantic,sep = "-"), 
                        paste("I",semantic,sep = "-"))

  }else{
    word.vec <- word
    
    label.vec <- paste("B",semantic,sep = "-")
    
  }
  
  # return(data.frame(word = word.vec, label = label.vec,row.names = NULL,stringsAsFactors = FALSE))
  return(list(word=word.vec , label=label.vec))
}

#identifiant des doc selon IOB tags
iob_id_doc <- function(word,id,semantic){
  #separer le mot par "_"
  if (grepl(pattern = "_", word,fixed=TRUE)){
  
    word.vec <- unlist(strsplit(word,"_"))
    
    doc.vec <- rep(id,length(word.vec))
    
  }else{
    
    doc.vec <- id
  }
  
  # return(data.frame(word = word.vec, label = label.vec,row.names = NULL,stringsAsFactors = FALSE))
  return(doc.vec)
}


#initialiser les vecteurs pour les tokens et IOB tags
iob.word <- c()
iob.label <- c()

#vecteur qui stocke les tokens apres avoir separé et libellé avec les tag IOB
iob.word <- unlist(sapply(as.character(toks_compound), function(x){
                  if(x %in% df_5_ent$lex) iob_tag(x, df_5_ent$sem[which(df_5_ent$lex==x )])$word
                  else x}),use.names = FALSE)

#vecteur qui stocke les tag IOB qui correspondent a des tokens au dessus
iob.label <- unlist(sapply(as.character(toks_compound), function(x){
                  if(x %in% df_5_ent$lex) iob_tag(x, df_5_ent$sem[which(df_5_ent$lex==x )])$label
                  else "O"}),use.names = FALSE)

#identifiant des documents
doc_id <- c()
doc_id <- unlist(apply(data_toks ,1, function(x){
  if(x[2] %in% df_5_ent$lex) c(doc_id,iob_id_doc(x[2], x[1] ,df_5_ent$sem[which(df_5_ent$lex==x[2])]))
  else c(doc_id,x[1])}),use.names = FALSE)

#longueur de vecteur a
length(iob.word)
#longueur de vecteur b
length(iob.label)
#longueur de vecteur identifiant document
length(doc_id)
#il faut que iob.word et  aient de meme longueur = 497668

####POS tagging avec NLP####

#telecharger le model en anglais
dl <- udpipe_download_model(language = "english")
# str(dl)

#Entrer le fichier qu'on a just téléchargé dans le directoir actuel
udmodel_english <- udpipe_load_model(file = "english-ud-2.0-170801.udpipe")
# udmodel_english <- udpipe_load_model(file = dl$file_model)

#Vu qu'on a 497.668 tokens, il prend bcp de temps a executer la fonction toute seule
#On va couper iob.word en 49 parties équales (length=10.000) et 1 partie (length=7668)
#Ensuite une boucle pour ajouter le resultat dans un dataframe
#Cette étape va prendre environ 4 heures à executer

####On a décidé d'exporter le résultat en fichier txt pour économiser le temps####

#couper en 49 parties égales
index_row <- split(1:490000, ceiling(seq_along(1:490000)/10000))

#la derniere partie est le reste
index_row$`50` <- 490001:497668

#50

#pos tagging pour 10.000 premiers tokens
pos1 <- as.data.frame(udpipe_annotate(udmodel_english, 
                                      x = paste(a$x[index_row$`1`],collapse = '\n'),
                                      tokenizer = "vertical"))
#ind pour observer les itérations
ind <- 0
for (i in index_row[-1]){
  #pos tagging de la partie suivante 
  pos <- as.data.frame(udpipe_annotate(udmodel_english,
                                       x = paste(a$x[i],collapse = '\n'),
                                       tokenizer = "vertical"))
  #combiner pos1 et pos
  pos1 <- rbind(pos1,pos)
  
  ind <- ind + 1
  print(ind)
  print(nrow(pos1))
  # break
}

####Charger le fichier de POS pour économiser le temps####
txt.udpipe <- read.table("POS_tag.txt",header = TRUE,stringsAsFactors = FALSE)

#dataframe avec tokens, lemma, pos tags, iob tags
pos.df <- data.frame(word = txt.udpipe$token,
                    lemma = txt.udpipe$lemma, 
                    pos = txt.udpipe$upos, 
                    label = iob.label, stringsAsFactors = FALSE)

####Special Verb Trigger####
special_verb_trigger <- function(data, win_size){
  # Description: fonction qui permet de récupérer une table ordonnée des verbes 
  # les plus fréquents occurant au voisinage des entités nommées labélisées.
  # Input: 
  # data (data.frame): contient les lemma (forme lemmatisée des tokens), pos (part of speech) et label.
  # win_size (integer): taille de la fenêtre de voisinage du mot analysé.
  # Output:
  # table.verb (table): table ordonnée des special verb trigger.
  list.ind <- which(data$label != "O")
  reject_first_and_last <- c(c(-win_size:-1), c(-length(list.ind):-length(list.ind)-win_size))
  list.ind2 <- list.ind[reject_first_and_last]
  win = c(c(-win_size:-1), c(1:win_size))
  
  list.verb <- c()
  for(i in list.ind2){
    for(w in win){
      if( data$pos[i + w] == "VERB" && !is.na(data$pos[i + w] )){
        list.verb <- c(list.verb, as.character(data$lemma[i+w]))
      }
    }
  }
  
  table.verb <- sort(table(list.verb), decreasing = TRUE)
  return(table.verb)
}
spe.verb.trig <- special_verb_trigger(pos.df, 2)

#60 premieres verbes les plus fréquentes
head(spe.verb.trig, n = 60)


#### Word formation pattern####
#fonction attribuer code WFP
# wfp_tag <- function(word){
# t <- data.frame(word = a[1:300], label = b[1:300], stringsAsFactors = FALSE)

#liste des mots greecs
greek <- c("alpha","beta","gamma","delta","epsilon","zeta"
           ,"eta","theta","iota","kappa","lambda","mu","nu"
           ,"xi","omicron","pi","rho","sigma","tau","upsilon"
           ,"phi","chi","psi","omega")

#alphabet sans A, T, C , G
sequence <- LETTERS[! LETTERS %in% c("A","G","T","C")]

#creer un dataframe avec word/label IOB/tag WFP 
pos.df <- dplyr::mutate(
  # data.frame(word=c("ACGC",",",".","(","1,25","A","1","23525",
  #                   "II","0.31","The","Whereas","IgM",
  #                   "kDa","H2A","T4","6C2","19D","alpha"),stringsAsFactors = FALSE) ,
  pos.df,
  WFP =  
    dplyr::case_when(
      word  == ","  ~ "Comma",
      word  == "."  ~ "Dot",
      word  %in% c("(",")","[","]") ~ " Parenthesis",
      word  %in% 0:9 ~ "OneDigit",
      grepl("^[[:digit:]]+$", word) & nchar(word) > 1 ~ "AllDigits",
      word %in% LETTERS ~ "OneCap",
      tolower(word)  %in% tolower(stopwords(language = "en")) ~ "StopWord",
      !grepl(paste0(c(sequence,0:9),collapse = "|"),gsub("[[:punct:]]", "", toupper(word))) ~ "ATCGsequence",
      
      !is.na(as.roman(word)) & is.na(as.numeric(word)) ~ "RomanDigit",
      grepl("^[[:upper:]]+$", word) & nchar(word) > 1 ~ "AllCaps",
      
      tolower(word) %in% greek ~ "GreekLetter",
      grepl("^[[:digit:]]\\,.*[[:digit:]]$",word) ~ "DigitCommaDigit",
      grepl("^[[:digit:]]\\..*[[:digit:]]$",word) ~ "DigitDotDigit",
      
      grepl("^[[:upper:]].*[[:lower:]]$",word) ~ "CapLowAlpha",
      grepl("^[[:upper:]].*[[:lower:]].*[[:upper:]]$",word) ~ "CapMixAlpha",
      grepl("^[[:lower:]].*[[:upper:]].*[[:lower:]]$",word) ~ "LowMixAlpha",
      grepl("^[[:upper:]].*[[:digit:]].*[[:upper:]]$",word) ~ "AlphaDigitAlpha",
      grepl("^[[:upper:]].*[[:digit:]]$",word) ~ "AlphaDigit",
      grepl("^[[:digit:]].*[[:upper:]].*[[:digit:]]$",word) ~ "DigitAlphaDigit",
      grepl("^[[:digit:]].*[[:upper:]|[:lower:]]$",word) ~ "DigitAlpha",
      
      TRUE                     ~ "Others"
    ))

#les 100eres lignes de data_wfp
head(data_wfp, n = 100)




