# Extraction des features

#### POS tagging avec NLP####

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

#### On a décidé d'exporter le résultat en fichier txt pour économiser le temps####

# Chargement des variables générées dans le pré-traitement (nécessaire pour la suite)
iob.word <- read.table("iob_word.var")
iob.label <- read.table("iob_label.var")
doc_id <- read.table("doc_id.var")


#couper en paquets de 10000
seg_size <- 10000
nb_bloc <- nrow(doc_id)%/%seg_size
index_row <- split(1:(nb_bloc*seg_size), ceiling(seq_along(1:(nb_bloc*seg_size))/seg_size))
#la derniere partie est le reste
index_row[[nb_bloc+1]] <- ((nb_bloc+1)*seg_size):nrow(doc_id)


#pos tagging pour 10.000 premiers tokens
pos1 <- as.data.frame(udpipe_annotate(udmodel_english,
                                      x = paste(iob.word$x[index_row$`1`],collapse = '\n'),
                                      tokenizer = "vertical"))

#ind pour observer les itérations
ind <- 0
for (i in index_row[-1]){
  #pos tagging de la partie suivante 
  pos <- as.data.frame(udpipe_annotate(udmodel_english,
                                       x = paste(iob.word$x[i],collapse = '\n'),
                                       tokenizer = "vertical"))
  #combiner pos1 et pos
  pos1 <- rbind(pos1,pos)
  
  ind <- ind + 1
  print(ind)
  print(nrow(pos1))
  # break
}

####Charger le fichier de POS pour économiser le temps####
udpipe.data <- read.table("POS_tag.txt",header = TRUE,stringsAsFactors = FALSE)

#dataframe avec tokens, lemma, pos tags, iob tags
pos.df <- data.frame(word = udpipe.data$token,
                     lemma = udpipe.data$lemma, 
                     pos = udpipe.data$upos, 
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






