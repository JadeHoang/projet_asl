# projet_asl
Projet Advanced Supervised Learning Master 2 Datamining

On a 5 fichiers principaux ：Data_pretraitement.R，feature_extraction.R ，CRF_experiments.R ， HMM_experiments.R ，NB_experiments.R

1 étape ：exécuter Data_pretraitement.R

2 étape ：exécuter feature_extraction.R（on a déjà sauvegardé tous les data dans un fichier data）

3 étape ：exécuter CRF_experiments.R ou HMM_experiments.R ou NB_experiments.R pour lé 2 modèlé CRF, NB et HMM.

##Data_pretraitement.R
Ce script de code contient le processus de pré-traitement du corpus GENIA de sorte à créer le dictionaire des termes qui viennent dé classes : DNA , PROTEIN , RNA , CELL TYPE, CELL LINE. A partir du dictionnaire, on a réussi à créer les labels sous forme BIO pour tous lé tokens du corpus.

##feature_extraction.R
Ce script de code contient les étapes d'extraire les descripteurs: Word Formation pattern, Special Verb Trigger, Part-of-speech et Affixes. Le résultat obtenu est un dataframe avec tous les features et leurs fenêtres (-1 et +1)

##CRF_experiments.R | NB_experiments.R | HMM_experiments.R
Ce sont les fichiers qui contiennent les fonctions à entraîner le classifieur avec les modèles CRF, NB et HMM.

##Fichier ***data***
Il existe un fichier ***data*** qui stocke dedans des fichiers .RData ou .var.
- affixes.var : un dataframe qui contient les informations sur les préfixes/suffixes.
- full_features.RData : un dataframe avec tous les features et leurs fenêtres.
- full_features_wo_win.RData : un dataframe avec tous les features sans leurs fenêtres.
- POS_tag.txt : fichier txt des annotations de POS (comme il passe beaucoup de temps à annoter les POS donc on l'a sauvergardé pour économiser le temps)
- word_label_id.RData : Le résultat après avoir exécuter Data_pretraitement.R (comme il passe beaucoup de temps à exécuter, on a sauvegarder les tokens, leurs libellés et le numéro de document qu'il convient)

##Fichier ***old_var***
Même objectif que celui de ***data*** mais il est l'ancienne version de ce dernier.

##Fichier GENIA_term_3.02
Fichier contient le fichier xml du corpus GENIA.

Remarque : il passe beaucoup de temp pour notre HMM, on sauvegarde les variables observations et epmatrix dans fichier data/full_features.RData     
