####Construire dataframe de affix base de label
#input
#    a: tous les token
#    b: labell de token
#output
#   un vector de suffix qui est efficacite
construt_suffix<-function(a,b){
  df_af <- data.frame(word=a,label=str_replace_all(b,"[BI-]",""),stringsAsFactors = FALSE)
  #si le nombre de character du mot >=5 on fait le substr, ici j'ai essayé avec 4 gram de suffix et prefix
  suf5 <- ifelse(nchar(df_af$word)>=5 ,str_sub(df_af$word, -5),'non_non')
  ####Construire dataframe suffix(5 grams) avec weight####
  df_suf<-data.frame(suf5,df_af$label,stringsAsFactors = FALSE)
  #enlever le mot moins 5 caractere et suffix contient '-'
  df_suf<-df_suf[df_suf$suf5 != "non_non" & is.na(str_extract(df_suf$suf5, "-"))==TRUE,]
  #compte frequence de suffix
  F_suf<-as.data.frame(table(df_suf))
  #mettre ordre descendre
  F_suf<-F_suf[order(-F_suf$Freq),]
  #chercher 100 plus frequence suffix qui est entitee
  F_suf_100<-F_suf[which(F_suf$df_af.label!="O"),]
  suf_100<-unique(F_suf_100$suf5)[1:100]
  #calculer weight par formule (in[]-out[])/(in[]+out[])
  weight_suf<-rep(0, 100)
  j<-1
  for(i in suf_100){
    nb_in<-sum(F_suf$Freq[which(F_suf$suf5==i & F_suf$df_af.label!="O")])
    nb_out<-sum(F_suf$Freq[which(F_suf$suf5==i & F_suf$df_af.label=="O")])
    weight_suf[j]<- (nb_in - nb_out)/(nb_in + nb_out)
    j<-j+1
  }
  df_weight_suf<-data.frame(suf_100,weight_suf)
  #on prend weight superieur 0.5
  df_weight_suf<-df_weight_suf[weight_suf>=0.7,]
  #construire comme dictionary de labels
  return(df_weight_suf[,1])
}

####Construire dataframe de préfixes de base des label
#input
#    a: tous les token
#    b: label de token
#output
#   un vector de prefix qui est efficacite 
construt_prefix<-function(a,b){
  df_af <- data.frame(word=a,label=str_replace_all(b,"[BI-]",""),stringsAsFactors = FALSE)
  #si le nombre de character du mot >=5 on fait le substr, ici j'ai essayé avec 4 gram de suffix et prefix
  pre4 <- ifelse(nchar(df_af$word)>=5 ,str_sub(df_af$word, 1,4),'non_non')
  ####Construire dataframe prefix(4 grams) avec weight####
  df_pref<-data.frame(pre4,df_af$label,stringsAsFactors = FALSE)
  df_pref<-df_pref[df_pref$pre4 != "non_non" & is.na(str_extract(df_pref$pre4, "-"))==TRUE,]
  F_pref<-as.data.frame(table(df_pref))#compte frequence de suffix
  F_pref<-F_pref[order(-F_pref$Freq),]
  F_pref_100<-F_pref[which(F_pref$df_af.label!="O"),]
  pref_100<-unique(F_pref_100$pre4)[1:100]
  weight_pref<-rep(0, 100)
  j<-1
  for(i in pref_100){
    nb_in<-sum(F_pref$Freq[which(F_pref$pre4==i & F_pref$df_af.label!="O")])
    nb_out<-sum(F_pref$Freq[which(F_pref$pre4==i & F_pref$df_af.label=="O")])
    weight_pref[j]<- (nb_in - nb_out)/(nb_in + nb_out)
    j<-j+1
  }
  df_weight_pref<-data.frame(pref_100,weight_pref)
  df_weight_pref<-df_weight_pref[weight_pref>=0.7,]
  return(df_weight_pref[,1])
}

###find affix par word###
#input
#    words:word token
#    pref:un vector de prefix
#    suf:un vector de suffix
#output
#    un dataframe 3 colonne (word,prefix,suffix)
#    si le prefix/suffix de word est dans le vector prefix/suffix, on mets TRUE dans cette colonne
#    sion FALSE
find_Affix<-function(words,pref,suf){
  require(hash)
  h<-hash()
  uniq_word<-unique(words)
  for(w in uniq_word){
    if(nchar(w)<5) .set(h, keys = w, values = rep(FALSE,2))
    else{
      w_suf<-w %>%
        str_sub(-5)
      w_pref<-w %>%
        str_sub(1,4)

      if(length(grep(w_suf, suf))==0) wsuf <- FALSE
      else wsuf <- TRUE#suf[grep(w_suf,suf[,1]),2]
      if(length(grep(w_pref, pref))==0) wpref<-FALSE
      else wpref<-TRUE#suf[grep(w_pref,pref[,1]),2]
      .set(h, keys = w, values = c(as.character(wpref),as.character(wsuf)))
    }
  }
  df_af<-foreach(w=words,.combine = "rbind")%do% c(w,values(h, w))
  
  colnames(df_af)<-c("word","prefix","suffix")
  return(df_af)
}

