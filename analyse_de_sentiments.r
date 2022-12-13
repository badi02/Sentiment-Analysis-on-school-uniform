
#1/ Extraction de donn?es avec l outil data scraper 

#Charger les librairies 

library("tm")
library("SnowballC") #text stemming
library("wordcloud")
library("RColorBrewer")
library("slam")

install.packages("pacman")
pacman::p_load(tm)

install.packages("slam")
install.packages(tm)
install.packages(SnowballC)
install.packages(wordcloud)
install.packages(RColorBrewer)

#2/ Chargement du texte

#On charge notre fichier de commentaires en utilisant 

#Le texte peut ?tre charg? en utilisant la fonction Corpus() du package tm. Corpus est une liste de 
#documents (dans notre cas, nous avons juste un seul fichier).

##########################
# Lire le fichier texte
filePath <- "uniforme.csv"
text <- readLines(filePath)
# Charger les donn?es comme un corpus (ensemble de documents)
#La fonction VectorSource() se charge de la cr?ation du corpus de textes (ensemble de vecteurs de textes)
docs <- Corpus(VectorSource(text))
#consultation du document 
inspect(docs)
##########################

##########################
#3/ Nettoyage du texte

#Le nettoyage du texte est effectu? en utilisant la fonction tm_map() pour remplacer, par exemple, 
#des caract?res sp?ciaux non utiles.
#Remplacer "/", "@" et "|" avec un espace



toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#consultation du document 
inspect(docs)

# La fonction tm_map() est utilis?e pour supprimer les espaces inutiles, pour convertir le texte en 
# minuscules, supprimer les "mots vides" (stopwords en anglais). Il s'agit des mots tr?s courants dans 
# une langue comme "le", "la", "nous", "et", etc.
# La valeur de l'information de ces "mots vides" est proche de z?ro en raison du fait qu'ils sont si communs
# dans une langue. La suppression de ce genre de mots est utile avant de poursuivre une analyse plus approfondie.
# Pour la suppression de ces mots vides, les langues support?es sont: danish, dutch, english, finnish, 
# french, german, hungarian, italian, norwegian, portuguese, russian, spanish et swedish. Le nom des
# langues est sensible ? la casse. Je vais aussi vous montrer comment faire pour supprimer votre propre liste de mots du texte.
# 
# Vous pouvez ?galement supprimer des chiffres et ponctuations avec les arguments removeNumbers et removePunctuation.
# 
# Une autre ?tape importante de pr?paration du texte est de faire du texte stemming. Ce processus consiste ? r?duire les mots ? leurs racines. 
# En d'autres termes, ce processus supprime les suffixes des mots pour les rendre simples et pour obtenir l'origine commune. 
# Par exemple, le text stemming va r?duire les mots "partir", "partant", "partons" ? la racine "partir".


# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les nombres
docs <- tm_map(docs, removeNumbers)
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, c("la", "je","suis","ca","va","être","car","veux","à","mon","étant",
            "une","on","doit","lui","plus","en","tout","des","de","d","les","du","qui","au","est","nous","parce","que",
            "très","le","après","celle","notre","d","sur","ca","me","ses","qui","dans","a")) 
# Supprimer les espaces vides suppl?mentaires
docs <- tm_map(docs, stripWhitespace)



# Text stemming
docs <- tm_map(docs, stemDocument)

#docs <- tm_map(docs, removeWords, stopwords("French"))


# Supprimer votre propre liste de mots non d?sir?s
# docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 



#consultation du document 
inspect(docs)

#4/ La matrice des mots

# La matrice des mots (term-documents matrix) est une table contenant la fr?quence des mots. 
# La fonction TermDocumentMatrix() du package text mining 




dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE) 
d <- data.frame(word = names(v),freq=v)
head(d, 10) #on affiche que les 10 mots les plus fr?quencts 






#5/ G?n?ration du nuage de mots 

#L'importance des mots peut ?tre illustr?e par un nuage de mots



  
set.seed(123)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=123, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))





# Le nuage de mots ci-dessus montre clairement que les mots "Will", "freedom", "dream", "day" et
# "together" sont les cinq mots les plus importants dans le texte "I have a dream" de Martin Luther King.


#6/ Exploration des mots fr?quents ainsi que leurs associations



findFreqTerms(dtm, lowfreq = 4) #les mots qui sont fr?quents au moins 4 fois dans le texte 



# On analyse l'association entre les mots (leur corr?lation) en utilisant la fonction
# findAssocs(). on identifie ici les mots qui sont le plus fr?quemment associ?s ?
#"MOT" dans notre texte.

findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "luniforme", corlimit = 0.3)


findAssocs(dtm, terms = "MOT", corlimit = 0.3) #####ici change 









#La matrice des mots les plus fr?quents 
head(d, 10)

#Repr?sentation des mots les plus fr?quents 


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")




#Interpr?tation 


#Conclusion 