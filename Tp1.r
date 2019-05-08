#Auteurs: Vartazian Geoffrey  Martinez Thomas

#Import de librairie
library(ggplot2)
library("gridExtra")

#chargement de la base de donnée depuis le fichier donné
Database <- read.table("A:/Mes Documents/Telechargement/DataAnalysis-master/data1TP1.txt", header = TRUE)

####Question1###

# Nuage de points simples
A<-ggplot(Database, aes(x=A, y=Y)) + geom_point() +  geom_smooth(method=lm)
B<-ggplot(Database, aes(x=B, y=Y)) + geom_point() +  geom_smooth(method=lm)
C<-ggplot(Database, aes(x=C, y=Y)) + geom_point() +  geom_smooth(method=lm)
D<-ggplot(Database, aes(x=D, y=Y)) + geom_point() +  geom_smooth(method=lm)
E<-ggplot(Database, aes(x=E, y=Y)) + geom_point() +  geom_smooth(method=lm)
grid.arrange(A, B, C, D, E)

#Réponse

#On observe pour le A qu'il y a une décroissance linéaire et que pour B est l'opposé de A. De plus,
#on observe que E peut s'apparenter à une parabole, que D augmente d'un coup comme une fonction exponentielle

#####Question2###

#Fonction calcul de coef avec la méthode de Pearson
Coef_Pearson<-function(X,Y){
  covariance<- cov(X,Y)
  ecart<-sd(X)*sd(Y)
  rslt<-covariance/ecart
  rslt
}

#Test
Coef_Pearson(Database$A,Database$Y)
Coef_Pearson(Database$B,Database$Y)
Coef_Pearson(Database$C,Database$Y)
Coef_Pearson(Database$D,Database$Y)
Coef_Pearson(Database$E,Database$Y)
#cor(Database$A,Database$Y)

#Réponse

# La plus petite est celle de A car elle est négative, 
# Cela peut s'expliquer grace a la regression linéaire que nous avons tracé précedemment qui est décroissante

####Question3###

#Fonction calcul de coef avec la méthode de Spearman
Coef_Spearman<-function(X,Y){
  denominateur<-(15^3)-15
  sum<-0
  rgX<-rank(X)
  rgY<-rank(Y)
  for (i in (1:15)) {
    rang<-rgX[i]-rgY[i]
    sum<-sum+(rang^2)
  }
  numerateur<-6*sum
  rslt<-1-(numerateur/denominateur)
  rslt
}

#Test
Coef_Spearman(Database$A,Database$Y)
Coef_Spearman(Database$B,Database$Y)
Coef_Spearman(Database$C,Database$Y)
Coef_Spearman(Database$D,Database$Y)
Coef_Spearman(Database$E,Database$Y)
#cor(Database$A,Database$Y,method="spearman")

#Réponse



####Question4###

# On peut définir un polynome ou une relation entre plusieurs variables pour décrire la relation non-linéaire et non-monotone entre E et Y
# Nous pouvons alors utiliser une transformation ou alors une fonction non-linéaire afin de calculer cette relation

#####################

#Partie2

#####Question5###
#Chargement des données
Student <- read.table("A:/Mes Documents/Telechargement/DataAnalysis-master/data2TP1.txt", header = TRUE)

#Calcul le score pour une variable quantitative
independance<-function(X){
  moy<-mean(X)
  ecart<-sd(X)
  rslt<-abs(moy-19)/(ecart/sqrt(length(X)))
  rslt
}

#résultat
independance(Student$Marseille)

#Réponse

#Nous prenons comme Hypothèse H0 que l'inflation n'affecte pas le cout
# Nous trouvons comme résultat 2.177
# Or la valeur théorique du tableau avec un degré de liberté de 14 
# pour un intervalle de confiance de 95% est 2.145
# Le score trouvé est supérieur à la valeur théorique donc on rejette l'hypothèse H0
# Donc en conclusion l'inflation  a eu un impact sur le cout de la vie

###Question6###
#Fonction qui calcule le score pour deux variables quantitatives
independance2<-function(X,Y){
  moyX<-mean(X)
  moyY<-mean(Y)
  ecartX<-sd(X)
  ecartY<-sd(Y)
  rslt<-abs(moyX-moyY)/sqrt(((ecartX^2)/length(X))+((ecartY^2)/length(Y)))
  rslt
}

#résultat
independance2(Student$Marseille,Student$Aix)

#Réponse

# Hypothèse H0: Il y a une dépendance significative entre Aix et Marseille
# Degré de liberté: 28
# Toujours un intervalle de confiance de 95%
# Ici la valeur théorique serait de 2.048
# Nous trouvons 2.32 donc la valeur étant supérieur, il n'y a pas de dépendance entre les deux villes
# Cependant si nous avions pris un intervalle de 98%, le résultat théorique aurait été 2.493
# Donc notre résultat aurait été inférieur à la valeur théorique et notre hypothèse de départ aurait été validé


###Question7###


#pour chaque categorie: (ratio/somme ratio) * somme fleurs observÃ©
#Calcul théorique
val1<-(9/16)*(1528+106+117+381)
val2<-(3/16)*(1528+106+117+381)
val3<-(3/16)*(1528+106+117+381)
val4<-(1/16)*(1528+106+117+381)
#Résultat
val1
val2
val3
val4
#Matrice contenant les résultats
data<-cbind(c(1528,106,117,381), c(val1,val2,val3,val4))

#Fonction Khi2
khi2<-function(data){
  sum<-0
  for (i in 1:4) {
    sum<-sum+(((data[i,1]-data[i,2])^2)/data[i,2])
  }
  sum
}
#Résultat
khi2(data)

#Réponse
# Hypothèse0: le ratio est 9,3,3,1 pour les plantes
# Degré de liberté: 
# Le rÃ©sultat du khi 2 est 966,61 est trÃ¨s supÃ©rieur a la valeur du tableau khi deux 7.81.
# On en conclut donc que l'hypothÃ¨se H0 n'est pas valide et donc le ratio n'est pas 9:3:3:1


###Question 8###
#Calcul théorique
vt1<-((29+5+46)*(29+40+18))/200
vt2<-((40+32+8)*(29+40+18))/200
vt3<-((18+22+0)*(29+40+18))/200
vt4<-((29+5+46)*(5+32+22))/200
vt5<-((40+32+8)*(5+32+22))/200
vt6<-((18+22+0)*(5+32+22))/200
vt7<-((29+5+46)*(46+8))/200
vt8<-((40+32+8)*(46+8))/200
vt9<-((18+22+0)*(46+8))/200

#Matrice contenant les résultats
absent<-cbind(c(29,40,18), c(vt1,vt2,vt3))
atypique<-cbind(c(5,32,22), c(vt4,vt5,vt6))
typique<-cbind(c(46,8,0), c(vt7,vt8,vt9))

#Fonction Khi2
khi2<-function(data){
  sum<-0
  for (i in 1:3) {
    sum<-sum+(((data[i,1]-data[i,2])^2)/data[i,2])
  }
  sum
}
#Résultat Forme
khi2(absent)
khi2(atypique)
khi2(typique)
khi2forme<-khi2(absent)+khi2(atypique)+khi2(typique)
khi2forme

#Calcul théorique
vt1<-((20+60)*(20+29+12))/200
vt2<-((29+51)*(20+29+12))/200
vt3<-((12+28)*(20+29+12))/200
vt4<-((20+60)*(60+51+28))/200
vt5<-((29+51)*(60+51+28))/200
vt6<-((12+28)*(60+51+28))/200

#Matrice qui contient les données
absent<-cbind(c(20,29,12), c(vt1,vt2,vt3))
present<-cbind(c(60,51,28), c(vt4,vt5,vt6))

#Résultat Couleur
khi2(absent)
khi2(present)
khi2couleur<-khi2(absent)+khi2(present)
khi2couleur


# Réponse
# H0: deux variables sont indépendantes
# Forme:
# Nous trouvons en pratique 75.2 ce qui est supérieur a la valeur théorique Donc
# Cela ne vérifie pas H0, elle est dépendante, elle est donc importante pour détecter un mélanome

# Couleur:
# Nous trouvons en pratique 2.39 ce qui est inférieur a la valeur théorique Donc
# Cela vérifie H0, donc elle est indépendante, elle n'est donc pas importante pour détecter un mélanome

###Question9###

# Un test paramétrique repose sur une distribution statistique supposée dans les données. 
# On doit donc vérifier les conditions de validité pour que le test soit fiable. 
# Avec les réponses que nous avons trouvées, on peut en conclure que le test Student/t n'est fiable que si les
# données associées à chaque échantillon suivent une distribution normale et si les variances des échantillons sont homogènes. 
# Il est donc parametrique.
# Les tests non-paramétriques, au contraire ne repose pas sur le même principe.
# On peut donc s'en servir même si on ne vérifie pas les conditions de validités des tests paramétriques.
# Le test du khi2 consiste à comparer des proportions d'échantillons indépendants. Or, on ne peut pas les considérer comme paramètres
# Ce test est donc non-paramétrique.
# Le test Student/t repose sur des valeurs concrètes et donc quantitatives. 
# Il ne sera donc pas possible d'appliquer ce processus de test à des données qualitatives


###Question10###

# On ne peut pas attribuer une valeur ou une caractéristique à une donnée qualitatives (d'où son nom)
# On peut prendre en considération le fait qu'une donnée qui n'est pas quantitative peut être considérer comme qualitative
# On sait que le coefficient de Pearson permet d'analyser les relations linéaires et le coefficient de Spearman les relations non-linéaires monotones, ces 
# deux méthodes reposant sur l'utilisation de la corrélation
# La définition même de la corrélation repose sur le lien entre plusieurs données caractéristiques et donc quantitative
# Les données étant quantitative, elles ne peuvent donc pas être qualitatives.
# On ne peut donc pas les utiliser sur des données qualitatives.