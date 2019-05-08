#Auteurs: Vartazian Geoffrey  Martinez Thomas

#Import de librairie
library(ggplot2)
library("gridExtra")

#chargement de la base de donn�e depuis le fichier donn�
Database <- read.table("A:/Mes Documents/Telechargement/DataAnalysis-master/data1TP1.txt", header = TRUE)

####Question1###

# Nuage de points simples
A<-ggplot(Database, aes(x=A, y=Y)) + geom_point() +  geom_smooth(method=lm)
B<-ggplot(Database, aes(x=B, y=Y)) + geom_point() +  geom_smooth(method=lm)
C<-ggplot(Database, aes(x=C, y=Y)) + geom_point() +  geom_smooth(method=lm)
D<-ggplot(Database, aes(x=D, y=Y)) + geom_point() +  geom_smooth(method=lm)
E<-ggplot(Database, aes(x=E, y=Y)) + geom_point() +  geom_smooth(method=lm)
grid.arrange(A, B, C, D, E)

#R�ponse

#On observe pour le A qu'il y a une d�croissance lin�aire et que pour B est l'oppos� de A. De plus,
#on observe que E peut s'apparenter � une parabole, que D augmente d'un coup comme une fonction exponentielle

#####Question2###

#Fonction calcul de coef avec la m�thode de Pearson
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

#R�ponse

# La plus petite est celle de A car elle est n�gative, 
# Cela peut s'expliquer grace a la regression lin�aire que nous avons trac� pr�cedemment qui est d�croissante

####Question3###

#Fonction calcul de coef avec la m�thode de Spearman
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

#R�ponse



####Question4###

# On peut d�finir un polynome ou une relation entre plusieurs variables pour d�crire la relation non-lin�aire et non-monotone entre E et Y
# Nous pouvons alors utiliser une transformation ou alors une fonction non-lin�aire afin de calculer cette relation

#####################

#Partie2

#####Question5###
#Chargement des donn�es
Student <- read.table("A:/Mes Documents/Telechargement/DataAnalysis-master/data2TP1.txt", header = TRUE)

#Calcul le score pour une variable quantitative
independance<-function(X){
  moy<-mean(X)
  ecart<-sd(X)
  rslt<-abs(moy-19)/(ecart/sqrt(length(X)))
  rslt
}

#r�sultat
independance(Student$Marseille)

#R�ponse

#Nous prenons comme Hypoth�se H0 que l'inflation n'affecte pas le cout
# Nous trouvons comme r�sultat 2.177
# Or la valeur th�orique du tableau avec un degr� de libert� de 14 
# pour un intervalle de confiance de 95% est 2.145
# Le score trouv� est sup�rieur � la valeur th�orique donc on rejette l'hypoth�se H0
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

#r�sultat
independance2(Student$Marseille,Student$Aix)

#R�ponse

# Hypoth�se H0: Il y a une d�pendance significative entre Aix et Marseille
# Degr� de libert�: 28
# Toujours un intervalle de confiance de 95%
# Ici la valeur th�orique serait de 2.048
# Nous trouvons 2.32 donc la valeur �tant sup�rieur, il n'y a pas de d�pendance entre les deux villes
# Cependant si nous avions pris un intervalle de 98%, le r�sultat th�orique aurait �t� 2.493
# Donc notre r�sultat aurait �t� inf�rieur � la valeur th�orique et notre hypoth�se de d�part aurait �t� valid�


###Question7###


#pour chaque categorie: (ratio/somme ratio) * somme fleurs observé
#Calcul th�orique
val1<-(9/16)*(1528+106+117+381)
val2<-(3/16)*(1528+106+117+381)
val3<-(3/16)*(1528+106+117+381)
val4<-(1/16)*(1528+106+117+381)
#R�sultat
val1
val2
val3
val4
#Matrice contenant les r�sultats
data<-cbind(c(1528,106,117,381), c(val1,val2,val3,val4))

#Fonction Khi2
khi2<-function(data){
  sum<-0
  for (i in 1:4) {
    sum<-sum+(((data[i,1]-data[i,2])^2)/data[i,2])
  }
  sum
}
#R�sultat
khi2(data)

#R�ponse
# Hypoth�se0: le ratio est 9,3,3,1 pour les plantes
# Degr� de libert�: 
# Le résultat du khi 2 est 966,61 est très supérieur a la valeur du tableau khi deux 7.81.
# On en conclut donc que l'hypothèse H0 n'est pas valide et donc le ratio n'est pas 9:3:3:1


###Question 8###
#Calcul th�orique
vt1<-((29+5+46)*(29+40+18))/200
vt2<-((40+32+8)*(29+40+18))/200
vt3<-((18+22+0)*(29+40+18))/200
vt4<-((29+5+46)*(5+32+22))/200
vt5<-((40+32+8)*(5+32+22))/200
vt6<-((18+22+0)*(5+32+22))/200
vt7<-((29+5+46)*(46+8))/200
vt8<-((40+32+8)*(46+8))/200
vt9<-((18+22+0)*(46+8))/200

#Matrice contenant les r�sultats
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
#R�sultat Forme
khi2(absent)
khi2(atypique)
khi2(typique)
khi2forme<-khi2(absent)+khi2(atypique)+khi2(typique)
khi2forme

#Calcul th�orique
vt1<-((20+60)*(20+29+12))/200
vt2<-((29+51)*(20+29+12))/200
vt3<-((12+28)*(20+29+12))/200
vt4<-((20+60)*(60+51+28))/200
vt5<-((29+51)*(60+51+28))/200
vt6<-((12+28)*(60+51+28))/200

#Matrice qui contient les donn�es
absent<-cbind(c(20,29,12), c(vt1,vt2,vt3))
present<-cbind(c(60,51,28), c(vt4,vt5,vt6))

#R�sultat Couleur
khi2(absent)
khi2(present)
khi2couleur<-khi2(absent)+khi2(present)
khi2couleur


# R�ponse
# H0: deux variables sont ind�pendantes
# Forme:
# Nous trouvons en pratique 75.2 ce qui est sup�rieur a la valeur th�orique Donc
# Cela ne v�rifie pas H0, elle est d�pendante, elle est donc importante pour d�tecter un m�lanome

# Couleur:
# Nous trouvons en pratique 2.39 ce qui est inf�rieur a la valeur th�orique Donc
# Cela v�rifie H0, donc elle est ind�pendante, elle n'est donc pas importante pour d�tecter un m�lanome

###Question9###

# Un test param�trique repose sur une distribution statistique suppos�e dans les donn�es. 
# On doit donc v�rifier les conditions de validit� pour que le test soit fiable. 
# Avec les r�ponses que nous avons trouv�es, on peut en conclure que le test Student/t n'est fiable que si les
# donn�es associ�es � chaque �chantillon suivent une distribution normale et si les variances des �chantillons sont homog�nes. 
# Il est donc parametrique.
# Les tests non-param�triques, au contraire ne repose pas sur le m�me principe.
# On peut donc s'en servir m�me si on ne v�rifie pas les conditions de validit�s des tests param�triques.
# Le test du khi2 consiste � comparer des proportions d'�chantillons ind�pendants. Or, on ne peut pas les consid�rer comme param�tres
# Ce test est donc non-param�trique.
# Le test Student/t repose sur des valeurs concr�tes et donc quantitatives. 
# Il ne sera donc pas possible d'appliquer ce processus de test � des donn�es qualitatives


###Question10###

# On ne peut pas attribuer une valeur ou une caract�ristique � une donn�e qualitatives (d'o� son nom)
# On peut prendre en consid�ration le fait qu'une donn�e qui n'est pas quantitative peut �tre consid�rer comme qualitative
# On sait que le coefficient de Pearson permet d'analyser les relations lin�aires et le coefficient de Spearman les relations non-lin�aires monotones, ces 
# deux m�thodes reposant sur l'utilisation de la corr�lation
# La d�finition m�me de la corr�lation repose sur le lien entre plusieurs donn�es caract�ristiques et donc quantitative
# Les donn�es �tant quantitative, elles ne peuvent donc pas �tre qualitatives.
# On ne peut donc pas les utiliser sur des donn�es qualitatives.