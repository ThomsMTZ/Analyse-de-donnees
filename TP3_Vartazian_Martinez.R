#AUTEUR:
#Vartazian Geoffrey
#Martinez Thomas

#librairie
library(ggplot2)
####QUestion 1 #####
# Créa tion d'une matrice de 300 ligne et de 2 colonne rempli de 0
# Et création d'un tableau contenant les couleurs
matrice<-matrix(0,nrow = 300,ncol=2)
couleur<-matrix(0,nrow = 300,ncol=1)
# Remplacement de la 1ere partie du tableau par des variables x et y uniformes sur [0,1]
matrice[1:100,1:2]<-runif(200, min=0, max=1)
couleur[1:100]<-"blue"
# Remplacement des valeurs des 200 lignes suivantes par des variables x et y gaussiennes independantes 
# en respectant les valeurs des moyennes et variances (variance = ecart type au carré 
# il faut donc modifier en conséquence)
matrice[101:200,1]<-rnorm(100,mean=4,sd=1)
couleur[101:200]<-"red"
matrice[101:200,2]<-rnorm(100,sd=1)
couleur[201:300]<-"green"
matrice[201:300,1]<-rnorm(100,mean=0.5,sd=sqrt(2))
matrice[201:300,2]<-rnorm(100,mean=6,sd=sqrt(2))
# Affichage du tableau en montrant bien par le biai de couleur les différentes parties
plot(matrice,col=couleur)

#Question_2
# On doit faire une matrice de distance des points 2 a 2
# (matrice carree) pour regrouper les points entre eux et obtenir des classes de points, ici 3 classes

nuage<-matrice
identite <- diag(300)
# On calcule les distances entre les points pour l'agencement des classes
cl<-dist(matrice, method = "euclidean", diag = FALSE, upper = FALSE)
tableau<-as.matrix(cl)

# On arrete la classification lorsqu'on a les 3 classes
while(nrow(tableau)>3){
# On trouve les points avec la plus petitedistance 
  min_dist<-which(tableau==min(cl),arr.ind = T)
  min1=min_dist[1,1]
  min2=min_dist[1,2]
#On calcule les coordonnees du barycentre de la nouvelle classe
  matrice[min1,1]<-(matrice[min1,1]+matrice[min2,1])/2
  matrice[min1,2]<-(matrice[min1,2]+matrice[min2,2])/2
# On met à jour la matrice des distances pour tenir compte de cette classe et supprimer les precedentes
  identite[min1,]<-identite[min1,]+identite[min2,]
  identite<-identite[-min2,]
  matrice<-matrice[-min2,]
# On recalcule les distances avec la nouvelle classe
  cl<-dist(matrice, method = "euclidean", diag = FALSE, upper = FALSE)
  tableau<-as.matrix(cl)
}


#Question_3
# Cette fonction sert à definir des couleurs pour rendre visibles les 3 classes de points
identite<-t(identite)
vect_col<-c(1:300)
for (i in 1:nrow(identite)){
  if(identite[i,1]==1){
    vect_col[i]="blue"
  }
  if(identite[i,2]==1){
    vect_col[i]="red"
  }
  if(identite[i,3]==1){
    vect_col[i]="orange"
  }
}
# On realise ici l'affichage des classes
plot(nuage,  col = vect_col)
points(matrice, pch=19)
