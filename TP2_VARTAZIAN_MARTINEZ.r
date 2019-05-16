#Auteurs: Vartazian Geoffrey Martinez Thomas

#import de librairie
library("gridExtra")
library("plot3D")

#chargement des données depuis le fichier texte
data <- read.table("/amuhome/m17013118/Documents/INFO/4A/Analyse Donnée/DataAnalysis-master/data1TP2.txt", header = TRUE)
summary(data)


A <- matrix(nrow=length(data$Poids), ncol=3)

for (i in (1:10)){
  for (j in 1:3){
    A[i,j] <- data[i,j]
  }
}

####Question1####
#On trace les coordonnées contenues dans le fichiers en 3D selon les variables
scatter3D(data$Stature,data$Poids,data$Taille,add=FALSE)

####Question2####
#On calcule la moyenne selon chaque variable pour définir le tableau centrée
moyStature <- data$Stature -(mean(data$Stature))
moyPoids <-data$Poids -(mean(data$Poids))
moyTaille <-data$Taille -(mean(data$Taille))

#Tableau centrée B
B <-cbind(moyStature,moyPoids,moyTaille)
print(B)

#Matrice de covariance
matriceCov<-cov(cbind(moyStature,moyPoids,moyTaille))
print(matriceCov)

####Question3####
x = eigen(matriceCov)
x$values # valeurs propres
x$vectors # vecteurs propres

####Question4####
#Les axes principaux sont  des vecteurs propres qui correspondent à la colonne stature, poids,taille
#Ici, ils sont déjà ordonnées selon leur importance au vu du résultat du calcul des valeurs propres

####Question5####
C<-B %*% x$vectors
C
#On vérifie que le résultat est cohérent à l'aide de princomp
verif<-princomp(A)$scores
verif

####Question6####
scatter3D(C[,1],C[,2],C[,3],add=FALSE)
scatter3D(x=c(0,-300*x$vectors[1,1]),y=c(0,-300*x$vectors[2,1]),z=c(0,-300*x$vectors[3,1]),add=TRUE,type='l')

####Question7####
plot(C[,1],C[,2])

###Question8###
#Les résultats obtenus après ces différents calculs semblent cohérent, car on retrouve bien la variable stature comme étant la plus importante
#avec la représentation du nuage de points en 2D