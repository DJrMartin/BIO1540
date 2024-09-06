##########################################
# TP n°1 : initation à R et Test du Chi2 #
##########################################
# Durée : 1h30

# Dans ce premier TP, votre objectif sera de déterminer le nombre d'heures de sports à réaliser
# par semaine pour limiter le risque de développer une pathologie cardiovasculaire. 

# L'objetif sous jacent de ce TP est de se familiriser avec la simmulation des données par conséquent 
# toutes les conclusions seront scienfitiquement infondées. 

# Pour avoir accès à des résultats scientifiques fondés sur ce sujet, je vous invite à lire la review suivante :
# https://www.ahajournals.org/doi/full/10.1161/JAHA.115.002495.

# Votre objectif est donc de déterminer un seuil concernant la pratique d'AP.

#####################
# Partie simulation #
#####################

# 0) Définir une seed à 12 avec la fonction set.seed()
set.seed(2)

# 1) Définir un nombre de sujets à 200 que vous enregistrerez dans un objet n.
n <- 200

# 2) Simuler pour les n sujets un nombre d'heures de sport par semaine.
# grâce à une loi de poisson avec un lamdba de 4.2. Nous ajouterons un bruit gaussien.
PA <- rpois(n, 4.2)+rnorm(n, 0, 1)

# 3) Quelle type de variable ?

# 4) Visualiser l'hétérogénéité de la variable activité physique, i.e. PA.

# 5) Définir un risque de développer une pathologie cardiovasculaire. 
# La variable Risk comprendra 2 modalités ("Pas de risque" et "risque").
# Vous devrez la créer à partir d'une loi binomiale où la probabilité de ne pas développer de risque (i.e. Pas de risque = 1)
# Sera proportionnelle au niveau d'activité physique. 

# Utiliser la fonction rbinom() pour introduire une probabilité de développé une pathologie cardiaque | nb heures de sport.
Risk <- NULL
for(i in 1:n){
  Risk <- c(Risk, rbinom(1, 1, PA[i]/10+(0.1)))
}
# Renommer la variable Risk (R) pour que le 1 soit égal à pas de risque.
# Renommer la variable Risk (R) pour que le 0 soit égal à risque.
Risk[Risk==1]="Pas_de_risque" ; Risk[Risk==0]="risque"

# 6) Quelle type de variable ?

###################
# Partie solution #
###################

# 7) Visualiser les données ensemble.
boxplot(PA~Risk)

# 8) Quelle est la probabilité d'avoir un risque de développer une pathologie cardiaque en fonction de la pratique du sport ?
# 8) a) Réaliser une boucle pour nb_PA étant le nombre d'heure de sport allant de 1 à 8 heures par semaine 
# pour déterminer la probabilité d'avoir un risque de développer une pathologie cardiaque.
P_risk = NULL
for (nb_PA in 1:8){
  P_risk <- c(P_risk, table(data.frame(Risk, PA>=nb_PA))[2,2]/sum(table(data.frame(Risk, PA>=nb_PA))[,2]))
}
# 8) b) Réaliser un graphique de la prababilité du risque en fonction de la pratique d'activité physique.
plot(P_risk~c(1:8), ylim=c(0,1))

# 9) Quel seuil pourrions vous définir ?

# 10) Utiliser le test du CHI2 pour vous aider à prendre une décision. Définir les hypothèses du test ? 
# À quelle réponse serions nous capable de répondre ?
# --> Est-ce que la proportion de sujet actif (seuil choisi) est la même dans le groupe "risque" que dans le groupe "pas de risque" ? 
# Cette question porte sur la relation entre les deux variables.

# 10) a) Réaliser une boucle pour nb_PA étant le nombre d'heure de sport allant de 1 à 8 heures par semaine 
# pour déterminer la statistique de test et la p value du test du CHI2
p.value = statistique = NULL
for (nb_PA in 1:8){
  statistique <- c(statistique, chisq.test(table(data.frame(Risk, PA>=nb_PA)))$statistic)
  p.value <- c(p.value, chisq.test(table(data.frame(Risk, PA>=nb_PA)))$p.value)
}

# 10) b) Réaliser un graphique de la statistique du test en fonction de la pratique d'activité physique 
# en colorant les points en fonction de la p.value.
plot(statistique~c(1:8), col=p.value<0.05)

# 10) c) Dans quelle condition les variables sont les plus dépendantes entre elles. 
which.min(p.value)

# 10) d) Visualiser la table de confusion à l'aide de la fonction barplot() avec le seuil choisi 
# en ajoutant les résultats du test statistique (statistique du test + p value).

# 11) Que peut-on conclure ?

