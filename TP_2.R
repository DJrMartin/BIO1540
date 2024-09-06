rm(list=ls())
########################################
# TP n°2 : Test statistique multivarié #
########################################
# Durée : 3h

# Nous utiliserons pour l'ensemble des prochains TPs un jeu de données issu d'une étude réalisée dans la Station Spatial Internationale. 
# L'ensemble des résultats est déjà publiées dans cet article : https://link.springer.com/article/10.1007/s12217-018-9653-2 aussi disponible dans le github.
# L'ensemble des données est téléchargeable sur la plateforme de la NASA : https://osdr.nasa.gov/bio/repo/data/studies/OSD-546.

# Objectifs : explorer les conséquences de la microgravité (i.e. pas de pesanteur) sur le métabolisme du fer 
# dans des Hematopoïetic Stem Cell en condition de différenciation cellulaire (HSC vers osteoblastes).

# 1) Importer les données 
df <- read.table(file = "~/Dropbox/COURS/UBS/BIO1540/TPs/iron_metabolism_RNA_TP.csv", sep=";", header = T)
load("~/Dropbox/COURS/UBS/BIO1540/TPs/meta_data.rda")

# 2) Définir les 4 conditions expérimentales. Montrer la matrice de confusion
table(meta_data[,2:3])
# Définir les conditions de ground versus Space ?
# Définir les conditions de osteogenic versus Standard. Lire le papier pour être au clair avec les conditions. 

# 4) Faire un petit récapitulatif de comment fonctionne le métabolisme du fer au niveau intra-cellulaire.
# Nous pourrons appuyer nos résultats sur de la littérature existante : https://faseb.onlinelibrary.wiley.com/doi/10.1096/fj.202301184R

# 5) définir n et p
n <- length(meta_data$ID)
p <- dim(df)[1]

#############################
# Statistiques descriptives #
#############################
# 6) Visualiser l'effet du macro-environnement sur chaque expression d'ARNm.
for(i in 1:p){boxplot(as.numeric(df[,-c(1,2)][, meta_data$ID][i,])~as.factor(meta_data$Macro_env), main=(df$SYMBOL)[i])}
# 6) a) Que peut-on dire ?
# 6) b) Quelles seraient les hypothèses H_O et H_1 ?
# 6) c) Les données sont-elles normales et/ou possèdent une variance similaire entre les groupes ?

# 7) Visualiser l'effet du micro-environnement sur chaque expression d'ARNm.
for(i in 1:p){boxplot(as.numeric(df[,-c(1,2)][, meta_data$ID][i,])~as.factor(meta_data$Micro_env), main=(df$SYMBOL)[i])}
# 7) a) Que peut-on dire ?
# 7) b) Quelles seraient les hypothèses H_O et H_1 ?
# 7) c) Les données sont-elles normales et/ou possèdent une variance similaire entre les groupes ?

######################
# Tests Statistiques #
######################
# 8) Quels tests statistiques utiliser ?

# 9) via une boucle for() réaliser l'ensemble des tests ? 
# 9) a) pour déterminer l'effet de la vit D3 sur l'expression des ARNm ?
# 9) b) pour déterminer l'effet de la microgravité sur l'expression des ARNm ?

# 10) a) Quelle est la probabilité de faire une erreur de type I ?
# 10) b) Appliquer une correction ? Dunn ou Bonferroni ? 

# 11) Que peut-on dire de l'effet de la VIT D3 ? Que peut-on dire de l'effet de la microgravité (espace) sur le métabolisme du fer dans les HSC/osteoblastes ?

#############################
# Modelisation à 2 facteurs #
#############################
# 12) Peut-on utiliser l'anova à 1 facteur ?

# 13) Comprendre comment fonctionne l'ANOVA à 2 facteurs ? Ainsi que l'interaction ?

# 14) Explorer les données en utilisant l'ANOVA à 2 facteurs ?

# 15) Conclure.

