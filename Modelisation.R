
library(tidyverse)
library(questionr)
library(ggpubr)
library(rstatix)
library(broom)
library(corrplot)
library(leaps)
library(glmulti)
library(rJava)
library(ggfortify)
library(rmarkdown)
library(cowplot)
#Importation des données 
setwd("C:/Users/yassi/OneDrive/Université/S7/Modèles de régression linéaire/Projet")
load("vagues.Rdata")

#On se trouve dans un modèle d'analyse  de covariance car les variables explicatives sont quantitatives et qualitatives
#Durant notre étude, on se fixe un seuil d'erreur de 5%
# Tout d'abord on transforme nos données en data frame pour pouvoir les visualiser


vagues = as.data.frame(vagues)




#On créee des colonnes indicatrices en fonction des variables qualitatives saison et direction
vagues <-  mutate(vagues,printemps = case_when(
  saison == 'Printemps'~ 1,
  saison != 'Printemps'~ 0
),ete = case_when(
  saison == 'Ete'~ 1,
  saison != 'Ete'~ 0
),
automne = case_when(
  saison == 'Automne'~ 1,
  saison != 'Automne'~ 0
),
hiver = case_when(
  saison == 'Hiver'~ 1,
  saison != 'Hiver'~ 0
),
est = case_when(
  direction == 'Est'~ 1,
  direction != 'Est'~ 0
),
ouest = case_when(
  direction == 'Ouest'~ 1,
  direction != 'Ouest'~ 0
),
nord = case_when(
  direction == 'Nord'~ 1,
  direction != 'Nord'~ 0
),sud = case_when(
  direction == 'Sud'~ 1,
  direction != 'Sud'~ 0
)
)

########################### Création d'un modèle ##########################################################################################

#On crée notre modèle de base avec toutes les variables explicatives en utilisant l'interaction entre les variables qualitatives saison et direction
model <- lm(vagues$h110d~vagues$temperature+vagues$pression+vagues$humidite_relative+vagues$point2rose+vagues$visibilite_horiz+vagues$vent_vit_moy+vagues$vent_vit_rafale+vagues$vent_dir+vagues$precipitation_cum+vagues$saison*vagues$direction)

summary(model)

# La modèle explique environ 54% de la variabilité de la hauteur des vagues
# la p-value du modèle est faible donc le modèle est significatif
#Les variables explicatives très significatifs sont vent_vit rafale et printemps:ouest car leur p-value est très faible
#les variables explicatives significatifs sont hiver:ouest, précipitation_cum, pression , vent_dir, température
#On constate qu'il y a beaucoup de coefficients ayant des p-values très élevés et donc le modèle n'est pas optimal
#On cherche donc à déterminer les corrélations entre les variables


#On visualise la matrice de corrélation
# On voit qu'il y a une forte corrélation positive entre : 
#vent_vit_rafale et vent_vit_moy
#point2rose et temperature
#ouest et vent_dir

#Forte Corrélation négative entre est et vent_dir
vagues2 <- select(vagues,-c("saison","direction"))
matrice_cor <- cor(vagues2)

corrplot(matrice_cor)


# Avant de déterminer un modèle optimal complet, on débute avec seulement les variables qualitatives
# afin de voir s'il existe une relation entre les variables qualitatives
# et la variable à expliquer
# On se retrouve avec une ANOVA à deux facteurs



#Tout d'abord, nous effectuons l'ANOVA à deux facteurs
model_anova <- lm(data=vagues, h110d~saison*direction)
summary(model_anova)

boxplot(data=vagues,h110d~saison)
boxplot(data=vagues,h110d~direction)
boxplot(data=vagues,h110d~saison*direction)



anova(model_anova)
#

#On constate que la p-value de direction et saison ainsi que l'interaction entre ces deux variables est très faible
#Cela veut que dire que l'impact de ces variables là est significatif sur la hauteur des vagues



#Tout d'abord, on teste l'hypothèse de linéarité entre les covariables et la variable-réponse pour chaque groupe 
#On constate qu'il y bien une relation linéaire entre les covariables et la variable réponse pour chaque groupe
ggscatter(
  vagues, x = "vent_dir", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)



ggscatter(
  vagues, x = "point2rose", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)


ggscatter(
  vagues, x = "temperature", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)


ggscatter(
  vagues, x = "pression", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)


ggscatter(
  vagues, x = "humidite_relative", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)


ggscatter(
  vagues, x = "visibilite_horiz", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)


ggscatter(
  vagues, x = "vent_vit_moy", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)


ggscatter(
  vagues, x = "vent_vit_rafale", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

ggscatter(
  vagues, x = "precipitation_cum", y = "h110d",
  facet.by  = c("saison", "direction"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

#Nous cherchons à savoir s'il existe une intéraction significative entre les covariables et les variables de groupement saison et direction


vagues %>%
  unite(col = "group", saison, direction) %>%
  anova_test( h110d ~ group*temperature+group*pression+group*humidite_relative+group*point2rose+group*precipitation_cum+group*visibilite_horiz+group*vent_vit_moy+group*vent_vit_rafale+group*vent_dir)

#On voit qu'il existe une intéraction significative avec vent_vit_rafale,precipitation_cum,point2rose,humidite_relative,temperature

#On utilise maintenant la méthode de backward afin de trouver un meilleur modèle 


select.variables <- step(model, direction="backward", data=vagues)
# 
summary(select.variables)
#Après avoir utiliser la méthode de backward, les variables temperature, visibilité_horiz et vent_vit_moy ont été retirés
#Le R² ajusté est de 0.5435 donc le modèle explique environ 54% de la variabilité de la hauteur des vagues
# Ce nouveau modèle a un p-value très faible donc le modèle est significatif
#La majorité des coefficients ont des p-values très significatifs car très faible


choix <- regsubsets(data=vagues,h110d~temperature+pression+humidite_relative+point2rose+visibilite_horiz+vent_vit_moy+vent_vit_rafale+vent_dir+precipitation_cum+direction*saison,int=T ,method="exh")
resume.choix <- summary(choix)
# 
taille <- as.double(rownames(resume.choix$wh))+1
plot(choix,scale="adjr2")


#On remarque sur ce graphique qu'après avoir obtenu une multitude de modèle en utilisant la méthode exhaustive
#Le meilleur R² ajusté que l'on peut obtenir est d'environ 0.55 donc notre modèle obtenu avec la méthode du backward
#est intéressant


#Maintenant nous allons tester les hypothèses de validations du modèle

#On effectue le test de normalité des résidus
model_backward <- lm(data=vagues,h110d~pression+humidite_relative+point2rose+vent_vit_rafale+vent_dir+precipitation_cum+saison+direction)


plot(model_backward,1)
#L'hypothèse d'homoscédasticité des résidus n'est pas vérifiée car on a une courbe légérement en cloche
#Alors que l'on doit obtenir une droite horizontale 

#On applique une transformation logarithmique afin de satistfaire cette hypothèse

model_backward <- lm(data=vagues,log(h110d)~pression+humidite_relative+point2rose+vent_vit_rafale+vent_dir+precipitation_cum+saison+direction)

summary(model_backward)
#On remarque que le R² ajusté est passé à environ 50% 
#Cependant, l'écart type résiduel a diminué de moitié et est de 0.4181

#On vérifie les hypothèses de validité du modèle

autoplot(model_backward)

#Toutes les hypothèses sont vérifiés !

#Ce modèle ayant un R² ajusté et un écart type résiduelle acceptable, on peut retenir ce modèle comme référence pour la partie Compléments.

#On créer le modèle complet correspondant à ce modèle afin de faire le test d'emboîtement dans la partie Compléments

model_complet_backward <- lm(data=vagues,log(h110d)~temperature+pression+humidite_relative+point2rose+visibilite_horiz+vent_vit_moy+vent_vit_rafale+vent_dir+precipitation_cum+saison*vagues$direction)
summary(model_complet_backward)


#############################Amélioration du modèle #############################################################################################

#Toutefois, on souhaite tenter d'améliorer ce modèle en effectuant certaines modifications sur nos données et notre modèle


model_ameliore <- regsubsets(h110d~temperature+pression+humidite_relative+point2rose+visibilite_horiz+vent_vit_moy+vent_vit_rafale+vent_dir+precipitation_cum+saison*direction,data=vagues,
                             nbest = 1,       # 1 seul meilleur pour chaque nombre de variables
                             nvmax = NULL,    # NULL pas de limites pour le 
                             force.in = NULL,  # pas de variables à inclure de force
                             force.out = NULL,  # pas de variables à exclure de force.
                             method = "exhaustive")  #


summary(model_ameliore)
plot(model_ameliore,scale="adjr2")
#On remarque que le R² ajusté maximale est d'environ 0.55
#On cherche à récupérer le modèle correspondant

summary.out <- summary(model_ameliore)


summary.out$which[which.max(summary.out$adjr2),]

#Cette sortie nous permet de savoir quelles sont les variables présentent dans le modèle ayant le plus grand R² ajusté

#Création du modèle
model_ameliore <- lm(data=vagues,h110d~1+humidite_relative+precipitation_cum+printemps:ouest+printemps:sud+temperature+vent_vit_rafale+ete+nord+vent_dir+hiver+hiver:ouest+hiver:sud)
summary(model_ameliore)


#Vérification des hypothèses de validité du modèle

autoplot(model_ameliore)

#On constate que l'hypothèse d'homoscédasticité n'est pas vérifiée 
#On effectue une transformation logorithmique
model_ameliore <- lm(data=vagues,log(h110d)~1+humidite_relative+precipitation_cum+printemps:ouest+printemps:sud+temperature+vent_vit_rafale+ete+nord+vent_dir+hiver:ouest+hiver:sud)


autoplot(model_ameliore)

#Après transformation, on voit que toutes les hypothèses de validité du modèle sont vérifiées

summary(model_ameliore)


#On cherche à vérifier la présence de points aberrants en utilisant la distance de Cook
plot(model_ameliore,5)
#On enleve le point aberrant  1 en utilisant la distance de Cook comme réference


#On supprime la 1ere ligne
vagues <- slice(vagues,2:453)

model_ameliore <- lm(data=vagues,log(h110d)~1+humidite_relative+precipitation_cum+printemps:ouest+printemps:sud+temperature+vent_vit_rafale+ete+nord+vent_dir+hiver:ouest+hiver:sud)
summary(model_ameliore)


model_ameliore <- lm(data=vagues,log(h110d)~1+humidite_relative+printemps:ouest+printemps:sud+temperature+vent_vit_rafale+ete+nord+vent_dir+hiver:ouest+hiver:sud)


autoplot(model_ameliore)
plot(model_ameliore,5)
vagues <- vagues[-380,]

model_ameliore <- lm(data=vagues,log(h110d)~1+humidite_relative+printemps:ouest+printemps:sud+temperature+vent_vit_rafale+ete+nord+vent_dir+hiver:ouest+hiver:sud)
summary(model_ameliore)
autoplot(model_ameliore)









model_ameliore2 <- regsubsets(log(h110d)~1+humidite_relative+printemps:ouest+printemps:sud+temperature+vent_vit_rafale+ete+nord+vent_dir+hiver:ouest+hiver:sud,data=vagues,
                              nbest = 1,       # 1 seul meilleur pour chaque nombre de variables
                              nvmax = NULL,    # NULL pas de limites pour le 
                              force.in = NULL,  # pas de variables à inclure de force
                              force.out = NULL,  # pas de variables à exclure de force.
                              method = "exhaustive")



#On utilise une nouvelle fois sur notre dernier modèle la méthode exhaustive et on constate que 
#nous ne pouvons pas espérer un meilleur modèle ayant un R² ajusté supérieur d'après notre graphique
plot(model_ameliore2,scale="adjr2")


##Compléments Question 6 test d'adéquation à la loi normale des 20 premiers résidus

residus_20 <- as.data.frame(model_backward$residuals[1:20])
residus_20sort <- arrange(residus_20,desc(residus_20))
var_residus_20 <- var(residus_20)
image_residus = pnorm(as.double(unlist(residus_20sort[1]),mean=0,sd = sqrt(var_residus_20)))

matrice_residus <- cbind(residus_20sort,image_residus)
n=20
kolmogov_a <- rep('NA',n)
kolmogov_b <- rep('NA',n)
kolmogov_max <- rep('NA',n)
for(i in 1:n){
  kolmogov_a[i] = abs(matrice_residus$image_residus[i] - (i/n))
  kolmogov_b[i] = abs(matrice_residus$image_residus[i] - (i-1)/n)
  kolmogov_max[i] = max(kolmogov_a[i],kolmogov_b[i])
}
max_col_kolmogov <- max(kolmogov_max)

matrice_residus <- mutate(matrice_residus,kolmogov_a)
matrice_residus <- mutate(matrice_residus,kolmogov_b)
matrice_residus <- mutate(matrice_residus,kolmogov_max)
colnames(matrice_residus)[1] = 'Résidus'
hist_res <- ggplot(model_backward,aes(x =model_backward$residuals,y=..density.. ))+
  geom_histogram(col='white',bins = 30) +
  stat_density(geom="line",colour='red',size=2,position = 'identity') 


hist_res

########Partie 3 Prédiction#################################################################################################################

##Prédiction de notre modèle amélioré
vagues_predict <- predict(model_ameliore, vagues, se.fit = FALSE, scale = NULL, df = Inf,
                          interval = c("none", "confidence", "prediction"),
                          level = 0.95, type = c("response", "terms"),
                          terms = NULL, na.action = na.pass,
                          pred.var = res.var/weights, weights = 1)

vagues_predict <- as.data.frame(vagues_predict)

vagues_base <- as.data.frame(vagues$h110d)

vagues_echantillon <- as.data.frame(cbind(rep(seq(1:451),2)))



dataset <- c(vagues_base$`vagues$h110d`,vagues_predict$vagues_predict)
vagues_echantillon <- mutate(vagues_echantillon,dataset = dataset)


combine_data <- c(rep('Base',451),rep('Prediction_Amélioré',451))


vagues_echantillon <- mutate(vagues_echantillon,type = combine_data)
colnames(vagues_echantillon)[1] = 'Y'


a <- ggplot(vagues_echantillon, aes(x = dataset))
a_density <-  a + geom_density(aes(fill = type), alpha = 0.4) + theme_minimal()
a_hist_density <- a + geom_histogram(aes(y = ..density.., color = type, fill = type),  alpha = 0.4, position = "identity") + geom_density(aes(color = type), size =1)+theme_minimal()

plot_grid(a_density,a_hist_density)

##Comparaison des prédictions entre notre modèle amélioré et le modèle obtenu avec le backward

##On supprime les mêmes observations 1 et 380 dans notre modèle backward afin que le nombre d'observations soient identitiques
#Et que les prédictions se font sur les mêmes observations

vagues_predict_backward <- predict(model_backward, vagues, se.fit = FALSE, scale = NULL, df = Inf,
                                   interval = c("none", "confidence", "prediction"),
                                   level = 0.95, type = c("response", "terms"),
                                   terms = NULL, na.action = na.pass,
                                   pred.var = res.var/weights, weights = 1)


vagues_predict_backward <- as.data.frame(vagues_predict_backward)

vagues_base <- as.data.frame(vagues$h110d)

vagues_echantillon_backward <- as.data.frame(cbind(rep(seq(1:451),2)))




dataset_backward <- c(vagues_base$`vagues$h110d`,vagues_predict_backward$vagues_predict_backward)
vagues_echantillon_backward <- mutate(vagues_echantillon_backward,dataset_backward = dataset_backward)


combine_data_backward <- c(rep('Base',451),rep('Prediction_Backward',451))


vagues_echantillon_backward <- mutate(vagues_echantillon_backward,type = combine_data_backward)
colnames(vagues_echantillon)[1] = 'Y'

b <- ggplot(vagues_echantillon_backward, aes(x = dataset_backward))
backward_density <-  b + geom_density(aes(fill = type), alpha = 0.4) + theme_minimal()
hist_density_backward <- b + geom_histogram(aes(y = ..density.., color = type, fill = type),  alpha = 0.4, position = "identity") + geom_density(aes(color = type), size =1) + theme_minimal()


plot_grid(backward_density,hist_density_backward)


## Comparaison des prédictions entre modèle backward et modèle amélioré



vagues_echantillon_comparaison <- as.data.frame(cbind(rep(seq(1:451),3)))

dataset_prediction <- c(vagues_base$`vagues$h110d`,vagues_predict_backward$vagues_predict_backward,vagues_predict$vagues_predict)
vagues_echantillon_comparaison <- mutate(vagues_echantillon_comparaison,dataset_prediction= dataset_prediction)


combine_data_comparaison <- c(rep('Base',451),rep('Prediction_Backward',451),rep('Prediction_Amélioré',451))


vagues_echantillon_comparaison <- mutate(vagues_echantillon_comparaison,type = combine_data_comparaison)
colnames(vagues_echantillon_comparaison)[1] = 'Y'


comparaison_plot <- ggplot(vagues_echantillon_comparaison, aes(x = dataset_prediction))
comparaison_plot_density <-  comparaison_plot + geom_density(aes(fill = type), alpha = 0.4) + theme_minimal()
hist_density_comparaison_plot <- comparaison_plot + geom_histogram(aes(y = ..density.., color = type, fill = type),  alpha = 0.4, position = "identity") + geom_density(aes(color = type), size =1) + theme_minimal()


plot_grid(comparaison_plot_density,hist_density_comparaison_plot)


##On remarque que les distributions des prédictions de nos deux modèles sont presque identitique
#Les deux modèles n'ont pas un pouvoir de prédiction élevé
#On peut expliquer cela par leur R² ajusté moyen
#Notre but n'est pas de prédire les observations d'entraînement mais plutôt de visualiser les données prédites par nos
#deux modèles et les comparer aux données terrains.

#On ne peut pas prédire des données qui nous ont permis de construire nos modèles
#Il serait intéressant d'avoir des données supplémentaires pour mettre à l'épreuve la prévision de nos modèles


#L'influence de la pression, point de rosé,visibilité à l'horizon,vitesse moyenne du vent,printemps,automne,sud,est,ouest n'est pas significatifs sur les hauteurs moyennes des vagues
#puisqu'ils ne font pas partie de nos modèles


