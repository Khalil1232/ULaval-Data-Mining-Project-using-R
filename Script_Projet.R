library("readr")
setwd("C:/mi_Projet/")
data=read.csv(file = "Projet-Mi-session-DataSet.csv",sep=";")
View(data)
#Détection des valeurs manquantes
any(is.na(data))

data$Genre
data$Genre[381]

data$Genre[381]="M"
data$Genre[381]
data$Genre



boxplot(data$ValeurAchat)

boxplot(data$Revenu)
boxplot(data$InvestBourse)
boxplot(data$InvestBitcoin)
boxplot(data$NbRetours)
boxplot(data$Carburant)
boxplot(data$Hypotheque)
boxplot(data$AchatPrecedent)
boxplot(data$ValeurAchat)
data$Fidelite
data$AmznPrime
#Remplace valeur abberante
boxplot(data$ValeurAchat)
l=length(data$ValeurAchat)
print(l)
max(data$ValeurAchat)
#Traitement de la valeur aberrante
summary(data$ValeurAchat)
data$ValeurAchat[476]=297.4
data$ValeurAchat[476]
boxplot(data$ValeurAchat)
library(tidyverse)
#On supprime la variable Genre
data <- data[,-1]
View(data)
#On transforme les variables AmznPrime et Fidelite en variables indicatrices
for (i in 1 : l){
  if(data$AmznPrime[i]=="Yes")
    data$AmznPrime[i]=1
  else
    data$AmznPrime[i]=0
}
for (i in 1 : l){
  if(data$Fidelite[i]=="Yes")
    data$Fidelite[i]=1
  else
    data$Fidelite[i]=0
}
View(data)
#Analyse descriptive

library(DataExplorer)
create_report(data)
barplot(table(data$Fidelite), 
        ylim = c(0, 3000), 
        main = "Bar Graph of Fidel and Non-Fidel", 
        col = "lightblue")

barplot(table(data$AmznPrime), 
        ylim = c(0, 3000), 
        main = "Bar Graph of amazon prime and Non-amazone prime", 
        col = "lightblue")
barplot(table(data$Genre), 
        ylim = c(0, 3000), 
        main = "Bar Graph of amazon prime and Non-amazone prime", 
        col = "lightblue")
barplot(table(data$Fidelite), horiz=TRUE, 
        ylim =c(0,1),
        xlim =c(0,3000),
        width = 0.05,
        space=0.1,
        col = c("lightblue", "red"))
title("Bar Graph of Fidèles and Non-Fidèles ", line=-20)

barplot(table(data$AmznPrime), horiz=TRUE, 
        ylim =c(0,1),
        xlim =c(0,3000),
        width = 0.05,
        space=0.1,
        col = c("lightblue", "red")
)
title("Bar Graph of amazon prime and Non-amazone prime ", line=-20)
barplot(table(data$Genre), horiz=TRUE, 
        ylim =c(0,1),
        xlim =c(0,3000),
        width = 0.05,
        space=0.1,
        col = c("lightblue", "red")
)
title("Bar Graph of Genre ", line=-20)

Tableau_1 <- table(data$Fidelite, data$AmznPrime, dnn=c("Fidelite", "Amazon Prime")) 
Tableau_1_T <- addmargins(Tableau_1, FUN = sum)
Tableau_1  
Tableau_1_Pt <- round(prop.table(Tableau_1), 4)???100                #Pourcentage total
Tableau_1_Pl <- round(prop.table(Tableau_1, margin = 1), 4)???100    #Pourcentage en ligne
Tableau_1_Pc <- round(prop.table(Tableau_1, margin = 2), 4)???100    #Pourcentage en colonne
Tableau_1_Pt
Tableau_1_Pl
Tableau_1_Pc
barplot(Tableau_, legend = rownames(Tableau_1), col = c("blue", "red"), ylim = c(0, 3300), ylab = "Count", xlab = "Amazon prime", main = "Comparison Bar Chart: Fidelite by Amazon Prime")
barplot(Tableau_1, col = c("red", "blue"), ylim = c(0, 3300), ylab = "Count", xlab = "Amazon prime", main = "Fidelite Count by Amazon prime", beside = TRUE)
legend("topright", c(rownames(Tableau_1)), col = c("red", "blue"), pch = 15, title = "Fidelite")
library(tidyverse)
#Affichage du diagramme à barres égales empilées de la variable NbRetours avec superposition de la variable Fidèlité
ggplot() + 
  geom_bar(data = data, aes(x = factor(NbRetours), fill = factor(Fidelite)), position = "stack") + 
  scale_x_discrete("Nb Retours") + 
  scale_y_continuous("Percent") + 
  guides(fill=guide_legend(title="Fidelite")) + 
  scale_fill_manual(values=c("green", "yellow"))

ggplot() + 
  geom_bar(data = data, aes(x = factor(NbRetours), fill = factor(Fidelite)), position = "fill") + 
  scale_x_discrete("Nb Retours") + 
  scale_y_continuous("Percent") + 
  guides(fill=guide_legend(title="Fidelite")) + 
  scale_fill_manual(values=c("green", "yellow"))

summary(data$InvestBitcoin)
#Intervalle de confiance
prop.test(table(data$Fidelite==0),conf.level = 0.95)$"conf.int"



t.test(data$ValeurAchat, data$Fidelite,alternative='two.sided',conf.level = 0.95);

#le test statistique adéquat pour comparer la moyenne des valeurs d'achat des clients qui
#ont l'adhésion au programme « fidélité » avec celle des clients qui n'ont pas cette adhésion. Le 
#test est-il significatif
subfidel1 <- subset(data, data$Fidelite == 1)
subfidel2 <- subset(data, data$Fidelite == 0)
View(subfidel1)
View(subfidel2)
t.test(subfidel1$ValeurAchat, subfidel2$ValeurAchat,alternative='two.sided',conf.level = 0.95);
summary(subfidel1$ValeurAchat)
summary(subfidel2$ValeurAchat)
#Regression Linéaire Multiple
RegModel_M1 <- lm(formula =ValeurAchat~Revenu+Hypotheque+AchatPrecedent+Age+InvestBourse+InvestBitcoin+NbRetours+Carburant+AmznPrime+Fidelite, data=data)
summary(RegModel_M1)
#Diagnostic
par(mfrow=c(2,2))
plot(RegModel_M1)
RegModel_M2 <- lm(formula =InvestBitcoin~Revenu+Hypotheque+AchatPrecedent+Age+InvestBourse+ValeurAchat+NbRetours+Carburant+AmznPrime+Fidelite, data=data)
summary(RegModel_M2)
#Diagnostic
par(mfrow=c(2,2))
plot(RegModel_M2)

