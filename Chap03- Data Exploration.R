#install.packages('Hmisc')
library(Hmisc)
# Data Expploration :
summary(credit)
hist.data.frame(credit[,1:16])
#Continuous Variables Analysis 
library(lattice)
histogram(~Duree_credit | Cible, data= credit, type="percent", col="grey", breaks = 10)
histogram(~Montant_credit | Cible, data= credit, type="percent", col="grey", breaks = 10)
histogram(~Age | Cible, data= credit, type="percent", col="grey", breaks = 10)

by(credit[,c("Age", "Duree_credit", "Montant_credit")], list(Cible=credit$Cible), summary)
# We can observe that there are a kind of relation between con var and the target.
# We can formal test this assumptions with Kruskal-Wallis (ignoring the normalite and the homoscedscite):
kruskal.test(credit$Age~credit$Cible)$statistic
kruskal.test(credit$Duree_credit~credit$Cible)$statistic
kruskal.test(credit$Montant_credit~credit$Cible)$statistic

# more robust with x^2 normalised :
sqrt((kruskal.test(credit$Age~credit$Cible)$statistic)/sum(!is.na(credit$Age)))
sqrt((kruskal.test(credit$Duree_credit~credit$Cible)$statistic)/sum(!is.na(credit$Age)))
sqrt((kruskal.test(credit$Montant_credit~credit$Cible)$statistic)/sum(!is.na(credit$Age)))

# Binning Con Var :
# Age
attach(credit)
#seq(0, 1, by=0.1)
q <- quantile(credit$Age, seq(0, 1, by=0.1))
#q
q[1]<-q[1]-1
qage<-cut(Age, q)
tab<-table(qage, Cible)
prop.table(tab, 1)
3
barplot(t(prop.table(tab, 1)[,2]), ylim=c(0,0.5), las =3, main="age", ylab="Taux Impayes", density=0)
abline(h=.3,lty=2)

Age <-cut(Age, c(0,25,Inf))
tab<-table(Age, credit$Cible)
prop.table(tab, 1)

#Duree_credit

q<-unique(quantile(credit$Duree_credit, seq(0, 1, by=0.05)))
q[1]<-q[1]-1
qduree<-cut(Duree_credit, q)
tab<-table(qduree, Cible)
prop.table(tab, 1)

barplot(t(prop.table(tab, 1)[,2]), ylim=c(0,0.5), las =3, main="Duree", ylab="Taux Impayes", density=0)
abline(h=.3,lty=2)

Duree_credit <-cut(Duree_credit, c(0,15,36, Inf))
tab <- table(Duree_credit, Cible)
prop.table(tab, 1)

# Montant Credit 
Montant_credit<-cut(Montant_credit, c(0, 4000, Inf))
tab <-table(Montant_credit, Cible)
prop.table(tab, 1)
