myVariableName <-c("Comptes", "Duree_credit", "Historique_credit", "Objet_credit", "Montant_credit", "Epargne",
                   "Anciennete_emploi", "Taux_effort", "Situation_familiale", "Garanties", "Anciennete_domicile", "Biens"
                   ,"Age", "Autres_credits", "Statut_domicile", "Nb_credits", "Type_emploi", "Nb_pers_charge", 
                   "Telephone", "Etranger", "Cible")

credit = read.table("http://blogperso.univ-rennes1.fr/stephane.tuffery/public/german.txt", h=FALSE, col.names = myVariableName)
credit$Cle <- seq(1,nrow(credit))
credit$Comptes <- as.factor(substr(credit$Comptes, 3, 3))
credit$Anciennete_emploi <-substr(credit$Epargne, 3,3)
credit$Epargne <-substr(credit$Epargne, 3,3)
credit$Epargne[credit$Epargne =="5"] <- "0"
credit$Etranger <- NULL
credit$Cible[credit$Cible ==1] <- 0
credit$Cible[credit$Cible == 2] <-1
credit$Cible <- as.factor(credit$Cible)


varquali <-c("Comptes","Epargne", "Historique_credit", "Objet_credit", "Situation_familiale", "Garanties", "Biens",
             "Autres_credits", "Statut_domicile", "Type_emploi", "Anciennete_emploi", "Telephone", "Nb_pers_charge" )

indices <- names(credit) %in% varquali

for (i in (1:ncol(credit)))
{if (indices[i] ==1)
    {credit[,i] <- factor(credit[,i])
}
}

varquanti <- c("Duree_credit", "Montant_credit", "Taux_effort", "Anciennete_domicile", "Age", "Nb_credits")

vars <- -grep('(Cle|Cible)', names(credit))

cles = read.table("http://blogperso.univ-rennes1.fr/stephane.tuffery/public/cles.txt", h=FALSE, col.names = "Cle")

id <-cles$Cle
test <-credit[-id,]
table(test$Cible)
