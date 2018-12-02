# filename: qc-medicare.R (v1)
# author:   kody crowell
# date:     desc 01, 2018

## see also
# https://www.bbc.com/news/health-42572110
# http://www.ramq.gouv.qc.ca/fr/donnees-et-statistiques/Pages/statistiques-frequemment-demandees.aspx
# https://www4.prod.ramq.gouv.qc.ca/IST/CD/CDF_DifsnInfoStats/CDF1_CnsulInfoStatsCNC_iut/DifsnInfoStats.aspx?ETAPE_COUR=2&LANGUE=en-CA#PosMiddleTab
# http://www.ramq.gouv.qc.ca/fr/donnees-et-statistiques/Pages/la-regie-en-quelques-chiffres.aspx
# http://www.vgq.gouv.qc.ca/fr/fr_publications/fr_rapport-annuel/fr_2018-2019-novembre2018/fr_Rapport2018-2019-novembre2018.pdf
# https://www.cbc.ca/news/canada/montreal/quebec-doctor-fees-pointe-st-charles-1.4497324
# https://ccpsc.qc.ca/en/node/1961

# load libs
library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)
library(lubridate)
library(scales)
library(readxl)
library(RColorBrewer)

theme_set(theme_minimal())

mir.red <- "#cf0808" #"#f00d0d"
mir.white <- "#f9f9f9"
mir.gray <- "#4b4b4b"
mir.lgray <- "#eeeeee"
red.ramp <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d')

theme_mir <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Georgia", color = mir.gray),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle=45, hjust=1),
      axis.text.y = element_text(size=rel(1)),
      axis.title.y = element_text(size=rel(1), vjust=2),
      axis.title.x = element_text(size=rel(1), vjust=-0.5),
      panel.grid.major = element_line(color = mir.lgray, size = 0.2),
      panel.grid.minor = element_line(color = mir.lgray, size = 0.2),
      plot.background = element_rect(fill = mir.white, color = NA), 
      panel.background = element_rect(fill = mir.white, color = NA), 
      legend.background = element_rect(fill = mir.white, color = NA),
      panel.border = element_blank(),
      ...
    )
}

## tabulate date
# rémunération versée en 2017-2018
tab1 <- data.frame(genre = c("spec", "omni"), 
                  million = c(4770, 2968),
                  nombre = c(11283, 10126))

# modes de rémunération de 2013-2014 à 2017-2018 (en pourcentage)
tab2 <- data.frame(annee = c("2013", "2014", "2015", "2016", "2017"),
                   acte = c(61.4, 61.5, 63.2, 66.4, 70.1),
                   autres = c(38.6, 38.5, 36.8, 33.6, 29.9))


# médecins présentant un écart de facturation (outils de dépistage) et taux de vérification
# seuil minimal de 50% et un cible de 75%
tab3 <- data.frame(annee = c("2016", "2016", "2017", "2017"),
                   genre = c("omni", "spec", "omni", "spec"),
                   nombre = c(163, 396, 120, 360),
                   verifies = c(25, 34, 12, 8))

# fluctuation des dossiers à analyser en inventaire
tab4 <- data.frame(annee = c("2015", "2016", "2017"),
                   en_debut = c(2,11,69),
                   pendant = c(16, 103, 135),
                   debut_d-analyse = c(7,45,114),
                   finalises = c(56, 31, 23))

# Il s’agit du pourcentage des dossiers ajoutés à l’inventaire pendant l’exercice qui étaient finalisés
# lors de nos travaux.

# La RAMQ n’a pas encore atteint le seuil de dossiers présentant des écarts de facturation
# (dépistage) à analyser annuellement.
# Des méthodes de contrôle prévues au processus d’analyse de la facturation sont peu
# réalisées à ce jour ou nécessitent d’être raffinées pour donner de meilleurs résultats.

# évolution des modes de rémunération
annx2 <- data.frame(annee = c("2013", "2014", "2015", "2016", "2017"),
                    acte = c(3857, 3995, 4460, 4960, 5424),
                    mixte = c(1431, 1498, 1562, 1484, 1269),
                    honoraires = c(379, 390, 405, 365, 369),
                    forfait = c(269, 266, 282, 301, 331),
                    salaire = c(84, 74, 70, 66, 55),
                    partic = c(75, 108, 110, 119, 104),
                    autre = c(168, 164, 172, 171, 186))

# Nombre de médecins ayant facturé 1 000 000 $ et plus à la RAMQ 
mil.plus <- read_xlsx("Medecin_montant_million_an.xlsx", col_names=T)

# La RAMQ a revu le processus d’analyse de la facturation afin qu’il soit plus
# efficace. Toutefois, en raison du court délai depuis l’implantation des nouvelles
# façons de faire, la RAMQ n’est pas encore en mesure de démontrer clairement
# les retombées positives de cette révision.

# En vertu de l’article 64 de la Loi sur l’assurance maladie, la RAMQ est tenue
# de vérifier périodiquement, par voie d’échantillonnage, si les services assurés
# dont elle a assumé le coût ont effectivement été rendus.

# Premièrement, certains modes de rémunération et mesures incitatives
# étaient peu ou pas contrôlés a posteriori. Deuxièmement, les travaux d’analyse
# de la facturation portaient sur un nombre restreint de services facturés. Par
# exemple, en 2014, seulement 2% des médecins avaient fait l’objet d’une analyse
# de leur facturation, et ce, généralement pour un seul code d’acte. Troisièmement,
# les outils utilisés pour analyser la facturation n’étaient concentrés que sur les
# médecins dont le profil de facturation s’écartait de la moyenne observée chez
# leurs confrères. La RAMQ omettait ainsi de considérer les cas où l’utilisation d’un
# code d’acte de façon inappropriée serait largement répandue dans la pratique.

# Il y a eu une récupération de près de 160000 dollars provenant de l’investigation
# concernant 64 médecins à la suite d’un échantillonnage ciblé
# parmi les 200 médecins ayant facturé le plus de per diem sans acte au
# cours d’une année

# Une surveillance accrue a aussi été mise en place à l’égard des médecins
# auxquels la RAMQ verse plus de 1 million de dollars annuellement (184 médecins
# en 2016)

# La RAMQ a entrepris des actions visant à élargir la portée des contrôles
# a posteriori pour les modes de rémunération autres que celui à l’acte, dont
# les mesures incitatives. Cependant, ces contrôles demeurent insuffisants
# pour fournir une assurance raisonnable que les versements sont conformes
# aux ententes

# She noted that as part of her audit the RAMQ had a closer 
# look at the billing practices of 184 doctors who charge 
# the RAMQ more than $1 million a year. With one series of 
# checks, it hauled back $10 million in overpayments.

# Les pouvoirs de la RAMQ ont été élargis afin de lui permettre d’accentuer
# l’effet dissuasif de ses mesures de contrôle. Il est cependant encore trop tôt
# pour apprécier l’impact réel de ces nouveaux pouvoirs.

# L’impact des nouvelles sanctions administratives pécuniaires pouvant être appliquées
# par la RAMQ ne peut être encore constaté et évalué en raison du court délai depuis
# l’entrée en vigueur de ce nouveau pouvoir

# $7.7 million handed to doctors in 2017-2018
# plus de 21 409 medecins inscrits a la regie de l'assurance maladie du quebec
# 70% a l'acte

# eyecare costs went up after accessory fees banned
# after accessory fee ban, 58% frais administratifs -- generaliste
# specialiste -- 83% soins des yeux, medicaments, frais administraifs
