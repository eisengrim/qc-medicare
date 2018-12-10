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
library(tidyr)
library(dplyr)
library(reshape2)
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
      axis.text.y = element_text(size=rel(1.2)),
      axis.title.y = element_text(size=rel(1.2), vjust=2),
      axis.title.x = element_text(size=rel(1.2), vjust=-0.5),
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
# 62 million requests for payment in 2017-2018
tab2 <- data.frame(annee = c("2013", "2014", "2015", "2016", "2017"),
                   acte = c(61.4, 61.5, 63.2, 66.4, 70.1),
                   autres = c(38.6, 38.5, 36.8, 33.6, 29.9))


# médecins présentant un écart de facturation (outils de dépistage) et taux de vérification
# seuil minimal de 50% et un cible de 75%
tab3 <- data.frame(annee = c("2016", "2016", "2017 †", "2017 †"),
                   genre = c("omni", "spec", "omni", "spec"),
                   nombre = c(163, 396, 120, 360),
                   verifies = c(25, 34, 12, 8))

# fluctuation des dossiers à analyser en inventaire
tab4 <- data.frame(annee = c("2015", "2016", "2017"),
                   en_debut = c(2,11,69),
                   pendant = c(16, 103, 135),
                   debut_danalyse = c(7,45,114),
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

# rise of FFS over years
labs = c("Fee-for-service and incentives", "Other programs and benefits", 
         "Remuneration package", "Fees package", "Mixed scheme", "Special measures",
         "Salary and fixed fees")
ggplot(data=annx2 %>% gather(mode, value, -annee)    , 
  aes(x=annee, y=value, group=as.factor(mode), colour=as.factor(mode))) +
  geom_line(stat="identity") +
  labs(x="Year", y="Number of Physicians",
       title="Physician remuneration schemes in Québec",
       subtitle="Yearly accruement of fee-for-service payments and measured incentives, 2013-2014 to 2017-2018",
       caption="Author: Kody Crowell (@hummushero); Source: VGQ, RAMQ (2018)") +
  scale_color_manual(values=rev(brewer.pal(9, "Reds")), name="", labels=labs,
                    guide = guide_legend(
                      direction = "vertical", keyheight = unit(2, units = "mm"),
                      keywidth = unit(50/length(labels), units = "mm"),
                      title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
                      reverse = T, label.position = "bottom")) +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1.2)),
        strip.text.y = element_text(size=rel(1.2)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) # 1400x800

# verified facturation
labs = c("General practitioner", "Specialist")
ggplot(data=melt(tab3) %>% mutate(value2=c(138, 362, 108, 352, 25, 34, 12, 8)), 
       aes(annee, value, group=genre, fill=genre, alpha=variable, label=value2)) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  labs(x="Year", y="Number of cases",
       title="Number of physicians in Québec with a billing discrepancy",
       subtitle="Total number of cases and number of cases verified, 2016-2017",
       caption="Author: Kody Crowell (@hummushero); Source: VGQ, RAMQ (2018)
       † 2017 verification incomplete at the time the audit was performed") +
  scale_fill_manual(values=rev(red.ramp[c(7,5)]), name="", labels=labs,
                      guide = guide_legend(
                        direction = "vertical", keyheight = unit(2, units = "mm"),
                        keywidth = unit(30/length(labels), units = "mm"),
                        title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
                        reverse = T, label.position = "bottom")) +
  scale_alpha_manual(values=c(0.5, 1), name="", labels=c("Non-verified", "Verified")) +
  geom_text(family="Georgia", vjust=-0.5, position=position_dodge(width = 1),
            show.legend = FALSE) +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1.2)),
        strip.text.y = element_text(size=rel(1.2)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) # 1000x800

# practices who charged a million or more to RAMQ -- total 166, 202, 195 (2016)
# opthal and radiologie upwards of 2 million
mil.plus.5 <- mil.plus %>% filter(`2016` > 9) %>% arrange(`2016`)
mil.plus.5$Spécialité <- factor(mil.plus.5$Spécialité, 
                                levels=c("Omnipraticien", "Cardiologie", "Radiologie diagnostique", "Ophtalmologie"))
# "Obstétrique et gynécologie",

labs <- c("General practice", "Cardiology", "Diagnotic radiology", "Opthalmology")
ggplot(data=mil.plus.5 %>% melt(), 
       aes(variable, value, group=Spécialité, fill=Spécialité, label=value)) +
  geom_bar(stat="identity", position="dodge", colour="black") +
  labs(x="Year", y="Number of practitioners",
       title="Profit or practice?",
       subtitle="Total number of physicians (by specialty) who billed RAMQ more than $1,000,000, 2014-2016",
       caption="Author: Kody Crowell (@hummushero); Source: RAMQ (2018)") +
  scale_fill_manual(values=rev(red.ramp), name="", labels=labs,
                    guide = guide_legend(
                      direction = "vertical", keyheight = unit(3, units = "mm"),
                      keywidth = unit(20/length(labels), units = "mm"),
                      title.position = 'right', title.hjust = 0.5, label.hjust = 0.5,
                      reverse = T, label.position = "bottom")) +
  scale_alpha_manual(values=c(0.5, 1), name="", labels=c("Non-verified", "Verified")) +
  geom_text(family="Georgia", vjust=-0.5, position=position_dodge(width = 1)) +
  theme_mir() +
  theme(strip.text.x = element_text(size=rel(1.2)),
        strip.text.y = element_text(size=rel(1.2)),
        strip.background = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0),
        plot.title = element_text(size=18, margin=margin(b=10)),
        plot.subtitle = element_text(size=12, color=mir.gray, face="italic",
                                     margin=margin(b=25)),
        plot.caption = element_text(size=10, margin=margin(t=10), 
                                    color="grey60", hjust=0)) # 1200x800

################################################################################
# notes

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
