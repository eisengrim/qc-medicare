# filename: qc-pharma.R (v1)
# author:   kody crowell
# date:     nov 30, 2018

## see also
# https://www.bbc.com/news/health-42572110
# http://www.ramq.gouv.qc.ca/fr/donnees-et-statistiques/Pages/statistiques-frequemment-demandees.aspx
# https://www4.prod.ramq.gouv.qc.ca/IST/CD/CDF_DifsnInfoStats/CDF1_CnsulInfoStatsCNC_iut/DifsnInfoStats.aspx?ETAPE_COUR=2&LANGUE=en-CA#PosMiddleTab
# http://www.ramq.gouv.qc.ca/fr/donnees-et-statistiques/Pages/la-regie-en-quelques-chiffres.aspx

# https://www.theglobeandmail.com/opinion/article-national-pharmacare-is-possible-but-it-wont-come-easy/
# https://www.huffingtonpost.ca/tara-gomes/no-one-is-talking-about-this-overmedicated-group-of-canadians_a_23262542/
# https://choosingwiselycanada.org/antipsychotic-for-disruptive-behaviour-dementia/
# https://www.cihi.ca/en/health-spending/2018/prescribed-drug-spending-in-canada
# https://www.cihi.ca/sites/default/files/document/pan-canadian-trends-opioid-prescribing-2017-en-web.pdf

# https://www.cihi.ca/en/1-in-4-canadian-seniors-is-prescribed-10-or-more-drugs
# https://www150.statcan.gc.ca/n1/pub/82-003-x/2009001/article/10801/findings-resultats-eng.htm
# https://www.cbc.ca/radio/quirks/neanderthals-good-at-art-treating-fake-news-and-more-1.4547017/over-medicated-seniors-are-an-unseen-epidemic-1.4547370

# https://globalnews.ca/news/4172338/deprescribing-too-much-medication/
# https://globalnews.ca/news/4024393/canadians-prescription-drugs-cut-groceries-heat-spending/


# load libs
library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)
library(lubridate)
library(scales)
library(RColorBrewer)

theme_set(theme_minimal())

mir.red <- "#cf0808" #"#f00d0d"
mir.white <- "#f9f9f9"
mir.gray <- "#4b4b4b"
mir.lgray <- "#eeeeee"
red.ramp <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d')

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Georgia", color = mir.gray),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = mir.lgray, size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = mir.white, color = NA), 
      panel.background = element_rect(fill = mir.white, color = NA), 
      legend.background = element_rect(fill = mir.white, color = NA),
      panel.border = element_blank(),
      ...
    )
}
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

# overprescribing
# 2/3 seniors take 5 drugs a day (CIHI 2012)
# 40% of those 85+ take 10+ per day
# main driver behind hospitalizations due to ADE
# efforts to deprescribe, STOMP
# antipsychotic drugs among those with disruptive behavioirs

# opioids
# hospitalizations
# cost to patient, govt