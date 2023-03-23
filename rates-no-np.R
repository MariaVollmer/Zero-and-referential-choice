# This script was written by Maria Vollmer (University of Freiburg, 
# Australian National University).

# This script is used in the analysis of the following paper:
# Vollmer, Maria. Accepted with minor revision. Comparing zero and referential 
# choice in eight languages with a focus on Mandarin Chinese. Studies in language.

# If you use this script, please make sure to cite the paper above. 
# If you have any questions, you can contact me via email: mariacvollmer@gmail.com

# This script produces the rates of zero and pronouns.

#### library
library(curl)
getwd()
# set to the directory in which you have your data
setwd(".")
source("antecedent-distance.R")
getwd()
library(data.table)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)
library(RColorBrewer)
library(rms)


#################################### barplots only zero and pro (no np)

### loading data

mc <- fread("mc_1905.tsv", 
            header = T,
            sep = "\t", 
            colClasses = list(factor= 1:2, character = 3:11), 
            encoding = "UTF-8")

mandarin <- fread("mc_mandarin-2001.tsv", 
                  header = T,
                  sep = "\t", 
                  colClasses = list(factor= 1:2, character = 3:11), 
                  encoding = "UTF-8")

speaker <-fread("mc_metadata.tsv", 
                header = T,
                sep = "\t", 
                encoding = "UTF-8")

refs <- rbind(mc, mandarin)

### getting speaker metadata
refs <- refs[speaker, speaker := speaker, on = c("text")]
refs[is.na(speaker), speaker := text]
unique(refs$speaker)

### preparing the data
refs[, I := .I]

# only including "phrase heads"
unique(refs$gfunc)
refs <- refs[!"" == refs$gfunc,]

# getting just 0 and pro (without np, other etc.)
unique(refs$gform)
refs <- refs[grepl("0|pro", gform), ]

# excluding f0
refs <- refs[!grepl("f0", gform), ]

# excluding 'nc'
refs <- refs[!grepl("nc", gform), ]

# excluding persian
refs <- refs[!grepl("persian", corpus), ]

unique(refs$gfunc)
unique(refs$gform)

# only including subjects, objects, goals and obliques
unique(refs$gfunc)
refs <- refs[!grepl("other", gfunc), ]
refs <- refs[!grepl("pred", gfunc), ]
refs <- refs[!grepl("poss", gfunc), ]
refs <- refs[!grepl("voc", gfunc), ]

unique(refs$gfunc)
refs[gfunc == "s_ds", gfunc := "s"]
refs <- refs[!grepl("appos", gfunc), ]
refs <- refs[!grepl("dt", gfunc), ]
refs <- refs[!grepl("lvc", gfunc), ]

unique(refs$gfunc)
refs[gfunc == "obl_dom", gfunc := "obl"]
refs[gfunc == "s_cpa_cv", gfunc := "s"]
refs[gfunc == "a_ds_cps", gfunc := "a"]
refs[gfunc == "a_a", gfunc := "a"]
refs[gfunc == "p_a", gfunc := "p"]

unique(refs$gfunc)
refs[gfunc == "a_cps_cv", gfunc := "a"]
refs[gfunc == "s_ds_cv", gfunc := "s"]
refs[gfunc == "a_cpa_in", gfunc := "a"]
refs[gfunc == "s_cps_in", gfunc := "s"]
refs[gfunc == "a_cps_pc", gfunc := "a"]

unique(refs$gfunc)
refs[grepl("ncs", gfunc), gfunc := "s"]
refs[grepl("nca", gfunc), gfunc := "a"]

unique(refs$gfunc)
refs[gfunc == "p_u", gfunc := "p"]
refs[gfunc == "a_u_ds", gfunc := "a"]

unique(refs$gfunc)
refs[grepl("a_ds", gfunc), gfunc := "a"]
refs[grepl("s_ds", gfunc), gfunc := "s"]
refs[grepl("s_cv", gfunc), gfunc := "s"]
refs[grepl("a_cv", gfunc), gfunc := "a"]
refs[grepl("a_cps", gfunc), gfunc := "a"]
refs[grepl("s_cps", gfunc), gfunc := "s"]
refs[gfunc == "s_in", gfunc := "s"]

unique(refs$gfunc)
refs[grepl("a_cpa", gfunc), gfunc := "a"]
refs[gfunc == "s_pc", gfunc := "s"]
refs[gfunc == "a_in", gfunc := "a"]
refs[grepl("s_cpa", gfunc), gfunc := "s"]
refs[gfunc == "a_u", gfunc := "a"]

unique(refs$gfunc)
refs[gfunc == "a_pc", gfunc := "a"]

unique(refs$gform)
refs[grepl("pro", gform), gform := "pro"]
refs[grepl("0", gform), gform := "0"]

unique(refs$gfunc)
unique(refs$gform)
unique(refs$graid)
unique(refs$corpus)
unique(refs$ganim)

### calculating tables

nAll <- refs[, .N, by = "corpus"]
nZero <- refs[grepl("0", gform), .N, by = "corpus", ]
testPro <- refs[grepl("pro", gform), .N, by = "corpus"]

counts <- nAll
counts$nZero <- nZero$N
counts$nPro <- nAll$N - nZero$N

counts$pZero <- round(100 * counts$nZero / counts$N, 2)
counts$pPro <- round(100 * counts$nPro / counts$N, 2)

setorder(counts, -pZero)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]

#### ggplot rate of zero
rateofzero <- ggplot(counts, 
                     aes(x = corpus, y = pZero, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", 
                               "lightblue", 
                               "lightcoral", 
                               "mediumturquoise", 
                               "lightblue",  
                               "mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise")) +
  theme(axis.text.x = element_text(size = 25,
                                   angle = 90, 
                                   hjust = 1),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25)) +
  labs(y= "Percentage of zero in comparison to pronouns", x = "") +
  scale_x_discrete(labels=c("Sanzhi", 
                            "Cypriot Greek", 
                            "Mandarin", 
                            "Northern Kurdish", 
                            "Tondano", 
                            "Teop", 
                            "Vera'a",
                            "English"))


rateofzero

############################# cite R, RStudio and packages

citation(package = "")
RStudio.Version()

