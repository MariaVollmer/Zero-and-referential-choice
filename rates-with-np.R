# This script was written by Maria Vollmer (University of Freiburg, 
# Australian National University).

# This script is used in the analysis of the following paper:
# Vollmer, Maria. Accepted with minor revision. Comparing zero and referential 
# choice in eight languages with a focus on Mandarin Chinese. Studies in language.

# If you use this script, please make sure to cite the paper above. 
# If you have any questions, you can contact me via email: mariacvollmer@gmail.com

# This script produces the rates of zero, pronoun and lexical expressions.

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

### preparing the data
refs[, I := .I]

# only including 'phrase heads' 
unique(refs$gfunc)
refs <- refs[!""==refs$gfunc,]

# getting just 0, np, and pro (without other etc.)
unique(refs$gform)
refs <- refs[grepl("0|np|pro", gform), ]

# only third person
refs <- refs[!grepl("1|2", ganim), ]
unique(refs$ganim)

# excluding f0
unique(refs$gform)
refs <- refs[!grepl("f0", gform), ]

# excluding 'nc'
refs <- refs[!grepl("nc", gform), ]

# excluding persian
refs <- refs[!grepl("persian", corpus), ]

# only including subjects, objects, goals, locations and obliques
unique(refs$gfunc)
refs <- refs[!grepl("other", gfunc), ]
refs <- refs[!grepl("pred", gfunc), ]
refs <- refs[!grepl("poss", gfunc), ]
refs <- refs[!grepl("voc", gfunc), ]
refs <- refs[!grepl("dt", gfunc), ]


unique(refs$gfunc)
refs[gfunc == "s_ds", gfunc := "s"]
refs <- refs[!grepl("appos", gfunc), ]
refs[gfunc == "p_lvc", gfunc := "p"]
refs <- refs[!grepl("lvc", gfunc), ]

unique(refs$gfunc)
refs[grepl("ncs", gfunc), gfunc := "s"]

unique(refs$gfunc)
refs[gfunc == "obl_dom", gfunc := "obl"]
refs[gfunc == "s_cpa_cv", gfunc := "s"]
refs[gfunc == "a_ds_cps", gfunc := "a"]
refs[gfunc == "a_a", gfunc := "a"]

unique(refs$gfunc)
refs[gfunc == "a_cpa", gfunc := "a"]
refs[gfunc == "a_cps_cv", gfunc := "a"]
refs[gfunc == "s_ds_cv", gfunc := "s"]
refs[gfunc == "a_cpa_in", gfunc := "a"]
refs[gfunc == "s_cps_in", gfunc := "s"]
refs[gfunc == "a_cps_pc", gfunc := "a"]

unique(refs$gfunc)
refs[gfunc == "p_u", gfunc := "p"]
refs[gfunc == "a_u_ds", gfunc := "a"]

unique(refs$gfunc)
refs[gfunc == "a_ds", gfunc := "a"]
refs[gfunc == "a_ds_cv", gfunc := "a"]
refs[gfunc == "a_ds_cps_cv", gfunc := "a"]
refs[gfunc == "s_cv", gfunc := "s"]
refs[gfunc == "a_cv", gfunc := "a"]
refs[gfunc == "s_cps_cv", gfunc := "s"]
refs[gfunc == "a_cps", gfunc := "a"]
refs[gfunc == "s_cps_ds_cv", gfunc := "s"]
refs[gfunc == "s_cps", gfunc := "s"]
refs[gfunc == "s_cps_ds", gfunc := "s"]

unique(refs$gfunc)
refs[gfunc == "s_in", gfunc := "s"]
refs[gfunc == "a_pc", gfunc := "a"]
refs[gfunc == "a_cps_in", gfunc := "a"]
refs[gfunc == "a_cpa", gfunc := "a"]
refs[gfunc == "s_pc", gfunc := "s"]
refs[gfunc == "a_cpa_pc", gfunc := "a"]
refs[gfunc == "a_in", gfunc := "a"]
refs[gfunc == "p_a", gfunc := "p"]


unique(refs$gfunc)
refs[gfunc == "s_cpa", gfunc := "s"]
refs[gfunc == "s_ds_pc", gfunc := "s"]
refs[gfunc == "a_cpa_cv", gfunc := "a"]
refs[gfunc == "s_cps_pc", gfunc := "s"]
refs[gfunc == "s_ds_cps_cv", gfunc := "s"]
refs[gfunc == "a_u", gfunc := "a"]

unique(refs$gform)
refs[grepl("np", gform), gform := "np"]
refs[grepl("pro", gform), gform := "pro"]
refs[grepl("0", gform), gform := "0"]

unique(refs$gform)
unique(refs$gfunc)
unique(refs$corpus)
unique(refs$text)
unique(refs$graid)
unique(refs$ganim)

### calculating tables

nAll <- refs[, .N, by = "corpus"]
nZero <- refs[grepl("0", gform), .N, by = "corpus", ]
nPro <- refs[grepl("pro", gform), .N, by = "corpus"]
testNP <- refs[grepl("np", gform), .N, by = "corpus"]

counts <- nAll
counts$nZero <- nZero$N
counts$nPro <- nPro$N
counts$nNP <- nAll$N - (nZero$N + nPro$N)

counts$pZero <- round(100 * counts$nZero / counts$N, 2)
counts$pPro <- round(100 * counts$nPro / counts$N, 2)
counts$pNP <- round(100 * counts$nNP / counts$N, 2)

setorder(counts, -pZero)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]

################################################################################
#### ggplot rate of zero
rateofzero <- ggplot(counts, 
                     aes(x = corpus, y = pZero, fill = corpus)) +
                geom_col(show.legend = FALSE) +
                scale_y_continuous(limits = c(0, 100)) +
                scale_fill_manual(values = c("mediumturquoise", "lightblue", 
                                             "mediumturquoise", "lightcoral",
                                             "mediumturquoise",  "lightblue", 
                                             "mediumturquoise", "lightblue")) +
                theme(
                  axis.text.x = element_text(size = 25, 
                                                 angle = 90, 
                                                 hjust = 1),
                  axis.text.y = element_text(size = 25),
                  axis.title.y = element_text(size = 25)) +
                labs(y= "Percentage of zero arguments", x = "") +
                scale_x_discrete(labels=c("Northern Kurdish", 
                                          "Sanzhi", 
                                          "Cypriot Greek", 
                                          "Mandarin", 
                                          "Tondano", 
                                          "Teop", 
                                          "Vera'a",
                                          "English"))

rateofzero


#### ggplot rate of pro 

setorder(counts, -pPro)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]

rateofpro <- ggplot(counts, 
                    aes(x = corpus, y = pPro, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise",  
                               "lightblue",
                               "mediumturquoise", 
                               "lightcoral", 
                               "mediumturquoise", 
                               "lightblue")) +
  theme(axis.text.x = element_text(size = 22, angle = 90, hjust = 1),
        axis.title.y = element_text(size = 22)) +
  labs(y= "Percentage of pronouns", x = "") +
  scale_x_discrete(labels=c("English", 
                            "Vera'a", 
                            "Teop", 
                            "Tondano", 
                            "Cypriot Greek", 
                            "Mandarin", 
                            "Sanzhi", 
                            "Northern Kurdish"))

 
rateofpro

#### ggplot rate of nps

setorder(counts, -pNP)
counts[, corpus := factor(corpus, ordered = TRUE, levels = counts$corpus)]

rateofnp <- ggplot(counts, aes(x = corpus, y = pNP, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("lightcoral", 
                               "mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise",  
                               "lightblue",
                               "mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise", 
                               "lightblue")) +
  theme(axis.text.x = element_text(size = 25, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25)) +
  labs(y = "Percentage of lexical expressions", x = "") +
  scale_x_discrete(labels = c("Mandarin", 
                              "Northern Kurdish", 
                              "Sanzhi", 
                              "English", 
                              "Cypriot Greek", 
                              "Vera'a", 
                              "Teop",
                              "Tondano"))

rateofnp

################################################## only subjects (refs2)

# filtering out subjects 

unique(refs$gfunc)
refs2 <- refs[grepl("s|a", gfunc), ]

unique(refs2$gfunc)
unique(refs2$ganim)
unique(refs2$graid)
unique(refs2$gform)

# creating table with counts and percentages

nAll2 <- refs2[, .N, by = "corpus"]
nZero2 <- refs2[grepl("0", gform), .N, by = "corpus", ]
nPro2 <- refs2[grepl("pro", gform), .N, by = "corpus"]
testNP2 <- refs2[grepl("np", gform), .N, by = "corpus"]

counts2 <- nAll2
counts2$nZero2 <- nZero2$N
counts2$nPro2 <- nPro2$N
counts2$nNP2 <- nAll2$N - (nZero2$N + nPro2$N)

counts2$pZero2 <- round(100 * counts2$nZero2 / counts2$N, 2)
counts2$pPro2 <- round(100 * counts2$nPro2 / counts2$N, 2)
counts2$pNP2 <- round(100 * counts2$nNP2 / counts2$N, 2)

setorder(counts2, -pZero2)
counts2[, corpus := factor(corpus, ordered = TRUE, levels = counts2$corpus)]

#### ggplot rate of zero (subjects)

rateofzerosubjects <- ggplot(counts2, 
                       aes(x = corpus, y = pZero2, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise", 
                               "lightcoral",
                               "mediumturquoise",  
                               "lightblue", 
                               "mediumturquoise", 
                               "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofzerosubjects


#### ggplot rate of pro (subjects)

setorder(counts2, -pPro2)
counts2[, corpus := factor(corpus, ordered = TRUE, levels = counts2$corpus)]

rateofprosubjects <- ggplot(counts2, 
                      aes(x = corpus, y = pPro2, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise",  
                               "lightblue", 
                               "lightcoral",
                               "mediumturquoise", 
                               "mediumturquoise", 
                               "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofprosubjects

#### ggplot rate of np subjects

setorder(counts2, -pNP2)
counts2[, corpus := factor(corpus, ordered = TRUE, levels = counts2$corpus)]

rateofnpsubjects <- ggplot(counts2, 
                           aes(x = corpus, y = pNP2, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("lightcoral", 
                               "mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise",  
                               "lightblue",
                               "mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise", 
                               "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

rateofnpsubjects

###################################### all syntactic functions except subjects

# filtering for all syntactic functions except subjects (refs3)

unique(refs$gfunc)

refs3 <- refs[grepl("p|g|l|obl", gfunc), ]

unique(refs3$gfunc)
unique(refs3$ganim)
unique(refs3$graid)
unique(refs3$gform)

# creating a table with counts and percentages (all except subjects)

nAll3 <- refs3[, .N, by = "corpus"]
nZero3 <- refs3[grepl("0", gform), .N, by = "corpus", ]
nPro3 <- refs3[grepl("pro", gform), .N, by = "corpus"]
testNP3 <- refs3[grepl("np", gform), .N, by = "corpus"]

counts3 <- nAll3
counts3$nZero3 <- nZero3$N
counts3$nPro3 <- nPro3$N
counts3$nNP3 <- nAll3$N - (nZero3$N + nPro3$N)

counts3$pZero3 <- round(100 * counts3$nZero3 / counts3$N, 2)
counts3$pPro3 <- round(100 * counts3$nPro3 / counts3$N, 2)
counts3$pNP3 <- round(100 * counts3$nNP3 / counts3$N, 2)

setorder(counts3, -pZero3)
counts3[, corpus := factor(corpus, ordered = TRUE, levels = counts3$corpus)]

#### ggplot rate of zero for all except subjects 
# = object, goal, oblique, location

rateofzeroexceptsubject <- ggplot(counts3, 
                                  aes(x = corpus, y = pZero3, fill = corpus)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise", 
                                "lightblue",  
                               "mediumturquoise", 
                               "lightcoral", 
                               "mediumturquoise", 
                               "lightblue")) +
  theme(axis.text.x = element_text(size = 25, 
                                   angle = 90, 
                                   hjust = 1),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25)) +
  labs(y= "Percentage of zero non-subjects", x = "") +
  scale_x_discrete(labels=c("Tondano", 
                            "Northern Kurdish", 
                            "Teop", 
                            "Sanzhi", 
                            "Vera'a", 
                            "Mandarin", 
                            "Cypriot Greek", 
                            "English"))


rateofzeroexceptsubject


################################################# only objects (refs4)

# filtering for only objects (refs4)

unique(refs$gfunc)

refs4 <- refs[grepl("p", gfunc), ]

unique(refs4$gfunc)
unique(refs4$gform)
unique(refs4$ganim)
unique(refs4$graid)
unique(refs4$corpus)

# calculating counts and percentages for only objects

nAll4 <- refs4[, .N, by = "corpus"]
nZero4 <- refs4[grepl("0", gform), .N, by = "corpus", ]
nPro4 <- refs4[grepl("pro", gform), .N, by = "corpus"]
testNP4 <- refs4[grepl("np", gform), .N, by = "corpus"]

counts4 <- nAll4
counts4$nZero4 <- nZero4$N
counts4$nPro4 <- nPro4$N
counts4$nNP4 <- nAll4$N - (nZero4$N + nPro4$N)

counts4$pZero4 <- round(100 * counts4$nZero4 / counts4$N, 2)
counts4$pPro4 <- round(100 * counts4$nPro4 / counts4$N, 2)
counts4$pNP4 <- round(100 * counts4$nNP4 / counts4$N, 2)

## Plotting barplot

setorder(counts4, -pZero4)
counts4[, corpus := factor(corpus, ordered = TRUE, levels = counts4$corpus)]

onlyobjects <- ggplot(counts4, 
                      aes(x = corpus, y = pZero4, fill = corpus)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("mediumturquoise", 
                               "lightblue", 
                               "mediumturquoise", 
                               "mediumturquoise",  
                               "lightcoral", 
                               "lightblue", 
                               "mediumturquoise", 
                               "lightblue")) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1))

onlyobjects

############################# cite R, RStudio and packages

citation(package = "")
RStudio.Version()

