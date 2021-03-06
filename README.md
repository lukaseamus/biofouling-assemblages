# biofouling-assemblages
Ecological analyses of biofouling assemblages on settlement tiles.


This repository contains two csv files and one R file. The csv files 
contain the raw (raw.csv) and processed (tile.csv) data. The R script (tile.R)
contains all analytical and data visualisation procedures. To reproduce 
the analysis pipeline, the processed data (tile.csv) ought to be used in
conjunction with the R script (tile.R).

Analyses include multi-way ANOVA, Student's t.test, Wilcoxon signed-rank test,
PERMANOVA, SIMPER, SIMPROF and PCO. Visualisations are mostly plotted with ggplot2 
and include histograms, scatterplots, barplots, boxplots, dendrograms, nMDS plots
and maps.

Packages used, include car, vegan, goeveg, clustsig, psych, ggplot2, gridExtra, 
ggmap, and ape. 


Luka Seamus Wright

University of Plymouth
