#### Study on marine sessile communities on tiles, 
#### submerged at 1 m and 4 m at Queen Anne's Battery marina;
#### the two additional factors orientation (vertical or horizontal) and material (slate or plastic)
#### were included in the study design


### Univariate analyses
## load data

tile <- read.csv("~/PATH/tile.csv")

## labelling

# categories
depth <- tile$depth
ori <- tile$orientation
mat <- tile$material

# individual species
austro <- tile$Austrominius_modestus
bug <- tile$Bugula_neritina
schloss <- tile$Botryllus_schlosseri
leach <- tile$Botrylloides_leachii
ciona <- tile$Ciona_intestinalis
diplo <- tile$Diplosoma_spongiforme
jassa <- tile$Jassa_falcata
molgula <- tile$Molgula_sp
water <- tile$Watersipora_subtorquata
janua <- tile$Janua_heterostropha
conch <- tile$Ascidia_conchilega
aspersa <- tile$Ascidiella_aspersa
scabra <- tile$Ascidiella_scabra
phaeo <- tile$Phaeophyceae_encrusing
corella <- tile$Corella_eumyota
oculata <- tile$Molgula_oculata
manhat <- tile$Molgula_manhattensis
coro <- tile$Corophium_sp
ment <- tile$Ascidia_mentula
amphi <- tile$Amphipoda_sp
crust <- tile$Crustacea_sp1
asci <- tile$Ascidian_sp1
social <- tile$Molgula_socialis
scrupo <- tile$Scrupocellaria_sp
anomia <- tile$Anomia_ephippium

## calculating species diversity indices

species <- tile[,7:97]

library(vegan)

S <- specnumber(species)
N <- sum(species)
M <- (S-1)/log(N)
D <- diversity(species,index = "invsimpson", base =exp(1))
H <- diversity(species, index = "shannon", base =exp(1))
E <- D/S

## data visualisation

library(ggplot2)

# histograms

ggplot(species, aes(S)) + 
  geom_histogram(binwidth = 2)

ggplot(species, aes(M)) + 
  geom_histogram(binwidth = .3)

ggplot(species, aes(D)) + 
  geom_histogram(binwidth = .8)

ggplot(species, aes(H)) + 
  geom_histogram(binwidth = .2)

ggplot(species, aes(E)) + 
  geom_histogram(binwidth = .07)

# boxplots

ggplot(tile, aes(depth, S)) + 
  geom_boxplot()

ggplot(tile, aes(ori, S)) + 
  geom_boxplot()

ggplot(tile, aes(mat, S)) + 
  geom_boxplot()

# individual species

ggplot(tile, aes(mat, austro)) +
  geom_boxplot()

ggplot(tile, aes(mat, bug)) +
  geom_boxplot()

ggplot(tile, aes(ciona, bug)) +
  geom_point()

ggplot(tile, aes(jassa, bug)) +
  geom_point()

# the other species are ommitted here to save space...


## statistical analyses

library(car)

leveneTest(S~depth*ori*mat)
leveneTest(M~depth*ori*mat)
leveneTest(D~depth*ori*mat)
leveneTest(H~depth*ori*mat)
leveneTest(E~depth*ori*mat)
# all diversity data are homogenous


leveneTest(austro~depth*ori*mat) # significant
leveneTest(bug~depth*ori*mat)
leveneTest(schloss~depth*ori*mat)
leveneTest(leach~depth*ori*mat)
leveneTest(ciona~depth*ori*mat) # significant
leveneTest(diplo~depth*ori*mat)
leveneTest(jassa~depth*ori*mat)
leveneTest(molgula~depth*ori*mat)
leveneTest(janua~depth*ori*mat)
leveneTest(water~depth*ori*mat)
leveneTest(conch~depth*ori*mat)
leveneTest(aspersa~depth*ori*mat)
leveneTest(scabra~depth*ori*mat)
leveneTest(phaeo~depth*ori*mat)
leveneTest(corella~depth*ori*mat)
leveneTest(oculata~depth*ori*mat)
leveneTest(manhat~depth*ori*mat)
leveneTest(coro~depth*ori*mat)
leveneTest(ment~depth*ori*mat)
leveneTest(amphi~depth*ori*mat)
leveneTest(crust~depth*ori*mat)
leveneTest(asci~depth*ori*mat)
leveneTest(social~depth*ori*mat)
leveneTest(scrupo~depth*ori*mat)
leveneTest(anomia~depth*ori*mat)
# all in individual species data except for Austrominius modestus and
# Ciona intestinalis are homogenous

logciona <- log(ciona+1)
leveneTest(logciona~depth*ori*mat)
# ciona is still heterogenous

sqrtaustro <- sqrt(austro)
leveneTest(sqrtaustro~depth*ori*mat)
# now austro is homogenous


# Three-way ANOVA

# diversity indices

mod <- aov(S~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(M~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(D~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(H~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(E~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories


# individual species

mod <- aov(sqrtaustro~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(bug~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(schloss~depth*ori*mat)
summary(mod)
# significant interaction between depth and material

mod <- aov(leach~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(logciona~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(diplo~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(jassa~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(molgula~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(janua~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(water~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(aspersa~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(conch~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(scabra~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(phaeo~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(corella~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(manhat~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(oculata~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(coro~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(ment~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(amphi~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(crust~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(asci~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(social~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(scrupo~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories

mod <- aov(anomia~depth*ori*mat)
summary(mod)
# no significant interaction between any of the categories


# the only species with significant interaction
mod <- aov(schloss~depth*mat)
summary(mod)
TukeyHSD(mod)
# slate:plastic are different at deep; shallow:deep are different on slate;
# deep:slate and shallow:plastic are different


# t-test

# diversity indices

shapiro.test(S)
shapiro.test(M)
shapiro.test(D)
shapiro.test(H)
shapiro.test(E)
# all data are normally distributed

t.test(S~depth)
t.test(S~ori) # significant **
t.test(S~mat)

t.test(M~depth)
t.test(M~ori) # significant **
t.test(M~mat)

t.test(D~depth) 
t.test(D~ori) 
t.test(D~mat) 

t.test(H~depth) 
t.test(H~ori) # significant *
t.test(H~mat) 

t.test(E~depth) 
t.test(E~ori) 
t.test(E~mat) 

# only orientation significantly affects species richness, 
# Margalef index and Shannon-Wiener index


# individual species

shapiro.test(sqrtaustro) # non-normal and already sqrt transformed
shapiro.test(bug) # non-normal
shapiro.test(leach) # non-normal
shapiro.test(logciona) # non-normal and already log transformed
shapiro.test(water) # non-normal
shapiro.test(diplo) # non-normal
shapiro.test(jassa) # non-normal
shapiro.test(molgula) # non-normal
shapiro.test(janua) # non-normal
shapiro.test(aspersa) # non-normal
shapiro.test(conch) # non-normal
shapiro.test(scabra) # non-normal
shapiro.test(phaeo) # non-normal
shapiro.test(corella) # non-normal
shapiro.test(manhat) # non-normal
shapiro.test(oculata) # non-normal
shapiro.test(ment) # non-normal
shapiro.test(amphi) # non-normal
shapiro.test(crust) # non-normal
shapiro.test(asci) # non-normal
shapiro.test(social) # non-normal
shapiro.test(scrupo) # non-normal
shapiro.test(anomia) # non-normal
shapiro.test(coro) # non-normal
# all data are non-normally distributed

logaustro <- log(austro+1)
shapiro.test(logaustro) # non-normal
logbug <- log(bug+1)
shapiro.test(logbug) # non-normal
logleach <- log(leach+1)
shapiro.test(logleach) # non-normal
logwater <- log(water+1)
shapiro.test(logwater) # non-normal
logdiplo <- log(diplo+1)
shapiro.test(logdiplo) # non-normal
logjassa <- log(jassa+1)
shapiro.test(logjassa) # non-normal
logmolgula <- log(molgula+1)
shapiro.test(logmolgula) # non-normal
logjanua <- log(janua+1)
shapiro.test(logjanua) # non-normal
logaspersa <- log(aspersa+1)
shapiro.test(logaspersa) # non-normal
logconch <- log(conch+1)
shapiro.test(logconch) # non-normal
logscabra <- log(scabra+1)
shapiro.test(logscabra) # non-normal
logphaeo <- log(phaeo+1)
shapiro.test(logphaeo) # non-normal
logcorella <- log(corella+1)
shapiro.test(logcorella) # non-normal
logmanhat <- log(manhat+1)
shapiro.test(logmanhat) # non-normal
logoculata <- log(oculata+1)
shapiro.test(logoculata) # non-normal
logment <- log(ment+1)
shapiro.test(logment) # non-normal
logamphi <- log(amphi+1)
shapiro.test(logamphi) # non-normal
logcrust <- log(crust+1)
shapiro.test(logcrust) # non-normal
logasci <- log(asci+1)
shapiro.test(logasci) # non-normal
logsocial <- log(social+1)
shapiro.test(logsocial) # non-normal
logscrupo <- log(scrupo+1)
shapiro.test(logscrupo) # non-normal
loganomia <- log(anomia+1)
shapiro.test(loganomia) # non-normal
logcoro <- log(coro+1)
shapiro.test(logcoro) # non-normal
# none of the species data are homogenous; there are no prerequisites for a t-test

# wilcox  tests
wilcox.test(austro~depth)
wilcox.test(austro~mat) # significant **
wilcox.test(austro~ori)

wilcox.test(bug~depth)
wilcox.test(bug~mat) # significant *
wilcox.test(bug~ori)

wilcox.test(leach~depth)
wilcox.test(leach~mat)
wilcox.test(leach~ori)

wilcox.test(ciona~depth)
wilcox.test(ciona~mat)
wilcox.test(ciona~ori) # significant ***

wilcox.test(water~depth)
wilcox.test(water~mat)
wilcox.test(water~ori)

wilcox.test(diplo~depth)
wilcox.test(diplo~mat)
wilcox.test(diplo~ori)

wilcox.test(jassa~depth) # significant *
wilcox.test(jassa~mat)
wilcox.test(jassa~ori)

wilcox.test(molgula~depth)
wilcox.test(molgula~mat)
wilcox.test(molgula~ori)

wilcox.test(janua~depth)
wilcox.test(janua~mat)
wilcox.test(janua~ori)

wilcox.test(aspersa~depth)
wilcox.test(aspersa~mat)
wilcox.test(aspersa~ori) # significant *

wilcox.test(conch~depth)
wilcox.test(conch~mat)
wilcox.test(conch~ori) # significant **

wilcox.test(scabra~depth)
wilcox.test(scabra~mat)
wilcox.test(scabra~ori)

wilcox.test(phaeo~depth)
wilcox.test(phaeo~mat)
wilcox.test(phaeo~ori)

wilcox.test(corella~depth)
wilcox.test(corella~mat)
wilcox.test(corella~ori)

wilcox.test(manhat~depth)
wilcox.test(manhat~mat)
wilcox.test(manhat~ori)

wilcox.test(oculata~depth)
wilcox.test(oculata~mat)
wilcox.test(oculata~ori)

wilcox.test(ment~depth)
wilcox.test(ment~mat)
wilcox.test(ment~ori)

wilcox.test(amphi~depth)
wilcox.test(amphi~mat)
wilcox.test(amphi~ori)

wilcox.test(crust~depth)
wilcox.test(crust~mat)
wilcox.test(crust~ori)

wilcox.test(asci~depth)
wilcox.test(asci~mat)
wilcox.test(asci~ori)

wilcox.test(social~depth)
wilcox.test(social~mat)
wilcox.test(social~ori)

wilcox.test(scrupo~depth)
wilcox.test(scrupo~mat)
wilcox.test(scrupo~ori)

wilcox.test(anomia~depth)
wilcox.test(anomia~mat) # significant *
wilcox.test(anomia~ori)

wilcox.test(coro~depth)
wilcox.test(coro~mat)
wilcox.test(coro~ori)

# material significantly affects the abundance of Austrominius modestus
# material significantly affects the abundance of Bugula neritina
# orientation significantly affects the abundance of Ciona intestinalis
# depth significantly affects the abundance of Jassa falcata 
# orientation significantly affects the abundance of Ascidiella aspersa
# orientation significantly affects the abundance of Ascidia conchilega
# material significantly affects the abundance of Anomia ephippium
# there is no evidence for atrificial substrata as stepping stones for invasive species,
# except for T. inopinata and A. modestus, which are affected by material (but in the opposite way)
# however, that could be due to the fact that invasive species are ubiquitous in the harbour environment


# descriptive statistics

ms <- tapply(S, list(ori), mean)
sds <- tapply(S, list(ori), sd)
ns <- tapply(S, list(ori), length)
ses <- sds/sqrt(ns)

mm <- tapply(M, list(ori), mean)
sdm <- tapply(M, list(ori), sd)
nm <- tapply(M, list(ori), length)
sem <- sdm/sqrt(nm)

mh <- tapply(H, list(ori), mean)
sdh <- tapply(H, list(ori), sd)
nh <- tapply(H, list(ori), length)
seh <- sdh/sqrt(nh)


## graphical output

library(ggplot2)
# species diversity indices

mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 legend.key = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.title = element_text(size = 12, face = "bold"),
                 text = element_text(family = "Helvetica"))


Sdf <- data.frame(x = c("Horizontal", "Vertical"),
                  y = ms, se = ses)
Splot <- ggplot(Sdf, aes(x, y, ymin = y - se, ymax = y + se)) +
  geom_pointrange(aes(colour = x)) +
  scale_colour_manual(values = c("#b194c0", "#dacbe5"), guide = F) +
  ylim(6, 11) +
  ylab("Species richness (± SE)\n") +
  xlab("\nOrientation") +
  mytheme
  
  
Mdf <- data.frame(x = c("Horizontal", "Vertical"),
                  y = mm, se = sem)
Mplot <- ggplot(Mdf, aes(x, y, ymin = y - se, ymax = y + se)) +
  geom_pointrange(aes(colour = x)) +
  scale_colour_manual(values = c("#b194c0", "#dacbe5"), guide = F) +
  ylim(0.65, 1.2) +
  ylab("Margalef diversity index (± SE)\n") +
  xlab("\nOrientation") +
  mytheme


Hdf <- data.frame(x = c("Horizontal", "Vertical"),
                  y = mh, se = seh)
Hplot <- ggplot(Hdf, aes(x, y, ymin = y - se, ymax = y + se)) +
  ylim(1.25, 1.7) +
  geom_pointrange(aes(colour = x)) +
  scale_colour_manual(values = c("#b194c0", "#dacbe5"), guide = F) +
  ylab("Shannon-Wiener diversity index (± SE)\n") +
  xlab("\nOrientation") +
  mytheme

require(gridExtra)

grid.arrange(Splot, Mplot, Hplot, ncol = 3)

# two-way interaction
library(psych)
bot <- describeBy(schloss, list(depth, mat), mat = T, digits = 2)

# as points...
ggplot(bot, aes(group1, mean, ymin = mean - se, ymax = mean + se, colour = group2)) +
  geom_pointrange() +
  geom_line(aes(group1, mean, group = group2, linetype = group2)) +
  scale_linetype_manual(values = c(2,1), labels = c("p > 0.05", "p < 0.05"),
                        guide = guide_legend(title = "Significance" 
                                             )) +
  scale_colour_manual(values = c("#fac67e", "#afccdf"), labels = c("Plastic", "Slate"),
                      guide = guide_legend(title = "Tile material" 
                                           ) ) +
  annotate("text", label = "*", x = "Deep", y = 7, size = 10, colour = "grey50") +
  ylab("Abundance of Botryllus schlosseri (± SE)\n") +
  xlab("\nDepth") +
  theme(legend.position = c(.9, .8)) +
  mytheme


# ...or as a bar plot
botplot <- ggplot(bot, aes(group1, mean, fill = group2)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position = position_dodge(0.9),
                width = .1, lwd = .4, colour = "grey50") +
  scale_fill_manual(values = c("#fddca3", "#f5a54a"), 
                      guide = guide_legend(title = "Material", 
                                           labels = c("Plastic", "Slate"))) +
  annotate("text", label = c("a","a","b","a"), x = c(.775, 1.775, 1.225, 2.225),
           y = bot$mean + bot$se + 0.8, size = 4.5) +
  ylab(expression(atop("Abundance of "*italic("Botryllus schlosseri")*" (± SE)"*"\n"))) +
  xlab("\nDepth") +
  theme(legend.position = c(.8, .85)) +
  mytheme


# individual species


ausplot <- ggplot(tile, aes(mat, austro)) +
  geom_boxplot(aes(fill = mat), width = .3, colour = "grey50") +
  scale_fill_manual(values = c("#fddca3", "#f5a54a"), guide = F) +
  geom_jitter(colour = "grey50", width = .12, alpha = .5, size = 2) +
  xlab("\nMaterial") +
  ylab(expression(atop("Abundance of "*italic("Austrominius modestus")*"\n"))) +
  mytheme

bugplot <- ggplot(tile, aes(mat, bug)) +
  geom_boxplot(aes(fill = mat), width = .3, colour = "grey50") +
  scale_fill_manual(values = c("#fddca3", "#f5a54a"), guide = F) +
  geom_jitter(width = .12, alpha = .5, colour = "grey50", size = 2) +
  xlab("\nMaterial") +
  ylab(expression(atop("Abundance of "*italic("Bugula neritina")))) +
  mytheme

cionaplot <- ggplot(tile, aes(ori, ciona)) +
  geom_boxplot(aes(fill = ori), width = .3, colour = "grey50") +
  scale_fill_manual(values = c("#b194c0", "#dacbe5"), guide = F) +
  geom_jitter(width = .12, alpha = .5, colour = "grey50", size = 2) +
  xlab("\nOrientation") +
  ylab(expression(atop("Abundance of "*italic("Ciona intestinalis")))) +
  mytheme

jassaplot <- ggplot(tile, aes(depth, jassa)) +
  geom_boxplot(aes(fill = depth), width = .3, colour = "grey50") +
  scale_fill_manual(values = c("#008691", "#a1d5cf"), guide = F) +
  geom_jitter(width = .12, alpha = .5, colour = "grey50", size = 2) +
  xlab("\nDepth") +
  ylab(expression(atop("Abundance of "*italic("Jassa falcata")*"\n"))) +
  mytheme

aspersaplot <- ggplot(tile, aes(ori, aspersa)) +
  geom_boxplot(aes(fill = ori), width = .3, colour = "grey50") +
  scale_fill_manual(values = c("#b194c0", "#dacbe5"), guide = F) +
  geom_jitter(width = .12, alpha = .4, colour = "grey50", size = 2) +
  xlab("\nOrientation") +
  ylab(expression(atop("Abundance of "*italic("Ascidiella aspersa")*"\n"))) +
  mytheme

conchplot <- ggplot(tile, aes(ori, conch)) +
  geom_boxplot(aes(fill = ori), width = .3, colour = "grey50") +
  scale_fill_manual(values = c("#b194c0", "#dacbe5"), guide = F) +
  geom_jitter(width = .12, alpha = .4, colour = "grey50", size = 2) +
  xlab("\nOrientation") +
  ylab(expression(atop("Abundance of "*italic("Ascidia conchilega")*"\n"))) +
  mytheme

ggplot(tile, aes(mat, anomia)) +
  geom_boxplot(aes(fill = mat), width = .3, colour = "grey50") +
  scale_fill_manual(values = c("#fddca3", "#f5a54a"), guide = F) +
  geom_jitter(width = .12, alpha = .4, colour = "grey50", size = 2) +
  xlab("\nMaterial") +
  ylab(expression(atop("Abundance of "*italic("Anomia ephippium")*"\n"))) +
  mytheme

grid.arrange(botplot, ausplot, bugplot, ncol = 3)
grid.arrange(cionaplot, aspersaplot, conchplot, ncol = 3)
jassaplot


## species abundance models

k <- sample(nrow(species), 1)
mod <- fisherfit(species[k,])
mod
plot(mod)


mod <- prestondistr(species[k,])
mod
plot(mod)


mod <- radfit(species[1,])
mod
plot(mod)
mod <- radfit(species[20,])
mod
plot(mod)
mod <- radfit(species[40,])
mod
plot(mod)



### Multivariate analysis

library(vegan)
## most abundant species

abundance <- colSums(species)
abundance <- sort(abundance)
histogram(abundance)
abundance <- as.data.frame(abundance)
# View(abundance)

total <- colSums(abundance)
total

dominant <- abundance[74:91,]
dominant <- as.data.frame(dominant)
spec <- colSums(dominant)
spec

spec/total # 18 most abundant species make up 90% of total abundance
18/91 # 18 dominant species = 20% of species account for 90% of total abundance


# dominant <- abundance[72:91,]
# dominant <- as.data.frame(dominant)
# spec <- colSums(dominant)
# spec
# 
# spec/total # 20 most abundant species make up 91% of total abundance
# 20/91 # 20 dominant species = 22% of species account for 91% of total abundance
# 
# dominant <- abundance[69:91,]
# dominant <- as.data.frame(dominant)
# spec <- colSums(dominant)
# spec
# 
# spec/total # 23 most abundant species make up 93% of total abundance
# 23/91 # 23 dominant species = 25% of species account for 93% of total abundance


## depth

tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv", row.names = 1)
tile <- tile[,6:96]

depthmds <- metaMDS(tile, autotransform = F)
depthmds$stress
stressplot(depthmds)

# depthmds <- metaMDS(tile, k = 4) 
# depthmds$stress # we would need 4 dimensions to get stress < 0.2

ordiplot(depthmds, display = "site", type = "t")

ordiplot(depthmds, type = "n")
points(depthmds, display = "site", pch = 20, col = "black")
ordihull(depthmds, groups = depth, draw = "polygon", col = grey.colors(2))

require(goeveg)
limited <- ordiselect(species, depthmds, ablim = .2) 
limited # to top 20% of species in terms of abundance

ordiplot(depthmds, type = "n")
points(depthmds, display = "site", pch = 20, col = "black")
ordihull(depthmds, groups = depth, draw = "polygon", col = grey.colors(2))
points(depthmds, display = "species", select = limited)


## material
tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv")
tile <- tile[,2:97]
tile <- data.frame(tile, row.names = 1)
tile <- tile[,5:95]

matmds <- metaMDS(tile, autotransform = F)
matmds$stress
stressplot(matmds)

ordiplot(matmds, display = "site", type = "t")

ordiplot(matmds, type = "n")
points(matmds, display = "site", pch = 20, col = "black")
ordihull(matmds, groups = mat, draw = "polygon", col = grey.colors(2))


## orientation
tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv")
tile <- tile[,3:97]
tile <- data.frame(tile, row.names = 1)
tile <- tile[,4:94]

orimds <- metaMDS(tile, autotransform = F)
orimds$stress
stressplot(orimds)

ordiplot(orimds, display = "site", type = "t")

ordiplot(orimds, type = "n")
points(orimds, display = "site", pch = 20, col = "black")
ordihull(orimds, groups = ori, draw = "polygon", col = grey.colors(2))


### using ggplot for final nMDS plots

## depth

df <- data.frame(depthmds$points, group = depth)
ellipse <- ordiellipse(depthmds, depth, display = "sites", kind = "se",
                       conf = 0.95, label = F)

veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

dfell <- data.frame()
for(g in levels(df$group)) {
  dfell <- rbind(dfell, cbind(as.data.frame(with(df[df$group==g,],
    veganCovEllipse(ellipse[[g]]$cov,ellipse[[g]]$center,ellipse[[g]]$scale))),
                              group = g))
}


dnmds <- ggplot(df, aes(MDS1, MDS2)) +
  geom_point(aes(colour = group)) + 
  geom_polygon(data = dfell, aes(NMDS1, NMDS2, colour = group,
                                 fill = group), size = 0.7, alpha = 0.3) +
  scale_fill_manual(values = c("#008691", "#a1d5cf"), guide = F) +
  annotate("text", label = "Stress = 0.24", x = 1.1, y = 1, size = 3.6) +
  scale_colour_manual(values = c("#008691", "#a1d5cf"),
                      guide=guide_legend(title = "Depth")) +
  theme_bw() + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid = element_blank(),
                     legend.position = c(.13, .9),
                     legend.background = element_blank(),
                     legend.key = element_blank()) 



## material

df <- data.frame(matmds$points, group = mat)
ellipse <- ordiellipse(matmds, mat, display = "sites", kind = "se",
                       conf = 0.95, label = F)

veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

dfell <- data.frame()
for(g in levels(df$group)) {
  dfell <- rbind(dfell, cbind(as.data.frame(with(df[df$group==g,],
                                                 veganCovEllipse(ellipse[[g]]$cov,ellipse[[g]]$center,ellipse[[g]]$scale))),
                              group = g))
}


mnmds <- ggplot(df, aes(MDS1, MDS2)) +
  geom_point(aes(colour = group)) + 
  geom_polygon(data = dfell, aes(NMDS1, NMDS2, colour = group,
                                 fill = group), size = 0.7, alpha = 0.3) +
  scale_fill_manual(values = c("#fddca3", "#f5a54a"), guide = F) +
  annotate("text", label = "Stress = 0.24", x = 1.2, y = 1, size = 3.6) +
  scale_colour_manual(values = c("#fddca3", "#f5a54a"),
                      guide=guide_legend(title = "Material")) +
  theme_bw() + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid = element_blank(),
                     legend.position = c(.13, .9),
                     legend.background = element_blank(),
                     legend.key = element_blank()) 


## orientation

df <- data.frame(orimds$points, group = ori)
ellipse <- ordiellipse(orimds, ori, display = "sites", kind = "se",
                       conf = 0.95, label = F)

veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

dfell <- data.frame()
for(g in levels(df$group)) {
  dfell <- rbind(dfell, cbind(as.data.frame(with(df[df$group==g,],
                              veganCovEllipse(ellipse[[g]]$cov,ellipse[[g]]$center,ellipse[[g]]$scale))),
                              group = g))
}

onmds <- ggplot(df, aes(MDS1, MDS2)) +
  geom_point(aes(colour = group)) + 
  geom_polygon(data = dfell, aes(NMDS1, NMDS2, colour = group,
                                 fill = group), size = 0.7, alpha = 0.3) +
  scale_fill_manual(values = c("#b194c0", "#dacbe5"), guide = F) +
  annotate("text", label = "Stress = 0.24", x = 0.95, y = 1, size = 3.6) +
  scale_colour_manual(values = c("#b194c0", "#dacbe5"),
                      guide=guide_legend(title = "Orientation")) +
  theme_bw() + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid = element_blank(),
                     legend.position = c(.15, .9),
                     legend.background = element_blank(),
                     legend.key = element_blank()) 



grid.arrange(dnmds, onmds, mnmds, ncol = 3)
# these ordination plots would not usually be plotted because stress > 0.2 !


# ## clustering (no multidimensional scaling)
# library(vegan)
# # depth
# 
# tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv", row.names = 1)
# tile <- tile[,6:96]
# sqrttile <- sqrt(tile)
# 
# dis <- vegdist(sqrttile)
# cluster <- hclust(dis,"average")
# plot(cluster)
# 
# cluster <- hclust(dis,"complete")
# plot(cluster)
# 
# dendro <- as.dendrogram(cluster)
# plot(dendro, type = "rectangle", horiz = T)
# 
# library(ape)
# phylo <- as.phylo(cluster)
# depthcol <- c("black", rep("grey",2), rep("black",3), rep("grey",3), rep("black",5),
#               rep("grey",7), "black", "grey", rep("black",4), "grey", "black", "grey",
#               "black", rep("grey",2), "black", "grey", rep("black",2), rep("grey",3))
# plot(phylo, type = "fan", label.offset = .03,
#      tip.color = depthcol)
# 
# 
# require(clustsig)
# 
# sim <- simprof(tile, num.expected = 1000, num.simulated = 1000, method.cluster = "average", 
#                method.distance = "braycurtis", method.transform = "squareroot", alpha = .05)
# 
# summary(sim) # there are no separate clusters
# simprof.plot(sim) # error because there are no separate clusters
# # simprof tries to find groupings regardless of pre-defined groups
# 
# 
# # material
# 
# tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv")
# tile <- tile[,2:101]
# tile <- data.frame(tile, row.names = 1)
# tile <- tile[,5:99]
# 
# sqrttile <- sqrt(tile)
# 
# dis <- vegdist(sqrttile)
# cluster <- hclust(dis,"average")
# plot(cluster)
# 
# dendro <- as.dendrogram(cluster)
# plot(dendro, type = "rectangle", horiz = T)
# 
# library(ape)
# phylo <- as.phylo(cluster)
# matcol <- c(rep("grey",2), rep("black",2), rep("grey",3), rep("black",3), "grey",
#             rep("black",4), rep("grey",3), rep("black",2), rep("grey",2), "black",
#             rep("grey",3), rep("black",4), rep("grey",3), "black", "grey", rep("black",4),
#             "grey")
# plot(phylo, type = "fan", label.offset = .03,
#      tip.color = matcol)

# require(clustsig)
# 
# sim <- simprof(tile, num.expected = 1000, num.simulated = 1000, method.cluster = "average", 
#                method.distance = "braycurtis", method.transform = "squareroot", alpha = .05)
# 
# summary(sim) # there are no separate clusters
# simprof.plot(sim) # error

# # orientation

# tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv")
# tile <- tile[,3:97]
# tile <- data.frame(tile, row.names = 1)
# tile <- tile[,4:94]
# 
# sqrttile <- sqrt(tile)
# dis <- vegdist(sqrttile)
# cluster <- hclust(dis,"complete")
# plot(cluster)
# 
# dendro <- as.dendrogram(cluster)
# plot(dendro, type = "rectangle", horiz = T)
# 
# library(ape)
# phylo <- as.phylo(cluster)
# oricol <- c("grey", rep("black", 5), "grey", "black", "grey", "black", "grey", "black", 
#              rep("grey", 2), rep("black", 2), rep("grey", 2), "black", rep("grey", 3),
#              rep("black", 2), "grey", "black", rep("grey", 4), rep("black", 2), "grey",
#              rep("black", 2), "grey", "black", "grey", rep("black", 2))
# plot(phylo, type = "fan", label.offset = .03,
#      tip.color = oricol)


# require(clustsig)
# 
# sim <- simprof(tile, num.expected = 1000, num.simulated = 1000, method.cluster = "average", 
#                method.distance = "braycurtis", method.transform = "squareroot", alpha = .05)
# 
# summary(sim) # there are no separate clusters
# simprof.plot(sim) # error


## PERMANOVA

tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv")
biota <- tile[,7:97]
env <- tile[,4:6]

sqrtbiota <- sqrt(biota)
dis <- vegdist(sqrtbiota)

adonis(sqrtbiota~., env, permutations = 9999)
adonis(sqrtbiota~depth*orientation*material, env, permutations = 9999)

# adonis(sqrtbiota~orientation, env, permutations = 9999)
# adonis(sqrtbiota~depth, env, permutations = 9999)
# adonis(sqrtbiota~material, env, permutations = 9999)
# adonis(sqrtbiota~material+orientation, env, permutations = 9999)
# adonis(sqrtbiota~orientation+material, env, permutations = 9999)

betad <- with(env, betadisper(dis, depth))
permutest(betad) # homogenous
TukeyHSD(betad)

betad <- with(env, betadisper(dis, orientation))
permutest(betad) # homogenous
TukeyHSD(betad)

betad <- with(env, betadisper(dis, material))
permutest(betad) # homogenous
TukeyHSD(betad)

## SIMPER analysis

simper <- with(env, simper(sqrtbiota, depth, permutations = 9999))
summary(simper, ordered = T)
# best explained by Jassa falcata
# however depth is not significant as a difference in community structure

simper <- with(env, simper(sqrtbiota, orientation, permutations = 9999))
summary(simper, ordered = T)
# best explained by Ciona intestinalis, Ascidiella aspersa, 
# Ascidia conchilega and Janua heterostropha

simper <- with(env, simper(sqrtbiota, material, permutations = 9999))
summary(simper, ordered = T)
# best explained by Austrominius modestus


## Environmental fitting

envcor <- envfit(depthmds, env, permutations = 9999)
envcor

envcor <- envfit(matmds, env, permutations = 9999)
envcor

envcor <- envfit(orimds, env, permutations = 9999)
envcor

# environmental fitting is not necessary in this case, as the only environmental factors at play are the 
# three factors depth, orientationa and material that were already separately analysed using PERMANOVA
# furthermore, there are no continuous environmental variables, so analysis with bioenv() is not justified


## Principal Coordinate Analysis

# material
tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv")
tile <- tile[,2:97]
tile <- data.frame(tile, row.names = 1)
tile <- tile[,5:95]

sqrttile <- sqrt(tile)

dis <- vegdist(sqrttile)
pcoa <- cmdscale(dis, k = (nrow(sqrttile)-1), eig = T)
ordiplot(scores(pcoa)[, c(1,2)], type = "t")
sp <- wascores(pcoa$points[,1:2], sqrttile)
text(sp, rownames(sp), cex = 0.7, col = "red")

# orientation
tile <- read.csv("~/Desktop/Plymouth University/MBIO217/Tile diversity/tile.csv")
tile <- tile[,3:97]
tile <- data.frame(tile, row.names = 1)
tile <- tile[,4:94]

sqrttile <- sqrt(tile)
dis <- vegdist(sqrttile)
pcoa <- cmdscale(dis, k = (nrow(sqrttile)-1), eig = T)
ordiplot(scores(pcoa)[, c(1,2)], type = "t")


### Plotting Map
require(ggplot2)
# require(maptools)
require(ggmap)
register_google(key="AIzaSyA0flWUPdyXto2khJDr9Jvgs4QNITGi2MM")

# UK <- geocode("UK")
# Britain <- ggmap(get_map(UK, zoom = 5, maptype = "toner-lite")) + 
#   coord_map()+ theme(plot.background = element_blank(),
#                      panel.background = element_blank()) +
#   xlab("Longitude") + ylab("Latitude") +
#   geom_point(x = -4.131199, y = 50.365790, 
#              size = 2.5, colour = "#f5a54a") + mytheme
# 
# 
# UK <- readShapePoly("~/Desktop/Plymouth University/MBIO217/Tile diversity/GB/gb_10km.shp")
# 
# ggplot(UK)+
#   geom_polygon(mapping = aes(x = long, y = lat, group = group),
#                color="#D9DBCE",fill="#F1F5E0")+
#   coord_map()+theme(plot.background = element_blank(),
#                     panel.background = element_blank())

plym <- geocode("Queen Anne Battery, Plymouth, UK")
plym <- c(-4.145755, 50.3615, -4.1245, 50.370)


Plymouth <- ggmap(get_map(plym, maptype = "toner-lite")) + 
  coord_map()+ theme(plot.background = element_blank(),
                     panel.background = element_blank(),
                     axis.title = element_blank(),
                     axis.line = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text = element_blank()) +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(x = -4.131199, y = 50.365790, size = 3, fill = "#fbb36d", colour = "#ef3a4d", pch = 21) +
  annotate("segment", x = -4.1279, xend = -4.125, y = 50.3622, yend = 50.3622) +
  annotate("text", label = "200 m", x = -4.12645, y = 50.3619, size = 5, family = "Helvetica")



UK <- map_data("world", region = "UK")
# Ireland <- map_data("world", region = "Ireland")
# one <- rbind(Ireland, UK)

Britain <- ggplot(UK)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               color="#8B8B8B",fill="#D9D9D9")+
  coord_map()+theme(plot.background = element_rect(fill = "white"),
                    panel.background = element_blank(),
                    axis.title = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank()) +
  geom_point(x = -4.131199, y = 50.365790, size = 3, fill = "#fbb36d", colour = "#ef3a4d", pch = 21) +
  annotate("segment", x = -6, xend = -3.15, y = 49.5, yend = 49.5, size = 1.5) +
  annotate("text", label = "200 km", x = -4.57, y = 48.8, size = 12, family = "Helvetica")


Plymouth
Britain

require(gridExtra)
grid.arrange(Britain, Plymouth, ncol = 2)

### Clean up
detach(package:car)
detach(package:vegan)
detach(package:psych)
detach(package:ggplot2)
detach(package:gridExtra)
detach(package:goeveg)
detach(package:ggmap)
rm(list = ls())
graphics.off()
cat("\014")

