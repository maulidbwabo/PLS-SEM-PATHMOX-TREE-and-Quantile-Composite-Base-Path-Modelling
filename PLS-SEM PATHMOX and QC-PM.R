##Paper one/ PLSPM AND PATHMOX
Hybrid <- read_csv("C:/Users/bwabo/OneDrive/Desktop/Ph.D. Thesis/Quantile PLSPM/Hybrid.csv")
Pathmox=Hybrid
summary(Pathmox)
Pathmox$Occupancy=as.factor(Pathmox$Occupancy)
Pathmox$Size=as.factor(Pathmox$Size)
Pathmox$Sex=as.factor(Pathmox$Sex)
Pathmox$`Education Level`=as.factor(Pathmox$`Education Level`)
str(Pathmox)
View(Pathmox)
dataz=subset(Pathmox,select = -c(17:20))
#path matrix
Knowledge= rep(0,5)
Sensing=rep(0,5)
Manage=rep(0,5)
Agility=rep(1,3)
Sustainable=rep(1,4)
##
## Inner model
px_path =rbind(Knowledge,Sensing,Manage,Agility,Sustainable)
innerplot(px_path)
## Outer model
px_outer= list(1:4, 5:7, 8:10, 11:13, 14:17)
##define vector of reflective modes
px.mod = rep("A", 5)
px_outer
## Run the plspm model
pxpls = plspm(dataz, px_path, px_outer, px.mod, scheme="centroid",
              scaled=FALSE)
pxpls
summary(pxpls)
plot(pxpls, what="inner")
plot(pxpls, what="loadings")
## PATHMOX USING LAVAAN
## structure
modelpx = "
AG~SC
AG~KS
AG~E
SP~AG+SC+KS+E
## Measurement model
    SC  =~ SC2 + SC3 + SC4
    KS  =~ KS1 + KS2 + KS3 + KS4
    E   =~ E1  + E2  + E3  
    AG  =~ AG1 + AG2 + AG3 
    SP  =~ SP1 + SP2 + SP3 "


##

##SCEM
modelSC <- "
# Structural model
AG~SC
AG~KS
AG~E
SP~AG+SC+KS+E
# Composite model
SC <~ SC2 + SC3 + SC4
KS <~  KS1 + KS2 + KS3 + KS4
E <~ E1  + E2  + E3 

# Reflective measurement model
AG  =~ AG1 + AG2 + AG3
SP  =~ SP1 + SP2 + SP3
"
path.res1 <- csem(.data = dataz, .model = modelSC)
path.res1$Estimates
path.res1$Information
summary(path.res1)
path.res$Information
##Identify the categorical variable to be used as input variables
Paxdata = Pathmox[,17:20]
str(Paxdata)
##
# Transform age into an ordered factor
Paxdata$Sex=as.factor(Paxdata$Sex)
Paxdata$Occupancy=as.factor(Paxdata$Occupancy)
Paxdata$Size=as.factor(Paxdata$Size)
Paxdata$`Education Level`=as.factor(Paxdata$`Education Level`)
##
str(Paxdata)
Paxdata$Sex= factor(Paxdata$Sex)
Paxdata$Occupancy=factor(Paxdata$Occupancy,levels = c("Manager","Employee"),ordered=T)
Paxdata$`Education Level`=factor(Paxdata$`Education Level`,
                                 levels = c("ACSE","Degree","Undergrad", "Graduated","Post"),ordered=T)
##Run Pathmox analysis
pax.pathmox = pls.pathmox(
  .model = modelpx ,
  .data = Pathmox,
  .catvar= Paxdata$Occupancy,
  .alpha = 0.05,
  .deep = 2
)
summary(pax.pathmox)
bar_impvar(pax.pathmox)
pax.pathmox$var_imp
pax.pathmox$other
## firm size 
pax.pathmox1 = pls.pathmox(
  .model = modelpx ,
  .data = Pathmox,
  .catvar= Paxdata$Size,
  .alpha = 0.05,
  .deep = 2
)
#
summary(pax.pathmox1)
pax.pathmox1$other
## Education level
pax.pathmox2 = pls.pathmox(
  .model = modelpx ,
  .data = Pathmox,
  .catvar= Paxdata$`Education Level`,
  .alpha = 0.05,
  .deep = 2
)
#summary
pax.pathmox2$
  pax.pathmox2$var_imp
summary(pax.pathmox2)
pax.pathmox2$terminal_paths
pax.pathmox2$Fc.r
plot(pax.pathmox2$Fg.r)
plot(pax.pathmox2$MOX)
plot(pax.pathmox2$terminal_paths)
#plotting 1 
plotx=plot(
  pax.pathmox,
  .root.col = "#CCFFFF",
  .node.col = "#99CCCC",
  .leaf.col = "#009999",
  .shadow.size = 0.004,
  .node.shadow = "#669999",
  .leaf.shadow = "#006666",
  .cex = 0.8,
  .seg.col = "#003333",
  .lwd = 2,
  .show.pval = TRUE,
  .pval.col = "#009999",
  .main = NULL,
  .cex.main = 1,
)
##plotting1
plot1=plot(
  pax.pathmox1,
  .root.col = "#CCFFFF",
  .node.col = "#99CCCC",
  .leaf.col = "#009999",
  .shadow.size = 0.004,
  .node.shadow = "#669999",
  .leaf.shadow = "#006666",
  .cex = 0.8,
  .seg.col = "#003333",
  .lwd = 2,
  .show.pval = TRUE,
  .pval.col = "#009999",
  .main = NULL,
  .cex.main = 1,
)
+
  plot2=plot(
    pax.pathmox2,
    .root.col = "#CCFFFF",
    .node.col = "#99CCCC",
    .leaf.col = "#009999",
    .shadow.size = 0.004,
    .node.shadow = "#669999",
    .leaf.shadow = "#006666",
    .cex = 0.8,
    .seg.col = "#003333",
    .lwd = 2,
    .show.pval = TRUE,
    .pval.col = "#009999",
    .main = NULL,
    .cex.main = 1,
  )

##Hybrid mutligroup analysis
# Run cSEM Model for Pathmox terminal nodes
pathlocalmodel = csem(
  .model = modelpx,
  .data = pax.pathmox2$hybrid)
pathlocalmodel
##Education level
pathlocalmodel1 = csem(
  .model = modelpx,
  .data = pax.pathmox$hybrid)
##Occupancy level
pathlocalmodel2 = csem(
  .model = modelpx,
  .data = pax.pathmox2$hybrid)
#
pathlocalmodel2
##
plot(pax.pathmox)
##
##Figure
figure= ggarrange(pax.pathmox,pax.pathmox1, pax.pathmox2,
                  labels = c("A", "B", "C"))
figure1=plot_grid(pax.pathmox, pax.pathmox1,pax.pathmox2,label_size = 12)
figure1
p3 <- ~plot(pax.pathmox, pax.pathmox1,pax.pathmox2)
p3
plot_grid(p3, labels = c("A", "B","C"), label_size = 12)
##patchwork
ggp<- (pax.pathmox + pax.pathmox1)/pax.pathmox2
ggp 
##
## Bootstrap
pathlocalmodel
pathlocalmodel1$`node 2`
pathlocalmodel2$`node 3`
# Check invariance and run MGA analysis (Hair et al., 2019)
testMICOM(pathlocalmodel1, .R = 60) 
to_compare <- "
#' # Structural model
AG~SC
AG~KS
AG~E
SP~AG+SC+KS+E
"
testMGD(pathlocalmodel1, .parameters_to_compare = to_compare,
        .R_bootstrap = 60,.approach_mgd = "Henseler")
## End(Not run)
assess(modelpx)
pathlocalmodel1
assess(pathlocalmodel)
predict(pathlocalmodel1)
Transparency =rep(0,4)
Accountability=rep(0,4)
Legal =c(1,1,0,0)
Value =c(1,1,1,0)
#bind the matrix

##
# list of blocks (outer model)
dy_blocks =list(1:4, 5:8, 9:12, 33:36)
##
km.out1=kmeans(data2,3,nstart = 20)
Hybrid_data=as.data.frame(Hybrid_data)
Hybrid_data
km.out
Hybrid_data=subset(Hybrid_data,select = -c(41:44))
dynamicP=subset(dynamic,select = -c(1:14))
view(data3)
dataz=subset(data3,select = -c(21:24))
dataz
str(dataz)
view(dataz)
##Lavaan
# fit the Holzinger and Swineford (1939) example
HS.model1 = 'Knowledge  =~ KS1 + KS2 + KS3 + KS4
             Sensing    =~ SC1 + SC2 + SC3 + SC4
             Experience =~ E1  + E2  + E3  + E4
             Agility    =~ AG1 + AG2 + AG3 + AG4
             Sustainable=~ SP1 + SP2 + SP3 + SP4'

##
fit = cfa(HS.model1, data=dataz)
##
pr.out1=prcomp(dataz , scale=TRUE)
pr.out1
pr.out1$sdev
pr.out1$rotation
dim(pr.out1$x)
biplot(pr.out1,scale = 0)
##
pr.out1$rotation=-pr.out1$rotation
pr.out1$x=-pr.out1$x
biplot (pr.out1 , scale =0)
##
pr.out1$sdev
#
pr.var=pr.out1$sdev^2
pr.var
##
pve=pr.var/sum(pr.var)
pve
##Kmeans
km.out3=kmeans (dataz,6, nstart =20)
km.out3

library(useful)
plot(km.out3,data = dataz)
##
km.outbest = FitKMeans(dataz, max.clusters=30, nstart=25,
                       seed=278613)
km.outbest
PlotHartigan(km.outbest)
library(cluster)
theGap1 = clusGap(dataz, FUNcluster=pam, K.max=20)
gapDF1 = as.data.frame(theGap1$Tab)
gapDF1
##
ggplot(gapDF1, aes(x=1:nrow(gapDF1))) +
  geom_line(aes(y=logW), color="blue") +
  geom_point(aes(y=logW), color="blue") +
  geom_line(aes(y=E.logW), color="green") +
  geom_point(aes(y=E.logW), color="green") +
  labs(x="Number of Clusters")
##
ggplot(gapDF1,aes(x=1:nrow(gapDF1))) +
  geom_line(aes(y=gap), color="red") +
  geom_point(aes(y=gap), color="red") +
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim),
                color="red") +
  labs(x="Number of Clusters", y="Gap")
##Quantile 
library(qcpm)
province
model <- "
ECOW ~ EDU
HEALTH ~ EDU + ECOW
# Reflective measurement model
EDU =~ EDU1 + EDU2 + EDU3 + EDU4 + EDU5 + EDU6 + EDU7
ECOW =~ ECOW1 + ECOW2 + ECOW3 + ECOW4 + ECOW5 + ECOW6
HEALTH =~ HEALTH1 + HEALTH2 + HEALTH3
"

##
model1 = "
AG~SC
AG~KS
AG~E
SP~AG+SC+KS+E
## Reflective measurement model
    SC  =~ SC1 + SC2 + SC3 + SC4
    KS  =~ KS1 + KS2 + KS3 + KS4
    E   =~ E1  + E2  + E3  + E4
    AG  =~ AG1 + AG2 + AG3 + AG4
    SP  =~ SP1 + SP2 + SP3 + SP4"

##
well.qcpm = qcpm(model1,dataz)
well.assessment = assessment(well.qcpm)
well.assessment
##
well.boot = boot(well.qcpm)
boot(well.qcpm, conf.level = 0.95, br = 5000)
##
qcpm_loads = as.data.frame(cbind(well.qcpm$outer.weights,well.qcpm$outer.loadings))
##
# add column names
colnames(qcpm_loads) = c("Global", paste("Class", 1:3, sep=""))

##
rebus_loads = as.data.frame(cbind(fut_pls$outer_model$loading,
                                  fut_rebus$loadings))
##Plot loadings 
##

plot(well.qcpm, what = "outer loadings", arr.width = 0.1)
##
plot(well.qcpm$outer.loadings)
plot(well.qcpm$path.coefficients, colpos = "#6890c4BB", colneg = "#f9675dBB",
     box.prop = 0.55, box.size = 0.08, box.cex = 1,
     box.col = "gray95", lcol = "gray95",
     txt.col = "gray40", arr.pos = 0.5, cex.txt = 0.9)
##
td1 <- ggplot(well.qcpm$data, aes(table, depth)) +
  xlim(50, 70) + ylim(50, 70)

##cluster analysis on scores 
qcpm_hclus1 = hclust(dist(well.qcpm$outer.loadings), method = "ward.D2")
# hierarchical cluster analysis on the LV scores
qcpm_hclus = hclust(dist(well.qcpm$path.coefficients), method = "ward.D2")

plot(qcpm_hclus1, xlab = "", sub = "", cex = 0.8)
abline(h = 1.3, col = "#bc014655", lwd = 4) 
##
qcclusters = cutree(qcpm_hclus, k = 3)
# latent variable scores in data frame
qcmp_scores = as.data.frame(well.qcpm$latent.scores)
##add cluster to coefficients
qcmp_scores$Cluster=as.factor(qcclusters)
fut_scores$Cluster = as.factor(clusters)
##

Hybrid_2_=as.data.frame(Hybrid_2_)
Hybrid_2_
data4=Hybrid_2_
datam=subset(data4,select = -c(21:24))
view(datam)
##
##
model3 = "
AG~SC
AG~KS
AG~E
SP~AG+SC+KS+E
## Reflective measurement model
    SC  =~ SC1 + SC2 + SC3 + SC4
    KS  =~ KS1 + KS2 + KS3 + KS4 
    E   =~ E1  + E2  + E3  + E4
    AG  =~ AG1 + AG2 + AG3 + AG4
    SP  =~ SP1 + SP2 + SP3 + SP4"

##
well.qcpm2 = qcpm(model3,datam)
well.assessment = assessment(well.qcpm)
well.assessment
##
well.boot = boot(well.qcpm)
##
sim_inner = matrix(c(0,0,0,0,0,0,1,1,0), 3, 3, byrow=TRUE)
##
# load dataset satisfaction
data(satisfaction)

# path matrix
IMAG = c(0,0,0,0,0,0)
EXPE = c(1,0,0,0,0,0)
QUAL = c(0,1,0,0,0,0)
VAL = c(0,1,1,0,0,0)
SAT = c(1,1,1,1,0,0)
LOY = c(1,0,0,0,1,0)
sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)

# plot diagram of path matrix
innerplot(sat_path)

# blocks of outer model
sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)

# vector of modes (reflective indicators)
sat_mod = rep("A", 6)

# apply plspm
satpls = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod,
               scaled = FALSE)
summary(satpls)
# plot diagram of the inner model
innerplot(satpls)

# plot loadings
outerplot(satpls, what = "loadings")

# plot outer weights
outerplot(satpls, what = "weights")

## End(Not run)
## Not run:
# Example of PATHMOX approach in customer satisfaction analysis
# (Spanish financial company).
# Model with 5 LVs (4 common factor: Image (IMAG), Value (VAL),
# Satisfaction (SAT), and Loyalty (LOY); and 1 composite construct:
# Quality (QUAL)
# load library and dataset csibank
library(genpathmx)
data("csibank")
# Define the model using the lavaan syntax. Use a set of regression formulas to define
# first the structural model and then the measurement model
CSImodel <- "
# Structural model
VAL ~ QUAL
SAT ~ IMAG + QUAL + VAL
LOY ~ IMAG + SAT
# Measurement model
# Composite
QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5 + qual6 + qual7
# Common factor
IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5 + imag6
VAL =~ val1 + val2 + val3 + val4
SAT =~ sat1 + sat2 + sat3
LOY =~ loy1 + loy2 + loy3"
# Run pathmox on one single variable
age = csibank[,2]
# Transform age into an ordered factor
age = factor(age, levels = c("<=25", "26-35", "36-45", "46-55",
                             "56-65", ">=66"),ordered = T)
csi.pathmox.age = pls.pathmox(
  .model = CSImodel ,
  .data = csibank,
  .catvar= age,
  .alpha = 0.05,
  .deep = 1
)
csi.pathmox.age
summary(csi.pathmox.age)
# Visualize the bar plot by comparing the nodes
bar_terminal(csi.pathmox.age, .LV = "SAT")
# Visualize the bar plot by comparing path coefficients
bar_terminal(csi.pathmox.age, .LV = "SAT", .bycoef = TRUE)
## End(Not run)


