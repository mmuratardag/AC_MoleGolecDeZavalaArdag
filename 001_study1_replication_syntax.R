
packages <- c("reshape2", "tidyverse", "knitr", "haven", "gridExtra", "naniar", "janitor", "ggrepel", "ggcorrplot","sjPlot",
              "psych", "lavaan", "semPlot", "semTools", "mirt", "sirt", "MplusAutomation", "OpenMx", "semtree",
              "processR",
              "future")

for(i in packages){
  if(!require(i, character.only = T)) install.packages(i, dependencies=T)
  library(i, character.only = T)
}
options(knitr.kable.NA = '') # for proper tables
plan(multiprocess)

load("study1_data.Rdata")
d <- d1
rm(d1)
colnames(d)

#########################################
#
# Descriptive statistics
#
#########################################
describe(d[, c("age","libcon")])
tabyl(d$sex)
tabyl(d$libcon)

#########################################
#
# National collective narcissism (CNn)
#
#########################################

CNnm <- d %>% select(cnn1,cnn2,cnn3,cnn4,cnn5)
describe(CNnm)
sjt.itemanalysis(CNnm, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(CNnm),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Collective narcissism (national) items used in study 1")
CNn_irt <- mirt(CNnm,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(CNn_irt)
coef (CNn_irt, IRTpars=T)
CNn_iif <- plot(CNn_irt, type = "infotrace", main = "Item Information Curves (IIC)")
CNn_ccc <- plot(CNn_irt, type = "trace", main = "Category Characteristic Curves (CCC)")
CNn_iise <- plot(CNn_irt, type = "infoSE", main = "Test Information & Standard Error (I & SE)")
CNn_tcc <- plot(CNn_irt, main = "Test Characteristic Curves (TCC)")
gridExtra::grid.arrange(CNn_iif,CNn_ccc,CNn_iise,CNn_tcc, ncol = 2, nrow=2, top = "Collective narcissism (national)")

CNn_CFA <- "CNn =~ cnn1 + cnn2 + cnn3 + cnn4 + cnn5"
CNn_CFA_fit <- cfa(CNn_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(CNn_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(CNn_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Collective narcissism (national) Study 1",line=0.5)
round(reliability(CNn_CFA_fit),2)

#########################################
#
# Religious collective narcissism (Cnc)
#
#########################################

CNrm <- d %>% select(cnc1,cnc2,cnc3,cnc4,cnc5)
describe(CNrm)
sjt.itemanalysis(CNnm, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(CNrm),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Collective narcissism (Catholic) items used in study 1")
CNr_irt <- mirt(CNrm,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(CNr_irt)
coef (CNr_irt, IRTpars=T, simplfy=T)
CNr_iif <- plot(CNr_irt, type = "infotrace", main = "IIC")
CNr_ccc <- plot(CNr_irt, type = "trace", main = "CCC")
CNr_iise <- plot(CNr_irt, type = "infoSE", main = "I & SE")
CNr_tcc <- plot(CNr_irt, main = "TCC")
gridExtra::grid.arrange(CNr_iif,CNr_ccc,CNr_iise,CNr_tcc, ncol = 2, nrow=2, top = "Collective narcissism (Catholic)")

CNr_CFA <- "CNr =~ cnc1 + cnc2 + cnc3 + cnc4 + cnc5"
CNr_CFA_fit <- cfa(CNr_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(CNr_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(CNr_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Collective narcissism (religious) Study 1",line=0.5)
round(reliability(CNr_CFA_fit),2)

###################################################
#
# National collective narcissism (CNn)
# Religious collective narcissism (Cnc)
# HO collective narcissism
#
###################################################
gCN_CFA <- "
CNn =~ cnn1 + cnn2 + cnn3 + cnn4 + cnn5
CNr =~ cnc1 + cnc2 + cnc3 + cnc4 + cnc5
gCN =~ CNn + CNr
"
gCN_CFA_fit <- cfa(gCN_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(gCN_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(gCN_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: general (HO) Collective narcissism Study 1",line=0.5)
round(reliability(gCN_CFA_fit),2)
round(reliabilityL2(gCN_CFA_fit, "gCN"),2)

gCN_fs <- lavPredict(gCN_CFA_fit, type = "lv", method = "EBM", label = T, fsm = T)
gCN_fs <- as.data.frame(gCN_fs)
describe(gCN_fs)
ggplot(melt(gCN_fs), aes(x=value)) + geom_histogram() + facet_wrap(~variable)
d$gCN <- scales::rescale(gCN_fs$gCN, to = c(0, 1), from = range(gCN_fs$gCN, na.rm = F, finite = T))
d$nCN <- scales::rescale(gCN_fs$CNn, to = c(0, 1), from = range(gCN_fs$CNn, na.rm = F, finite = T))
d$rCN <- scales::rescale(gCN_fs$CNr, to = c(0, 1), from = range(gCN_fs$CNr, na.rm = F, finite = T))
colnames(d)

rm(list=setdiff(ls(), "d"))

#########################################
#
# gay threat & gender roles
#
#########################################

GAYthrM <- d %>% select(gaythreat1,gaythreat2,gaythreat3,gaythreat4,gaythreat5,
                        gaythreat6,gaythreat7,gaythreat8,gaythreat9,gaythreat10,
                        gaythreat11,gaythreat12,gaythreat13,gaythreat14,gaythreat15,gaythreat16)
describe(GAYthrM)
sjt.itemanalysis(GAYthrM, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(GAYthrM),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Gay threat + national & religious gender roles items used in study 1")

paGT <- fa.parallel (GAYthrM, fm="ml", fa="fa", n.iter=1000, cor="cor", sim=T, use="pairwise", main = "PA National + Religious Gender roles & gay threat items")
paGT
vssGT <- vss (GAYthrM, n = 8, fm="mle", rotate = "oblimin", title = "VSS Gay Threat Items")
vssGT
cormatGT <- cor(GAYthrM)
ggcorrplot(cormatGT, hc.order = T,
           title = "Gay Threat correlation matrix",
           type = "lower", 
           lab = T,
           #p.mat = p.mat,
           lab_size = 2, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"),
           tl.cex = 8,
           ggtheme=theme_bw)

GT2d  <- fa(r=cormatGT, nfactors=2, fm="ml", rotate="oblimin")
print (GT2d, cut=0.3, sort = T)
fa.diagram (GT2d)
GT3d  <- fa(r=cormatGT, nfactors=3, fm="ml", rotate="oblimin")
print (GT3d, cut=0.3, sort = T)
fa.diagram (GT3d)
GT4d  <- fa(r=cormatGT, nfactors=4, fm="ml", rotate="oblimin")
print (GT4d, cut=0.3, sort = T)
fa.diagram (GT4d)

model_ESEM <- '
efa("efa1")*f1 + efa("efa1")*f2 +
efa("efa1")*f3 =~ gaythreat1 + gaythreat2 + gaythreat3 + gaythreat4 + gaythreat5 + gaythreat6 + gaythreat7 + gaythreat8 + gaythreat9 + gaythreat10 + gaythreat11 + gaythreat12 + gaythreat13 + gaythreat14 + gaythreat15 + gaythreat16
'

fit_ESEM <- sem(model = model_ESEM, data = d, information = "observed",
                verbose = TRUE, rotation = "geomin", 
                rotation.se = "delta", # or "bordered"
                # mimic Mplus
                meanstructure = TRUE,
                rotation.args = list(rstarts = 30, row.weights = "none",  
                                     algorithm = "gpa", orthogonal = FALSE,
                                     jac.init.rot = TRUE,
                                     std.ov = TRUE, # row standard = correlation
                                     geomin.epsilon = 0.0001))
summary(fit_ESEM, fit.measures=T, standardized=T, rsquare=T)
semPaths(fit_ESEM, "mod", "std", rotation = 4, layout = "tree3", edge.label.cex=1, curve = 1)
title("ESEM: National + religious gender roles & gay threat items Study 1",line=1.2)
parameterEstimates(fit_ESEM, standardized=T) %>% 
  filter(op == "=~") %>% 
  mutate(stars = ifelse(pvalue < .001, "***", 
                        ifelse(pvalue < .01, "**", 
                               ifelse(pvalue < .05, "*", "")))) %>%
  select('Latent Factor'=lhs, 
         Indicator=rhs, 
         B=est, 
         SE=se, Z=z, 
         Beta=std.all, 
         sig=stars) %>% mutate_if(is.numeric, round, 3)

GT3d_CFA <- "
ReligiousGR =~ gaythreat2 + gaythreat3 + gaythreat4 + gaythreat13
NationalGR =~ gaythreat1 + gaythreat5 + gaythreat6 + gaythreat7 + gaythreat8
GayThr =~ gaythreat10 + gaythreat11 + gaythreat12 + gaythreat14 + gaythreat15
"
GT3d_CFA_fit <- cfa(GT3d_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GT3d_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GT3d_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4, curve = 2)
title("CFA: Gender roles & gay threat Study 1",line=0.5)
round(reliability(GT3d_CFA_fit),2)

GAYthrM_RGR <- d %>% select(gaythreat2 , gaythreat3 , gaythreat4 , gaythreat13)
GAYthrM_TGR <- d %>% select(gaythreat1 , gaythreat5 , gaythreat6 , gaythreat7, gaythreat8)
GAYthrM_gaythr <- d %>% select(gaythreat10 , gaythreat11 , gaythreat12 , gaythreat14 , gaythreat15)

GAYthrM_RGR_irt <- mirt(GAYthrM_RGR,
                        model = 1,
                        itemtype = "graded",
                        technical=list(removeEmptyRows=T))
summary(GAYthrM_RGR_irt)
coef (GAYthrM_RGR_irt, IRTpars=T)
GAYthrM_RGR_iif <- plot(GAYthrM_RGR_irt, type = "infotrace", main = "IIC")
GAYthrM_RGR_ccc <- plot(GAYthrM_RGR_irt, type = "trace", main = "CCC")
GAYthrM_RGR_iise <- plot(GAYthrM_RGR_irt, type = "infoSE", main = "I & SE")
GAYthrM_RGR_tcc <- plot(GAYthrM_RGR_irt, main = "TCC")
gridExtra::grid.arrange(GAYthrM_RGR_iif,GAYthrM_RGR_ccc,GAYthrM_RGR_iise,GAYthrM_RGR_tcc, ncol = 2, nrow=2, top = "Religious Gender Roles")

GAYthrM_TGR_irt <- mirt(GAYthrM_TGR,
                        model = 1,
                        itemtype = "graded",
                        technical=list(removeEmptyRows=T))
summary(GAYthrM_TGR_irt)
coef (GAYthrM_TGR_irt, IRTpars=T)
GAYthrM_TGR_iif <- plot(GAYthrM_TGR_irt, type = "infotrace", main = "IIC")
GAYthrM_TGR_ccc <- plot(GAYthrM_TGR_irt, type = "trace", main = "CCC")
GAYthrM_TGR_iise <- plot(GAYthrM_TGR_irt, type = "infoSE", main = "I & SE")
GAYthrM_TGR_tcc <- plot(GAYthrM_TGR_irt, main = "TCC")
gridExtra::grid.arrange(GAYthrM_TGR_iif,GAYthrM_TGR_ccc,GAYthrM_TGR_iise,GAYthrM_TGR_tcc, ncol = 2, nrow=2, top = "National Gender Roles")

GAYthrM_GT_irt <- mirt(GAYthrM_gaythr,
                       model = 1,
                       itemtype = "graded",
                       technical=list(removeEmptyRows=T))
summary(GAYthrM_GT_irt)
coef (GAYthrM_GT_irt, IRTpars=T)
GAYthrM_GT_iif <- plot(GAYthrM_GT_irt, type = "infotrace", main = "IIC")
GAYthrM_GT_ccc <- plot(GAYthrM_GT_irt, type = "trace", main = "CCC")
GAYthrM_GT_iise <- plot(GAYthrM_GT_irt, type = "infoSE", main = "I & SE")
GAYthrM_GT_tcc <- plot(GAYthrM_GT_irt, main = "TCC")
gridExtra::grid.arrange(GAYthrM_GT_iif,GAYthrM_GT_ccc,GAYthrM_GT_iise,GAYthrM_GT_tcc, ncol = 2, nrow=2, top = "Gay Threat")

GT3d_R_CFA <- "
ReligiousGR =~ gaythreat2 + gaythreat3 + gaythreat4 + gaythreat13
NationalGR =~ gaythreat5 + gaythreat6 + gaythreat8
GayThr =~ gaythreat12 + gaythreat14
"
GT3d_R_CFA_fit <- cfa(GT3d_R_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GT3d_R_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GT3d_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4, curve = 2)
title("CFA: Refined Gender roles & gay threat",line=0.5)
round(reliability(GT3d_R_CFA_fit),2)

anova(GT3d_CFA_fit, GT3d_R_CFA_fit)
cf <- compareFit(GT3d_CFA_fit, GT3d_R_CFA_fit, nested=T)
summary(cf)

par(mfrow=c(1,2))
semPaths(GT3d_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4, curve = 2)
title("CFA: Gender roles & gay threat Study 1",line=0.5)
semPaths(GT3d_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4, curve = 2)
title("CFA: Refined Gender roles & gay threat",line=0.5)
dev.off()

GT_HO_CFA <- "
relGR =~ gaythreat2 + gaythreat3 + gaythreat4 + gaythreat13
natGR =~ gaythreat5 + gaythreat6 + gaythreat8
natGT =~ gaythreat12 + gaythreat14
HO_GR =~ relGR + natGR
"
GT_HO_CFA_fit <- cfa(GT_HO_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GT_HO_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GT_HO_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Refined Gender roles & gay threat with HO gender roles",line=1.2)
round(reliabilityL2(GT_HO_CFA_fit, "HO_GR"),2)
GT_fs <- lavPredict(GT_HO_CFA_fit, type = "lv", method = "EBM", label = T, fsm = T)
GT_fs <- as.data.frame(GT_fs)
describe(GT_fs)
ggplot(melt(GT_fs), aes(x=value)) + geom_histogram() + facet_wrap(~variable)
d$hoGR <- scales::rescale(GT_fs$HO_GR, to = c(0,1), from = range(GT_fs$HO_GR, na.rm = F, finite = T))
d$nGT <- scales::rescale(GT_fs$natGT, to = c(0,1), from = range(GT_fs$natGT, na.rm = F, finite = T))
d$rGR <- scales::rescale(GT_fs$relGR, to = c(0,1), from = range(GT_fs$relGR, na.rm = F, finite = T))
d$nGR <- scales::rescale(GT_fs$natGR, to = c(0,1), from = range(GT_fs$natGR, na.rm = F, finite = T))
colnames(d)

rm(list=setdiff(ls(), "d"))

#########################################
#
# Gender roles (GendRole)
#
#########################################

GRm <- d %>% select(gendrole1,gendrole2,gendrole3,gendrole4,gendrole5,
                    gendrole6,gendrole7,gendrole8,gendrole9,gendrole10,
                    gendrole11,gendrole12,gendrole13,gendrole14,gendrole15)
describe(GRm)
sjt.itemanalysis(GRm, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(GRm),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Traditional beliefs about gender roles & gender identity scale items used in study 1")

GR_b_irt <- mirt(GRm[,1:8],
                 model = 1,
                 itemtype = "graded",
                 technical=list(removeEmptyRows=T))
summary(GR_b_irt)
coef (GR_b_irt, IRTpars=T)
GR_b_iif <- plot(GR_b_irt, type = "infotrace", main = "IIC")
GR_b_ccc <- plot(GR_b_irt, type = "trace", main = "CCC")
GR_b_iise <- plot(GR_b_irt, type = "infoSE", main = "I & SE")
GR_b_tcc <- plot(GR_b_irt, main = "TCC")
gridExtra::grid.arrange(GR_b_iif,GR_b_ccc,GR_b_iise,GR_b_tcc, ncol = 2, nrow=2, top = "Traditional beliefs about gender")

GR_i_irt <- mirt(GRm[,9:15],
                 model = 1,
                 itemtype = "graded",
                 technical=list(removeEmptyRows=T))
summary(GR_i_irt)
coef (GR_i_irt, IRTpars=T)
GR_i_iif <- plot(GR_i_irt, type = "infotrace", main = "IIC")
GR_i_ccc <- plot(GR_i_irt, type = "trace", main = "CCC")
GR_i_iise <- plot(GR_i_irt, type = "infoSE", main = "I & SE")
GR_i_tcc <- plot(GR_i_irt, main = "TCC")
gridExtra::grid.arrange(GR_i_iif,GR_i_ccc,GR_i_iise,GR_i_tcc, ncol = 2, nrow=2, top = "Traditional gender identity")

GR_CFA <- "
Gbe =~ gendrole1 + gendrole2 + gendrole3 + gendrole4 + gendrole5 + gendrole6 + gendrole7 + gendrole8
Gid =~ gendrole9 + gendrole10 + gendrole11 + gendrole12 + gendrole13 + gendrole14 + gendrole15
"
GR_CFA_fit <- cfa(GR_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GR_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GR_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Traditional gender roles Scale Study 1",line=0.5)
round(reliability(GR_CFA_fit),2)

GR_R_CFA <- "
Gbe =~ gendrole1 + gendrole2 + gendrole6 + gendrole7
Gid =~ gendrole9 + gendrole11 + gendrole12 + gendrole13
"
GR_R_CFA_fit <- cfa(GR_R_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GR_R_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GR_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Refined Traditional gender roles Scale Study 1",line=1)
round(reliability(GR_R_CFA_fit),2)

par(mfrow=c(1,2))
semPaths(GR_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Traditional gender roles Scale Study 1",line=0.5)
semPaths(GR_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Refined Traditional gender roles Scale Study 1",line=1)
dev.off()

anova(GR_CFA_fit, GR_R_CFA_fit)
cf <- compareFit(GR_CFA_fit, GR_R_CFA_fit, nested=T)
summary(cf)

tBG_fs <- lavPredict(GR_R_CFA_fit, type = "lv", method = "EBM", label = T, fsm = T)
tBG_fs <- as.data.frame(tBG_fs)
describe(tBG_fs)
ggplot(melt(tBG_fs), aes(x=value)) + geom_histogram() + facet_wrap(~variable)
d$tBG <- scales::rescale(tBG_fs$Gbe, to = c(0,1), from = range(tBG_fs$Gbe, na.rm = F, finite = T))
d$tGI <- scales::rescale(tBG_fs$Gid, to = c(0,1), from = range(tBG_fs$Gid, na.rm = F, finite = T))
colnames(d)

rm(list=setdiff(ls(), "d"))

#########################################
#
# Modern Homonegativity Scale (MHS)
#
#########################################

MHSm <- d %>% select(mhs1,mhs2,mhs3,mhs4,mhs5,mhs6,mhs7,mhs8,mhs9,mhs10,mhs11,mhs12,mhs13)
describe(MHSm)
sjt.itemanalysis(MHSm, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(MHSm),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Modern homonegativity scale items used in study 1")

MHS_irt <- mirt(MHSm,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(MHS_irt)
coef (MHS_irt, IRTpars=T)
MHS_iif <- plot(MHS_irt, type = "infotrace", main = "IIC")
MHS_ccc <- plot(MHS_irt, type = "trace", main = "CCC")
MHS_iise <- plot(MHS_irt, type = "infoSE", main = "I & SE")
MHS_tcc <- plot(MHS_irt, main = "TCC")
gridExtra::grid.arrange(MHS_iif,MHS_ccc,MHS_iise,MHS_tcc, ncol = 2, nrow=2, top = "Modern homonegativity scale")

MHS_CFA <- "mhs =~ mhs1 + mhs2 + mhs3 + mhs4 + mhs5 + mhs6 + mhs7 + mhs8 + mhs9 + mhs10 + mhs11 + mhs12 + mhs13"
MHS_CFA_fit <- cfa(MHS_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(MHS_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(MHS_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Modern Homonegativity Scale Study 1",line=0.5)
round(reliability(MHS_CFA_fit),2)

MHS_R_CFA <- "mhs =~ mhs8 + mhs9 + mhs11 + mhs13"
MHS_R_CFA_fit <- cfa(MHS_R_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(MHS_R_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(MHS_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Refined/Shortened Modern Homonegativity Scale",line=1.1)
round(reliability(MHS_R_CFA_fit),2)

par(mfrow=c(1,2))
semPaths(MHS_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Modern Homonegativity Scale Study 1",line=0.5)
semPaths(MHS_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Refined/Shortened Modern Homonegativity Scale",line=1.1)
dev.off()

anova(MHS_CFA_fit, MHS_R_CFA_fit)
cf <- compareFit(MHS_CFA_fit, MHS_R_CFA_fit, nested=T)
summary(cf)

MHS_fs <- lavPredict(MHS_R_CFA_fit, type = "lv", method = "EBM", label = T, fsm = T)
MHS_fs <- as.data.frame(MHS_fs)
describe(MHS_fs)
ggplot(melt(MHS_fs), aes(x=value)) + geom_histogram() + facet_wrap(~variable)
d$MHN <- scales::rescale(MHS_fs$mhs, to = c(0,1), from = range(MHS_fs$mhs, na.rm = F, finite = T))
colnames(d)

rm(list=setdiff(ls(), "d"))

describe(d[,c("gCN","nCN","rCN","hoGR","nGT","rGR","nGR","tBG","MHN")])
ggplot(melt(d[,c("gCN","nCN","rCN","hoGR","nGT","rGR","nGR","tBG","MHN")]), aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title = "Histogram of normalized factor scores + IAT", subtitle = "Study 1")

describe(d[,c("gCN","hoGR","nGT","tBG","MHN")])
ggplot(melt(d[,c("gCN","hoGR","nGT","tBG","MHN")]), aes(x=value)) + geom_density() + facet_wrap(~variable) +
  labs(title = "Density plot of normalized factor scores + IAT", subtitle = "Study 1")
sjt.itemanalysis(d[,c("gCN","hoGR","nGT","tBG","MHN")], show.kurtosis = T, encoding = "UTF-8")

save(d, file = "study1_data_processed.Rdata")
#rio::export(d, "study1_data_processed.csv")
rm(d)

load("study1_data_processed.Rdata")
colnames(d)

# X = gCN
# M1 = hGR
# M2 = tBG
# M3 = nGT
# Y = MHN

pmacroModel(6.3)

SEM_Med_exp <-"
# Mediation Effect
MHN ~ b1*hoGR+b2*tBG+b3*nGT+c1*gCN
hoGR ~ a1*gCN
tBG ~ a2*gCN+d1*hoGR
nGT ~ a3*gCN+d2*tBG+c2*hoGR
ind1:=a1*b1
ind2:=a2*b2
ind3:=a3*b3
ind4:=a1*d1*b2
ind5:=a1*c2*b3
ind6:=a2*d2*b3
ind7:=a1*d1*d2*b3
total1:=c1+a1*b1+a2*b2+a3*b3+a1*d1*b2+a1*c2*b3+a2*d2*b3+a1*d1*d2*b3
C1:=ind2-ind1
C2:=ind3-ind1
C3:=ind4-ind1
C4:=ind5-ind1
C5:=ind6-ind1
C6:=ind7-ind1
C7:=ind3-ind2
C8:=ind4-ind2
C9:=ind5-ind2
C10:=ind6-ind2
C11:=ind7-ind2
C12:=ind4-ind3
C13:=ind5-ind3
C14:=ind6-ind3
C15:=ind7-ind3
C16:=ind5-ind4
C17:=ind6-ind4
C18:=ind7-ind4
C19:=ind6-ind5
C20:=ind7-ind5
C21:=ind7-ind6
"
SEM_Med_exp_fit <- sem(SEM_Med_exp, data=d, estimator="ml",
                       se = "bootstrap", bootstrap = 10000,
                       parallel = "multicore", ncpus = 7L)
summary(SEM_Med_exp_fit, fit.measures=T, standardized=T, rsquare=T)
estimatesTable(SEM_Med_exp_fit) %>% kable()
parameterestimates(SEM_Med_exp_fit, boot.ci.type = "bca.simple", standardized = T) %>%
  filter(op == "~") %>%
  mutate_if(is.numeric, round, 3) %>% kable()
parameterestimates(SEM_Med_exp_fit, boot.ci.type = "bca.simple", standardized = T) %>%
  filter(op == ":=") %>%
  mutate_if(is.numeric, round, 3) %>% kable()
semPaths(SEM_Med_exp_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 1, layout = "circle2")