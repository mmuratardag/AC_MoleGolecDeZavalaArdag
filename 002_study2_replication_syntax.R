#############################################################################
##
#
# load the necessary libraries and the dataset
#
##
##############################################################################

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

load("study2_data.Rdata")
d <- d2
rm(d2)
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
  labs(title="Histograms of Collective narcissism (national) items used in study 2 replication")
CNn_irt <- mirt(CNnm,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(CNn_irt)
coef (CNn_irt, IRTpars=T)
CNn_iif <- plot(CNn_irt, type = "infotrace", main = "IIC")
CNn_ccc <- plot(CNn_irt, type = "trace", main = "CCC")
CNn_iise <- plot(CNn_irt, type = "infoSE", main = "I & SE")
CNn_tcc <- plot(CNn_irt, main = "TCC")
gridExtra::grid.arrange(CNn_iif,CNn_ccc,CNn_iise,CNn_tcc, ncol = 2, nrow=2, top = "Collective narcissism (national)")

CNn_CFA <- "CNn =~ cnn1 + cnn2 + cnn3 + cnn4 + cnn5"
CNn_CFA_fit <- cfa(CNn_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(CNn_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(CNn_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Collective narcissism (national) Study 2",line=0.5)
round(reliability(CNn_CFA_fit),2)

#########################################
#
# Religious collective narcissism (Cnc)
#
#########################################

CNrm <- d %>% select(cnc1,cnc2,cnc3,cnc4,cnc5)
describe(CNrm)
sjt.itemanalysis(CNrm, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(CNrm),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Collective narcissism (religious) items used in study 2 replication")
CNr_irt <- mirt(CNrm,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(CNr_irt)
coef (CNr_irt, IRTpars=T)
CNr_iif <- plot(CNr_irt, type = "infotrace", main = "IIC")
CNr_ccc <- plot(CNr_irt, type = "trace", main = "CCC")
CNr_iise <- plot(CNr_irt, type = "infoSE", main = "I & SE")
CNr_tcc <- plot(CNr_irt, main = "TCC")
gridExtra::grid.arrange(CNr_iif,CNr_ccc,CNr_iise,CNr_tcc, ncol = 2, nrow=2, top = "Collective narcissism (religious)")

CNr_CFA <- "CNr =~ cnc1 + cnc2 + cnc3 + cnc4 + cnc5"
CNr_CFA_fit <- cfa(CNr_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(CNr_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(CNr_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Collective narcissism (religious) Study 2",line=0.5)
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
title("CFA: general (HO) Collective narcissism Study 2",line=0.5)
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

nGRGT <- d %>% select(Relgen1,Relgen2,Relgen3,Relgen4,
                      Natgen1,Natgen2,Natgen3,Natgen4,
                      gaythreat10,gaythreat11,gaythreat12,gaythreat14)
describe(nGRGT)
ggplot(melt(nGRGT),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Religious & national gender roles and gay threat items used in study 2")

nGRGT_shr <- d %>% select(Relgen1,Relgen2,Relgen3,Relgen4,
                          Natgen1,Natgen3,Natgen4,
                          gaythreat11,gaythreat12,gaythreat14)
ggplot(melt(nGRGT_shr),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Religious + national gender roles and gay threat items used in study 2")
sjt.itemanalysis(nGRGT_shr, show.kurtosis = T, encoding = "UTF-8")

GAYthrM_RGR <- d %>% select(Relgen1,Relgen2,Relgen3,Relgen4)
GAYthrM_NGR <- d %>% select(Natgen1,Natgen2,Natgen3,Natgen4)
GAYthrM_gaythr <- d %>% select(gaythreat10, gaythreat11, gaythreat12, gaythreat14)

GAYthrM_RGR_irt <- mirt(GAYthrM_RGR,
                        model = 1,
                        itemtype = "graded",
                        technical=list(removeEmptyRows=T))
summary(GAYthrM_RGR_irt)
coef (GAYthrM_RGR_irt, IRTpars=T)
GAYthrM_RGR_iif <- plot(GAYthrM_RGR_irt, type = "infotrace", main = "IIC")
GAYthrM_RGR_ccc <- plot(GAYthrM_RGR_irt, type = "trace", main = "CCC")
GAYthrM_RGR_iise <- plot(GAYthrM_RGR_irt, type = "infoSE", main = "I & SE")
GAYthrM_RGR_tcc <- plot(GAYthrM_RGR_irt, main = "Test Characteristic Curve (TCC)")
gridExtra::grid.arrange(GAYthrM_RGR_iif,GAYthrM_RGR_ccc,GAYthrM_RGR_iise,GAYthrM_RGR_tcc, ncol = 2, nrow=2, top = "Study 2 Replication Religious Gender Roles")

GAYthrM_NGR_irt <- mirt(GAYthrM_NGR,
                        model = 1,
                        itemtype = "graded",
                        technical=list(removeEmptyRows=T))
summary(GAYthrM_NGR_irt)
coef (GAYthrM_NGR_irt, IRTpars=T)
GAYthrM_NGR_iif <- plot(GAYthrM_NGR_irt, type = "infotrace", main = "IIC")
GAYthrM_NGR_ccc <- plot(GAYthrM_NGR_irt, type = "trace", main = "CCC")
GAYthrM_NGR_iise <- plot(GAYthrM_NGR_irt, type = "infoSE", main = "I & SE")
GAYthrM_NGR_tcc <- plot(GAYthrM_NGR_irt, main = "Test Characteristic Curve (TCC)")
gridExtra::grid.arrange(GAYthrM_NGR_iif,GAYthrM_NGR_ccc,GAYthrM_NGR_iise,GAYthrM_NGR_tcc, ncol = 2, nrow=2, top = "Study 2 Replication National Gender Roles")

GAYthrM_irt <- mirt(GAYthrM_gaythr,
                    model = 1,
                    itemtype = "graded",
                    technical=list(removeEmptyRows=T))
summary(GAYthrM_irt)
coef (GAYthrM_irt, IRTpars=T)
GAYthrM_iif <- plot(GAYthrM_irt, type = "infotrace", main = "IIC")
GAYthrM_ccc <- plot(GAYthrM_irt, type = "trace", main = "CCC")
GAYthrM_iise <- plot(GAYthrM_irt, type = "infoSE", main = "I & SE")
GAYthrM_tcc <- plot(GAYthrM_irt, main = "TCC")
gridExtra::grid.arrange(GAYthrM_iif,GAYthrM_ccc,GAYthrM_iise,GAYthrM_tcc, ncol = 2, nrow=2, top = "Study 2 Replication National Gay threat")

GRGT_CFA <- "
ReligiousGR =~ Relgen1 + Relgen2 + Relgen3
NationalGR =~ Natgen1 + Natgen3 + Natgen4
GayThr =~ gaythreat12 + gaythreat14 
"
GRGT_CFA_fit <- cfa(GRGT_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GRGT_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GRGT_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4, curve = 2)
title("CFA: Gender roles & gay threat Study 2",line=0.5)
round(reliability(GRGT_CFA_fit),2)

GT_HO_CFA <- "
relGR =~ Relgen1 + Relgen2 + Relgen3
natGR =~ Natgen1 + Natgen3 + Natgen4
natGT =~ gaythreat12 + gaythreat14
HO_GR =~ relGR + natGR
"
GT_HO_CFA_fit <- cfa(GT_HO_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GT_HO_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GT_HO_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Gender roles & gay threat HO Study 2",line=1)
round(reliability(GT_HO_CFA_fit),2)
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
                    gendrole6,gendrole7)
describe(GRm)
GRm_sh <- d %>% select(gendrole1,gendrole2, gendrole6,gendrole7)
sjt.itemanalysis(GRm_sh, show.kurtosis = T, encoding = "UTF-8")
ggplot(melt(GRm),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Gender roles items used in study 2 replication")
ggplot(melt(GRm_sh),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Traditional belief about gender roles items used in study 2 replication")

GR_b_irt <- mirt(GRm,
                 model = 1,
                 itemtype = "graded",
                 technical=list(removeEmptyRows=T))
summary(GR_b_irt)
coef (GR_b_irt, IRTpars=T)
GR_b_iif <- plot(GR_b_irt, type = "infotrace", main = "IIC")
GR_b_ccc <- plot(GR_b_irt, type = "trace", main = "CCC")
GR_b_iise <- plot(GR_b_irt, type = "infoSE", main = "I & SE")
GR_b_tcc <- plot(GR_b_irt, main = "Test Characteristic Curve (TCC)")
gridExtra::grid.arrange(GR_b_iif,GR_b_ccc,GR_b_iise,GR_b_tcc, ncol = 2, nrow=2, top = "Study 2 Replication Traditional beliefs about gender")

GR_R_CFA <- "
Gbe =~ gendrole1 + gendrole2 + gendrole6 + gendrole7
"
GR_R_CFA_fit <- cfa(GR_R_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(GR_R_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(GR_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Traditional beliefs about gender roles Study 2",line=1.1)
round(reliability(GR_R_CFA_fit),2)
tBG_fs <- lavPredict(GR_R_CFA_fit, type = "lv", method = "EBM", label = T, fsm = T)
tBG_fs <- as.data.frame(tBG_fs)
describe(tBG_fs)
ggplot(melt(tBG_fs), aes(x=value)) + geom_histogram() + facet_wrap(~variable)
d$tBG <- scales::rescale(tBG_fs$Gbe, to = c(0,1), from = range(tBG_fs$Gbe, na.rm = F, finite = T))
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
  labs(title="Histograms of Modern homosexuality scale items used in study 2")
MHSm_s <- d %>% select(mhs8,mhs9,mhs11,mhs13)
ggplot(melt(MHSm_s),aes(x=value)) + geom_histogram() + facet_wrap(~variable) +
  labs(title="Histograms of Modern homosexuality scale items used in study 2")

MHS_irt <- mirt(MHSm,
                model = 1,
                itemtype = "graded",
                technical=list(removeEmptyRows=T))
summary(MHS_irt)
coef (MHS_irt, IRTpars=T)
MHS_iif <- plot(MHS_irt, type = "infotrace", main = "IIC")
MHS_ccc <- plot(MHS_irt, type = "trace", main = "CCC")
MHS_iise <- plot(MHS_irt, type = "infoSE", main = "I & SE")
MHS_tcc <- plot(MHS_irt, main = "Test Characteristic Curve (TCC)")
gridExtra::grid.arrange(MHS_iif,MHS_ccc,MHS_iise,MHS_tcc, ncol = 2, nrow=2, top = "Study 2 Replication Modern homonegativity scale")

MHS_R_CFA <- "mhs =~ mhs8 + mhs9 + mhs11 + mhs13"
MHS_R_CFA_fit <- cfa(MHS_R_CFA, data=d, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(MHS_R_CFA_fit, fit.measures=T, standardized=T, rsquare=T)
semPaths(MHS_R_CFA_fit, "mod", "std", intercepts = F, edge.label.cex=1, rotation = 4)
title("CFA: Modern Homonegativity Scale Study 2",line=1)
round(reliability(MHS_R_CFA_fit),2)
MHS_fs <- lavPredict(MHS_R_CFA_fit, type = "lv", method = "EBM", label = T, fsm = T)
MHS_fs <- as.data.frame(MHS_fs)
describe(MHS_fs)
ggplot(melt(MHS_fs), aes(x=value)) + geom_histogram() + facet_wrap(~variable)
d$MHN <- scales::rescale(MHS_fs$mhs, to = c(0,1), from = range(MHS_fs$mhs, na.rm = F, finite = T))
colnames(d)

rm(list=setdiff(ls(), "d"))

describe(d[,c("gCN","nCN","rCN","hoGR","nGT","rGR","nGR","tBG","MHN")])
ggplot(melt(d[,c("gCN","nCN","rCN","hoGR","nGT","rGR","nGR","tBG","MHN")]), aes(x=value)) + geom_histogram() + facet_wrap(~variable)

describe(d[,c("gCN","hoGR","nGT","tBG","MHN")])
ggplot(melt(d[,c("gCN","hoGR","nGT","tBG","MHN")]), aes(x=value)) + geom_density() + facet_wrap(~variable) +
  labs(title = "Density plot of normalized factor scores + IAT", subtitle = "Study 2")
sjt.itemanalysis(d[,c("gCN","hoGR","nGT","tBG","MHN")], show.kurtosis = T, encoding = "UTF-8")

save(d, file = "study2_data_processed.Rdata")
#rio::export(d, "study2_data_processed.csv")
rm(d)

load("study2_data_processed.Rdata")
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
