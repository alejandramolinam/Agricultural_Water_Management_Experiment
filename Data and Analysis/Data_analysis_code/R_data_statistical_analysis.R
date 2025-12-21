# Preliminaries and Libraries
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
library(plm)
library(marginaleffects)
library(stargazer)
library(dplyr)
library(sandwich)
library(lme4)
library(lmtest)
library(clubSandwich)
library(ggplot2)
#-------------------------------------------------------------------------------
# Paths
data_path <- "../data/processed/Data.csv"
save_tex_path <- "../tables/"
#------------------------------------------------------------------------------- 
# Read Processed Data
DF = read.csv( file = data_path )

#-------------------------------------------------------------------------------
# CREATE NEW COLUMNS
DF <- DF %>% mutate(summer = ifelse(round>5,1,0))

#-------------------------------------------------------------------------------
# Transform categorical variables in to factors
DF$player <- as.factor(DF$player)
DF$session <- as.factor(DF$session)
DF$treatment <- as.factor(DF$treatment)
DF$Inspection_past <- as.factor(DF$Inspection_past)
DF$Fine_past <- as.factor(DF$Fine_past)
DF$WUA <- as.factor(DF$WUA)
DF$woman <- as.factor(DF$woman)
DF$student <- as.factor(DF$student)
DF$summer <-as.factor(DF$summer)

#-------------------------------------------------------------------------------
# SUBSAMPLES
DFNC <- DF %>% filter(NCbinary==1)    # Only when non-compliance
DFCTRL <- DF %>% filter(treatment==0) # Only non treated participants
DFTTO <- DF %>% filter(treatment==1)  # Only treated participants
DFNCTTO <- DF %>% filter(treatment==1 & NCbinary==1)  # Only when non-compliance on treated participants
DFWUA <- DF %>% filter(WUA==1)        # Only water right owners    
DFNOWUA <- DF %>% filter(WUA==0)      # Only non water right owners
DFWUANC <- DF %>% filter(WUA==1 & NCbinary==1) # Only water right owners when non-compliance   
DFNOWUANC <- DF %>% filter(WUA==0 & NCbinary==1) # Only non water right owners when non-compliance   
DFspring <- DF %>% filter(round<=5)   # Only rounds with low demands
DFsummer <- DF %>% filter(round>5)    # Only rounds with high demands 
DFmen <- DF %>% filter(woman==0)      # Only men
DFwomen <- DF %>% filter(woman==1)    # Only women
DFriskavH <- DF %>% filter(risk_av_cat==1)# Only high risk aversion
DFriskavL <- DF %>% filter(risk_av_cat==2) # Only low risk aversion

#-------------------------------------------------------------------------------
################################################################################
#-------------------------------------------------------------------------------
#  ECONOMETRICAL MODELS
################################################################################

###### OLS MODELS ##############################################################
# ROBUST STANDAR ERRORS CLUSTERED BY PLAYER
###############################################################################

################################################################################
## COMPLETE SAMPLE
#-------------------------------------------------------------------------------

# Dependent variable: non-compliance binary
mblm1 <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
             + treatment + Inspection_past, data=DF)

# Robust Standard Errors
mblm1_robust <- coef_test(mblm1, vcov = "CR2", cluster=DF$player)
mblm1_robust_se <- mblm1_robust$SE
mblm1_robust_p <- mblm1_robust$p_Satt

# Add Controls
mblm2 <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
             + treatment + Inspection_past
            + beliefs_pre_cat + WUA + risk_av_cat + education_level_cat + woman 
            +WUA*treatment , data=DF)

# Robust Standard Errors
mblm2_robust <- coef_test(mblm2, vcov = "CR2", cluster=DF$player)
mblm2_robust_se <- mblm2_robust$SE
mblm2_robust_p <- mblm2_robust$p_Satt


###############################################################################
# Dependent variable: non-compliance in hours only when non-compliance 
mhNClm1 <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
              + treatment + Inspection_past, data=DFNC)
# Robust Standard Errors
mhNClm1_robust <- coef_test(mhNClm1, vcov = "CR2", cluster=DFNC$player)
mhNClm1_robust_se <- mhNClm1_robust$SE
mhNClm1_robust_p <- mhNClm1_robust$p_Satt
# Add Controls
mhNClm2 <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu 
              + treatment + Inspection_past + beliefs_pre_cat + 
                WUA + risk_av_cat + education_level_cat+ woman  
               +WUA*treatment, data=DFNC)
# Robust Standard Errors
mhNClm2_robust <- coef_test(mhNClm2, vcov = "CR2", cluster=DFNC$player)
mhNClm2_robust_se <- mhNClm2_robust$SE
mhNClm2_robust_p <- mhNClm2_robust$p_Satt


################################################################################
### MODELS OUTPUT TABLES ####

# Controls
stargazer(mblm2,mhNClm2, 
          type = "latex",
          out = paste0(save_tex_path,"OLS_TwoParts_NC_robustSE_cluster_player_Controls.tex"),
          se = list(mblm2_robust_se,mhNClm2_robust_se),  # robust SE
          p = list(mblm2_robust_p,mhNClm2_robust_p),    # robust p-values
          title = "Non-Compliance Two Parts Decision",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          dep.var.labels = c('Non-compl. decision',
                             'Non-compl. hours'),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'Initial Beliefs',
                               'WUA Membership (WUA)',
                               'Risk Aversion (High)',
                               'Education Level (Higher Education)',
                               'Gender (Woman)',
                               'Exo Scarcity*Exp. Last Endo Scarcity',
                               'Collective Scarcity Awareness on WUA'),
          report = "vc*"
)

# No-controls and Controls
stargazer(mblm1,mblm2,mhNClm1,mhNClm2, 
          type = "latex",
          out = paste0(save_tex_path,"OLS_TwoParts_NC_robustSE_cluster_player.tex"),
          se = list(mblm1_robust_se,mblm2_robust_se,mhNClm1_robust_se,mhNClm2_robust_se),  # robust SE
          p = list(mblm1_robust_p,mblm2_robust_p,mhNClm1_robust_p,mhNClm2_robust_p),    # robust p-values
          title = "Non-Compliance Two Parts Decision",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          dep.var.labels = c('Non-compliance decision',
                             'Non-compliance hours'),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'Initial Beliefs',
                               'WUA Membership (WUA)',
                               'Risk Aversion (High)',
                               'Education Level (Higher Education)',
                               'Gender (Woman)',
                               'Exo Scarcity*Exp. Last Endo Scarcity',
                               'Collective Scarcity Awareness on WUA')
         )


################################################################################
## WUA SAMPLE
#-------------------------------------------------------------------------------

# Dependent variable: non-compliance binary
mblm1WUA <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
            + Endo_pastB*Exo + treatment + Inspection_past, data=DFWUA)

# Robust Standard Errors
mblm1WUA_robust <- coef_test(mblm1WUA, vcov = "CR2", cluster=DFWUA$player)
mblm1WUA_robust_se <- mblm1WUA_robust$SE
mblm1WUA_robust_p <- mblm1WUA_robust$p_Satt

# Add Controls
mblm2WUA <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
            + Endo_pastB*Exo + treatment + Inspection_past
            + beliefs_pre_cat + risk_av_cat + education_level_cat + woman 
            , data=DFWUA)

# Robust Standard Errors
mblm2WUA_robust <- coef_test(mblm2WUA, vcov = "CR2", cluster=DFWUA$player)
mblm2WUA_robust_se <- mblm2WUA_robust$SE
mblm2WUA_robust_p <- mblm2WUA_robust$p_Satt


###############################################################################
# Dependent variable: non-compliance in hours only when non-compliance 
mhNClm1WUA <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
              + treatment + Inspection_past, data=DFWUANC)
# Robust Standard Errors
mhNClm1WUA_robust <- coef_test(mhNClm1WUA, vcov = "CR2", cluster=DFWUANC$player)
mhNClm1WUA_robust_se <- mhNClm1WUA_robust$SE
mhNClm1WUA_robust_p <- mhNClm1WUA_robust$p_Satt
# Add Controls
mhNClm2WUA <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu 
              + treatment + Inspection_past + beliefs_pre_cat + 
               risk_av_cat + education_level_cat+ woman  
              , data=DFWUANC)
# Robust Standard Errors
mhNClm2WUA_robust <- coef_test(mhNClm2WUA, vcov = "CR2", cluster=DFWUANC$player)
mhNClm2WUA_robust_se <- mhNClm2WUA_robust$SE
mhNClm2WUA_robust_p <- mhNClm2WUA_robust$p_Satt

################################################################################
## NON-WUA SAMPLE
#-------------------------------------------------------------------------------

# Dependent variable: non-compliance binary
mblm1NOWUA <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
               + Endo_pastB*Exo + treatment + Inspection_past, data=DFNOWUA)

# Robust Standard Errors
mblm1NOWUA_robust <- coef_test(mblm1NOWUA, vcov = "CR2", cluster=DFNOWUA$player)
mblm1NOWUA_robust_se <- mblm1NOWUA_robust$SE
mblm1NOWUA_robust_p <- mblm1NOWUA_robust$p_Satt

# Add Controls
mblm2NOWUA <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
               + Endo_pastB*Exo + treatment + Inspection_past
               + beliefs_pre_cat + risk_av_cat + education_level_cat + woman 
               , data=DFNOWUA)

# Robust Standard Errors
mblm2NOWUA_robust <- coef_test(mblm2NOWUA, vcov = "CR2", cluster=DFNOWUA$player)
mblm2NOWUA_robust_se <- mblm2NOWUA_robust$SE
mblm2NOWUA_robust_p <- mblm2NOWUA_robust$p_Satt


###############################################################################
# Dependent variable: non-compliance in hours only when non-compliance 
mhNClm1NOWUA <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                 + treatment + Inspection_past, data=DFNOWUANC)
# Robust Standard Errors
mhNClm1NOWUA_robust <- coef_test(mhNClm1NOWUA, vcov = "CR2", cluster=DFNOWUANC$player)
mhNClm1NOWUA_robust_se <- mhNClm1NOWUA_robust$SE
mhNClm1NOWUA_robust_p <- mhNClm1NOWUA_robust$p_Satt
# Add Controls
mhNClm2NOWUA <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu 
                 + treatment + Inspection_past + beliefs_pre_cat + 
                   risk_av_cat + education_level_cat+ woman  
                 , data=DFNOWUANC)
# Robust Standard Errors
mhNClm2NOWUA_robust <- coef_test(mhNClm2NOWUA, vcov = "CR2", cluster=DFNOWUANC$player)
mhNClm2NOWUA_robust_se <- mhNClm2NOWUA_robust$SE
mhNClm2NOWUA_robust_p <- mhNClm2NOWUA_robust$p_Satt


################################################################################
### MODELS OUTPUT TABLES ####

# No controls
stargazer(mblm1,mblm1WUA,mblm1NOWUA,mhNClm1,mhNClm1WUA,mhNClm1NOWUA, 
          type = "latex",
          out = paste0(save_tex_path,"OLS_TwoParts_NC_robustSE_cluster_player_TOTAL_WUA_NONWUA_NoControls.tex"),
          se = list(mblm1_robust_se,mblm1WUA_robust_se,mblm1NOWUA_robust_se,
                    mhNClm1_robust_se,mhNClm1WUA_robust_se,mhNClm1NOWUA_robust_se),  # robust SE
          p = list(mblm1_robust_p,mblm1WUA_robust_p,mblm1NOWUA_robust_p,
                   mhNClm1_robust_p,mhNClm1WUA_robust_p,mhNClm1NOWUA_robust_p),    # robust p-values
          title = "Non-Compliance Two Parts Decision Total, WUA, Non-WUA",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          dep.var.labels = c('Non-compliance decision',
                             'Non-compliance hours'),
          column.labels = c('Total','WUA','NON-WUA','Total','WUA','NON-WUA'),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'WUA Membership (WUA)',
                               'Exo Scarcity*Exp. Last Endo Scarcity',
                               'Collective Scarcity Awareness on WUA')
)

# Controls
stargazer(mblm2,mblm2WUA,mblm2NOWUA,mhNClm2,mhNClm2WUA,mhNClm2NOWUA, 
          type = "latex",
          out = paste0(save_tex_path,"OLS_TwoParts_NC_robustSE_cluster_player_TOTAL_WUA_NONWUA.tex"),
          se = list(mblm2_robust_se,mblm2WUA_robust_se,mblm2NOWUA_robust_se,
                    mhNClm2_robust_se,mhNClm2WUA_robust_se,mhNClm2NOWUA_robust_se),  # robust SE
          p = list(mblm2_robust_p,mblm2WUA_robust_p,mblm2NOWUA_robust_p,
                   mhNClm2_robust_p,mhNClm2WUA_robust_p,mhNClm2NOWUA_robust_p),    # robust p-values
          title = "Non-Compliance Two Parts Decision Total, WUA, Non-WUA",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          dep.var.labels = c('Non-compliance decision',
                             'Non-compliance hours'),
          column.labels = c('Total','WUA','NON-WUA','Total','WUA','NON-WUA'),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'Initial Beliefs',
                               'WUA Membership (WUA)',
                               'Risk Aversion (High)',
                               'Education Level (Higher Education)',
                               'Gender (Woman)',
                               'Exo Scarcity*Exp. Last Endo Scarcity',
                               'Collective Scarcity Awareness on WUA')
)

################################################################################
# OTHER MODELS INCLUDING NON-SIGNIFICANT INDEPENDENT VARIABLES


# Dependent variable: non-compliance binary
mbothlm1 <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + 
                 Endo_accu + demand_partner + treatment + Inspection_past + Fine_past, 
               data=DF)
# Robust Standard Errors
mbothlm1_robust <- coef_test(mbothlm1, vcov = "CR2", cluster=DF$player)
mbothlm1_robust_se <- mbothlm1_robust$SE
mbothlm1_robust_p <- mbothlm1_robust$p_Satt
# Add Controls
mbothlm2 <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + 
                 Endo_accu + demand_partner + treatment + Inspection_past + Fine_past +    
                 WUA+beliefs_pre_cat + risk_av_cat + woman, data=DF)
# Robust Standard Errors
mbothlm2_robust <- coef_test(mbothlm2, vcov = "CR2", cluster=DF$player)
mbothlm2_robust_se <- mbothlm2_robust$SE
mbothlm2_robust_p <- mbothlm2_robust$p_Satt


###############################################################################
# Dependent variable: non-compliance in hours only when non-compliance 
mhothNClm1 <- lm(NChours~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + 
                   Endo_accu + demand_partner + treatment + Inspection_past + Fine_past, 
                 data=DFNC)
# Robust Standard Errors
mhothNClm1_robust <- coef_test(mhothNClm1, vcov = "CR2", cluster=DFNC$player)
mhothNClm1_robust_se <- mhothNClm1_robust$SE
mhothNClm1_robust_p <- mhothNClm1_robust$p_Satt
# Add Controls
mhothNClm2 <- lm(NChours~  Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + 
                   Endo_accu + demand_partner + treatment + Inspection_past + Fine_past +    
                   WUA+beliefs_pre_cat + risk_av_cat + woman, data=DFNC)
# Robust Standard Errors
mhothNClm2_robust <- coef_test(mhothNClm2, vcov = "CR2", cluster=DFNC$player)
mhothNClm2_robust_se <- mhothNClm2_robust$SE
mhothNClm2_robust_p <- mhothNClm2_robust$p_Satt


################################################################################
### MODELS OUTPUT TABLES ####

stargazer(mbothlm1,mbothlm2,mhothNClm1,mhothNClm2, 
          type = "latex",
          out = paste0(save_tex_path,"OLS_TwoParts_NC_more_variables_robustSE_cluster_player.tex"),
          se = list(mbothlm1_robust_se,mbothlm2_robust_se,
                    mhothNClm1_robust_se,mhothNClm2_robust_se),  # robust SE
          p = list(mbothlm1_robust_p,mbothlm2_robust_p,
                   mhothNClm1_robust_p,mhothNClm2_robust_p),    # robust p-values
          title = "Non-Compliance Two Parts Decision",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          dep.var.labels = c('Non-compliance decision',
                             'Non-compliance hours'),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Partner Exogenous Scarcity',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'Fine Previous Round',
                               'Initial Beliefs',
                               'WUA Membership (WUA)',
                               'Risk Aversion (High)',
                               'Education Level (Higher Education)',
                               'Gender (Woman)',
                               'Exo Scarcity*Exp. Last Endo Scarcity',
                               'Collective Scarcity Awareness on WUA')
)

################################################################################
# COMPLETE SAMPLE: INDEPENDENT VARIABLES SEPARATED (Preliminary multiple hypothesis testing)

# Dependent variable: non-compliance binary
mblmV1 <- lm(NCbinary~ Exo, data=DF)
mblmV1_robust <- coef_test(mblmV1, vcov = "CR2", cluster=DF$player)
mblmV1_robust_se <- mblmV1_robust$SE
mblmV1_robust_p <- mblmV1_robust$p_Satt

mblmV2 <- lm(NCbinary~ Exo_accu_tot, data=DF)
mblmV2_robust <- coef_test(mblmV2, vcov = "CR2", cluster=DF$player)
mblmV2_robust_se <- mblmV2_robust$SE
mblmV2_robust_p <- mblmV2_robust$p_Satt

mblmV3 <- lm(NCbinary~ Endo_pastB, data=DF)
mblmV3_robust <- coef_test(mblmV3, vcov = "CR2", cluster=DF$player)
mblmV3_robust_se <- mblmV3_robust$SE
mblmV3_robust_p <- mblmV3_robust$p_Satt

mblmV4 <- lm(NCbinary~ Endo_past2, data=DF)
mblmV4_robust <- coef_test(mblmV4, vcov = "CR2", cluster=DF$player)
mblmV4_robust_se <- mblmV4_robust$SE
mblmV4_robust_p <- mblmV4_robust$p_Satt

mblmV5 <- lm(NCbinary~ Endo_accu, data=DF)
mblmV5_robust <- coef_test(mblmV5, vcov = "CR2", cluster=DF$player)
mblmV5_robust_se <- mblmV5_robust$SE
mblmV5_robust_p <- mblmV5_robust$p_Satt

mblmV6 <- lm(NCbinary~ treatment, data=DF)
mblmV6_robust <- coef_test(mblmV6, vcov = "CR2", cluster=DF$player)
mblmV6_robust_se <- mblmV6_robust$SE
mblmV6_robust_p <- mblmV6_robust$p_Satt

mblmV7<- lm(NCbinary~ Inspection_past, data=DF)
mblmV7_robust <- coef_test(mblmV7, vcov = "CR2", cluster=DF$player)
mblmV7_robust_se <- mblmV7_robust$SE
mblmV7_robust_p <- mblmV7_robust$p_Satt

mblmV8<- lm(NCbinary~ WUA, data=DF)
mblmV8_robust <- coef_test(mblmV8, vcov = "CR2", cluster=DF$player)
mblmV8_robust_se <- mblmV8_robust$SE
mblmV8_robust_p <- mblmV8_robust$p_Satt

models<-list(mblmV1,mblmV2,mblmV3,mblmV4,
             mblmV5,mblmV6,mblmV7,mblmV8)

# Output Table
stargazer(models,
          type = "latex",
          out = paste0(save_tex_path,"OLS_NCBinary_Independent_Variables_cluster_player.tex"),
          title = "Non-compliance binary - Independent variables",
          se = list(mblmV1_robust_se,mblmV2_robust_se,mblmV3_robust_se,
                    mblmV4_robust_se,mblmV5_robust_se,mblmV6_robust_se,
                    mblmV7_robust_se,mblmV8_robust_se),  # robust SE
          p = list(mblmV1_robust_p,mblmV2_robust_p,mblmV3_robust_p,
                   mblmV4_robust_p,mblmV5_robust_p,mblmV6_robust_p,
                   mblmV7_robust_p,mblmV8_robust_p),    # robust p-values
          float = FALSE,
          header = FALSE,
          model.names = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.table.layout = "s",
          omit="Constant",
          font.size = "scriptsize",
          covariate.labels = c('Exogenous Scarcity',
                               'Accum. Exo. Scarcity',
                               'Last Endo. Scarcity (Binary)',
                               'Last Endo. Scarcity (Hours)',
                               'Accum. Endo. Scarcity (Hours)',
                               'Collective Scarcity Awar.',
                               'Inspection previous round',
                               'WUA')
)

# Dependent variable: non-compliance in hours only when non-compliance 
mhlmV1 <- lm(NChours~ Exo, data=DFNC)
mhlmV1_robust <- coef_test(mhlmV1, vcov = "CR2", cluster=DFNC$player)
mhlmV1_robust_se <- mhlmV1_robust$SE
mhlmV1_robust_p <- mhlmV1_robust$p_Satt

mhlmV2 <- lm(NChours~ Exo_accu_tot, data=DFNC)
mhlmV2_robust <- coef_test(mhlmV2, vcov = "CR2", cluster=DFNC$player)
mhlmV2_robust_se <- mhlmV2_robust$SE
mhlmV2_robust_p <- mhlmV2_robust$p_Satt

mhlmV3 <- lm(NChours~ Endo_pastB, data=DFNC)
mhlmV3_robust <- coef_test(mhlmV3, vcov = "CR2", cluster=DFNC$player)
mhlmV3_robust_se <- mhlmV3_robust$SE
mhlmV3_robust_p <- mhlmV3_robust$p_Satt

mhlmV4 <- lm(NChours~ Endo_past2, data=DFNC)
mhlmV4_robust <- coef_test(mhlmV4, vcov = "CR2", cluster=DFNC$player)
mhlmV4_robust_se <- mhlmV4_robust$SE
mhlmV4_robust_p <- mhlmV4_robust$p_Satt

mhlmV5 <- lm(NChours~ Endo_accu, data=DFNC)
mhlmV5_robust <- coef_test(mhlmV5, vcov = "CR2", cluster=DFNC$player)
mhlmV5_robust_se <- mhlmV5_robust$SE
mhlmV5_robust_p <- mhlmV5_robust$p_Satt

mhlmV6 <- lm(NChours~ treatment, data=DFNC)
mhlmV6_robust <- coef_test(mhlmV6, vcov = "CR2", cluster=DFNC$player)
mhlmV6_robust_se <- mhlmV6_robust$SE
mhlmV6_robust_p <- mhlmV6_robust$p_Satt

mhlmV7 <- lm(NChours~ Inspection_past, data=DFNC)
mhlmV7_robust <- coef_test(mhlmV7, vcov = "CR2", cluster=DFNC$player)
mhlmV7_robust_se <- mhlmV7_robust$SE
mhlmV7_robust_p <- mhlmV7_robust$p_Satt

mhlmV8 <- lm(NChours~ WUA, data=DFNC)
mhlmV8_robust <- coef_test(mhlmV8, vcov = "CR2", cluster=DFNC$player)
mhlmV8_robust_se <- mhlmV8_robust$SE
mhlmV8_robust_p <- mhlmV8_robust$p_Satt

# Output Table
models<-list(mhlmV1,mhlmV2,mhlmV3,mhlmV4,mhlmV5,mhlmV6,mhlmV7,mhlmV8)
stargazer(models,
          type = "latex",
          out = paste0(save_tex_path,"OLS_NCHours_Independent_Variables_cluster_player.tex"),
          title = "Non-compliance hours - Independent variables",
          se = list(mhlmV1_robust_se,mhlmV2_robust_se,mhlmV3_robust_se,
                    mhlmV4_robust_se,mhlmV5_robust_se,mhlmV6_robust_se,
                    mhlmV7_robust_se),  # robust SE
          p = list(mhlmV1_robust_p,mhlmV2_robust_p,mhlmV3_robust_p,
                   mhlmV4_robust_p,mhlmV5_robust_p,mhlmV6_robust_p,
                   mhlmV7_robust_p),    # robust p-values
          header = FALSE,
          model.names = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.table.layout = "s",
          omit="Constant",
          covariate.labels = c('Exogenous Scarcity',
                               'Accum. Exogenous Scarcity',
                               'Last Endogenous Scarcity (Binary)',
                               'Last Endogenous Scarcity (Hours)',
                               'Accum. Endogenous Scarcity (Hours)',
                               'Collective Scarcity Awareness',
                               'Inspection previous round')
)

#-------------------------------------------------------------------------------
# MULTIPLE HYPOTHESIS TESTING 
#-------------------------------------------------------------------------------
library(xtable)

# Dependent variable: non-compliance binary
mblm1 <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB + Endo_past2 + Endo_accuB + Endo_accu + 
              + treatment + Inspection_past, data=DF)
mblm1_robust <- coef_test(mblm1, vcov = "CR2", cluster=DF$player)# Robust Standard Errors
tb_mblm1_robust <- as.data.frame(mblm1_robust)  
tb_mblm1_robust$padj_BH <- p.adjust(mblm1_robust$p_Satt, method = "BH")  
tb_mblm1_robust$padj_holm <- p.adjust(mblm1_robust$p_Satt, method = "holm")
tb_mblm1_robust$padj_bonferroni <- p.adjust(mblm1_robust$p_Satt, method = "bonferroni")

# Dependent variable: non-compliance hours 
mhlm1 <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu + treatment +
              Inspection_past , data=DFNC)
mhlm1_robust <- coef_test(mhlm1, vcov = "CR2", cluster=DFNC$player)# Robust Standard Errors
tb_mhlm1_robust <- as.data.frame(mhlm1_robust)  
tb_mhlm1_robust$padj_BH <- p.adjust(mhlm1_robust$p_Satt, method = "BH")  
tb_mhlm1_robust$padj_holm <- p.adjust(mhlm1_robust$p_Satt, method = "holm")
tb_mhlm1_robust$padj_bonferroni <- p.adjust(mhlm1_robust$p_Satt, method = "bonferroni")

# OUTPUT TABLES
tb_mblm1_robust <- as.matrix(tb_mblm1_robust[, c("beta", "SE","p_Satt","padj_BH","padj_holm","padj_bonferroni")])
tb_mhlm1_robust <- as.matrix(tb_mhlm1_robust[, c("beta", "SE","p_Satt","padj_BH","padj_holm","padj_bonferroni")])

labs1 <- c('(Intercept)',
           'Exogenous Scarcity',
           'Accum. Exogenous Scarcity',
           'Last Endogenous Scarcity (Binary)',
           'Last Endogenous Scarcity (Hours)',
           'Accum. Endogenous Scarcity',
           'Collective Scarcity Awareness',
           'Inspection previous round',
           'Exo*Last Endogenous Scarcity (Binary)')

labs2 <- c('(Intercept)',
           'Exogenous Scarcity',
           'Accum. Exogenous Scarcity',
           'Last Endogenous Scarcity (Hours)',
           'Accum. Endogenous Scarcity',
           'Collective Scarcity Awareness',
           'Inspection previous round')


rownames(tb_mblm1_robust) <- labs1
rownames(tb_mhlm1_robust) <- labs2

stargazer(tb_mblm1_robust,
          type = "latex",
          title = "Multiple Hypothesis Testing - Non-compliance decision",
          label = "tab:pvalues",
          summary = FALSE,
          out = paste0(save_tex_path,"OLS_TwoParts_NCbinary_robustSE_cluster_player_p_values_ajusted.tex")
)

stargazer(tb_mhlm1_robust,
          type = "latex",
          title = "Multiple Hypothesis Testing - Non-compliance in hours",
          label = "tab:pvalues",
          summary = FALSE,
          out = paste0(save_tex_path,"OLS_TwoParts_NChours_robustSE_cluster_player_p_values_ajusted.tex")
)

#-------------------------------------------------------------------------------
########################ROBUSTNESS ANALISYS#####################################
#-------------------------------------------------------------------------------

# Non Compliance Binary

# NON-TREATED
CTRL <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
           + Inspection_past, data=DFCTRL)
CTRLblmrobust <- coef_test(CTRL, vcov = "CR2", cluster=DFCTRL$player)
CTRLblmrobust_se <- CTRLblmrobust$SE
CTRLblmrobust_p <- CTRLblmrobust$p_Satt
#TREATED
TTO <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
          + Inspection_past, data=DFTTO)
TTOblmrobust <- coef_test(TTO, vcov = "CR2", cluster=DFTTO$player)
TTOblmrobust_se <- TTOblmrobust$SE
TTOblmrobust_p <- TTOblmrobust$p_Satt
#WUA
WUA <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
          + treatment + Inspection_past, data=DFWUA)
WUAblmrobust <- coef_test(WUA, vcov = "CR2", cluster=DFWUA$player)
WUAblmrobust_se <- WUAblmrobust$SE
WUAblmrobust_p <- WUAblmrobust$p_Satt
#NOWUA
NOWUA <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
            + treatment + Inspection_past, data=DFNOWUA)
NOWUAblmrobust <- coef_test(NOWUA, vcov = "CR2", cluster=DFNOWUA$player)
NOWUAblmrobust_se <- NOWUAblmrobust$SE
NOWUAblmrobust_p <- NOWUAblmrobust$p_Satt
#WOMEN
WOM <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
          + treatment + Inspection_past, data=DFwomen)
WOMblmrobust <- coef_test(WOM, vcov = "CR2", cluster=DFwomen$player)
WOMblmrobust_se <- WOMblmrobust$SE
WOMblmrobust_p <- WOMblmrobust$p_Satt
#MEN
MEN <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
          + treatment + Inspection_past, data=DFmen)
MENblmrobust <- coef_test(MEN, vcov = "CR2", cluster=DFmen$player)
MENblmrobust_se <- MENblmrobust$SE
MENblmrobust_p <- MENblmrobust$p_Satt
#Low Risk Aversion
RAvL <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
           + treatment + Inspection_past, data=DFriskavL)
RAvLblmrobust <- coef_test(RAvL, vcov = "CR2", cluster=DFriskavL$player)
RAvLblmrobust_se <- RAvLblmrobust$SE
RAvLblmrobust_p <- RAvLblmrobust$p_Satt
#High Risk Aversion
RAvH <- lm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
           + treatment + Inspection_past, data=DFriskavH)
RAvHblmrobust <- coef_test(RAvH, vcov = "CR2", cluster=DFriskavH$player)
RAvHblmrobust_se <- RAvHblmrobust$SE
RAvHblmrobust_p <- RAvHblmrobust$p_Satt

# Output Table
models<-list(CTRL,TTO,WUA,NOWUA,WOM,MEN,RAvL,RAvH)
stargazer(models,
          type = "latex",
          out = paste0(save_tex_path,"OLS_NCBinary_ROBUSTNESS_ANALYSIS.tex"),
          title = "Robustness Analysis",
          se = list(CTRLblmrobust_se,TTOblmrobust_se,WUAblmrobust_se,
                    NOWUAblmrobust_se,WOMblmrobust_se,MENblmrobust_se,
                    RAvLblmrobust_se,RAvHblmrobust_se),  # robust SE
          p = list(CTRLblmrobust_p,TTOblmrobust_p,WUAblmrobust_p,
                   NOWUAblmrobust_p,WOMblmrobust_p,MENblmrobust_p,
                   RAvLblmrobust_p,RAvHblmrobust_p),    # robust p-values
          column.labels = c('CTRL','TTO','WUA','NOWUA','WOM','MEN','RAvL','RAvH'),
          header = FALSE,
          model.names = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.table.layout = "s",
          omit="Constant",
          covariate.labels = c('Exo. Scarcity',
                               'Exo. Scarcity Accu',
                               'Endo. Scarcity (Bin)',
                               'Endo. Scarcity (Hour)',
                               'Endo. Scarcity Accu',
                               'Collective Scarcity',
                               'Inspection Prev. Round',
                               'Exo*Endo(Bin)'
          ),
          report = "vc*"
)

#-------------------------------------------------------------------------------
# Non Compliance in Hours
#-------------------------------------------------------------------------------

# NOT TREATED
CTRL <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
           + Inspection_past, data=DFCTRL)
CTRLhlmrobust <- coef_test(CTRL, vcov = "CR2", cluster=DFCTRL$player)
CTRLhlmrobust_se <- CTRLhlmrobust$SE
CTRLhlmrobust_p <- CTRLhlmrobust$p_Satt
#TREATED
TTO <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
          + Inspection_past, data=DFTTO)
TTOhlmrobust <- coef_test(TTO, vcov = "CR2", cluster=DFTTO$player)
TTOhlmrobust_se <- TTOhlmrobust$SE
TTOhlmrobust_p <- TTOhlmrobust$p_Satt
#WUA
WUA <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
          + treatment + Inspection_past, data=DFWUA)
WUAhlmrobust <- coef_test(WUA, vcov = "CR2", cluster=DFWUA$player)
WUAhlmrobust_se <- WUAhlmrobust$SE
WUAhlmrobust_p <- WUAhlmrobust$p_Satt
#NOWUA
NOWUA <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
            + treatment + Inspection_past, data=DFNOWUA)
NOWUAhlmrobust <- coef_test(NOWUA, vcov = "CR2", cluster=DFNOWUA$player)
NOWUAhlmrobust_se <- NOWUAhlmrobust$SE
NOWUAhlmrobust_p <- NOWUAhlmrobust$p_Satt
#WOMEN
WOM <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
          + treatment + Inspection_past, data=DFwomen)
WOMhlmrobust <- coef_test(WOM, vcov = "CR2", cluster=DFwomen$player)
WOMhlmrobust_se <- WOMhlmrobust$SE
WOMhlmrobust_p <- WOMhlmrobust$p_Satt
#MEN
MEN <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
          + treatment + Inspection_past, data=DFmen)
MENhlmrobust <- coef_test(MEN, vcov = "CR2", cluster=DFmen$player)
MENhlmrobust_se <- MENhlmrobust$SE
MENhlmrobust_p <- MENhlmrobust$p_Satt
#Low Risk Aversion
RAvL <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
           + treatment + Inspection_past, data=DFriskavL)
RAvLhlmrobust <- coef_test(RAvL, vcov = "CR2", cluster=DFriskavL$player)
RAvLhlmrobust_se <- RAvLhlmrobust$SE
RAvLhlmrobust_p <- RAvLhlmrobust$p_Satt
#High Risk Aversion
RAvH <- lm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
           + treatment + Inspection_past, data=DFriskavH)
RAvHhlmrobust <- coef_test(RAvH, vcov = "CR2", cluster=DFriskavH$player)
RAvHhlmrobust_se <- RAvHhlmrobust$SE
RAvHhlmrobust_p <- RAvHhlmrobust$p_Satt

# Output Table
models<-list(CTRL,TTO,WUA,NOWUA,WOM,MEN,RAvL,RAvH)
stargazer(models,
          type = "latex",
          out = paste0(save_tex_path,"OLS_NChours_ROBUSTNESS_ANALYSIS.tex"),
          title = "Robustness Analysis",
          se = list(CTRLhlmrobust_se,TTOhlmrobust_se,WUAhlmrobust_se,
                    NOWUAhlmrobust_se,WOMhlmrobust_se,MENhlmrobust_se,
                    RAvLhlmrobust_se,RAvHhlmrobust_se),  # robust SE
          p = list(CTRLhlmrobust_p,TTOhlmrobust_p,WUAhlmrobust_p,
                   NOWUAhlmrobust_p,WOMhlmrobust_p,MENhlmrobust_p,
                   RAvLhlmrobust_p,RAvHhlmrobust_p),    # robust p-values
          header = FALSE,
          model.names = TRUE,
          column.labels = c('CTRL','TTO','WUA','NOWUA','WOM','MEN','RAvL','RAvH'),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.table.layout = "s",
          omit="Constant",
          covariate.labels = c('Exo. Scarcity',
                               'Exo. Scarcity Accu',
                               'Endo. Scarcity (Hours)',
                               'Endo. Scarcity Accu',
                               'Collective Scarcity',
                               'Inspection Prev. Round'
          ),
          report = "vc*"
)

#-------------------------------------------------------------------------------
###### LOGIT MODELS ############################################################
#-------------------------------------------------------------------------------

################################################################################

# Dependent variable: non-compliance binary
mblog1 <- glm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu + treatment +
                Inspection_past , data=DF, family=binomial)
mblog1_robust <- coef_test(mblog1, vcov = "CR2", cluster=DF$player)
mblog1_se   <- mblog1_robust$SE
mblog1_p   <- mblog1_robust$p_Satt

# Marginal Effects
vcCR2 <- function(model) {
  idx <- as.integer(rownames(model.frame(model)))  
  cl  <- DF$player[idx]
  clubSandwich::vcovCR(model, cluster = cl, type = "CR2")
}

mblog1mfx <- avg_slopes(mblog1, vcov = vcCR2)
mblog1mfxcoef <- mblog1mfx$estimate
mblog1mfxse <- mblog1mfx$std.error
mblog1mfxp <- mblog1mfx$p.value

# Add controls
mblog2 <- glm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu 
              + treatment + Inspection_past 
              + beliefs_pre_cat + WUA + risk_av_cat + education_level_cat + woman,
              data=DF, family=binomial)
mblog2_robust <- coef_test(mblog2, vcov = "CR2", cluster=DF$player)
mblog2_se   <- mblog2_robust$SE
mblog2_p   <- mblog2_robust$p_Satt

# Marginal Effects
vcCR2 <- function(model){
  mf <- model.frame(model)
  cl <- DF$player[match(rownames(mf), rownames(DF))]
  vcovCR(model, cluster = cl, type = "CR2")
}
mblog2mfx <- avg_slopes(mblog2, vcov = vcCR2)
mblog2mfxcoef <- mblog2mfx$estimate
mblog2mfxse <- mblog2mfx$std.error
mblog2mfxp <- mblog2mfx$p.value

# MODELS OUTPUT TABLES
stargazer(mblog1,mblog2, 
          type = "latex",
          out = paste0(save_tex_path,"LOGIT_Binary_NC_robustSE_cluster_player.tex"),
          title = "Non-Compliance Binary Decision - Logit Models",
          se   = list(mblog1_se,mblog2_se),
          p    = list(mblog1_p,mblog2_p),
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'Initial Beliefs',
                               'WUA Membership (WUA)',
                               'Risk Aversion (High)',
                               'Education Level (Higher Education)',
                               'Gender (Woman)',
                               'Exo Scarcity*Exp. Last Endo Scarcity')
)

stargazer(mblog1,mblog2, 
          coef = list(mblog1mfxcoef[c(4,4,5,3,2,1,7,6)],mblog2mfxcoef[c(4,4,5,3,2,1,11,6,8,7,10,9,12)]),
          se   = list(mblog1mfxse[c(4,4,5,3,2,1,7,6)],mblog2mfxse[c(4,4,5,3,2,1,11,6,8,7,10,9,12)]),
          p    = list(mblog1mfxp[c(4,4,5,3,2,1,7,6)],mblog2mfxp[c(4,4,5,3,2,1,11,6,8,7,10,9,12)]),
          type = "latex",
          out = paste0(save_tex_path,"LOGIT_Marginal_Effects_Binary_NC_robustSE_cluster_player.tex"),
          title = "Non-Compliance Binary Decision - Marginal Effects Logit Models",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'Initial Beliefs',
                               'WUA Membership (WUA)',
                               'Risk Aversion (High)',
                               'Education Level (Higher Education)',
                               'Gender (Woman)',
                               'Exo Scarcity*Exp. Last Endo Scarcity')
)



#-------------------------------------------------------------------------------
###### PANEL MODELS WITH FIXED EFFECTS PER PARTICIPANT  #######################
#-------------------------------------------------------------------------------
###############################################################################

###############################################################################
# Dependent variable: Non-compliance binary (0: Comply; 1: Non comply)
mbplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
              + Inspection_past, data=DF, index=c("player"), model="within")
# Robust Standard Errors
mbplm1_robust <- coef_test(mbplm1, vcov = "CR2", cluster=DF$player)
mbplm1_robust_se <- mbplm1_robust$SE
mbplm1_robust_p  <- mbplm1_robust$p_Satt

###############################################################################
# Dependent variable: non-compliance in hours only when non-compliance 

mhNCplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                + Inspection_past , data=DFNC, 
                index=c("player"), model="within")
# Robust Standard Errors
mhNCplm1_robust <- coef_test(mhNCplm1, vcov = "CR2", cluster=DF$player)
mhNCplm1_robust_se <- mhNCplm1_robust$SE
mhNCplm1_robust_p <- mhNCplm1_robust$p_Satt

################################################################################
#### OUTPUT TABLE
stargazer(mbplm1, mhNCplm1,  
          type = "latex",
          out = paste0(save_tex_path,"PLM_TwoParts_NC_robustSE_cluster_player.tex"),
          title = "Panel Models Fixed Effecs",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          covariate.labels = c('Exo. Scarcity',
                               'Accum. Exo. Scarcity',
                               'Endo. Scarcity (binary)',
                               'Endo. Scarcity (hours)',
                               'Accum. Endo. Scarcity',
                               'Inspection '
          ),
          report = "vc*"
)


################################################################################

# ROBUSTNESS ANALYSIS - PANEL MODELS RANDOM EFFECTS

#-------------------------------------------------------------------------------
# Non Compliance Binary
#-------------------------------------------------------------------------------

mbCTRLplm1 <-plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
           + Inspection_past, data=DFCTRL, index=c("player"), model="within")
mbCTRLplm1_robust <- coef_test(mbCTRLplm1, vcov = "CR2", cluster=DFCTRL$player)
mbCTRLplm1_robust_se <- mbCTRLplm1_robust$SE
mbCTRLplm1_robust_p  <- mbCTRLplm1_robust$p_Satt

mbTTOplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
           + Inspection_past, data=DFTTO, index=c("player"), model="within")
mbTTOplm1_robust <- coef_test(mbTTOplm1, vcov = "CR2", cluster=DFTTO$player)
mbTTOplm1_robust_se <- mbTTOplm1_robust$SE
mbTTOplm1_robust_p  <- mbTTOplm1_robust$p_Satt

mbWUAplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
          + Inspection_past,data=DFWUA, index=c("player"), model="within")
mbWUAplm1_robust <- coef_test(mbWUAplm1, vcov = "CR2", cluster=DFWUA$player)
mbWUAplm1_robust_se <- mbWUAplm1_robust$SE
mbWUAplm1_robust_p  <- mbWUAplm1_robust$p_Satt

mbNOWUAplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
                 + Inspection_past,data=DFNOWUA, index=c("player"), model="within")
mbNOWUAplm1_robust <- coef_test(mbNOWUAplm1, vcov = "CR2", cluster=DFNOWUA$player)
mbNOWUAplm1_robust_se <- mbNOWUAplm1_robust$SE
mbNOWUAplm1_robust_p  <- mbNOWUAplm1_robust$p_Satt

mbWOMplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
                 + Inspection_past,data=DFwomen, index=c("player"), model="within")
mbWOMplm1_robust <- coef_test(mbWOMplm1, vcov = "CR2", cluster=DFwomen$player)
mbWOMplm1_robust_se <- mbWOMplm1_robust$SE
mbWOMplm1_robust_p  <- mbWOMplm1_robust$p_Satt

mbMENplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
                 + Inspection_past,data=DFmen, index=c("player"), model="within")
mbMENplm1_robust <- coef_test(mbMENplm1, vcov = "CR2", cluster=DFmen$player)
mbMENplm1_robust_se <- mbMENplm1_robust$SE
mbMENplm1_robust_p  <- mbMENplm1_robust$p_Satt

mbRavLplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
                 + Inspection_past,data=DFriskavL, index=c("player"), model="within")
mbRavLplm1_robust <- coef_test(mbRavLplm1, vcov = "CR2", cluster=DFriskavL$player)
mbRavLplm1_robust_se <- mbRavLplm1_robust$SE
mbRavLplm1_robust_p  <- mbRavLplm1_robust$p_Satt

mbRavHplm1 <- plm(NCbinary~ Exo + Exo_accu_tot + Endo_pastB*Exo + Endo_past2 + Endo_accu
                  + Inspection_past,data=DFriskavH, index=c("player"), model="within")
mbRavHplm1_robust <- coef_test(mbRavHplm1, vcov = "CR2", cluster=DFriskavH$player)
mbRavHplm1_robust_se <- mbRavHplm1_robust$SE
mbRavHplm1_robust_p  <- mbRavHplm1_robust$p_Satt

models <- list(mbCTRLplm1,mbTTOplm1,mbWUAplm1,mbNOWUAplm1,
               mbWOMplm1,mbMENplm1,mbRavLplm1,mbRavHplm1)

# Output Table
stargazer(models,
          type = "latex",
          out = paste0(save_tex_path,"PLM_NCBinary_ROBUSTNESS_ANALYSIS.tex"),
          title = "Robustness Analysis",
          se = list(mbCTRLplm1_robust_se,mbTTOplm1_robust_se,
                    mbWUAplm1_robust_se,mbNOWUAplm1_robust_se,
                    mbWOMplm1_robust_se,mbMENplm1_robust_se,
                    mbRavLplm1_robust_se,mbRavHplm1_robust_se),  # robust SE
          p = list(mbCTRLplm1_robust_p,mbTTOplm1_robust_p,
                   mbWUAplm1_robust_p,mbNOWUAplm1_robust_p,
                   mbWOMplm1_robust_p,mbMENplm1_robust_p,
                   mbRavLplm1_robust_p,mbRavHplm1_robust_p),    # robust p-values
          column.labels = c('CTRL','TTO','WUA','NOWUA','WOM','MEN','RAvL','RAvH'),
          header = FALSE,
          model.names = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.table.layout = "s",
          omit="Constant",
          covariate.labels = c('Exo. Scarcity',
                               'Exo. Scarcity Accu',
                               'Endo. Scarcity (Bin)',
                               'Endo. Scarcity (Hour)',
                               'Endo. Scarcity Accu',
                               'Collective Scarcity',
                               'Inspection Prev. Round',
                               'Exo*Endo(Bin)'
          )
)


#-------------------------------------------------------------------------------
# Non Compliance in Hours
#-------------------------------------------------------------------------------

mhCTRLplm1 <-plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                 + Inspection_past, data=DFCTRL, index=c("player"), model="within")
mhCTRLplm1_robust <- coef_test(mhCTRLplm1, vcov = "CR2", cluster=DFCTRL$player)
mhCTRLplm1_robust_se <- mhCTRLplm1_robust$SE
mhCTRLplm1_robust_p  <- mhCTRLplm1_robust$p_Satt

mhTTOplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                 + Inspection_past, data=DFTTO, index=c("player"), model="within")
mhTTOplm1_robust <- coef_test(mhTTOplm1, vcov = "CR2", cluster=DFTTO$player)
mhTTOplm1_robust_se <- mhTTOplm1_robust$SE
mhTTOplm1_robust_p  <- mhTTOplm1_robust$p_Satt

mhWUAplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                 + Inspection_past,data=DFWUA, index=c("player"), model="within")
mhWUAplm1_robust <- coef_test(mhWUAplm1, vcov = "CR2", cluster=DFWUA$player)
mhWUAplm1_robust_se <- mhWUAplm1_robust$SE
mhWUAplm1_robust_p  <- mhWUAplm1_robust$p_Satt

mhNOWUAplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                   + Inspection_past,data=DFNOWUA, index=c("player"), model="within")
mhNOWUAplm1_robust <- coef_test(mhNOWUAplm1, vcov = "CR2", cluster=DFNOWUA$player)
mhNOWUAplm1_robust_se <- mhNOWUAplm1_robust$SE
mhNOWUAplm1_robust_p  <- mhNOWUAplm1_robust$p_Satt

mhWOMplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                 + Inspection_past,data=DFwomen, index=c("player"), model="within")
mhWOMplm1_robust <- coef_test(mhWOMplm1, vcov = "CR2", cluster=DFwomen$player)
mhWOMplm1_robust_se <- mhWOMplm1_robust$SE
mhWOMplm1_robust_p  <- mhWOMplm1_robust$p_Satt

mhMENplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                 + Inspection_past,data=DFmen, index=c("player"), model="within")
mhMENplm1_robust <- coef_test(mhMENplm1, vcov = "CR2", cluster=DFmen$player)
mhMENplm1_robust_se <- mhMENplm1_robust$SE
mhMENplm1_robust_p  <- mhMENplm1_robust$p_Satt

mhRavLplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                  + Inspection_past,data=DFriskavL, index=c("player"), model="within")
mhRavLplm1_robust <- coef_test(mhRavLplm1, vcov = "CR2", cluster=DFriskavL$player)
mhRavLplm1_robust_se <- mhRavLplm1_robust$SE
mhRavLplm1_robust_p  <- mhRavLplm1_robust$p_Satt

mhRavHplm1 <- plm(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu
                  + Inspection_past,data=DFriskavH, index=c("player"), model="within")
mhRavHplm1_robust <- coef_test(mhRavHplm1, vcov = "CR2", cluster=DFriskavH$player)
mhRavHplm1_robust_se <- mhRavHplm1_robust$SE
mhRavHplm1_robust_p  <- mhRavHplm1_robust$p_Satt

models <- list(mhCTRLplm1,mhTTOplm1,mhWUAplm1,mhNOWUAplm1,
               mhWOMplm1,mhMENplm1,mhRavLplm1,mhRavHplm1)

# Output Table
stargazer(models,
          type = "latex",
          out = paste0(save_tex_path,"PLM_NCHours_ROBUSTNESS_ANALYSIS.tex"),
          title = "Robustness Analysis",
          se = list(mhCTRLplm1_robust_se,mhTTOplm1_robust_se,
                    mhWUAplm1_robust_se,mhNOWUAplm1_robust_se,
                    mhWOMplm1_robust_se,mhMENplm1_robust_se,
                    mhRavLplm1_robust_se,mhRavHplm1_robust_se),  # robust SE
          p = list(mhCTRLplm1_robust_p,mhTTOplm1_robust_p,
                   mhWUAplm1_robust_p,mhNOWUAplm1_robust_p,
                   mhWOMplm1_robust_p,mhMENplm1_robust_p,
                   mhRavLplm1_robust_p,mhRavHplm1_robust_p),    # robust p-values
          column.labels = c('CTRL','TTO','WUA','NOWUA','WOM','MEN','RAvL','RAvH'),
          header = FALSE,
          model.names = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.table.layout = "s",
          omit="Constant",
          covariate.labels = c('Exo. Scarcity',
                               'Exo. Scarcity Accu',
                               'Endo. Scarcity (Hour)',
                               'Endo. Scarcity Accu',
                               'Collective Scarcity',
                               'Inspection Prev. Round'
          )
)
#-------------------------------------------------------------------------------
###### HIERARCHICAL MODELS #####################################################
################################################################################

# Dependent variable: Non-compliance binary (0: Comply; 1: Non comply)
mblmer1 <- lmer(NCbinary~ Exo + Exo_accu_tot + Exo*Endo_pastB + Endo_past2 + Endo_accu 
                + treatment + Inspection_past
                + (1 | player ),data=DF,
                control = lmerControl(optimizer = "bobyqa", 
                                      optCtrl = list(maxfun = 100000)))
# Robust Standard Errors
mblmer1_robust <- coef_test(mblmer1, vcov = "CR2", cluster=DF$player)
mblmer1_robust_se <- mblmer1_robust$SE
mblmer1_robust_p <- mblmer1_robust$p_Satt

# Add Controls
mblmer2 <- lmer(NCbinary~ Exo + Exo_accu_tot + Exo*Endo_pastB + Endo_past2 + Endo_accu + treatment
                +Inspection_past + beliefs_pre_cat + WUA 
                + risk_av_cat + woman + (1 | player ),data=DF,
                control = lmerControl(optimizer = "bobyqa", 
                                      optCtrl = list(maxfun = 100000)))
# Robust Standard Errors
mblmer2_robust <- coef_test(mblmer2, vcov = "CR2", cluster=DF$player)
mblmer2_robust_se <- mblmer2_robust$SE
mblmer2_robust_p <- mblmer2_robust$p_Satt

###############################################################################
# Dependent variable: non-compliance in hours only when non-compliance 
mhNClmer1 <- lmer(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu 
                  + treatment + Inspection_past 
                + (1 | player ),data=DFNC,
                control = lmerControl(optimizer = "bobyqa", 
                                      optCtrl = list(maxfun = 100000)))
# Robust Standard Errors
mhNClmer1_robust <- coef_test(mhNClmer1, vcov = "CR2", cluster=DFNC$player)
mhNClmer1_robust_se <- mhNClmer1_robust$SE
mhNClmer1_robust_p <- mhNClmer1_robust$p_Satt

# Add Controls
mhNClmer2 <- lmer(NChours~ Exo + Exo_accu_tot + Endo_past2 + Endo_accu 
                  + treatment + Inspection_past + beliefs_pre_cat + 
                    WUA + risk_av_cat + education_level_cat+ woman  
                  +WUA*treatment + (1 | player ),data=DFNC,
                control = lmerControl(optimizer = "bobyqa", 
                                      optCtrl = list(maxfun = 100000)))
# Robust Standard Errors
mhNClmer2_robust <- coef_test(mhNClmer2, vcov = "CR2", cluster=DFNC$player)
mhNClmer2_robust_se <- mhNClmer2_robust$SE
mhNClmer2_robust_p <- mhNClmer2_robust$p_Satt

################################################################################
### MODELS OUTPUT TABLE ####

stargazer(mblmer1,mblmer2,mhNClmer1,mhNClmer2, 
          type = "latex",
          out = paste0(save_tex_path,"LMER_TwoParts_NC_robustSE_cluster_player.tex"),
          se = list(mblmer1_robust_se,mblmer2_robust_se,mhNClmer1_robust_se,mhNClmer2_robust_se),  # robust SE
          p = list(mblmer1_robust_p,mblmer2_robust_p,mhNClmer1_robust_p,mhNClmer2_robust_p),    # robust p-values
          title = "Non-Compliance Two Parts Decision",
          header = FALSE,
          model.names = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f","ser"),
          dep.var.labels = c('Non-compliance decision',
                             'Non-compliance hours'),
          covariate.labels = c('Exogenous Scarcity',
                               'Exogenous Scarcity Accu.',
                               'Experience Last Endo. Scarcity (binary)',
                               'Last Endogenous Scarcity',
                               'Endogenous Scarcity Accu.',
                               'Collective Scarcity Awareness (binary)',
                               'Inspection Previous Round (binary)',
                               'Initial Beliefs',
                               'WUA Membership (WUA)',
                               'Risk Aversion (High)',
                               'Education Level (Higher Education)',
                               'Gender (Woman)',
                               'Exo Scarcity*Exp. Last Endo Scarcity',
                               'Collective Scarcity Awareness on WUA')
)

