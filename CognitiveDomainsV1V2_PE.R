################################################################################
# Calculates cognitive domains for VETSA 1.                                    #
#                                                                              #
# Based on syntax files in:                                                    #
# M:/PSYCH/KREMEN/VETSA DATA FILES_852014/Cognitive Domains_April 2015 syntax/ #
#                                                                              #
# Creates cognitive domains from practice effects corrected dataset. This      #
# will generate domains for both vetsa 1 and 2.                                #
# Individual tests from each domain have already been filtered based on        #
# quality ("Z" score) prior to practice effect correction. Tests scores are    #
# then standardized based on VETSA 1 means and standard deviations. The        #
# composite score is calculated as the mean of standardized test scores within # 
# each domain.                                                                 #
#                                                                              #
# Note: The mean of z-scored tests is not itself a z-score. Domain comprised   #
# of multiple tests may have an SD less than 1.                                #
################################################################################


library(dplyr)
library(psych)

## Create function to scale by both timepoints by VETSA 1 mean and sd ##
scaleVals = function(df,varV1) {
  varV2 = paste0(varV1, "_V2p")
  zvarV1 = paste0("z", varV1)
  zvarV2 = paste0("z", varV2)
  df[[zvarV1]] = scale(df[[varV1]], center=T, scale=T)
  meanVal = attr(df[[zvarV1]], which="scaled:center")
  sdVal = attr(df[[zvarV1]], which="scaled:scale")
  df[[zvarV2]] = scale(df[[varV2]], center=meanVal, scale=sdVal)
  df
}

# Load vetsa 1 and 2 practice effect corrected  data
df = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_PE.csv")


#---------------------#
# Visual-Spatial Data #
#---------------------#

# Z-score test scores
df = scaleVals(df, "MR1COR")
df = scaleVals(df, "HFTOTCOR")

# Create domain score
df$zVisSpat = rowMeans(df[c("zMR1COR","zHFTOTCOR")], na.rm=T)
df$zVisSpat_V2p = rowMeans(df[c("zMR1COR_V2p","zHFTOTCOR_V2p")], na.rm=T)

#--------------------#
# Abstract Reasoning #
#--------------------#

# Square-root transformed MTXRAW
df$MTXTRAN = -1*sqrt(36 - df$MTXRAW)
df$MTXTRAN_V2p = -1*sqrt(36 - df$MTXRAW_V2p)

# Z-score test scores
df = scaleVals(df, "MTXTRAN")

# Create domain score
df$zAbsReason = as.numeric(df[,"zMTXTRAN"])
df$zAbsReason_V2p = as.numeric(df[,"zMTXTRAN_V2p"])

#-------------------------------#
# Short Term and Working Memory #
#-------------------------------#

# Square-root Transformed Reading Span
# NOTE 1: vetsa1 RSATOT is actually pulled from RSATOTrev in the merge dataset. This equates the two versions of the test.
# NOTE 2: Tasnform subtracts from 48 which is the max score. Original cognitive domain syntax subtracts RSATOT from 46. 
df$RSATOTTRAN = -1*sqrt(48 - df$RSATOT)
df$RSATOTTRAN_V2p = -1*sqrt(48 - df$RSATOT_V2p)

# Z-score test scores
df = scaleVals(df, "DSFRAW")
df = scaleVals(df, "DSBRAW")
df = scaleVals(df, "LNTOT")
df = scaleVals(df, "SSPFRAW")
df = scaleVals(df, "SSPBRAW")
df = scaleVals(df, "RSATOTTRAN")

# Create domain score
df$zSTWKMem = rowMeans(df[,c("zDSFRAW","zDSBRAW","zLNTOT","zSSPFRAW","zSSPBRAW","zRSATOTTRAN")], na.rm=T)
df$zSTWKMem_V2p = rowMeans(df[,c("zDSFRAW_V2p","zDSBRAW_V2p","zLNTOT_V2p","zSSPFRAW_V2p",
                                 "zSSPBRAW_V2p","zRSATOTTRAN_V2p")], na.rm=T)

#-----------------#
# Episodic Memory #
#-----------------#

# Z-score test scores
df = scaleVals(df, "CVATOT")
df = scaleVals(df, "CVSDFR")
df = scaleVals(df, "CVLDFR")
df = scaleVals(df, "LMITOT")
df = scaleVals(df, "LMDTOT")
df = scaleVals(df, "VRITOT")
df = scaleVals(df, "VRDTOT")

# Create domain score
df$zEpsMem = rowMeans(df[,c("zCVATOT","zCVSDFR","zCVLDFR","zLMITOT","zLMDTOT","zVRITOT","zVRDTOT")], na.rm=T)
df$zEpsMem_V2p = rowMeans(df[,c("zCVATOT_V2p","zCVSDFR_V2p","zCVLDFR_V2p","zLMITOT_V2p",
                                "zLMDTOT_V2p","zVRITOT_V2p","zVRDTOT_V2p")], na.rm=T)

#----------------#
# Verbal Fluency #
#----------------#

# Z-score test scores
df = scaleVals(df, "LFCOR")
df = scaleVals(df, "CFCOR")

# Create domain scores
df$zVerbFlu = rowMeans(df[,c("zLFCOR","zCFCOR")], na.rm=T)
df$zVerbFlu_V2p = rowMeans(df[,c("zLFCOR_V2p","zCFCOR_V2p")], na.rm=T)

#------------------#
# Processing Speed #
#------------------#

# Transform Trails time data so that positive is better performance
# NOTE: Implausible times (e.g., too long or too short) have already been filtered out
df$TRL2TRAN = -1*(df$TRL2TLOG)
df$TRL2TRAN_V2p = -1*(df$TRL2TLOG_V2p)
df$TRL3TRAN = -1*(df$TRL3TLOG)
df$TRL3TRAN_V2p = -1*(df$TRL3TLOG_V2p)

# Create Z-scores
df = scaleVals(df, "TRL2TRAN")
df = scaleVals(df, "TRL3TRAN")
df = scaleVals(df, "STRWRAW")
df = scaleVals(df, "STRCRAW")

# Create domain scores
df$zProcSpeed = rowMeans(df[,c("zSTRWRAW","zSTRCRAW","zTRL2TRAN","zTRL3TRAN")],na.rm=T)
df$zProcSpeed_V2p = rowMeans(df[,c("zSTRWRAW_V2p","zSTRCRAW_V2p","zTRL2TRAN_V2p","zTRL3TRAN_V2p")],na.rm=T)

#---------------------------------------#
# Executive Function : Trails Switching #
#---------------------------------------#

# Adjust Trails Condition 4 for Trails 2 and 3
df$TRL4TLOG[is.na(df$TRL2TLOG)] = NA
df$TRL4TLOG[is.na(df$TRL3TLOG)] = NA
df$TRL4TLOG_V2p[is.na(df$TRL2TLOG_V2p)] = NA
df$TRL4TLOG_V2p[is.na(df$TRL3TLOG_V2p)] = NA

# NOTE 1: Log transformed variables are used to adjust Trails 4, 
# thus square-root transform used in original syntax is no longer necessary.
# NOTE 2: Add intercept back in so mean differences between waves are maintained. Otherwise residuals are de-meaned.
mod.TRL4TLOG = lm(TRL4TLOG ~ TRL2TLOG + TRL3TLOG, data=df, na.action = "na.exclude")
mod.TRL4TLOG_V2p = lm(TRL4TLOG_V2p ~ TRL2TLOG_V2p + TRL3TLOG_V2p, data=df, na.action = "na.exclude")
df$TRL4ADJTRAN = -1 * (residuals(mod.TRL4TLOG) + coef(mod.TRL4TLOG)[["(Intercept)"]])
df$TRL4ADJTRAN_V2p = -1 * (residuals(mod.TRL4TLOG_V2p) + coef(mod.TRL4TLOG_V2p)[["(Intercept)"]])

# Z-score test scores
df = scaleVals(df, "TRL4ADJTRAN")

# Create domain scores
df$zExecTrailsSwitch = as.numeric(df[,"zTRL4ADJTRAN"])
df$zExecTrailsSwitch_V2p = as.numeric(df[,"zTRL4ADJTRAN_V2p"])

#-----------------------------------------#
# Executive Function : Category Switching #
#-----------------------------------------#

# Adjust Category Switching for Category Fluency
mod.CSSACC = lm(CSSACC ~ CFCOR, data=df, na.action = "na.exclude")
mod.CSSACC_V2p = lm(CSSACC_V2p ~ CFCOR_V2p, data=df, na.action = "na.exclude")
df$CSSACCADJ = residuals(mod.CSSACC) + coef(mod.CSSACC)[["(Intercept)"]]
df$CSSACCADJ_V2p = residuals(mod.CSSACC_V2p) + coef(mod.CSSACC_V2p)[["(Intercept)"]]

# Z-score test scores
df = scaleVals(df, "CSSACCADJ")

# Create domain scores
df$zExecCategorySwitch = as.numeric(df[,"zCSSACCADJ"])
df$zExecCategorySwitch_V2p = as.numeric(df[,"zCSSACCADJ_V2p"])

#---------------------------------#
# Executive Function : Inhibition #
#---------------------------------#

# Filter out Stroop errors
df$STRIT[which(df$STRIT=="1")] = NA
df$STRIT[which(df$STRIT>90)] = NA
df$STRIT_V2p[which(df$STRIT_V2p=="1")] = NA
df$STRIT_V2p[which(df$STRIT_V2p>90)] = NA

# Z-score test scores
df = scaleVals(df, "STRIT")
df$zExecInhibit = as.numeric(df[,"zSTRIT"])
df$zExecInhibit_V2p = as.numeric(df[,"zSTRIT_V2p"])


#-------------------#
#  Save out datset  #
#-------------------#

# Select cognitive domain variables
dfCogDomains = df %>%
  dplyr::select(VETSAID,
                zVisSpat,zMR1COR,zHFTOTCOR,
                zSTWKMem,zDSFRAW,zDSBRAW,zLNTOT,zSSPFRAW,zSSPBRAW,zRSATOTTRAN,
                zEpsMem,zCVATOT,zCVSDFR,zCVLDFR,zLMITOT,zLMDTOT,zVRITOT,zVRDTOT,
                zAbsReason,zMTXTRAN,
                zVerbFlu,zLFCOR,zCFCOR,
                zProcSpeed,zSTRWRAW,zSTRCRAW,zTRL2TRAN,zTRL3TRAN,
                zExecTrailsSwitch,zTRL4ADJTRAN,
                zExecCategorySwitch,zCSSACCADJ,
                zExecInhibit,zSTRIT,
                zVisSpat_V2p,zMR1COR_V2p,zHFTOTCOR_V2p,
                zSTWKMem_V2p,zDSFRAW_V2p,zDSBRAW_V2p,zLNTOT_V2p,zSSPFRAW_V2p,zSSPBRAW_V2p,zRSATOTTRAN_V2p,
                zEpsMem_V2p,zCVATOT_V2p,zCVSDFR_V2p,zCVLDFR_V2p,zLMITOT_V2p,zLMDTOT_V2p,zVRITOT_V2p,zVRDTOT_V2p,
                zAbsReason_V2p,zMTXTRAN_V2p,
                zVerbFlu_V2p,zLFCOR_V2p,zCFCOR_V2p,
                zProcSpeed_V2p,zSTRWRAW_V2p,zSTRCRAW_V2p,zTRL2TRAN_V2p,zTRL3TRAN_V2p,
                zExecTrailsSwitch_V2p,zTRL4ADJTRAN_V2p,
                zExecCategorySwitch_V2p,zCSSACCADJ_V2p,
                zExecInhibit_V2p,zSTRIT_V2p) 

# Save out data
write.csv(dfCogDomains, 
          "~/netshare/M/PSYCH/KREMEN/Practice Effect Cognitive Domains/data/V1V2_CognitiveDomains_PracEffects.csv",
          row.names = F)
