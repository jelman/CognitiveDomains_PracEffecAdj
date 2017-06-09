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


library(sjmisc)
library(dplyr)
library(psych)

# Load vetsa 1 and 2 practice effect corrected  data
df = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_PE.csv")


##########################################
# Replace missing data and trim outliers #
##########################################

#----------------#
# Verbal Ability #
#----------------#

df$VOCRAW[which(df$ZVOCAB=="2")] = NA
# Transformed VOCRAW
df$voctran = -1 * sqrt(80-df$VOCRAW)

#---------------------#
# Visual-Spatial Data #
#---------------------#

## Mental Rotation ##

# vetsa1 MENTAL ROTATION: Total Correct PART1
df$MR1COR[which(df$ZMENROT==2)] = NA
df$MR2COR[which(df$ZMENROT==2)] = NA
# MENTAL ROTATION: Total Correct PARTS 1&2
df$MRTOTCOR= df$MR1COR + df$MR2COR

hfCols = c("HFIGC1","HFIGC3","HFIGC5")
df[which(df$ZHF==2), hfCols] = NA

# vetsa1 Hidden Figures Total Correct Parts 1,3,5
df$HFTOTCOR = rowSums(df[hfCols])

#--------------------#
# Abstract Reasoning #
#--------------------#

df$MTXRAW[which(df$ZMATRIX=="2")] = NA
df$MTXRAW[which(df$MTXRAW=="0")] = NA

# Square-root transformed MTXRAW
df$MTXTRAN = -1*sqrt(36 - df$MTXRAW)


#-------------------------------#
# Short Term and Working Memory #
#-------------------------------#

## Digit Span ##
df$dsfraw[which(df$ZDGSPNF=="2")] = NA
df$dsbraw[which(df$ZDGSPNB=="2")] = NA

## Letter-Number Sequencing ##
df$lntot[which(df$ZLNS=="2")] = NA
df$lntot[which(df$lntot=="0")] = NA

## Spatial Span ##
df$sspfraw[which(df$ZSPSPNF=="2")] = NA
df$sspbraw[which(df$ZSPSPNB=="2")] = NA

## Reading Span ##
df$RSATOTrev[which(df$ZRDSPNA=="2")] = NA

# Square-root Transformed Reading Span
df$RSATOTrevtran = -1*sqrt(46 - df$RSATOTrev)

#-----------------#
# Episodic Memory #
#-----------------#

## CVLT ##
cvltiCols = c("cvatot","CVSDFR")
df[which(df$ZCVLTI=="2"), cvltiCols] = NA

df$CVLDFR[which(df$ZCVLTD=="2")] = NA
df$cvatot[which(df$cva5raw=="0")] = NA

cvltfrCols = c("CVSDFR","CVLDFR")
df[which(df$cvatot=="0"), cvltfrCols] = NA

## Logical Memory ##

logmemCols = c("lmitot","lmdtot")
df[which(df$ZLMI=="2"), logmemCols] = NA

df$lmdtot[which(df$ZLMD=="2")] = NA
df$lmdtot[is.na(df$lmitot)] = NA

## Visual Reproduction ##

vrCols = c("vritot","vrdtot")
df[which(df$ZVRI=="2"), vrCols] = NA

df$vrdtot[which(df$ZVRD=="2")] = NA
df$vrdtot[is.na(df$vritot)] = NA

#--------------------#
# Executive Function #
#--------------------#

## Trails Condition 4 ##

df$TRL4T[which(df$ZTRAIL4=="2")] = NA
df$TRL4T[is.na(df$TRL2T)] = NA
df$TRL4T[is.na(df$TRL3T)] = NA

# vetsa1 Trails 4 Adjusted for Trails 2&3
df$TRL4TADJ = with(df, 
                             (TRL4T - (TRL2T*0.7752) + (TRL3T*1.0438)))
# vetsa1 SQRT-Transformed Trails 4 Adjusted
df$TRL4ADJTRAN = -1*sqrt(80 + df$TRL4TADJ)

## Category Fluency Switching ##
df$CFCOR[which(df$ZFLUC=="2")] = NA
df$CSSACC[which(df$ZFLUCS=="2")] = NA

# vetsa1 Category Switching Accuracy - Adjusted
df$CSSACCADJ = df$CSSACC - (0 + (df$CFCOR*0.1329))

## Stroop ##
df$STRIT [which(df$STRIT=="1")] = NA
df$STRIT[which(df$STRIT>90)] = NA

df$strcwraw[which(df$ZSTROOPCW=="2") ] = NA
df$strcwraw[is.na(df$strwraw)] = NA
df$strcwraw[is.na(df$strcraw)] = NA

df[which(df$vetsaid=="19885B"),"strcraw"] = 86
df[which(df$vetsaid=="19885B"),"strcwraw"] = 45

#----------------#
# Verbal Fluency #
#----------------#

df$LFCOR[which(df$ZFLUL=="2")] = NA
df$CFCOR[which(df$ZFLUC=="2")] = NA

#------------------#
# Processing Speed #
#------------------#

## Stroop ##
df$strcraw[which(df$ZSTROOPC=="2")] = NA
df$strwraw[which(df$ZSTROOPW=="2")] = NA

## Trails ##
df$TRL2T[which(df$ZTRAIL2=="2")] = NA
df$TRL3T[which(df$ZTRAIL3=="2")] = NA

df$TRL2T[which(df$TRL2T>120)] = NA
df$TRL3T[which(df$TRL3T>120)] = NA

# Log Transformed Trails
df$TRL2TRAN = -1*log(df$TRL2T)
df$TRL3TRAN = -1*log(df$TRL3T)


#########################################################
# Creating standardized scores and composite scores.    #
# Standardization is based off of VETSA1 Means and SDs  #
#########################################################

## Create function to save mean and SD of all variables ##
addScaleVals = function(df,varname, x) {
  meanVal = attr(x, which="scaled:center")
  sdVal = attr(x, which="scaled:scale")
  rbind(df, data.frame(Variable=varname, Mean=meanVal, SD=sdVal))
}

## Initialize dataframe to hold means and SDs # #
scaleValues = data.frame()


# Vetsa1 Verbal Ability 
df$zvoctran = scale(df$voctran)
scaleValues = addScaleVals(scaleValues, "voctran", df$zvoctran)

df$Verbal = df$zvoctran
df$zVerbal = scale(df$Verbal)
scaleValues = addScaleVals(scaleValues, "Verbal", df$zVerbal)

# vetsa1 Visual-Spatial Ability 
df$zMR1COR = scale(df$MR1COR)
scaleValues = addScaleVals(scaleValues, "MR1COR", df$zMR1COR)
df$zHFTOTCOR = scale(df$HFTOTCOR)
scaleValues = addScaleVals(scaleValues, "HFTOTCOR", df$zHFTOTCOR)

df$VisSpat = rowMeans(df[c("zMR1COR","zHFTOTCOR")])
df$zVisSpat = scale(df$VisSpat)
scaleValues = addScaleVals(scaleValues, "VisSpat", df$zVisSpat)

# vetsa1 Abstract Reasoning 
df$zMTXTRAN = scale(df$MTXTRAN)
scaleValues = addScaleVals(scaleValues, "MTXTRAN", df$zMTXTRAN)

df$AbsReason = as.numeric(df$zMTXTRAN)
df$zAbsReason = scale(df$AbsReason)
scaleValues = addScaleVals(scaleValues, "AbsReason", df$zAbsReason)

# Vetsa1 Working Memory 
df$zdsfraw = scale(df$dsfraw)
scaleValues = addScaleVals(scaleValues, "dsfraw", df$zdsfraw)
df$zdsbraw = scale(df$dsbraw)
scaleValues = addScaleVals(scaleValues, "dsbraw", df$zdsbraw)
df$zlntot = scale(df$lntot)
scaleValues = addScaleVals(scaleValues, "lntot", df$zlntot)
df$zsspfraw = scale(df$sspfraw)
scaleValues = addScaleVals(scaleValues, "sspfraw", df$zsspfraw)
df$zsspbraw = scale(df$sspbraw)
scaleValues = addScaleVals(scaleValues, "sspbraw", df$zsspbraw)
df$zrsatotrevtran = scale(df$RSATOTrevtran) 
scaleValues = addScaleVals(scaleValues, "RSATOTrevtran", df$zrsatotrevtran)

df$STWKMem = rowMeans(df[,c("zdsfraw","zdsbraw",
                                              "zlntot","zsspfraw",
                                              "zsspbraw","zrsatotrevtran")])
df$zSTWKMem = scale(df$STWKMem)
scaleValues = addScaleVals(scaleValues, "STWKMem", df$zSTWKMem)

# vetsa1 Episodic Memory 
df$zcvatot = scale(df$cvatot)
scaleValues = addScaleVals(scaleValues, "cvatot", df$zcvatot)
df$zcvsdfr = scale(df$CVSDFR)
scaleValues = addScaleVals(scaleValues, "CVSDFR", df$zcvsdfr)
df$zcvldfr = scale(df$CVLDFR)
scaleValues = addScaleVals(scaleValues, "CVLDFR", df$zcvldfr)
df$zlmitot = scale(df$lmitot)
scaleValues = addScaleVals(scaleValues, "lmitot", df$zlmitot)
df$zlmdtot = scale(df$lmdtot)
scaleValues = addScaleVals(scaleValues, "lmdtot", df$zlmdtot)
df$zvritot = scale(df$vritot)
scaleValues = addScaleVals(scaleValues, "vritot", df$zvritot)
df$zvrdtot = scale(df$vrdtot)
scaleValues = addScaleVals(scaleValues, "vrdtot", df$zvrdtot)

df$EpsMem = rowMeans(df[,c("zcvatot","zcvsdfr",
                                             "zcvldfr","zlmitot",
                                             "zlmdtot","zvritot",
                                             "zvrdtot")])
df$zEpsMem = scale(df$EpsMem)
scaleValues = addScaleVals(scaleValues, "EpsMem", df$zEpsMem)

# Vetsa1 Verbal Fluency 
df$zlfcor = scale(df$LFCOR)
scaleValues = addScaleVals(scaleValues, "LFCOR", df$zlfcor)
df$zcfcor = scale(df$CFCOR)
scaleValues = addScaleVals(scaleValues, "CFCOR", df$zcfcor)

df$VerbFlu = rowMeans(df[,c("zlfcor","zcfcor")])
df$zVerbFlu = scale(df$VerbFlu)
scaleValues = addScaleVals(scaleValues, "VerbFlu", df$zVerbFlu)

# Vetsa1 Processing Speed
df$zstrwraw = scale(df$strwraw)
scaleValues = addScaleVals(scaleValues, "strwraw", df$zstrwraw)
df$zstrcraw = scale(df$strcraw)
scaleValues = addScaleVals(scaleValues, "strcraw", df$zstrcraw)
df$ztrl2tran = scale(df$TRL2TRAN)
scaleValues = addScaleVals(scaleValues, "TRL2TRAN", df$ztrl2tran)
df$ztrl3tran = scale(df$TRL3TRAN)
scaleValues = addScaleVals(scaleValues, "TRL3TRAN", df$ztrl3tran)

df$ProcSpeed = rowMeans(df[,c("zstrwraw","zstrcraw",
                                                "ztrl2tran","ztrl3tran")],na.rm=T)
df$zProcSpeed = scale(df$ProcSpeed)
scaleValues = addScaleVals(scaleValues, "ProcSpeed", df$zProcSpeed)

# Vetsa1 Executive Functioning - Trails Switching 
df$ztrl4adjtran = scale(df$TRL4ADJTRAN) 
scaleValues = addScaleVals(scaleValues, "TRL4ADJTRAN", df$ztrl4adjtran)

df$ExecTrailsSwitch = as.numeric(df$ztrl4adjtran )
df$zExecTrailsSwitch = scale(df$ExecTrailsSwitch)
scaleValues = addScaleVals(scaleValues, "ExecTrailsSwitch", df$zExecTrailsSwitch)

# vetsa1 Executive Functioning - Category Switching
df$zCSSACCADJ = scale(df$CSSACCADJ)
scaleValues = addScaleVals(scaleValues, "CSSACCADJ", df$zCSSACCADJ)

df$ExecCategorySwitch = as.numeric(df$zCSSACCADJ)
df$zExecCategorySwitch = scale(df$ExecCategorySwitch)
scaleValues = addScaleVals(scaleValues, "ExecCategorySwitch", df$zExecCategorySwitch)

# vetsa1 Executive Functioing - Inhibition
df$zstrit = scale(df$STRIT) 
scaleValues = addScaleVals(scaleValues, "strit", df$zstrit)

df$ExecInhibit = as.numeric(df$zstrit)
df$zExecInhibit = scale(df$ExecInhibit)
scaleValues = addScaleVals(scaleValues, "ExecInhibit", df$zExecInhibit)

#-------------------#
#  Save out datset  #
#-------------------#

# Select cognitive domain variables
vetsa1CogDomains = df %>%
  dplyr::select(vetsaid,zVerbal,zvoctran,zVisSpat,zMR1COR,zHFTOTCOR,
                zSTWKMem,zdsfraw,zdsbraw,zlntot,zsspfraw,zsspbraw,
                zrsatotrevtran,zEpsMem,zcvatot,zcvsdfr,zcvldfr,
                zlmitot,zlmdtot,zvritot,zvrdtot,zAbsReason,zMTXTRAN,
                zVerbFlu,zlfcor,zcfcor,zExecTrailsSwitch,ztrl4adjtran,
                zProcSpeed,zstrwraw,zstrcraw,ztrl2tran,ztrl3tran,
                zExecCategorySwitch,zCSSACCADJ,zExecInhibit,zstrit) 

# Save out data
write.csv(vetsa1CogDomains, 
          "K:/Projects/Cognitive Domains/data/V1_CognitiveDomains.csv",
          row.names = F)

# Save out Means and SDs for use in scaling Vetsa 2 data
write.csv(scaleValues, 
          "/home/jelman/netshare/K/Projects/Cognitive Domains/data/V1_CognitiveDomains_Means_SDs.csv",
          row.names = F)
