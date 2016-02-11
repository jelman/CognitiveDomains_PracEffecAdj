library(sjmisc)
library(dplyr)
library(psych)

# Load vetsa 2 merged data
vetsa1Dat = read_sas("K:/data/VETSA1_Aug2014/vetsa1merged_25aug2015_nomiss.sas7bdat")

##########################################
# Replace missing data and trim outliers #
##########################################

#----------------#
# Verbal Ability #
#----------------#

vetsa1Dat$VOCRAW[which(vetsa1Dat$ZVOCAB=="2")] = NA
# Transformed VOCRAW
vetsa1Dat$voctran = -1 * sqrt(80-vetsa1Dat$VOCRAW)

#---------------------#
# Visual-Spatial Data #
#---------------------#

## Mental Rotation ##

# vetsa1 MENTAL ROTATION: Total Correct PART1
vetsa1Dat$MR1COR[which(vetsa1Dat$ZMENROT==2)] = NA
vetsa1Dat$MR2COR[which(vetsa1Dat$ZMENROT==2)] = NA
# MENTAL ROTATION: Total Correct PARTS 1&2
vetsa1Dat$MRTOTCOR= vetsa1Dat$MR1COR + vetsa1Dat$MR2COR

hfCols = c("HFIGC1","HFIGC3","HFIGC5")
vetsa1Dat[which(vetsa1Dat$ZHF==2), hfCols] = NA

# vetsa1 Hidden Figures Total Correct Parts 1,3,5
vetsa1Dat$HFTOTCOR = rowSums(vetsa1Dat[hfCols])

#--------------------#
# Abstract Reasoning #
#--------------------#

vetsa1Dat$MTXRAW[which(vetsa1Dat$ZMATRIX=="2")] = NA
vetsa1Dat$MTXRAW[which(vetsa1Dat$MTXRAW=="0")] = NA

# Square-root transformed MTXRAW
vetsa1Dat$MTXTRAN = -1*sqrt(36 - vetsa1Dat$MTXRAW)


#-------------------------------#
# Short Term and Working Memory #
#-------------------------------#

## Digit Span ##
vetsa1Dat$dsfraw[which(vetsa1Dat$ZDGSPNF=="2")] = NA
vetsa1Dat$dsbraw[which(vetsa1Dat$ZDGSPNB=="2")] = NA

## Letter-Number Sequencing ##
vetsa1Dat$lntot[which(vetsa1Dat$ZLNS=="2")] = NA
vetsa1Dat$lntot[which(vetsa1Dat$lntot=="0")] = NA

## Spatial Span ##
vetsa1Dat$sspfraw[which(vetsa1Dat$ZSPSPNF=="2")] = NA
vetsa1Dat$sspbraw[which(vetsa1Dat$ZSPSPNB=="2")] = NA

## Reading Span ##
vetsa1Dat$RSATOTrev[which(vetsa1Dat$ZRDSPNA=="2")] = NA

# Square-root Transformed Reading Span
vetsa1Dat$RSATOTrevtran = -1*sqrt(46 - vetsa1Dat$RSATOTrev)

#-----------------#
# Episodic Memory #
#-----------------#

## CVLT ##
cvltiCols = c("cvatot","CVSDFR")
vetsa1Dat[which(vetsa1Dat$ZCVLTI=="2"), cvltiCols] = NA

vetsa1Dat$CVLDFR[which(vetsa1Dat$ZCVLTD=="2")] = NA
vetsa1Dat$cvatot[which(vetsa1Dat$cva5raw=="0")] = NA

cvltfrCols = c("CVSDFR","CVLDFR")
vetsa1Dat[which(vetsa1Dat$cvatot=="0"), cvltfrCols] = NA

## Logical Memory ##

logmemCols = c("lmitot","lmdtot")
vetsa1Dat[which(vetsa1Dat$ZLMI=="2"), logmemCols] = NA

vetsa1Dat$lmdtot[which(vetsa1Dat$ZLMD=="2")] = NA
vetsa1Dat$lmdtot[is.na(vetsa1Dat$lmitot)] = NA

## Visual Reproduction ##

vrCols = c("vritot","vrdtot")
vetsa1Dat[which(vetsa1Dat$ZVRI=="2"), vrCols] = NA

vetsa1Dat$vrdtot[which(vetsa1Dat$ZVRD=="2")] = NA
vetsa1Dat$vrdtot[is.na(vetsa1Dat$vritot)] = NA

#--------------------#
# Executive Function #
#--------------------#

## Trails Condition 4 ##

vetsa1Dat$TRL4T[which(vetsa1Dat$ZTRAIL4=="2")] = NA
vetsa1Dat$TRL4T[is.na(vetsa1Dat$TRL2T)] = NA
vetsa1Dat$TRL4T[is.na(vetsa1Dat$TRL3T)] = NA

# vetsa1 Trails 4 Adjusted for Trails 2&3
vetsa1Dat$TRL4TADJ = with(vetsa1Dat, 
                             (TRL4T - (TRL2T*0.7752) + (TRL3T*1.0438)))
# vetsa1 SQRT-Transformed Trails 4 Adjusted
vetsa1Dat$TRL4ADJTRAN = -1*sqrt(80 + vetsa1Dat$TRL4TADJ)

## Category Fluency Switching ##
vetsa1Dat$CFCOR[which(vetsa1Dat$ZFLUC=="2")] = NA
vetsa1Dat$CSSACC[which(vetsa1Dat$ZFLUCS=="2")] = NA

# vetsa1 Category Switching Accuracy - Adjusted
vetsa1Dat$CSSACCADJ = vetsa1Dat$CSSACC - (0 + (vetsa1Dat$CFCOR*0.1329))

## Stroop ##
vetsa1Dat$STRIT [which(vetsa1Dat$STRIT=="1")] = NA
vetsa1Dat$STRIT[which(vetsa1Dat$STRIT>90)] = NA

vetsa1Dat$strcwraw[which(vetsa1Dat$ZSTROOPCW=="2") ] = NA
vetsa1Dat$strcwraw[is.na(vetsa1Dat$strwraw)] = NA
vetsa1Dat$strcwraw[is.na(vetsa1Dat$strcraw)] = NA

vetsa1Dat[which(vetsa1Dat$vetsaid=="19885B"),"strcraw"] = 86
vetsa1Dat[which(vetsa1Dat$vetsaid=="19885B"),"strcwraw"] = 45

#----------------#
# Verbal Fluency #
#----------------#

vetsa1Dat$LFCOR[which(vetsa1Dat$ZFLUL=="2")] = NA
vetsa1Dat$CFCOR[which(vetsa1Dat$ZFLUC=="2")] = NA

#------------------#
# Processing Speed #
#------------------#

## Stroop ##
vetsa1Dat$strcraw[which(vetsa1Dat$ZSTROOPC=="2")] = NA
vetsa1Dat$strwraw[which(vetsa1Dat$ZSTROOPW=="2")] = NA

## Trails ##
vetsa1Dat$TRL2T[which(vetsa1Dat$ZTRAIL2=="2")] = NA
vetsa1Dat$TRL3T[which(vetsa1Dat$ZTRAIL3=="2")] = NA

vetsa1Dat$TRL2T[which(vetsa1Dat$TRL2T>120)] = NA
vetsa1Dat$TRL3T[which(vetsa1Dat$TRL3T>120)] = NA

# Log Transformed Trails
vetsa1Dat$TRL2TRAN = -1*log(vetsa1Dat$TRL2T)
vetsa1Dat$TRL3TRAN = -1*log(vetsa1Dat$TRL3T)


#########################################################
# Creating standardized scores and composite scores.    #
# Standardization is based off of VETSA1 Means and SDs  #
#########################################################

# VETSA1 Verbal Ability
vetsa1Dat$zVerbal = (vetsa1Dat$voctran - (-4.0930)) / 0.9726

# Standardized MR1COR
vetsa1Dat$zMR1COR = (vetsa1Dat$MR1COR - 47.5498) / 13.4463

# Standardized HFTOTCOR
vetsa1Dat$zHFTOTCOR = (vetsa1Dat$HFTOTCOR - 21.2055) / 9.0907

# vetsa1 Visual-Spatial Ability
vetsa1Dat$zVisSpat = rowMeans(vetsa1Dat[c("zMR1COR","zHFTOTCOR")])

# vetsa1 Abstract Reasoning
vetsa1Dat$zAbsReason = (vetsa1Dat$MTXTRAN - (-3.5041)) / 0.7799

vetsa1Dat$zdsfraw = (vetsa1Dat$dsfraw - 10.1910)/2.2851
vetsa1Dat$zdsbraw = (vetsa1Dat$dsbraw - 6.5109)/2.1673
vetsa1Dat$zlntot = (vetsa1Dat$lntot - 10.1434)/2.3546
vetsa1Dat$zsspfraw = (vetsa1Dat$sspfraw - 8.0273)/1.6550
vetsa1Dat$zsspbraw = (vetsa1Dat$sspbraw - 7.4063)/1.8013
# Standardized/Transformed Reading Span
vetsa1Dat$zrsatotrevtran = (vetsa1Dat$RSATOTrevtran - (-3.2157))/0.7699 

# vetsa1 Working Memory
vetsa1Dat$zSTWKMem = rowMeans(vetsa1Dat[,c("zdsfraw","zdsbraw",
                                              "zlntot","zsspfraw",
                                              "zsspbraw","zrsatotrevtran")])

vetsa1Dat$zcvatot = (vetsa1Dat$cvatot - 42.8466)/8.5101
vetsa1Dat$zcvsdfr = (vetsa1Dat$CVSDFR - 8.6420)/2.7382 
vetsa1Dat$zcvldfr = (vetsa1Dat$CVLDFR - 9.0669)/2.8865 
vetsa1Dat$zlmitot = (vetsa1Dat$lmitot - 23.4731)/6.1563
vetsa1Dat$zlmdtot = (vetsa1Dat$lmdtot - 20.0125)/6.6278
vetsa1Dat$zvritot = (vetsa1Dat$vritot - 78.2430)/12.4047
vetsa1Dat$zvrdtot = (vetsa1Dat$vrdtot - 54.7521)/19.5090
#vetsa1 Episodic Memory
vetsa1Dat$zEpsMem = rowMeans(vetsa1Dat[,c("zcvatot","zcvsdfr",
                                             "zcvldfr","zlmitot",
                                             "zlmdtot","zvritot",
                                             "zvrdtot")])

vetsa1Dat$zlfcor = (vetsa1Dat$LFCOR - 36.8792)/10.6089
vetsa1Dat$zcfcor = (vetsa1Dat$CFCOR - 38.2818)/7.5251 
# vetsa1 Verbal Fluency
vetsa1Dat$zVerbFlu = rowMeans(vetsa1Dat[,c("zlfcor","zcfcor")])


vetsa1Dat$zstrwraw = (vetsa1Dat$strwraw - 93.4668)/14.3125
vetsa1Dat$zstrcraw = (vetsa1Dat$strcraw - 69.2195)/11.3414
vetsa1Dat$ztrl2tran = (vetsa1Dat$TRL2TRAN - (-3.4530))/0.3138
vetsa1Dat$ztrl3tran = (vetsa1Dat$TRL3TRAN - (-3.4520))/0.3274
vetsa1Dat$ztrl4tran = (vetsa1Dat$TRL4ADJTRAN - (-10.3190))/1.2969 

# vetsa1 Processing Speed
vetsa1Dat$zProcSpeed = rowMeans(vetsa1Dat[,c("zstrwraw","zstrcraw",
                                                "ztrl2tran","ztrl3tran")])

# vetsa1 Executive Functioning - Trails Switching
vetsa1Dat$zExecTrailsSwitch = vetsa1Dat$ztrl4tran 

# vetsa1 Executive Functioning - Category Switching
vetsa1Dat$zExecCategorySwitch = (vetsa1Dat$CSSACCADJ - 6.4447)/2.8568

# vetsa1 Executive Functioing - Inhibition
vetsa1Dat$zExecInhibit = (vetsa1Dat$STRIT - 47.1756)/6.8088 


#-------------------#
#  Save out datset  #
#-------------------#

# Select cognitive domain variables
vetsa1CogDomains = vetsa1Dat %>%
  dplyr::select(vetsaid,zVisSpat,zMR1COR,zHFTOTCOR,zSTWKMem,
                zdsfraw,zdsbraw,zlntot,zsspfraw,zsspbraw,
                zrsatotrevtran,zEpsMem,zcvatot,zcvsdfr,zcvldfr,
                zlmitot,zlmdtot,zvritot,zvrdtot,zAbsReason,
                zVerbFlu,zlfcor,zcfcor,zExecTrailsSwitch,
                zExecCategorySwitch,zExecInhibit,zProcSpeed,
                zstrwraw,zstrcraw,ztrl2tran,ztrl3tran, ztrl4tran) 

# Save out data
write.csv(vetsa1CogDomains, 
          "K:/Projects/Cognitive Domains/data/V1_CognitiveDomains.csv",
          row.names = F)
