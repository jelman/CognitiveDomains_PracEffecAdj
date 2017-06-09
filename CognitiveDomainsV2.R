################################################################################
# Calculates cognitive domains for VETSA 2.                                    #
#                                                                              #
# Based on syntax files in:                                                    #
# M:/PSYCH/KREMEN/VETSA DATA FILES_852014/Cognitive Domains_April 2015 syntax/ #
#                                                                              #
# Individual tests from each domain are filtered based on quality              #
# score ("z" rating). Tests scores are then standardized based on              #
# VETSA 1 means and standard deviations. The composite score is                #
# calculated as the mean of standardized test scores within each domain.       #
#                                                                              #
# Note: The mean of z-scored tests is not itself a z-score. Domain comprised   #
# of multiple tests may have an SD less than 1. Therefore, domains are further #
# standardized based on VETSA 1 means and SDs                                  #
################################################################################

library(sjmisc)
library(dplyr)
library(psych)

# Load vetsa 2 merged data
vetsa2Dat = read_sas("/home/jelman/netshare/K/data/VETSA2_April2015/vetsa2merged_1dec2015_edits.sas7bdat")

# Convert to dataframe to avoid problems with attributes and labels later
vetsa2Dat = tbl_df(vetsa2Dat)

##########################################
# Replace missing data and trim outliers #
##########################################

#------------------#
# Processing Speed #
#------------------#

## Stroop ##
vetsa2Dat$strcraw_V2[which(vetsa2Dat$ZSTROOPC_v2=="2")] = NA
vetsa2Dat$strwraw_V2[which(vetsa2Dat$ZSTROOPW_v2=="2")] = NA

## Trails ##
vetsa2Dat$TRL2T_v2[which(vetsa2Dat$ZTRAIL2_v2=="2")] = NA
vetsa2Dat$TRL3T_v2[which(vetsa2Dat$ZTRAIL3_v2=="2")] = NA

vetsa2Dat$TRL2T_v2[which(vetsa2Dat$TRL2T_v2>120)] = NA
vetsa2Dat$TRL3T_v2[which(vetsa2Dat$TRL3T_v2>120)] = NA

# Log Transformed Trails
vetsa2Dat$TRL2TRAN_v2 = -1*log(vetsa2Dat$TRL2T_v2)
vetsa2Dat$TRL3TRAN_v2 = -1*log(vetsa2Dat$TRL3T_v2)

#---------------------#
# Visual-Spatial Data #
#---------------------#

## Mental Rotation ##

# VETSA2 MENTAL ROTATION: Total Correct PART1
vetsa2Dat$MR1COR_v2[which(vetsa2Dat$ZMENROT_v2==2)] = NA

hfCols = c("HFIGC1_v2","HFIGC3_v2","HFIGC5_v2")
vetsa2Dat[which(vetsa2Dat$ZHF_v2==2), hfCols] = NA

# VETSA2 Hidden Figures Total Correct Parts 1,3,5
vetsa2Dat$HFTOTCOR_V2 = rowSums(vetsa2Dat[hfCols])

#--------------------#
# Abstract Reasoning #
#--------------------#

vetsa2Dat$MTXRAW_v2[which(vetsa2Dat$ZMATRIX_v2=="2")] = NA
vetsa2Dat$MTXRAW_v2[which(vetsa2Dat$MTXRAW_v2=="0")] = NA

# Square-root transformed MTXRAW_v2
vetsa2Dat$MTXTRAN_v2 = -1*sqrt(36 - vetsa2Dat$MTXRAW_v2)

#-------------------------------#
# Short Term and Working Memory #
#-------------------------------#

## Digit Span ##
vetsa2Dat$dsfraw_V2[which(vetsa2Dat$ZDGSPNF_v2=="2")] = NA
vetsa2Dat$dsbraw_V2[which(vetsa2Dat$ZDGSPNB_v2=="2")] = NA

## Letter-Number Sequencing ##
vetsa2Dat$lntot_V2[which(vetsa2Dat$ZLNS_v2=="2")] = NA
vetsa2Dat$lntot_V2[which(vetsa2Dat$lntot_V2=="0")] = NA

## Spatial Span ##
vetsa2Dat$sspfraw_V2[which(vetsa2Dat$ZSPSPNF_v2=="2")] = NA
vetsa2Dat$sspbraw_V2[which(vetsa2Dat$ZSPSPNB_v2=="2")] = NA

## Reading Span ##
vetsa2Dat$RSATOT_V2[which(vetsa2Dat$ZRDSPNA_v2=="2")] = NA

# Square-root Transformed Reading Span
vetsa2Dat$RSATOTTRAN_V2 = -1*sqrt(46 - vetsa2Dat$RSATOT_V2)

#-----------------#
# Episodic Memory #
#-----------------#

## CVLT ##
cvltiCols = c("cvatot_v2","CVSDFR_v2")
vetsa2Dat[which(vetsa2Dat$ZCVLTI_v2=="2"), cvltiCols] = NA

vetsa2Dat$CVLDFR_v2[which(vetsa2Dat$ZCVLTD_v2=="2")] = NA
vetsa2Dat$cvatot_v2[which(vetsa2Dat$cva5raw_v2=="0")] = NA

cvltfrCols = c("CVSDFR_v2","CVLDFR_v2")
vetsa2Dat[which(vetsa2Dat$cvatot_v2=="0"), cvltfrCols] = NA

## Logical Memory ##

logmemCols = c("lmitot_V2","lmdtot_V2")
vetsa2Dat[which(vetsa2Dat$ZLMI_v2=="2"), logmemCols] = NA

vetsa2Dat$lmdtot_V2[which(vetsa2Dat$ZLMD_v2=="2")] = NA
vetsa2Dat$lmdtot_V2[is.na(vetsa2Dat$lmitot_V2)] = NA

## Visual Reproduction ##

vrCols = c("vritot_V2","vrdtot_V2")
vetsa2Dat[which(vetsa2Dat$ZVRI_v2=="2"), vrCols] = NA

vetsa2Dat$vrdtot_V2[which(vetsa2Dat$ZVRD_v2=="2")] = NA
vetsa2Dat$vrdtot_V2[is.na(vetsa2Dat$vritot_V2)] = NA

#--------------------#
# Executive Function #
#--------------------#

## Trails Condition 4 ##

vetsa2Dat$TRL4T_v2[which(vetsa2Dat$ZTRAIL4_v2=="2")] = NA
vetsa2Dat$TRL4T_v2[is.na(vetsa2Dat$TRL2T_v2)] = NA
vetsa2Dat$TRL4T_v2[is.na(vetsa2Dat$TRL3T_v2)] = NA

# VETSA2 Trails 4 Adjusted for Trails 2&3
vetsa2Dat$TRL4TADJ_v2 = with(vetsa2Dat, 
                        (TRL4T_v2 - (TRL2T_v2*0.7623) + (TRL3T_v2*1.3343)))
# VETSA2 SQRT-Transformed Trails 4 Adjusted
vetsa2Dat$TRL4ADJTRAN_v2 = -1*sqrt(80 + vetsa2Dat$TRL4TADJ_v2)

## Category Fluency Switching ##
vetsa2Dat$CFCOR_V2[which(vetsa2Dat$ZFLUC_v2=="2")] = NA
vetsa2Dat$CSSACC_v2[which(vetsa2Dat$ZFLUCS_v2=="2")] = NA

# VETSA2 Category Switching Accuracy - Adjusted
vetsa2Dat$CSSACCADJ_v2 = vetsa2Dat$CSSACC_v2 - (0 + (vetsa2Dat$CFCOR_V2*0.1400))

## Stroop ##
vetsa2Dat$strit_V2[which(vetsa2Dat$strit_V2=="1")] = NA
vetsa2Dat$strit_V2[which(vetsa2Dat$strit_V2>90)] = NA

vetsa2Dat$strcwraw_V2[which(vetsa2Dat$ZSTROOPCW_v2=="2") ] = NA
vetsa2Dat$strcwraw_V2[is.na(vetsa2Dat$strwraw_V2)] = NA
vetsa2Dat$strcwraw_V2[is.na(vetsa2Dat$strcraw_V2)] = NA

# VETSA2 Stroop Color/Word Adjusted
vetsa2Dat$strcwadj_v2 = with(vetsa2Dat, 
                    strcwraw_V2 - (0 + (strwraw_V2*0.1390) + (strcraw_V2*0.3269)))

## Verbal Fluency ##

vetsa2Dat$LFCOR_V2[which(vetsa2Dat$ZFLUL_v2=="2")] = NA
vetsa2Dat$CFCOR_V2[which(vetsa2Dat$ZFLUC_v2=="2")] = NA


#########################################################
# Creating standardized scores and composite scores.    #
# Standardization is based off of VETSA1 Means and SDs #
#########################################################

# Load means and SDs from Vetsa 1
scaleValues = read.csv("/home/jelman/netshare/K/Projects/Cognitive Domains/data/V1_CognitiveDomains_Means_SDs.csv")

# VETSA2 Visual-Spatial Ability #
vetsa2Dat$zMR1COR_v2 = scale(vetsa2Dat$MR1COR_v2, 
                       center=scaleValues$Mean[scaleValues$Variable=="MR1COR"]
                       ,scale=scaleValues$SD[scaleValues$Variable=="MR1COR"])
vetsa2Dat$zHFTOTCOR_v2 = scale(vetsa2Dat$HFTOTCOR_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="HFTOTCOR"],
                         scale=scaleValues$SD[scaleValues$Variable=="HFTOTCOR"])

vetsa2Dat$VisSpat_v2 = rowMeans(vetsa2Dat[c("zMR1COR_v2","zHFTOTCOR_v2")])
vetsa2Dat$zVisSpat_v2 = scale(vetsa2Dat$VisSpat_v2,
                              center=scaleValues$Mean[scaleValues$Variable=="VisSpat"],
                              scale=scaleValues$SD[scaleValues$Variable=="VisSpat"])

# VETSA2 Abstract Reasoning #
vetsa2Dat$zMTXTRAN_v2 = scale(vetsa2Dat$MTXTRAN_v2, 
                              center=scaleValues$Mean[scaleValues$Variable=="MTXTRAN"],
                              scale=scaleValues$SD[scaleValues$Variable=="MTXTRAN"])
vetsa2Dat$AbsReason_v2 = vetsa2Dat$zMTXTRAN_v2
vetsa2Dat$zAbsReason_v2 = scale(vetsa2Dat$AbsReason_v2,
                              center=scaleValues$Mean[scaleValues$Variable=="AbsReason"],
                              scale=scaleValues$SD[scaleValues$Variable=="AbsReason"])

# VETSA2 Working Memory
vetsa2Dat$zdsfraw_v2 = scale(vetsa2Dat$dsfraw_V2, 
                        center=scaleValues$Mean[scaleValues$Variable=="dsfraw"],
                        scale=scaleValues$SD[scaleValues$Variable=="dsfraw"])
vetsa2Dat$zdsbraw_v2 = scale(vetsa2Dat$dsfraw_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="dsbraw"],
                         scale=scaleValues$SD[scaleValues$Variable=="dsbraw"])
vetsa2Dat$zlntot_v2 = scale(vetsa2Dat$lntot_V2, 
                           center=scaleValues$Mean[scaleValues$Variable=="lntot"],
                           scale=scaleValues$SD[scaleValues$Variable=="lntot"])
vetsa2Dat$zsspfraw_v2 = scale(vetsa2Dat$sspfraw_V2, 
                          center=scaleValues$Mean[scaleValues$Variable=="sspfraw"],
                          scale=scaleValues$SD[scaleValues$Variable=="sspfraw"])
vetsa2Dat$zsspbraw_v2 = scale(vetsa2Dat$sspbraw_V2, 
                          center=scaleValues$Mean[scaleValues$Variable=="sspbraw"],
                          scale=scaleValues$SD[scaleValues$Variable=="sspbraw"])
vetsa2Dat$zrsatottran_v2 = scale(vetsa2Dat$RSATOTTRAN_V2, 
                    center=scaleValues$Mean[scaleValues$Variable=="RSATOTrevtran"],
                    scale=scaleValues$SD[scaleValues$Variable=="RSATOTrevtran"])

vetsa2Dat$STWKMem_v2 = rowMeans(vetsa2Dat[,c("zdsfraw_v2","zdsbraw_v2",
                                              "zlntot_v2","zsspfraw_v2",
                                              "zsspbraw_v2","zrsatottran_v2")])
vetsa2Dat$zSTWKMem_v2 = scale(vetsa2Dat$STWKMem_v2,
                                center=scaleValues$Mean[scaleValues$Variable=="STWKMem"],
                                scale=scaleValues$SD[scaleValues$Variable=="STWKMem"])

# VETSA2 Episodic Memory #
vetsa2Dat$zcvatot_v2 = scale(vetsa2Dat$cvatot_v2, 
                        center=scaleValues$Mean[scaleValues$Variable=="cvatot"],
                        scale=scaleValues$SD[scaleValues$Variable=="cvatot"])
vetsa2Dat$zcvsdfr_v2 = scale(vetsa2Dat$CVSDFR_v2, 
                         center=scaleValues$Mean[scaleValues$Variable=="CVSDFR"],
                         scale=scaleValues$SD[scaleValues$Variable=="CVSDFR"])
vetsa2Dat$zcvldfr_v2 = scale(vetsa2Dat$CVLDFR_v2, 
                         center=scaleValues$Mean[scaleValues$Variable=="CVLDFR"],
                         scale=scaleValues$SD[scaleValues$Variable=="CVLDFR"])
vetsa2Dat$zlmitot_v2 = scale(vetsa2Dat$lmitot_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="lmitot"],
                         scale=scaleValues$SD[scaleValues$Variable=="lmitot"])
vetsa2Dat$zlmdtot_v2 = scale(vetsa2Dat$lmdtot_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="lmdtot"],
                         scale=scaleValues$SD[scaleValues$Variable=="lmdtot"])
vetsa2Dat$zvritot_v2 = scale(vetsa2Dat$vritot_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="vritot"],
                         scale=scaleValues$SD[scaleValues$Variable=="vritot"])
vetsa2Dat$zvrdtot_v2 = scale(vetsa2Dat$vrdtot_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="vrdtot"],
                         scale=scaleValues$SD[scaleValues$Variable=="vrdtot"])

vetsa2Dat$EpsMem_v2 = rowMeans(vetsa2Dat[,c("zcvatot_v2","zcvsdfr_v2",
                                             "zcvldfr_v2","zlmitot_v2",
                                             "zlmdtot_v2","zvritot_v2",
                                             "zvrdtot_v2")])
vetsa2Dat$zEpsMem_v2 = scale(vetsa2Dat$EpsMem_v2,
                              center=scaleValues$Mean[scaleValues$Variable=="EpsMem"],
                              scale=scaleValues$SD[scaleValues$Variable=="EpsMem"])

# VETSA2 Verbal Fluency #
vetsa2Dat$zlfcor_v2 = scale(vetsa2Dat$LFCOR_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="LFCOR"],
                         scale=scaleValues$SD[scaleValues$Variable=="LFCOR"])
vetsa2Dat$zcfcor_v2 = scale(vetsa2Dat$CFCOR_V2, 
                        center=scaleValues$Mean[scaleValues$Variable=="CFCOR"],
                        scale=scaleValues$SD[scaleValues$Variable=="CFCOR"])

vetsa2Dat$VerbFlu_v2 = rowMeans(vetsa2Dat[,c("zlfcor_v2","zcfcor_v2")])
vetsa2Dat$zVerbFlu_v2 = scale(vetsa2Dat$VerbFlu_v2,
                             center=scaleValues$Mean[scaleValues$Variable=="VerbFlu"],
                             scale=scaleValues$SD[scaleValues$Variable=="VerbFlu"])

# VETSA2 Processing Speed
vetsa2Dat$zstrwraw_v2 = scale(vetsa2Dat$strwraw_V2, 
                          center=scaleValues$Mean[scaleValues$Variable=="strwraw"],
                          scale=scaleValues$SD[scaleValues$Variable=="strwraw"])
vetsa2Dat$zstrcraw_v2 = scale(vetsa2Dat$strcwraw_V2, 
                          center=scaleValues$Mean[scaleValues$Variable=="strcraw"],
                          scale=scaleValues$SD[scaleValues$Variable=="strcraw"])
vetsa2Dat$ztrl2tran_v2 = scale(vetsa2Dat$TRL2TRAN_v2, 
                        center=scaleValues$Mean[scaleValues$Variable=="TRL2TRAN"],
                        scale=scaleValues$SD[scaleValues$Variable=="TRL2TRAN"])
vetsa2Dat$ztrl3tran_v2 = scale(vetsa2Dat$TRL3TRAN_v2, 
                       center=scaleValues$Mean[scaleValues$Variable=="TRL3TRAN"],
                       scale=scaleValues$SD[scaleValues$Variable=="TRL3TRAN"])

vetsa2Dat$ProcSpeed_v2 = rowMeans(vetsa2Dat[,c("zstrwraw_v2","zstrcraw_v2",
                                            "ztrl2tran_v2","ztrl3tran_v2")])
vetsa2Dat$zProcSpeed_v2 = scale(vetsa2Dat$ProcSpeed_v2,
                              center=scaleValues$Mean[scaleValues$Variable=="ProcSpeed"],
                              scale=scaleValues$SD[scaleValues$Variable=="ProcSpeed"])

# VETSA2 Executive Functioning - Trails Switching
vetsa2Dat$ztrl4adjtran_v2 = scale(vetsa2Dat$TRL4ADJTRAN_v2, 
                     center=scaleValues$Mean[scaleValues$Variable=="TRL4ADJTRAN"],
                     scale=scaleValues$SD[scaleValues$Variable=="TRL4ADJTRAN"])

vetsa2Dat$ExecTrailsSwitch_v2 = vetsa2Dat$ztrl4adjtran_v2 
vetsa2Dat$zExecTrailsSwitch_v2 = scale(vetsa2Dat$ExecTrailsSwitch_v2,
                                center=scaleValues$Mean[scaleValues$Variable=="ExecTrailsSwitch"],
                                scale=scaleValues$SD[scaleValues$Variable=="ExecTrailsSwitch"])

# VETSA2 Executive Functioning - Category Switching
vetsa2Dat$zCSSACCADJ_v2 = scale(vetsa2Dat$CSSACCADJ_v2, 
                       center=scaleValues$Mean[scaleValues$Variable=="CSSACCADJ"],
                       scale=scaleValues$SD[scaleValues$Variable=="CSSACCADJ"])

vetsa2Dat$ExecCategorySwitch_v2 = vetsa2Dat$zCSSACCADJ_v2
vetsa2Dat$zExecCategorySwitch_v2 = scale(vetsa2Dat$ExecCategorySwitch_v2,
                                       center=scaleValues$Mean[scaleValues$Variable=="ExecCategorySwitch"],
                                       scale=scaleValues$SD[scaleValues$Variable=="ExecCategorySwitch"])

# VETSA2 Executive Functioing - Inhibition
vetsa2Dat$zstrit_v2 = scale(vetsa2Dat$strit_V2, 
                         center=scaleValues$Mean[scaleValues$Variable=="strit"],
                         scale=scaleValues$SD[scaleValues$Variable=="strit"])
vetsa2Dat$ExecInhibit_v2 = vetsa2Dat$zstrit_v2
vetsa2Dat$zExecInhibit_v2 = scale(vetsa2Dat$ExecInhibit_v2,
                                         center=scaleValues$Mean[scaleValues$Variable=="ExecInhibit"],
                                         scale=scaleValues$SD[scaleValues$Variable=="ExecInhibit"])

#-------------------#
#  Save out datset  #
#-------------------#

<<<<<<< HEAD
zVars = c("zVisSpat_v2","zMR1COR_v2","zHFTOTCOR_v2","zSTWKMem_v2","zdsfraw_v2",
          "zdsbraw_v2","zlntot_v2","zsspfraw_v2","zsspbraw_v2","zrsatottran_v2",
          "zEpsMem_v2","zcvatot_v2","zcvsdfr_v2","zcvldfr_v2","zlmitot_v2",
          "zlmdtot_v2","zvritot_v2","zvrdtot_v2","zAbsReason_v2","zMTXTRAN_v2",
          "zVerbFlu_v2","zlfcor_v2","zcfcor_v2","zExecTrailsSwitch_v2",
          "ztrl4adjtran_v2","zExecCategorySwitch_v2","zCSSACCADJ_v2","zExecInhibit_v2",
          "zstrit_v2","zProcSpeed_v2","zstrwraw_v2","zstrcraw_v2","ztrl2tran_v2","ztrl3tran_v2")
rawVars = c("VisSpat_v2","MR1COR_v2","HFTOTCOR_V2","AbsReason_v2","MTXTRAN_v2","STWKMem_v2",
            "dsfraw_V2","dsbraw_V2","lntot_V2","sspfraw_V2","sspbraw_V2","RSATOTTRAN_V2",
            "EpsMem_v2","cvatot_v2","CVSDFR_v2","CVLDFR_v2","lmitot_V2","lmdtot_V2","vritot_V2",
            "vrdtot_V2","VerbFlu_v2","LFCOR_V2","CFCOR_V2","strwraw_V2","strcraw_V2","TRL2TRAN_v2",
            "TRL3TRAN_v2","ExecTrailsSwitch_v2","TRL4ADJTRAN_v2","ExecCategorySwitch_v2",
            "CSSACCADJ_v2","ExecInhibit_v2","strit_V2")

# Select all cognitive domain variables
vetsa2CogDomainsAll = vetsa2Dat %>%
  dplyr::select(vetsaid,one_of(zVars,rawVars))

# Save out data
write.csv(vetsa2CogDomainsAll, 
          "/home/jelman/netshare/K/Projects/Cognitive Domains/data/V2_CognitiveDomains_All.csv",
          row.names = F)

# Select only z-scored variables
vetsa2CogDomainsZ = vetsa2Dat %>%
  dplyr::select(vetsaid,one_of(zVars))

# Save out z-scored data only
write.csv(vetsa2CogDomainsZ, 
          "/home/jelman/netshare/K/Projects/Cognitive Domains/data/V2_CognitiveDomains_Zscored.csv",
=======
# Select cognitive domain variables
vetsa2CogDomains = vetsa2Dat %>%
  dplyr::select(vetsaid,zVisSpat_v2,zMR1COR_v2,zHFTOTCOR_v2,zSTWKMem_v2,
                zdsfraw_v2,zdsbraw_v2,zlntot_v2,zsspfraw_v2,zsspbraw_v2,
                zrsatottran_v2,zEpsMem_v2,zcvatot_v2,zcvsdfr_v2,zcvldfr_v2,
                zlmitot_v2,zlmdtot_v2,zvritot_v2,zvrdtot_v2,zAbsReason_v2,
                zMTXTRAN_v2,zVerbFlu_v2,zlfcor_v2,zcfcor_v2,zExecTrailsSwitch_v2,
                ztrl4adjtran_v2,zExecCategorySwitch_v2,zCSSACCADJ_v2,
                zExecInhibit_v2,zstrit_v2,zProcSpeed_v2,zstrwraw_v2,
                zstrcraw_v2,ztrl2tran_v2,ztrl3tran_v2) 

# Save out data
write.csv(vetsa2CogDomains, 
          "K:/Projects/Cognitive Domains/data/V2_CognitiveDomains.csv",
>>>>>>> parent of 68e7086... Save out raw scores in addition to z-scored variables
          row.names = F)
