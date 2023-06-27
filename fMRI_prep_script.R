#######################################
####fMRI metrics extraction script####
######################################

#Project: Long-term effects of prenatal infection on the human brain: a prospective multimodal neuroimaging study (in the Generation R cohort)

#Author: Anna Suleri 

rm(list=ls()) #clear environment 
set.seed(2022) #set seed for reproducibility script 

####Opening libraries----
libraries <- c("foreign", "haven", "dplyr")
invisible(lapply(libraries, require, character.only = T))

####Set working directory and load in data----
setwd("PUT_PATH_WHERE_YOU_KEEP_THE_DATA")
wd <- getwd()

df1 <- readRDS("within_between_fullCon_genr.rds") #matrix (see fMRI_matrix_extraction_script.R how this was calculated)
df2 <- readRDS("sub-id_has_conMat.rds") #linking with subject id file 
df3 <- read.csv("all_ses-F13_wei_Gordon_fsSC_corMatZ_8pAroma_graphMetrics_22aug2022.csv", header = T)
df4 <- as.integer(gsub('sub-','', df2)) #extracting characters from matched string in order to get just numbers

####Extracting graph theory metrics----
df3$IDC <- as.integer(gsub('sub-','', df3$subj)) #removing strings 
gtm_df <- df3[, c('IDC', 'cpl', 'ge', 'cc')] #selecting columns we need 
gtm_df$mod <- gtm_df$cc #because cc was coded as mod accidentally in python processing script so change name 
gtm_df2 <- select(gtm_df, -c('cc'))

setwd("PUT_PATH_WHERE_YOU_WANT_THE_RESULTS")

write.csv(gtm_df2, "fmri_graphtheory_static.csv")

####Extracting within and between connectivity----
#Making an empty dataframe in which we loop for each participant
#Then write a loop in which we are calculating the within and between Fc for each participant for all 13 networks 
fc_fMRI_df <- data.frame() #creating an empty df to feed all results in of for loop 

networks <- c('None', 'Default', 'ParietoOccip', 'FrontoParietal', 'Salience', 'CinguloOperc', 'MedialParietal', 'DorsalAttn', 'VentralAttn', 'Visual', 'SMhand', 'SMmouth', 'Auditory')

for (i in 1:length(df4)){ #for every subject id in df4
  for (n in networks){ #for every network as specified in vector 'networks'
  subID = df4[i] #defining subject ID as everyone in df 4 
  #Extracting within FC
  wiNetwork_none <- df1[[i]]["None", "None"] #selecting diagonal value for each network for each participant
  wiNetwork_default<- df1[[i]]["Default", "Default"]
  wiNetwork_parietooccip <- df1[[i]]["ParietoOccip", "ParietoOccip"]
  wiNetwork_frontoparietal<- df1[[i]]["FrontoParietal", "FrontoParietal"]
  wiNetwork_salience<- df1[[i]]["Salience", "Salience"]
  wiNetwork_cingulooperc<- df1[[i]]["CinguloOperc", "CinguloOperc"]
  wiNetwork_medialparietal <- df1[[i]]["MedialParietal", "MedialParietal"]
  wiNetwork_dorsalattn<- df1[[i]]["DorsalAttn", "DorsalAttn"]
  wiNetwork_ventralattn<- df1[[i]]["VentralAttn", "VentralAttn"]
  wiNetwork_visual<- df1[[i]]["Visual", "Visual"]
  wiNetwork_smhand<- df1[[i]]["SMhand", "SMhand"]
  wiNetwork_smmouth<- df1[[i]]["SMmouth", "SMmouth"]
  wiNetwork_auditory<- df1[[i]]["Auditory", "Auditory"]
  #Extracting between fc
  btNetworks <- c() #creating an empty vector in R 
  for (x in networks){
    if (x != n){ #so not including the diagonal value 
      btNetworks <- c(btNetworks, x)
    }
  }
  btNetwork_none <- mean(df1[[i]]['None',btNetworks]) #taking the mean of a network combined with all other networks 
  btNetwork_default<- mean(df1[[i]]['Default',btNetworks])
  btNetwork_ParietoOccip <- mean(df1[[i]]['ParietoOccip',btNetworks])
  btNetwork_FrontoParietal<- mean(df1[[i]]['FrontoParietal',btNetworks])
  btNetwork_Salience <- mean(df1[[i]]['Salience',btNetworks])
  btNetwork_CinguloOperc<- mean(df1[[i]]['CinguloOperc',btNetworks])
  btNetwork_MedialParietal<- mean(df1[[i]]['MedialParietal',btNetworks])
  btNetwork_DorsalAttn<- mean(df1[[i]]['DorsalAttn',btNetworks])
  btNetwork_VentralAttn<- mean(df1[[i]]['VentralAttn',btNetworks])
  btNetwork_Visual<- mean(df1[[i]]['Visual',btNetworks])
  btNetwork_SMhand<- mean(df1[[i]]['SMhand',btNetworks])
  btNetwork_SMmouth<- mean(df1[[i]]['SMmouth',btNetworks])
  btNetwork_Auditory<- mean(df1[[i]]['Auditory',btNetworks])
  #feeding output in empty df 
  fc_fMRI_df[i,1] <- wiNetwork_none
  fc_fMRI_df[i,2]<- wiNetwork_default
  fc_fMRI_df[i,3]<- wiNetwork_parietooccip
  fc_fMRI_df[i,4]<- wiNetwork_frontoparietal
  fc_fMRI_df[i,5]<- wiNetwork_salience
  fc_fMRI_df[i,6]<- wiNetwork_cingulooperc
  fc_fMRI_df[i,7]<- wiNetwork_medialparietal
  fc_fMRI_df[i,8]<- wiNetwork_dorsalattn
  fc_fMRI_df[i,9]<- wiNetwork_ventralattn
  fc_fMRI_df[i,10]<- wiNetwork_visual
  fc_fMRI_df[i,11]<- wiNetwork_smhand
  fc_fMRI_df[i,12]<- wiNetwork_smmouth
  fc_fMRI_df[i,13]<- wiNetwork_auditory
  fc_fMRI_df[i,14] <- btNetwork_none
  fc_fMRI_df[i,15]<- btNetwork_default
  fc_fMRI_df[i,16]<- btNetwork_ParietoOccip
  fc_fMRI_df[i,17]<- btNetwork_FrontoParietal
  fc_fMRI_df[i,18]<- btNetwork_Salience
  fc_fMRI_df[i,19]<- btNetwork_CinguloOperc
  fc_fMRI_df[i,20]<- btNetwork_MedialParietal
  fc_fMRI_df[i,21]<- btNetwork_DorsalAttn
  fc_fMRI_df[i,22]<- btNetwork_VentralAttn
  fc_fMRI_df[i,23]<- btNetwork_Visual
  fc_fMRI_df[i,24]<- btNetwork_SMhand
  fc_fMRI_df[i,25]<- btNetwork_SMmouth
  fc_fMRI_df[i,26]<- btNetwork_Auditory
  fc_fMRI_df[i,27] <- subID
  }
}

colnames(fc_fMRI_df) <- c("wiNetwork_none", "wiNetwork_default","wiNetwork_ParietoOccip","wiNetwork_FrontoParietal","wiNetwork_Salience","wiNetwork_CinguloOperc","wiNetwork_MedialParietal","wiNetwork_DorsalAttn","wiNetwork_VentralAttn","wiNetwork_Visual","wiNetwork_SMhand","wiNetwork_SMmouth","wiNetwork_Auditory","btNetwork_none","btNetwork_default","btNetwork_ParietoOccip","btNetwork_FrontoParietal","btNetwork_Salience","btNetwork_CinguloOperc","btNetwork_MedialParietal","btNetwork_DorsalAttn","btNetwork_VentralAttn","btNetwork_Visual","btNetwork_SMhand","btNetwork_SMmouth","btNetwork_Auditory", 'IDC')

write.csv(fc_fMRI_df, "fmri_fc_with_and_bt.csv")

#use these csv files for fmri in main_script.R 
