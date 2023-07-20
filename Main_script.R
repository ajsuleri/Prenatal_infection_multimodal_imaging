#####################################################
####Project prenatal infection/multimodal imaging####
#####################################################

#Project: Long-term effects of prenatal infection on the human brain: a prospective multimodal neuroimaging study (in the Generation R cohort)

#Author: Anna Suleri 

#######Preparation#######----
rm(list=ls()) #clear environment 
set.seed(2022) #set seed for reproducibility script 

###Libraries
libraries <- c("foreign", "haven", "dplyr", "mice", "xlsx", "corrplot", "ggplot2", "ggpubr", "ggseg", "ggplot2", "RColorBrewer")
invisible(lapply(libraries, require, character.only = T))

###Set working directory
setwd("PUT_PATH_WHERE_YOU_KEEP_THE_DATA")
wd <- getwd()

###Opening dataframes----
#selecting right brain outcomes per modality and summing up hemispheres because we do not have any lateralized hypotheses 
df1 <- readRDS("f13_freesurfer_v6_14oct2020_aparc_stats_pull23Nov2020.rds")
df1$IDC <- df1$idc #to align it with covariates dataframes 
ddf1 <- dplyr::select(df1, -c('lh_bankssts_thickavg_f13', 'lh_bankssts_surfarea_f13', 'lh_caudalanteriorcingulate_thickavg_f13', 'lh_caudalanteriorcingulate_surfarea_f13', 'lh_caudalmiddlefrontal_thickavg_f13', 'lh_caudalmiddlefrontal_surfarea_f13', 'lh_cuneus_thickavg_f13', 'lh_cuneus_surfarea_f13', 'lh_entorhinal_thickavg_f13', 'lh_entorhinal_surfarea_f13', 'lh_fusiform_thickavg_f13', 'lh_fusiform_surfarea_f13', 'lh_inferiorparietal_thickavg_f13', 'lh_inferiorparietal_surfarea_f13', 'lh_inferiortemporal_thickavg_f13', 'lh_inferiortemporal_surfarea_f13', 'lh_isthmuscingulate_thickavg_f13', 'lh_isthmuscingulate_surfarea_f13', 'lh_lateraloccipital_thickavg_f13', 'lh_lateraloccipital_surfarea_f13', 'lh_lateralorbitofrontal_thickavg_f13', 'lh_lateralorbitofrontal_surfarea_f13', 'lh_lingual_thickavg_f13', 'lh_lingual_surfarea_f13', 'lh_medialorbitofrontal_thickavg_f13', 'lh_medialorbitofrontal_surfarea_f13', 'lh_middletemporal_thickavg_f13', 'lh_middletemporal_surfarea_f13', 'lh_parahippocampal_thickavg_f13', 'lh_parahippocampal_surfarea_f13', 'lh_paracentral_thickavg_f13', 'lh_paracentral_surfarea_f13', 'lh_parsopercularis_thickavg_f13', 'lh_parsopercularis_surfarea_f13', 'lh_parsorbitalis_thickavg_f13', 'lh_parsorbitalis_surfarea_f13', 'lh_parstriangularis_thickavg_f13', 'lh_parstriangularis_surfarea_f13', 'lh_parsorbitalis_thickavg_f13', 'lh_parsorbitalis_surfarea_f13', 'lh_parstriangularis_thickavg_f13', 'lh_parstriangularis_surfarea_f13', 'lh_pericalcarine_thickavg_f13', 'lh_pericalcarine_surfarea_f13', 'lh_postcentral_thickavg_f13', 'lh_postcentral_surfarea_f13', 'lh_posteriorcingulate_thickavg_f13', 'lh_posteriorcingulate_surfarea_f13', 'lh_precentral_thickavg_f13', 'lh_precentral_surfarea_f13', 'lh_precuneus_thickavg_f13', 'lh_precuneus_surfarea_f13', 'lh_rostralanteriorcingulate_thickavg_f13', 'lh_rostralanteriorcingulate_surfarea_f13', 'lh_rostralmiddlefrontal_thickavg_f13', 'lh_rostralmiddlefrontal_surfarea_f13', 'lh_superiorfrontal_thickavg_f13', 'lh_superiorfrontal_surfarea_f13', 'lh_supramarginal_thickavg_f13', 'lh_supramarginal_surfarea_f13', 'lh_frontalpole_thickavg_f13', 'lh_frontalpole_surfarea_f13', 'lh_temporalpole_thickavg_f13', 'lh_temporalpole_surfarea_f13', 'lh_transversetemporal_thickavg_f13', 'lh_transversetemporal_surfarea_f13', 'lh_insula_thickavg_f13', 'lh_insula_surfarea_f13', 'lh_MeanThickness_f13', 'lh_superiorparietal_thickavg_f13', 'lh_superiorparietal_surfarea_f13', 'lh_superiortemporal_thickavg_f13', 'lh_superiortemporal_surfarea_f13'))
ddf1 <- dplyr::select(ddf1, -c('rh_bankssts_thickavg_f13', 'rh_bankssts_surfarea_f13', 'rh_caudalanteriorcingulate_thickavg_f13', 'rh_caudalanteriorcingulate_surfarea_f13', 'rh_caudalmiddlefrontal_thickavg_f13', 'rh_caudalmiddlefrontal_surfarea_f13', 'rh_cuneus_thickavg_f13', 'rh_cuneus_surfarea_f13', 'rh_entorhinal_thickavg_f13', 'rh_entorhinal_surfarea_f13', 'rh_fusiform_thickavg_f13', 'rh_fusiform_surfarea_f13', 'rh_inferiorparietal_thickavg_f13', 'rh_inferiorparietal_surfarea_f13', 'rh_inferiortemporal_thickavg_f13', 'rh_inferiortemporal_surfarea_f13', 'rh_isthmuscingulate_thickavg_f13', 'rh_isthmuscingulate_surfarea_f13', 'rh_lateraloccipital_thickavg_f13', 'rh_lateraloccipital_surfarea_f13', 'rh_lateralorbitofrontal_thickavg_f13', 'rh_lateralorbitofrontal_surfarea_f13', 'rh_lingual_thickavg_f13', 'rh_lingual_surfarea_f13', 'rh_medialorbitofrontal_thickavg_f13', 'rh_medialorbitofrontal_surfarea_f13', 'rh_middletemporal_thickavg_f13', 'rh_middletemporal_surfarea_f13', 'rh_parahippocampal_thickavg_f13', 'rh_parahippocampal_surfarea_f13', 'rh_paracentral_thickavg_f13', 'rh_paracentral_surfarea_f13', 'rh_parsopercularis_thickavg_f13', 'rh_parsopercularis_surfarea_f13', 'rh_parsorbitalis_thickavg_f13', 'rh_parsorbitalis_surfarea_f13', 'rh_parstriangularis_thickavg_f13', 'rh_parstriangularis_surfarea_f13', 'rh_parsorbitalis_thickavg_f13', 'rh_parsorbitalis_surfarea_f13', 'rh_parstriangularis_thickavg_f13', 'rh_parstriangularis_surfarea_f13', 'rh_pericalcarine_thickavg_f13', 'rh_pericalcarine_surfarea_f13', 'rh_postcentral_thickavg_f13', 'rh_postcentral_surfarea_f13', 'rh_posteriorcingulate_thickavg_f13', 'rh_posteriorcingulate_surfarea_f13', 'rh_precentral_thickavg_f13', 'rh_precentral_surfarea_f13', 'rh_precuneus_thickavg_f13', 'rh_precuneus_surfarea_f13', 'rh_rostralanteriorcingulate_thickavg_f13', 'rh_rostralanteriorcingulate_surfarea_f13', 'rh_rostralmiddlefrontal_thickavg_f13', 'rh_rostralmiddlefrontal_surfarea_f13', 'rh_superiorfrontal_thickavg_f13', 'rh_superiorfrontal_surfarea_f13', 'rh_supramarginal_thickavg_f13', 'rh_supramarginal_surfarea_f13', 'rh_frontalpole_thickavg_f13', 'rh_frontalpole_surfarea_f13', 'rh_temporalpole_thickavg_f13', 'rh_temporalpole_surfarea_f13', 'rh_transversetemporal_thickavg_f13', 'rh_transversetemporal_surfarea_f13', 'rh_insula_thickavg_f13', 'rh_insula_surfarea_f13', 'rh_MeanThickness_f13', 'rh_superiorparietal_thickavg_f13', 'rh_superiorparietal_surfarea_f13', 'rh_superiortemporal_thickavg_f13', 'rh_superiortemporal_surfarea_f13'))
ddf1$tot_bankssts <- ddf1$lh_bankssts_vol_f13 + ddf1$rh_bankssts_vol_f13
ddf1$tot_caudalanteriorcingulate <- ddf1$lh_caudalanteriorcingulate_vol_f13+ ddf1$rh_caudalanteriorcingulate_vol_f13
ddf1$tot_caudalmiddlefrontal<- ddf1$lh_caudalmiddlefrontal_vol_f13+ ddf1$rh_caudalmiddlefrontal_vol_f13
ddf1$tot_cuneus<- ddf1$lh_cuneus_vol_f13+ ddf1$rh_cuneus_vol_f13
ddf1$tot_entorhinal<- ddf1$lh_entorhinal_vol_f13+ ddf1$rh_entorhinal_vol_f13
ddf1$tot_fusiform<- ddf1$lh_fusiform_vol_f13+ ddf1$rh_fusiform_vol_f13
ddf1$tot_inferiorparietal<- ddf1$lh_inferiorparietal_vol_f13+ ddf1$rh_inferiorparietal_vol_f13
ddf1$tot_inferiortemporal<- ddf1$lh_inferiortemporal_vol_f13+ ddf1$rh_inferiortemporal_vol_f13
ddf1$tot_isthmuscingulate<- ddf1$lh_isthmuscingulate_vol_f13+ ddf1$rh_isthmuscingulate_vol_f13
ddf1$tot_lateraloccipital<- ddf1$lh_lateraloccipital_vol_f13+ ddf1$rh_lateraloccipital_vol_f13
ddf1$tot_lateralorbitofrontal<- ddf1$lh_lateralorbitofrontal_vol_f13+ ddf1$rh_lateralorbitofrontal_vol_f13 
ddf1$tot_lingual<- ddf1$lh_lingual_vol_f13+ ddf1$rh_lingual_vol_f13
ddf1$tot_medialorbitofrontal<- ddf1$lh_medialorbitofrontal_vol_f13+ ddf1$rh_medialorbitofrontal_vol_f13 
ddf1$tot_middletemporal<- ddf1$lh_middletemporal_vol_f13+ ddf1$rh_middletemporal_vol_f13
ddf1$tot_parahippocampal<- ddf1$lh_parahippocampal_vol_f13+ ddf1$rh_parahippocampal_vol_f13
ddf1$tot_paracentral<- ddf1$lh_paracentral_vol_f13+ ddf1$rh_paracentral_vol_f13
ddf1$tot_parsopercularis<- ddf1$lh_parsopercularis_vol_f13+ ddf1$rh_parsopercularis_vol_f13
ddf1$tot_parsorbitalis<- ddf1$lh_parsorbitalis_vol_f13+ ddf1$rh_parsorbitalis_vol_f13
ddf1$tot_parstriangularis<- ddf1$lh_parstriangularis_vol_f13+ ddf1$rh_parstriangularis_vol_f13
ddf1$tot_pericalcerine<- ddf1$lh_pericalcarine_vol_f13+ ddf1$rh_pericalcarine_vol_f13
ddf1$tot_postcentral<- ddf1$lh_postcentral_vol_f13+ ddf1$rh_postcentral_vol_f13
ddf1$tot_posteriorcingulate<- ddf1$lh_posteriorcingulate_vol_f13+ ddf1$rh_posteriorcingulate_vol_f13
ddf1$tot_precentral<- ddf1$lh_precentral_vol_f13+ ddf1$rh_precentral_vol_f13
ddf1$tot_precuneus<- ddf1$lh_precuneus_vol_f13+ ddf1$rh_precuneus_vol_f13
ddf1$tot_rostralanteriorcingulate<- ddf1$lh_rostralanteriorcingulate_vol_f13+ ddf1$rh_rostralanteriorcingulate_vol_f13 
ddf1$tot_rostralmiddlefrontal<- ddf1$lh_rostralmiddlefrontal_vol_f13+ ddf1$rh_rostralmiddlefrontal_vol_f13
ddf1$tot_superiorfrontal<- ddf1$lh_superiorfrontal_vol_f13+ ddf1$rh_superiorfrontal_vol_f13
ddf1$tot_superiorparietal<- ddf1$lh_superiorparietal_vol_f13+ ddf1$rh_superiorparietal_vol_f13
ddf1$tot_superiortemporal<- ddf1$lh_superiortemporal_vol_f13+ ddf1$rh_superiortemporal_vol_f13
ddf1$tot_supramarginal<- ddf1$lh_supramarginal_vol_f13+ ddf1$rh_supramarginal_vol_f13
ddf1$tot_frontalpole<- ddf1$lh_frontalpole_vol_f13+ ddf1$rh_frontalpole_vol_f13
ddf1$tot_temporalpole<- ddf1$lh_temporalpole_vol_f13+ ddf1$rh_temporalpole_vol_f13
ddf1$tot_transversetemporal<- ddf1$lh_transversetemporal_vol_f13+ ddf1$rh_transversetemporal_vol_f13
ddf1$tot_insula<- ddf1$lh_insula_vol_f13+ ddf1$rh_insula_vol_f13
ddf1_total <- dplyr::select(ddf1, 70:104)
df2 <- readRDS("f13_freesurfer_v6_14oct2020_aseg_stats_pull23Nov2020_v1.rds")
df2$IDC <- df2$idc
ddf2 <- dplyr::select(df2, -c('Left_Inf_Lat_Vent_vol_f13', 'Third_Ventricle_vol_f13', 'Fourth_Ventricle_vol_f13', 'Left_VentralDC_vol_f13', 'Left_vessel_vol_f13', 'Left_choroid_plexus_vol_f13', 'Right_Inf_Lat_Vent_vol_f13', 'Right_VentralDC_vol_f13', 'Right_vessel_vol_f13', 'Right_choroid_plexus_vol_f13', 'Fifth_Ventricle_vol_f13', 'WM_hypointensities_vol_f13', 'Left_WM_hypointensities_vol_f13', 'Right_WM_hypointensities_vol_f13', 'non_WM_hypointensities_vol_f13', 'Left_non_WM_hypointensities_vol_f13', 'Right_non_WM_hypointensities_vol_f13'))
ddf2$lateral_ventricles <- ddf2$Left_Lateral_Ventricle_vol_f13 + ddf2$Right_Lateral_Ventricle_vol_f13
ddf2$tot_cerebellar_whitematter <- ddf2$Left_Cerebellum_White_Matter_vol_f13 + ddf2$Right_Cerebellum_White_Matter_vol_f13
ddf2$tot_cerebellar_cortex<- ddf2$Left_Cerebellum_Cortex_vol_f13 + ddf2$Right_Cerebellum_Cortex_vol_f13
ddf2$tot_cerebellum <- ddf2$tot_cerebellar_whitematter + ddf2$tot_cerebellar_cortex
ddf2$tot_thalamus<- ddf2$Left_Thalamus_Proper_vol_f13 + ddf2$Right_Thalamus_Proper_vol_f13
ddf2$tot_caudate<- ddf2$Left_Caudate_vol_f13 + ddf2$Right_Caudate_vol_f13
ddf2$tot_putamen<- ddf2$Left_Putamen_vol_f13 + ddf2$Right_Putamen_vol_f13
ddf2$tot_pallidum<- ddf2$Left_Putamen_vol_f13 + ddf2$Right_Putamen_vol_f13
ddf2$tot_hippocampus<- ddf2$Left_Hippocampus_vol_f13 + ddf2$Right_Hippocampus_vol_f13
ddf2$tot_amygdala<- ddf2$Left_Amygdala_vol_f13 + ddf2$Right_Amygdala_vol_f13
ddf2$tot_accumbens<- ddf2$Left_Accumbens_area_vol_f13 + ddf2$Right_Accumbens_area_vol_f13
ddf2_total <- dplyr::select(ddf2, 9, 25:41)
df4 <- readRDS("f13_GenR_MRI_eddy_dipy_wls_14Feb2022_autoPtx_dti_stats_inc_glob_meas.rds")
df4$IDC <- df4$idc
ddf4_FA <- dplyr::select(df4, c('unc_l_dti_dipy_wls_wavg_FA_f13', 'atr_l_dti_dipy_wls_wavg_FA_f13', 'ml_r_dti_dipy_wls_wavg_FA_f13', 'ifo_l_dti_dipy_wls_wavg_FA_f13', 'ifo_r_dti_dipy_wls_wavg_FA_f13', 'atr_r_dti_dipy_wls_wavg_FA_f13', 'unc_r_dti_dipy_wls_wavg_FA_f13', 'str_r_dti_dipy_wls_wavg_FA_f13', 'cgc_l_dti_dipy_wls_wavg_FA_f13', 'ptr_l_dti_dipy_wls_wavg_FA_f13', 'str_l_dti_dipy_wls_wavg_FA_f13', 'slf_r_dti_dipy_wls_wavg_FA_f13', 'ar_r_dti_dipy_wls_wavg_FA_f13', 'slf_l_dti_dipy_wls_wavg_FA_f13', 'fmi_dti_dipy_wls_wavg_FA_f13', 'ptr_r_dti_dipy_wls_wavg_FA_f13', 'cgc_r_dti_dipy_wls_wavg_FA_f13', 'fma_dti_dipy_wls_wavg_FA_f13', 'ilf_l_dti_dipy_wls_wavg_FA_f13', 'ilf_r_dti_dipy_wls_wavg_FA_f13', 'mcp_dti_dipy_wls_wavg_FA_f13', 'cgh_l_dti_dipy_wls_wavg_FA_f13', 'cst_r_dti_dipy_wls_wavg_FA_f13', 'ar_l_dti_dipy_wls_wavg_FA_f13', 'ml_l_dti_dipy_wls_wavg_FA_f13', 'cst_l_dti_dipy_wls_wavg_FA_f13', 'cgh_r_dti_dipy_wls_wavg_FA_f13', 'IDC'))
ddf4_FA$dti_unc_FA <- (ddf4_FA$unc_l_dti_dipy_wls_wavg_FA_f13 + ddf4_FA$unc_r_dti_dipy_wls_wavg_FA_f13) / 2
ddf4_FA$dti_cgc_FA<- (ddf4_FA$cgc_l_dti_dipy_wls_wavg_FA_f13 + ddf4_FA$cgc_r_dti_dipy_wls_wavg_FA_f13) / 2
ddf4_FA$dti_slf_FA<- (ddf4_FA$slf_l_dti_dipy_wls_wavg_FA_f13 + ddf4_FA$slf_r_dti_dipy_wls_wavg_FA_f13) / 2
ddf4_FA$dti_fmi_FA<- (ddf4_FA$fmi_dti_dipy_wls_wavg_FA_f13 + ddf4_FA$fmi_dti_dipy_wls_wavg_FA_f13) / 2
ddf4_FA$dti_fma_FA<- (ddf4_FA$fma_dti_dipy_wls_wavg_FA_f13 + ddf4_FA$fma_dti_dipy_wls_wavg_FA_f13) / 2
ddf4_FA$dti_ilf_FA<- (ddf4_FA$ilf_l_dti_dipy_wls_wavg_FA_f13 + ddf4_FA$ilf_r_dti_dipy_wls_wavg_FA_f13) / 2
ddf4_FA$dti_cst_FA<- (ddf4_FA$cst_l_dti_dipy_wls_wavg_FA_f13 + ddf4_FA$cst_r_dti_dipy_wls_wavg_FA_f13) / 2
ddf4_FA_final <- dplyr::select(ddf4_FA, 28:35)
ddf4_MD <- dplyr::select(df4, c('unc_l_dti_dipy_wls_wavg_MD_f13', 'atr_l_dti_dipy_wls_wavg_MD_f13', 'ml_r_dti_dipy_wls_wavg_MD_f13', 'ifo_l_dti_dipy_wls_wavg_MD_f13', 'ifo_r_dti_dipy_wls_wavg_MD_f13', 'atr_r_dti_dipy_wls_wavg_MD_f13', 'unc_r_dti_dipy_wls_wavg_MD_f13', 'str_r_dti_dipy_wls_wavg_MD_f13', 'cgc_l_dti_dipy_wls_wavg_MD_f13', 'ptr_l_dti_dipy_wls_wavg_MD_f13', 'str_l_dti_dipy_wls_wavg_MD_f13', 'slf_r_dti_dipy_wls_wavg_MD_f13', 'ar_r_dti_dipy_wls_wavg_MD_f13', 'slf_l_dti_dipy_wls_wavg_MD_f13', 'fmi_dti_dipy_wls_wavg_MD_f13', 'ptr_r_dti_dipy_wls_wavg_MD_f13', 'cgc_r_dti_dipy_wls_wavg_MD_f13', 'fma_dti_dipy_wls_wavg_MD_f13', 'ilf_l_dti_dipy_wls_wavg_MD_f13', 'ilf_r_dti_dipy_wls_wavg_MD_f13', 'mcp_dti_dipy_wls_wavg_MD_f13', 'cgh_l_dti_dipy_wls_wavg_MD_f13', 'cst_r_dti_dipy_wls_wavg_MD_f13', 'ar_l_dti_dipy_wls_wavg_MD_f13', 'ml_l_dti_dipy_wls_wavg_MD_f13', 'cst_l_dti_dipy_wls_wavg_MD_f13', 'cgh_r_dti_dipy_wls_wavg_MD_f13', "IDC", 'mean_FA_genr_f13', 'mean_MD_genr_f13', 'missingness_genr_f13'))
ddf4_MD$dti_unc_MD <- (ddf4_MD$unc_l_dti_dipy_wls_wavg_MD_f13 + ddf4_MD$unc_r_dti_dipy_wls_wavg_MD_f13) / 2
ddf4_MD$dti_cgc_MD<- (ddf4_MD$cgc_l_dti_dipy_wls_wavg_MD_f13 + ddf4_MD$cgc_r_dti_dipy_wls_wavg_MD_f13) / 2
ddf4_MD$dti_slf_MD<- (ddf4_MD$slf_l_dti_dipy_wls_wavg_MD_f13 + ddf4_MD$slf_r_dti_dipy_wls_wavg_MD_f13) / 2
ddf4_MD$dti_fmi_MD<- (ddf4_MD$fmi_dti_dipy_wls_wavg_MD_f13 + ddf4_MD$fmi_dti_dipy_wls_wavg_MD_f13) / 2
ddf4_MD$dti_fma_MD<- (ddf4_MD$fma_dti_dipy_wls_wavg_MD_f13 + ddf4_MD$fma_dti_dipy_wls_wavg_MD_f13) / 2
ddf4_MD$dti_ilf_MD<- (ddf4_MD$ilf_l_dti_dipy_wls_wavg_MD_f13 + ddf4_MD$ilf_r_dti_dipy_wls_wavg_MD_f13) / 2
ddf4_MD$dti_cst_MD<- (ddf4_MD$cst_l_dti_dipy_wls_wavg_MD_f13 + ddf4_MD$cst_r_dti_dipy_wls_wavg_MD_f13) / 2
ddf4_MD_final <- dplyr::select(ddf4_MD, 28:38)
df5 <- readRDS("f13_freesurfer_v6_14oct2020_tbv_stats_pull23Nov2020_v2.rds")
df5$IDC <- df5$idc
ddf5 <- dplyr::select(df5, -c('idc', 'lhCortexVol_f13', 'rhCortexVol_f13', 'lhCerebralWhiteMatterVol_f13', 'rhCerebralWhiteMatterVol_f13', "SupraTentorialVol_f13", "SupraTentorialVolNotVent_f13", "SupraTentorialVolNotVentVox_f13", "lh_NumVert_f13", "lh_WhiteSurfArea_f13", "lh_MeanThickness_f13","rh_NumVert_f13","rh_WhiteSurfArea_f13", "rh_MeanThickness_f13", 'SubCortGrayVol_f13', 'TotalGrayVol_f13'))
df6 <- read.spss('Covariates_MRI_analyses.sav', to.data.frame = T)
ddf6 <- dplyr::select(df6, IDC, IDM, MOTHER, GENDER, ETHNMv2, SMOKE_ALL, mdrink_updated, EDUCM_3groups, INCOME)
ddf6$INCOME <- as.factor(ifelse(ddf6$INCOME == 'less than 450' | ddf6$INCOME == '450-600 euro' | ddf6$INCOME == '600-700 euro' | ddf6$INCOME == '700-800 euro' | ddf6$INCOME == '800-900 euro' | ddf6$INCOME == '900-1200 euro' | ddf6$INCOME == '1200-1400 euro' | ddf6$INCOME == '1400-1600 euro' | ddf6$INCOME == '1600-1800 euro' | ddf6$INCOME == '1800-2000 euro' | ddf6$INCOME == '2000-2200 euro', '<2000', ifelse(is.na(ddf6$INCOME), NA, '>2000'))) #recode to above and below 2000 EUR 
ddf6$ETHNMv2 <- as.factor(ifelse(ddf6$ETHNMv2 == 'Dutch', 'Dutch', ifelse(ddf6$ETHNMv2 == 'Indonesian' | ddf6$ETHNMv2 == 'Cape Verdian' | ddf6$ETHNMv2 == 'Moroccan' | ddf6$ETHNMv2 == 'Dutch Antilles' | ddf6$ETHNMv2 == 'Surinamese' | ddf6$ETHNMv2 == 'Turkish' | ddf6$ETHNMv2 == 'African' | ddf6$ETHNMv2 == 'American, western' | ddf6$ETHNMv2 == 'American, non western' | ddf6$ETHNMv2 == 'Asian, western' | ddf6$ETHNMv2 == 'Asian, non western' | ddf6$ETHNMv2 == 'European' | ddf6$ETHNMv2 == 'Oceanie', 'Not Dutch', NA))) #recode to 'dutch' and 'not dutch' 
df7 <- read.spss('Covariaten_Anna.sav', to.data.frame = T)
ddf7 <- dplyr::select(df7, IDC, GSI, AGE_M_v2)
df8 <- read.spss('IDC-IDM-MOTHER.sav', to.data.frame = T)
df9 <- read.spss('GR1093-E1_CBCL_18062020.sav', to.data.frame = T)
ddf9 <- dplyr::select(df9, IDC, AGECHILD_GR1093, sum_int_14, sum_ext_14, cbcl_sum_14)
df10 <- read.spss("GR1001-F11-12_22112016.sav", to.data.frame = T)
ddf10 <- dplyr::select(df10, IDM, f1200101)
df11 <- read.spss('GESTAGEINTAKE_08102012.sav', to.data.frame = T)
df12 <- readRDS('genr_mri_core_data_20220311.rds')
df12$IDC <- df12$idc
ddf12 <- dplyr::select(df12, -c('idc'))

df13 <- read.csv("fmri_fc_with_and_bt.csv", header = T) #for data preparation fMRI see fMRI_prep_script 
df13 <- dplyr::select(df13, -c('X'))
df14 <- read.csv("fmri_graphtheory_static.csv", header = T) #for data preparation fMRI see fMRI_prep_script 
df14 <- dplyr::select(df14, -c('X'))

#merging dataframes of covariates and outcome 
merger1 <- merge(ddf1_total, ddf2_total, by = c('IDC'), all = T)
merger3 <- merge(merger1, ddf4_FA_final, by = c('IDC'), all = T)
merger4 <- merge(merger3, ddf4_MD_final, by = c('IDC'), all = T)
merger5 <- merge(merger4, ddf5, by = c('IDC'), all = T)
merger6 <- merge(merger5, ddf6, by = c('IDC'), all = T)
merger7 <- merge(merger6, ddf7, by = c('IDC'), all = T)
merger8 <- merge(merger7, df8, by = c('IDC', 'IDM', 'MOTHER'), all = T)
merger9 <- merge(merger8, ddf9, by = c('IDC'), all = T)
merger10 <- merge(merger9, ddf10, by = c('IDM'), all = T)
merger11 <- merge(merger10, df11, by = c('IDM'), all = T)
merger12 <- merge(ddf12, merger11, by = c('IDC'), all = T)
merger13 <- merge(merger12, df13, by = c('IDC'), all = T)
merger14 <- merge(merger13, df14, by = c('IDC'), all = T)

###Making prenatal infection sum score---- 
df18 <- read.spss('GR1001-C2-13_22112016.sav', to.data.frame = T)
ddf18 <- dplyr::select(df18, IDM, c1100101,c1100301,c1100501,c1100701,c1100901,c1101101,c1101301,c1102301,c1102901, c1103101, c1103301, c1103501, c1103701) #trimester 1
df22 <- read.spss('GR1003-A8-11_02092013.sav', to.data.frame = T)
ddf22 <- dplyr::select(df22, IDM, a0900103, a0900303, a0900503, a0900703, a0900903, a0901103, a0901303, a0902303, a0902903, a0903103, a0903303, a0903503, a0903703) #trimester 2
df25 <- read.spss('GR1005-B8-11_22112016.sav', to.data.frame = T)
ddf25 <- dplyr::select(df25, IDM, b0900105, b0900305, b0900505, b0900705, b0900905, b0901105, b0901305, b0902305, b0902905, b0903105, b0903305, b0903505, b0903705) #trimester 3
df26 <- read.spss('20220412_Anna-GR1001_C11_Otherinfections.sav', to.data.frame = T)
df27 <- read.spss('PrenatalQuest_Otherinfections_25042022.sav', to.data.frame = T)

ddf18$fever_tri1 <- ifelse(ddf18$c1100101 == 'Yes', 1, ifelse(ddf18$c1100101 == 'No', 0, NA))
ddf18$flu_tri1 <- ifelse(ddf18$c1100301 == 'Yes', 1, ifelse(ddf18$c1100301 == 'No', 0, NA))
ddf18$pharyngitis_tri1<- ifelse(ddf18$c1100501 == 'Yes', 1, ifelse(ddf18$c1100501 == 'No', 0, NA))
ddf18$rhinitis_tri1<- ifelse(ddf18$c1100701 == 'Yes', 1, ifelse(ddf18$c1100701 == 'No', 0, NA))
ddf18$sinusitis_tri1<- ifelse(ddf18$c1100901 == 'Yes', 1, ifelse(ddf18$c1100901 == 'No', 0, NA))
ddf18$earinf_tri1<- ifelse(ddf18$c1101101 == 'Yes', 1, ifelse(ddf18$c1101101 == 'No', 0, NA))
ddf18$pneumonia_tri1<- ifelse(ddf18$c1101301 == 'Yes', 1, ifelse(ddf18$c1101301 == 'No', 0, NA))
ddf18$dermatitis_tri1<- ifelse(ddf18$c1102301 == 'Yes', 1, ifelse(ddf18$c1102301 == 'No', 0, NA))
ddf18$herpeszoster_tri1<- ifelse(ddf18$c1102901 == 'Yes', 1, ifelse(ddf18$c1102901 == 'No', 0, NA))
ddf18$enteritis_tri1<- ifelse(ddf18$c1103101 == 'Yes', 1, ifelse(ddf18$c1103101 == 'No', 0, NA))
ddf18$cystitis_tri1<- ifelse(ddf18$c1103301 == 'Yes', 1, ifelse(ddf18$c1103301 == 'No', 0, NA))
ddf18$jaundice_tri1<- ifelse(ddf18$c1103501 == 'Yes', 1, ifelse(ddf18$c1103501 == 'No', 0, NA))
ddf18$otherinf_tri1 <- ifelse(ddf18$c1103701 == 'Yes', 1, ifelse(ddf18$c1103701 == 'No', 0, NA))
df26$STD_tri1 <- ifelse(df26$Chlamydia_or_other_STD == 'Yes', 1, ifelse(df26$Chlamydia_or_other_STD == 'No', 0, NA))
df26$lower_resp_inf_tri1 <- ifelse(df26$Lower_respiratory_infection == 'Yes', 1, ifelse(df26$Lower_respiratory_infection == 'No', 0, NA))
df26$upper_resp_inf_tri1 <- ifelse(df26$Upper_respiratory_infection == 'Yes', 1, ifelse(df26$Upper_respiratory_infection == 'No', 0, NA))
df26$GI_inf_tri1 <- ifelse(df26$Gastro_intestinal_infection == 'Yes', 1, ifelse(df26$Gastro_intestinal_infection == 'No', 0, NA))
df26$UWI_tri1 <- ifelse(df26$UWI == 'Yes', 1, ifelse(df26$UWI == 'No', 0, NA))

ddf22$fever_tri2 <- ifelse(ddf22$a0900103 == 'Yes', 1, ifelse(ddf22$a0900103 == 'No', 0, NA))
ddf22$flu_tri2 <- ifelse(ddf22$a0900303 == 'Yes', 1, ifelse(ddf22$a0900303 == 'No', 0, NA))
ddf22$pharyngitis_tri2<- ifelse(ddf22$a0900503 == 'Yes', 1, ifelse(ddf22$a0900503 == 'No', 0, NA))
ddf22$rhinitis_tri2 <- ifelse(ddf22$a0900703 == 'Yes', 1, ifelse(ddf22$a0900703 == 'No', 0, NA))
ddf22$sinusitis_tri2<- ifelse(ddf22$a0900903 == 'Yes', 1, ifelse(ddf22$a0900903 == 'No', 0, NA))
ddf22$earinf_tri2<- ifelse(ddf22$a0901103 == 'Yes', 1, ifelse(ddf22$a0901103 == 'No', 0, NA))
ddf22$pneumonia_tri2<- ifelse(ddf22$a0901303 == 'Yes', 1, ifelse(ddf22$a0901303 == 'No', 0, NA))
ddf22$dermatitis_tri2<- ifelse(ddf22$a0902303 == 'Yes', 1, ifelse(ddf22$a0902303 == 'No', 0, NA))
ddf22$herpeszoster_tri2<- ifelse(ddf22$a0902903 == 'Yes', 1, ifelse(ddf22$a0902903 == 'No', 0, NA))
ddf22$enteritis_tri2<- ifelse(ddf22$a0903103 == 'Yes', 1, ifelse(ddf22$a0903103 == 'No', 0, NA))
ddf22$cystitis_tri2<- ifelse(ddf22$a0903303 == 'Yes', 1, ifelse(ddf22$a0903303 == 'No', 0, NA))
ddf22$jaundice_tri2<- ifelse(ddf22$a0903503 == 'Yes', 1, ifelse(ddf22$a0903503 == 'No', 0, NA))
ddf22$otherinf_tri2 <- ifelse(ddf22$a0903703 == 'Yes', 1, ifelse(ddf22$a0903703 == 'No', 0, NA))
df27$STD_tri2 <- ifelse(df27$GR1003_Chlamydia_or_other_STD == 'Yes', 1, ifelse(df27$GR1003_Chlamydia_or_other_STD == 'No', 0, NA))
df27$lower_resp_inf_tri2 <- ifelse(df27$GR1003_Lower_respiratory_infection == 'Yes', 1, ifelse(df27$GR1003_Lower_respiratory_infection == 'No', 0, NA))
df27$upper_resp_inf_tri2 <- ifelse(df27$GR1003_Upper_respiratory_infection == 'Yes', 1, ifelse(df27$GR1003_Upper_respiratory_infection == 'No', 0, NA))
df27$GI_inf_tri2 <- ifelse(df27$GR1003_Gastro_intestinal_infection == 'Yes', 1, ifelse(df27$GR1003_Gastro_intestinal_infection == 'No', 0, NA))
df27$UWI_tri2 <- ifelse(df27$GR1003_UWI == 'Yes', 1, ifelse(df27$GR1003_UWI == 'No', 0, NA))

ddf25$fever_tri3 <- ifelse(ddf25$b0900105 == 'Yes', 1, ifelse(ddf25$b0900105 == 'No', 0, NA))
ddf25$flu_tri3 <- ifelse(ddf25$b0900305 == 'Yes', 1, ifelse(ddf25$b0900305 == 'No', 0, NA))
ddf25$pharyngitis_tri3<- ifelse(ddf25$b0900505 == 'Yes', 1, ifelse(ddf25$b0900505 == 'No', 0, NA))
ddf25$rhinitis_tri3<- ifelse(ddf25$b0900705 == 'Yes', 1, ifelse(ddf25$b0900705 == 'No', 0, NA))
ddf25$sinusitis_tri3<- ifelse(ddf25$b0900905 == 'Yes', 1, ifelse(ddf25$b0900905 == 'No', 0, NA))
ddf25$earinf_tri3<- ifelse(ddf25$b0901105 == 'Yes', 1, ifelse(ddf25$b0901105 == 'No', 0, NA))
ddf25$pneumonia_tri3<- ifelse(ddf25$b0901305 == 'Yes', 1, ifelse(ddf25$b0901305 == 'No', 0, NA))
ddf25$dermatitis_tri3<- ifelse(ddf25$b0902305 == 'Yes', 1, ifelse(ddf25$b0902305 == 'No', 0, NA))
ddf25$herpeszoster_tri3<- ifelse(ddf25$b0902905 == 'Yes', 1, ifelse(ddf25$b0902905 == 'No', 0, NA))
ddf25$enteritis_tri3<- ifelse(ddf25$b0903105 == 'Yes', 1, ifelse(ddf25$b0903105 == 'No', 0, NA))
ddf25$cystitis_tri3<- ifelse(ddf25$b0903305 == 'Yes', 1, ifelse(ddf25$b0903305 == 'No', 0, NA))
ddf25$jaundice_tri3<- ifelse(ddf25$b0903505 == 'Yes', 1, ifelse(ddf25$b0903505 == 'No', 0, NA))
ddf25$otherinf_tri3 <- ifelse(ddf25$b0903705 == 'Yes', 1, ifelse(ddf25$b0903705 == 'No', 0, NA))
df27$STD_tri3 <- ifelse(df27$GR1005_Chlamydia_or_other_STD == 'Yes', 1, ifelse(df27$GR1005_Chlamydia_or_other_STD == 'No', 0, NA))
df27$lower_resp_inf_tri3 <- ifelse(df27$GR1005_Lower_respiratory_infection == 'Yes', 1, ifelse(df27$GR1005_Lower_respiratory_infection == 'No', 0, NA))
df27$upper_resp_inf_tri3 <- ifelse(df27$GR1005_Upper_respiratory_infection == 'Yes', 1, ifelse(df27$GR1005_Upper_respiratory_infection == 'No', 0, NA))
df27$GI_inf_tri3 <- ifelse(df27$GR1005_Gastro_intestinal_infection == 'Yes', 1, ifelse(df27$GR1005_Gastro_intestinal_infection == 'No', 0, NA))
df27$UWI_tri3 <- ifelse(df27$GR1005_UWI == 'Yes', 1, ifelse(df27$GR1005_UWI == 'No', 0, NA))

df_mi <- merge(ddf18, ddf25, by = 'IDM', all = TRUE)
ddf_mi <- merge(df_mi, ddf22, by = 'IDM', all = TRUE)
dddf_mi <- merge(ddf_mi, df26, by = 'IDM', all = TRUE)
ddddf_mi <- merge(dddf_mi, df27, by = 'IDM', all = TRUE)

##Making clusters
#Upper respiratory tract infection
ddddf_mi$clean_upper_resp_inf_tri1 <- ifelse(ddddf_mi$upper_resp_inf_tri1 == 1 | ddddf_mi$rhinitis_tri1 == 1 | ddddf_mi$pharyngitis_tri1 == 1 | ddddf_mi$earinf_tri1 == 1 | ddddf_mi$sinusitis_tri1 == 1, 1, 0)
ddddf_mi$clean_upper_resp_inf_tri2 <- ifelse(ddddf_mi$upper_resp_inf_tri2 == 1 | ddddf_mi$rhinitis_tri2 == 1 | ddddf_mi$pharyngitis_tri2 == 1 | ddddf_mi$earinf_tri2 == 1 | ddddf_mi$sinusitis_tri2 == 1, 1, 0)
ddddf_mi$clean_upper_resp_inf_tri3 <- ifelse(ddddf_mi$upper_resp_inf_tri3 == 1 | ddddf_mi$rhinitis_tri3 == 1 | ddddf_mi$pharyngitis_tri3 == 1 | ddddf_mi$earinf_tri3 == 1 | ddddf_mi$sinusitis_tri3 == 1, 1, 0)

#Lower respiratory tract infection
ddddf_mi$clean_lower_resp_inf_tri1 <- ifelse(ddddf_mi$lower_resp_inf_tri1 == 1 | ddddf_mi$pneumonia_tri1 == 1, 1, 0)
ddddf_mi$clean_lower_resp_inf_tri2 <- ifelse(ddddf_mi$lower_resp_inf_tri2 == 1 | ddddf_mi$pneumonia_tri2 == 1, 1, 0)
ddddf_mi$clean_lower_resp_inf_tri3 <- ifelse(ddddf_mi$lower_resp_inf_tri3 == 1 | ddddf_mi$pneumonia_tri3 == 1, 1, 0)

#Cleaning cystitis
ddddf_mi$clean_uwi_tri1 <- ifelse(ddddf_mi$UWI_tri1 == 1 | ddddf_mi$cystitis_tri1 == 1, 1, 0)
ddddf_mi$clean_uwi_tri2 <- ifelse(ddddf_mi$UWI_tri2 == 1 | ddddf_mi$cystitis_tri2 == 1, 1, 0)
ddddf_mi$clean_uwi_tri3 <- ifelse(ddddf_mi$UWI_tri3 == 1 | ddddf_mi$cystitis_tri3 == 1, 1, 0)

#Gastrointestinal tract infection 
ddddf_mi$clean_GI_inf_tri1 <- ifelse(ddddf_mi$GI_inf_tri1 ==1 | dddf_mi$enteritis_tri1 ==1, 1, 0)
ddddf_mi$clean_GI_inf_tri2 <- ifelse(ddddf_mi$GI_inf_tri2 ==1 | dddf_mi$enteritis_tri2 ==1, 1, 0)
ddddf_mi$clean_GI_inf_tri3 <- ifelse(ddddf_mi$GI_inf_tri3 ==1 | dddf_mi$enteritis_tri3 ==1, 1, 0)

##Trimester-based infection sum score 
ddddf_mi$sumscore_inf_tri1 <- ddddf_mi$clean_upper_resp_inf_tri1 + ddddf_mi$clean_lower_resp_inf_tri1 + ddddf_mi$clean_GI_inf_tri1 + ddddf_mi$flu_tri1 + ddddf_mi$clean_uwi_tri1 + ddddf_mi$dermatitis_tri1 + ddddf_mi$jaundice_tri1 + ddddf_mi$herpeszoster_tri1 + ddddf_mi$STD_tri1 + ddddf_mi$fever_tri1
ddddf_mi$sumscore_inf_tri2 <- ddddf_mi$clean_upper_resp_inf_tri2 + ddddf_mi$clean_lower_resp_inf_tri2 + ddddf_mi$clean_GI_inf_tri2 + ddddf_mi$flu_tri2 + ddddf_mi$clean_uwi_tri2 + ddddf_mi$dermatitis_tri2 + ddddf_mi$jaundice_tri2 + ddddf_mi$herpeszoster_tri2 + ddddf_mi$STD_tri2+ ddddf_mi$fever_tri2
ddddf_mi$sumscore_inf_tri3 <- ddddf_mi$clean_upper_resp_inf_tri3 + ddddf_mi$clean_lower_resp_inf_tri3 + ddddf_mi$clean_GI_inf_tri3 + ddddf_mi$flu_tri3 + ddddf_mi$clean_uwi_tri3 + ddddf_mi$dermatitis_tri3 + ddddf_mi$jaundice_tri3 + ddddf_mi$herpeszoster_tri3 + ddddf_mi$STD_tri3+ ddddf_mi$fever_tri3

##Spanning pregnancy infection sum score
ddddf_mi$sumscore_inf_tot <- ddddf_mi$sumscore_inf_tri1 + ddddf_mi$sumscore_inf_tri2 + ddddf_mi$sumscore_inf_tri3 

##Fever severity marker 
ddddf_mi$sumscore_fever_tot <- ddddf_mi$fever_tri1 + ddddf_mi$fever_tri2 + ddddf_mi$fever_tri3 #total pregnancy fever severity marker

###Merging datasets
#ddddf_mi with merger12 (merging merged df of outcome and covariates with merged df of exposure)
full_df <- merge(merger14, ddddf_mi, by = c('IDM'), all = T)

###Inclusion and exclusion criteria

#general exclusion criteria ----
#'mothers should be enrolled in trimester 1
inclusioncri1 <- subset(full_df, INTAKEPERIOD == '<18 weeks') #only selecting mothers that enrolled in their first trimester 
inclusioncri2 <- subset(inclusioncri1, complete.cases(sumscore_inf_tot)) #only selecting mothers with information on maternal infection 
inclusioncri3 <- subset(inclusioncri2, mri_consent_f13 == "yes") #only selecting mothers with complete information on brain and consent 
inclusioncri4 <- subset(inclusioncri3, has_braces_mri_f13 == "no") #braces
inclusioncri5 <- subset(inclusioncri4, exclude_incidental_f13 == "include") #incidental findings 

#'making subgroup DTI
inclusioncri8 <- subset(inclusioncri5, dti_man_qc_f13 == 'usable') #manual qc dti
inclusioncri9 <- subset(inclusioncri8, complete.cases(missingness_genr_f13)) #exclude missing dti tracts
inclusioncri9 <- inclusioncri9[sample(nrow(inclusioncri9)),] #making a random order in the df
inclusioncri10 <- inclusioncri9[!duplicated(inclusioncri9$MOTHER),] #excluding 1 of the twins/siblings

#'making subgroup T1
inclusioncri6 <- subset(inclusioncri5, t1_has_nii_f13 == "yes") #scan type (exclusion criterium T1)
inclusioncri7 <- subset(inclusioncri6,freesurfer_qc_f13 == "usable") #qc sMRI (exclusion criterium T1)
inclusioncri11 <- inclusioncri7[sample(nrow(inclusioncri7)),] #making a random order in the df
#excluding 1 of the sibs/twins 
inclusioncri12 <- inclusioncri11[!duplicated(inclusioncri11$MOTHER),] #excluding 1 of the twins/siblings

#'making subgroup fMRI
inclusioncri14 <- subset(inclusioncri5, rsfmri_has_nii_f13 == 'yes')
inclusioncri15 <- subset(inclusioncri14, num_vols_bold_f13 == 200)
inclusioncri16 <- subset(inclusioncri14, mean_bold_rms_f13 <= 0.25)
inclusionrcri17 <- subset(inclusioncri16, exclude_bold_f13 == 'include' & is.na(inclusioncri16$exclude_reg_prob_bold_f13))
inclusioncri19 <- inclusionrcri17[!duplicated(inclusionrcri17$MOTHER),]

###Removing columns that i don't need to make final df 
df_final_T1 <- dplyr::select(inclusioncri12, -c('INTAKEPERIOD', 'GESTINT', 'c1100101', 'c1100301', 'c1100501', 'c1100701', 'c1100901', 'c1101101', 'c1101301', 'c1102301', 'c1102901', 'c1103101', 'c1103301', 'c1103501', 'c1103701', 'flu_tri1', 'pharyngitis_tri1', 'rhinitis_tri1', 'sinusitis_tri1', 'earinf_tri1', 'pneumonia_tri1', 'dermatitis_tri1', 'herpeszoster_tri1', 'enteritis_tri1', 'cystitis_tri1', 'jaundice_tri1', 'otherinf_tri1', 'b0900105', 'b0900305', 'b0900505', 'b0900705', 'b0900905', 'b0901105', 'b0901305', 'b0902305', 'b0902905', 'b0903105', 'b0903305', 'b0903505', 'rhinitis_tri3', 'sinusitis_tri3', 'earinf_tri3', 'pneumonia_tri3', 'dermatitis_tri3', 'herpeszoster_tri3', 'enteritis_tri3', 'cystitis_tri3', 'jaundice_tri3', 'otherinf_tri3', 'a0900103', 'a0900303', 'a0900503', 'a0900703', 'a0900903', 'a0901103', 'a0901303', 'a0902303', 'a0902903', 'a0903103', 'a0903303', 'a0903503', 'a0903703', 'flu_tri2', 'pharyngitis_tri2', 'rhinitis_tri2', 'sinusitis_tri2', 'earinf_tri2', 'pneumonia_tri2', 'dermatitis_tri2', 'herpeszoster_tri2', 'enteritis_tri2', 'cystitis_tri2', 'jaundice_tri2', 'otherinf_tri2', 'Chlamydia_or_other_STD', 'Lower_respiratory_infection', 'Upper_respiratory_infection', 'Gastro_intestinal_infection', 'UWI', 'STD_tri1', 'lower_resp_inf_tri1', 'upper_resp_inf_tri1', 'GI_inf_tri1', 'UWI_tri1', 'GR1001_Chlamydia_or_other_STD', 'GR1001_Lower_respiratory_infection', 'GR1001_Upper_respiratory_infection', 'GR1001_Gastro_intestinal_infection', 'GR1001_UWI', 'GR1001_Other', 'GR1003_Chlamydia_or_other_STD', 'GR1003_Lower_respiratory_infection', 'GR1003_Upper_respiratory_infection', 'GR1003_Gastro_intestinal_infection', 'GR1003_UWI', 'GR1003_Other', 'GR1003_Any_other_infection', 'GR1005_Chlamydia_or_other_STD', 'GR1005_Lower_respiratory_infection', 'GR1005_Upper_respiratory_infection', 'GR1005_Gastro_intestinal_infection', 'GR1005_UWI', 'GR1005_Other', 'GR1005_Any_other_infection', 'STD_tri2', 'lower_resp_inf_tri2', 'upper_resp_inf_tri2', 'GI_inf_tri2', 'UWI_tri2', 'STD_tri3', 'lower_resp_inf_tri3', 'upper_resp_inf_tri3', 'GI_inf_tri3', 'UWI_tri3', 'clean_upper_resp_inf_tri1', 'clean_upper_resp_inf_tri2', 'clean_upper_resp_inf_tri3', 'clean_lower_resp_inf_tri1', 'clean_lower_resp_inf_tri2', 'clean_lower_resp_inf_tri3', 'clean_uwi_tri1', 'clean_uwi_tri2', 'clean_uwi_tri3', 'clean_GI_inf_tri1', 'clean_GI_inf_tri2', 'clean_GI_inf_tri3', 'flu_tri3', 'b0903705', 'pharyngitis_tri3', 'mri_consent_f05', 'mri_consent_f09', 'mri_consent_f13', 'age_child_mri_f05', 'age_child_mri_f09', 't1_has_nii_f05', 't1_has_nii_f09', 't1_has_nii_f13', 't1_asset_has_nii_f09', 't1_braces_has_nii_f13', 'qdec_has_lgi_f05', 'qdec_has_lgi_f09', 'qdec_has_lgi_f13', 'dti_has_nii_f05', 'dti_has_nii_f09', 'dti_has_nii_f13', 'rsfmri_has_nii_f05', 'rsfmri_has_nii_f09', 'rsfmri_has_nii_f13', 'has_braces_mri_f05', 'has_braces_mri_f09', 'has_braces_mri_f13', 'exclude_incidental_f05', 'exclude_incidental_f09', 'exclude_incidental_f13', 'freesurfer_qc_f05', 'freesurfer_qc_f09', 'freesurfer_qc_f13', 'folders_f05', 'folders_f09', 'folders_f13', 'num_vols_bold_f09', 'num_vols_bold_f13', 'mean_bold_rms_f09', 'mean_bold_rms_f13', 'n_outliers_bold_f09', 'n_outliers_bold_f13', 'exclude_bold_f09', 'exclude_bold_f13', 'reg_prob_bold_f09', 'reg_prob_bold_f13', 'exclude_reg_prob_bold_f09', 'exclude_reg_prob_bold_f13', 'software_ver_long_f05', 'software_ver_long_f09', 'software_ver_long_f13', 'software_ver_short_f05', 'software_ver_short_f09', 'software_ver_short_f13', 'dwi_eddy_qc_mot_abs_f05', 'dwi_eddy_qc_mot_rel_f05', 'dwi_eddy_qc_outliers_pe_f05', 'dwi_eddy_qc_mot_abs_f09','dwi_eddy_qc_mot_rel_f09', 'dwi_eddy_qc_outliers_pe_f09','dwi_eddy_qc_mot_abs_f13','dwi_eddy_qc_mot_rel_f13', 'dwi_eddy_qc_outliers_pe_f13', 'dti_man_qc_f05', 'dti_man_qc_f09', 'dti_man_qc_f13', 'dti_auto_qc_f05', 'dti_auto_qc_f09', 'dti_auto_qc_f13', 'dti_overall_qc_f05', 'dti_overall_qc_f09', 'dti_overall_qc_f13', 'dwi_eddy_qc_outliers_tot_f13', 'dwi_eddy_qc_outliers_tot_f09', 'dwi_eddy_qc_outliers_tot_f05'))
df_final_DTI <- dplyr::select(inclusioncri10, -c('INTAKEPERIOD', 'GESTINT', 'c1100101', 'c1100301', 'c1100501', 'c1100701', 'c1100901', 'c1101101', 'c1101301', 'c1102301', 'c1102901', 'c1103101', 'c1103301', 'c1103501', 'c1103701', 'flu_tri1', 'pharyngitis_tri1', 'rhinitis_tri1', 'sinusitis_tri1', 'earinf_tri1', 'pneumonia_tri1', 'dermatitis_tri1', 'herpeszoster_tri1', 'enteritis_tri1', 'cystitis_tri1', 'jaundice_tri1', 'otherinf_tri1', 'b0900105', 'b0900305', 'b0900505', 'b0900705', 'b0900905', 'b0901105', 'b0901305', 'b0902305', 'b0902905', 'b0903105', 'b0903305', 'b0903505', 'rhinitis_tri3', 'sinusitis_tri3', 'earinf_tri3', 'pneumonia_tri3', 'dermatitis_tri3', 'herpeszoster_tri3', 'enteritis_tri3', 'cystitis_tri3', 'jaundice_tri3', 'otherinf_tri3', 'a0900103', 'a0900303', 'a0900503', 'a0900703', 'a0900903', 'a0901103', 'a0901303', 'a0902303', 'a0902903', 'a0903103', 'a0903303', 'a0903503', 'a0903703', 'flu_tri2', 'pharyngitis_tri2', 'rhinitis_tri2', 'sinusitis_tri2', 'earinf_tri2', 'pneumonia_tri2', 'dermatitis_tri2', 'herpeszoster_tri2', 'enteritis_tri2', 'cystitis_tri2', 'jaundice_tri2', 'otherinf_tri2', 'Chlamydia_or_other_STD', 'Lower_respiratory_infection', 'Upper_respiratory_infection', 'Gastro_intestinal_infection', 'UWI', 'STD_tri1', 'lower_resp_inf_tri1', 'upper_resp_inf_tri1', 'GI_inf_tri1', 'UWI_tri1', 'GR1001_Chlamydia_or_other_STD', 'GR1001_Lower_respiratory_infection', 'GR1001_Upper_respiratory_infection', 'GR1001_Gastro_intestinal_infection', 'GR1001_UWI', 'GR1001_Other', 'GR1003_Chlamydia_or_other_STD', 'GR1003_Lower_respiratory_infection', 'GR1003_Upper_respiratory_infection', 'GR1003_Gastro_intestinal_infection', 'GR1003_UWI', 'GR1003_Other', 'GR1003_Any_other_infection', 'GR1005_Chlamydia_or_other_STD', 'GR1005_Lower_respiratory_infection', 'GR1005_Upper_respiratory_infection', 'GR1005_Gastro_intestinal_infection', 'GR1005_UWI', 'GR1005_Other', 'GR1005_Any_other_infection', 'STD_tri2', 'lower_resp_inf_tri2', 'upper_resp_inf_tri2', 'GI_inf_tri2', 'UWI_tri2', 'STD_tri3', 'lower_resp_inf_tri3', 'upper_resp_inf_tri3', 'GI_inf_tri3', 'UWI_tri3', 'clean_upper_resp_inf_tri1', 'clean_upper_resp_inf_tri2', 'clean_upper_resp_inf_tri3', 'clean_lower_resp_inf_tri1', 'clean_lower_resp_inf_tri2', 'clean_lower_resp_inf_tri3', 'clean_uwi_tri1', 'clean_uwi_tri2', 'clean_uwi_tri3', 'clean_GI_inf_tri1', 'clean_GI_inf_tri2', 'clean_GI_inf_tri3', 'flu_tri3', 'b0903705', 'pharyngitis_tri3', 'mri_consent_f05', 'mri_consent_f09', 'mri_consent_f13', 'age_child_mri_f05', 'age_child_mri_f09', 't1_has_nii_f05', 't1_has_nii_f09', 't1_has_nii_f13', 't1_asset_has_nii_f09', 't1_braces_has_nii_f13', 'qdec_has_lgi_f05', 'qdec_has_lgi_f09', 'qdec_has_lgi_f13', 'dti_has_nii_f05', 'dti_has_nii_f09', 'dti_has_nii_f13', 'rsfmri_has_nii_f05', 'rsfmri_has_nii_f09', 'rsfmri_has_nii_f13', 'has_braces_mri_f05', 'has_braces_mri_f09', 'has_braces_mri_f13', 'exclude_incidental_f05', 'exclude_incidental_f09', 'exclude_incidental_f13', 'freesurfer_qc_f05', 'freesurfer_qc_f09', 'freesurfer_qc_f13', 'folders_f05', 'folders_f09', 'folders_f13', 'num_vols_bold_f09', 'num_vols_bold_f13', 'mean_bold_rms_f09', 'mean_bold_rms_f13', 'n_outliers_bold_f09', 'n_outliers_bold_f13', 'exclude_bold_f09', 'exclude_bold_f13', 'reg_prob_bold_f09', 'reg_prob_bold_f13', 'exclude_reg_prob_bold_f09', 'exclude_reg_prob_bold_f13', 'software_ver_long_f05', 'software_ver_long_f09', 'software_ver_long_f13', 'software_ver_short_f05', 'software_ver_short_f09', 'software_ver_short_f13', 'dwi_eddy_qc_mot_abs_f05', 'dwi_eddy_qc_mot_rel_f05', 'dwi_eddy_qc_outliers_pe_f05', 'dwi_eddy_qc_mot_abs_f09','dwi_eddy_qc_mot_rel_f09', 'dwi_eddy_qc_outliers_pe_f09','dwi_eddy_qc_mot_abs_f13','dwi_eddy_qc_mot_rel_f13', 'dwi_eddy_qc_outliers_pe_f13', 'dti_man_qc_f05', 'dti_man_qc_f09', 'dti_man_qc_f13', 'dti_auto_qc_f05', 'dti_auto_qc_f09', 'dti_auto_qc_f13', 'dti_overall_qc_f05', 'dti_overall_qc_f09', 'dti_overall_qc_f13', 'dwi_eddy_qc_outliers_tot_f13', 'dwi_eddy_qc_outliers_tot_f09', 'dwi_eddy_qc_outliers_tot_f05'))
df_final_fmri <- dplyr::select(inclusioncri19, -c('INTAKEPERIOD', 'GESTINT', 'c1100101', 'c1100301', 'c1100501', 'c1100701', 'c1100901', 'c1101101', 'c1101301', 'c1102301', 'c1102901', 'c1103101', 'c1103301', 'c1103501', 'c1103701', 'flu_tri1', 'pharyngitis_tri1', 'rhinitis_tri1', 'sinusitis_tri1', 'earinf_tri1', 'pneumonia_tri1', 'dermatitis_tri1', 'herpeszoster_tri1', 'enteritis_tri1', 'cystitis_tri1', 'jaundice_tri1', 'otherinf_tri1', 'b0900105', 'b0900305', 'b0900505', 'b0900705', 'b0900905', 'b0901105', 'b0901305', 'b0902305', 'b0902905', 'b0903105', 'b0903305', 'b0903505', 'rhinitis_tri3', 'sinusitis_tri3', 'earinf_tri3', 'pneumonia_tri3', 'dermatitis_tri3', 'herpeszoster_tri3', 'enteritis_tri3', 'cystitis_tri3', 'jaundice_tri3', 'otherinf_tri3', 'a0900103', 'a0900303', 'a0900503', 'a0900703', 'a0900903', 'a0901103', 'a0901303', 'a0902303', 'a0902903', 'a0903103', 'a0903303', 'a0903503', 'a0903703', 'flu_tri2', 'pharyngitis_tri2', 'rhinitis_tri2', 'sinusitis_tri2', 'earinf_tri2', 'pneumonia_tri2', 'dermatitis_tri2', 'herpeszoster_tri2', 'enteritis_tri2', 'cystitis_tri2', 'jaundice_tri2', 'otherinf_tri2', 'Chlamydia_or_other_STD', 'Lower_respiratory_infection', 'Upper_respiratory_infection', 'Gastro_intestinal_infection', 'UWI', 'STD_tri1', 'lower_resp_inf_tri1', 'upper_resp_inf_tri1', 'GI_inf_tri1', 'UWI_tri1', 'GR1001_Chlamydia_or_other_STD', 'GR1001_Lower_respiratory_infection', 'GR1001_Upper_respiratory_infection', 'GR1001_Gastro_intestinal_infection', 'GR1001_UWI', 'GR1001_Other', 'GR1003_Chlamydia_or_other_STD', 'GR1003_Lower_respiratory_infection', 'GR1003_Upper_respiratory_infection', 'GR1003_Gastro_intestinal_infection', 'GR1003_UWI', 'GR1003_Other', 'GR1003_Any_other_infection', 'GR1005_Chlamydia_or_other_STD', 'GR1005_Lower_respiratory_infection', 'GR1005_Upper_respiratory_infection', 'GR1005_Gastro_intestinal_infection', 'GR1005_UWI', 'GR1005_Other', 'GR1005_Any_other_infection', 'STD_tri2', 'lower_resp_inf_tri2', 'upper_resp_inf_tri2', 'GI_inf_tri2', 'UWI_tri2', 'STD_tri3', 'lower_resp_inf_tri3', 'upper_resp_inf_tri3', 'GI_inf_tri3', 'UWI_tri3', 'clean_upper_resp_inf_tri1', 'clean_upper_resp_inf_tri2', 'clean_upper_resp_inf_tri3', 'clean_lower_resp_inf_tri1', 'clean_lower_resp_inf_tri2', 'clean_lower_resp_inf_tri3', 'clean_uwi_tri1', 'clean_uwi_tri2', 'clean_uwi_tri3', 'clean_GI_inf_tri1', 'clean_GI_inf_tri2', 'clean_GI_inf_tri3', 'flu_tri3', 'b0903705', 'pharyngitis_tri3', 'mri_consent_f05', 'mri_consent_f09', 'mri_consent_f13', 'age_child_mri_f05', 'age_child_mri_f09', 't1_has_nii_f05', 't1_has_nii_f09', 't1_has_nii_f13', 't1_asset_has_nii_f09', 't1_braces_has_nii_f13', 'qdec_has_lgi_f05', 'qdec_has_lgi_f09', 'qdec_has_lgi_f13', 'dti_has_nii_f05', 'dti_has_nii_f09', 'dti_has_nii_f13', 'rsfmri_has_nii_f05', 'rsfmri_has_nii_f09', 'rsfmri_has_nii_f13', 'has_braces_mri_f05', 'has_braces_mri_f09', 'has_braces_mri_f13', 'exclude_incidental_f05', 'exclude_incidental_f09', 'exclude_incidental_f13', 'freesurfer_qc_f05', 'freesurfer_qc_f09', 'freesurfer_qc_f13', 'folders_f05', 'folders_f09', 'folders_f13', 'num_vols_bold_f09', 'num_vols_bold_f13', 'mean_bold_rms_f09', 'mean_bold_rms_f13', 'n_outliers_bold_f09', 'n_outliers_bold_f13', 'exclude_bold_f09', 'exclude_bold_f13', 'reg_prob_bold_f09', 'reg_prob_bold_f13', 'exclude_reg_prob_bold_f09', 'exclude_reg_prob_bold_f13', 'software_ver_long_f05', 'software_ver_long_f09', 'software_ver_long_f13', 'software_ver_short_f05', 'software_ver_short_f09', 'software_ver_short_f13', 'dwi_eddy_qc_mot_abs_f05', 'dwi_eddy_qc_mot_rel_f05', 'dwi_eddy_qc_outliers_pe_f05', 'dwi_eddy_qc_mot_abs_f09','dwi_eddy_qc_mot_rel_f09', 'dwi_eddy_qc_outliers_pe_f09','dwi_eddy_qc_mot_abs_f13','dwi_eddy_qc_mot_rel_f13', 'dwi_eddy_qc_outliers_pe_f13', 'dti_man_qc_f05', 'dti_man_qc_f09', 'dti_man_qc_f13', 'dti_auto_qc_f05', 'dti_auto_qc_f09', 'dti_auto_qc_f13', 'dti_overall_qc_f05', 'dti_overall_qc_f09', 'dti_overall_qc_f13', 'dwi_eddy_qc_outliers_tot_f13', 'dwi_eddy_qc_outliers_tot_f09', 'dwi_eddy_qc_outliers_tot_f05'))

write.csv(df_final_T1, 'T1_multimodal_imaging_df.csv')
write.csv(df_final_fmri, 'fmri_multimodal_imaging_df.csv')

######Checking CRP correlation with infection score######
CRP_df <- read.spss('MOTHERPREGNANCY-CRP_17072015.sav', to.data.frame = T)
full_w_crp_df <- merge(full_df, CRP_df, by = c('IDM'), all = T)

CRP_inf <- dplyr::select(full_w_crp_df, CRP_g1,sumscore_inf_tri1,sumscore_inf_tri2,sumscore_inf_tri3,sumscore_inf_tot,sumscore_fever_tot)

CRP_inf <- CRP_inf %>% rename("CRP trimester 1" = CRP_g1, "Infection trimester 1" = sumscore_inf_tri1, "Infection trimester 2" = sumscore_inf_tri2, "Infection trimester 3" = sumscore_inf_tri3, "Total infection score" = sumscore_inf_tot, "Total fever score" = sumscore_fever_tot)

corr_crp <- cor(CRP_inf, use="pairwise.complete.obs")
corrplot::corrplot(corr_crp, method = 'color', col.lim =c(0,1))

######Visualize demographics infection score######
#Selecting infection variables for df
infection_df <- full_df[, c("clean_upper_resp_inf_tri1", "clean_upper_resp_inf_tri2", "clean_upper_resp_inf_tri3", "clean_lower_resp_inf_tri1", "clean_lower_resp_inf_tri2", "clean_lower_resp_inf_tri3","clean_GI_inf_tri1", "clean_GI_inf_tri2", "clean_GI_inf_tri3", "flu_tri1", "flu_tri2", "flu_tri3", "clean_uwi_tri1", "clean_uwi_tri2", "clean_uwi_tri3","dermatitis_tri1", "dermatitis_tri2", "dermatitis_tri3", "jaundice_tri1", "jaundice_tri2", "jaundice_tri3", "herpeszoster_tri1", "herpeszoster_tri2", "herpeszoster_tri3", "STD_tri1", "STD_tri2", "STD_tri3", "fever_tri1", "fever_tri2", "fever_tri3")]
infection_df$up_resp_inf <- infection_df$clean_upper_resp_inf_tri1 + infection_df$clean_upper_resp_inf_tri2 + infection_df$clean_upper_resp_inf_tri3
infection_df$low_resp_inf <- infection_df$clean_lower_resp_inf_tri1 + infection_df$clean_lower_resp_inf_tri2 + infection_df$clean_lower_resp_inf_tri3
infection_df$GI_inf <- infection_df$clean_GI_inf_tri1 + infection_df$clean_GI_inf_tri2 + infection_df$clean_GI_inf_tri3
infection_df$flu <- infection_df$flu_tri1 + infection_df$flu_tri2 + infection_df$flu_tri3
infection_df$UWI <- infection_df$clean_uwi_tri1 + infection_df$clean_uwi_tri2 + infection_df$clean_uwi_tri3
infection_df$dermatitis <- infection_df$dermatitis_tri1 + infection_df$dermatitis_tri2+ infection_df$dermatitis_tri3
infection_df$eye_inf <- infection_df$jaundice_tri1 + infection_df$jaundice_tri2 + infection_df$jaundice_tri3
infection_df$herpeszoster <- infection_df$herpeszoster_tri1 + infection_df$herpeszoster_tri2 + infection_df$herpeszoster_tri3
infection_df$STD <- infection_df$STD_tri1 + infection_df$STD_tri2 + infection_df$STD_tri3
infection_df$fever <- infection_df$fever_tri1 + infection_df$fever_tri2 + infection_df$fever_tri3
total_inf_df <- infection_df[, c("up_resp_inf", "low_resp_inf", "GI_inf","flu", "UWI","dermatitis","eye_inf","herpeszoster","fever", "STD")]

dd <- total_inf_df %>% rename(a = up_resp_inf, b = low_resp_inf, c = GI_inf, d = flu, e = UWI, f= dermatitis, g = eye_inf, h = herpeszoster, i = fever, j = STD)

names <- c("a", "b", "c","d", "e","f","g","h","i", "j")
dd[,names] <- lapply(dd[,names], factor)

#calculating frequency in % for each infection type
summary(dd)

#calculate %prevalence of each infection type and put that in the dataframe
tri_df <- data.frame(
  Infection <- as.factor(c("a", "b", "c","d", "e","f","g","h","i")),
  Trimester_one <- as.numeric(c(55.5, 17.4, 16.4, 2.6,2.6, 0.3, 0.1, 0.1, 0.1)),
  Trimester_two <- as.numeric(c(54.9, 15.3, 14.2, 2.3,2.7, 0.3, 0.2, 0.2, 0.2)),
  Trimester_three <- as.numeric(c(51.7, 15.9, 12.7, 3.5,1.7, 0.4, 0.2, 0.1, 0.1))
)

a <- ggplot(tri_df, aes(x = Infection, y = Trimester_one, fill = Infection)) + geom_bar(stat = 'identity') + scale_fill_brewer(palette = 'Set3', labels = c("Upper respiratory infection", "Gastro-intestinal infection", "Flu", "Urinary tract infection", "Dermatitis", "Lower respiratory infection", "Herpes Zoster", "Eye infection", "STD")) + theme_classic() + labs(fill = 'Infection type') + ylim(0, 60) + labs(title = 'Trimester 1', y= '%') 

b <- ggplot(tri_df, aes(x = Infection, y = Trimester_two, fill = Infection)) + geom_bar(stat = 'identity')  + scale_fill_brewer(palette = 'Set3', labels = c("Upper respiratory infection", "Gastro-intestinal infection", "Flu", "Urinary tract infection", "Dermatitis", "Lower respiratory infection", "Herpes Zoster", "Eye infection", "STD")) + theme_classic() + labs(fill = 'Infection type') + ylim(0, 60)+ labs(title = 'Trimester 2', y= '%')

c <- ggplot(tri_df, aes(x = Infection, y = Trimester_three, fill = Infection)) + geom_bar(stat = 'identity') + scale_fill_brewer(palette = 'Set3', labels = c("Upper respiratory infection", "Gastro-intestinal infection", "Flu", "Urinary tract infection", "Dermatitis", "Lower respiratory infection", "Herpes Zoster", "Eye infection", "STD")) + theme_classic() + labs(fill = 'Infection type') + ylim(0, 60)+ labs(title = 'Trimester 3', y= '%')

figure <- ggarrange(a, b, c, 
                    labels =c('A', 'B', 'C'),
                    ncol = 3, nrow =1, common.legend = T, legend = 'bottom')
figure

annotate_figure(figure, top = text_grob('Distribution prenatal infection types', color = 'black', face = 'bold', size =14))

ggexport(figure, filename = 'Figure_infections.pdf')

#######Multiple testing correction#######-----
#Compute number of effective tests to get new p value threshold
compute_effective_tests <- function(d) {
  #d is a data frame with only the variables you want to include
  #e.g.,
  #
  #d <- data.frame(x, y, z, a, b, c)
  alpha = 0.05
  M <- length(d)
  cor_mat <- cor(d, use="pairwise.complete.obs")
  evals = eigen(cor_mat)$values
  #in the Galwey / Gao Paper, they seem to ignore negative values by setting them to 0...
  #they say it is a rare occurance.?
  evals[evals<0] <- 0
  if (0 %in% evals) {
    print("WARNING:    Negative Eigen values present")
    print("setting to zero")
    print(evals)
  }
  #formula from Galway 2009 http://www.ncbi.nlm.nih.gov/pubmed/19217024
  Meff = ((sum(sqrt(evals)))^2) / sum(evals)
  Minfo <- c(M, Meff)
  #provide the new pvalue to achieve alpha 0.05
  sidak = 1 - ((1 - alpha)^(1/Meff))
  Minfo <- c(Minfo, sidak)
  names(Minfo) <- c('M', 'Meff', 'sidak')
  return(Minfo)
}

global_df <- dplyr::select(df_final_T1, mean_FA_genr_f13, mean_MD_genr_f13, lateral_ventricles,tot_cerebellum, CerebralWhiteMatterVol_f13, genr_tbv_f13, CortexVol_f13, cpl, ge, mod)

compute_effective_tests(d = global_df) #7.5 tests 

exploratory_df <- dplyr::select(df_final_T1, 5:39, 49:62,66:72, 90:115 )

compute_effective_tests(d = exploratory_df) #47.1 tests 

#Correlation matrix
global_df_new <- global_df  %>% rename("Global mean fractional anisotropy" = mean_FA_genr_f13, "Global mean diffusivity" = mean_MD_genr_f13, "Lateral ventricle volume" =lateral_ventricles, "Cerebellar volume" =tot_cerebellum, "Cerebral white matter volume" =CerebralWhiteMatterVol_f13, "Total brain volume"=genr_tbv_f13, "Cortical gray matter volume" = CortexVol_f13, "Characteristic path length"=cpl, "Global efficiency"= ge, "Modularity" = mod)

cor_mat <- cor(global_df_new, use="pairwise.complete.obs")
corrplot::corrplot(cor_mat, method = 'color') 

cor_mat2 <- cor(exploratory_df, use="pairwise.complete.obs")
corrplot::corrplot(cor_mat2, method = 'color')

#######Non-response analysis#######-----
#for the variables: maternal ethnicity (categorical), maternal age (continuous), household income (categorical), maternal education (categorical)
#categorical variables with chisq and continuous variables with t-test
df_NR1 <- full_df[!(full_df$IDC %in% df_final_T1$IDC),] #data frame with the excluded sample 
df_NR2 <- full_df[!(full_df$IDC %in% df_final_DTI$IDC),]
df_NR3 <- full_df[!(full_df$IDC %in% df_final_fmri$IDC),]

t.test(df_NR1$AGE_M_v2, df_final_T1$AGE_M_v2, paired = F) #sign 
t.test(df_NR2$AGE_M_v2, df_final_DTI$AGE_M_v2, paired = F) #sign 
t.test(df_NR3$AGE_M_v2, df_final_fmri$AGE_M_v2, paired = F) #sign 

#T1
summary(df_NR1$INCOME)
summary(df_final_T1$INCOME)
chisq.test(cbind(table(df_NR1$INCOME), table(df_final_T1$INCOME))) #sign

summary(df_NR1$ETHNMv2)
summary(df_final_T1$ETHNMv2)
chisq.test(cbind(table(df_NR1$ETHNMv2), table(df_final_T1$ETHNMv2))) #sign

summary(df_NR1$EDUCM)
summary(df_final_T1$EDUCM_3groups)
chisq.test(cbind(table(df_NR1$EDUCM_3groups), table(df_final_T1$EDUCM_3groups))) #sign

#DTI
summary(df_NR2$INCOME)
summary(df_final_DTI$INCOME)
chisq.test(cbind(table(df_NR2$INCOME), table(df_final_DTI$INCOME))) #sign

summary(df_NR2$ETHNMv2)
summary(df_final_DTI$ETHNMv2)
chisq.test(cbind(table(df_NR2$ETHNMv2), table(df_final_DTI$ETHNMv2))) #sign

summary(df_NR2$EDUCM)
summary(df_final_DTI$EDUCM_3groups)
chisq.test(cbind(table(df_NR2$EDUCM_3groups), table(df_final_DTI$EDUCM_3groups))) #sign

#fMRI
summary(df_NR3$INCOME)
summary(df_final_fmri$INCOME)
chisq.test(cbind(table(df_NR3$INCOME), table(df_final_fmri$INCOME))) #sign

summary(df_NR3$ETHNMv2)
summary(df_final_fmri$ETHNMv2)
chisq.test(cbind(table(df_NR3$ETHNMv2), table(df_final_fmri$ETHNMv2))) #sign

summary(df_NR3$EDUCM)
summary(df_final_fmri$EDUCM_3groups)
chisq.test(cbind(table(df_NR3$EDUCM_3groups), table(df_final_fmri$EDUCM_3groups))) #sign


#######Baseline characteristics#######-----
summary_continuous <- function(x)
{
  standev <- sd(x, na.rm = T)
  meanvar <- mean(x, na.rm = T)
  print(paste0(round(meanvar, 1), '(', round(standev, 1), ')'))
}

summary_categorical <- function(x)
{
  tab1 <- prop.table(table(x, useNA = 'always'))
  tab2 <- table(x, useNA = "always")
  print(paste(round(tab1 * 100, 1), '%', names(tab1), collapse = ','))
  print(paste(tab2, names(tab2)))
}

baselinevars <- c("age_child_mri_f13", "GENDER","AGE_M_v2","ETHNMv2","EDUCM_3groups","INCOME","f1200101","SMOKE_ALL","mdrink_updated","GSI","sumscore_inf_tot", "sumscore_fever_tot", "sumscore_inf_tri1","sumscore_inf_tri2","sumscore_inf_tri3", "genr_tbv_f13", "mean_FA_genr_f13","mean_MD_genr_f13", 'ge', 'mod', 'cpl')

#For T1 sample
for(i in baselinevars){
  x <- df_final_T1[, i]
  message(i)
  if (class(x) == 'numeric') {
    summary_continuous(x)
  } else { summary_categorical(x) }
}

#For DTI sample 
for(i in baselinevars){
  x <- df_final_DTI[, i]
  message(i)
  if (class(x) == 'numeric') {
    summary_continuous(x)
  } else { summary_categorical(x) }
}

#For fMRI sample
for(i in baselinevars){
  x <- df_final_fmri[, i]
  message(i)
  if (class(x) == 'numeric') {
    summary_continuous(x)
  } else { summary_categorical(x) }
}


#######Multiple imputation for missing values#######----
#Impute second hits and covariates (mice)
missvalues1 <- cbind("# NA" = sort(colSums(is.na(df_final_T1))),
                    "% NA" = round(sort(colMeans(is.na(df_final_T1))) * 100, 2))
missvalues2 <- cbind("# NA" = sort(colSums(is.na(df_final_DTI))),
                     "% NA" = round(sort(colMeans(is.na(df_final_DTI))) * 100, 2))
missvalues3 <- cbind("# NA" = sort(colSums(is.na(df_final_fmri))),
                     "% NA" = round(sort(colMeans(is.na(df_final_fmri))) * 100, 2))
missvalues1
missvalues2
missvalues3

#' Rule of thumb for number of datasets to impute
mean(missvalues1[, 2]) #2
mean(missvalues2[,2]) #2
mean(missvalues3[,2]) #1

#Running setup imputation run
imp0 <- mice(df_final_T1, maxit = 0, defaultMethod = c("norm", "logreg", "polyreg", "polr"))
imp1 <- mice(df_final_DTI, maxit = 0, defaultMethod = c("norm", "logreg", "polyreg", "polr"))
imp2 <-  mice(df_final_fmri, maxit = 0, defaultMethod = c("norm", "logreg", "polyreg", "polr"))
imp0$loggedEvents 
imp1$loggedEvents
imp2$loggedEvents

#Imputation method matrix
meth <- imp0$method
meth2 <- imp1$method
meth3 <- imp2$method

#Variables that should not be imputed are set to ""
meth[c(1:76)] <- ""
meth2[c(1:76)] <- ""
meth3[c(1:76)] <- ""

#Predictor matrix
pred <- imp0$predictorMatrix
pred2 <- imp1$predictorMatrix
pred3 <- imp2$predictorMatrix

pred[, c(1:76, 86:124)] <- 0 #variables not to be used as predictor are set to 0
pred2[, c(1:76, 86:124)] <- 0
pred3[, c(1:76, 86:124)] <- 0

#Visit sequence
visSeq <- imp0$visitSequence
visSeq2 <- imp1$visitSequence
visSeq3 <- imp2$visitSequence

#Performing the imputation
#'30 iterations (generally recommended) with 30 datasets (rule of thumb) for reaching convergence
imp.test <- mice(df_final_T1, method = meth, predictorMatrix = pred, visitSequence = visSeq, maxit = 30, m = 30, printFlag = TRUE)

imp.test2 <- mice(df_final_DTI, method = meth2, predictorMatrix = pred2, visitSequence = visSeq2, maxit = 30, m = 30, printFlag = TRUE)

imp.test3 <- mice(df_final_fmri, method = meth3, predictorMatrix = pred3, visitSequence = visSeq3, maxit = 30, m = 30, printFlag = TRUE)

#'check trace plots if convergence is reached 
plot(imp.test) 
plot(imp.test2)
plot(imp.test3)

#Transformations----
imp.test_long <- complete(imp.test, include = T, action = "long")
imp.test_long2 <- complete(imp.test2, include = T, action = "long")
imp.test_long3 <- complete(imp.test3, include = T, action = "long")

infectionvars <- c('sumscore_inf_tri1', 'sumscore_inf_tri2', 'sumscore_inf_tri3', 'sumscore_inf_tot', 'sumscore_fever_tot') 
globalbrainvars <- c("Brain_Stem_vol_f13", "lateral_ventricles", "tot_cerebellum", "tot_thalamus", "tot_caudate", "tot_putamen", "tot_amygdala", "tot_accumbens", "CortexVol_f13", "CerebralWhiteMatterVol_f13", "genr_tbv_f13", "eTIV_f13", "tot_hippocampus", "tot_pallidum")
specificbrainvars<- c("tot_bankssts","tot_caudalanteriorcingulate", "tot_caudalmiddlefrontal", "tot_cuneus", "tot_entorhinal", "tot_fusiform","tot_inferiorparietal","tot_inferiortemporal", "tot_isthmuscingulate","tot_lateraloccipital", "tot_lateralorbitofrontal", "tot_lingual","tot_medialorbitofrontal","tot_middletemporal", "tot_parahippocampal", "tot_paracentral","tot_parsopercularis", "tot_parsorbitalis","tot_parstriangularis","tot_pericalcerine", "tot_postcentral","tot_posteriorcingulate","tot_precentral", "tot_precuneus","tot_rostralanteriorcingulate", "tot_rostralmiddlefrontal", "tot_superiorfrontal","tot_superiorparietal","tot_superiortemporal", "tot_supramarginal","tot_frontalpole","tot_temporalpole", "tot_transversetemporal","tot_insula", "CC_Posterior_vol_f13", "CC_Mid_Posterior_vol_f13", "CC_Central_vol_f13", "CC_Mid_Anterior_vol_f13", "CC_Anterior_vol_f13")
DTIvars <- c("dti_unc_FA","dti_cgc_FA", "dti_slf_FA","dti_fmi_FA","dti_fma_FA", "dti_ilf_FA","dti_cst_FA" , "dti_unc_MD", "dti_cgc_MD", "dti_slf_MD","dti_fmi_MD","dti_fma_MD", "dti_ilf_MD","dti_cst_MD", 'mean_FA_genr_f13', 'mean_MD_genr_f13')
fMRIvars <- c("wiNetwork_none", "wiNetwork_default", "wiNetwork_ParietoOccip","wiNetwork_FrontoParietal","wiNetwork_Salience","wiNetwork_CinguloOperc","wiNetwork_MedialParietal","wiNetwork_DorsalAttn","wiNetwork_VentralAttn","wiNetwork_Visual","wiNetwork_SMhand","wiNetwork_SMmouth","wiNetwork_Auditory","btNetwork_none","btNetwork_default","btNetwork_ParietoOccip","btNetwork_FrontoParietal","btNetwork_Salience","btNetwork_CinguloOperc","btNetwork_MedialParietal","btNetwork_DorsalAttn","btNetwork_VentralAttn","btNetwork_Visual","btNetwork_SMhand","btNetwork_SMmouth","btNetwork_Auditory","btNetwork_SMhand","btNetwork_SMmouth","btNetwork_Auditory","cpl","ge", "mod")

scalevars1 <- c(all_of(infectionvars), all_of(globalbrainvars), all_of(specificbrainvars))
scalevars2 <- c(all_of(infectionvars), all_of(DTIvars))
scalevars3 <- c(all_of(infectionvars), all_of(fMRIvars))

for (x in scalevars1){ #write a function to standardize all continuous exposure and outcome variables at once
  t <- imp.test_long[x]
  colname <- paste0(colnames(t), "_standardized")
  imp.test_long[,colname] <- as.numeric(scale(t))
}

for (x in scalevars2){ 
  z <- imp.test_long2[x]
  colname <- paste0(colnames(z), "_standardized")
  imp.test_long2[,colname] <- as.numeric(scale(z))
}

for (x in scalevars3){ 
  z <- imp.test_long3[x]
  colname <- paste0(colnames(z), "_standardized")
  imp.test_long3[,colname] <- as.numeric(scale(z))
}

imp.test_mids_T1 <- as.mids(imp.test_long)
imp.test_mids_DTI <- as.mids(imp.test_long2)
imp.test_mids_fmri <- as.mids(imp.test_long3)

#######ANALYSES#######
setwd("PUT_PATH_WHERE_YOU_WANT_THE_RESULTS")

#######Linear regression: global measures#######----
global_brain_ztrans <- c("lateral_ventricles_standardized","CerebralWhiteMatterVol_f13_standardized","CortexVol_f13_standardized", "tot_cerebellum_standardized","Brain_Stem_vol_f13_standardized", "tot_thalamus_standardized", "tot_caudate_standardized", "tot_putamen_standardized", "tot_amygdala_standardized", "tot_accumbens_standardized")

results_globalbrain <- data.frame()

for (x in global_brain_ztrans){
  f <- paste0(x,"~ sumscore_inf_tot_standardized + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI + eTIV_f13_standardized")
  bval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,6]
  results_globalbrain[x,1] <- bval
  results_globalbrain[x,2] <- seval
  results_globalbrain[x,3] <- lowerCI
  results_globalbrain[x,4] <- upperCI
  results_globalbrain[x,5] <- pval
}

colnames(results_globalbrain) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_globalbrain$sign = "" 
results_globalbrain$sign[results_globalbrain$pval < 0.05] = "*"
results_globalbrain$sign[results_globalbrain$pval < 0.01] = "**"
results_globalbrain$sign[results_globalbrain$pval < 0.001] = "***"

write.xlsx(results_globalbrain, "Results_T1_global.xlsx")

#forgotton structure so individually running this regression 
summary(pool(with(imp.test_mids_T1, lm(tot_hippocampus_standardized~ sumscore_inf_tot_standardized + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI + eTIV_f13_standardized))), conf.int = T)
summary(pool(with(imp.test_mids_T1, lm(tot_hippocampus_standardized~ sumscore_inf_tot_standardized + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI))), conf.int = T)

#######Linear regression: desilian killiany atlas, all specific areas#######----
DK_atlas_ztrans <- paste0(specificbrainvars, "_standardized")

results_DK_atlas <- data.frame()

for (x in DK_atlas_ztrans){
  f <- paste0(x,"~ sumscore_inf_tot_standardized + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI + eTIV_f13_standardized")
  bval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,6]
  results_DK_atlas[x,1] <- bval
  results_DK_atlas[x,2] <- seval
  results_DK_atlas[x,3] <- lowerCI
  results_DK_atlas[x,4] <- upperCI
  results_DK_atlas[x,5] <- pval
}

colnames(results_DK_atlas) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_DK_atlas$sign = "" 
results_DK_atlas$sign[results_DK_atlas$pval < 0.05] = "*"
results_DK_atlas$sign[results_DK_atlas$pval < 0.01] = "**"
results_DK_atlas$sign[results_DK_atlas$pval < 0.001] = "***"

write.xlsx(results_DK_atlas, "Results_T1_exploratory.xlsx")

#######Linear regression: DTI - FA #######----
DTI_ztrans <- paste0(DTIvars, "_standardized")

results_DTI <- data.frame()

for (x in DTI_ztrans){
  f <- paste0(x,"~ sumscore_inf_tot_standardized + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI")
  bval <- summary(pool(with(imp.test_mids_DTI, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test_mids_DTI, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test_mids_DTI, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test_mids_DTI, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test_mids_DTI, lm(as.formula(f)))),conf.int = T)[2,6]
  results_DTI[x,1] <- bval
  results_DTI[x,2] <- seval
  results_DTI[x,3] <- lowerCI
  results_DTI[x,4] <- upperCI
  results_DTI[x,5] <- pval
}

colnames(results_DTI) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_DTI$sign = "" 
results_DTI$sign[results_DTI$pval < 0.05] = "*"
results_DTI$sign[results_DTI$pval < 0.01] = "**"
results_DTI$sign[results_DTI$pval < 0.001] = "***"

write.xlsx(results_DTI, "Results_DTI.xlsx")

#######TOtal brain volume#######----
#Seperate because not normalized for ICV, so running all analyses for total infection, severity marker and timing of infection effect together in this loop 
all_exposure_vars <- c('sumscore_inf_tot_standardized', 'sumscore_fever_tot_standardized', 'sumscore_inf_tri1_standardized', 'sumscore_inf_tri2_standardized', 'sumscore_inf_tri3_standardized')

results_tbv <- data.frame()

for(x in all_exposure_vars){
  f <- paste0("genr_tbv_f13_standardized ~ GENDER + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + EDUCM_3groups + GSI +", x)
  bval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[9,2]
  seval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[9,3]
  lowerCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[9,7]
  upperCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[9,8]
  pval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[9,6]
  results_tbv [x,1] <- bval
  results_tbv [x,2] <- seval
  results_tbv [x,3] <- lowerCI
  results_tbv [x,4] <- upperCI
  results_tbv [x,5] <- pval
}

colnames(results_tbv) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_tbv$sign = "" 
results_tbv$sign[results_tbv$pval < 0.05] = "*"
results_tbv$sign[results_tbv$pval < 0.01] = "**"
results_tbv$sign[results_tbv$pval < 0.001] = "***"

write.xlsx(results_tbv, "Results_totalbrainvolume.xlsx")

#######Sensitivity analysis: sMRI (without correcting for ICV#######----
combined_t1_vars <- c(global_brain_ztrans, DK_atlas_ztrans)

results_sens_analysis <- data.frame()

for (x in combined_t1_vars){
  f <- paste0(x,"~ sumscore_inf_tot_standardized + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI")
  #values for total infection 
  bval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,6]
  results_sens_analysis[x,1] <- bval
  results_sens_analysis[x,2] <- seval
  results_sens_analysis[x,3] <- lowerCI
  results_sens_analysis[x,4] <- upperCI
  results_sens_analysis[x,5] <- pval
}

colnames(results_sens_analysis) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_sens_analysis$sign = "" 
results_sens_analysis$sign[results_sens_analysis$pval < 0.05] = "*"
results_sens_analysis$sign[results_sens_analysis$pval < 0.01] = "**"
results_sens_analysis$sign[results_sens_analysis$pval < 0.001] = "***"

write.xlsx(results_sens_analysis, "Results_T1_sens_analysis.xlsx")

#######Linear regression: fMRI#######----
fMRIvars_ztrans <- paste0(fMRIvars, "_standardized")

results_fMRI_analysis <- data.frame()

for (x in fMRIvars_ztrans){
  f <- paste0(x,"~ sumscore_inf_tot_standardized + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI")
  #values for total infection 
  bval <- summary(pool(with(imp.test_mids_fmri, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test_mids_fmri, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test_mids_fmri, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test_mids_fmri, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test_mids_fmri, lm(as.formula(f)))),conf.int = T)[2,6]
  results_fMRI_analysis[x,1] <- bval
  results_fMRI_analysis[x,2] <- seval
  results_fMRI_analysis[x,3] <- lowerCI
  results_fMRI_analysis[x,4] <- upperCI
  results_fMRI_analysis[x,5] <- pval
}

colnames(results_fMRI_analysis) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_fMRI_analysis$sign = "" 
results_fMRI_analysis$sign[results_fMRI_analysis$pval < 0.05] = "*"
results_fMRI_analysis$sign[results_fMRI_analysis$pval < 0.01] = "**"
results_fMRI_analysis$sign[results_fMRI_analysis$pval < 0.001] = "***"

write.xlsx(results_fMRI_analysis, "Results_fMRI.xlsx")

#######Moderation analysis #######----
#Significant brain outcomes: effect of sex, effect of timing and severity ----
significant_brain_outcomes <- c("CerebralWhiteMatterVol_f13_standardized", "tot_caudalanteriorcingulate_standardized", "tot_accumbens_standardized")

#effect of sex
results_sex_analysis <- data.frame()

for (x in significant_brain_outcomes){
  f <- paste0(x,"~ sumscore_inf_tot_standardized*GENDER + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + EDUCM_3groups + GSI + eTIV_f13_standardized")
  #values for total infection 
  bval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,6]
  results_sex_analysis[x,1] <- bval
  results_sex_analysis[x,2] <- seval
  results_sex_analysis[x,3] <- lowerCI
  results_sex_analysis[x,4] <- upperCI
  results_sex_analysis[x,5] <- pval
}

colnames(results_sex_analysis) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_sex_analysis$sign = "" 
results_sex_analysis$sign[results_sex_analysis$pval < 0.05] = "*"
results_sex_analysis$sign[results_sex_analysis$pval < 0.01] = "**"
results_sex_analysis$sign[results_sex_analysis$pval < 0.001] = "***"

write.xlsx(results_sex_analysis, "Results_sex_moderation.xlsx")

#effect of severity
results_severity_analysis <- data.frame()

for (x in significant_brain_outcomes){
  f <- paste0(x,"~ sumscore_fever_tot_standardized + GENDER + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + EDUCM_3groups + GSI + eTIV_f13_standardized")
  #values for total infection 
  bval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,6]
  results_severity_analysis[x,1] <- bval
  results_severity_analysis[x,2] <- seval
  results_severity_analysis[x,3] <- lowerCI
  results_severity_analysis[x,4] <- upperCI
  results_severity_analysis[x,5] <- pval
}

colnames(results_severity_analysis) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_severity_analysis$sign = "" 
results_severity_analysis$sign[results_severity_analysis$pval < 0.05] = "*"
results_severity_analysis$sign[results_severity_analysis$pval < 0.01] = "**"
results_severity_analysis$sign[results_severity_analysis$pval < 0.001] = "***"

write.xlsx(results_severity_analysis, "Results_severity_analysis.xlsx")

#effect of timing
results_timing_analysis <- data.frame()

for (x in significant_brain_outcomes){
  f <- paste0(x,"~ sumscore_inf_tri1_standardized + GENDER + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + EDUCM_3groups + GSI + eTIV_f13_standardized")
  #values for trimester 1
  bval_tri1 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,2]
  seval_tri1 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI_tri1 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI_tri1 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,8]
  pval_tri1 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(f)))),conf.int = T)[2,6]
  results_timing_analysis[x,1] <- bval_tri1
  results_timing_analysis[x,2] <- seval_tri1
  results_timing_analysis[x,3] <- lowerCI_tri1
  results_timing_analysis[x,4] <- upperCI_tri1
  results_timing_analysis[x,5] <- pval_tri1
  #values for trimester 2
  g <- paste0(x,"~ sumscore_inf_tri2_standardized + GENDER + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + EDUCM_3groups + GSI + eTIV_f13_standardized")
  bval_tri2 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(g)))),conf.int = T)[2,2]
  seval_tri2 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(g)))),conf.int = T)[2,3]
  lowerCI_tri2 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(g)))),conf.int = T)[2,7]
  upperCI_tri2 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(g)))),conf.int = T)[2,8]
  pval_tri2 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(g)))),conf.int = T)[2,6]
  results_timing_analysis[x,6] <- bval_tri2
  results_timing_analysis[x,7] <- seval_tri2
  results_timing_analysis[x,8] <- lowerCI_tri2
  results_timing_analysis[x,9] <- upperCI_tri2
  results_timing_analysis[x,10] <- pval_tri2
  #values for trimester 3
  h <- paste0(x,"~ sumscore_inf_tri3_standardized + GENDER + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + EDUCM_3groups + GSI + eTIV_f13_standardized")
  bval_tri3 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(h)))),conf.int = T)[2,2]
  seval_tri3 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(h)))),conf.int = T)[2,3]
  lowerCI_tri3 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(h)))),conf.int = T)[2,7]
  upperCI_tri3 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(h)))),conf.int = T)[2,8]
  pval_tri3 <- summary(pool(with(imp.test_mids_T1, lm(as.formula(h)))),conf.int = T)[2,6]
  results_timing_analysis[x,11] <- bval_tri3
  results_timing_analysis[x,12] <- seval_tri3
  results_timing_analysis[x,13] <- lowerCI_tri3
  results_timing_analysis[x,14] <- upperCI_tri3
  results_timing_analysis[x,15] <- pval_tri3
}

colnames(results_timing_analysis) <- c("bval_tri1", "seval_tri1", "lowerCI_tri1", "upperCI_tri1", "pval_tri1", "bval_tri2", "seval_tri2", "lowerCI_tri2", "upperCI_tri2", "pval_tri2", "bval_tri3", "seval_tri3", "lowerCI_tri3", "upperCI_tri3", "pval_tri3")

write.xlsx(results_timing_analysis, "Results_timing-infection_analysis.xlsx")

#######Plotting significant brain outcomes----
setwd("PUT_PATH_WHERE_YOU_WANT_THE_FIGURES")

#significant brain outcomes (after ICV): caudal anterior cingulate,  cerebral white matter, accumbens
df_final_T1$fever_bin <- as.factor(ifelse((df_final_T1$fever_tri1 == 1 | df_final_T1$fever_tri2 == 1 | df_final_T1$fever_tri3 ==1), 'fever', 'no fever'))

qplot(data=df_final_T1, sumscore_inf_tot, tot_accumbens, xlab = 'prenatal infection', ylab = 'accumbens volume', geom = c('point', 'smooth'), facets = GENDER ~ fever_bin) 

qplot(data=df_final_T1, sumscore_inf_tot, tot_caudalanteriorcingulate, xlab = 'prenatal infection', ylab = 'caudal anterior cingulate volume', geom = c('point', 'smooth'), facets = GENDER ~ fever_bin)

qplot(data=df_final_T1, sumscore_inf_tot, CerebralWhiteMatterVol_f13, xlab = 'prenatal infection', ylab = 'cerebral white matter volume', geom = c('point', 'smooth'), facets = GENDER ~ fever_bin)


#######Figure neuroanatomy#######----

#general figure all regions#
ggseg()

ggseg(mapping = aes(fill = region), colour = 'black') + scale_fill_brain('dk') + theme(legend.justification = c(1,0),legend.position = 'bottom', legend.text = element_text(size = 5))+ guides(fill = guide_legend(ncol =3)) #for DK atlas, general (example)

ggseg(atlas = 'aseg', mapping = aes(fill = region)) + theme(legend.justification = c(1,0), legend.position = 'bottom', legend.text = element_text(size = 5)) + guides(fill = guide_legend(ncol =3)) #for subcortical structures, general (example)

#Figure for cortical regions 
DK_regions <- data.frame(
  region = c('bankssts', 'caudal anterior cingulate', 'caudal middle frontal', 'cuneus', 'entorhinal', 'fusiform', 'inferior parietal', 'inferior temporal', 'isthmus cingulate', 'lateral occipital', 'lateral orbitofrontal', 'lingual', 'medial orbitofrontal', 'middle temporal', 'parahippocampal', 'paracentral', 'pars opercularis', 'pars orbitalis', 'pars triangularis', 'pericalcarine', 'postcentral', 'posterior cingulate', 'precentral', 'precuneus', 'rostral anterior cingulate', 'rostral middle frontal', 'superior frontal', 'superior temporal', 'supramarginal', 'frontal pole', 'temporal pole', 'transverse temporal', 'insula'),
  B_coefficient = c(-0.038, -0.104, -0.032, -0.041, -0.002, -0.030, -0.019, -0.050, -0.027, -0.026, -0.063, 0.003, -0.047, -0.018, -0.072, -0.030, 0.025, -0.058, -0.001, -0.020, -0.054, -0.051, -0.053, -0.020, -0.050, -0.059, -0.030, -0.038, -0.001, -0.026, 0.002, -0.068, -0.016)) #df for all cortical regions, coefficients without icv adjustment 

brein <- DK_regions %>% 
  ggseg(mapping=aes(fill=B_coefficient), position = "stacked", colour = "black",
        #only show one hemisphere, as we have averaged values across both hemispheres
        hemisphere = "left", atlas = dk) + theme(text = element_text(family = "calibri", size = 10),
        axis.text = element_text(family = "calibri", size = 10),
        legend.position = "right",
        axis.text.y = element_blank()) + labs(title = 'Cortical brain regions', caption = '*indicates p < 0.05')

brein

######Visualize significant regression######
a <- ggplot(df_final_T1, aes(sumscore_inf_tot, CerebralWhiteMatterVol_f13)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='brown2') + labs(x = 'Prenatal infection sum score', y = 'Cerebral white matter (mm)', caption = 'B = -0.069; 95% CI -0.123, 0.015; p = 0.011') + theme_classic() + theme(axis.title = element_text(size = 10))
b <- ggplot(df_final_T1, aes(sumscore_inf_tot, tot_caudalanteriorcingulate)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='blue3') + labs(x = 'Prenatal infection sum score', y = 'Caudal anterior cingulate (mm)', caption = 'B = -0.104; 95% CI -0.164, -0.054; p < 0.001') + theme_classic()+ theme(axis.title = element_text(size = 10))
c <- ggplot(df_final_T1, aes(sumscore_inf_tot, tot_entorhinal)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='blue3') + labs(x = 'Prenatal infection sum score', y = 'Entorhinal (mm)', caption = 'B = -0.062; 95% CI -0.121, -0.004; p = 0.035') + theme_classic()+ theme(axis.title = element_text(size = 10))
d <- ggplot(df_final_T1, aes(sumscore_inf_tot, tot_lateraloccipital)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='blue3') + labs(x = 'Prenatal infection sum score', y = 'Lateral occipital (mm)', caption = 'B = -0.063; 95% CI -0.120, -0.007; p = 0.027') + theme_classic()+ theme(axis.title = element_text(size = 10))
e <- ggplot(df_final_T1, aes(sumscore_inf_tot, tot_parahippocampal)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='blue3') + labs(x = 'Prenatal infection sum score', y = 'Parahippocampal (mm)', caption = 'B = -0.072; 95% CI -0.132, -0.012; p = 0.018') + theme_classic()+ theme(axis.title = element_text(size = 10))
f <- ggplot(df_final_T1, aes(sumscore_inf_tot, tot_parsorbitalis)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='blue3') + labs(x = 'Prenatal infection sum score', y = 'Pars orbitalis (mm)', caption = 'B = -0.058; 95% CI -0.117, -0.001; p = 0.047') + theme_classic()+ theme(axis.title = element_text(size = 10))
g <- ggplot(df_final_T1, aes(sumscore_inf_tot, tot_rostralmiddlefrontal)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='blue3') + labs(x = 'Prenatal infection sum score', y = 'Rostral middle frontal (mm)', caption = 'B = -0.059; 95% CI -0.113, -0.005; p = 0.030') + theme_classic()+ theme(axis.title = element_text(size = 10))
h <- ggplot(df_final_T1, aes(sumscore_inf_tot, tot_transversetemporal)) + stat_smooth(method = 'lm', formula = y~x, geom = 'smooth', color ='blue3') + labs(x = 'Prenatal infection sum score', y = 'Transverse temporal (mm)', caption = 'B = -0.068; 95% CI -0.127, -0.009; p = 0.023') + theme_classic()+ theme(axis.title = element_text(size = 10))
  
figure3 <- ggarrange(a,b, c, d, e, f, g, h, 
                       labels =c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                       ncol = 4, nrow =2)

figure3

annotate_figure(figure3, top = text_grob('Association prenatal infection and brain', color = 'black', face = 'bold', size = 14))

#\ END SCRIPT 
