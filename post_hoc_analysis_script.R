#######################################
#############Extra analysis#############
#######################################

#Project: Long-term effects of prenatal infection on the human brain: a prospective multimodal neuroimaging study (in the Generation R cohort)

#This is an extra analysis. Given our structural findings in the caudal anterior cingulate, we applied a post-hoc analysis (upon request of reviewers of journal) to investigate whether the functional connectivity of the caudal anterior cingulate cortex (calculated based on four regions of the Gordon Parcellation scheme) associates with the other 329 regions of the Gordon Parcellation scheme and the Freesurfer subcortical segmentation. 

#Author: Anna Suleri 

## set working directory and libraries 
setwd("PUT_PATH_WHERE_YOU_KEEP_THE_DATA")

libraries <- c('foreign', 'haven', 'dplyr', 'openxlsx', 'readxl', 'kableExtra', 'kable')
invisible(lapply(libraries, require, character.only = T))

id <- readRDS("sub-id_has_conMat.Rds")

## loop  over each csv file so that we have a matrix with vars we want for each sub id and save that
for (i in id) {
  path <- paste0("path_to_data", i, "path_to_data")
  #map the brain data with labels
  conmat<- read.table(path)
  diag(conmat) <- NA
  #calculate what we want 
  acc_conmat <- conmat[1:333, c(127, 144, 147, 149)]
  acc_conmat$acc <- acc_conmat$V127 + acc_conmat$V144 + acc_conmat$V147 + acc_conmat$V149 #summing up acc vars 
  df_new <- select(acc_conmat, -c(1:4)) #deleting individual vars we dont need
  df_new_mat <- matrix(df_new) #creating a matrix where i put in corerlation matrix for each sub id 
  write.xlsx(df_new_mat, file = paste0("path_to_results", i, ".xlsx")) #saving excel file results 
}

## create empty dataframe and put in all correlations for each sub id, so row is subject id for the person and column variables are the connectivity between acc and the other 333 (-4 parts of ACC) regions 
dataframe <- data.frame(rep(NA, 333))

for (i in id) {
  acc_conmat_results <- read.xlsx(paste0("path_to_results", i, ".xlsx"), 
                                  colNames = FALSE, 
                                  skipEmptyRows = FALSE)
  dataframe <- cbind(dataframe, acc_conmat_results)
}

dataframe$rep.NA..333. <- NULL

colnames(dataframe) <- id #replace with subid 

write.xlsx(dataframe, file = "path_to_results")

## now we have all the files we run the regressions 
#read in correlation matrices, id, covariates, exposure, gordon labels 
df <- read.xlsx("acc_conmat_df.xlsx", colNames = F, skipEmptyRows = F)

id <- readRDS("sub-id_has_conMat.Rds")

df_covariates <- read.csv("fmri_multimodal_imaging_df.csv") #read in fmri sample
df_covariates <- dplyr::select(df_covariates, c('IDC', 'age_child_mri_f13', 'eTIV_f13', 'GENDER', 'ETHNMv2', 'SMOKE_ALL', 'mdrink_updated', 'EDUCM_3groups', 'INCOME', 'GSI', 'AGE_M_v2', 'f1200101', 'sumscore_inf_tot', 'sumscore_inf_tri1', 'sumscore_inf_tri2', 'sumscore_inf_tri3')) #selecting only exposure and covariates 

gordon <- read.table("Gordon333_FreesurferSubcortical.32k_fs_LR.dlabel.CLUT.txt", sep = "\t")
labels <- gordon[-seq(2, 704, 2), ]
cort_gordon_labels <- labels[-c(334:352)]

#exchange rows and columns
df2 <- as.data.frame(t(df))

#align gordon names with df and add idc name to id column
colnames(df2)[1] <- "IDC"
colnames(df2)[2:334] <- cort_gordon_labels 

#extract 'sub-' from IDC column 
df2$IDC <- as.integer(gsub('sub-','', df2$IDC)) #removing strings 

#only select ids who are also in fmri imaging sample
df3 <- df2[(df2$IDC %in% df_covariates$IDC),]
df4 <- merge(df3, df_covariates, by = 'IDC', all.x = T)

#exchange character structure for numeric for vars
df4[c('GENDER', 'ETHNMv2', 'SMOKE_ALL', 'mdrink_updated', 'EDUCM_3groups', 'INCOME', 'GSI', 'AGE_M_v2', 'f1200101')] <- sapply(df4[c('GENDER', 'ETHNMv2', 'SMOKE_ALL', 'mdrink_updated', 'EDUCM_3groups', 'INCOME', 'GSI', 'AGE_M_v2', 'f1200101')], factor)
df4[c(1:336, 343:349)]<- lapply(df4[c(1:336, 343:349)], as.numeric)

write.csv(df4, 'df4.csv')

#impute covariates
missvalues <- cbind("# NA" = sort(colSums(is.na(df4))),
                    "% NA" = round(sort(colMeans(is.na(df4))) * 100, 2))

imp0 <- mice(df4, maxit = 0, defaultMethod = c("norm", "logreg", "polyreg", "polr"))
meth <- imp0$method
meth[c(1:334)] <- "" #not impute
pred <- imp0$predictorMatrix
pred[, c(1:334)] <- 0 #not use as predictor 
visSeq <- imp0$visitSequence
imp.test <- mice(df4, method = meth, predictorMatrix = pred, visitSequence = visSeq, maxit = 30, m = 30, printFlag = TRUE)

#transformations
imp.test_long <- complete(imp.test, include = T, action = "long")
imp.test_long2 <- dplyr::select(imp.test_long, -c('L_CinguloOperc_3', 'L_CinguloOperc_20', 'R_CinguloOperc_23', 'R_CinguloOperc_25')) #these 4 items from gordon parcellation scheme are used to depict cACC
imp.test2 <- as.mids(imp.test_long2)

#run linear regression for 333 connectivity correlatinos between 333 regions and acc
setwd("PUT_PATH_WHERE_YOU_WANT_THE_RESULTS")

outcomes <- colnames(dplyr::select(imp.test_long2, starts_with("L_"), starts_with('R_')))

results_fc_acc_region <- data.frame()

for (x in outcomes){
  f <- paste0(x,"~ sumscore_inf_tot + AGE_M_v2 + ETHNMv2 + age_child_mri_f13 + INCOME + GENDER + EDUCM_3groups + GSI")
  bval <- summary(pool(with(imp.test, lm(as.formula(f)))),conf.int = T)[2,2]
  seval <- summary(pool(with(imp.test, lm(as.formula(f)))),conf.int = T)[2,3]
  lowerCI <- summary(pool(with(imp.test, lm(as.formula(f)))),conf.int = T)[2,7]
  upperCI <- summary(pool(with(imp.test, lm(as.formula(f)))),conf.int = T)[2,8]
  pval <- summary(pool(with(imp.test, lm(as.formula(f)))),conf.int = T)[2,6]
  results_fc_acc_region[x,1] <- bval
  results_fc_acc_region[x,2] <- seval
  results_fc_acc_region[x,3] <- lowerCI
  results_fc_acc_region[x,4] <- upperCI
  results_fc_acc_region[x,5] <- pval
}

colnames(results_fc_acc_region) <- c("bval", "seval", "lowerCI", "upperCI", "pval")

results_fc_acc_region$sign = "" 
results_fc_acc_region$sign[results_fc_acc_region$pval < 0.05] = "*"
results_fc_acc_region$sign[results_fc_acc_region$pval < 0.01] = "**"
results_fc_acc_region$sign[results_fc_acc_region$pval < 0.001] = "***"

write.xlsx(results_fc_acc_region, "results_fc_acc_region.xlsx")

#fdr-bh
results_fc_acc_region <- read_excel("results_fc_acc_region.xlsx")

pval <- unlist(results_fc_acc_region[, 5]) #select p value column 

results_fc_acc_region$pval_adjusted <- p.adjust(pval, method = 'fdr') #fdr correction 

is.num <- sapply(results_fc_acc_region, is.numeric)
results_fc_acc_region[is.num] <- lapply(results_fc_acc_region[is.num], round, 3) #3 decimals per output 
results_fc_acc_region <- select(results_fc_acc_region, -'sign')
results_fc_acc_region <- select(results_fc_acc_region, -'seval')

cort_gordon_labels2 <- cort_gordon_labels[-c(127, 144, 147, 149)]

rownames(results_fc_acc_region)[1:329] <- cort_gordon_labels2 #add rownames 

write.xlsx(results_fc_acc_region, 'clean_fc_acc_region_results.xlsx') #excel file with results including fdr correction 

#clean up table ; and make a nice table for word file (supplemental material in manuscript)
df <- read_excel("clean_fc_acc_region_results.xlsx")
colnames <- c('Gordon parcellation region', 'beta coefficient', 'lower CI', 'upper CI', 'p-value', 'FDR p-value')
df$'Gordon parcellation region' <- df$...1
df$'beta coefficient' <- df$bval
df$'lower CI' <- df$lowerCI
df$'upper CI' <- df$upperCI
df$'p-value' <- df$pval
df$'FDR p-value' <- df$pval_adjusted

df2 <- select(df, 'Gordon parcellation region', 'beta coefficient', 'lower CI', 'upper CI', 'p-value', 'FDR p-value')

kbl(df2) %>% kable_styling(bootstrap_options = "hover", font_size = 10, fixed_thead = T, full_width = F, position = "float_left")

#\ 

