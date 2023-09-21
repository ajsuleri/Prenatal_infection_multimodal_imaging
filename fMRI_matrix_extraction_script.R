##############################################################################
########### This is the script for extracting within and between connectivity
##############################################################################

#Project: Long-term effects of prenatal infection on the human brain: a prospective multimodal neuroimaging study (in the Generation R cohort).

################ read the data #########################
# read the brain label data
setwd("PUT_PATH_WHERE_YOU_KEEP_THE_DATA")
gordon <- read.table("Gordon333_FreesurferSubcortical.32k_fs_LR.dlabel.CLUT.txt", sep = "\t")
labels <- gordon[-seq(2, 704, 2), ]
length(labels)
# clean the label names
labels <- gsub('^[LR]_', "", labels)
labels_group <- gsub("\\_.*", "",labels)
# the unique label names
labels_unique <- unique(labels_group)

################ map the data with brain ################

# the ids
id <- readRDS("sub-id_has_conMat.rds")

############ extract within and between connectivity

wit_bet_Con <- lapply(id, function(i) {
            ################ read the raw data 
            path <- paste0("path_to_the_data") # path to the data
            conMat <- read.table(path)
            
            ################ map the brain data with the labels ################
            conmat <- as.matrix(conMat)

            # we need to set diagonal to NA in order to calculate the mean of within and between connectivity 
            diag(conmat) <- NA
            colnames(conmat) <- labels_group
            rownames(conmat) <- labels_group
          
            ################ calculate the mean within and between connectivity ################
  
            # go through all the possible combinations of parcels to calculate the mean connectivity between each parcel
            between <- lapply(1:(length(labels_unique)-1), function(col){ # go through the parcels
              bet_mean <- sapply((col+1):length(labels_unique), function(row){
                mean(conmat[rownames(conmat) == labels_unique[row], 
                            colnames(conmat) == labels_unique[col]])
                
              })
            })
            # create an empty matrix to fill in within and between connectivity
            Mat <- matrix(NA,length(labels_unique),length(labels_unique))
            Mat[lower.tri(Mat, diag=F)] <- unlist(between)
            Mat[upper.tri(Mat, diag=F)] <- t(Mat)[upper.tri(Mat, diag=F)]
            
            # go through all the parcels to calculate the mean connectivity within each parcel
            con_within <- sapply(labels_unique, function(x) {mean(conmat[rownames(conmat) == x, colnames(conmat) == x], na.rm=TRUE)})
            diag(Mat) <- con_within
            
            colnames(Mat) <- labels_unique
            rownames(Mat) <- labels_unique
            
            return(Mat)
          })

saveRDS(wit_bet_Con, "within_between_fullCon_genr.rds")
