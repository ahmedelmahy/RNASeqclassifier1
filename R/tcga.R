update_counts <- function(selected_study, class){
  s <- selected_study[,which(!(is.na(class) | class == "NA"))]
  return(s)
}
#
update_class <- function(class){
  class <- class[which(!(is.na(class) | class == "NA"))]
  return(as.factor(as.character(class)))
}


create_class <- function(selected_study, selected_clinical, post_class){
  clinical_id <- selected_clinical$patient.bcr_patient_barcode
  df <- data.frame(class = post_class, id = clinical_id)
  RNASeq_id <- colnames(selected_study)
  df <- df[which(df$id %in% RNASeq_id),]
  class <- df$class[match(df$id, RNASeq_id)]
  return(class)
}


selectStudy <- function(clinical_s3, s3, selected = "acc"){
  keep <- which(clinical_s3$admin.disease_code %in% selected)
  keep_code <- clinical_s3$patient.bcr_patient_barcode[keep]
  keep_row_s3 <- which(rownames(s3) %in% keep_code)
  s4 <- s3[keep_row_s3,]
  s4 <- as.matrix(as.data.frame(lapply(s4, as.numeric)))
  rownames(s4) <- rownames(s3[keep_row_s3,])
  return(t(s4))
}

selectClinicalStudy <- function(clinical_s3, selected = "acc"){
  keep <- which(clinical_s3$admin.disease_code %in% selected)
  return(clinical_s3[keep,])
}



#' @param id abbreviation of the tcga study
downloadTcgaStudy <- function(id){
  if (id == "acc"){
    # Download
    linkRNA = "http://gdac.broadinstitute.org/runs/stddata__2016_01_28/data/ACC/20160128/gdac.broadinstitute.org_ACC.Merge_rnaseqv2__illuminahiseq_rnaseqv2__unc_edu__Level_3__RSEM_genes_normalized__data.Level_3.2016012800.0.0.tar.gz"
    linkClinical = "http://gdac.broadinstitute.org/runs/stddata__2016_01_28/data/ACC/20160128/gdac.broadinstitute.org_ACC.Merge_Clinical.Level_1.2016012800.0.0.tar.gz"
  }
  else {
    return(0);
  }
  download.file(linkRNA, destfile = "RNAseqDf.tar.gz")
  download.file(linkClinical, destfile = "ClinicalDf.tar.gz")
  lRNA = untar("RNAseqDf.tar.gz",list = TRUE)
  untar("RNAseqDf.tar.gz")
  lClinical = untar("ClinicalDf.tar.gz", list = TRUE)
  untar("ClinicalDf.tar.gz")
  #----------------------------------------
  # read RNASeq
  r <- readLines(lRNA[1])
  s<- strsplit(r, split = "\t")
  s2 <- as.data.frame(s, stringsAsFactors = FALSE)
  s3 <- s2[,-2]
  geneIds <- gsub("^.*[|]","ID",as.character(s3[1,-1]))
  sampleIds <- as.character(s3[-1,1])
  s3[,1] <- NULL
  s3 <- s3[-1,]
  colnames(s3) <- geneIds
  rownames(s3) <- tolower(sampleIds)
  rownames(s3) <- sapply(rownames(s3), FUN = function(x) paste0(strsplit(x,"-")[[1]][1:3],collapse = "-"))
  rnaseqDf <<- s3
  rnaseqDf2 <- sapply(rnaseqDf, as.numeric)
  keep <- which(colSums(rnaseqDf2) > 0)
  rownames(rnaseqDf2) <- rownames(rnaseqDf)
  rnaseqDf2 <<- rnaseqDf2[,keep]
  #----------------------------------------------
  # clinical df
  clinical_r <- readLines(lClinical[1])
  clinical_s<- strsplit(clinical_r, split = "\t")
  clinical_s2 <- as.data.frame(clinical_s, stringsAsFactors = FALSE)
  clinical_s3 <- clinical_s2[,-1]
  colnames(clinical_s3) <- clinical_s3[1,]
  clinical_s3 <- clinical_s3[-1,]
  clinicalDf <<- clinical_s3

  keep <- which(sapply(clinicalDf, FUN = function(x){
    sum(x == "NA") < (length(x)-10)
  }))
  keepPatient = which(clinicalDf$patient.bcr_patient_barcode %in%  rownames(rnaseqDf2))
  clinicalDf2 <<- clinicalDf[keepPatient,keep]
  clinicalDf3 <<- clinicalDf2[order(match(clinicalDf2$patient.bcr_patient_barcode,rownames(rnaseqDf2))),]
  #-----------------------------------------------
}


