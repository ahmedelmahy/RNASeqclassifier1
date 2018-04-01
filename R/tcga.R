update_counts <- function(selected_study, class){
  s <- selected_study[,which(!(is.na(class) | class == "NA"))]
  return(s)
}

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
