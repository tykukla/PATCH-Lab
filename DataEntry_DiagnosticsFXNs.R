# --------------------------------------------- # 
# Script to read in a dataset and test that it  #
# is consistent with the stable isotope         #
# database                                      #
# ---                                           #
# T Kukla (Stanford Univ. 2021)                 #
#                                               #
# Date created: Sept 14, 2021                   #
# Last updated: Sept 14, 2021                   #
# --------------------------------------------- # 
# rm(list=ls())
# ... read in the full database for comparison
# dfx <- read.csv('Isotope-data.csv')
# 
# # ... read in the test template
# dftest <- read.csv('test_template.csv')



DATA_DIAGNOSTICS <- function(dftest, dfx){
  # --- first test whether columns are correct
  ColumnTest <- Test_DataColumns(dftest=dftest, dfx=dfx)
  ColumnTest_result <- ifelse(ColumnTest=="Pass", "PASS", "FAIL")
  ColumnTest_message <- ifelse(ColumnTest=="Pass", "", ColumnTest)
  
  # --- second test whether data types are consistent
  DataTypeTest <- Test_DataTypes(dftest=dftest, dfx=dfx)
  DataType_result <- ifelse(DataTypeTest=="Pass", "PASS", "FAIL")
  DataTypeTest_message <- ifelse(DataTypeTest=="Pass", "", DataTypeTest)
  
  # --- third test for data outliers
  OutlierTest <- Test_DataOutliers(dfx=dfx, dftest=dftest)
  OutlierTest_result <- ifelse(OutlierTest=="Pass", "No outliers", "Outlier Warning")
  OutlierTest_message <- ifelse(OutlierTest=="Pass", "", OutlierTest)
  
  # --- return the result
  OutputMat <- matrix(nrow=19, ncol=1)
  OutputMat[1,1] <- '================================================================='
  OutputMat[2,1] <- ""
  OutputMat[3,1] <- paste("Test column name consistency: Result = ", ColumnTest_result, sep='')
  OutputMat[4,1] <- ""
  OutputMat[5,1] <- ColumnTest_message
  OutputMat[6,1] <- ""
  OutputMat[7,1] <- '-----------------------------------------------------------------'
  OutputMat[8,1] <- ""
  OutputMat[9,1] <- paste("Test data type consistency: Result = ", DataType_result, sep='')
  OutputMat[10,1] <- ""
  OutputMat[11,1] <- DataTypeTest_message
  OutputMat[12,1] <- ""
  OutputMat[13,1] <- '-----------------------------------------------------------------'
  OutputMat[14,1] <- ""
  OutputMat[15,1] <- paste("Test for outlier data: Result = ", OutlierTest_result, sep='')
  OutputMat[16,1] <- ""
  OutputMat[17,1] <- OutlierTest_message
  OutputMat[18,1] <- ""
  OutputMat[19,1] <- '================================================================='

  # # -- WRITE TO DOCUMENT
  # filen <- paste('/Users/sandraschachat/Downloads/', 'TESTDIAGNOSTIC.txt', sep='')
  # fileConn <- file(filen)
  # writeLines(OutputMat, fileConn)
  # close(fileConn)
  
  return(OutputMat)
}













## --------------------------- INDIVIDUAL TEST FUNCTIONS -----------------------

## TESTS TO CONDUCT
## [1] Are the number of columns correct
##     -- if not, what's missing?
Test_DataColumns <- function(dftest=dftest, dfx=dfx){
  # are all columns equal?
  step1 <- all.equal(colnames(dfx), colnames(dftest))
  if(step1[1]==T){  
    # return result
    Result.1 = "Pass"
  } else{
    # find missing and / or extra columns
    missing_cols_idx <- which(colnames(dfx) %in% colnames(dftest))
    missing_cols <- colnames(dfx[-missing_cols_idx])
    extra_cols_idx <- which(colnames(dftest) %in% colnames(dfx))
    extra_cols <- colnames(dftest[-extra_cols_idx])
    
    # return result
    if(length(missing_cols) > 0 & length(extra_cols) > 0){
      R1a <- paste("Columns missing:", paste(missing_cols, collapse=', '), sep=' ')
      R1b <- paste("Columns extra:", paste(extra_cols, collapse=', '), sep=' ')
      suggestion <- "There are both missing and extra columns. Check that column names are identical to template. Add missing and remove extra columns."
      
      Result.1 <- paste(R1a, R1b, suggestion, sep='   ||   ')
    } else if(length(missing_cols) > 0){
      R1a <- paste("Columns missing:", paste(missing_cols, collapse=', '), sep=' ')
      suggestion <- "There are some columns missing. Check that column names are identical to template and add missing columns."
      
      Result.1 <- paste(R1a, suggestion, sep='   ||   ')
    } else if(length(extra_cols) > 0){
      R1b <- paste("Columns extra:", paste(extra_cols, collapse=', '), sep=' ')
      suggestion <- "There are too many data columns. Check that column names are identical to template and remove extra columns."
      
      Result.1 <- paste(R1b, suggestion, sep='   ||   ')
    } else{
      Result.1 <- "Data columns in wrong order. Re-order based on the template."
    }
    
    # final result
    return(Result.1)
  }
}



# ----------------------------------------------------------

# dftest <- read.csv('IsoCompilation-MAIN_R-ready.csv')

## [2] Are column data types correct
Test_DataTypes <- function(dfx=dfx, dftest=dftest){
  # only test the columns that show up in dfx and dftest
  colname_check <- which(colnames(dftest) %in% colnames(dfx))
  # remove the columns not shared
  dftest1 <- dftest[colname_check] 
  dfx1 <- dfx[colnames(dftest)[colname_check]]
  # get in the same order
  dftest2 <- dftest1[order(colnames(dftest1))]
  dfx2 <- dfx1[order(colnames(dfx1))]
  
  # find all the entry columns that are fully empty
  isempty <- vector()
  for(i in 1:ncol(dftest2)){
    temp_data <- dftest2[,i]
    na_idx <- length(which(is.na(temp_data)))
    
    isempty[i] <- ifelse(na_idx != length(temp_data), "N", "Y" )
  }
  
  # now check that the column types are the same
  coltypes_template <- sapply(dfx2, class)
  coltypes_entry <- sapply(dftest2, class)
  # there are three possible names for number (double, numeric, integer) but we treat them as one so change all to "numeric"
  coltypes_template[which(coltypes_template=="double" | coltypes_template=="numeric" | coltypes_template=="integer")] <- "numeric"
  coltypes_entry[which(coltypes_entry=="double" | coltypes_entry=="numeric" | coltypes_entry=="integer")] <- "numeric"
  # test that all are equal
  dattype_idx <- all.equal(coltypes_template, coltypes_entry)
  if(dattype_idx==T){
    Result.1 = "Pass"  
  } else{
    # find the not equal ones that are also not empty
    colmatch_idx <- coltypes_entry != coltypes_template
    mismatch_idx <- which(colmatch_idx==T & isempty=="N")
    # find the places to fix
    logic_fix <- which(coltypes_template[mismatch_idx] == "logical")
    numeric_fix <- which(coltypes_template[mismatch_idx] == "numeric")
    logic_fix_name <- colnames(dftest2)[mismatch_idx[logic_fix]]
    logic_fix_problem <- coltypes_entry[mismatch_idx[logic_fix]]
    logic_fix_answer <- coltypes_template[mismatch_idx[logic_fix]]
    numeric_fix_name <- colnames(dftest2)[mismatch_idx[numeric_fix]]
    numeric_fix_problem <- coltypes_entry[mismatch_idx[numeric_fix]]
    numeric_fix_answer <- coltypes_template[mismatch_idx[numeric_fix]]
    
    if(length(logic_fix_name) > 0 & length(numeric_fix_name) > 0){
      R1 <- "Some column data types are incompatible with template types"
      R1a <- paste(paste("Column", logic_fix_name, "has data type", logic_fix_problem, "but should be", logic_fix_answer, sep=' '), collapse= ' -- ')
      R1b <- paste(paste("Column", numeric_fix_name, "has data type", numeric_fix_problem, "but should be", numeric_fix_answer, sep=' '), collapse= ' -- ')
      R1c <- paste(R1a, R1b, sep = ' -- ')
      Result.1 <- paste(R1, R1c, sep=": ")
    } else if(length(logic_fix_name) > 0){
      R1 <- "Some column data types are incompatible with template types"
      R1a <- paste(paste("Column", logic_fix_name, "has data type", logic_fix_problem, "but should be", logic_fix_answer, sep=' '), collapse= ' -- ')
      Result.1 <- paste(R1, R1a, sep=": ")
    } else if(length(numeric_fix_name) > 0){
      R1 <- "Some column data types are incompatible with template types"
      R1b <- paste(paste("Column", numeric_fix_name, "has data type", numeric_fix_problem, "but should be", numeric_fix_answer, sep=' '), collapse= ' -- ')
      Result.1 <- paste(R1, R1b, sep=": ")
    } else{ # there are mismatches, but they aren't incompatible
      Result.1 <- "Pass"
    }
  }
  
  # ... now check individual columns
  # find Y / N columns
  Y.N_column <- vector()
  for(i in 1:ncol(dfx)){
    temp_dat <- unique(dfx[,i])
    temp_idx <- length(unique(dfx[,i]))
    if(length(temp_idx) <= 3){
      # see if values are Y and N
      YN_idx <- temp_dat %in% c("Y", "N", "")
      YN_len <- length(which(YN_idx==T))
      if(YN_len != length(YN_idx)){
        Y.N_column[i] <- FALSE
      } else{Y.N_column[i] <- TRUE}
    } else{Y.N_column[i] <- FALSE}
    # length(temp_idx) <= 2 & temp_dat %in% c("Y", "N")
  }
  YN_out <- colnames(dfx)[Y.N_column]
  YN.FAIL <- vector()
  for(j in 1:length(YN_out)){
    if(YN_out[j] %in% colnames(dftest)){
      thisYN_test <- dftest[YN_out[j]] %in% c("Y", "N", "") | length(which(!is.na(dftest[,YN_out[j]])))==0
      if(length(which(thisYN_test==FALSE)) > 0){
        YN.FAIL[j] <- TRUE
      } else{YN.FAIL[j] <- FALSE}
    } else{YN.FAIL[j] <- TRUE}
    
  }
  if(length(which(YN.FAIL)) > 0){
    R2a <- paste("Column", YN_out[which(YN.FAIL==T)], "should have values 'Y' or 'N', but I detected other values or the column is missing.")
    Result.2 <- R2a
  } else{Result.2 <- "Pass"}
  
  # Test sample type and sample material
  Sample_type_options <- unique(dfx$Sample_type)
  Sample_material_options <- unique(dfx$Sample_material)
  # does the template have only these options
  Sample_type_test <- dftest$Sample_type %in% Sample_type_options
  Sample_material_test <- dftest$Sample_material %in% Sample_material_options
  
  if(length(which(Sample_type_test==F)) > 0 & length(which(Sample_material_test==F)) > 0){
    R3 <- "Some fixed input columns have incompatible entries"
    R3a <- paste("Sample_type column does not support:", paste(dftest$Sample_type[which(Sample_type_test==F)], collapse=', '), collapse=' ' )
    R3b <- paste("Sample_material column does not support:", paste(dftest$Sample_material[which(Sample_material_test==F)], collapse=', '), collapse=' ' )
    Result.3 <- paste(R3a, R3b, sep = '  --  ')
  } else if(length(which(Sample_type_test==F)) > 0){
    R3 <- "Some fixed input columns have incompatible entries"
    R3a <- paste("Sample_type column does not support:", paste(dftest$Sample_type[which(Sample_type_test==F)], collapse=', '), collapse=' ' )
    Result.3 <- paste(R3a, sep = '  --  ')
  } else if(length(which(Sample_material_test==F)) > 0){
    R3 <- "Some fixed input columns have incompatible entries"
    R3b <- paste("Sample_material column does not support:", paste(dftest$Sample_material[which(Sample_material_test==F)], collapse=', '), collapse=' ' )
    Result.3 <- paste(R3b, sep = '  --  ')
  } else{Result.3 <- "Pass" }
  
  ALLRESULTS <- as.data.table(cbind(c(Result.1, Result.2, Result.3))) # put it all together
  PASS_idx <- which(ALLRESULTS$V1=="Pass")   # identify passes
  # if all 3 pass, return pass
  if(length(PASS_idx) == nrow(ALLRESULTS)){
    Return_result <- "Pass"
  } else{
    Return_these <- ALLRESULTS[which(ALLRESULTS$V1 != "Pass")]
    Return_result <- paste(Return_these$V1, collapse= ' -- ')
  }
  
  return(Return_result)
}





# ----------------------------------------------------------



## [3] Are isotope values proximal to existing data

Test_DataOutliers <- function(dftest=dftest, dfx=dfx){
  # if data are empty, return that
  if(nrow(dftest)==0 | all(sapply(dftest, class)=="logical")){  # test if the csv file is empty
    Result = "It seems that all of the columns might be empty. Please double check that you uploaded the correct file and not the template."
  } else{
    # find all numeric columns
    coltypes_entry <- sapply(dftest, class)
    # there are three possible names for number (double, numeric, integer) but we treat them as one so change all to "numeric"
    coltypes_entry[which(coltypes_entry=="double" | coltypes_entry=="numeric" | coltypes_entry=="integer")] <- "numeric"
    numeric_cols1 <- coltypes_entry[which(coltypes_entry=="numeric")]
    # find just the columns that are also in dfx
    dfx_dftest_numeric_cols <- colnames(dfx)[which(colnames(dfx) %in% names(numeric_cols1))]
    numeric_cols <- numeric_cols1[which(names(numeric_cols1) %in% dfx_dftest_numeric_cols)]
    
    Result_vec <- vector()
    for(i in 1:length(numeric_cols)){
      thiscolumn <- names(numeric_cols)[i]
      prior_dat <- dfx[,thiscolumn]
      dat_class <- class(prior_dat)
      if(dat_class == "logical" | dat_class == "character"){
        Result_vec[i] <- ""
      } else{
        if(thiscolumn %in% colnames(dftest)){
          test_dat <- dftest[,thiscolumn]
          percentile_values <- ecdf(prior_dat)(test_dat)
          Low_value <- percentile_values[which(percentile_values < 0.025)]
          High_value <- percentile_values[which(percentile_values > 0.975)]
          Low_idx <- which(percentile_values < 0.025)
          High_idx <- which(percentile_values > 0.975)
          
          if(length(Low_idx) > 0){
            R1a <- paste("Rows", paste(Low_idx, collapse=', '), "of column", thiscolumn, "are lower than", paste(100-(round(Low_value,4)*100), collapse=', '), "% of all data")
          } else{R1a <- ""}
          if(length(High_idx) > 0){
            R1b <- paste("Rows", paste(High_idx, collapse=', '), "of column", thiscolumn, "are higher than", paste((round(High_value,4)*100), collapse=', '), "% of all data")
          } else{R1b <- ""}
          if(nchar(R1a) == 0 & nchar(R1b) == 0){Result_vec[i] <- ""
          } else if(nchar(R1a) > 0 & nchar(R1b) == 0){Result_vec[i] <- R1a
          } else if(nchar(R1b) > 0 & nchar(R1a)== 0){Result_vec[i] <- R1b
          } else{Result_vec[i] <- paste(R1a, R1b, sep='  --  ')}
        } else{Result_vec[i] <- paste("Column", thiscolumn, "is missing. Outliers were not calculated.", sep=' ')}
      }
    }
    
    # get result vec into single vector
    R4 = "Some data are outliers. Consider double checking"
    Result_paste <- paste(Result_vec[which(nchar(Result_vec) > 0)], collapse = '  --  ')
    Result <- ifelse(nchar(Result_paste) > 0, paste(R4, Result_paste, sep= ': '), "Pass")
  }
  
  return(Result)
}




