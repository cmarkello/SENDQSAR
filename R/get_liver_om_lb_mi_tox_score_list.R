#' @title get_liver_om_lb_mi_tox_score_list
#'
#' @description
#' This function processes liver organ toxicity scores, body weight z-scores, and other related metrics
#' for a set of studies or XPT files. It can output individual scores, z-scores by USUBJID, or averaged scores
#' for multiple studies, and handles errors during the processing steps.
#'
#' @param studyid_or_studyids A character vector or a single study ID to process.
#' If multiple studies are provided, the function processes each study sequentially. (Mandatory)
#'
#' @param path_db A character string specifying the path to the database or directory containing the data files.
#' (Mandatory)
#'
#' @param fake_study A boolean flag indicating if the study data is simulated (`TRUE`) or real (`FALSE`). Default is `FALSE`. (Optional)
#'
#' @param use_xpt_file A boolean flag indicating whether to use an XPT file for the study data. Default is `FALSE`. (Mandatory)
#'
#' @param multiple_xpt_folder A character string specifying the path to the folder containing multiple XPT files.
#' (Optional)
#'
#' @param output_individual_scores A boolean flag indicating whether individual scores should be returned (`TRUE`) or averaged scores (`FALSE`). Default is `FALSE`. (Optional)
#'
#' @param output_zscore_by_USUBJID A boolean flag indicating whether to output z-scores by `USUBJID` (`TRUE`) or averaged scores (`FALSE`). Default is `FALSE`. (Optional)
#'
#' @return A data frame containing the calculated scores for each study. The type of result depends on the flags passed:
#' - If `output_individual_scores` is `TRUE`, a data frame with individual scores for each study is returned.
#' - If `output_zscore_by_USUBJID` is `TRUE`, a data frame with z-scores by `USUBJID` for each study is returned.
#' - If neither flag is set, the function returns a data frame with averaged scores for each study.
#'
#' @examples
#' \dontrun{
#' # Get averaged scores for a single study
#' result <- get_liver_om_lb_mi_tox_score_list(
#'   studyid_or_studyids = "Study_001",
#'   path_db = "path/to/database"
#' )
#'
#' # Get individual scores for multiple studies
#' result_individual_scores <- get_liver_om_lb_mi_tox_score_list(
#'   studyid_or_studyids = c("Study_001", "Study_002"),
#'   path_db = "path/to/database",
#'   output_individual_scores = TRUE
#' )
#' }
#'
#' @export



get_liver_om_lb_mi_tox_score_list <- function (studyid_or_studyids = FALSE,
                                               path_db,
                                               fake_study = FALSE,
                                               use_xpt_file = FALSE,
                                               output_individual_scores = FALSE,
                                               output_zscore_by_USUBJID = FALSE ) {

   # "multiple_xpt_folder" argument control the studyid/xpt folder directory

  # Enforce mutual exclusivity: If both are TRUE, throw an error
  if (output_individual_scores && output_zscore_by_USUBJID) {
    stop("Error: Both 'output_individual_scores' and 'output_zscore_by_USUBJID' cannot be TRUE at the same time.")
  }


if(output_individual_scores ) {

  # master bwzscore
  print('DEBUG FLAG1')

  # master liverToBW_df
  master_liverToBW <-  data.frame(STUDYID = NULL, avg_liverToBW_zscore = NULL)

  # Master LB list
  master_lb_score_six <- data.frame(STUDYID = NULL, avg_alb_zscore = NULL, avg_ast_zscore = NULL, avg_alp_zscore = NULL,
                                    avg_alt_zscore = NULL, avg_bili_zscore = NULL, avg_ggt_zscore = NULL)

  # master mi_df
  master_mi_df <- data.frame()

  # Initialize an empty data frame to store the names of studies with errors
  Error_studies <- list()

  # Initialize the master error data frame to have the details of the errors
  #master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)
  master_error_df <- data.frame(STUDYID = character() ,
                                Block = character(),
                                ErrorMessage = character(),
                                #Time = POSIXct(),
                                stringsAsFactors = FALSE)


} else if (output_zscore_by_USUBJID){


  master_liverToBW <- list()

  master_lb_score <- list()

  master_mi_score <- list()

  # Initialize an empty list to store the names of studies with errors
  Error_studies <- list()

  # Initialize the master error data frame to have the details of the errors
  #master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)
  master_error_df <- data.frame(STUDYID = character() ,
                                Block = character(),
                                ErrorMessage = character(),
                                #Time = POSIXct(),
                                stringsAsFactors = FALSE)



  } else {

  # Create FOUR SCORE DATA FRAME for "LiverToBodyweight" , "LB" & "MI" Score
  FOUR_Liver_Score_avg <-  data.frame(STUDYID = character(),
                                      BWZSCORE_avg = numeric(),
                                      liverToBW_avg = numeric(),
                                      LB_score_avg = numeric()
                                      , MI_score_avg = numeric())

  # Initialize an empty data frame to store the names of studies with errors
  Error_studies <- list()

  # Initialize the master error data frame to have the details of the errors
  #master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)
  master_error_df <- data.frame(STUDYID = character() ,
                                Block = character(),
                                ErrorMessage = character(),
                                #Time = POSIXct(),
                                stringsAsFactors = FALSE)
}

  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------

  # iterate over studyid or each xpt folder
#for (studyid in selected_studies){
for (studyid in studyid_or_studyids ){
  print(studyid)


    path_db <- studyid # giving the path of the xpt OR csv folder

    print(path_db)

  # Initialize a flag variable at the start of each iteration
  first_block_success <- TRUE

  # First Block with its own tryCatch for master_compiledata~~~~~~~~~~~~~~~~~~
  tryCatch({

    # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
    studyid <- if (use_xpt_file) NULL else studyid

     # if use_xpt_file = TRUE,studyid should be NULL..........................
    # Call "get_liver_compiledata" function to get the master_compiledata

    output_get_compile_data <- get_compile_data(studyid = studyid ,
                                                path_db = path_db, # problem is here
                                                fake_study = fake_study,
                                                use_xpt_file = use_xpt_file)

    #return as.data.frame "master_compiledata"
    #master_compiledata

    # GET the  "master_compiledata" -data frame- from the output of the --
    master_compiledata <- output_get_compile_data

    # Create a copy of master_compiledata for the diagnostic purpose
    master_compiledata_copy <- master_compiledata

    },
    # Handle the errors
    error = function(e) {

    # Handling errors
    message("Error in master Compilation Data  calculation: ", e$message)

    # Log the error
    if (use_xpt_file) {
      error_block1 <- data.frame(STUDYID = path_db ,
                                 Block = "compiledata",
                                 ErrorMessage = e$message,
                                 #Time = Sys.time(),
                                 stringsAsFactors = FALSE)

    } else {
    error_block1 <- data.frame(STUDYID = studyid,
                               Block = "compiledata",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)

    }

    master_error_df <<- rbind(master_error_df, error_block1)

    # Set the flag to FALSE to indicate the first block failed
    first_block_success <<- FALSE

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the flag to decide whether to proceed to the next iteration of the loop
  if (!first_block_success) {

    # Append STUDYID  to the error_studies list
    Error_studies <- c(Error_studies, studyid)

    next
  }

  #-----------------end of master_compiledata calculation----------------------

  #----------------------score_accumulation_df--------------------------------
  #This block for "Adding a new row for the current STUDYID in FOUR_Liver_Score"
  tryCatch({

    # Initialize the "FOUR_Liver_Score"
    # when  output_individual_scores == FALSE
     # && output_zscore_by_USUBJID == FALSE

    if (!output_individual_scores && !output_zscore_by_USUBJID) {
      new_row_in_four_liver_scr_avg <- data.frame(STUDYID = unique(master_compiledata$STUDYID),
                                                  BWZSCORE_avg = NA,
                                              liverToBW_avg = NA,
                                              LB_score_avg = NA,
                                              MI_score_avg = NA)

      FOUR_Liver_Score_avg <- rbind(FOUR_Liver_Score_avg, new_row_in_four_liver_scr_avg)
    }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in FOUR_Liver_Score: ", e$message)

    # Log the error
    if (use_xpt_file) {
      error_block_flscrdf <- data.frame(STUDYID = path_db,
                                        Block = "FOUR_Liver_Score",
                                        ErrorMessage = e$message,
                                        #Time = Sys.time(),
                                        stringsAsFactors = FALSE)

      } else {
    error_block_flscrdf <- data.frame(STUDYID = studyid,
                                      Block = "FOUR_Liver_Score",
                                      ErrorMessage = e$message,
                                      #Time = Sys.time(),
                                      stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block_flscrdf)

      }

  })

  #-----------------END--of ---score_accumulation_df----------------------------

  #------------------Calculation_of--BodyWeight_zScore--------------------------

  tryCatch({

    if(output_individual_scores){
     # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

    bwzscore_BW <- get_bw_score(studyid = studyid,
                                 path_db = path_db,
                                 fake_study = fake_study,
                                 use_xpt_file = use_xpt_file,
                                 master_compiledata = master_compiledata,
                                 return_individual_scores = TRUE,
                                 return_zscore_by_USUBJID = FALSE)

    # bwzscore_BW <- as.data.frame(bwzscore_BW)
    # master_bwzscore_BW <- rbind(master_bwzscore_BW, bwzscore_BW )

    } else if (output_zscore_by_USUBJID) {

      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      BW_zscore_by_USUBJID_HD <-get_bw_score(studyid = studyid,
                                             path_db = path_db,
                                             fake_study = fake_study,
                                             use_xpt_file = use_xpt_file,
                                             master_compiledata = master_compiledata,
                                             return_individual_scores = FALSE,
                                             return_zscore_by_USUBJID = TRUE)

      BW_zscore_by_USUBJID_HD <- as.data.frame(BW_zscore_by_USUBJID_HD)

    } else {

      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # need to think more and make acccordingly~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # bwzscore_BW <- get_bw_score (studyid = studyid,
      #                              path_db = path_db,
      #                              fake_study = fake_study,
      #                              use_xpt_file = use_xpt_file,
      #                              multiple_xpt_folder = multiple_xpt_folder,
      #                              master_compiledata = master_compiledata ,
      #                              return_individual_scores = FALSE,
      #                              return_zscore_by_USUBJID = FALSE)

      averaged_HD_BWzScore <- get_bw_score(studyid = studyid,
                                            path_db = path_db,
                                            fake_study = fake_study,
                                            use_xpt_file = use_xpt_file,
                                            master_compiledata = master_compiledata ,
                                            return_individual_scores = FALSE,
                                            return_zscore_by_USUBJID = FALSE)
   print(averaged_HD_BWzScore)
      # # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame............

      # Extract the liverToBW value for the current STUDYID from liverToBW_df
      calculated_BWzScore_value <-  averaged_HD_BWzScore$BWZSCORE_avg[averaged_HD_BWzScore$STUDYID == unique(master_compiledata$STUDYID)]
      #calculated_liverToBW_value <- liverToBW_df$liverToBW_avg

      # Update the liverToBW value in FOUR_Liver_Score_avg for the current STUDYID
      FOUR_Liver_Score_avg$BWZSCORE_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_BWzScore_value

    }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in BodyWeight_zScore calculation: ", e$message)

    # Log the error
    if (use_xpt_file) {
      error_block2 <- data.frame(STUDYID = path_db,
                                 Block = "BWZscore",
                                 ErrorMessage = e$message,
                                 #Time = Sys.time(),
                                 stringsAsFactors = FALSE)
    } else {

    error_block2 <- data.frame(STUDYID = studyid,
                               Block = "BWZscore",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    }
    master_error_df <<- rbind(master_error_df, error_block2)

  })

#---------------------------"OM_DATA"-(Liver_Organ to Body Weight zScore)-------
  tryCatch({


    # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
    studyid <- if (use_xpt_file) NULL else studyid

    if(output_individual_scores){
      # when, output_individual_scores == TRUE
      # bwzscore_BW  need to be calculated, so we don't need to calcualte
      # here
      HD_liver_zscore_df <- get_livertobw_score(studyid = studyid,
                                                       path_db = path_db,
                                                       fake_study = fake_study,
                                                       use_xpt_file = use_xpt_file,
                                                       master_compiledata = master_compiledata,
                                                       bwzscore_BW = bwzscore_BW,
                                                       return_individual_scores = TRUE,
                                                       return_zscore_by_USUBJID = FALSE)

      HD_liver_zscore_df <- as.data.frame(HD_liver_zscore_df)
      master_liverToBW <- rbind(master_liverToBW, HD_liver_zscore_df )

    } else if (output_zscore_by_USUBJID) {

      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      bwzscore_BW <- get_bw_score(studyid = studyid,
                                   path_db = path_db,
                                   fake_study = fake_study,
                                   use_xpt_file = use_xpt_file,
                                   master_compiledata = master_compiledata,
                                   return_individual_scores = TRUE,
                                   return_zscore_by_USUBJID = FALSE)


      liverTOBW_zscore_by_USUBJID_HD <- get_livertobw_score(studyid = studyid,
                                          path_db = path_db ,
                                          fake_study = fake_study,
                                          use_xpt_file = use_xpt_file,
                                          master_compiledata = master_compiledata,
                                          bwzscore_BW = bwzscore_BW,
                                          return_individual_scores = FALSE,
                                          return_zscore_by_USUBJID = TRUE)

      liverTOBW_zscore_by_USUBJID_HD <- as.data.frame(liverTOBW_zscore_by_USUBJID_HD)
      liverTOBW_study_identifier <- unique(liverTOBW_zscore_by_USUBJID_HD$STUDYID)

      # append to the master data frame list
      # Use the study_identifier as the list index
      master_liverToBW[[as.character(liverTOBW_study_identifier)]] <- liverTOBW_zscore_by_USUBJID_HD


      } else {

      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid


       # if (is.null(bwzscore_BW)) {
        bwzscore_BW <- get_bw_score(studyid = studyid,
                                     path_db = path_db,
                                     fake_study = fake_study,
                                     use_xpt_file = use_xpt_file,
                                     master_compiledata = master_compiledata,
                                     return_individual_scores = TRUE,
                                     return_zscore_by_USUBJID = FALSE)


      averaged_liverToBW_df <- get_livertobw_score(studyid = studyid,
                                                    path_db = path_db,
                                                    fake_study = fake_study,
                                                    use_xpt_file = use_xpt_file,
                                                    master_compiledata = master_compiledata,
                                                    bwzscore_BW = bwzscore_BW ,
                                                    return_individual_scores = FALSE,
                                                    return_zscore_by_USUBJID = FALSE)
      # {{{{...... N.B. Here is special case, if we use bwzscore_BW in else
      # condition from the previous step, it will provide
      # the  "1x2" STUDYID & avg_bwscore df but we need
      # full  bwzscore_BW. if NULL,  bwzscore_BW will be calcualted...}}}}


      # # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame................
      # Create "liverToBW_df" for FOUR_Liver_Score_avg
      liverToBW_df <- averaged_liverToBW_df  %>%
        dplyr::rename(liverToBW_avg = avg_liverToBW_zscore)

            # Extract the liverToBW value for the current STUDYID from liverToBW_df
      calculated_liverToBW_value <- liverToBW_df$liverToBW_avg[liverToBW_df$STUDYID == unique(master_compiledata$STUDYID)]
      #calculated_liverToBW_value <- liverToBW_df$liverToBW_avg
      # Update the liverToBW value in FOUR_Liver_Score_avg for the current STUDYID
      print(calculated_liverToBW_value)
      FOUR_Liver_Score_avg$liverToBW_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_liverToBW_value

      # add liverToBW_df to master_liverToBW
      #master_liverToBW <- dplyr::bind_rows(master_liverToBW, liverToBW_df)

    }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in Liver_Organ to Body Weight zScore: ", e$message)

    # Log the error
    if (use_xpt_file) {
      error_block3 <- data.frame(STUDYID = path_db,
                                 Block = "LiverToBW",
                                 ErrorMessage = e$message,
                                 #Time = Sys.time(),
                                 stringsAsFactors = FALSE)

    } else {

    error_block3 <- data.frame(STUDYID = studyid,
                               Block = "LiverToBW",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    }

    master_error_df <<- rbind(master_error_df, error_block3)

  })


  #<><><><><><><><><><><><><><><><><><>"""LB"""" zscoring <><><><><><><><><><><>
  tryCatch({
    # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
    studyid <- if (use_xpt_file) NULL else studyid

    if(output_individual_scores){
    master_lb_scores <- get_lb_score(studyid = studyid,
                                     path_db = path_db,
                                     fake_study= fake_study,
                                     use_xpt_file = use_xpt_file,
                                     master_compiledata = master_compiledata,
                                     return_individual_scores = TRUE,
                                     return_zscore_by_USUBJID = FALSE)

    print("DEBUG flag before master_lb_score_six")
    master_lb_score_six <- rbind(master_lb_score_six , master_lb_scores)
    print("DEBUG flag after master_lb_score_six")

    } else if (output_zscore_by_USUBJID) {

      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      LB_zscore_by_USUBJID_HD <- get_lb_score(studyid = studyid,
                                              path_db = path_db,
                                              fake_study= fake_study,
                                              use_xpt_file = use_xpt_file,
                                              master_compiledata = master_compiledata,
                                              return_individual_scores = FALSE,
                                              return_zscore_by_USUBJID = TRUE)

      lb_study_identifier <- unique(LB_zscore_by_USUBJID_HD$STUDYID)

      # append to the master data frame list
      # Use the study_identifier as the list index
      master_lb_score[[as.character(lb_study_identifier)]] <- LB_zscore_by_USUBJID_HD

      } else {
      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      averaged_LB_score <- get_lb_score(studyid = studyid,
                                        path_db = path_db,
                                        fake_study= fake_study,
                                        use_xpt_file = use_xpt_file ,
                                        master_compiledata = master_compiledata,
                                        return_individual_scores = FALSE,
                                        return_zscore_by_USUBJID = FALSE)

        # Append the LB_zscore to the "FOUR_Liver_Score_avg" data frame
        # Extract the LB_score value for the current STUDYID from LB_df
        calculated_LB_value <- averaged_LB_score$LB_score_avg[ averaged_LB_score$STUDYID == unique(master_compiledata$STUDYID)]

        # Update the LB_score value in FOUR_Liver_Score for the current STUDYID
        FOUR_Liver_Score_avg$LB_score_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_LB_value


        #master_lbxx_list[[j]] <- lb_score_final_list
        #master_lb_score_six <- rbind(master_lb_score_six , averaged_LB_score)

      }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in LB zscoring: ", e$message)

    # Log the error
    if (use_xpt_file) {
      error_block4 <- data.frame(STUDYID = path_db,
                                 Block = "LB",
                                 ErrorMessage = e$message,
                                 #Time = Sys.time(),
                                 stringsAsFactors = FALSE)

    } else {

    error_block4 <- data.frame(STUDYID = studyid,
                               Block = "LB",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    }
    ## DEBUG
    write.csv(master_compiledata, "/home/cjmarkello/precisionFDAassetts/Predictive_Modeling_of_Hepatotoxicity/debug_output/master_compiledata.csv")
    write.csv(master_lb_scores, "/home/cjmarkello/precisionFDAassetts/Predictive_Modeling_of_Hepatotoxicity/debug_output/master_lb_scores.csv")
    write.csv(master_lb_score_six, "/home/cjmarkello/precisionFDAassetts/Predictive_Modeling_of_Hepatotoxicity/debug_output/master_lb_score_six.csv")
    master_error_df <<- rbind(master_error_df, error_block4)
    write.csv(master_error_df, "/home/cjmarkello/precisionFDAassetts/Predictive_Modeling_of_Hepatotoxicity/debug_output/master_error_df.csv")
  })

  #<><><><><><><><><><><><><><><><><><>"""MI"""" zscoring <><><><><><><><><><><>
  tryCatch({

    #mi_score_final_list <- get_liver_mi_score(j, dbtoken, ts, master_compiledata)

    #master_mixx_list[[j]] <- mi_score_final_list

   if(output_individual_scores ){

     # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
     studyid <- if (use_xpt_file) NULL else studyid

     mi_score_final_list_df <- get_mi_score(studyid = studyid,
                                            path_db = path_db,
                                            fake_study = fake_study,
                                            use_xpt_file = use_xpt_file,
                                            master_compiledata = master_compiledata ,
                                            return_individual_scores = TRUE,
                                            return_zscore_by_USUBJID = FALSE)


     master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)

    #master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)

   } else if (output_zscore_by_USUBJID) {


     # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
     studyid <- if (use_xpt_file) NULL else studyid

     MI_score_by_USUBJID_HD <-get_mi_score(studyid = studyid,
                                           path_db = path_db,
                                           fake_study = fake_study,
                                           use_xpt_file = use_xpt_file,
                                           master_compiledata = master_compiledata ,
                                           return_individual_scores = FALSE,
                                           return_zscore_by_USUBJID = TRUE)


     mi_study_identifier <- unique(MI_score_by_USUBJID_HD$STUDYID)

     # append to the master data frame list
     # Use the study_identifier as the list index
     master_mi_score[[as.character(mi_study_identifier)]] <- MI_score_by_USUBJID_HD


   } else{

     # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
     studyid <- if (use_xpt_file) NULL else studyid

     averaged_MI_score <- get_mi_score(studyid = studyid,
                                       path_db = path_db,
                                       fake_study = fake_study,
                                       use_xpt_file = use_xpt_file,
                                       master_compiledata = master_compiledata ,
                                       return_individual_scores = FALSE,
                                       return_zscore_by_USUBJID = FALSE)

     # Append the "MI_zscore"MI_score_avg" to the "FOUR_Liver_Score_avg" data frame
     # Extract the "LB_score"MI_score_avg" value for the current STUDYID from
     calculated_MI_value <- averaged_MI_score$MI_score_avg[averaged_MI_score$STUDYID == unique(master_compiledata$STUDYID)]

     # Update the LB_score value in FOUR_Liver_Score for the current STUDYID
     FOUR_Liver_Score_avg$MI_score_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_MI_value

    }


  }, error = function(e) {
    # Handling errors of the secondary operation

    # Log the error
    if (use_xpt_file) {
      error_block5 <- data.frame(STUDYID = path_db,
                                 Block = "MI",
                                 ErrorMessage = e$message,
                                 #Time = Sys.time(),
                                 stringsAsFactors = FALSE)
      master_error_df <<- rbind(master_error_df, error_block5)

    } else {
    error_block5 <- data.frame(STUDYID = studyid,
                               Block = "MI",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block5)
    }

    # Create MI_final_score with NA values
    #return(data.frame(STUDYID = NA, avg_MI_score = NA))
  })

}

    #### print(head(master_liverToBW))

  # Print to see the dataframe
  if(output_individual_scores){
    print(head(master_lb_score_six))
    print(head(master_mi_df))

  }


  #---------------------------------------------------------------------------
  #---------------------------------------------------------------------------

  if (output_individual_scores) {

    # Function to create a dummy data frame if input is empty
    create_dummy_df <- function() {
      data.frame(
        STUDYID = NA,
        na_lb_score = NA
      )
    }

    # Check if each data frame is empty and replace if necessary
    if (nrow(master_liverToBW) == 0) {
      master_liverToBW <- create_dummy_df()
    }
    if (nrow(master_lb_score_six) == 0) {
      master_lb_score_six <- create_dummy_df()
    }
    if (nrow(master_mi_df) == 0) {
      master_mi_df <- create_dummy_df()
    }

    # Perform the merge using full_join to keep all rows from each data frame
    combined_output_individual_scores <- master_liverToBW %>%
      dplyr::full_join(master_lb_score_six, by = "STUDYID") %>%
      dplyr::full_join(master_mi_df, by = "STUDYID")

  } else if (output_zscore_by_USUBJID ) {

    combined_liverToBW <- dplyr::bind_rows(master_liverToBW)

    combined_lb_score <- dplyr::bind_rows(master_lb_score)

    combined_mi_score <- dplyr::bind_rows(master_mi_score)


    # Merge the first two data frames (df_liverToBW and df_LB) on STUDYID and USUBJID
    combined_df <-  combined_liverToBW %>%
      dplyr::full_join(combined_lb_score, by = c("STUDYID", "USUBJID"))

    # Merge the result with the third data frame (df_mi) on STUDYID and USUBJID
    final_output_zscore_by_USUBJID <- combined_df %>%
      dplyr::full_join( combined_mi_score, by = c("STUDYID", "USUBJID"))


  } else {

    FOUR_Liver_Score_avg <- FOUR_Liver_Score_avg

    # Round all columns from the second column onward to two decimal places
    FOUR_Liver_Score_avg[, 2:ncol(FOUR_Liver_Score_avg)] <- round(FOUR_Liver_Score_avg[, 2:ncol(FOUR_Liver_Score_avg)], 2)

  }

  #-------------return statement based on conditions---------------


   if (output_individual_scores) {
    # return(list(master_liverToBW = master_liverToBW,
    #           master_lb_score_six = master_lb_score_six,
    #           master_mi_df  = master_mi_df,
    #           Error_studies =  Error_studies,
    #           master_error_df = master_error_df))
     return(combined_output_individual_scores)

   } else if(output_zscore_by_USUBJID) {

     # return(list(combined_liverToBW <- combined_liverToBW,
     #
     #             combined_lb_score <- combined_lb_score ,
     #
     #             combined_mi_score <-  combined_mi_score ))

     return(final_output_zscore_by_USUBJID)
       #
       # liverTOBW_zscore_by_USUBJID_HD = master_liverToBW,
       #           LB_zscore_by_USUBJID_HD = master_lb_score,
       #           MI_score_by_USUBJID_HD  = master_mi_score,
       #           Error_studies =  Error_studies,
       #           master_error_df = master_error_df))

   } else {

   return(FOUR_Liver_Score_avg)

   }


}

