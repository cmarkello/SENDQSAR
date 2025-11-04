
get_dose_level_score <- function(studyid ,
                                 path_db,
                                 fake_study=FALSE,
                                 use_xpt_file = FALSE,
                                 master_compiledata = NULL) {

  # Get the SETCD from 'get_treatment_group_amin' function
  # Treatment group is free from TK animals and recovery animals---------------
  ## ??? Is it possible that, lowest value meaning vehicle can be removed?????????????????????
  trtm_gr_setcd_df <- get_treatment_group_amin(studyid=studyid,
                                      path_db=db_path,
                                      fake_study=fake_study,
                                      use_xpt_file=use_xpt_file)
  trtm_gr_setcds <- unique(trtm_gr_setcd_df $Treatment_SETCD )


  #---------------------------------------------------------------------
  # Retrieve the domain data
  # Define a function to query the database by domain
  fetch_domain_data <- function(db_connection, domain_name, studyid) {
    domain_name <- toupper(domain_name)
    query_statement <- paste0('SELECT * FROM ', domain_name, " WHERE STUDYID = :x")
    query_result <- DBI::dbGetQuery(db_connection, statement = query_statement, params = list(x = studyid))
    query_result
  }

  # Establish a connection to the SQLite database
  db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_db)

  # Fetch data for the 'dm' domain
  dm <- fetch_domain_data(db_connection, 'dm', studyid)

  # Fetch data for the 'ts' domain
  ts <- fetch_domain_data(db_connection, 'ts', studyid)

  # Fetch data for the 'tx' domain
  tx <- fetch_domain_data(db_connection, 'tx', studyid)

  # Fetch data for the 'lb' domain
  lb <- fetch_domain_data(db_connection, 'lb', studyid)

  # Close the database connection
  DBI::dbDisconnect(db_connection)

  #-------------------------------------------------------------------------
  # Filer the dm for the "dose_setcd " value
  filtered_dm <- dm[dm$SETCD  %in% trtm_gr_setcds, c('STUDYID','USUBJID','SPECIES','SEX','ARMCD','ARM','SETCD')]

  # get the dose level info from the tx
  trtdose_levels <- tx[tx$TXPARMCD == "TRTDOS", c("SETCD", "TXVAL")]

  #Filer the dose level for retaining the "trtm_gr_setcds" only
  final_setcds <- trtdose_levels[trtdose_levels$SETCD %in% trtm_gr_setcds, ]

  # Add "dose_level" column
  final_setcds$dose_level <- final_setcds$TXVAL

  # Find the lowest value in 'TXVAL' column
  min_dose <- min(final_setcds$dose_level, na.rm = TRUE)  # Get the minimum value, ignoring NAs

  # Assign "vehicle" to the lowest dose level
  final_setcds$dose_level[final_setcds$dose_level == min_dose] <- "vehicle"

  # get the vehicle animal


  for (setcd in trtm_gr_setcds ) {

   #-------------------------------------------------------------------
    # filter the "filtered_dm" for the current SETCD to create a
    # compile data----------------------------------------------------
    #usubjid_trtm_gr_setcds <- filtered_dm

    # process the "lb" score here
    # clean the lb data

    cleanded_lb_df <- lb[lb$USUBJID %in% filtered_dm$USUBJID, c("STUDYID",
                                                                           "USUBJID",
                                                                           "LBSPEC",
                                                                           "LBTESTCD",
                                                                            "LBSTRESN",
                                                                           "VISITDY")]

   # vehicle usubjid
    # vehicle has to be unique for each of the specimen for the particular organ
    vehicle_setcd <- final_setcds[final_setcds$dose_level == 'vehicle',"SETCD"]
    vehicle_df <- filtered_dm[filtered_dm$SETCD == vehicle_setcd,  ]

    lb_vehile_df <- cleanded_lb_df[cleanded_lb_df$USUBJID %in% vehicle_df$USUBJID, ]

    mean_vehicle_ = mean(lb_vehile_df$LBSTRESN, na.rm = TRUE)
    sd_vehicle = stats::sd(lb_vehile_df$LBSTRESN, na.rm = TRUE)



 # get the data frame for the vehicle and calculate the mean

















    vehicle_animals <-  master_compile_data[ master_compile_data$ARMCD == 'vehicle', ]
    #----------------------------------------------------------------------

    # calculate the lb score for the current setcd


    current_setcd_lb_score <- get_lb_score(studyid = studyid,
                                          path_db=path_db,
                                          fake_study= FALSE,
                                          use_xpt_file = FALSE,
                                          master_compiledata = compile_data,
                                          return_individual_scores = FALSE,
                                          return_zscore_by_USUBJID = FALSE)

    print(dim(current_setcd_lb_score))


    # calculate the score for each of the setcd animals


    # the data frame for each of the SETCD will act as a compile data

  }




}
