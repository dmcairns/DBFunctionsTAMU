require(DBI)
require(dbplyr)

##### Database Connection ----

createDBConnection <- function(oldServer=FALSE){
  if (oldServer == TRUE){
    hostIP <- '128.194.19.22'
  } else {
    theHost <- Sys.info()[["nodename"]]
    if(theHost == "geog-shiny.artsci.tamu.edu") {
      hostIP <- '127.0.0.1'
    } else {
      hostIP <- '128.194.46.125'
    }
  }

  DBI::dbConnect(RMySQL::MySQL(),
                    dbname = 'DH_Admin_Data',
                    host = hostIP,
                    user = Sys.getenv("DB_USER"),
                    password = Sys.getenv("DB_PASSWORD"))
}

closeAllDBConnections <- function(){
  my_driver <- DBI::dbDriver("MySQL")
  all_connections <- DBI::dbListConnections(my_driver)
  lapply(all_connections, DBI::dbDisconnect)
}

upsert_mariadb <- function(conn, local_df, target_table, unique_keys) {
  # 0. Early return if input dataframe is empty
  if (nrow(local_df) == 0) {
    message("Input data frame is empty. No operations performed.")
    return(0)
  }

  # Strip R row names attribute so DBI doesn't create extra columns
  rownames(local_df) <- NULL

  # 1. Generate a unique name for the temporary staging table
  staging_table <- paste0("tmp_staging_", as.integer(Sys.time()), "_", sample(1000:9999, 1))

  # Ensure the staging table is dropped even if the function errors out early
  on.exit({
    try(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS `%s`;", staging_table)), silent = TRUE)
  }, add = TRUE)

  # 2. Upload local data frame to temporary table
  message("Uploading local data to a temporary staging table...")
  DBI::dbWriteTable(conn, name = staging_table, value = local_df,
                    overwrite = TRUE, temporary = TRUE, row.names = FALSE)

  # 3. Build dynamic column strings and join conditions
  all_cols <- colnames(local_df)
  escaped_all_cols <- paste0('`', all_cols, '`', collapse = ", ")
  escaped_staging_cols <- paste0('s.`', all_cols, '`', collapse = ", ")

  # Create key match conditions for WHERE NOT EXISTS (e.g., t.`col1` <=> s.`col1` AND t.`col2` <=> s.`col2`)
  # Note: Uses NULL-safe equality operator (<=>) so NULL values match correctly
  join_conditions <- paste0(
    't.`', unique_keys, '` <=> s.`', unique_keys, '`',
    collapse = " AND "
  )

  # 4. Construct SQL to INSERT only rows where matching unique_keys don't already exist
  # row_names in target_table will automatically populate via default/auto_increment
  upsert_sql <- sprintf("
    INSERT INTO `%s` (%s)
    SELECT %s
    FROM `%s` s
    WHERE NOT EXISTS (
      SELECT 1
      FROM `%s` t
      WHERE %s
    );
  ", target_table, escaped_all_cols, escaped_staging_cols, staging_table, target_table, join_conditions)

  # 5. Execute query
  message("Executing remote database insert for new records...")
  rows_affected <- DBI::dbExecute(conn, upsert_sql)

  message(sprintf("Success! Processed %d local rows (%d new rows inserted) into '%s'.",
                  nrow(local_df), rows_affected, target_table))
  return(rows_affected)
}

checkForUniqueKeys <- function(inTable, oldServer=FALSE){
  # This function checks for the existence of unique keys in a database table
  dbConn <- DBFunctionsTAMU::createDBConnection(oldServer=FALSE)
  t.results <- DBI::dbGetQuery(dbConn, "SELECT TABLE_NAME, COLUMN_NAME, CONSTRAINT_NAME, REFERENCED_TABLE_NAME
                                        FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
                                        WHERE TABLE_SCHEMA = 'DH_Admin_Data'")
  DBFunctionsTAMU::closeAllDBConnections()
  if(nrow(t.results)>0){
    keys <- t.results %>%
      dplyr::filter(TABLE_NAME==inTable) %>%
      pull(COLUMN_NAME)
  }
  if (length(keys>0)) return(keys) else return(NULL)
}

#####


