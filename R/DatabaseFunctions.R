require(DBI)
require(dbplyr)

##### Database Connection Functions----

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
  # 1. Generate a unique name for the temporary staging table
  staging_table <- paste0("tmp_staging_", as.integer(Sys.time()), "_", sample(1000:9999, 1))

  # Ensure the staging table is dropped even if the function errors out early
  on.exit({
    if (DBI::dbExistsTable(conn, staging_table)) {
      DBI::dbRemoveTable(conn, staging_table)
    }
  })

  # 2. Upload the local data frame to a temporary table in MariaDB
  message("Uploading local data to a temporary staging table...")
  DBI::dbWriteTable(conn, name = staging_table, value = local_df,
               overwrite = TRUE, temporary = TRUE)

  # 3. Get all column names to build the SQL dynamically
  all_cols <- colnames(local_df)
  non_key_cols <- setdiff(all_cols, unique_keys)

  escaped_all_cols <- paste0('`', all_cols, '`', collapse = ", ")
  escaped_staging_cols <- paste0('s.`', all_cols, '`', collapse = ", ")

  # Handle case where there are no non-key columns to update
  if (length(non_key_cols) > 0) {
    update_assignments <- paste0(
      '`', non_key_cols, '` = s.`', non_key_cols, '`',
      collapse = ", "
    )
  } else {
    # If no columns to update, perform a dummy update on the first key to avoid syntax error
    update_assignments <- paste0('`', unique_keys[1], '` = s.`', unique_keys[1], '`')
  }

  # 4. Construct the correct MariaDB "ON DUPLICATE KEY UPDATE" SQL string
  upsert_sql <- sprintf("
    INSERT INTO `%s` (%s)
    SELECT %s FROM `%s` s
    ON DUPLICATE KEY UPDATE %s;
  ", target_table, escaped_all_cols, escaped_staging_cols, staging_table, update_assignments)

  # 5. Execute the upsert query
  message("Executing remote database upsert...")
  rows_affected <- DBI::dbExecute(conn, upsert_sql)

  message(sprintf("Success! Processed %d rows (affected count: %d) in '%s'.", nrow(local_df), rows_affected, target_table))
  return(rows_affected)
}
#####


