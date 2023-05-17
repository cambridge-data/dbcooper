## First, utilities to access a table from a connection ID.

#' Access a single table
#' 
#' @param table_name Name of the table in the database. If no table is
#' provided, returns a list of tables as a character vector.
#' @template con-id
dbc_table <- function(table_name = NULL, con_id) {
  con <- dbc_get_connection(con_id)
  
  if (is.null(table_name)) {
    return(dbc_list_tables(con))
  }
  
  return(dplyr::tbl(con, dbplyr::sql(paste("( SELECT * FROM", table_name, ")"))))
  
}


#' Run a query on a SQL database and get a remote table back
#' 
#' @param query Either a SQL query as a string, a file containing a SQL
#' query, or a YAML file with a \code{sql} parameter.
#' @template con-id
#' 
#' @return A \code{tbl_sql} with the remote results
dbc_query <- function(query, con_id) {
  dplyr::tbl(dbc_get_connection(con_id), dplyr::sql(query_from_str(query)))
}

#' Execute a query on a SQL database
#' 
#' @param query Either a SQL query as a string, a file containing a SQL
#' query, or a YAML file with a \code{sql} parameter.
#' @template con-id
dbc_execute <- function(query, con_id) {
  DBI::dbExecute(dbc_get_connection(con_id), query)
}
