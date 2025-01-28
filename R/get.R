#' @title Get SQL Server connection
#'
#' @description
#' Uses DBI to connect to the SQL Server.
#'
#' @returns A SQL Server database connection.
#'
#' @noRd
#'
get_sql_server_conn <- function() {
  db_name <- get_overall_info("db_name")
  server_name <- get_overall_info("server_name")

  conn <- DBI::dbConnect(odbc::odbc(),
                         Driver = "SQL Server",
                         Server = server_name,
                         Database = db_name,
                         Trusted_Connection = "Yes"
  )

  return(conn)
}

#' @title Get schema, table, column data frame from query
#'
#' @description
#' When queries are used to fetch the data tables, this function breaks down
#' the query to extract the necessary info for the schema, table, column data
#' frame that is passed to \code{get_attr_tbl}.
#'
#'
#' @param qry A string containing a data table SQL query.
#'
#' @returns A data frame with the schema, table, and column names for the data
#' table.
#'
#' @noRd
#'
#' @importFrom magrittr %<>%
#'
get_stc_df <- function(qry) {
  # Empty vectors to hold things
  prefix_list <- c()
  col_list <- c()
  schema_list <- c()
  table_list <- c()
  as_list <- c()
  alias_list <- c()

  for (i in seq_along(qry)) {
    qry_line <- qry[i]
    # Strip square brackets, commas, and tabs to make it easier to work with
    qry_line <- gsub("\\[|\\]|,|\t", "", qry_line)

    if (trimws(tolower(qry_line)) == "select") {
      next
    } else if (startsWith(tolower(qry_line), "from")) {
      schema_list <- c(schema_list, trimws(
        stringr::str_extract(qry_line, "(?i)(?<=FROM\\s)[^\\.]+")
      ))
      table_list <- c(
        table_list,
        trimws(stringr::str_extract(
          qry_line, "(?<=\\.)([^\\s]+)(?=\\s(?i)AS)"
        ))
      )

      as_list <- c(
        as_list,
        trimws(stringr::str_extract(
          qry_line, "(?i)(?<=AS\\s).*"
        ))
      )
    } else if (grepl("join", tolower(qry_line), fixed = TRUE)) {
      schema_list <- c(schema_list, trimws(
        stringr::str_extract(
          qry_line,
          "(?i)(?<=JOIN\\s)[^.]+(?=\\.)"
        )
      ))
      table_list <- c(table_list, trimws(
        stringr::str_extract(
          qry_line,
          "(?<=\\.)([^\\s]+)(?=\\s(?i)AS)"
        )
      ))
      as_list <- c(as_list, trimws(
        stringr::str_extract(qry_line, "(?i)(?<=AS\\s)[^\\s]+")
      ))
    } else if (grepl("\\.", qry_line)) {
      prefix_col <- strsplit(trimws(qry_line), "\\.")[[1]]
      prefix_list <- c(prefix_list, prefix_col[1])

      if (grepl(" AS ", prefix_col[2], ignore.case = TRUE)) {
        col_split <- strsplit(prefix_col[2], "(?i) AS ")[[1]]
        col_list <- c(col_list, trimws(col_split[1]))
        alias_list <- c(alias_list, trimws(col_split[2]))
      } else {
        alias_list <- c(alias_list, NA)
        col_list <- c(col_list, trimws(prefix_col[2]))
      }
    }
  }

  qry_parts_df <- data.frame(
    column_name = col_list,
    alias_name = alias_list,
    as = prefix_list
  )

  qry_tbls_df <- data.frame(
    as = as_list,
    schema = schema_list,
    table_name = table_list
  )

  qry_tbls_df <- dplyr::right_join(qry_tbls_df,
                                   qry_parts_df,
                                   by = dplyr::join_by(as)
  )

  qry_tbls_df %<>% dplyr::select(-as)

  return(qry_tbls_df)
}

#' @title Create query for column descriptions and FKs
#'
#' @param schema A string of the schema name.
#' @param tbl_name A string of the table name.
#' @param coln A string of the column name.
#'
#' @returns A string with the query to pull the information from the database
#' needed for the attribute table.
#'
#' @noRd
#'
create_attr_qry <- function(schema, tbl_name, coln) {
  db_name <- get_overall_info("db_name")

  qry <- paste0("
      SELECT
          C.name AS attributeName,
          E.value AS attributeDefinition,
          P.name AS class,
          SFK.name AS RefSchemaName,
          TFK.name AS RefTableName
      FROM  ", db_name, ".sys.schemas AS S
      INNER JOIN  ", db_name, ".sys.tables AS T ON S.schema_id = T.schema_id
      INNER JOIN  ", db_name, ".sys.columns AS C ON T.object_id = C.object_id
      INNER JOIN  ", db_name, ".sys.types
        AS P ON C.system_type_id = P.system_type_id
      LEFT JOIN  ", db_name, ".sys.extended_properties
        AS E ON T.object_id = E.major_id AND C.column_id = E.minor_id
      LEFT JOIN  ", db_name, ".sys.foreign_key_columns
        AS FKC ON C.column_id = FKC.parent_column_id
        AND C.object_id = FKC.parent_object_id
      LEFT JOIN  ", db_name, ".sys.foreign_keys
        AS FK ON FKC.constraint_object_id = FK.object_id
      LEFT JOIN  ", db_name, ".sys.tables
        AS TFK ON FK.referenced_object_id = TFK.object_id
      LEFT JOIN  ", db_name, ".sys.schemas
        AS SFK ON TFK.schema_id = SFK.schema_id
      WHERE S.name = '", schema, "' AND T.name = '", tbl_name, "'
        AND C.name = '", coln, "'
  ")

  return(qry)
}

#' @title Get data table attributes and foreign keys
#'
#' @description Get the attributes for a data table, as well as the reference
#' schema and table for any foreign keys (RefSchemaName and RefTableName). The
#'  resulting data frame column names are formatted for use with EMLeditor:
#' attributeName (column name), attributeDefinition (column description),
#' class (data type), unit (empty), dataTimeStringFormat (empty),
#' missingValueCode (empty), and missingValueCodeExplanation (empty).
#'
#' The attribute data frame can be modified using functions from
#' 'nccn_data_pkg_tools':
#' * \code{\link[nccndatapkgtools]{convert_data_type_to_class}}
#' * \code{\link[nccndatapkgtools]{missing_values}}
#' * \code{\link[nccndatapkgtools]{missing_values_all_tbls}}
#' * \code{\link[nccndatapkgtools]{add_units_from_attr_name}}
#'
#' @param stc_df A data frame with a row for each column of the data table with
#' the schema, table name, and column name.
#'
#' @returns A data frame.
#'
#' @noRd
#'
get_attr_tbl <- function(stc_df) {
  conn <- get_sql_server_conn()

  # Column names for metadata attribute table, along with the FK table info,
  # RefSchemaName and RefTableName, which are used to create catvar tables.
  attr_tbl_coln <- c(
    "attributeName",
    "attributeDefinition",
    "class",
    "RefSchemaName",
    "RefTableName",
    "unit",
    "dateTimeFormatString",
    "missingValueCode",
    "missingValueCodeExplanation"
  )

  attr_df <- data.frame(
    matrix(nrow = length(stc_df), ncol = length(attr_tbl_coln))
  )
  colnames(attr_df) <- attr_tbl_coln

  # Create an empty data frame for the last four columns of the attribute table,
  # otherwise, the data frame returned by the query cannot be added to the
  # attribute table data frame
  na_df <- data.frame(matrix(nrow = 1, ncol = 4))
  colnames(na_df) <- attr_tbl_coln[6:9]

  for (i in seq_len(nrow(stc_df))) {
    qry <- create_attr_qry(
      stc_df$schema[i],
      stc_df$table_name[i],
      stc_df$column_name[i]
    )

    col_attr <- DBI::dbGetQuery(conn, qry)
    col_attr_row <- dplyr::bind_cols(col_attr, na_df)
    attr_df[i, ] <- col_attr_row[1, ]
  }

  DBI::dbDisconnect(conn)

  # Remove parentheses stuff because it screws with metadata creation
  attr_df$attributeDefinition <- gsub(
    "\\(.*?\\)", "",
    attr_df$attributeDefinition
  )

  # Data types need to be converted to classes so that a field-unit dictionary
  # CSV can be created for numeric classes.
  attr_df <- convert_data_type_to_class(attr_df)

  return(attr_df)
}

#' @title Get data table descriptions from query file names
#'
#' @description
#' Get the data table descriptions from the database extended properties for
#' tables with 'data' schema. The resulting dataframe is formatted
#' to be used with \code{\link[nccndatapkgtools]{nccndatapkgtools}}.
#'
#' @param qry_path (optional) A string with the folder path to the query files
#' for fetching the data tables.
#'
#' @returns A dataframe with dataFile column (table names), and
#' dataFileDescriptions (table description).
#'
#' @noRd
#'
get_tbl_desc <- function(qry_path) {
  message("Getting data table descriptions...")

  conn <- get_sql_server_conn()
  db_name <- get_overall_info("db_name")

  qry <- paste0("
    SELECT TABLE_NAME FROM  ", db_name, ".INFORMATION_SCHEMA.TABLES
    WHERE TABLE_SCHEMA = 'data'")

  tbl_names_list <- DBI::dbGetQuery(conn, qry)

  # Declare some vectors to hold things
  table_names <- c()
  table_descs <- c()

  qry_files_list <- list.files(qry_path, recursive = FALSE)

  for (fn in qry_files_list) {
    tbl_name <- stringr::str_extract(fn, "[^.]+")

    table_names <- c(table_names, tbl_name)

    if (tbl_name %in% tbl_names_list$TABLE_NAME) {
      # create a query to get the table descriptions
      qry <- paste0("SELECT E.value As dataFileDescriptions
      FROM  ", db_name, ".sys.schemas AS S
      INNER JOIN  ", db_name, ".sys.tables AS T ON S.schema_id = T.schema_id
  	  INNER JOIN  ", db_name, ".sys.extended_properties
  	    AS E ON T.object_id = E.major_id
      WHERE minor_id = 0 AND S.name = 'data' AND T.name = '", tbl_name, "'")

      tbl_desc <- DBI::dbGetQuery(conn, qry)
      table_descs <- c(table_descs, tbl_desc[[1]])
    } else {
      table_descs <- c(table_descs, NA)

      message("No table description for ", tbl_name)
    }
  }

  tbl_desc_df <- data.frame(
    dataFile = table_names,
    dataFileDescriptions = table_descs
  )

  DBI::dbDisconnect(conn)

  message("Successfully retrieved data table descriptions!")

  return(tbl_desc_df)
}

#' @title Get a data table
#'
#' @description
#' Retrieves a data table from a database.
#'
#' Not meant to be used on its own; it is primarily meant to only be used
#' in \code{\link{fetch_sql_server_data}}.
#'
#' @param schema A string of the database table schema.
#' @param tbl_name A string of the database table name.
#'
#' @returns A data frame with the table's data.
#'
#' @noRd
#'
get_data_table <- function(schema, tbl_name) {
  conn <- get_sql_server_conn()
  db_name <- get_overall_info("db_name")

  qry <- paste0("SELECT * from ", db_name, ".", schema, ".", tbl_name)
  data_tbl <- DBI::dbGetQuery(conn, qry)

  DBI::dbDisconnect(conn)

  return(data_tbl)
}
