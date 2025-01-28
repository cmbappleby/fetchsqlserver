#' @title Fetch data tables
#'
#' @description
#' Fetches data tables using queries stored in .sql or .txt files. The queries
#' must be in a specific format. See documentation for more details.
#'
#' Not meant to be used on its own; it is primarily meant to only be used in
#' \code{\link{fetch_sql_server_data}}.
#'
#'
#' @param qry_path A string with the folder path to the query files
#' for fetching the data tables. If \code{NULL}, the queries will be pulled
#' from the folder named "queries" in the working directory. The working
#' directory is set when \code{\link{read_data_pkg_info()}} is ran. Or you can
#' set the working directory yourself using using \code{setwd()} or
#' \code{\link{set_pkg_dir()}}. However,
#' if the "queries" folder is not found in the working directory, the user is
#' prompted to select the folder.
#'
#' @returns A large list of data frames.
#'
#' @export
#'
fetch_data_tables <- function(qry_path) {
  db_name <- get_overall_info("db_name")

  if (is.null(qry_path)) {
    qry_path <- file.path(get_pkg_wd(), "queries")
  }

  if (!dir.exists(qry_path)) {
    msg <- paste0("The query path does not exist! The 'queries' folder ",
                  "needs to be selected.")
    svDialogs::dlg_message(msg, type = "ok")

    qry_path <- svDialogs::dlg_dir(
      default = get_pkg_wd(),
      title = "Select folder containing queries")$res
  }

  message(
    "Fetching data tables from ", db_name, " using queries from ",
    qry_path, "."
  )

  conn <- get_sql_server_conn()

  qry_files_list <- list.files(qry_path, recursive = FALSE)

  # Create an empty list to hold the tables as data frames
  data_tables_list <- list()

  for (i in seq_along(qry_files_list)) {
    qry <- readLines(file.path(qry_path, qry_files_list[i]), warn = FALSE)

    # Put the query into one line
    qry <- paste(qry, collapse = "\n")
    data_tables_list[[i]] <- DBI::dbGetQuery(conn, qry)
  }

  DBI::dbDisconnect(conn)

  message("Successfully fetched data tables from ", db_name, "!")

  return(data_tables_list)
}

#' @title Fetch attributes and foreign key references using queries
#'
#' @description
#' Get the attributes for the data tables, as well as the reference schema and
#' table for any foreign keys (RefSchemaName and RefTableName). The resulting
#' data frame column names are formatted for use with EMLeditor: attributeName
#' (column name), attributeDefinition (column description), class (data type),
#' unit (empty), dataTimeStringFormat (empty), missingValueCode (empty), and
#' missingValueCodeExplanation (empty).
#'
#' Additionally, two CSVs are saved for the user to fill out. catvar_tables.csv
#' is created using the foreign keys associated with data table attributes. The
#' CSV contains a list of lookup/ref tables with their schema and table name, as
#'  well as a Code and Description column. The Code column where the user fills
#' in the column or field name from the lookup/ref table associated with the
#' data table. The Description column is where the user fills in the column or
#' or field from the lookup/ref table that contains the description of the Code.
#'
#' This function also converts the attribute (field) data types to EML classes.
#' All attributes with a numeric field must have a unit specified in the
#' attribute table. field_unit_dict.csv contains a list of all the fields with
#' a numeric class. Under the Unit column, the user fills out the unit for that
#' field. The unit must be from the EMLassemblyline unit dictionary. This
#' dictionary can be found by running
#' \code{\link[EMLassemblyline]view_unit_dictionary()}, and it is also saved in
#' the data package info Excel spreadsheet under the units_lists sheet.
#'
#' Not meant to be used on its own; it is primarily meant to only be used in
#' \code{\link{fetch_sql_server_data}}.
#'
#' @param qry_path (optional) A string with the folder path to the query files
#' for fetching the data tables. If \code{NULL}, the queries will be pulled
#' from the folder named "queries" in the working directory. The working
#' directory is set when \code{\link{read_data_pkg_info()}} is ran. Or you can
#' set the working directory yourself using using \code{setwd()} or
#' \code{\link{set_pkg_dir()}} However,
#' if the "queries" folder is not found in the working directory, the user is
#' prompted to select the folder.
#'
#' @returns A list of data frames containing the table attributes.
#'
#' @export
#'
#' @importFrom magrittr %<>%
#'
fetch_attr_tables <- function(qry_path = NULL) {
  if (is.null(qry_path)) {
    qry_path <- file.path(get_pkg_wd(), "queries")
  }

  if (!dir.exists(qry_path)) {
    msg <- paste0("The query path does not exist! The 'queries' folder ",
                  "needs to be selected.")
    svDialogs::dlg_message(msg, type = "ok")

    qry_path <- svDialogs::dlg_dir(
      default = get_pkg_wd(),
      title = "Select folder containing queries")$res
  }

  message("Fetching attribute tables...")

  # Create an empty list to hold the attribute data frames
  attr_tbl_list <- list()

  qry_files_list <- list.files(qry_path, recursive = FALSE)

  catvar_df <- data.frame(RefSchemaName = character(),
                          RefTableName = character(),
                          stringsAsFactors = FALSE)

  # Must pass NULL to create_field_unit_csv() for the first loop
  field_unit_df <- NULL

  for (i in seq_along(qry_files_list)) {
    qry <- readLines(file.path(qry_path, qry_files_list[i]))
    stc_df <- get_stc_df(qry)
    attr_df <- get_attr_tbl(stc_df)

    catvar_df <- dplyr::bind_rows(
      catvar_df,
      attr_df[, c("RefSchemaName", "RefTableName")]
    )

    field_unit_df <- create_field_unit_df(stc_df, attr_df, field_unit_df)

    # If aliases were used, they need to be updated in the data table
    if (any(!is.na(stc_df$alias_name))) {
      alias_df <- stc_df[!is.na(stc_df$alias_name), ]
      attr_df <- update_coln_alias(attr_df, alias_df)
    }

    attr_tbl_list[[i]] <- attr_df
  }

  catvar_df %<>% dplyr::filter(!is.na(RefSchemaName))

  catvar_df%<>% dplyr::distinct()

  catvar_df %<>% dplyr::rename(Schema = RefSchemaName,
                               Table = RefTableName)
  catvar_df$Code <- NA
  catvar_df$Description <- NA

  catvar_df <- add_catvar_code_desc(catvar_df)

  pkg_wd <- get_pkg_wd()

  write.csv(catvar_df,
            file.path(pkg_wd, "catvar_tables.csv"),
            quote = FALSE,
            na = "",
            row.names = FALSE
  )

  write.csv(field_unit_df,
            file.path(pkg_wd, "field_unit_dict.csv"),
            quote = FALSE,
            na = "",
            row.names = FALSE
  )

  message("Successfully fetched attribute tables!\n",
          "catvar_tables.csv and field_unit_dict.csv saved to ", pkg_wd, "\n",
          "The catvar CSV needs to be verified before running ",
          "`create_catvar_tbls()`, and the field-unit CSV needs to be filled ",
          "out before running `add_units`.")

  return(attr_tbl_list)
}

#' @title Fetch SQL Server data
#'
#' @description
#' Fetches the data tables using the user-provided queries, the data tables'
#' attributes and foreign keys, and data table descriptions. For
#' more information on the format of the attributes and foreign keys, see
#' \code{\link{fetch_attr_tbls_fk}}. The data table descriptions come from the
#'  database extended properties. The resulting data frame is formatted to be
#' used with functions from \code{fetchsqlserver} (see documentation).
#'
#'
#' @param qry_path (optional) A string with the folder path to the query files
#' for fetching the data tables. If \code{NULL}, the queries will be pulled
#' from the folder named "queries" in the working directory. The working
#' directory is set when \code{\link{read_data_pkg_info()}} is ran. Or you can
#' set the working directory yourself using using \code{setwd()} or
#' \code{\link{set_pkg_dir()}} However,
#' if the "queries" folder is not found in the working directory, the user is
#' prompted to select the folder.
#'
#' @returns A large list consisting of [[1]]
#' data tables in data frames from 'data' schema, [[2]] attributes and
#' foreign keys for those data tables in data frames, and [[3]] a data frame
#' with table names and descriptions.
#'
#' @export
#'
fetch_sql_server_data <- function(qry_path = NULL) {
  if (is.null(qry_path)) {
    qry_path <- file.path(get_pkg_wd(), "queries")
  }

  if (!dir.exists(qry_path)) {
    msg <- paste0("The query path does not exist! The 'queries' folder ",
                  "needs to be selected.")
    svDialogs::dlg_message(msg, type = "ok")

    qry_path <- svDialogs::dlg_dir(
      default = get_pkg_wd(),
      title = "Select folder containing queries")$res
  }

  data_tables_list <- fetch_data_tables(qry_path)
  attr_list <- fetch_attr_tables(qry_path)
  tbl_desc_list <- get_tbl_desc(qry_path)

  data_list <- list(data_tables_list, attr_list, tbl_desc_list)

  return(data_list)
}

#' @title Add Code and Description to catvar_df
#'
#' @description
#' Adds the name of the first column of the catvar table to the Code column and
#' adds the name of the second column to the Description column. This is to
#' save the user time since most lookup tables are set up with the Code column
#' first and the Description column second.
#'
#'
#' @param catvar_df A data frame containing the schema and table name of lookup
#' tables for catvar tables.
#'
#' @return A data frame containing the schema and table name of lookup
#' tables for catvar tables with the first column of the table added as the
#' Code and the second column added as the Description.
#'
#' @noRd
#'
add_catvar_code_desc <- function(catvar_df) {
  conn <- get_sql_server_conn()

  for (i in seq_len(nrow(catvar_df))) {
    schema <- catvar_df$Schema[i]
    tbl <- catvar_df$Table[i]

    tbl_cols <- DBI::dbListFields(conn, DBI::Id(schema = schema, table = tbl))

    catvar_df$Code[i] <- tbl_cols[1]
    catvar_df$Description[i] <- tbl_cols[2]
  }

  DBI::dbDisconnect(conn)

  return(catvar_df)
}

#' @title Create field-unit dictionary data frame
#'
#' @description
#' Creates a data frame for the field unit dictionary, which is used to add
#' units the attributes tables. Fields with a class of 'numeric' are added to
#' the data frame. A column named 'unit' is added to the data frame. This
#' column is to be filled out by the user. \code{\link{fetch_attr_tables()}}
#' saves this data frame as a CSV.
#'
#'
#' @param stc_df A data frame of the schema, table, and column names created by
#' \code{\link{get_stc_df()}}.
#' @param attr_df A data frame of the attribute table created by
#' \code{\link{get_attr_tbl()}} before the data frame is updated with alias
#' names.
#' @param field_unit_df (optional) A data frame containing existing field-unit
#' data frame. This should only be \code{NULL} during the first loop of
#' \code{\link{fetch_attr_tables()}}.
#'
#' @returns A data frame of the field-unit dictionary, which consists of the
#' schema, table name, column name, and column name alias, along with an empty
#' 'unit' column.
#'
#' @noRd
#'
#' @importFrom magrittr %<>%
#'
create_field_unit_df <- function(stc_df, attr_df, field_unit_df=NULL) {
  stc_df$class <- attr_df$class

  stc_df %<>% dplyr::filter(class == 'numeric')

  stc_df %<>% dplyr::select(-c(class))

  if (nrow(stc_df) != 0) {
    stc_df$unit <- NA
  }

  if (is.null(field_unit_df)) {

    return(stc_df)

  } else if (nrow(stc_df) == 0) {

    return(field_unit_df)

  } else {
    field_unit_df <- dplyr::bind_rows(field_unit_df, stc_df)

    return(field_unit_df)
  }
}

#' @title Update column name(s) with alias(es)
#'
#' @param attr_df A data frame of data table attributes.
#' @param alias_df A data frame of only rows from the schema-table-column data
#' frame with aliases.
#'
#' @returns An updated attribute table data frame.
#'
#' @noRd
#'
#' @importFrom magrittr %<>%
#'
update_coln_alias <- function(attr_df, alias_df) {
  for (i in seq_len(nrow(alias_df))) {
    attr_df %<>%
      dplyr::mutate(
        attributeName = dplyr::case_when(
          attributeName == alias_df$column_name[i] ~ alias_df$alias_name[i],
          .default = attributeName
        )
      )
  }

  return(attr_df)
}
