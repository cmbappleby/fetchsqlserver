#' @title Convert attribute table data types to EML classes
#'
#' @description
#' When the attribute tables are fetched using
#' \code{\link{fetch_attr_tables()}}, the SQL Server data types for
#' each column are are stored under the 'unit' column. These data types need
#' to be converted to the appropriate EML class to produce EML metadata
#' attribute tables for the data package. \code{convert_data_type_to_class()}
#' will do this conversion. It will also add the applicable dateTimeFormatString
#'  to the date classes.
#'
#'
#' @param attr_df A data frame of the attribute table created by
#' \code{\link{fetch_attr_tables()}}
#'
#' @returns A data frame of the attribute table with the EML classes, and
#' dateTimeFormatString added, if applicable.
#'
#' @export
#'
#' @importFrom magrittr %<>%
#'
convert_data_type_to_class <- function(attr_df) {
  date_format <- get_overall_info("date_format")
  time_format <- get_overall_info("time_format")
  date_time_format <- get_overall_info("date_time_format")

  attr_df %<>% dplyr::mutate(dateTimeFormatString = dplyr::case_when(
    class == "date" ~ date_format,
    class == "time" ~ time_format,
    class == "datetime" ~ date_time_format,
    .default = dateTimeFormatString
  ))

  attr_df %<>% dplyr::mutate(class = dplyr::case_when(
    class == "int" ~ "numeric",
    class == "smallint" ~ "numeric",
    class == "tinyint" ~ "numeric",
    class == "decimal" ~ "numeric",
    class == "bigint" ~ "numeric",
    class == "real" ~ "numeric",
    class == "float" ~ "numeric",
    class == "double" ~ "numberic",
    class == "varchar" ~ "character",
    class == "char" ~ "character",
    class == "bit" ~ "character",
    class == "bool" ~ "character",
    class == "boolean" ~ "character",
    class == "date" ~ "Date",
    class == "time" ~ "Date",
    class == "datetime" ~ "Date",
    .default = class
  ))

  return(attr_df)
}

#' @title Create categorical variable tables for applicable data tables to use
#' in EML metadata creation.
#'
#' @description
#' Using the RefSchemaName and RefTableName columns from the attribute table,
#' along with catvar_tables.csv
#' (created by \code{\link{fetch_sql_server_data()}}
#' or \code{\link{fetch_attr_tables()}} and filled out by the user), this
#' function creates a catvar data
#' frames with attributeName, code, and definition columns Also, updates the
#' attribute table class to 'categorical' for all columns with catvars. The
#' catvar_tables.csv file must be populated before running this function, and it
#' must have the original file name.
#'
#'
#' @param data_list A large list from \code{\link{fetch_sql_server_data()}}
#'
#' @returns A large list containing [[1]] the data_list with the updated
#' attribute tables, [[2]] a vector with all the table names (strings) that
#' have catvar tables, and [[3]] a list of catvar data frames. Before
#' running \code{\link{save_data_pkg()}}, the data_list and catvar table names
#' list and catvar data frames list need to be extracted (see example).
#'
#' @export
#'
create_catvar_tbls <- function(data_list) {
  message("Creating categorical variable tables...")

  attr_list <- data_list[[2]]

  catvar_tbls_df <- read_catvar_tbls_csv()

  # Hold catvar table names and tables
  catvar_tbl_names <- c()
  catvar_tbl_list <- list()

  # Need an index specifically for catvar tables because in case not all data
  # tables require a catvar table.
  cv_i <- 0

  for (i in seq_along(attr_list)) {
    attr_df <- attr_list[[i]]

    if (any(!is.na(attr_df$RefSchemaName))) {
      catvar_list <- create_catvar_table(attr_df, catvar_tbls_df)

      if (!is.null(catvar_list)) {
        cv_i <- cv_i + 1
        attr_list[[i]] <- catvar_list[[1]]
        catvar_tbl_list[[cv_i]] <- catvar_list[[2]]
        catvar_tbl_names <- append(catvar_tbl_names, data_list[[3]]$dataFile[i])
      }
    }
  }

  data_list[[2]] <- attr_list

  catvar_data_list <- list(data_list, catvar_tbl_names, catvar_tbl_list)

  message("Catvar tables created!")

  return(catvar_data_list)
}

#' @title Add EML units to the attribute table
#'
#' @description
#' Use this function after \code{\link{convert_data_type_to_class()}} to add EML
#'  units to the 'numeric' classes in all the attribute tables. A dictionary is
#' used to look up the attributeName
#' and get the EML unit. If an attributeName(s) is not found in the dictionary,
#' the attributeName(s) with missing EML units will be provided to the user.
#' Also, if a unit is not a standard EML unit, a TSV file with the custom units,
#'  custom_units.txt, will be saved to the working directory. This file is
#' needed to create EML metadata.
#'
#'
#' @param data_list A large list from
#' \code{\link[fetchsqlserver]{fetch_sql_server_data()}}.
#'
#'
#' @returns A large list (data_list) with the updated attribute table(s).
#'
#' @export
#'
#'
#' @importFrom magrittr %<>%
#'
add_units <- function(data_list) {
  unit_dict <- read_field_unit_dict()

  got_cust_units <- FALSE

  while (!got_cust_units) {
    if (exists(".cust_units", envir = .GlobalEnv)) {
      cust_units_df <- get(".cust_units", envir = .GlobalEnv)
      got_cust_units <- TRUE
    } else {
      read_cust_units_sheet()
    }
  }

  cust_units <- c()

  for (i in seq_along(data_list[[2]])) {
    attr_df <- data_list[[2]][[i]]

    attr_df %<>%
      dplyr::mutate(unit = dplyr::if_else(class == "numeric",
                                          purrr::map_chr(
                                            attributeName,
                                            ~ get_unit(.x, unit_dict)
                                          ),
                                          NA_character_
      ))

    missing_units <- attr_df %>%
      dplyr::filter(class == "numeric") %>%
      dplyr::summarize(any_missing = any(is.na(unit))) %>%
      dplyr::pull(any_missing)

    if (missing_units) {
      cat(
        "Unit not found for the following attributeName(s) in",
        data_list[[3]]$dataFile[i], "table: "
      )
      missing_unit_attr <- attr_df %>%
        dplyr::filter(class == "numeric" & is.na(unit)) %>%
        select(attributeName) %>%
        pull()
      cat(missing_unit_attr, "\n")
    }

    cust_units <- append(cust_units, intersect(attr_df$unit, cust_units_df$id))

    data_list[[2]][[i]] <- attr_df
  }

  # Check for custom units because they may or may not be present
  if (length(cust_units) > 0) {
    data_pkg_folder <- get_data_pkg_folder()

    ex_cust_units <- subset(cust_units_df, id %in% cust_units)

    write.table(
      ex_cust_units,
      file = file.path(data_pkg_folder, "custom_units.txt"),
      append = FALSE,
      quote = TRUE,
      sep = "\t",
      na = "",
      row.names = FALSE,
      col.names = TRUE)

    cat("At least one of the attribute tables has custom units.",
        "custom_units.txt saved to:\n", data_pkg_folder)
  }

  return(data_list)
}

#' @title Add missing value info to attribute table or get a list of columns
#' with missing data
#'
#' @description
#' Adds generic missing value information to the attribute table if a column
#' contains NA or null values, or gets a list of columns with \code{NA} or
#' \code{NULL} values if the missing value info in the attribute table needs to
#' be customized beyond the generic missingValueCode and
#' missingValueCodeExplanation provided in the data pkg info Excel spreadsheet.
#'
#' \code{missing_values} checks to see if the missingValueCode is \code{NA} or
#' \code{NULL}. If it is, it is assumed that no custom values have been entered
#' for missingValueCode AND missingValueCodeExplanation. As a result, this
#' function can be used after customizing applicable missing value information
#' to fill in the rest with the generic info.
#'
#' @seealso \code{\link{missing_values_all_tbls()}}
#'
#'
#' @param data_df A data table data frame pulled from the large list (returned
#' from \code{\link{fetch_sql_server_data()}}) using
#' \code{\link{get_tbl_as_df()}} or another means.
#' @param attr_df (optional) A data frame of the attribute table pulled from
#' \code{\link{fetch_sql_server_data()}})
#' using \code{\link{get_tbl_as_df()}} or another means. The default is
#' \code{NULL}, and if this parameter is \code{NULL}, only a vector of column
#' names that contain \code{NA} or \code{NULL} values is returned.
#'
#'
#' @returns An attribute table data frame, if one was provided, with updated
#' missingValueCode and missingValueCodeExplanations for \code{NA} or
#' \code{NULL} columns, otherwise, a vector of column names that contain
#' \code{NA} or \code{NULL} values.
#'
#' @export
#'
#'
missing_values <- function(data_df, attr_df = NULL) {
  # Only used if there is no attr_df provided
  if (is.null(attr_df)) {
    na_columns <- c()
  }

  for (col in colnames(data_df)) {
    if (any(is.na(data_df[[col]])) || any(is.null(data_df[[col]]))) {
      if (is.null(attr_df)) {
        na_columns <- append(na_columns, col)
      } else {
        mvc <- attr_df$missingValueCode[attr_df$attributeName == col]

        # Only update missing value code & explanation if it is not present
        if (is.na(mvc) || is.null(mvc)) {
          attr_df$missingValueCode[attr_df$attributeName == col] <-
            get_overall_info("mvc")
          attr_df$missingValueCodeExplanation[attr_df$attributeName
                                                 == col] <- get_overall_info("mvce")
        }
      }
    }
  }

  if (is.null(attr_df)) {
    return(na_columns)
  } else {
    return(attr_df)
  }
}

#' @title Add missing value info to all attribute tables in the data_list
#'
#' @description
#' Adds generic missing value information to all the attribute tables in a
#' data_list if a column contains NA or null values. Uses
#' \code{\link{missing_values()}}, which checks to see if the missingValueCode
#' is \code{NA} or \code{NULL}. If it is, it is assumed that no custom values
#' have been entered for missingValueCode AND missingValueCodeExplanation. As a
#' result, this function can be used to fill in the remaining \code{NA} and
#' \code{NULL} column missing value information with the generic info after
#' customizing applicable missing value information.
#'
#' @seealso \code{\link{missing_values()}}
#'
#' @param data_list A large list from
#' \code{\link{fetch_sql_server_data()}}.
#'
#' @returns A large list (data_list) with the updated attribute table(s).
#'
#' @export
#'
missing_values_all_tbls <- function(data_list) {
  for (i in seq_along(data_list[[1]])) {
    df <- data_list[[1]][[i]]
    attr_df <- data_list[[2]][[i]]
    data_list[[2]][[i]] <- missing_values(df, attr_df)
  }

  return(data_list)
}

#' @title Save data package files
#'
#' @description
#' Saves the data tables to CSVs, the attribute and catvar tables to TSVs, and
#' saves the data names and descriptions to an XLSX (used to create EML metadata
#' ) using \code{\link{create_eml_metadata()}}. When \code{save_data_pkg()}
#' runs, the folder path selected for the data package folder will be saved as a
#'  global variable. If continuing with metadata creation, this global variable
#' will be used to read/write to the data package folder. The following should
#' be complete before running \code{save_data_pkg()}:
#' * Data tables are wrangled, flattened, cleaned, etc.
#' * Attribute data types converted to EML class using
#' \code{\link{convert_data_type_class()}}, if necessary
#' * Units added to attribute tables using
#' \code{\link{add_units()}}
#' * Missing value info added to attribute tables using
#' \code{\link{missing_values()}} and/or \code{\link{missing_values_all_tbls()}}
#' * Categorical variable tables created using
#' \code{\link{create_catvar_tbls()}}
#'
#' @param data_list A large list from
#' \code{\link[fetchsqlserver]{fetch_sql_server_data()}}.
#' @param catvar_list A large list from \code{\link{create_catvar_tbls()}}.
#'
#' @export
#'
save_data_pkg <- function(data_list, catvar_list) {
  data_pkg_folder <- get_data_pkg_folder()

  network_code <- get_overall_info("network_code")
  project_name <- get_overall_info(("project_name"))

  prefix <- paste0(network_code, "_", gsub(" ", "", project_name), "_")
  data_list[[3]]$dataFile <- paste0(prefix, data_list[[3]]$dataFile)
  catvar_list[[1]] <- paste0(prefix, catvar_list[[1]])
  message(
    "The prefix '", prefix,
    "' has been added to all the table names."
  )

  message("Saving data and attribute tables...\n")
  for (i in seq_along(data_list[[1]])) {
    data_df <- data_list[[1]][[i]]
    attr_df <- data_list[[2]][[i]]
    tbl_name <- data_list[[3]]$dataFile[i]

    data_df <- quote_comma_cols(data_df)

    # create the file name
    fn <- paste0(tbl_name, ".csv")
    # write the file
    write.csv(data_df,
              file.path(data_pkg_folder, fn),
              quote = FALSE,
              na = "",
              row.names = FALSE
    )

    attr_df <- rearrange_attr_tbl(data_df, attr_df)

    attr_df <- convert_data_type_to_class(attr_df)

    no_mvc <- check_missing_values(data_df, attr_df)

    cust_units_path <- file.path(data_pkg_folder, "custom_units.txt")

    if (!file.exists(cust_units_path)) {
      message("custom_units.txt not found. Does your data package have ",
              "custom units?")

      has_cust <- get_user_response()

      if (has_cust == "Yes") {
        cust_units_path <- svDialogs::dlg_open(
          default = data_pkg_folder,
          title = "Please select custom_units.csv",
          multiple = FALSE)
      } else {
        cust_units_path <- NULL
      }
    }

    missing_units <- check_units(attr_df, cust_units_path)

    missing_attr_def <- check_attr_def(attr_df)

    # Remove the RefSchemaName and RefTableName columns because they are not
    # part of the metadata attribute tables.
    attr_df %<>% select(-c(RefSchemaName, RefTableName))

    fn <- paste0("attributes_", tbl_name, ".txt")
    write.table(attr_df,
                file = file.path(data_pkg_folder, fn),
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                na = ""
    )

    if (!is.null(no_mvc) ||
        !is.null(missing_units) ||
        !is.null(missing_attr_def)) {
      message(tbl_name, " has the following issues:")

      if (!is.null(no_mvc)) {
        message("The following attributes do not have required missing value ",
                "information:")
        message(paste(no_mvc, collapse = ", "), "\n")
      }

      if (!is.null(missing_units)) {
        message("The following attributes are either numeric class with no ",
                "unit, or the unit is not in the unit dictionary or ",
                "custom_units.txt:")
        message(paste(missing_units, collapse = ", "), "\n")
      }

      if (!is.null(missing_attr_def)) {
        message("The attributes are missing attribute definitions:")
        message(paste(missing_attr_def, collapse = ", "), "\n")
      }
    }
  }

  message("Saving catvar tables...")
  for (i in seq_along(catvar_list[[1]])) {
    fn <- paste0("catvars_", catvar_list[[1]][i], ".txt")
    write.table(catvar_list[[2]][[i]],
                file = file.path(data_pkg_folder, fn),
                sep = "\t",
                quote = FALSE,
                row.names = FALSE,
                na = ""
    )
  }

  message("Saving data names and descriptions...")
  data_names_descs <- data_list[[3]]

  check_tbl_desc(data_names_descs)

  # Put them in alphabetical order because that's the order the files will be
  # read during metadata creation.
  data_names_descs <- data_names_descs[order(data_names_descs$dataFile,
                                             decreasing = FALSE
  ), ]

  # It's saved as an xlsx so it's not read during metadata creation
  writexl::write_xlsx(data_names_descs,
                      path = file.path(
                        data_pkg_folder,
                        "data_names_descriptions.xlsx"
                      ),
                      col_names = TRUE,
                      format_headers = FALSE,
                      use_zip64 = FALSE
  )

  message("Done! Data package files saved to:\n", data_pkg_folder, "\n\n",
          "If you received any warnings, update the appropriate files prior ",
          "metadata creation. The best way to open the .txt files for editing ",
          "is to right-click the file, go to Open with, and select Excel.")
}

#' @title Put quotes around columns with commas
#'
#' @description
#' Checks each column of the data table data frame for values with commas. If
#' any commas are found, all the values are put in double quotes. This prevents
#' columns with commas being read as multiple columns in the data table CSV.
#'
#'
#' @param data_df A data table data frame pulled from the large list (returned
#' from \code{\link{fetch_sql_server_data()}}) using
#' \code{\link{get_tbl_as_df()}} or another means.
#'
#' @returns A data frame of the data table

#' @noRd
#'
quote_comma_cols <- function(data_df) {
  for (coln in colnames(data_df)) {
    if (any(grepl(",", data_df[[coln]]))) {
      data_df[[coln]] <- shQuote(data_df[[coln]])
    }
  }

  return(data_df)
}

#' @title Check missing value information
#'
#' @description
#' Checks the data table columns for \code{NA} or \code{NULL} values and checks
#' the attribute table for missing value information. If there are \code{NA} or
#' \code{NULL} values in the data table and no missing value information in the
#' attribute table, the attributeNames/data table column names are returned.
#'
#'
#' @param data_df A data table data frame pulled from the large list (returned
#' from \code{\link{fetch_sql_server_data()}}) using
#' \code{\link{get_tbl_as_df()}} or another means.
#' @param attr_df A data frame of the attribute table pulled from
#' \code{\link{fetch_sql_server_data()}})
#' using \code{\link{get_tbl_as_df()}} or another means.
#'
#' @returns A vector of attributeNames
#'
#' @noRd
#'
check_missing_values <- function(data_df, attr_df) {
  na_cols <- missing_values(data_df)

  no_mvc <- c()

  for (attr_name in na_cols) {
    rnum <- match(attr_name, attr_df$attributeName)

    if (is.na(attr_df$missingValueCode[rnum]) ||
        is.null(is.na(attr_df$missingValueCode[rnum]))) {
      no_mvc <- c(no_mvc, attr_name)
    }

    if (is.na(attr_df$missingValueCodeExplanation[rnum]) ||
        is.null(is.na(attr_df$missingValueCodeExplanation[rnum]))) {
      no_mvc <- c(no_mvc, attr_name)
    }
  }

  if (length(no_mvc) > 0) {
    no_mvc <- unique(no_mvc)

    return(no_mvc)
  }

  return(NULL)
}

#' @title Check for attribute table units
#'
#' @description
#' Checks that all numeric EML class attributes have a unit and that all units
#' are either in the EML unit dictionary or in custom_units.csv. If a numeric
#' class attribute is missing a unit or the unit is not found in the dictionary,
#'  the attributeNames are returned.
#'
#'
#' @param attr_df A data frame of the attribute table pulled from
#' \code{\link{fetch_sql_server_data()}})
#' using \code{\link{get_tbl_as_df()}} or another means.
#' @param cust_units_path (optional) A string of the path to custom_units.csv.
#'
#' @returns A vector of attribute names
#'
#' @noRd
#'
check_units <- function(attr_df, cust_units_path = NULL) {
  missing_units <- c()

  attr_df %<>% dplyr::filter(class == "numeric")

  for (i in seq_len(nrow(attr_df))) {
    if (is.na(attr_df$unit[i]) || is.null(attr_df$unit[i])) {
      missing_units <- c(missing_units, attr_df$attributeName[i])
    }
  }

  eml_units <- EML::get_unitList()
  eml_units <- eml_units$units

  if (!is.null(cust_units_path)) {
    cust_units_df <- read.table(
      file = cust_units_path,
      header = TRUE,
      sep = "\t"
    )

    all_units <- c(eml_units$id, cust_units_df$id)
  } else {
    all_units <- eml_units
  }

  attr_df %<>% dplyr::filter(!is.na(unit))

  for (i in seq_len(nrow(attr_df))) {
    if (!(attr_df$unit[i] %in% all_units)) {
      missing_units <- c(missing_units, attr_df$attributeName[i])
    }
  }

  if (length(missing_units) > 0) {
    return(missing_units)
  }

  return(NULL)
}

#' @title Check for missing attribute definitions
#'
#' @description
#' Checks the attribute table for any missing attributeDefinitions. If there
#' are missing attributeDefinitions, the attributeNames are returned.
#'
#'
#' @param attr_df A data frame of the attribute table pulled from
#' \code{\link{fetch_sql_server_data()}})
#' using \code{\link{get_tbl_as_df()}} or another means.
#'
#' @returns A vector of attribute names
#'
#' @noRd
#'
check_attr_def <- function(attr_df) {
  attr_df %<>% dplyr::filter(is.na(attributeDefinition))

  if (nrow(attr_df) > 0) {
    missing_attr_def <- attr_df$attributeName

    return(missing_attr_def)
  }

  return(NULL)
}

#' @title Check for missing table descriptions
#'
#' @description
#' Checks the for missing data table descriptions. If any descriptions are
#' missing, a vector of table names is returned.
#'
#'
#' @param tbl_name_desc_df A data frame of the data table names and descriptions
#'
#' @noRd
#'
check_tbl_desc <- function(tbl_name_desc_df) {
  tbl_name_desc_df %<>% dplyr::filter(
    is.na(tbl_name_desc_df$dataFileDescriptions)
  )

  if (nrow(tbl_name_desc_df) >0) {
    missing_tbl_desc <- tbl_name_desc_df$dataFile

    message("The following table(s) are missing a table description:\n",
            missing_tbl_desc)
  }
}

#' @title Create categorical variable table for a data table
#'
#' @description
#' Using the RefSchemaName and RefTableName columns from the attribute table,
#' finds categorical variables for the data table and creates a catvar data
#' frame with attributeName, code, and definition columns. Code and Definition
#' columns are pulled from catvar_tables.csv, which is filled out by the user.
#' Also, updates the attribute table class to 'categorical' for all columns with
#' catvars.
#'
#'
#' @param attr_df A data frame of the attribute table pulled from the large
#' list (returned from \code{\link[fetchsqlserver]{fetch_sql_server_data()}})
#' using \code{\link{get_tbl_as_df()}}.
#' @param schema (optional) A string of the data table schema to ignore when
#' looking at foreign keys. For example, if all FK relationships between tables
#' with the schema 'data' are part of the data model and referencing lookup
#' tables, any columns with an FK to tables with the schema 'data' will be
#' ignored. The default is 'data'.
#'
#' @returns A list containing [1] the updated attribute table data frame and
#' [2] the catvar data frame
#'
#' @noRd
#'
create_catvar_table <- function(attr_df, catvar_tbls_df) {
  ref_schema_col <- attr_df$RefSchemaName
  ref_tbl_col <- attr_df$RefTableName

  # Hold catvar column data
  attr_name_catvar_col <- c()
  code_col <- c()
  def_col <- c()

  for (i in seq_along(ref_schema_col)) {
    ref_schema <- ref_schema_col[i]

    if (!is.na(ref_schema)) {
      ref_tbl <- ref_tbl_col[i]

      code_def <- catvar_tbls_df %>%
        dplyr::filter(Schema == ref_schema, Table == ref_tbl) %>%
        dplyr::select(Code, Description)

      if (nrow(code_def) == 0) {
        stop("The code and definition could not be found for ", ref_schema,
             ".", ref_tbl, " in catvar_tables.csv. Please check the CSV.")
      }

      ref_df <- get_data_table(ref_schema, ref_tbl)

      attr_name_catvar_col <- append(
        attr_name_catvar_col,
        rep(
          attr_df$attributeName[i],
          nrow(ref_df)
        )
      )

      # There should only be one row in the data frame
      code <- code_def[["Code"]][1]
      def <- code_def[["Description"]][1]

      code_col <- c(code_col, ref_df[[code]])
      def_col <- c(def_col, ref_df[[def]])
      attr_df$class[i] <- "categorical"
    }
  }

  # Only create a catvar table if there is one to create
  if (length(attr_name_catvar_col > 0)) {
    catvar_tbl <- data.frame(
      attributeName = attr_name_catvar_col,
      code = code_col,
      definition = def_col
    )
    catvar_list <- list(attr_df, catvar_tbl)
  } else {
    catvar_list <- NULL
  }

  return(catvar_list)
}

#' @title Read catvar_tables.csv
#'
#' @returns A data frame of catvar_tables.csv
#'
#' @noRd
#'
read_catvar_tbls_csv <- function() {
  pkg_wd <- get_pkg_wd()

  catvar_csv_path <- file.path(pkg_wd, "catvar_tables.csv")

  if (!file.exists(catvar_csv_path)) {
    msg <- paste0("Could not find catvar_tables.csv in the working directory. ",
                  "Please select the file...")
    svDialogs::dlg_message(msg, type = "ok")

    catvar_csv_path <- svDialogs::dlg_dir(
      default = pkg_wd,
      title = "Select catvar_tables.csv")$res
  }

  catvar_tbls_df <- read.csv(catvar_csv_path, header = TRUE, sep = ",")

  return(catvar_tbls_df)
}

#' @title Read field_unit_dict.csv
#'
#' @description
#' Reads the CSV that contains the database fields/attribute
#' names that are numeric and their associated EML units into a data frame.
#'  Used by \code{\link{add_units()}}.
#'
#'
#' @returns A data frame of the field-unit dictionary CSV
#'
#' @noRd
#'
#' @importFrom magrittr %<>%
#'
read_field_unit_dict <- function() {
  pkg_wd <- get_pkg_wd()

  dict_csv_path <- file.path(pkg_wd, "field_unit_dict.csv")

  if (!file.exists(dict_csv_path)) {
    msg <- paste0("Could not find field_unit_dict.csv in working directory. ",
                  "Please select the file...")
    svDialogs::dlg_message(msg, type = "ok")

    dict_csv_path <- svDialogs::dlg_dir(
      default = pkg_wd,
      title = "Select field_unit_dict.csv")$res
  }

  unit_dict_df <- read.csv(dict_csv_path, header = TRUE, sep = ",")

  unit_dict_df %<>%
    dplyr::mutate(column_name = ifelse(alias_name != "",
                                       alias_name, column_name))

  unit_cols <- unit_dict_df$column_name
  eml_units <- unit_dict_df$unit

  unit_dict <- hash::hash(unit_cols, eml_units)

  return(unit_dict)
}

#' @title Gets EML unit associated with field/attributeName
#'
#' @description
#' As the title implies, this function gets the EML unit from the dictionary
#' provided that is associated with the attributeName that is provided. It
#' returns a blank space to differentiate an NA from an EML unit that was not
#' found.
#'
#'
#' @param attr_name A string of the attributeName/field from the attribute
#' table.
#' @param unit_dict A hash dictionary created from
#' \code{\link{read_field_unit_dict()}}.
#'
#' @returns A string with the EML unit if the attributeName/field is found in
#' the dictionary. Otherwise, a string with one space (" ") is returned.
#'
#' @noRd
#'
get_unit <- function(attr_name, unit_dict) {
  eml_unit <- unit_dict[[attr_name]]

  # Can't return a null
  if (is.null(eml_unit)) {
    eml_unit <- NA_character_
  }

  return(eml_unit)
}
