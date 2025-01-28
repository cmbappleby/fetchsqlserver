#' @title Gather NPS-specific metadata information
#'
#' @description
#' Gathers the NPS-specific metadata information such as intellectual
#' rights, CUI code, park units, producing units, and DataStore reference using
#'  user inputs. After running \code{gather_nps_info()}, the next step is to run
#' \code{\link{add_nps_metadata()}}.
#'
#'
#' @param eml_info A hash dictionary created by
#' \code{\link{gather_eml_info()}} containing the base metadata needed
#'  to create EML.
#'
#' @returns A hash dictionary created by containing the base and NPS-specific
#' metadata needed to create EML.
#'
#' @export
#'
gather_nps_info <- function(eml_info) {

  cui_codes <- c('PUBLIC',
                 'FED ONLY',
                 'FEDCON',
                 'DL ONLY',
                 'NOCON')
  cat('Select the appropriate CUI code:\n')
  cui_code <- get_user_response(cui_codes)
  eml_info[['cui_code']] <- cui_code

  int_rights_list <- c('public', 'CC0', 'restricted')
  cat('Select the appropriate intellectual rights:\n')
  int_rights <- get_user_response(int_rights_list)
  eml_info[['int_rights']] <- int_rights

  cat('Does the data package have an existing DataStore reference?\n')
  ds_ref_exists <- get_user_response()

  if (ds_ref_exists == 'Yes') {
    ds_ref <- readline(prompt = "Enter the 7-digit DataStore Reference ID: ")
    suppressWarnings(ds_ref_is_num <- as.integer(ds_ref))

    while (nchar(ds_ref) != 7 || is.na(ds_ref_is_num)) {
      cat("The ID you entered was not 7 digits!\n")
      ds_ref <- readline(prompt = "Enter the 7-digit DataStore Reference ID: ")
      suppressWarnings(ds_ref_is_num <- as.integer(ds_ref))
    }

    display_response(ds_ref)

    eml_info[['ds_ref']] <- as.integer(ds_ref)
  }

  eml_info[['project_ref']] <- get_overall_info("project_ref")
  eml_info[['park_units']] <- get_overall_info("park_units")
  eml_info[['prod_units']] <- get_overall_info("producing_units")

  return(eml_info)
}

#' @title Add NPS-specific fields to base EML
#'
#' @description
#' After the base EML has been created using \code{\link{create_eml_metadata()}}
#' , this function is used to add the NPS-specific fields. The metadata can then
#'  be written to XML using \code{\link{write_eml_xml()}}.
#'
#'
#' @param eml_info A hash dictionary created by
#' \code{\link{gather_eml_info()}} and \code{\link{gather_nps_info()}}
#'  containing the metadata needed to create EML.
#' @param my_metadata A list containing metadata information such as overall
#' package details, geographic, taxonomic and temporal information, and core
#' metadata information. Also called an \code{eml_object} by
#' \code{\link[EMLeditor]{EMLeditor}}.
#'
#' @returns A list containing metadata information such as overall
#' package details, geographic, taxonomic and temporal information, core
#' metadata information, and  NPS-specific metadata. Also called an
#' \code{eml_object} by \code{EMLeditor}.
#'
#' @export
#'
create_nps_eml <- function(eml_info, my_metadata) {
  message('Validating EML...')
  # check EML for validity
  eml_valid <- check_valid_eml(my_metadata)

  if (!eml_valid) {
    stop('Cannot add NPS-specific fields to EML until the EML is valid.')
  }

  # verify there is nps info present in eml_info
  if (is.null(eml_info[['prod_units']])) {
    stop("NPS info is not present in `eml_info`, run `gather_nps_information`.")
  }

  # add CUI code to metadata
  my_metadata <- EMLeditor::set_cui_code(my_metadata, eml_info[['cui_code']])

  # add intellectual rights to metadata
  my_metadata <- EMLeditor::set_int_rights(my_metadata,
                                           eml_info[['int_rights']])

  # add DataStore project reference
  # my_metadata <- EMLeditor::set_project(my_metadata,
  #                                       eml_info[['project_ref']],
  #                                       FALSE,
  #                                       TRUE,
  #                                       TRUE)

  if (is.null(eml_info[['ds_ref']])) {
    my_metadata <- EMLeditor::set_datastore_doi(my_metadata)
  } else {
    my_metadata<- EMLeditor::set_doi(my_metadata, eml_info[['ds_ref']])
  }

  # language
  my_metadata <- EMLeditor::set_language(my_metadata, "English")

  # add park units to the metadata
  my_metadata <- EMLeditor::set_content_units(my_metadata,
                                              eml_info[['park_units']])

  # set producing units
  my_metadata <- EMLeditor::set_producing_units(my_metadata,
                                                eml_info[['prod_units']])

  # validate EML
  eml_valid <- check_valid_eml(my_metadata)

  if (!eml_valid) {
    message('Despite the issues, the input EML metadata object was modified',
            '. Use `EMLassemblyline` or `EMLeditor` functions to fix the ',
            'EML. Do not attempt to edit it by hand.')
  } else {
    cat('The input EML metadata object was modified with NPS-specific',
        'fields. Use `write_eml_xml` to write the XML file and check it.\n')
  }

  return(my_metadata)
}

#' @title Write EML metadata to XML file
#'
#' @description
#' Writes EML metadata saved as an \code{eml_object} to an XML file. If an
#' \code{eml_info} object is passed in, the \code{eml_info} object will be
#' updated with the DataStore reference. This can be useful if data package
#' creation is an iterative process.
#'
#'
#' @param my_metadata A list containing metadata information such as overall
#' package details, geographic, taxonomic and temporal information, core
#' metadata information, and  NPS-specific metadata. Also called an
#' \code{eml_object} by \code{\link[EMLeditor]{EMLeditor}}.
#' @param eml_info (optional) A hash dictionary created by containing the base
#' and NPS-specific metadata needed to create EML generated by
#' \code{\link{gather_eml_info()}} and \code{\link{gather_nps_info()}}.
#'
#' @returns A hash dictionary created by containing the base
#' and NPS-specific metadata
#'
#' @export
#'
write_eml_xml <- function(my_metadata, eml_info=NULL) {
  data_pkg_folder <- get_data_pkg_folder()
  if (!check_data_pkg_folder(data_pkg_folder)) {
    stop('The folder path ', data_pkg_folder, ' is not right. ',
         'Check the path and try again.')
  }

  # validate EML
  eml_valid <- check_valid_eml(my_metadata)

  if (!eml_valid) {
    stop('XML file not written.')
  }

  # get the metadata ID to create the package name
  metadata_id <- my_metadata[2]

  # create the file name
  xml_name <- paste0(metadata_id, ".xml")

  # write to xml
  EML::write_eml(my_metadata, xml_name)

  if (data_pkg_folder != getwd()) {
    # xml file is automatically saved to the working directory; we need to move
    #  xml file if they are not one in the same
    wd_xml <- file.path(getwd(), xml_name)
    dff_xml <- file.path(data_pkg_folder, xml_name)
    file.rename(wd_xml, dff_xml)
  }

  # check .xml
  EMLeditor::check_eml(data_pkg_folder)

  message('EML successfully saved as ', xml_name, ' in ',
          data_pkg_folder, '! The next step is to check the data package using',
          ' `DPchecker::run_congruence_checks(data_pkg_folder). Part of these ',
          'checks include ensuring all dates are within the start and end ',
          'dates used for temporal coverage. Once the data package passes ",
          "congruence checks, upload',
          ' it using `EMLeditor::upload_data_package(data_pkg_folder)`.')

  if (!is.null(eml_info)) {
    # variable to know whether to add DataStore Reference
    add_ds_ref <- 'Yes'
    # if a reference is already in eml_info, see if they want to replace it
    if (!is.null(eml_info[['ds_ref']])) {
      message('`eml_info` object already contains this DataStore Reference ID:',
              ' ', eml_info[['ds_ref']], '. Would you like to replace it?')
      add_ds_ref <- get_user_response()
    }
    if (add_ds_ref == 'Yes'){
      # read the xml as an emld object
      emld_object <- EML::read_eml(xml_name, from = 'xml')
      # get the DS reference ID
      ds_ref <- EMLeditor::get_ds_id(emld_object)
      # add to eml_info
      eml_info[['ds_ref']] <- ds_ref
      # tell the people what was done
      cat(paste0('The DataStore Reference ID ', ds_ref,
                 ', was added to the eml_info object!'))

      return(eml_info)
    }
  }

}
