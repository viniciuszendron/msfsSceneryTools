#' Check lods
#'
#' @param xmlPath
#'
#' @return
#' @export
#'
#' @examples
checkLods <- function(xmlPath) {

  obj <- xml2::read_xml(xmlPath)

  LODSnode <- xml2::xml_find_all(obj, ".//LODS")
  # Quantos LODS possui? Se 0, arquivo corrompido, caso contrário, arquivo OK
  nLods <- length(xml2::xml_children(LODSnode))

  # Get texture ID
  guid <- toupper(
    stringr::str_remove_all(
      xml2::xml_attr(obj, "guid"),
      pattern = "\\{|\\}"
    )
  )

  status <- ifelse(nLods < 1, FALSE, TRUE)

  file <- basename(xmlPath)

  message(file, ": ", ifelse(status, "OK", "ERROR"))

  return(list(valid = status, guid = guid, filename = file))

}

#' Check multiple lods
#'
#' @param modelLibDir
#'
#' @return
#' @export
#'
#' @examples
checkAllLods <- function(modelLibDir) {

  files <- dir(modelLibDir)

  xmls <- files[stringr::str_detect(files, "xml")]

  xmlsDir <- file.path(modelLibDir, xmls)

  res <- lapply(xmlsDir, function(x) checkLods(x))
  names(res) <- stringr::str_sub(basename(xmlsDir), end = -5)

  invalids <- Filter(function(x) isFALSE(x[[1]][1]), res)
  valids <- Filter(function(x) isTRUE(x[[1]][1]), res)

  message("----------------------------")
  message(length(valids), " arquivos válidos.")
  if (length(invalids) == 0) {
    message("Não há arquivos inválidos para serem removidos.")
    return(res)
  }
  message(length(invalids), " arquivos inválidos.")
  message("----------------------------")

  invisible(res)

}

#' Fix Lods
#'
#' @param PackageSourcesDir
#' @param invalids
#'
#' @return
#' @export
#'
#' @examples
fixLods <- function(PackageSourcesDir, invalids) {

  # Delete invalid XML from modelLib
  message("Removendo arquivos XML inválidos em modelLib")
  filesToRemove <- paste0(file.path(PackageSourcesDir, "modelLib", sapply(invalids, "[[", 3)))
  status <- file.remove(filesToRemove)
  message(sum(status), " arquivos removidos")
  message("----------------------------")
  #

  # Clean corrupted guids from objects.xml
  invalidGuids <- sapply(invalids, "[[", 2)
  fixObjectsXML(xmlPath = file.path(PackageSourcesDir, "scene/objects.xml"), invalidGuids = invalidGuids)

}

