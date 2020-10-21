#' Check .bin and .gltf Files
#'
#' @param xmlPath
#' @param nlods
#'
#' @return
#' @export
#'
#' @examples
checkFilesBinGltf <- function(xmlPath, nlods) {

  # Tests
  #xmlPath = xmlsDir[[1]]
  #nlods = 3
  #

  id <- stringr::str_sub(basename(xmlPath), end = -5)
  lods <- paste0("_LOD0", 0:(nlods-1))
  gltfDir <- file.path(dirname(xmlPath), paste0(id, lods, ".gltf"))
  binDir <- file.path(dirname(xmlPath), paste0(id, lods, ".bin"))

  # Get texture ID
  guid <- toupper(
    stringr::str_remove_all(
      xml2::xml_attr(xml2::read_xml(xmlPath), "guid"),
      pattern = "\\{|\\}"
    )
  )

  hasAllGltf <- all(sapply(gltfDir, file.exists))
  message(id, ifelse(hasAllGltf, " GLTF files valid", " GLTF files invalid"))
  hasAllBin <- all(sapply(binDir, file.exists))
  message(id, ifelse(hasAllGltf, " BIN files valid", " BIN files invalid"))

  isAllValid <- hasAllGltf && hasAllBin

  message(id, ": ", ifelse(isAllValid, "OK", "ERROR"))

  return(list(valid = isAllValid,
              xmlname = basename(xmlPath),
              binGltfnames = c(basename(gltfDir), basename(binDir)),
              guid = guid))

}

#' Check multiple .bin and .gltf files
#'
#' @param modelLibDir
#' @param nlods
#'
#' @return
#' @export
#'
#' @examples
checkAllFilesBinGltf <- function(modelLibDir, nlods) {

  files <- dir(modelLibDir)

  xmls <- files[stringr::str_detect(files, "xml")]

  xmlsDir <- file.path(modelLibDir, xmls)

  res <- lapply(xmlsDir, function(x) checkFilesBinGltf(x, nlods))
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


#' Fix missing bin/gltf
#'
#' @param PackageSourcesDir
#' @param invalids
#'
#' @return
#' @export
#'
#' @examples
fixBinGltf <- function(PackageSourcesDir, invalids) {

  # Delete invalid XML from modelLib
  message("Removendo arquivos BIN e GLTF inválidos em modelLib")
  filesToRemove <- paste0(file.path(PackageSourcesDir, "modelLib", sapply(invalids, "[[", 3)))
  xmlsToRemove <- paste0(file.path(PackageSourcesDir, "modelLib", sapply(invalids, "[[", 2)))
  status <- file.remove(c(filesToRemove, xmlsToRemove))
  message(sum(status), " arquivos removidos")
  message("----------------------------")
  #

  # Clean corrupted guids from objects.xml
  invalidGuids <- sapply(invalids, "[[", 4)
  fixObjectsXML(xmlPath = file.path(PackageSourcesDir, "scene/objects.xml"), invalidGuids = invalidGuids)

}
