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
#' @param PackageSourcesDir Path to PackageSources directory.
#' @param invalids List with invalid files from `checkAllLods`.
#' @param deleteTextures Whether to delete textures in modelLib/texture related to the corrupted data.
#'
#' @export
#'
#' @examples
fixLods <- function(PackageSourcesDir, invalids, deleteTextures = TRUE) {

  # Delete invalid XML from modelLib
  message("Removendo arquivos XML inválidos em modelLib")
  filesToRemove <- file.path(PackageSourcesDir, "modelLib", sapply(invalids, "[[", 3))
  status <- file.remove(filesToRemove)
  message(sum(status), " arquivos XML removidos")
  message("----------------------------")

  # Delete correspondent textures
  if (isTRUE(deleteTextures)) {
    ids <- stringr::str_remove(sapply(invalids, "[[", 3), ".xml$")
    texToRemove <- list.files(file.path(PackageSourcesDir, "modelLib", "texture"),
                              paste0(ids, collapse = "|"),
                              all.files = TRUE,
                              full.names = TRUE)
    statusPNG <- file.remove(texToRemove)
    message(sum(statusPNG), " arquivos PNG (texture) removidos")
    message("----------------------------")
  }

  # Clean corrupted guids from objects.xml
  invalidGuids <- sapply(invalids, "[[", 2)
  fixObjectsXML(xmlPath = file.path(PackageSourcesDir, "scene/objects.xml"), invalidGuids = invalidGuids)

}

#' Remove lods from project
#'
#' @param PackageSourcesDir Path to PackageSources directory.
#' @param lodsToRemove
#' A (character) vector with lods to remove.
#' Example: if you wan't to remove LOD01 and LOD02, provide a vector c("01", "02").
#' @param removeBinGltf Logical. Whether to remove correspondent .bin and .gltf files.
#' @param removeTextures Logical. Whether to remove correspondent texture files.
#'
#' @export
removeLods <- function(PackageSourcesDir, lodsToRemove, removeBinGltf = TRUE, removeTextures = TRUE) {
  modelLibDir <- file.path(PackageSourcesDir, "modelLib")
  xmlFiles <- list.files(modelLibDir, pattern = ".xml$", full.names = TRUE)

  message("Removendo LODS ", paste0(lodsToRemove, collapse = ", "), " de .xmls em modelLib")
  lapply(xmlFiles, function(xml) removeLodNodesFromXML(xml, lodsToRemove))
  message(length(xmlFiles), " arquivos .xml alterados")

  if (removeBinGltf) {
    message("Removendo arquivos .bin e .gltf correspondentes em modelLib")
    removeBinGltfByLods(modelLibDir = modelLibDir, lodsToRemove = lodsToRemove)
  }

  if (removeTextures) {
    message("Removendo texturas correspondentes em modelLib/texture")
    removeModelLibTexturesByLods(textureDir = file.path(modelLibDir, "texture"),
                                 lodsToRemove = lodsToRemove)
  }
}

#' Remove lod nodes from XML
#'
#' @param xmlPath Path of the xml file of the object.
#' @param lodsToRemove
#' A (character) vector with lods to remove.
#' Example: if you wan't to remove LOD01 and LOD02, provide a vector c("01", "02").
#'
#' @export
removeLodNodesFromXML <- function(xmlPath, lodsToRemove) {
  obj <- xml2::read_xml(xmlPath)
  LibObjNodes <- xml2::xml_find_all(obj, "//LODS/LOD")
  pattern <- paste0(paste0("_LOD", lodsToRemove, ".gltf\""), collapse = "|")
  nodesToRemove <- LibObjNodes[stringr::str_detect(LibObjNodes, pattern)]
  xml2::xml_remove(nodesToRemove)
  # Write new file
  file.remove(xmlPath)
  xml2::write_xml(obj, xmlPath)
}

#' Remove .bin and .gltf by lods
#'
#' @param modelLibDir Path to modelLib directory.
#' @param lodsToRemove
#' A (character) vector with lods to remove.
#' Example: if you wan't to remove LOD01 and LOD02, provide a vector c("01", "02").
#'
#' @return
#' @export
removeBinGltfByLods <- function(modelLibDir, lodsToRemove) {
  lodsToRemoveF <- paste0("_LOD", lodsToRemove)
  pattern <- paste0(
    c(
      paste0(lodsToRemoveF, ".bin"),
      paste0(lodsToRemoveF, ".gltf")
    ),
    collapse = "|"
  )
  filesToRemove <- list.files(modelLibDir, pattern = pattern, full.names = TRUE)
  status <- file.remove(filesToRemove)
  message(sum(status), " arquivos BIN/GLTF removidos (LODS ", paste(lodsToRemove, collapse = ", "), ")")
  message("----------------------------")
}

#' Remove textures by lods
#'
#' @param textureDir Path to modelLib/texture directory.
#' @param lodsToRemove
#' A (character) vector with lods to remove.
#' Example: if you wan't to remove LOD01 and LOD02, provide a vector c("01", "02").
#'
#' @return
#' @export
removeModelLibTexturesByLods <- function(textureDir, lodsToRemove) {
  lodsToRemoveF <- paste0("_LOD", lodsToRemove, collapse = "|")
  filesToRemove <- list.files(textureDir, pattern = lodsToRemoveF, full.names = TRUE)
  status <- file.remove(filesToRemove)
  message(sum(status), " texturas removidas (LODS ", paste(lodsToRemove, collapse = ", "), ")")
  message("----------------------------")
}
