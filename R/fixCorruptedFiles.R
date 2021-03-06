#' Detect and Remove Corrupted Files
#'
#' @param PackageSourcesDir Path to PackageSources directory.
#' @param nlods Number of lods of your package.
#' Example: If you have files named \*_LOD00.\*, \*_LOD01.\* and \*_LOD02.\*, then nlods is 3.
#' If you have only files named \*_LOD00.\*, then set nlods = 1.
#' @param deleteTextures Whether to delete textures in modelLib/texture related to the corrupted data.
#'
#' @export
#'
#' @example
fixCorruptedFiles <- function(PackageSourcesDir, nlods, deleteTextures = TRUE) {

  modelLibDir <- file.path(PackageSourcesDir, "modelLib")

  files <- dir(modelLibDir)

  xmls <- files[stringr::str_detect(files, "xml")]

  xmlsDir <- file.path(modelLibDir, xmls)

  message("Buscando por erros em ", length(xmlsDir), " arquivos xml...")
  message("----------------------------")
  res <- lapply(xmlsDir, function(x) isValidXML(x, nlods = nlods))

  resLods <- lapply(res, function(x) x$lods)
  resBinGltf <- lapply(res, function(x) x$binGltf)
  invalidLods <- Filter(function(x) isFALSE(x[[1]][1]), resLods)
  validLods <- Filter(function(x) isTRUE(x[[1]][1]), resLods)
  invalidBinGltf <- Filter(function(x) isFALSE(x[[1]][1]), resBinGltf)
  validBinGltf <- Filter(function(x) isTRUE(x[[1]][1]), resBinGltf)

  message("----------------------------")
  message("VERIFICAÇÃO DE LODS")
  message(length(validLods), " arquivos válidos.")
  if (length(invalidBinGltf) == 0) {
    message("Não há arquivos inválidos para serem removidos.")
    #return()
  } else {
    message(length(invalidBinGltf), " arquivos inválidos.")
    message("----------------------------")
    fixLods(PackageSourcesDir, invalidLods, deleteTextures = deleteTextures)
  }


  message("----------------------------")
  message("VERIFICAÇÃO DE BINs/GLTFs")
  message(length(validBinGltf), " arquivos válidos.")
  if (length(invalidBinGltf) == 0) {
    message("Não há arquivos inválidos para serem removidos.")
    #return()
  } else {
    message(length(invalidBinGltf), " arquivos inválidos.")
    message("----------------------------")
    fixBinGltf(PackageSourcesDir, invalidBinGltf, deleteTextures = deleteTextures)
  }

}


#' Check if a object is valid
#'
#' @param xmlPath Path of the xml file.
#'
#' @return Result of validation for both lods corrupted and missing bin or gltf files.
#' @export
isValidXML <- function(xmlPath, nlods) {

  lods <- checkLods(xmlPath)
  files <- checkFilesBinGltf(xmlPath, nlods)

  return(list(lods = lods, binGltf = files))


}


