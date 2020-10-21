# library(magrittr)
# dir_modelLib = "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources/modelLib"
# xmls = dir(dir_modelLib) %>% .[stringr::str_detect(., "xml")]
# count_xmls = length(xmls)
# modelLibObjs = xmls %>% stringr::str_remove_all(., stringr::fixed(".xml"))
#
# dir_scene = "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources/scene"
# objects.xml = xml2::read_xml(file.path(dir_scene, "objects.xml"))
# SceneryObjectNodes = xml2::xml_find_all(objects.xml, ".//SceneryObject")
# length(SceneryObjectNodes)
#
#
# dir_output = "D:/FSProjects/florianopolis-megapack/florianopolis-mega/Packages/msfsmaps-florianopolis-mega"
# filesTEXTURE = dir(file.path(dir_output, "scenery/msfsmaps/TEXTURE"))
# filesTEXTUREUnique = filesTEXTURE %>%
#   stringr::str_remove_all(., stringr::fixed(".PNG.DDS")) %>%
#   stringr::str_remove_all(., stringr::fixed(".json")) %>%
#   stringr::str_remove_all(., "_.*") %>%
#   unique()
#
# modelLibObjs[!modelLibObjs %in% filesTEXTUREUnique]


#install.packages("xml2")
#install.packages("stringr")


#' Detect and Remove Corrupted Files
#'
#' @param PackageSourcesDir
#'
#' @return
#' @export
#'
#' @example
fixCorruptedFiles <- function(PackageSourcesDir, nlods) {

  # Testes
  # PackageSourcesDir <- "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources"
  # PackageSourcesDir <- "D:/FSProjects/rio-megateste/PackageSources"
  # PackageSourcesDir <- "D:/FSProjects/florianopolis-megapack/florianopolis-part6/PackageSources"
  #

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
    fixLods(PackageSourcesDir, invalidLods)
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
    fixBinGltf(PackageSourcesDir, invalidBinGltf)
  }

}


#' Check if a object is valid
#'
#' @param xmlPath Path of the xml file.
#'
#' @return Result of validation for both lods corrupted and missing bin or gltf files.
#' @export
isValidXML <- function(xmlPath, nlods) {

  # Testes
  #xmlPath = "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources/modelLib/036173415260400522.xml"
  #

  lods <- checkLods(xmlPath)
  files <- checkFilesBinGltf(xmlPath, nlods)

  return(list(lods = lods, binGltf = files))


}


