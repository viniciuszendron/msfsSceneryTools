#' Update Scenery Files
#'
#' Delete scenery files across the project that are no longer registered in scene/objects.xml.
#' It can also remove .PNG.DDS and .PNG.DDS.json files located inside of a TEXTURE folder of an already built project by providing the TEXTURE path in `deleteBuiltTextures` argument.
#'
#' @param xmlPath Path to objects.xml file.
#' @param modelLibDir Path to modelLib directory.
#' If NULL/not declared, will search for default location.
#' @param deleteBuiltTextures If desired, pass path of TEXTURE directory in built package to delete textures already built.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' updateSceneryFiles("D:/FSProjects/my-package/PackageSources/scene/objects.xml")
#' updateSceneryFiles("D:/FSProjects/my-package/PackageSources/scene/objects.xml", deleteBuiltTextures = "D:/FSProjects/my-package/Packages/my-package/scenery/my-company/TEXTURE")
#' }
updateSceneryFiles <- function(xmlPath, modelLibDir = NULL, deleteBuiltTextures) {

  # Tests
  # xmlPath <- "D:/FSProjects/florianopolis-part2/PackageSources/scene/objects.xml"
  # modelLibDir <- "D:/FSProjects/florianopolis-part2/PackageSources/modelLib"
  #

  baseName <- basename(xmlPath)
  if (!baseName == "objects.xml") stop("Must be the objects.xml. Usually located inside PackageSources/scene.")

  if (!missing(deleteBuiltTextures)) stopifnot(inherits(deleteBuiltTextures, "character"))

  if (is.null(modelLibDir)) {
    if (!basename(dirname(xmlPath)) == "scene") stop("Could not guess the right location of modelLib, please provide modelLibDir explicitly.")
    modelLibDir <- stringr::str_replace_all(xmlPath, paste0("scene/", baseName), "modelLib")
  }
  textureDir <- file.path(modelLibDir, "texture")

  # Get all valid GUIDS from scene/objects.xml
  guidsObjectsXML <- objectsXmlGuids(xmlPath)

  # Open all .xml in modelLib and match GUIDS with number IDs
  modelLibXmls <- list.files(modelLibDir, pattern = ".xml$", full.names = TRUE)
  guidsModelLibXMLs <- sapply(modelLibXmls, getXmlGuid, USE.NAMES = FALSE)

  # Create a vector with files to delete
  namesToRemoveModelLib <- guidsModelLibXMLs[!guidsModelLibXMLs %in% guidsObjectsXML]
  namesToRemoveModelLib <- names(namesToRemoveModelLib)
  filesToRemoveModelLib <- list.files(modelLibDir, pattern = paste0(namesToRemoveModelLib, collapse = "|"), full.names = TRUE)
  filesToRemoveModelLibTex <- list.files(textureDir, pattern = paste0(namesToRemoveModelLib, collapse = "|"), full.names = TRUE)
  # Find files to remove (.DDS, .json)
  filesToRemoveBuiltTex <- list.files(deleteBuiltTextures, pattern = paste0(namesToRemoveModelLib, collapse = "|"), full.names = TRUE)
  if (length(namesToRemoveModelLib) == 0) {
    if (length(filesToRemoveModelLibTex) == 0) {
      message("Scenery files are up to date with objects.xml. Nothing to remove.")
      return(invisible())
    } else {
      # Ask yes/no confirmation question
      res <- try(askYesNo(paste0("Are you sure you want to delete all objects with the following name(s)?\n", paste0(namesToRemoveModelLib, collapse = "\n"))))
      if (inherits(res, "try-error")) res <- try(askYesNo("Are you sure you want to delete all objects not registered in objects.xml?"))
      if (!inherits(res, "try-error") & !isTRUE(res)) {
        message("Operation aborted.")
        return(invisible())
      }
      # Delete all remaining .xml, .gltf and .bin inside modelLibDir
      message("Removing .PNG.DDS/.PNG.json texture files in TEXTURE directory")
      statusDBT <- file.remove(filesToRemoveBuiltTex)
      message(sum(statusDBT), " .PNG.DDS/.PNG.json files (TEXTURE built) removed")
      message("----------------------------")
      return(invisible())
    }

  }

  # Ask yes/no confirmation question
  res <- try(askYesNo(paste0("Are you sure you want to delete all objects with the following name(s)?\n", paste0(namesToRemoveModelLib, collapse = "\n"))))
  if (inherits(res, "try-error")) res <- try(askYesNo("Are you sure you want to delete all objects not registered in objects.xml?"))
  if (!inherits(res, "try-error") & !isTRUE(res)) {
    message("Operation aborted.")
    return(invisible())
  }

  # Delete all remaining .xml, .gltf and .bin inside modelLibDir
  message("Removing .xml/.gltf/.bin files in modelLib")
  statusML <- file.remove(filesToRemoveModelLib)
  message(sum(statusML), " .xml/.gltf/.bin files (texture) removed")
  message("----------------------------")

  # Delete all textures (.png) inside textureDir
  message("Removing .png texture files in modelLib")
  statusPNG <- file.remove(filesToRemoveModelLibTex)
  message(sum(statusPNG), " .png files (texture) removed")
  message("----------------------------")

  if (!missing(deleteBuiltTextures)) {

    # Find files to remove (.DDS, .json)
    filesToRemoveBuiltTex <- list.files(deleteBuiltTextures, pattern = paste0(namesToRemoveModelLib, collapse = "|"), full.names = TRUE)
    # Delete all remaining .xml, .gltf and .bin inside modelLibDir
    message("Removing .PNG.DDS/.PNG.json texture files in TEXTURE directory")
    statusDBT <- file.remove(filesToRemoveBuiltTex)
    message(sum(statusDBT), " .PNG.DDS/.PNG.json files (TEXTURE built) removed")
    message("----------------------------")

  }

  return(invisible())

}

#' Get GUID from .xml
#'
#' Get GUID from a modelLib .xml file.
#'
#' @param xmlPath Path to .xml in modelLib.
#'
#' @return GUID related to the .xml provided.
#' @export
#'
#' @examples getXmlGuid("D:/FSProjects/my-package/PackageSources/modelLib/03726152506153634.xml")
getXmlGuid <- function(xmlPath) {
  obj <- xml2::read_xml(xmlPath)
  # Get guid
  guid <- toupper(
    stringr::str_remove_all(
      xml2::xml_attr(obj, "guid"),
      pattern = "\\{|\\}"
    )
  )
  names(guid) <- stringr::str_remove(basename(xmlPath), ".xml$")
  return(guid)
}
