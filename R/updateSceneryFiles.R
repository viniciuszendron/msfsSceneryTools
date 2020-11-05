#' Update Scenery Files
#'
#' @param xmlPath Path to objects.xml file.
#' @param modelLibDir Path to modelLib directory.
#' If NULL, will search for default location.
#' @param createBackup
#'
#' @export
#'
#' @examples
updateSceneryFiles <- function(xmlPath, modelLibDir = NULL, createBackup = FALSE) {
  if (is.null(modelLibDir)) {
    #modelLibDir <- file.path(xmlPath, "modelLib")
  }
  textureDir <- file.path(modelLibDir, "texture")

  # Open objects.xml
  obj <- xml2::read_xml(xmlPath)

  if (isTRUE(createBackup)) {
    xml2::write_xml(obj, file.path(dirname(xmlPath), paste0(basename(xmlPath), ".bak")))
  }

  # Get all GUIDS


  # Open all .xml in modelLib and match GUIDS with number IDs
  # Create a vector with correspondences


  # Delete all textures (.png) inside textureDir
  # Provide yes/no question

  statusPNG <- file.remove()

  # Delete all remaining .xml, .gltf and .bin inside modelLibDir
  # Provide yes/no question

  statusML <- file.remove()

}
