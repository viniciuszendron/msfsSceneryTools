#' Fix objects.xml
#'
#' @param xmlPath Path to the objects.xml file (usually located inside PackageSources/scene).
#' @param invalidGuids Vector with invalid GUIDS to remove from objects.xml.
#' @param createBackup Create a backup of objects.xml.
#'
#' @export
fixObjectsXML <- function(xmlPath, invalidGuids, createBackup = FALSE) {

  # Testes
  # xmlPath <- "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources/scene/objects.xml"
  # xmlPath <- "D:/FSProjects/florianopolis-megapack/florianopolis-mega - Copia/PackageSources/scene/objects.xml"
  #

  obj <- xml2::read_xml(xmlPath)

  if (isTRUE(createBackup)) {
    xml2::write_xml(obj, file.path(dirname(xmlPath), paste0(basename(xmlPath), ".bak")))
  }

  LibObjNodes <- xml2::xml_find_all(obj, "//SceneryObject/LibraryObject")
  message(basename(xmlPath), " inicializado com ", length(LibObjNodes), " entradas")

  #guidsIndex <- stringr::str_replace_all() xml2::xml_attr(LibObjNodes, attr = "name")
  guidsIndex <- xml2::xml_attr(LibObjNodes, attr = "name")

  nodesToRemove <- which(guidsIndex %in% paste0("{", invalidGuids, "}"))
  # nodesToKeep <- which(!guidsIndex %in% paste0("{", invalidGuids, "}"))

  # newObj <- xml2::xml_children(obj)[nodesToKeep]
  message("Removendo ", length(nodesToRemove), " entradas inválidas")
  xml2::xml_remove(xml2::xml_children(obj)[nodesToRemove])
  message(length(nodesToRemove), " entradas inválidas removidas")
  message("XML resultante com ", xml2::xml_length(obj), " entradas válidas")

  # Write new file
  file.remove(xmlPath)
  xml2::write_xml(obj, xmlPath)

}


#' Get guids from objects.xml nodes
#'
#' Get all guids identifiers registered in objects.xml file.
#'
#' @param xmlPath Path to the objects.xml file (usually located inside PackageSources/scene).
#'
#' @return A vector with the guids of all objects.xml nodes.
#' @export
#'
#' @examples
#' \donttest{
#' objectsXmlGuids("D:/FSProjects/my-package/PackageSources/scene/objects.xml")
#' }
objectsXmlGuids <- function(xmlPath){

  # Tests
  # xmlPath <- "D:/FSProjects/maceio/PackageSources/scene/objects.xml"
  #

  obj <- xml2::read_xml(xmlPath)

  LibObjNodes <- xml2::xml_find_all(obj, "//SceneryObject/LibraryObject")
  message(basename(xmlPath), " inicializado com ", length(LibObjNodes), " entradas")

  guids <- xml2::xml_attr(LibObjNodes, attr = "name")

  guids <- stringr::str_replace_all(guids, "\\{|\\}", "")

  return(guids)

}

