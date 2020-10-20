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


#' Detect and Remove Corrupted Data
#'
#' @param PackageSourcesDir 
#'
#' @return
#' @export
detectAndRemoveCorruptedData <- function(PackageSourcesDir) {
  
  # Testes
  # PackageSourcesDir <- "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources"
  #
  
  modelLibDir <- file.path(PackageSourcesDir, "modelLib")
  
  files <- dir(modelLibDir)
  
  xmls <- files[stringr::str_detect(files, "xml")]
  
  xmlsDir <- file.path(modelLibDir, xmls)
  
  message("Buscando por erros em ", length(xmlsDir), " arquivos xml...")
  message("----------------------------")
  res <- lapply(xmlsDir, isValidXML)
  
  invalids <- Filter(function(x) isFALSE(x[[1]][1]), res)
  valids <- Filter(function(x) isTRUE(x[[1]][1]), res)
  
  message("----------------------------")
  message(length(valids), " arquivos válidos.")
  if (length(invalids) == 0) {
    message("Não há arquivos inválidos para serem removidos.")
    return()
  }
  message(length(invalids), " arquivos inválidos.")
  message("----------------------------")
  
  # Delete invalid XML from modelLib
  message("Removendo arquivos XML inválidos em modelLib")
  filesToRemove <- paste0(file.path(PackageSourcesDir, "modelLib", sapply(invalids, "[[", 3)))
  file.remove(filesToRemove)
  message(length(filesToRemove), " arquivos removidos")
  message("----------------------------")
  #
  
  # Clean corrupted guids from objects.xml
  invalidGuids <- sapply(invalids, "[[", 2)
  fixObjectsXML(xmlPath = file.path(PackageSourcesDir, "scene/objects.xml"), invalidGuids = invalidGuids)
  
  
}

#' Fix objects.xml
#'
#' @param xmlPath 
#' @param invalidGuids 
#' @param createBackup 
#'
#' @return
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

  # xml2::xml_add_parent(newObj, "FSData version=\"9.0\"")
  # # xml2::xml_remove(obj)
  # new <- xml2::xml_new_root("FSData")
  # xml2::xml_add_child(new, newObj)
  # # CREATE XML FILE
  # doc <- xml2::xml_new_document()
  # root <- xml2::xml_new_root("FSData version=\"9.0\"") #newXMLNode("session", doc = doc)
  # #xml2::xml_ne
  # root <- xml2::xml_add_child(new, "teste")
  
  
  # doc <- xml2::xml_new_document()
  # doc %>% 
  #   xml2::xml_add_child(., "FSData version=\"9.0\"") %>%
  #   {
  #     xml2::xml_add_child(., "child", 1, .where = "after")
  #   }
  # # doc <- xml2::xml_new_document()
  # # doc %>% 
  # #   xml2::xml_add_child(., "parent") %>%
  # #   {
  # #     xml2::xml_add_child(., "child", 1, .where = "after")
  # #     (xml2::xml_add_child(., "child") %>% xml2::xml_add_child("grandchild", 2))
  # #     xml2::xml_add_child(., "child", 3, .where = "after")
  # #     xml2::xml_add_child(., "child", 4, .where = "after")
  # #   }
  # message(doc)
  
  # 
  # x <- xml2::read_xml("<foo><bar><baz/></bar></foo>")
  # x1 <- x %>% xml2::xml_children() %>% .[[1]]
  # x2 <- x1 %>% xml2::xml_children() %>% .[[1]]
  # 
  # xml2::xml_remove(x1)
  # rm(x1)
  # gc()
  
}



#' Check if is valid XML
#'
#' @param xmlPath 
#'
#' @return
#' @export
isValidXML <- function(xmlPath) {
  
  # Testes
  #xmlPath = "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources/modelLib/036173415260400522.xml"
  #
  
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
  
  message(file, ": ", ifelse(status, "OK", "ERRO"))
  
  return(list(valid = status, guid = guid, filename = file))
  
}

message("Script Carregado com Sucesso!")
