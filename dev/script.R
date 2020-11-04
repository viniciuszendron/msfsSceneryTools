# Install Package if not installed
if (!require("remotes")) install.packages("remotes")
if (!require("msfsSceneryTools")) remotes::install_github("viniciuszendron/msfsSceneryTools@main")

# Check Package Version
packageVersion("msfsSceneryTools")

# Set main variables
# Change these values to the values of your scenery!
PackageSourcesDir <- "C:/.../PackageSources"
modelLibDir <- "D:/.../PackageSources/modelLib"
nlods <- 1 # Number of lods

# Perform checks
binGltfCheck <- msfsSceneryTools::checkAllFilesBinGltf(modelLibDir, nlods = nlods)
lodsCheck <- msfsSceneryTools::checkAllLods(modelLibDir)

# Check which files are INVALID
invalidBinGltf <- Filter(function(x) isFALSE(x[[1]][1]), binGltfCheck)
sapply(invalidBinGltf, "[[", 4) # Get guids of invalidBinGltf
names(invalidBinGltf)
invalidLods <- Filter(function(x) isFALSE(x[[1]][1]), lodsCheck)
names(invalidLods)

# Check which are VALID
validBinGltf <- Filter(function(x) isTRUE(x[[1]][1]), binGltfCheck)
names(validBinGltf)
validLods <- Filter(function(x) isTRUE(x[[1]][1]), lodsCheck)
names(validLods)

# Fix both Lods and missing bin/gltf files
msfsSceneryTools::fixCorruptedFiles(PackageSourcesDir, nlods = nlods)

# Just check if there are no invalid files anymore
invisible(msfsSceneryTools::checkAllFilesBinGltf(modelLibDir, nlods = nlods))
invisible(msfsSceneryTools::checkAllLods(modelLibDir))


# Now, it is time to build the scenery package and see if everything is fine. It should not return error or failed.
# Code below is not necessary - debug only

# Check if the exported package is OK
library(magrittr)
xmls <- dir(modelLibDir) %>% .[stringr::str_detect(., "xml")]
count_xmls <- length(xmls)
modelLibObjs <- xmls %>% stringr::str_remove_all(., stringr::fixed(".xml"))

#dir_scene <- "D:/FSProjects/florianopolis-megapack/florianopolis-mega/PackageSources/scene"
#objects.xml <- xml2::read_xml(file.path(dir_scene, "objects.xml"))
#SceneryObjectNodes <- xml2::xml_find_all(objects.xml, ".//SceneryObject")
#length(SceneryObjectNodes)

outputDir <- "D:/FSProjects/maceio/Packages/flv-maceio"

filesTEXTURE <- dir(file.path(outputDir, "scenery/flv/TEXTURE"))
filesTEXTUREUnique <- filesTEXTURE %>%
  stringr::str_remove_all(., stringr::fixed(".PNG.DDS")) %>%
  stringr::str_remove_all(., stringr::fixed(".json")) %>%
  stringr::str_remove_all(., "_.*") %>%
  unique()

modelLibObjs[!modelLibObjs %in% filesTEXTUREUnique]


# Get all objects.xml nodes and compare
guids <- objectsXmlGuids("D:/FSProjects/maceio/PackageSources/scene/objects.xml")

guidsBinGltf <- sapply(binGltfCheck, "[[", 4)
guidsLods <- sapply(lodsCheck, "[[", 2)
length(lodsCheck)
length(binGltfCheck)
length(guids)

all(
  length(lodsCheck) == length(binGltfCheck),
  length(guids) == length(lodsCheck),
  length(guids) == length(binGltfCheck)
)

guidsBinGltf[guidsBinGltf %in% guids]
guids[guids %in% guidsBinGltf]
guidsBinGltf[!guidsBinGltf %in% guids]
