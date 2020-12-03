# msfsSceneryTools


<!-- badges: start -->
[![R build status](https://github.com/viniciuszendron/msfsSceneryTools/workflows/R-CMD-check/badge.svg)](https://github.com/viniciuszendron/msfsSceneryTools/actions)
[![version](https://img.shields.io/badge/version-0.3.0-red.svg)](https://semver.org)
<!-- badges: end -->

This package provides some functions to Microsoft Flight Simulator Scenery developers, especially those who extract photogrammetry data from other sources and need to do some cleaning and removing corrupted files.

## Installation

After installing R (https://www.r-project.org), just run the code below to install the latest package stable version available:

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("viniciuszendron/msfsSceneryTools@main")
```

If a message asking to update packages appear, just press Enter key or select the option "None".


## Changelog

### Version 0.3.0

#### Remove Lods

New function `removeLods` remove specific LODS from a project. It removes LOD entries from .xml located in modelLib, its correspondent .bin/.gltf and textures in modelLib/textures.

#### Functions

##### New Functions

- `removeLods`: Remove lods from project.
- `removeLodNodesFromXML`: Remove lod nodes from XML.
- `removeBinGltfByLods`: Remove .bin and .gltf by lods.
- `removeModelLibTexturesByLods`: Remove textures by lods.


### Version 0.2.0

#### Update Scenery Files

The new function `updateSceneryFiles` was created to remove remaining scenery files from modelLib and modelLib/texture that are no longer registered in scene/objects.xml (e.g. deleted photogrammetry tiles from in-game scenery editor). It can also remove .PNG.DDS and .PNG.DDS.json files located inside of a TEXTURE folder of an already built project.

#### Functions

##### New Functions

- `updateSceneryFiles`: Delete scenery files across the project (modelLib and modelLib/texture) that are no longer registered in scene/objects.xml. It can also remove .PNG.DDS and .PNG.DDS.json files located inside of a TEXTURE folder of an already built project by providing the TEXTURE path in `deleteBuiltTextures` argument.
- `getXmlGuid`: Get GUID from a modelLib .xml file.

### Version 0.1.2

- Functions `fixLods` and `fixBinGltf` (both executed from `fixCorruptedFiles`) now removes all correspondent texture files (.png) from folder *texture*. Previously, only the corrupted .bin, .gltf and .xml were deleted, but the textures themselves were not. This behavior could make the final package bigger to the end user (as Flight Simulator editor does not remove them automatically).
- New function: `objectsXmlGuids` (Get all guids identifiers registered in objects.xml file).
