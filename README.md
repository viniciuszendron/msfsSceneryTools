# msfsSceneryTools

<!-- badges: start -->
[![R build status](https://github.com/viniciuszendron/msfsSceneryTools/workflows/R-CMD-check/badge.svg)](https://github.com/viniciuszendron/msfsSceneryTools/actions)
[![version](https://img.shields.io/badge/version-0.1.2-blue.svg)](https://semver.org)
<!-- badges: end -->

This package provides some functions to Microsoft Flight Simulator Scenery developers, especially those who extract photogrammetry data from other sources and need to do some cleaning and removing corrupted files.

## Installation

After installing R, just run the code above to install the latest version available:

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("viniciuszendron/msfsSceneryTools@main")
```

## Changelog

### Version 0.1.2

- Functions `fixLods` and `fixBinGltf` (both executed from `fixCorruptedFiles`) now removes all correspondent texture files (.png) from folder *texture*. Previously, only the corrupted .bin, .gltf and .xml were deleted, but the textures themselves were not. This behavior could make the final package bigger to the end user (as Flight Simulator editor does not remove them automatically).
- New function: `objectsXmlGuids` (Get all guids identifiers registered in objects.xml file).
