# eCOTAT+ Algorithm for flow direction upscaling
Efficient parallel upscaling algorithm of river drainage networks for large data sets

CONTACT
-------------

adrianorpaz@ct.ufpb.br
 
+55 83 3216 7355 

HARDWARE REQUIREMENTS
-------------
Devices that run Windows 10 for desktop editions (a 1 GHz or faster processor)

SOFTWARE ESPECIFICATIONS
-------------
**PROGRAM LANGUAGE:**	Fortran 90 

**SOFTWARE REQUIREMENTS:**	`Fortran 90 compiler`, Windows 

**PROGRAM SIZE:**	38.6 Kb
 
AVAILABILITY
-------------
The source codes are available for download at the link: 

	https://github.com/lacmaufpb/eCOTAT

REFERENCE
============

Paz, A.R.; Collischonn, W.; Silveira, A.L.L. Improvements in large scale drainage networks derived from digital elevation models. Water Resources Research, v. 42, p. W08502, 2006. https://doi.org/10.1029/2005WR004544

VERSION
============
This algorithm is an improvement from COTAT+ algorithm.

USAGE
============
The following input files are required, with the specification as described:

Input files:
* **DIRHIGH (.rst;.rdc):**  high resolution flow directions: raster, Idrisi/TerrSet format, integer/binary (see codification below)
* **AREAHIGH (.rst;.rdc):** high resolution flow accumulated area: raster, Idrisi/TerrSet format, real/binary, attributes in km2
* **INPUT_UPSCALING (.txt):** upscaling configuration (ascii)

The names of the input files are defined along the code and could be changed.
The number of cores for the parallelization should be adjusted in the lines 368 and 610
in the 'num_threads()' argument prior to the two parallelized loops.

**FLOW DIRECTION CODE:**
|ArcGIS|    |    |     |eCOTAT+|    |     |   | 
|------|----|----|-----|-------|----|-----|---|
|      | 32 | 64 | 128 |       | 64 | 128 | 1 |
|      | 16 | *  |  1  |       | 32 |  *  | 2 |
|      | 8  | 4  |  2  |       | 16 |  8  | 4 |


* Verifies if the flow path traced is out of the 3x3 neighbouring cells (in this case, the flow direction of the cell is maintained, according to i. the last outlet pixel found, if any has been found; ii. the neighbouring cell last visited during flow path tracing).

* The flow path traced is out of the 3x3 neighbouring cells, without reaching any outlet pixel. The flow direction of the cell is defined according to the neighbouring cell last visited during flow path tracing

The configuration of the upscaling is set in the 'input_upscaling' input file. 
The following information is defined in this file: a) high-resolution cell size (same units of the input raster); b) low-resolution cell size (same units of the input raster); c) parameter Area Threshold (AT; km2); d) parameter Minimum Upstream Flow Path (MUFP; km); e) Selection for using (1) or not (0) a mask.
For example, for flow direction upscaling from 1 m to 100 m, option a) must assume the value '1', and option b) must assume the value '100'. The definition of AT and MUFP are according to Paz et al. (2006), which recommend AT equal to the low-resolution cell area and MUFP equal to 1/5 of the low-resolution cell size. In the example, AT should be set as 0.01 (i.e., 10000 m2, but in km2), and MUFP should be set as 0.02 (i.e., 20 m, but in km).

The upscaling algorithm requires an integer number of pixels (high resolution) inside each low-resolution cell. Therefore, the ratio between the high-resolution pixel and the low-resolution cell must be an integer. Additionally, the geographic extension of all input raster must be the same. These input rasters must also follow the specifications regarding data format. The QGIS software could convert raster files from other formats to the Idrisi format required by the algorithm. The Idrisi format was adopted due to having a specific file for the data matrix (the .rst file) and another specific format for the documentation (the .rdc file), and both being very easy to read by a Fortran code. The output file follows this format and could be converted to other raster formats using the QGIS software.
The flow direction codes of input and output files could also be reclassified from or to other coding systems using the QGIS software.

The algorithm produces the following output file:
* **DIRLOW (.rst;.rdc):** low resolution flow directions: raster, Idrisi/TerrSet format, integer/binary. The same flow direction code of the DIRHIGH file is followed: 1 (northeast), 2 (east), 4 (southeast), 8 (south), 16 (southwest), 32 (west), 64 (northwest), and 128 (north).
The name of the output file is defined along the code and could be changed with caution.

**Input and output files for testing the code are also available in this GIT:**

[example_input_files/](example_input_files)

	[https://github.com/lacmaufpb/eCOTAT/example_input_files/](https://github.com/lacmaufpb/eCOTAT/tree/main/Example_InputFiles)

[example_output_files/](example_output_files)

	[https://github.com/lacmaufpb/eCOTAT/example_output_files/](https://github.com/lacmaufpb/eCOTAT/tree/main/Example_OutputFiles)

