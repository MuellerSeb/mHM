
# LICENSE

[TOC]

## COPYRIGHT

This file is part of the UFZ CHS mesoscale hydrologic model (mHM) which
is a spatially explicit multiscale hydrologic model that uses grid
cells as a primary hydrologic unit and the multiscale parameter
regionalization (MPR) technique.


## COPYRIGHT HOLDERS

 Copyright(c) 2005-2025, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.

The code is a property of:

> The Department Computational Hydrosystems (CHS) at the
> Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ<br/>
> Registered Office: Leipzig<br/>
> Registration Office: Amtsgericht Leipzig<br/>
> Trade Register: Nr. B 4703

The chronological list of mHM developers is provided in the AUTHORS file.

The UFZ(CHS) mesoscale hydrologic model mHM is free software. You can
redistribute it and/or modify it under the terms of the GNU General
Public License as published by the free Software Foundation either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You received a copy of the GNU Lesser General Public License along
with the UFZ CHS mesoscale hydrologic model mHM. It can be found
in the files `COPYING` and `COPYING.LESSER` provided with this software.
The complete GNU license text can also be found at <http://www.gnu.org/licenses/>.


## Contact

- mHM Admins (E-mail: mhm-admin@ufz.de)
- Project Leader: Prof. Dr. Luis Samaniego (E-mail: luis.samaniego@ufz.de)
- Dr. Rohini Kumar (E-mail: rohini.kumar@ufz.de)
- Supervisor: Prof. Dr. Sabine Attinger (E-mail: sabine.attinger@ufz.de)

> Department Computational Hydrosystems (CHS)<br/>
> Helmholtz Centre for Environmental Research - UFZ<br/>
> Permoserstr. 15<br/>
> 04318 Leipzig, Germany


## Redistribution

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

-  Redistributions of source code must retain the above
   copyright notice, this list of conditions, the following disclaimer
   and the modification conditions.
-  Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions, the following disclaimer and the
   modification conditions in the documentation and/or other materials
   provided with the distribution.
-  Neither the name of Helmholtz-Zentrum fuer Umweltforschung GmbH -
   UFZ, nor the names of its contributors may be used to endorse or
   promote products derived from this software without specific prior
   written permission.
-  Redistributions of source code are allowed for research purposes
   ONLY. For commercial applications you need to consult the contact
   persons of the Department Computational Hydrosystems (CHS)
   at the UFZ.


## Modification

If the software is modified to produce derivative works, such modified
software should be clearly marked, so as not to confuse it with the
version available from UFZ. The following sources are to be
acknowledged:

Original mHM conceptualization, river routing and MPR:
> Samaniego L., R. Kumar, S. Attinger (2010): Multiscale parameter regionalization of a grid-based hydrologic model at the mesoscale. Water Resour. Res., 46,W05523, doi:10.1029/2008WR007327, http://onlinelibrary.wiley.com/doi/10.1029/2008WR007327/abstract

> Kumar, R., L. Samaniego, and S. Attinger (2013): Implications of distributed hydrologic model parameterization on water fluxes at multiple scales and locations, Water Resour. Res., 49, doi:10.1029/2012WR012195, http://onlinelibrary.wiley.com/doi/10.1029/2012WR012195/abstract

mHM Code:
> Luis Samaniego et al., mesoscale Hydrologic Model. Zenodo. doi:10.5281/zenodo.1069202, https://doi.org/10.5281/zenodo.1069202

To cite a certain version, have a look at the [Zenodo site](https://zenodo.org/record/3239055).

We request to include the following notice in any derivative work
based on mHM. Please include the following lines at every location
of the derivative code where pieces of the original mHM code
have been taken:

```fortran
!> \note The following lines have been obtained from the mHM source (https://git.ufz.de/mhm/mhm)
!! - gitversion:        < Provide git version of the original source >
!! - origin:            < Provide the name of the original subroutine >
!! - lines:             < Provide from-to lines in the original subroutine >
!! - authors:           < Provide the complete list of original authors >
!! - modifications:     < Indicate in detail your contribution >
!! - date_modification: < Indicate the date of the modification >
!> \copyright (c) 2005 - today, mHM-Developers (CHS)
```

> **NOTE**
> Failure to provide this information in derivative works
> constitutes a breach of the mHM Copyright.
> < .. > information need to be provided by any developer using
> the original code of mHM.


## Logo and Figures License

Notwithstanding the licensing terms applied to the software and its documentation as detailed above, the logo of mHM (doc/figures/logo) and the figures of the documentation (doc/figures) are made available under the Creative Commons Attribution 4.0 International License (CC BY 4.0). This grants permission to use, share, and adapt the logo and figures, provided appropriate credit is given, and any changes are indicated. For more information on this license, visit https://creativecommons.org/licenses/by/4.0/.

To attribute correctly, please include the following:

- Credit to mHM and, if possible, a link back to our website or the relevant repository.
- Indication if changes were made to the logo and/or the figures.

This licensing applies solely to the logo and the figures and is independent of the software's licensing terms, including its documentation, which may be subject to different licensing terms as specified in other sections of this document.


## Breach of the Copyright

The following are the potential causes for the Breach of mHM Copyright.

-   Deletion of the original copyright in the derived codes.
-   Improper citation of the used material in the derived codes.
    See the notice above.
-   Deletion of the original author's list and the development history
    (partially or in totality).
-   Changing any paragraph of the GNU licence provided.
-   Not inclusion of the GNU licence in the derived code.
-   Not citing the suggested papers in any written work that is based on
    the mHM code or derived software.

If the derived mHM code has breached the Copyright, the Copyright Holders
will request the repository provider (e.g., GitHub) to take down the
respective code following the corresponding guidelines. In the case of GitHub,
this is specified at

https://docs.github.com/en/site-policy/content-removal-policies/dmca-takedown-policy


## Disclaimer of Warranty

THERE IS NO WARRANTY FOR THIS SOFTWARE, TO THE EXTENT PERMITTED BY
APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE
HELMHOLTZ-ZENTRUM FUER UMWELTFORSCHUNG GMBH - UFZ AND CONTRIBUTORS
PROVIDE THIS SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS
WITH YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.


## Limitation of Liability

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL THE HELMHOLTZ-ZENTRUM FUER UMWELTFORSCHUNG GMBH - UFZ AND
CONTRIBUTORS OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS THE
SOFTWARE AS PERMITTED BY GNU GENERAL PUBLIC LICENSE, BE LIABLE FOR
DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL
DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE SOFTWARE
(INCLUDING BUT NOT LIMITED TO LOSS OF DATA, OR DATA BEING RENDERED
INACCURATE OR LOSSES SUSTAINED BY THE USER OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER PROGRAMS),  EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Redistribution of Example Data

The example data provided with the UFZ CHS mesoscale hydrologic model
mHM are strictly for use in non-commercial research and non-commercial
education projects only. They are not to be redistributed or used
without the consideration of the licenses they were originally provided
under. The following list acknowledges the providers of the example
data for mHM and links to the licenses the data was provided under.

### e-OBS gridded dataset
-  We acknowledge the E-OBS dataset for temperature and precipitation
    from the EU-FP6 project ENSEMBLES
    (http://ensembles-eu.metoffice.com) and the data providers in the
    ECA&D project (http://www.ecad.eu)

-  Haylock, M.R., N. Hofstra, A.M.G. Klein Tank, E.J. Klok,
    P.D. Jones, M. New. 2008: A European daily high-resolution gridded
    dataset of surface temperature and precipitation.
    J. Geophys. Res (Atmospheres), 113, D20119, doi:10.1029/2008JD10201

-  http://www.ecad.eu/download/ensembles/ensembles.php (08.04.2014)

### Harmonized World Soil Database
-  We acknowledge the Harmonized World Soil Database dataset of the
    Food and Agriculture Organization of the United Nations (FAO), the
    International Institute for Applied Systems Analysis (IIASA),
    International Soil Reference and Information Centre (ISRIC),
    Institute of Soil Science at Chinese Academy of Sciences (ISSCAS)
    and Joint Research Centre of the European Commission (JRC) for
    providing the soil data.

-  FAO/IIASA/ISRIC/ISSCAS/JRC, 2012. Harmonized World Soil Database
    (version 1.2). FAO, Rome, Italy and IIASA, Laxenburg, Austria.

-  http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-
    database/HTML/ (08.04.2014)

### SRTM
-  We acknowledge the U.S. Geological Survey's Earth Resources
    Observation and Science (EROS) Center and NASA's Land Processes
    Distributed Active Archive Center (LP DAAC) for providing the
    digital elevation model.

-  https://lta.cr.usgs.gov/citation (08.04.2014)

### European Soil Database
-  We acknowledge the European Commission for providing
    hydrogeological data.

-  http://ec.europa.eu/geninfo/legal_notices_en.htm (08.04.2014)


## Used and included libraries

- `doxygen-awesome-css` (`doc/doxygen-awesome-css/`)
  - purpose: documentation formatting
  - sources: https://github.com/jothepro/doxygen-awesome-css
  - authors: jothepro
  - license: MIT License (included)

- `cmake-fortran-scripts` (`cmake/`)
  - purpose: cmake scripts
  - sources: https://git.ufz.de/chs/cmake-fortran-scripts
  - authors: CHS Developers
  - license: MIT License (included)

- `HPC-Fortran-module-loads` (`hpc-module-loads/`)
  - purpose: module load scripts
  - sources: https://git.ufz.de/chs/HPC-Fortran-module-loads
  - authors: CHS Developers
  - license: MIT License (included)

- `FORCES` (linked statically by default)
  - purpose: fortran library
  - sources: https://git.ufz.de/chs/forces
  - authors: CHS Developers
  - license: LGPLv3+

## Notices for Python binary wheels

Python wheels are built with zlib, HDF5, netCDF-C, and FORCES. Platform
repair tools can also bundle the compiler runtime libraries required by the
compiled extension. The applicable notices are reproduced below. The GNU
GPL and LGPL texts referenced by the runtime notices are provided in
`COPYING` and `COPYING.LESSER`.

FORCES retains the license statements supplied by its upstream source
archive. Its version is selected by `version_forces.txt`; this notice does
not reclassify FORCES or resolve any separate compatibility question.

### zlib

Source: https://zlib.net/

```
Copyright notice:

 (C) 1995-2026 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu
```

### HDF5

Source: https://github.com/HDFGroup/hdf5

```
Copyright Notice and License Terms for
HDF5 (Hierarchical Data Format 5) Software Library and Utilities
-----------------------------------------------------------------------------

HDF5 (Hierarchical Data Format 5) Software Library and Utilities
Copyright 2006 by The HDF Group.

NCSA HDF5 (Hierarchical Data Format 5) Software Library and Utilities
Copyright 1998-2006 by The Board of Trustees of the University of Illinois.

All rights reserved.

This software library and utilities is covered by the 3-clause BSD License.

Redistribution and use in source and binary forms, with or without
modification, are permitted for any purpose (including commercial purposes)
provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions, and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions, and the following disclaimer in the documentation
   and/or materials provided with the distribution.

3. Neither the name of The HDF Group, the name of the University, nor the
   name of any Contributor may be used to endorse or promote products derived
   from this software without specific prior written permission from
   The HDF Group, the University, or the Contributor, respectively.

DISCLAIMER:
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
“AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

For further details, please refer to the full license text available
at https://opensource.org/licenses/bsd-3-clause

You are under no obligation whatsoever to provide any bug fixes, patches, or
upgrades to the features, functionality or performance of the source code
("Enhancements") to anyone; however, if you choose to make your Enhancements
available either publicly, or directly to The HDF Group, without imposing a
separate written license agreement for such Enhancements, then you hereby
grant the following license: a non-exclusive, royalty-free perpetual license
to install, use, modify, prepare derivative works, incorporate into other
computer software, distribute, and sublicense such enhancements or derivative
works thereof, in binary and source code form.

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

Contributors:   National Center for Supercomputing Applications (NCSA) at
the University of Illinois, Fortner Software, Unidata Program Center
(netCDF), The Independent JPEG Group (JPEG), Jean-loup Gailly and Mark Adler
(gzip), and Digital Equipment Corporation (DEC).

-----------------------------------------------------------------------------

Portions of HDF5 were developed with support from the Lawrence Berkeley
National Laboratory (LBNL) and the United States Department of Energy
under Prime Contract No. DE-AC02-05CH11231.

-----------------------------------------------------------------------------

Portions of HDF5 were developed with support from Lawrence Livermore
National Laboratory and the United States Department of Energy under
Prime Contract No. DE-AC52-07NA27344.

-----------------------------------------------------------------------------

Portions of HDF5 were developed with support from the University of
California, Lawrence Livermore National Laboratory (UC LLNL).
The following statement applies to those portions of the product and must
be retained in any redistribution of source code, binaries, documentation,
and/or accompanying materials:

   This work was partially produced at the University of California,
   Lawrence Livermore National Laboratory (UC LLNL) under contract
   no. W-7405-ENG-48 (Contract 48) between the U.S. Department of Energy
   (DOE) and The Regents of the University of California (University)
   for the operation of UC LLNL.

   DISCLAIMER:
   THIS WORK WAS PREPARED AS AN ACCOUNT OF WORK SPONSORED BY AN AGENCY OF
   THE UNITED STATES GOVERNMENT. NEITHER THE UNITED STATES GOVERNMENT NOR
   THE UNIVERSITY OF CALIFORNIA NOR ANY OF THEIR EMPLOYEES, MAKES ANY
   WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY OR RESPONSIBILITY
   FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY INFORMATION,
   APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
   WOULD NOT INFRINGE PRIVATELY- OWNED RIGHTS. REFERENCE HEREIN TO ANY
   SPECIFIC COMMERCIAL PRODUCTS, PROCESS, OR SERVICE BY TRADE NAME,
   TRADEMARK, MANUFACTURER, OR OTHERWISE, DOES NOT NECESSARILY CONSTITUTE
   OR IMPLY ITS ENDORSEMENT, RECOMMENDATION, OR FAVORING BY THE UNITED
   STATES GOVERNMENT OR THE UNIVERSITY OF CALIFORNIA. THE VIEWS AND
   OPINIONS OF AUTHORS EXPRESSED HEREIN DO NOT NECESSARILY STATE OR REFLECT
   THOSE OF THE UNITED STATES GOVERNMENT OR THE UNIVERSITY OF CALIFORNIA,
   AND SHALL NOT BE USED FOR ADVERTISING OR PRODUCT ENDORSEMENT PURPOSES.

-----------------------------------------------------------------------------

Portions of HDF5 were developed with support from the National Science
Foundation under Federal Award No. 2534078.

-----------------------------------------------------------------------------
```

### netCDF-C

Source: https://github.com/Unidata/netcdf-c

```
Copyright 2025 Unidata

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
```

### GCC runtime libraries

Wheels built with GNU Fortran can contain `libgcc`, `libgfortran`, and
`libquadmath`. The GNU license texts are provided in `COPYING` and
`COPYING.LESSER`. `libgcc` and `libgfortran` are distributed with the
following exception.

Source: https://gcc.gnu.org/

```
GCC RUNTIME LIBRARY EXCEPTION

Version 3.1, 31 March 2009

Copyright (C) 2009 Free Software Foundation, Inc. <http://fsf.org/>

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.

This GCC Runtime Library Exception ("Exception") is an additional
permission under section 7 of the GNU General Public License, version
3 ("GPLv3"). It applies to a given file (the "Runtime Library") that
bears a notice placed by the copyright holder of the file stating that
the file is governed by GPLv3 along with this Exception.

When you use GCC to compile a program, GCC may combine portions of
certain GCC header files and runtime libraries with the compiled
program. The purpose of this Exception is to allow compilation of
non-GPL (including proprietary) programs to use, in this way, the
header files and runtime libraries covered by this Exception.

0. Definitions.

A file is an "Independent Module" if it either requires the Runtime
Library for execution after a Compilation Process, or makes use of an
interface provided by the Runtime Library, but is not otherwise based
on the Runtime Library.

"GCC" means a version of the GNU Compiler Collection, with or without
modifications, governed by version 3 (or a specified later version) of
the GNU General Public License (GPL) with the option of using any
subsequent versions published by the FSF.

"GPL-compatible Software" is software whose conditions of propagation,
modification and use would permit combination with GCC in accord with
the license of GCC.

"Target Code" refers to output from any compiler for a real or virtual
target processor architecture, in executable form or suitable for
input to an assembler, loader, linker and/or execution phase.
Notwithstanding that, Target Code does not include data in any format
that is used as a compiler intermediate representation, or used for
producing a compiler intermediate representation.

The "Compilation Process" transforms code entirely represented in
non-intermediate languages designed for human-written code, and/or in
Java Virtual Machine byte code, into Target Code. Thus, for example,
use of source code generators and preprocessors need not be considered
part of the Compilation Process, since the Compilation Process can be
understood as starting with the output of the generators or
preprocessors.

A Compilation Process is "Eligible" if it is done using GCC, alone or
with other GPL-compatible software, or if it is done without using any
work based on GCC. For example, using non-GPL-compatible Software to
optimize any GCC intermediate representations would not qualify as an
Eligible Compilation Process.

1. Grant of Additional Permission.

You have permission to propagate a work of Target Code formed by
combining the Runtime Library with Independent Modules, even if such
propagation would otherwise violate the terms of GPLv3, provided that
all Target Code was generated by Eligible Compilation Processes. You
may then convey such a combination under terms of your choice,
consistent with the licensing of the Independent Modules.

2. No Weakening of GCC Copyleft.

The availability of this Exception does not imply any general
presumption that third-party software is unaffected by the copyleft
requirements of the license of GCC.
```

### MinGW-w64 CRT

Windows wheels are built with the MinGW-w64 UCRT64 toolchain.

Source: https://www.mingw-w64.org/

```
Copyright (c) 2009, 2010 by the mingw-w64 project

This license has been certified as open source. It has also been designated
as GPL compatible by the Free Software Foundation (FSF).

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions in source code must retain the accompanying copyright
      notice, this list of conditions, and the following disclaimer.
   2. Redistributions in binary form must reproduce the accompanying
      copyright notice, this list of conditions, and the following disclaimer
      in the documentation and/or other materials provided with the
      distribution.
   3. Names of the copyright holders must not be used to endorse or promote
      products derived from this software without prior written permission
      from the copyright holders.
   4. The right to distribute this software or to use it for any purpose does
      not give you the right to use Servicemarks (sm) or Trademarks (tm) of
      the copyright holders. Use of them is covered by separate agreement
      with the copyright holders.
   5. If any files are modified, you must cause the modified files to carry
      prominent notices stating that you changed the files and the date of
      any change.

Disclaimer

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESSED
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Some runtime files carry the following public-domain disclaimer:

This file has no copyright assigned and is placed in the Public Domain.
This file is a part of the w64 mingw-runtime package.

The w64 mingw-runtime package and its code is distributed in the hope that
it will be useful but WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESSED OR
IMPLIED ARE HEREBY DISCLAIMED. This includes but is not limited to
warranties of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

### winpthreads

Windows wheels can contain `libwinpthread`, supplied by MinGW-w64.

Source: https://www.mingw-w64.org/

```
Copyright (c) 2011 mingw-w64 project

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

Parts of this library are derived from the Posix Threads library for
Microsoft Windows:

Copyright (C) 2010 Lockless Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Lockless Inc. nor the names of its contributors may
   be used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
```
