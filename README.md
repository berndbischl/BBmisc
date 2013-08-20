BBmisc
======

Miscellaneous helper functions for and from B. Bischl and some other guys at TU Dortmund, mainly for package development.

Offical CRAN release site: 
http://cran.r-project.org/web/packages/BBmisc/index.html


How to install bleeding edge devlelopment version:

- Clone from git repo here

- Have recent version of R properly installed with all build tools.
  For Windows this will include 

  http://cran.r-project.org/bin/windows/Rtools/

- Have roxygen2, devtools and testhat R packages installed

- In a console run "make install" to install. Done.

- "make" will list all other build targets

- If you have problems (e.g. in Windows) because there is no "git" command line
  tool that we use to figure out the build number, remove these lines from the Makefile:

  echo "Setting version ...
  
  ${RSCRIPT} ./tools/set-version
  
