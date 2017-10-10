

displaceplot
=======

The goal of displaceplot is to provide routine to process the DISPLACE simulation output files.

Installation
------------

You can install displaceplot from github with:

``` r
# install.packages("devtools")
devtools::install_github("frabas/displaceplot")
```

or alternatively, stable releases of the displaceplot R package can be found on 
``` r
https://github.com/frabas/displaceplot/releases
```

Use
------------
First load the package into R and search for documentation by doing:
``` r
library(displaceplot)
library(help=displaceplot)
?mapNodeAverageLayerFiles
```

Dev.
------------
For Windows, compile with
``` r
 R CMD INSTALL --build displaceplot
```
For Unix-based, use
``` r
R CMD build displaceplot
```
Don´t forget to detach first when required with
``` r
detach('package:displaceplot', unload=TRUE)
```

