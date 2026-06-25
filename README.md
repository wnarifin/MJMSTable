# MJMSTable
R package for generating publication-ready tables according to 
The Malaysian Journal of Medical Sciences (MJMS <http://www.mjms.usm.my/index.html>) 
statistical requirements <http://www.mjms.usm.my/MJMS23052016/01MJMS23052016_ED.pdf>.

## Installation

The `MJMSTable` package is currently available on GitHub. You can install the development version directly from [GitHub](https://github.com/wnarifin/MJMSTable) using the `devtools` or `remotes` package.

### Prerequisites

Install `devtools`/`remotes` package:

```r
install.packages("devtools")

```

```r
install.packages("remotes")

```

### Install `MJMSTable`

Install `MJMSTable` from GitHub:

```r
devtools::install_github("wnarifin/MJMSTable", build_vignettes = TRUE)

```

or

```r
remotes::install_github("wnarifin/MJMSTable", build_vignettes = TRUE)
```

### Loading the Package

After the installation is complete, you can load the package into your R session and access the documentation:

```r
library(MJMSTable)

# View the package index and help pages
?MJMSTable

# Open the package vignette / tutorial
vignette("MJMSTable")

```
