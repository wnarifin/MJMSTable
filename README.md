# MJMSTable
R package for generating publication-ready tables according to 
The Malaysian Journal of Medical Sciences (MJMS, <https://ejournal.usm.my/mjms/>) 
statistical requirements (Reporting Statistical Results in Medical Journals, <https://pmc.ncbi.nlm.nih.gov/articles/PMC5101968/>).

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

### Vignette

You can also view the vignette / quick-start document here: <https://wnarifin.github.io/vignette/MJMSTable.html>
