# annif

[![R-CMD-check](https://github.com/fennicahub/annif/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/fennicahub/annif/actions/workflows/check-standard.yaml)
[![issues](https://img.shields.io/github/issues/fennicahub/annif)](https://github.com/fennicahub/annif/issues)
[![pulls](https://img.shields.io/github/issues-pr/fennicahub/annif)](https://github.com/fennicahub/annif/pulls)


The `annif` package provides tools to access the serivice for interoperable thesauri, ontologies and classification schemes for different subject areas. 

## Installation instructions


The devel version of annif can be installed from GitHub as follows:

``` r
# Install annif if not already installed
if (!requireNamespace("annif", quietly = TRUE)) {
  remotes::install_github("fennicahub/annif")
}
```

``` r
remotes::install_github("fennicahub/annif")
```

## Example
The basic functionality of 'annif' can be explored as follows:


``` r
# Load the package
library(annif)
# Perform a simple search and print a table

project_info <- get_annif_project_info(project_id = "yso-fi")
print(project_info)
```



## Contribute

Contributions are very welcome:

- [Use issue tracker](https://github.com/fennicahub/annif/issues) for feedback and bug reports.
- [Send pull requests](https://github.com/fennicahub/annif/)
- [Star us on the Github page](https://github.com/fennicahub/annif/)

### Disclaimer

This package is in no way officially related to or endorsed by annif.

When using metadata retrieved from annif database in your work, please
indicate that the metadata source is annif. If your re-use involves some
kind of modification to data or text, please state this clearly to the
end user. See annif policy on [copyright and free re-use of data
](https://api.annif.fi/) for more detailed information and certain exceptions.
