
# darkside

darkside imports Graphpad prism xml files (`pzfx` files) as a data frame
/ tibble.

## Installation

You can install darkside from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("koncina/darkside")
```

## Usage

  - Use `pzfx_tables(path)` to list the data tables contained in the
    `pzfx` file.
  - Use `read_pzfx(path, data_table)` to load the data table from the
    pzfx file (by index or name).
