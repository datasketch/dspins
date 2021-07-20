
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dspins

<!-- badges: start -->

<!-- badges: end -->

Wrapper for [`rstudio/pins`](https://github.com/rstudio/pins) to save DS
elements in folders of users or organisations within AWS S3 buckets.

## Installation

You can install the development version of dspins with:

``` r
devtools::install_github("datasketch/dspins")
```

## Examples

### Create board

The following two lines will create a pin board called `board`, using
the folder `test` within the AWS S3 bucket `user.dskt.ch`. They return
the same result, as `bucket_id` defaults to `"user"`.

To create a board, the `.env` file with the necessary AWS access keys
must be located in the root directory.

``` r
board <- ds_board_s3(user_name = "test", bucket_id = "user")
board <- ds_board_s3("test")
```

### Write/read pin

To pin an element to the board created above, use `pin_write()` or
`pin_urls()`; `pin_urls()` calls `pin_write()` and additionally returns
the URLs to file location in the DS profile.

To load a pin, pass the slug of a pin of type `fringe` and `dsviz` to
`pin_read()`. This will raise an error if the type of the pin is `drop`.
For pins of type `drop`, use `pin_download()` to download the pin and to
display the file path(s) of the downloaded file(s).

``` r
fringe_mtcars <- homodatum::fringe(mtcars, name = "Mtcars dataset")

# Write pins
board %>% pin_write(fringe_mtcars)

urls <- board %>% pin_urls(fringe_mtcars)

# Read pins
board %>% pin_read("mtcars-dataset")

# For a pin of type drop
board %>% pin_download("slug-of-drop-pin")
```

### Check existence, delete, meta data

To check whether a pin exists, delete a pin, or return a list of the
meta data saved in its `data.txt` file, use the functions
`pin_exists()`, `pin_delete()`, and `pin_meta()` respectively.

``` r
board %>% pin_exists("mtcars-dataset")

board %>% pin_delete("mtcars-dataset")

board %>% pin_meta("mtcars-dataset")
```

### List pins

The `pin_list()` function returns a character vector of the slugs of all
pins saved to the board. Using `extended = FALSE`, it returns a
dataframe with one row per pin and the metadata from the board’s
`data.txt` file across its columns.

``` r
pins <- board %>% pin_list()

pins_extended <- board %>% pin_list(extended = TRUE)
```
