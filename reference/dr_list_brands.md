# View which brands have styling options available for them

Check which brands a \_brand.yml and an optional 'logo.png' have been
uploaded to DisplayR for.

## Usage

``` r
dr_list_brands(quiet = FALSE)
```

## Arguments

- quiet:

  whether to print the name of detected brands or not.

## Value

A list of folder names relating to brands/

## Examples

``` r
brands <- dr_list_brands()
#> ✔ 2 brands available
#> • samyds
#> • shareds
```
