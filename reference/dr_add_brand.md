# Add brand styling to your project

Add brand files to your project from DisplayR's inst/brands folder. If
you need a new brand to be added, contact the Data Science Team

## Usage

``` r
dr_add_brand(brand, directory = getwd(), overwrite = NULL)
```

## Arguments

- brand:

  Name of the brand you wish to load, as it appears in
  [`dr_list_brands()`](dr_list_brands.md)

- directory:

  The directory you want the brand information to be added to, as a
  default it uses the current working directory

- overwrite:

  Whether to overwrite any existing files

## Value

Invisible

## Examples

``` r
brands <- dr_list_brands()
#> ✔ 2 brands available
#> • samyds
#> • shareds
dr_add_brand("shareds")
#> ✔ Added shareds brand files:
#> • _brand.yml
#> • logo.png
```
