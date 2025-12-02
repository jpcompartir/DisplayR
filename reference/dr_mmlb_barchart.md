# Create a stacked bar chart for Microsoft's Making My Life Better classification

Generates a stacked bar chart using 'mschart' with custom colors for
'yes' and 'no' groups.

## Usage

``` r
dr_mmlb_barchart(data, colours, state_var)
```

## Arguments

- data:

  A dataframe containing the states and their corresponding 'yes' and
  'no' counts.

- colours:

  A named vector of colors to apply to the 'yes' groups in the chart.

- state_var:

  The column with the state being plotted (x axis) \#'

## Value

An object of class 'ms_barchart' representing the stacked bar chart.

## Examples

``` r
df <- dplyr::tribble(
  ~state, ~yes, ~no,
  "Brings me joy", 4861, 167,
  "Empowers me", 4715, 313,
  "Feels connected", 922, 4106,
  "Inspires new ideas", 4906, 122,
  "Simplifies tech", 3954, 1074
)

colours <- c("#0078D4", "#107C10", "#FFB900", "#008575", "#D83B01")
chart <- dr_mmlb_barchart(df, colours, state_var = state)
print(chart, preview = TRUE)
#> * 'ms_barchart' object
#> 
#> * original data [10,4] (sample):
#>             state class    n               group
#> 1   Brings me joy   yes 4861   Brings me joy_yes
#> 2   Brings me joy    no  167    Brings me joy_no
#> 3     Empowers me   yes 4715     Empowers me_yes
#> 4     Empowers me    no  313      Empowers me_no
#> 5 Feels connected   yes  922 Feels connected_yes
#> 
#> * series data [5,11] (sample):
#>                state Brings me joy_yes Brings me joy_no Empowers me_yes
#> 1      Brings me joy              4861              167              NA
#> 2        Empowers me                NA               NA            4715
#> 3    Feels connected                NA               NA              NA
#> 4 Inspires new ideas                NA               NA              NA
#> 5    Simplifies tech                NA               NA              NA
#>   Empowers me_no Feels connected_yes Feels connected_no Inspires new ideas_yes
#> 1             NA                  NA                 NA                     NA
#> 2            313                  NA                 NA                     NA
#> 3             NA                 922               4106                     NA
#> 4             NA                  NA                 NA                   4906
#> 5             NA                  NA                 NA                     NA
#>   Inspires new ideas_no Simplifies tech_yes Simplifies tech_no
#> 1                    NA                  NA                 NA
#> 2                    NA                  NA                 NA
#> 3                    NA                  NA                 NA
#> 4                   122                  NA                 NA
#> 5                    NA                3954               1074
```
