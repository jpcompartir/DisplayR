# Add a new slide to a PowerPoint presentation and insert a chart

This function creates a new slide with a specified layout and inserts a
specified chart into the PowerPoint presentation using the officer
package.

## Usage

``` r
disp_add_slide(
  presentation,
  chart,
  layout = "Title and Content",
  master = "Office Theme"
)
```

## Arguments

- presentation:

  An officer::pptx object representing the PowerPoint presentation.

- chart:

  An mschart::ms_chart object representing the chart to be inserted.

- layout:

  A character string specifying the slide layout to be used (default:
  "Title and Content").

- master:

  A character string specifying the master slide to be used (default:
  "Office Theme").

## Value

A modified officer::pptx object with the new slide and chart added.
