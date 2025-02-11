
<!-- README.md is generated from README.Rmd. Please edit that file -->

# InduTrace

<!-- badges: start -->

[InduTrace](https://github.com/tchonova/InduTrace/actions)
<!-- badges: end -->

`InduTrace` is an R package that contains an interactive Shiny
application. The tool is designed to help users prioritize and classify
contaminants based on their temporal patterns and identify contaminants
with similar patterns in time-profiles from long-term, high-frequency
river monitoring data. The application offers a user-friendly interface
with a familiar dashboard layout. For more details, please refer to
Chonova et al., 2025, Water Research.

## Citation

If you use the application please cite: <br> - Chonova T., Honti M.,
Loos M., Ruppe S., Langlois I., Griesshaber D., Fenner K., Singer H.,
2025. Unveiling industrial emissions in a large European river: Insights
from data mining of high-frequency measurements. Water Research.<br>

## Installation

You can install the development version of `InduTrace` directly from
GitHub. First, make sure you have the `devtools` package installed:

``` r
devtools::install_github("tchonova/InduTrace")
```

## Features and Usage

1.  **Read More buttons**  
    The application includes **“Read More”** buttons throughout the
    interface, which provide additional details of the specific
    functionalities, helping you understand the purpose and usage of
    each feature in the app.

2.  **Example Dataset**  
    An example dataset `test_dataset_InduTrace.csv` is included in the
    `data/` folder of the package. You can use this dataset to test the
    app’s features and explore its functionalities. Simply select the
    dataset from the app’s interface to begin.

3.  **Publication**  
    For detailed methodology and theoretical context, please refer to
    the publication: Chonova et al., 2024, Water Research.

## Running the Shiny App

Once installed, you can launch the Shiny app by running:

``` r
library(InduTrace)
InduTrace()
```

## Dependencies

The following packages are required to run `InduTrace`:<br> `shiny`<br>
`shinycssloaders`<br> `shinyFeedback`<br> `dplyr`<br> `readr`<br>
`tidyr`<br> `lubridate`<br> `ggplot2`<br> `forcats`<br> `tibble`<br>
`patchwork`<br>

## License

`InduTrace` is open-source and licensed under the CC BY 4.0 License. You
are free to use, modify, and distribute the app under the terms of this
license.
