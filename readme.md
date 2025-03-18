# Ibexplorer Documentation

## Overview

Ibexplorer is a Shiny web application designed for processing and filtering PCIbex results files. It allows users to upload a CSV file, select specific columns, filter rows based on a search phrase, preview the processed data, and download the formatted dataset.

## Installation

To run Ibexplorer, you need R and the required packages installed on your system.

### Prerequisites

1. Install R (https://cran.r-project.org/)
2. Install Required Packages

Run the following command in R to install the necessary dependencies:

```r
install.packages(c("shiny", "tidyverse", "shinythemes", "psych", "DT", "bslib"))
```

## Running the Application
Save the application script as `app.R` and run it in R or RStudio using:

```r
shiny::runApp("app.R")
```

This will launch the app in a web browser.

## Features

- Supports CSV and TXT files up to 30MB.
- Processes PCIbex results files automatically.
- Users can select which columns to include in the processed dataset.
- Allows filtering rows based on a user-provided search phrase.
- Retains only rows where at least one cell contains the search phrase.
- Displays a searchable and sortable table of the filtered dataset.
- Provides a summary of numeric columns.
- Users can download the formatted dataset as a CSV file.

## Usage Guide

1. Upload a CSV or TXT file.
2. Click the **Submit** button to process the file.
3. (Optional) Select the columns to keep.
4. (Optional) Enter a search phrase to filter rows.
5. View the processed data in the preview tab.
6. Download the filtered dataset by clicking **Download formatted CSV**.

## Troubleshooting

- Ensure that your file is in CSV or TXT format.
- Check that the file size does not exceed 30MB.
- If no data appears, verify that the correct columns are selected.
- The explorer works only with the unmodified PCIbex results file.

## License

This application is open-source and available for modification and redistribution under the MIT License.

## Contact

For questions or issues, reach out via GitHub or email the developer: anna.pryslopska[at]gmail.com.