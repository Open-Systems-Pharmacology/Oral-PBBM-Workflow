# OSP Solubility Toolbox

This folder contains the OSP Solubility Toolbox, an R-based framework designed to analyze *in vitro* solubility data. The framework is described in greater detail in the article series by Vrenken et al. [[1](#References)]. It includes a Shiny App for interactive data visualization and parameter fitting, as well as a set of functions for solubility calculations at different pH values in the absence or presence of bile salts.

## Features

- **Data Import**: Easily read solubility data from an Excel file.
- **Parameter Fitting**: Fit solubility-related parameters to observed solubility data.
- **Shiny App**: Interactive interface for data analysis and visualization.
- **User-Friendly Template**: An Excel template that can be populated with user-specific data for seamless integration.

## Prerequisites

To use the OSP Solubility Toolbox, ensure you have the following installed:

- R (version 4.0 or higher)
- RStudio (recommended for development)
- Required R packages (see the list below)

## Getting Started

To use the OSP Solubility Toolbox, follow these steps:

1. **Prepare Your Data:** Enter the solubility data and physicochemical properties of your compound into the Excel template file `SolubilityData.xlsx`. 


2. **Open the R Script:** Open `OSP_SolubilityToolbox.R` in RStudio and install the required R packages (if not already installed):

   ```R
   install.packages(c('shiny', 'shinyjs', 'shinythemes', 'shinyWidgets', 'gdata', 'openxlsx', 'ggplot2', 'gridExtra'))
   ```

   
3. **Start the Shiny App**: To launch the Sihny App, click on "Run App" in the top right corner of the Source Panel. Alternatively, you can start the Shiny App by executing the following command in the R console:

   ```R
   runApp('OSP_SolubilityToolbox.R')
   ```
   
   Once the app is running, a new window will open displaying the graphical user interface (GUI) of the OSP Solubility Toolbox. The GUI consists of the following four tabs that should be completed consecutively:
      *  API Properties
      *  Aqueous SOlubility
      *  Biorelevant Solubility
      *  Surface pH

## Detailed Usage Instructions



## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 1: in vitro modeling of Vericiguat. *Eur J Pharm Sci.* 2025.](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow)
