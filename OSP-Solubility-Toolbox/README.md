# OSP Solubility Toolbox

This folder contains the OSP Solubility Toolbox, an R-based framework designed to analyze *in vitro* solubility data. The framework is descrbased in greater detail in Vrenken et al. [[1](#References)]. It includes a Shiny App for interactive data visualization and parameter fitting, as well as a set of functions for solubility calculations at different pH values in the absence or presence of bile salts.

## Features

- **Data Import**: Easily read solubility data from an Excel file.
- **Parameter Fitting**: Fit solubility-related parameters to observed solubility data.
- **Shiny App**: Interactive interface for data analysis and visualization.
- **User-Friendly Template**: An Excel template that can be populated with user-specific data for seamless integration.

## Getting Started

### Prerequisites

To use the OSP Solubility Toolbox, ensure you have the following installed:

- R (version 4.0 or higher)
- RStudio (recommended for development)
- Required R packages (see the list below)

### Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/yourusername/OSP-Solubility-Toolbox.git
   ```

2. Open the R script `OSP_SolubilityToolbox.R` in RStudio.

3. Install the required R packages (if not already installed):

   ```R
   install.packages(c('shiny', 'shinythemes', 'shinyWidgets', 'gdata', 'openxlsx', 'ggplot2', 'gridExtra'))
   ```

4. Start the Shiny App in R:

   ```R
   runApp('OSP_SolubilityToolbox.R')
   ```

### Usage

1. Populate the provided Excel template `SolubilityData.xlsx` with your solubility data.
2. Run the R script to start the Shiny App and fit the solubility-related parameters.
3. Explore the Shiny App for interactive analysis and visualization of your results.

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 1: in vitro modeling of Vericiguat. *Eur J Pharm Sci.* 2025.](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow)



---

Feel free to customize any sections, such as the contact information or specific details about the functions and features, to better suit your project!
