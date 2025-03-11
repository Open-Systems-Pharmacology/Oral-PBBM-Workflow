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

## Usage

To use the OSP Solubility Toolbox, follow these steps:

1. **Prepare Your Data:** Enter the solubility data and physicochemical properties of your compound into the Excel template file `SolubilityData.xlsx`. 


2. **Open the R Script:** Open `OSP_SolubilityToolbox.R` in RStudio and install the required R packages (if not already installed):

   ```R
   install.packages(c('shiny', 'shinythemes', 'shinyWidgets', 'gdata', 'openxlsx', 'ggplot2', 'gridExtra'))
   ```

   
3. **Start the Shiny App**: Click on "Run App" in the top right corner of the Source Panel to start the Shiny App. Alternatively, the Shiny App can be started via the following code:

   ```R
   runApp('OSP_SolubilityToolbox.R')
   ```


4. **Input API Properties:** A new window will open with a graphical user interface (GUI). In the first tab (API Properties), manually enter the drug name, lipophilicity, and molecular weight (MW). Load your solubility data by clicking on the "Browse" button and selecting your Excel file `SolubilityData.xlsx` populated with data.


5. **Fit Aqueous Solubility Parameters:** In the second tab (Aqueous Solubility), enter the pKa values and types as well as the reference solubility and pH. Select the scaling method used during parameter optimization:
   * Linear scaling method: Minimizes the sum of squared residuals of the actual values
   * Logarithmic scaling method: Minimizes the sum of squared residuals of the log-transformed values
     
   Click on "Estimate SG" to fit the `Solubility Gain per charge factor`. The solubility-pH profile will be plotted on the right panel alongside the solubility estimated by PK-Sim. You may need to manually adjust the starting values to improve convergence of the fit (the plot updates automatically with manual changes).
   
   Residual plots can be displayed by clicking on the respective button below the solubility-pH profile. Please note that these plots will not updated automatically with manual changes of the `Solubility Gain per charge factor`.


6. **Fit Biorelevant Solubility Parameters:** In the third tab (Biorelevant Solubility), enter a value for the intrisic solubility and pH (an automatically claulcted value for the intrisic solubility and pH is suggested below the input fields) and select the scaling method used during parameter optimization. 

   Fit the water-to-micelles partition coefficients for the ionized and non-ionized API by clicking on "Estimate Km" (if solubility data from biorelevant media is available).

   Predicted vs. observed solubilities and residual plots can be displayed by clicking on the respective buttons. Please note that these plots will not updated automatically with manual changes of the input values.

   As with the previous step, you may need to manually adjust the starting values to ensure convergence of the fit. Feel free to experiment with different values; the fit will run automatically as you enter new data in the fields.

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 1: in vitro modeling of Vericiguat. *Eur J Pharm Sci.* 2025.](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow)
