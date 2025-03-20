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

   
3. **Start the Shiny App**: To launch the Shiny App, click on "Run App" in the top right corner of the Source Panel. Alternatively, you can start the Shiny App by executing the following command in the R console:

   ```R
   runApp('OSP_SolubilityToolbox.R')
   ```
   
   Once the app is running, a new window will open displaying the graphical user interface (GUI) of the OSP Solubility Toolbox. The GUI consists of the following four tabs that should be completed consecutively:
      *  API Properties
      *  Aqueous SOlubility
      *  Biorelevant Solubility
      *  Surface pH

## Detailed Usage Instructions

### 1. Prepare Observed Data

Populate the data template file [SolubilityData.xlsx](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/OSP-Solubility-Toolbox/SolubilityData.xlsx) with your solubility data in aqueous and biorelevant media. 

#### Aqueous Solubility Data 

Enter your aqueous solubility data in the first sheet, titled `Observed.Aqueous`. The columns are defined as follows:
   * `ID.aq`: Unique identified for each experiment.
   * `Source`: Source of the reported solubility value.
   * `Medium`: Medium in which the solubility was measured.
   * `pH.final`: pH value of the medium. For non-neutral compounds, use the pH measured at the end of the experiment, if available.
   * `Obs.aq.sol`: Observed solubility of the compound. The input must be a numeric value.
   * `Obs.aq_unit`: Unit of solubility. Input must be one of the following strings:
      * Molar concentration units: `m`, `mm`, `um`, `nm`, `mol/l`, `mmol/l`, `umol/l`, `nmol/l`
      * Mass concentration units: `g/l`, `mg/l`, `ug/l`, `ng/l`, `pg/l`, `mg/dl`, `ug/dl`, `ng/dl`, `pg/dl`, `mg/ml`, `ug/ml`, `ng/ml`, `pg/ml`
   * `SD.aq.Sol`: Arithmetic standard deviation of solubility value (optional).
   * `SD.aq_unit`: Unit of the arithmetic standard deviation. The same units as `Obs.aq_unit` are allowed (optional).

#### Biorelevant Solubility Data 

Enter your solubility data measured in biorelevant media in the second sheet, titled `Observed.Biorelevant`. The columns are defined as follows:
   * `ID.BR`: Unique identified for each experiment.
   * `Source`: Source of the reported solubility value.
   * `Name`: Name of the biorelevant medium (e.g. FaSSIF, FeSSIF).
   * `pH.BR`: pH value of the biorelevant medium.
   * `BS_mM`: Molar concentration of bile salts present in the biorelevant medium, expressed in millimolar. The table below lists molar concentrations of bile salts in the most commonly used biorelevant media.
   * `Obs.BR.sol`: Observed solubility of the compound in the biorelevant medium. The input must be a numeric value.
   * `Obs.BR_unit`: Unit of solubility. Input should match one of the allowed units for the column `Obs.aq_unit` in the aqueous solubility sheet.
   * `SD.BR.Sol`: Arithmetic standard deviation of the solubility value (optional).
   * `SD.BR_unit`: Unit of the arithmetic standard deviation. The same units as `Obs.BR_unit` are allowed (optional).

### 2. Define API Properties and Import Data in Shiny App

Start the OSP Solubility Toolbox as Shiny App as described above. A window will open displaying the first tab of the app, titled `API Properties`. In this tab, you can define the basic properties of the compound, import observed data, and save or load user settings. 

In the section `Save/Load Settings` you can save the settings (user-defined input values) of the current session or load the settings from a previous session. 
Please note that observed data, fit results, and figures are not saved. If you load user-defined input values from a previous session, the observed data need to be imported again separately.

If you are starting a new project, manually enter the following information about your compound in the section `API Properties`:
* `Active Pharmaceutical Ingredient (API):` The name of the active pharmaceutical ingredient (API).
* `LogP`: The lipophilicity value of the API, typically represented as logP or logD.
* `Mol. Wt.`: The molecular weight of the API in g/mol.

Next, load your solubility data by clicking on the `Browse` button in the section `Observed Solubilities`. Select your prepared Excel file, `SolubilityData.xlsx`, which contains the necessary solubility data.

### 3. Fit Aqueous Solubility Parameters

Proceed to the second tab, `Aqueous Solubility`. In this section, you will fit the `Solubility Gain per charge`, which modulates the solubility-pH profile. 
Enter the following parameters for your compound:
* pKa Values and Types: Input the pKa values of the API along with their corresponding types.
* Reference Solubility: Enter the reference solubility value for the API.
* pH: Specify the pH of the solution in which the reference solubility was measured.

A solubility-pH profile will be generated and displayed on the right panel, alongside the solubility estimated by PK-Sim using the `Solubility Gain per charge` value defined in the respective input field. 
This figure is dynamic and will update if you modify the value for `Solubility Gain per charge`.

Choose a scaling method for parameter optimization:
* Linear Scaling Method: This method minimizes the sum of squared residuals of the actual values.
* Logarithmic Scaling Method: This method minimizes the sum of squared residuals of the log-transformed values.

Click on the `Estimate SG` button to fit the Solubility Gain per Charge Factor. You may need to manually adjust the starting values to improve the fit's convergence.

You can also view residual plots by clicking on the respective button below the solubility-pH profile. Note that these plots will not update automatically with manual changes to the `Solubility Gain per Charge Factor`.

### 4. Fit Biorelevant Solubility Parameters

Next, navigate to the third tab, `Biorelevant Solubility`. Here, you will fit the water-to-micelle partition coefficients for the ionized and unionized compound species, if solubility data from biorelevant media is available.

To do so, enter the following information:
* Intrinsic Solubility: Input the intrinsic solubility value for the API. An automatically calculated value for intrinsic solubility and pH will be suggested below the input fields.
* pH: Enter the pH at which the intrinsic solubility is measured or calculated.

A plot showing predicted vs. observed biorelevant solubilities will be displayed. This figure is dynamic and will update if you change input values for relevant parameters in the tabs `Aqueous Solubility` and `Biorelevant Solubility`. 

Select the scaling method for parameter optimization. Fit the water-to-micelles partition coefficients for both the ionized and non-ionized compound species by clicking on the `Estimate Km` button. As with the previous step, you may need to manually adjust the starting values to ensure convergence of the fit. Feel free to experiment with different values; the fit will automatically update as you enter new data into the respective fields.

You can display residual plots by clicking on the respective button below the predicted vs. observed biorelevant solubilities plot. 

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 1: in vitro modeling of Vericiguat. *Eur J Pharm Sci.* 2025.](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow)
