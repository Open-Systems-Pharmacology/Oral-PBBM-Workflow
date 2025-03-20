# In Vitro Dissolution Model

This folder contains the *in vitro* dissolution model developed by Vrenken et al. [[1](#References)]. The model is implemented in MoBi and is designed to simulate drug release kinetics from various formulations. 
It accounts for the influence of bile salt micelles on solubility, as well as the effects of particle size and hydrodynamic conditions on the diffusion layer thickness during the dissolution process.
The model is built upon the [OSP Solubility Toolbox](https://github.com/AndreDlm/Oral-PBBM-Workflow/tree/main/OSP-Solubility-Toolbox), with several parameters in this dissolution model derived from the OSP Solubility Toolbox.

## Features

## Prerequisites

To use the OSP Solubility Toolbox, ensure you have the [OSP Software Suite](https://github.com/Open-Systems-Pharmacology/Suite/releases) installed (version 11.3 or higher).

## Getting Started

Download the MoBi template file distributed in this repository and open it with MoBi. 

## Usage

Relevant parameters of the compound and formulation as well as the dissolution experiment are defined in the **Parameter Start Values** Building Block. The `API Properties` Building Block in this section contains the follwoing parameters that may need to be adjusted:
* `iBin`: Unique identifier for each particle size bin. This value should not be modified.
* `Volume`: Volume of the dissolution medium which can be either 500 mL or 900 mL.
* `Density (drug)`: Specific gravity of the drug.
* `Dose`: Mass of the drug used in the dissolution experiment.
* `Molecular weight`: Molar mass of the drug.
* `PrecipitatedDrugSoluble`: Boolean variable that indicates whether the precipitated drug can (re)dissolve (set value to `1`) or not (set value to `0`).
* `Thickness (unstirred water layer)`: Thickness of the unstirred water layer surrounding a drug particle. This parameter my not be applicable depending on the dissolution model used. **Further details needed:**
    UseHydrodynamicModel = 1 --> h_u = 2 * r/Sh
    UseHydrodynamicModel = 0 --> UseHintzJohnson?
                                 UseHintzJohnson = 1 --> h_u = min(r_current; h_limit)
                                 UseHintzJohnson = 0 --> h_u = h_limit
    h_limit = Thickness (unstirred water layer)
* `NBins`: Number of particle size bins utilized in the dissolution model, with a maximum allowed value of 10.
* `r_mean` and `r_gsd`: If particle sizes are assumed to be log-normally distributed, `r_mean` represents the geometric mean of the particle sizes, and `r_gsd` denotes the geometric standard deviation. These values are not used if the particle sizes for each bin are manually specified or fitted during parameter optimization.
* `unitC_1µm`: This parameters is used in the calculation of different parameters related to the fluid velocity (such as the Reynolds number, micro-eddie velocity, and terminal velocity) according to the equations presented by Sugano et al.**The final dissolution model does not use the Sugano equations; hence, this parameter is not needed and can be deleted (?)**
* `precipitationrate`: Precipitation rate of the drug, applicable only if `PrecipitatedDrugSoluble` is set to `0`.
* `pH`: pH of the of dissolution medium.
* `Reference pH`: pH of the solution in which the `Solubility at reference pH` of the drug is measured.
* `Solubility at reference pH`: Measured aqueous (thermodynamic) solubility of the drug.
* `Solubility gain per charge`: Factor by which the solubility increases with each ionization step of the drug. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value. 
* `Compound type {0...2}`: Defines the drug's ionization state(s); the following values can be used: `-1`: acid; `0`: neutral; `+1`: base
* `pKa value {0...2}`: p*K*<sub>a</sub> value for functional group `{0...2}` of the drug.
* `Immediately dissolve particles smaller than`: Threshold value below which drug particles do not continue to shrink through dissolution, but are immediately dissolved. This improves the comptutaional speed of the model solving.
* `BS_Critical Micellar Concentration`: Molar concentration of bile salts or surfactants in a solution at which micelles start to form. Below this concentration, bile salts or surfactants primarily exist as individual molecules dispersed in the solution and no micelles are formed.
* `BS_Concentration`: Molar concentration of bile salts or surfactants present in the dissolution medium.
* `BS_Lok K_neutral`: Logarithm of the water-to-micelle partition coefficient for the unionized drug species. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value.
* `Disintegration Lambda`: Variable used in the Weibull function to reduce the initial number of particles, applicable only if `enableTabletDisintegration` is set to `1`.
* `enableTabletDisintegration`: Boolean variable that indicates whether tablet disintegration is active (set value to `1`) or inactive (set value to `0`). If set to `1`, drug dissolution is initially slowed down by multiplying the number of particles in the dissolution equation with a time-dependent factor that increases from 0 to 1 according to a Weibull function according to the following function: 

  <a href="https://www.codecogs.com/eqnedit.php?latex=\texttt{Number&space;of&space;particles&space;}=\texttt{&space;}(1-exp(-\texttt{lambda}\cdot&space;\frac{\texttt{Time}}{\texttt{Tablet&space;surface&space;area}}^{\texttt{alpha}}))\cdot&space;(\texttt{Maximum&space;number&space;of&space;particles}-1)&plus;1" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\texttt{Number&space;of&space;particles&space;}=\texttt{&space;}(1-exp(-\texttt{lambda}\cdot&space;\frac{\texttt{Time}}{\texttt{Tablet&space;surface&space;area}}^{\texttt{alpha}}))\cdot&space;(\texttt{Maximum&space;number&space;of&space;particles}-1)&plus;1" title="\texttt{Number of particles }=\texttt{ }(1-exp(-\texttt{lambda}\cdot \frac{\texttt{Time}}{\texttt{Tablet surface area}}^{\texttt{alpha}}))\cdot (\texttt{Maximum number of particles}-1)+1" /></a>

* `Disintegration Alpha`: Variable used in the Weibull function to reduce the initial number of particles, applicable only if `enableTabletDisintegration` is set to `1`.
* `BS_Lok K_ionized`: Logarithm of the water-to-micelle partition coefficient for the ionized drug species. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value.
* `BS_C H2O`: Molar concentration of water. This parameter is temperature-dependent and might need to be adjusted for temperatures other than 37°C. It is used in the calculation of the biorelevant solubility of the drug. 
* `H_Radius paddle max (H)`, `H_radius paddle min (I)`, `H_Radius vessel (R)`, `H_Distance between paddle and vessel bottom`, `H_Height of the paddle (B)`, `H_Shaft diameter (Q)`, and `H_T`: These parameters correspond to various geometrical measurements of the dissolution apparatus and vessel. They are pre-defined for the USP-2 dissolution apparatus but may require adjustments for use with other dissolution apparatuses.
* `H_RPM`: Revolutions per minute indicating how quickly the stirrer of the dissolution apparatus is rotating, which affects the hydrodynamics of the dissolution medium and the rate at which the drug dissolves.
* `H_Temperature`: Used to calculate the density of water. **Only used for Sugano implementation or also for Pepin model?**
* ... **Add additional parameters** ...

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 1: in vitro modeling of Vericiguat. *Eur J Pharm Sci.* 2025.](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow)
