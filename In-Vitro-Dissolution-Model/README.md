# In Vitro Dissolution Model

This folder contains the *in vitro* dissolution model developed by Vrenken et al. [[1](#References)]. The model is implemented in MoBi and is designed to simulate drug release kinetics from various formulations. 
It accounts for the influence of bile salt micelles on solubility, as well as the effects of particle size and hydrodynamic conditions on the diffusion layer thickness during the dissolution process.
The model is built upon the [OSP Solubility Toolbox](https://github.com/AndreDlm/Oral-PBBM-Workflow/tree/main/OSP-Solubility-Toolbox), with several parameters in this dissolution model derived from the OSP Solubility Toolbox.

## Features
- **Data Import**: Batch import of dissolution data from an Excel file.
- **Parameter Fitting**: Fit dissolution-related parameters to observed dissolution data.
- **Dissolution Module**: MoBi template file to model in vitro dissolution experiments.
- **Dissolution Database Template**: An Excel template that can be populated with user-specific data for seamless integration.

## Prerequisites

To use the in vitro dissolution model, ensure you have the [OSP Software Suite](https://github.com/Open-Systems-Pharmacology/Suite/releases) installed. Please note that this workflow was developed with version 11.2 and has not been tested with higher software versions.

**Note:** If you use OSP version <13, please update the OSP Suite dimension database in both your Mobi installation folder as follows: 
Download the file [`OSPSuite.Dimensions.xml`](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/In-Vitro-Dissolution-Model/OSPSuite.Dimensions.xml) distributed in this repository and copy it into both your Mobi and PK-Sim installation folder (overwrite the existing file in each folder). 

## Getting Started

### 1. Importing observed in vitro dissolution data in MoBi (Optional)

**Required steps:**

- Download the [`In vitro db template.xlsx`](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/In-Vitro-Dissolution-Model/Input%20in%20vitro%20database/In%20vitro%20db%20template.xlsx) file and fill `‘Studies’` tab with especially the in vitro set-up, compound and ID; `‘ReleaseProfiles’` Variability is optional, `‘Analyte’` at least name and MW; and `‘projects’` tab. *Be mindful of columns with formulas and make sure to keep them intact and extend them for new inputs, this helps ensuring the input integrity.*
- Download the [`InVitro-db-JSON-builder.R`](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/In-Vitro-Dissolution-Model/InVitro-db-JSON-builder.R) script and the [`JSONauxiliaryFunctionsInVitroDb.R`](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/In-Vitro-Dissolution-Model/Auxiliary%20functions/JSONauxiliaryFunctionsInVitroDb.R) located in the `Auxiliary functions` folder.
- Run the `InVitro-db-JSON-builder.R` script after specifying the working directory and in vitro database location. *The initial plotting can be skipped.*
- Open PK-Sim **in developer mode** and load the [`TransferDummy.pksim5`](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/In-Vitro-Dissolution-Model/TransferDummy.pksim5) file. Then load observed data .JSON file from the previous step. Run dummy simulation and drag all observed data of interest in to the simulation. Save simulation into .pkml file.

This .pkml file can be loaded into you MoBi project file.

Files can be loaded as building blocks in PK-Sim<sup>®</sup> when the application is started **in developer mode** via the Windows command prompt as follows (see also [here](https://github.com/Open-Systems-Pharmacology/Forum/issues/305) for an alternative way): 

`cd C:\Program Files\Open Systems Pharmacology\PK-Sim [VERSION NUMBER]`

`PKSim /dev`

### 2. Preparing the dissolution model

**Required steps:**

- Download the MoBi template file [`In-Vitro-Dissolution-Model_Template.mbp3`](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/In-Vitro-Dissolution-Model/In-Vitro-Dissolution-Model_Template.mbp3) distributed in this repository and open it with MoBi. 
- *Optional: load your saved .pkml file with observed dissolution data created in the previous step by selecting the `Import/Export` tab and loading the saved simulation*
- Select the appropriate dissolution sub-model based on the available data. The following decision tree can assist in making this choice:

<img src="https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/Figures/DecisionTree_in-vitro-dissolution-model.png" width="558.6" height="495.6" />

## Usage

### 1. Setting up the simulations
Relevant parameters of the compound and formulation as well as the dissolution experiment are defined in the **Parameter Start Values** Building Block. The `API Properties` Building Block in this section contains the following parameters that may need to be adjusted (*Tip: clone the*  **Parameter Start Values** *Building Block and change the parameters according to the API properties and set-up of the experiments to be modeled*):

* `iBin`: Unique identifier for each particle size bin. This value should not be modified.
* `Volume`: Volume of the dissolution medium which can be either 500 mL or 900 mL.
* `Density (drug)`: Specific gravity of the drug.
* `Dose`: Mass of the drug used in the dissolution experiment.
* `Molecular weight`: Molar mass of the drug.
* `PrecipitatedDrugSoluble`: Boolean variable that indicates whether the precipitated drug can (re)dissolve (set value to `1`) or not (set value to `0`).
* `Thickness (unstirred water layer)`: Thickness of the unstirred water layer surrounding a drug particle, also referred to as diffusional boundary layer adjacent to the dissolving surface. This parameter is only used when the parameter `UseHydrodynamicModel` is set to `0` (see below for further explanations).
* `NBins`: Number of particle size bins utilized in the dissolution model, with a maximum allowed value of 10.
* `r_mean` and `r_gsd`: If particle sizes are assumed to be log-normally distributed, `r_mean` represents the geometric mean of the particle sizes, and `r_gsd` denotes the geometric standard deviation. These values are not used if the particle sizes for each bin are manually specified or fitted during parameter optimization.
* `unitC_1µm`: This unit conversion parameter is used in the calculation of the 'rel_amountFactor' for each particle bin when a log-normal distribution is used.
* `precipitationrate`: Precipitation rate of the drug, applicable only if `PrecipitatedDrugSoluble` is set to `0`.
* `pH`: pH of the dissolution medium. Table 1 lists pH of commonly used biorelevant media.

  **Table 1: pH values and bile salt concentrations of several biorelevant media**
    
  | Medium    | pH  | Bile Salt Concentration [mM] | Bile salt type      | Lecithin concentration [mM]| Source |
  |-----------|-----|------------------------------|---------------------|----------------------------|--------|
  | FaSSGF    | 1.6 | 0.08                         | Sodium taurocholate | 0.02                       | Markopoulos et al. [[3](#References)] |
  | FaSSIF-V1 | 6.5 | 3.0                          | Sodium taurocholate | 0.75                       | Markopoulos et al. [[3](#References)] |
  | FaSSIF-V2 | 6.5 | 3.0                          | Sodium taurocholate | 0.2                        | Markopoulos et al. [[3](#References)] |
  | FaSSCoF   | 7.8 | 0.15                         | Sodium cholate      | 0.3                        | Markopoulos et al. [[3](#References)] |
  | FEDGAS<sub>early</sub>  | 6.0 | 0.62           | Sodium taurocholate | *not reported*             | [https://biorelevant.com/learning_center/FEDGAS-composition](https://biorelevant.com/learning_center/FEDGAS-composition) |
  | FEDGAS<sub>middle</sub> | 4.5 | 0.62           | Sodium taurocholate | *not reported*             | [https://biorelevant.com/learning_center/FEDGAS-composition](https://biorelevant.com/learning_center/FEDGAS-composition) |
  | FEDGAS<sub>late</sub>   | 3.0 | 0.62           | Sodium taurocholate | *not reported*             | [https://biorelevant.com/learning_center/FEDGAS-composition](https://biorelevant.com/learning_center/FEDGAS-composition) |
  | FeSSIF-V1 | 5.0 | 15                           | Sodium taurocholate | 3.75                       | Markopoulos et al. [[3](#References)] |
  | FeSSIF-V2 | 5.8 | 10                           | Sodium taurocholate | 2.0                        | Markopoulos et al. [[3](#References)] |
  | FeSSCoF   | 6.0 | 0.6                          | Sodium cholate      | 0.5                        | Markopoulos et al. [[3](#References)] |

  
* `Reference pH`: pH of the solution in which the `Solubility at reference pH` of the drug is measured.
* `Solubility at reference pH`: Measured aqueous (thermodynamic) solubility of the drug.
* `Solubility gain per charge`: Factor by which the solubility increases with each ionization step of the drug. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value. 
* `Compound type {0...2}`: Defines the drug's ionization state(s); the following values can be used: `-1`: acid; `0`: neutral; `+1`: base
* `pKa value {0...2}`: p*K*<sub>a</sub> value for functional group `{0...2}` of the drug.
* `Immediately dissolve particles smaller than`: Threshold value below which drug particles do not continue to shrink through dissolution, but are immediately dissolved. This improves the computational speed of the model solving.
* `BS_Critical Micellar Concentration`: Molar concentration of bile salts or surfactants in a solution at which micelles start to form. Below this concentration, bile salts or surfactants primarily exist as individual molecules dispersed in the solution and no micelles are formed. Table 2 lists critical micellar concentrations for various surfactants reported in the literature.

  **Table 2: Critical micelle concentrations in aqueous solutions reported for several surfactants**
  
  | Medium | Surfactant(s)                | Surfactant Concentration or Ratio  | Critical Micellar Concentration [mM]  | Source |
  | ------ | ---------------------------- | -------------------| --------------------- | -------------- |
  | Aqueous solution | Sodium dodecyl sulfate (SDS) | na                       | 8.0 | Hammouda, B. [[2](#References)] |
  | Aqueous solution | SDS                          | 0.02%                    | 0.07 | Lehto et al. [[9](#References)] |
  | Aqueous solution | SDS                          | 0.1%                     | 3.5 | Lehto et al. [[9](#References)] |
  | Aqueous solution | SDS                          | 0.2%                     | 6.9 | Lehto et al. [[9](#References)] |
  | Aqueous solution | Tween 80                     | 0.014%                   | 0.11 | Lehto et al. [[9](#References)] |
  | Aqueous solution | Tween 80                     | 0.035%                   | 0.27 | Lehto et al. [[9](#References)] |
  | Aqueous solution | Tween 80                     | 0.07%                    | 0.53 | Lehto et al. [[9](#References)] |
  | Aqueous solution | Sodium taurocholate          | na                       | 6.0 | Gomez et al. [[7](#References)] |
  | Aqueous solution | Sodium taurocholate          | na                       | 0.4 | Lehto et al. [[9](#References)] |
  | Aqueous solution | Sodium taurocholate/phosphatidylcholine  | 4:1 | 0.6 | Gomez et al. [[7](#References)] |
  | FaSSIF-V1 | Sodium taurocholate/lecithin (20°C) | 4:1 | 4.9 | Gomez et al. [[7](#References)] |
  | FaSSIF-V1 | Sodium taurocholate/lecithin (25°C) | 4:1 | 4.7 | Gomez et al. [[7](#References)] |
  | FaSSIF-V1 | Sodium taurocholate/lecithin (35°C) | 4:1 | 4.2 | Gomez et al. [[7](#References)] |
  | FaSSIF-V1 | Sodium taurocholate/lecithin        | 4:1 | 1.4 | Xie et al. [[8](#References)] |
  | FaSSIF-V1 | Sodium taurocholate/lecithin        | 4:1 | 3.0 | Lehto et al. [[9](#References)] |
  
* `BS_Concentration`: Molar concentration of bile salts or surfactants present in the dissolution medium. Table 1 above lists molar concentrations of bile salts in commonly used biorelevant media.
* `BS_Log K_neutral`: Logarithm of the water-to-micelle partition coefficient for the unionized drug species. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value.
* `BS_Log K_ionized`: Logarithm of the water-to-micelle partition coefficient for the ionized drug species. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value.  
* `BS_C H2O`: Molar concentration of water. This parameter is temperature-dependent and might need to be adjusted for temperatures other than 37°C. It is used in the calculation of the biorelevant solubility of the drug.

* `Disintegration Lambda`: Variable used in the Weibull function to reduce the initial number of particles, applicable only if `enableTabletDisintegration` is set to `1`.
* `enableTabletDisintegration`: Boolean variable that indicates whether tablet disintegration is active (set value to `1`) or inactive (set value to `0`). If set to `1`, drug dissolution is initially slowed down by expressing the number of particles ($N$) as follows:

    $N = (1-\exp(-\lambda \frac{t^\alpha}{SA})) (N_\text{max} -1) +1$

  In this equation, $\alpha$ stands for `Disintegration Alpha`, $\lambda$ stands for `Disintegration Lambda`, $t$ represents time, $SA$ the surface area of the tablet, and $N_\text{max}$ the maximum number of particles.
* `Disintegration Alpha`: Variable used in the Weibull function to reduce the initial number of particles, applicable only if `enableTabletDisintegration` is set to `1`.

* `Micellar diffusion coefficient`: Diffusion coefficient of micelles. Generally, the diffusion coefficient of drug bound to micelles ($D_b$) is assumed to be equal to the micellar diffusion coefficient ($D_{mic}$), unless the **Effective Diffusion Model** is activated (see `UseEffectiveDiffusion`), in which case the effective diffusion coefficient ($D_{eff}$) is utilized instead of $D_{mic}$.
  
  Table 3 and 4 list values for $D_{mic}$ of different micelles that were calculated from the reported micelle size and the dynamic viscosity of water at 37°C (6.847 * 10<sup>-4</sup> Pa·s) using the Stokes-Einstein-Sutherland equation. 

  **Table 3: Diffusion coefficients of bile salt/lecithin micelles in biorelevant media calculated using the Stokes-Einstein-Sutherland equation**
  
  | Medium    | Temperature [°C] | Micelle type    | Bile Salt Concentration [mM] | Lecithin Concentration [mM] | Micelle diameter [nm] (Source)            | Calculated micellar diffusion coefficient [cm²/s] |
  | --------- | ---------------- | --------------- | ---------------------------- | --------------------------- | ----------------------------------------- | ------------------------------------------------- |
  | FaSSIF-V1 | 37 | Taurocholic acid/egg lecithin | 3.0                          | 0.75                        | 54.4 (Okazaki et al. [[4](#References)])  | 1.220 * 10<sup>-7</sup> |
  | FaSSIF-V1 | 37 | Sodium taurocholate/lecithin  | 3.0                          | 0.75                        | 61.5 (biorelevant.com [[5](#References)]) | 1.079 * 10<sup>-7</sup> |
  | FaSSIF-V2 | 25 | Sodium taurocholate/lecithin  | 3.0                          | 0.2                         | 20.8 (Pepin et al. [[6](#References)])    | 3.190 * 10<sup>-7</sup> |
  | FeSSIF-V1 | 37 | Taurocholic acid/egg lecithin | 15                           | 3.75                        | 6.3 (Okazaki et al. [[4](#References)])   | 1.053 * 10<sup>-6</sup> |
  | FeSSIF-V1 | 37 | Sodium taurocholate/lecithin  | 15                           | 3.75                        | 5.9 (biorelevant.com [[5](#References)])  | 1.125 * 10<sup>-6</sup> |
  | FeSSIF-V1 | 25 | Sodium taurocholate/lecithin  | 15                           | 3.75                        | 6.0 (Pepin et al. [[6](#References)])     | 1.106 * 10<sup>-6</sup> |
    
    
  **Table 4: Diffusion coefficients of sodium dodecyl sulfate (SDS) micelles in aqueous medium calculated using the Stokes-Einstein-Sutherland equation**
  
  | SDS Concentration [%] | Micelle radius [nm] (Source) | Calculated micellar diffusion coefficient [cm²/s] |
  | --------------------- | -------------------------------------------- | --------------------------------- |
  | 0.2%            | 1.26 (Hammouda et al. [[2](#References)]) | 2.633 * 10<sup>-6</sup> |
  | 0.5%            | 1.26 (Clifford and Pethica [[15](#References)], Hammouda et al.[[2](#References)]) | 1.813 * 10<sup>-6</sup> |
  | *not reported*  | 1.5 (Duplatre et al. [[16](#References)]) | 2.212 * 10<sup>-6</sup> |

* `UseEffectiveDiffusion`: Boolean variable that indicates how the thickness of the unstirred water layer surrounding particles of bound drug ($h_b$) is calculated. $h_b$ is calculated from the thickness of the unstirred water layer surrounding free drug particles ($h_u$) and the diffusion layer thickness ratio ($h_r$) as follows:

  $h_b = h_u \cdot h_r$

  If `UseEffectiveDiffusion` is set to `0`, $h_r$ is expressed as the cube root of the ratio between micellar diffusion coefficient ($D_{mic}$) and aqueous diffusion coefficient ($D_{aq}$ or $D_u$): $\sqrt[3]{\frac{D_{mic}}{D_{aq}}}$. 

  If `UseEffectiveDiffusion` is set to `1`, $h_r$ is 1 and $h_b$ equals $h_u$. Additionally, the diffusion coefficients of both free and bound drug ($D_u$ and $D_b$) are combined into the effective diffusion coefficient ($D_{eff}$) which is calculated as follows:

  $D_{eff} = \frac{S_{aq}}{S_{br}} \cdot D_u + D_{mic} (1-\frac{S_{aq}}{S_{br}})$

  Here, $D_u$ represents the diffusion coefficient of the unbound drug (equivalent to the aqueous diffusion coefficient, $D_{aq}$), $D_{mic}$ the micellar diffusion coefficient, $S_{aq}$ the aqueous solubility, and $S_{br}$ the biorelevant solubility.

  Therefore, if `UseEffectiveDiffusion` is set to `1`, bound and unbound drug species are treated as equivalent species, resulting in $h_b$ = $h_u$ and $D_u = D_{mic} = D_{eff}$. This model should be used cautiously when extrapolating to other experimental conditions.
  

* `H_Radius paddle max (H)`, `H_radius paddle min (I)`, `H_Radius vessel (R)`, `H_Distance between paddle and vessel bottom (X)`, `H_Height of the paddle (B)`, `H_Shaft diameter (Q)`, and `H_T`: These parameters correspond to various geometrical measurements of the dissolution apparatus and vessel. They are pre-defined for the USP-2 dissolution apparatus but may require adjustments for use with other dissolution apparatuses. Pepin et al. [[10](#References)] report the following values for the USP2 dissolution appartus:
  
  **Table 5: Geometrical dimensions of the USP2 dissolution apparatus according to Pepin et al. [[10](#References)]**
  
  | Variable | Parameter name           | Value | Unit |
  | -------- | ------------------------ | ----- | ---- |
  | H        | Maximum paddle radius    | 37.0  | mm   |
  | I        | Minimum paddle radius    | 21.0  | mm   |
  | R        | Vessel internal radius   | 50.5  | mm   |
  | X        | Distance between paddle and vessel bottom | 25.0 | mm |
  | B        | Height of the paddle     | 19.0  | mm   |
  | Q        | Shaft diameter           | 9.8   | mm   |
  | H_T      | Thickness of the paddle  | 4.0   | mm   |

  Further information on the geometry of the USP2 dissolution appartus can be found in Pepin et al. [[10](#References)].
  
* `H_RPM`: Revolutions per minute indicating how quickly the stirrer of the dissolution apparatus is rotating, which affects the hydrodynamics of the dissolution medium and the rate at which the drug dissolves.
* `H_Temperature`: Used to calculate the density of water. When changing this parameter from 37 <sup>o</sup>C, other parameters might need to be adjusted manually (e.g., solubility related parameters, p*K*<sub>a</sub>, diffusion coefficients)
* `H_Volume border`: Determines which experimentally fitted cubic function is used to calculate the `H_Power number` inputted by the USP-2 paddle. The default value (600 mL) is based on the experimental data used by Pepin et al. [[10](#References)] with 500 ml and 900 ml of fluid in the USP-2 vessel.
* `H_Power per mass correction factor`: Correction factor as the unit W/kg is not included in the OSP dimension database.
* `H_Gravitational acceleration`: Acceleration due to earth's gravity (constant).
* `H_gCor`: Correction factor for `H_Gravitational acceleration`, as unit of acceleration (m/s<sup>2</sup>) is not included in the OSP dimension database.
* `H_K limit value`: Threshold to determine whether the dissolving drug particle moves with the fluid (the kinetic forces acting on the particle are higher than the resistant forces) or resists movement (particle resistance energy is higher than the particle kinetic energy when moving with the fluid). The value of 1 means that the ration between the kinetic and resistance energy is ≥ 1 the particle moves with the fluid and if the ratio is < 1 the particle resists movement, which affects the calculation method of the Reynolds number.

* `UseHydrodynamicModel` and `UseHintzJohnson`: Boolean variables that indicate how the thickness of the unstirred water layer surrounding particles of free drug ($h_u$) is calculated. Generally, the thickness of the unstirred water layer primarily depends on two factors: the size of the dissolving drug particles and the agitation rate of the surrounding solution. The following models are implemented to describe $h_u$:
  
    * **Hydrodynamic model**: Among the various models implemented, the hydrodynamic model is the most mechanistic approach. It is based on the framework presented by Pepin et al. [[10](#References)] and calculates the thickness of the unstirred water for unbound drug ($h_u$) from the current particle radius ($r$) and the Sherwood number ($Sh$):
      
      $h_u = \frac{2r}{Sh}$

      The Sherwood number characterizes the mass transfer of dissolved drug molecules from the surface of particles into the surrounding fluid and is influenced by various key parameters related to the system, such as agitation rate and viscosity, as well as the dissolution process, including particle size and diffusion characteristics. A higher Sherwood number indicates that convective mass transfer plays a significant role compared to diffusion, thereby enhancing the dissolution rate of the drug particles. In contrast, a lower Sherwood number suggests that diffusion is the limiting factor in the dissolution process. For detailed information on its calculation, please refer to the article by Pepin et al. [[10](#References)] (The implemented Model 3 in specific).

      This model is used if the variable `UseHydrodynamicModel` is set to `1`. 

    * **Hintz-Johnson model**: A simplified model proposed by Hintz and Johnson [[11](#References)] which assumes that $h_u$ is equal to the particle radius for particles with a radius smaller than the specified value for `Thickness (unstirred water layer)`. For larger particles that exceed this value, $h_u$ is assumed to be equal to `Thickness (unstirred water layer)`. This assumption has been validated for small particles (<20 to 100 μm) in a USP II paddle device at a paddle speed of 100 rpm (Sheng et al. [[12](#References)]). However, at a lower speed of 50 rpm, $h_u$ approximates $\sqrt{r}$ (Sheng et al. [[12](#References)], Niebergall et al. [[13](#References)]).
      
      Therefore, this model does not explicitly account for the agitation rate of the medium and should be used cautiously when extrapolating to other experimental conditions. It is utilized if the variable `UseHydrodynamicModel` is set to `0` and the variable `UseHintzJohnson`is set to `1`. 
 
    * **Orginal Particle Dissolution Model**: This model is the simplest approach and is based on the framework proposed by Willmann et al. [[14](#References)]. It assumes that $h_u$ is constant and equal to the value specified for `Thickness (unstirred water layer)`. In the model presented by Willmann et al. [[14](#References)], a value of 30 µm is used for the `Thickness (unstirred water layer)`.
      
      Therefore, this model should be used cautiously when extrapolating to other experimental conditions. It is utilized if the variable `UseHydrodynamicModel` is set to `0` and the variable `UseHintzJohnson`is set to `0`. 

* `Surface integration factor`: Parameter that can be used to modify precipitation kinetics. Setting it to a very high value will prevent precipitation almost completely.

When all parameter start values are cloned and populated with the correct parameter values, you can create the simulations for all the experiments that you want to model.

### 2. Adopting the Product-Particle Size Distribution (P-PSD) approach
* Create a Parameter Identification with all experiments of interest in the ‘Data’ tab for a specific formulation/Batch.
  * Set weight of all experiments to 0 except for one based on:
    * Preferably medium does not contain surfactants.
    * Maximize the datapoints on the dissolution slope, preferably as many points between 20-80% of the dose dissolved.
  * In the ‘Parameters’ tab select the `r_mean` and `r_gsd` parameters – for log-normal distribution fitting, or `radius (at t=0)` and `rel_amountFactor` for all bins of the formulation.
    * Log-normal distribution fitting: Fit, Monte – Carlo might be necessary for fitting.
    * Separate bins: Fix `radius (at t=0)` for all bins and cover a wide range with the 10 particle bins and run the optimization.
      * After this initial run you know roughly in which range you should specify your particle radius bins.
      * For computational load reasons it is advisable to end up with the least amount of bins that are still able to capture the observed data in the different experiments. Do this by setting the `rel_amountFactor` of a bin to 0 and clicking the 'fix parameter box'.

* In the ‘Configuration’ tab it is advisable to test optimization with both the Levenberg – Marquardt and  Monte – Carlo optimization algorithms with multiple runs.

* In general there is some trial and error involved during this step of the PBBM development, try different experiments to optimize the P-PSD to if the initial one does not work. Also try many initial particle sizes and look at your resulting Time Profiles to give an indication whether the particle size of some bins should be increased (slow down dissolution) or decreased (increase dissolution rate) which will affect the different dissolution phases in you dissolution profiles.

### 3. Exporting P-PSD to PK-Sim
* Follow step 3 from the [IVIVC workflow](https://github.com/Open-Systems-Pharmacology/IVIVC-with-particle-dissolution-module-in-OSP/tree/master) to create your formulation building block which you can use in PK-Sim. Download the input and output files included there as you will need some of them.
  * Replace in `PSV_CompoundA_BatchX_lognormal.xlsx` (Output_files) the initial radii and rel_amounts to your fitted values and create the building blocks using the `JSON.R` script (Input_files).
  * You might need to adjust `BB_Formulation_ParticleDissolution_10Bins_input.json` (Input_files) when you use less than 10 bins, alternatively you could set rel_amounts of the not used extra bins to 0 (*I have not tested this latter approach*)
  * Run `JSON.R` conversion script

* Load your PK-Sim project file in developer mode and load the JSON formulation Building Block from snapshot.
  

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 1: in vitro modeling of Vericiguat. *Eur J Pharm Sci.* 2025.](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow)

[2] [Hammouda B. Temperature Effect on the Nanostructure of SDS Micelles in Water. J Res Natl Inst Stand Technol. 2013 Apr 11;118:151-67. doi: 10.6028/jres.118.008. PMID: 26401428; PMCID: PMC4487321.](https://pubmed.ncbi.nlm.nih.gov/26401428/)

[3] [Markopoulos C, Andreas CJ, Vertzoni M, Dressman J, Reppas C. In-vitro simulation of luminal conditions for evaluation of performance of oral drug products: Choosing the appropriate test media. Eur J Pharm Biopharm. 2015 Jun;93:173-82. doi: 10.1016/j.ejpb.2015.03.009. Epub 2015 Mar 30. PMID: 25836053.](https://pubmed.ncbi.nlm.nih.gov/25836053/)

[4] [Okazaki A, Mano T, Sugano K. Theoretical dissolution model of poly-disperse drug particles in biorelevant media. J Pharm Sci. 2008 May;97(5):1843-52. doi: 10.1002/jps.21070. PMID: 17828749.](https://pubmed.ncbi.nlm.nih.gov/17828749/)

[5] [biorelevant: Product information](https://biorelevant.com)

[6] [Pepin XJH, Sanderson NJ, Blanazs A, Grover S, Ingallinera TG, Mann JC. Bridging in vitro dissolution and in vivo exposure for acalabrutinib. Part I. Mechanistic modelling of drug product dissolution to derive a P-PSD for PBPK model input. Eur J Pharm Biopharm. 2019 Sep;142:421-434. doi: 10.1016/j.ejpb.2019.07.014. Epub 2019 Jul 12. PMID: 31306753.](https://pubmed.ncbi.nlm.nih.gov/31306753/)

[7] [Gomez, S. M., Cristancho, D. M., & Martinez, F. (2012). Physicochemical Aspects of the Solubilization of Ibuprofen in Biorelevant Media: Modified and Classical FaSSIF Systems. Lat. Am. J. Pharm, 31(9), 1261-9.](https://www.academia.edu/download/52136815/Physicochemical_aspects_of_the_solubiliz20170313-28557-163u2hp.pdf)

[8] [Xie X, Cardot JM, Garrait G, Thery V, El-Hajji M, Beyssac E. Micelle dynamic simulation and physicochemical characterization of biorelevant media to reflect gastrointestinal environment in fasted and fed states. Eur J Pharm Biopharm. 2014 Oct;88(2):565-73. doi: 10.1016/j.ejpb.2014.05.020. Epub 2014 Jun 19. PMID: 24954150.](https://pubmed.ncbi.nlm.nih.gov/24954150/)

[9] [Lehto P, Kortejärvi H, Liimatainen A, Ojala K, Kangas H, Hirvonen J, Tanninen VP, Peltonen L. Use of conventional surfactant media as surrogates for FaSSIF in simulating in vivo dissolution of BCS class II drugs. Eur J Pharm Biopharm. 2011 Aug;78(3):531-8. doi: 10.1016/j.ejpb.2011.02.007. Epub 2011 Feb 15. PMID: 21329757.](https://pubmed.ncbi.nlm.nih.gov/21329757/)

[10] [Pepin X, Goetschy M, Abrahmsén-Alami S. Mechanistic Models for USP2 Dissolution Apparatus, Including Fluid Hydrodynamics and Sedimentation. J Pharm Sci. 2022 Jan;111(1):185-196. doi: 10.1016/j.xphs.2021.10.006. Epub 2021 Oct 16. PMID: 34666045.](https://pubmed.ncbi.nlm.nih.gov/34666045/)

[11] [Hintz, R. J., & Johnson, K. C. (1989). The effect of particle size distribution on dissolution rate and oral absorption. International Journal of Pharmaceutics, 51(1), 9-17.](https://www.sciencedirect.com/science/article/pii/0378517389900690)

[12] [Sheng JJ, Sirois PJ, Dressman JB, Amidon GL. Particle diffusional layer thickness in a USP dissolution apparatus II: a combined function of particle size and paddle speed. J Pharm Sci. 2008 Nov;97(11):4815-29. doi: 10.1002/jps.21345. PMID: 18314890.](https://pubmed.ncbi.nlm.nih.gov/18314890/)

[13] [Niebergall PJ, Milosovich G, Goyan JE. Dissolution rate studies. II. Dissolution of particles under conditions of rapid agitation. J Pharm Sci. 1963 Mar;52:236-41. doi: 10.1002/jps.2600520310. PMID: 13938476.](https://pubmed.ncbi.nlm.nih.gov/13938476/)

[14] [Willmann S, Thelen K, Becker C, Dressman JB, Lippert J. Mechanism-based prediction of particle size-dependent dissolution and absorption: cilostazol pharmacokinetics in dogs. Eur J Pharm Biopharm. 2010 Sep;76(1):83-94. doi: 10.1016/j.ejpb.2010.06.003. Epub 2010 Jun 8. PMID: 20554023.](https://pubmed.ncbi.nlm.nih.gov/20554023/)

[15] [Clifford, J.and Pethica B.A. The Self-Diffusion Coefficient of Sodium Dodecyl Sulfate Micelles.  The Journal of Physical Chemistry 1966  70(10): 3345-3346. doi: 10.1021/j100882a506.](https://doi.org/10.1021/j100882a506)

[16] [Duplâtre G, Ferreira Marques M. F., da Graça Miguel M. Size of Sodium Dodecyl Sulfate Micelles in Aqueous Solutions as Studied by Positron Annihilation Lifetime Spectroscopy. The Journal of Physical Chemistry 1996 100(41):16608-16612. doi: 10.1021/jp960644m.](https://doi.org/10.1021/jp960644m)




