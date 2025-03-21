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
* `unitC_1µm`: This parameters is used in the calculation of different parameters related to the fluid velocity (such as the Reynolds number, micro-eddie velocity, and terminal velocity) according to the equations presented by Sugano et al.**The final dissolution model does not use the Sugano equations; hence, this parameter is not needed and can be deleted from the model file (?)**
* `precipitationrate`: Precipitation rate of the drug, applicable only if `PrecipitatedDrugSoluble` is set to `0`.
* `pH`: pH of the of dissolution medium. Table 1 lists pH of commonly used biorelevant media.
  
    **Table 1: pH values and bile salt concentrations of several biorelevant media**
    | Medium    | pH  | Bile Salt Concentration [mM] | Bile salt type | Lecithin concentration [mM] |
    | --------- | --- | ---------------------------- | -------------- | --------------------------- |
    | FaSSGF    | 1.6 | 0.08 | Sodium taurocholate | 0.02 |
    | FaSSIF-V1 | 6.5 | 3.0 | Sodium taurocholate | 0.75 |
    | FaSSIF-V2 | 6.5 | 3.0 | Sodium taurocholate | 0.2 |
    | FaSSCoF   | 7.8 | 0.15 | Sodium cholate | 0.3 |
    | FeSSIF-V1 | 5.0 | 15  | Sodium taurocholate | 3.75 |
    | FeSSIF-V2 | 5.8 | 10  | Sodium taurocholate | 2.0 |
    | FeSSCoF   | 6.0 | 0.6 | Sodium cholate | 0.5 |
    
    Source: Markopoulos et al. [[3](#References)].
  
* `Reference pH`: pH of the solution in which the `Solubility at reference pH` of the drug is measured.
* `Solubility at reference pH`: Measured aqueous (thermodynamic) solubility of the drug.
* `Solubility gain per charge`: Factor by which the solubility increases with each ionization step of the drug. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value. 
* `Compound type {0...2}`: Defines the drug's ionization state(s); the following values can be used: `-1`: acid; `0`: neutral; `+1`: base
* `pKa value {0...2}`: p*K*<sub>a</sub> value for functional group `{0...2}` of the drug.
* `Immediately dissolve particles smaller than`: Threshold value below which drug particles do not continue to shrink through dissolution, but are immediately dissolved. This improves the comptutaional speed of the model solving.
* `BS_Critical Micellar Concentration`: Molar concentration of bile salts or surfactants in a solution at which micelles start to form. Below this concentration, bile salts or surfactants primarily exist as individual molecules dispersed in the solution and no micelles are formed. Table 2 lists critical micellar concentrations for various surfactants reported in the literature.
  
    **Table 2: Critical micelle concentrations in aqueous solutions reported for several surfactants**
    | Medium | Surfactant(s)                | Surfactant Concentration or Ratio  | Critical Micellar Concentration [mM]  | Source |
    | ------ | ---------------------------- | ---------------------------------- | ------------------------------------- | -------------- |
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
* `BS_Lok K_neutral`: Logarithm of the water-to-micelle partition coefficient for the unionized drug species. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value.
* `Disintegration Lambda`: Variable used in the Weibull function to reduce the initial number of particles, applicable only if `enableTabletDisintegration` is set to `1`.
* `enableTabletDisintegration`: Boolean variable that indicates whether tablet disintegration is active (set value to `1`) or inactive (set value to `0`). If set to `1`, drug dissolution is initially slowed down by multiplying the number of particles in the dissolution equation with a time-dependent factor that increases from 0 to 1 according to a Weibull function according to the following function:

    ![image](https://github.com/user-attachments/assets/340cd142-8827-4dd1-b300-2b6ffa49ea99)

* `Disintegration Alpha`: Variable used in the Weibull function to reduce the initial number of particles, applicable only if `enableTabletDisintegration` is set to `1`.
* `BS_Lok K_ionized`: Logarithm of the water-to-micelle partition coefficient for the ionized drug species. This parameter can be fitted using the OSP Solubility Toolbox and should be updated with the fitted value.
* `BS_C H2O`: Molar concentration of water. This parameter is temperature-dependent and might need to be adjusted for temperatures other than 37°C. It is used in the calculation of the biorelevant solubility of the drug. 
* `H_Radius paddle max (H)`, `H_radius paddle min (I)`, `H_Radius vessel (R)`, `H_Distance between paddle and vessel bottom`, `H_Height of the paddle (B)`, `H_Shaft diameter (Q)`, and `H_T`: These parameters correspond to various geometrical measurements of the dissolution apparatus and vessel. They are pre-defined for the USP-2 dissolution apparatus but may require adjustments for use with other dissolution apparatuses.
* `H_RPM`: Revolutions per minute indicating how quickly the stirrer of the dissolution apparatus is rotating, which affects the hydrodynamics of the dissolution medium and the rate at which the drug dissolves.
* `H_Temperature`: Used to calculate the density of water. **Only used for Sugano implementation or also for Pepin model?**
* `Micellar diffusion coefficient`: Diffusion coefficient of micelles. The diffusion coefficient of drug bound to micelles (D<sub>b</sub>) is assumed to be equal to the micellar diffusion coefficient. Table 3 and 4 list micellar diffusion coefficients for different micelle types calculated from the reported micelle size and the dynamic viscosity of water at 37°C (6.847 * 10<sup>-4</sup> Pa·s) using the Stokes-Einstein-Sutherland equation. 

    **Table 3: Diffusion coefficient of bile salt/lecithin micelles in biorelevant media calculated using the Stokes-Einstein-Sutherland equation**
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
    | --------------------- | ---------------------------- | ------------------------------------------------- |
    | 0.2%            | 1.26 (Hammouda 2013) | 2.633 * 10<sup>-6</sup> |
    | 0.5%            | 1.26 (Clifford and Pethica 1966 + Hammouda 2013]) | 1.813 * 10<sup>-6</sup> |
    | *not reported*  | 1.5 (Duplatre 1996) | 2.212 * 10<sup>-6</sup> |


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



