# PBPK Model

This repository contains the necessary files to run a beta version of PK-Sim that integrates the PBBM features presented by Vrenken et al. [[1](#References)]. These features will be officially implemented in PK-Sim version 13. 

## Getting started
To get started with the beta version, please follow these steps:

* Download PK-Sim portable (version 11.2 or higher): 
    * version [11.2](https://github.com/Open-Systems-Pharmacology/PK-Sim/releases/tag/v11.2.142)
    * version [11.3](https://github.com/Open-Systems-Pharmacology/PK-Sim/releases/tag/v11.3.208)
    * version [12.0](https://github.com/Open-Systems-Pharmacology/PK-Sim/releases/tag/v12.0.440)
* Extract the contents of the downloaded zip folder.
* Navigate to the installation sub-folder and replace the existing PK-Sim database (`PKSimDB.sqlite`) and  OSP Suite dimension database (`OSPSuite.Dimensions.xml`) with the corresponding files provided in this repository.
* Launch the upgraded beta version by double-clicking on `PK-Sim.exe`.

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 2: in vivo pharmacokinetic modeling of vericiguat. Eur J Pharm Sci.* 2025 July 2; 107189. doi: 10.1016/j.ejps.2025.107189. Epub ahead of print.](https://www.sciencedirect.com/science/article/pii/S0928098725001885)
