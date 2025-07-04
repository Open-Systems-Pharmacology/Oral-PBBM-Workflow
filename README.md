# Oral-PBBM-workflow
Within this repository, we distribute the physiologically-based biopharmaceutics (PBBM) workflow for orally administered drug products that was previously published by Vrenken et al. [[1](#References),[2](#References)]. The workflow consists of three tools that build upon each other:

![gim](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/Figures/PBBM_workflow.png)

The following decision tree assists in determining which features of the OSP Solubility Toolbox and the in vitro dissolution model are relevant based on the type of input provided.

![gim](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/blob/main/Figures/DecisionTree.png)

TTemplate files for each of these tools are available in their respective sub-folders. These templates are ready to use and need to be populated with your own data. 

The application of this workflow to vericiguat, along with the corresponding files, can be found in the "Vericiguat" sub-folder.

## OSP Solubility Toolbox

The OSP Solubility Toolbox has been presented previously by Vrenken et al. [[1](#References)]. The toolbox is a Shiny App based on R which can be found, along with a detailed guidance for users, in the respective [sub-folder](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/tree/main/OSP-Solubility-Toolbox).

## In Vitro Dissolution Model

The in vitro dissolution model has been presented previously by Vrenken et al. [[1](#References)]. A MoBi template file integrating the novel dissolution model as well as a detailed guidance for users can be found in the respective [sub-folder](https://github.com/Open-Systems-Pharmacology/Oral-PBBM-Workflow/tree/main/In-Vitro-Dissolution-Model).

## PBPK Model Upgrade

The PBPK model upgrade incorporating relevant changes required for this PBBM workflow will be made available after publication of the respective article by Vrenken et al. [[2](#References)].

## Code of conduct
Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md#contributor-covenant-code-of-conduct).

## Contribution
We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md#ways-to-contribute). If you are contributing code, please be familiar with the [coding standard](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS.md#visual-studio-settings).

## License
The framework is distributed under the [GPLv2 License](https://github.com/Open-Systems-Pharmacology/Suite/blob/develop/LICENSE).

## References
[1] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 1: in vitro modeling of Vericiguat. *Eur J Pharm Sci.* 2025 Jun 10;212:107164. doi: 10.1016/j.ejps.2025.107164. Epub ahead of print. PMID: 40505839](https://www.sciencedirect.com/science/article/pii/S0928098725001630)

[2] [Vrenken P, Vertzoni M, Frechen S, Solodenko J, Meyer M, Muenster U, Dallmann A. Development of a novel Physiologically Based Biopharmaceutics modeling (PBBM) framework using the Open Systems Pharmacology Suite, Part 2: in vivo pharmacokinetic modeling of vericiguat. Eur J Pharm Sci.* 2025 July 2; 107189. doi: 10.1016/j.ejps.2025.107189. Epub ahead of print.](https://www.sciencedirect.com/science/article/pii/S0928098725001885)
