R-scripts to generate figures for paper:

Feasibility of peak temperature targets in light of institutional constraints

Christoph Bertram, Elina Brutschin, Laurent Drouet, Gunnar Luderer, Bas van Ruijven, Lara Aleluia Reis, Luiz Bernardo Baptista, Harmen-Sytze de Boer, Ryna Cui, Vassilis Daioglou, Florian Fosse, Dimitris Fragkiadakis, Oliver Fricko, Shinichiro Fujimori, Nate Hultman, Gokul Iyer, Kimon Keramidas, Volker Krey, Elmar Kriegler, Robin D Lamboll, Rahel Mandaroux, Pedro Rochedo, Joeri Rogelj, Roberto Schaeffer,Diego Silva, Isabela Tagomori, Detlef van Vuuren, Zoi Vrontisi, Keywan Riahi

https://doi.org/10.1038/s41558-024-02073-4

Script 1 generates the governance input data for the IAMs, as well as Figure 1 and Extended Data Figure 1.
Script 2 reads in scenario results from the IAMs (which needs to be downloaded separately from zenodo), and generates Figures 3, and 4 of the main paper, and Extended Data Figures 2, 3

Folder structure:

- data: includes the data on carbon budgets from Forster et al. 2023 and governance effectiveness indicators from Andrijevic et al., and further requires the data file from the zenodo scenario data repository (expected name 

- mappings: includes the country ISO to native model region mappings for 9 models and the IPCC R10 region aggregation (similar mapping for additional model would enable creation of input data set for further models, and name would need to be included in for loop in script 1)

- plots: this is where plots are saved to

- output: this is where governance files (csv and cs4r format) are saved to (which then were used as input to IAM models)


(the plots were done with R version 4.3.0, but the code was tested to be robust with other versions too)