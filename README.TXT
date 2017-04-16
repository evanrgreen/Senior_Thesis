Senior Thesis in Applied Math for Evan Green

To run the R files on your local computer, you should first clone the repository. Then, you will need to change the path in the first chunk of each file to the indicate where you have put the folder entitled “Grade Data.” Additionally, if you do not have all the packages that are imported in the first chunk installed, you will need to install those before the code will run. You can also run Install_Packages.R in the R Files folder. 

There are 4 R files that were integral to the project. They are all in the R Files folder. 

Analyze_Grade_Networks.RMD

The file preforms the majority of analysis about the network properties. This includes creating random graphs to test for the likelihood of different observed properties, calculating and visualizing the average grades and degree distribution by gender and analyzing the factors that lead to more stable edges.

Grade_Regressions.RMD

This file tests and validates various models that predicted the grades of students based on different assumptions and available information. 

Imputing_Missing_Tests.RMD

This file tests different models for imputing the excused absences on tests and outputs a new file that has a complete, imputed set of grades. 

Visualize_Network.RMD

This file uses a few different packages to create information visuals of the network itself.
