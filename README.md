## bdeR - a library in R to access IMB data



This is a module to access data from IMB (Mauro Borges Statistic and Socioeconomic Institute). Such information
can be found at https://painelmunicipal.imb.go.gov.br/ by entering variable code, IBGE code or location IMB code.

Install from github: 

 install.packages("devtools")
 
 devtools::install_github("boliveirageo/bdeR")


 Usage:

      #Import library
      library(bdeR)
      
      #Variables information from Statistics Database of IMB 
      variables <- getVariables()
      
      #Municipalites information from Statistics Database of IMB
      location <- getLocation()
      
      #Access data from Statistics Database of IMB in variables code of 1 and 2.
      data <- getData(codVar='1;2')
