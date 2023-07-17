library(httr)
library(jsonlite)


#Object apps in API
apps = data.frame(
  "urlMain" = c("https://painelmunicipal.imb.go.gov.br/visao/"),
  "Location" = c("localidade.php?formatado=0&json=1&codigolocalidade=&codigoibge="),
  "Variables" = c("variavel.php?formatado=0&json=1&codigovariavel=")

)

#' Function to get Locations/municipalities from BDE
#'
#' This is function get all  Locations/municipalities from Goias in Statistics Database
#' Not require parameters
#'
#' @example getLocation()

getLocation = function(){

  urldata = paste(apps$urlMain,apps$Location,sep="")
  dataApi = httr::GET(urldata)

  datadf = jsonlite::fromJSON(rawToChar(dataApi$content),flatten = TRUE)
  return(datadf)
}

#Function to get variables from BDE
#'
#' This is function get all  Locations/municipalities from Goias in Statistics Database
#'
#' @param codVar Variable code in Statistics Database. Parameter not required
#'
#' @examples getVariables() All variables in tatistics Database
#'           getVariables(codVar=1) Get information variable code 1
#'

getVariables = function(codVar=NULL){

  parameter = paste(apps$Variables,codVar,sep="")
  urldata = paste(apps$urlMain,parameter,sep="")

  dataApi = httr::GET(urldata)

  datadf = jsonlite::fromJSON(rawToChar(dataApi$content),flatten = TRUE)
  return(datadf)
}

#Function to get all data from Statistics Database
#'
#' This is function get all  Locations/municipalities from Goias in Statistics Database
#'
#' @param codVar Variable code in Statistics Database. Parameter not required
#' @param codibge IBGE code. But insert 'T' to get all Locations/municipalities from Goias in Statistics Database
#' @param initialyear
#' @param finalyear
#' @param timeseries
#'
#' @example getdata(codVar='1;2') Access data from Statistics Database of IMB in variables code of 1 and 2

getData = function(codVar=15,codibge='T',initialyear=NULL,finalyear=NULL,
                   timeseries=NULL){

  if (length(initialyear) == 0){
    if (length(finalyear) == 0){
      ultimoano <- paste('|',toString(1),sep='')
      periodo <- paste('|',NULL,sep='')
    }
  }else{
    ultimoano <- paste('|',toString(1),sep='')
    periodo <- paste('|',toString(1),sep='')
  }

  #Parameters
  codibge < paste('|',toString(codibge),sep='')
  codvarbde <- paste('|',toString(codVar),sep='')
  anoinicial <- paste('|',initialyear,sep='')
  anofinal <- paste('|',finalyear,sep='')
  ultimoano <- ultimoano
  periodo <- periodo
  seriehistorica <- paste('|',toString(timeseries),sep='')
  auxvar <- paste('|',toString(1),sep='')
  auxund <- paste('|',toString(1),sep='')
  auxvarfnt <-paste('|',toString(1),sep='')
  auxfnt <- paste('|',toString(1),sep='')
  auxvarnota <- paste('|',toString(1),sep='')
  auxnota <- paste('|',toString(1),sep='')

  parameter <- paste('dados.php?parametros=0|1||',codibge,codvarbde,
                     anoinicial,anofinal,ultimoano,periodo,seriehistorica,
                     auxvar,auxund,auxvarfnt,auxfnt,auxvarnota,auxnota,sep="")

  #Generating the api address
  urldata = paste(apps$urlMain,parameter,sep="")

  #Accessing data from API
  dataApi = httr::GET(urldata)
  datadf = jsonlite::fromJSON(rawToChar(dataApi$content),flatten = TRUE)

  #Create dataframe empty
  df = data.frame()

  #Loop all rows in dataset
  for(i in rownames(datadf)){
    #Create new empty row
    rows = c()
    print(i)
    for(j in colnames(datadf)){

      #Verify columns in dataset.
      col = strsplit(j, split='.',fixed=T)[[1]]
      if(col[1] == 'anos'){
        if (!is.na(datadf[i,j])){
          rows2 = rows
          rows2["ano"] = col[2]
          rows2["valor"] = datadf[i,j]

          newdf = data.frame(t(rows2))
          df = rbind(df,newdf)
        }
      }else{
        if(col[1] != 'fontes'){
          rows[j] = datadf[i,j]
        }
      }
    }
  }

  #Return dataset
  return(df)
}

data <- getData(codVar='1;2')
