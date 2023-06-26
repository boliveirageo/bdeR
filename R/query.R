library(httr)
library(jsonlite)


#Object apps in API
apps = data.frame(
  "urlMain" = c("https://painelmunicipal.imb.go.gov.br/visao/"),
  "Location" = c("localidade.php?formatado=0&json=1&codigolocalidade=&codigoibge="),
  "Variables" = c("variavel.php?formatado=0&json=1&codigovariavel=")
  
) 

#Function to get Locations/municipalities from BDE
getLocation = function(){
  
  urldata = paste(apps$urlMain,apps$Location,sep="")
  dataApi = GET(urldata)
  
  datadf = fromJSON(rawToChar(dataApi$content),flatten = TRUE)
  return(datadf)
}

#Function to get variables from BDE
getVariables = function(codVar=NULL){
  
  parameter = paste(apps$Variables,codVar,sep="")
  urldata = paste(apps$urlMain,parameter,sep="")
  
  dataApi = GET(urldata)
  
  datadf = fromJSON(rawToChar(dataApi$content),flatten = TRUE)
  return(datadf)
}

#Function to get 
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
  
  
  urldata = paste(apps$urlMain,parameter,sep="")
  print(urldata)
  dataApi = GET(urldata)
  
  datadf = fromJSON(rawToChar(dataApi$content),flatten = TRUE)
  return(datadf)
}


locations = getLocation()
varia = getVariables(codVar=15)
data = getData(codVar='15;17',timeseries=5)
