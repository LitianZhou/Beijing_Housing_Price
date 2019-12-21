### Illustrate examples of connecting to/ close connection and query

library(RPostgreSQL)

# parameters for connecting to database
pg = dbDriver("PostgreSQL")

endpoint <- 'beijing-housing.copmdh9kwiqr.us-east-2.rds.amazonaws.com'
portnum <- 5432
username <- 'biostat625'
pwd <- 'shinygroup2'

# set up connection object
con = dbConnect(pg, user=username, password=pwd,
                host=endpoint, port=portnum, dbname='housing')

gen_data = function(district, bdtypes){
  querystring = "SELECT price, totalprice, square, bathroom, buildingtype, constructiontime, renovationcondition, 
            elevator, subway, district, tradetime, year, season, lng, lat FROM beijinghousing %s;"
  
  finalconstraint = ""
  disconstraint = ""
  bdconstraint = ""
  multidistrict = TRUE
  multibdtype = TRUE
  inference = TRUE
  
  if (district != "all"){ # there is a district constraint
    disconstraint = sprintf("district = '%s'", district)
    multidistrict = FALSE
  }
  if (length(bdtypes) < 3){ # there is a buildingtype constraint
    if (length(bdtypes) == 1){
      multibdtype = FALSE
      bdconstraint = sprintf("buildingtype = '%s'", bdtypes)
    } else if(length(bdtypes) == 2){
      bdconstraint = sprintf("buildingtype IN ('%s', '%s')", bdtypes[1], bdtypes[2])
    }
  }
  
  if (disconstraint !="" & bdconstraint !=""){
    finalconstraint = sprintf("WHERE %s AND %s", disconstraint, bdconstraint)
  } else if(disconstraint != ""){
    finalconstraint = sprintf("WHERE %s", disconstraint)
  } else if(bdconstraint != ""){
    finalconstraint = sprintf("WHERE %s", bdconstraint)
  } else{
    finalconstraint = ""
  }
  
  querystring = sprintf(querystring, finalconstraint)
  result = dbGetQuery(con, querystring)
    
  if(dim(result)[1] < 70){ inference = FALSE }
  
  return(list("data"=result, "multidistrict" = multidistrict, "multibdtype" = multibdtype, "inference"=inference))
}

gen_full_data = function(district, bdtypes){
  querystring = "SELECT price, totalprice, square, livingroom, drawingroom, kitchen, bathroom, floor, buildingtype, constructiontime, renovationcondition, 
            elevator, subway, district, tradetime, year, season, lng, lat, buildingstructure, fiveyearsproperty FROM beijinghousing %s;"
  
  finalconstraint = ""
  disconstraint = ""
  bdconstraint = ""
  multidistrict = TRUE
  multibdtype = TRUE
  inference = TRUE
  
  if (district != "all"){ # there is a district constraint
    disconstraint = sprintf("district = '%s'", district)
    multidistrict = FALSE
  }
  if (length(bdtypes) < 3){ # there is a buildingtype constraint
    if (length(bdtypes) == 1){
      multibdtype = FALSE
      bdconstraint = sprintf("buildingtype = '%s'", bdtypes)
    } else if(length(bdtypes) == 2){
      bdconstraint = sprintf("buildingtype IN ('%s', '%s')", bdtypes[1], bdtypes[2])
    }
  }
  
  if (disconstraint !="" & bdconstraint !=""){
    finalconstraint = sprintf("WHERE %s AND %s", disconstraint, bdconstraint)
  } else if(disconstraint != ""){
    finalconstraint = sprintf("WHERE %s", disconstraint)
  } else if(bdconstraint != ""){
    finalconstraint = sprintf("WHERE %s", bdconstraint)
  } else{
    finalconstraint = ""
  }
  
  querystring = sprintf(querystring, finalconstraint)
  result = dbGetQuery(con, querystring)
  
  if(dim(result)[1] < 70){ inference = FALSE }
  
  return(list("data"=result, "multidistrict" = multidistrict, "multibdtype" = multibdtype, "inference"=inference))
}
# example
# result = gen_data("DaXing", c("Plate", "Tower"))

# close connection to database
# dbDisconnect(con)
