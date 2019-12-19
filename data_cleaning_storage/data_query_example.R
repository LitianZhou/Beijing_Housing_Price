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

# next is some query examples

## query single variable
singlevar = dbGetQuery(con, "SELECT constructiontime FROM housing;")

## query multiple variables
multivar= dbGetQuery(con, 'SELECT lng,lat,totalprice FROM housing;')

## query based on conditions (numeric values)
numericcondition = dbGetQuery(con, "SELECT totalprice,lng,lat,buildingstructure FROM housing
                       WHERE totalprice > 300 AND totalprice < 1000;")

## query based on conditions (string values)
stringcondition = dbGetQuery(con, "SELECT totalprice,lng,lat,buildingstructure,district FROM housing
                       WHERE district IN ('HaiDian', 'ChaoYang') ORDER BY totalprice DESC;")


## query based on conditions (dates)
datecondition = dbGetQuery(con, "SELECT totalprice,lng,lat,buildingstructure,tradetime FROM housing
                       WHERE tradetime>='2015-01-01' and tradetime <'2016-12-31' ORDER BY tradetime DESC, totalprice DESC;")

gen_data = function(district, bdtypes){
  querystring = "SELECT price, totalprice, square, bathroom, buildingtype, constructiontime, renovationcondition, \
            elevator, subway, district, season FROM housing %s;"
  
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
    } else{
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
    
  if(dim(result)[1] < 70){ invertible = FALSE }
  
  return(list("data"=result, "multidistrict" = multidistrict, "multibdtype" = multibdtype, "inference"=inference))
}

dbDisconnect(con)
