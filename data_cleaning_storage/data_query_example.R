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
singlevar = dbGetQuery(con, "SELECT buildingstructure FROM housing;")

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

dbDisconnect(con)
