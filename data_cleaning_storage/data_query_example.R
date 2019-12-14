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

## query all the data
alldata = dbGetQuery(con, "SELECT buildingstructure FROM housing;")

## query longitude and latitude
geographical= dbGetQuery(con, 'SELECT lng,lat FROM housing;')


structure = dbGetQuery(con, "SELECT buildingstructure FROM housing;")
