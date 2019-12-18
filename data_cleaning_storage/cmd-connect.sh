#!/bin/bash

psql \
   --host=beijing-housing.copmdh9kwiqr.us-east-2.rds.amazonaws.com \
   --port=5432 \
   --username=biostat625 \
   --password \
   --dbname=housing 
