# Nowcasting Reported COVID-19 Hospitalizations Using De-Identified, Aggregated Medical Insurance Claims Data

This directory contains code and data for "Nowcasting Reported COVID-19 Hospitalizations Using De-Identified, Aggregated Medical Insurance Claims Data". The preprint is available at https://www.medrxiv.org/content/10.1101/2023.12.22.23300471v1

The entire nowcasting process and its analysis could be reproduced in the `code` folder. Producing predictions for scenario 1 would take around 12 hours, for scenario 2 would take around 2 days. For this reason we have made both predictions and features available at https://hhs-nowcasting.s3.amazonaws.com. Running `bucket_download.sh` downloads both the versioned feature and prediction files. This should allow replication with minimal additional effort. 
