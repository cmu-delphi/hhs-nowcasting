#!/bin/bash
#SBATCH --output=/.out

feat_url="hhs-nowcasting.s3.us-east-1.amazonaws.com/versioned_feature.zip" 
pred_url="hhs-nowcasting.s3.us-east-1.amazonaws.com/predictions.zip"

feat_name="versioend_feature.zip"
pred_name="predictions.zip"

# Directory where to extract files
extract_dir="versioned_feature"
# Check if the directory exists, if not, create it
if [ ! -d "$extract_dir" ]; then
    mkdir "$extract_dir"
fi

# Download the feature file and prediction file
wget -O "$feat_name" "$feat_url"
wget -O "$pred_name" "$pred_url"

# Extract the files into the specified directory
unzip -o "$feat_name" -d "$extract_dir"
unzip -o "$pred_name" -d "$extract_dir"

# Remove the zip files
rm "$feat_name"
rm "$pred_name"