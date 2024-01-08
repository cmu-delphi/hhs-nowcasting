#!/bin/bash

feat_url="hhs-nowcasting.s3.us-east-1.amazonaws.com/versioned_feature.zip" 
pred_url="hhs-nowcasting.s3.us-east-1.amazonaws.com/predictions.zip"

feat_name="versioend_feature.zip"
pred_name="predictions.zip"

wget -O "$feat_name" "$feat_url"
wget -O "$pred_name" "$pred_url"

unzip "$feat_name"
unzip "$pred_name"

rm "$feat_name"
rm "$pred_name"