#!/bin/bash

# Check if an argument is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <markdown-file>"
    exit 1
fi

# Assign the first argument to a variable
markdown_file=$1

# Run the Rscript command with the specified markdown file
/usr/bin/Rscript --default-packages=methods,datasets,utils,grDevices,graphics,stats,knitr,rmarkdown -e 'rmarkdown::render(commandArgs(trailingOnly=TRUE)[1])' "$markdown_file"

