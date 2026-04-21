#!/bin/bash
set -e

module load R/4.3.2-gfbf-2023a
R --version
Rscript PS10_Winkle.R

echo "DONE"

