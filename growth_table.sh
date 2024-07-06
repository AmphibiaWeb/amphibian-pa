# bash command language

# In this script, 
# huge refers to PAs with GIS_T_AREA between 10,000–100,000 km2     
# large:                                     1,000–10,000 km2 
# mid:                                       100–1,000 km2 
# small:                                     10–100 km2
# micro:                                     0–10 km2

####################################################################################################
# STAGE 1:  create growth curves for amphibians added to PA networks composed of different sized PAs

# Required input files:
# *_WDPA_ID.csv refers to WDPA_IDs for each size category, extracted from the world database of 
#           protected areas database, July 2021 version: WDPA_WDOECM_Jul2021_Public_all_csv.csv

# establish index of size prefixes
prefixes=("large" "mid" "small" "micro")

# Resample protected areas of each size category in 1000 discrete runs
# NOTE: depending on your system, runs may need to be run in batches
for prefix in "${prefixes[@]}"; do
    Rscript PA_network_build.R ${prefix}_WDPA_ID.csv ${prefix}_growth.csv
done    

####################################################################################################
# STAGE 2: subsample groth tables for plotting with AccumulationCurve.R: 
#     subsample large, mid, small, and micro_growth.csv so they match the length of huge_growth.csv

# establish the downsampling command
downsample='{
    printf $1;  # Always include the first column (0)
    for (i = 2; i < NF; i++) {
        if ((i - 1) % '$interval' == 0) {
            printf "," $i;
        }
    }
    printf "," $NF "\n";  # Always include the last column (where area adds to the WDPA terrestrial area)
}'

# Establish the target number of columns for subsampling larger csvs 
length=$(head -n 1 huge_growth.csv | awk -F, '{print NF}')

for prefix in "${prefixes[@]}"; do
    # calculate number of columns in *_growth.csv
    eval "${prefix}_length=$(head -n 1 ${prefix}_growth.csv | awk -F, '{print NF}')"
    # Calculate the interval for downsampling
    eval "${prefix}_interval=$(( (${prefix}_length - 2) / (length - 2) ))" # -2 to adjust for retaining the first and last columns
    # Apply the downsampling command
    eval "awk -F, \"\$downsample_command\" ${prefix}_growth.csv > sub_${prefix}_growth.csv"
done
