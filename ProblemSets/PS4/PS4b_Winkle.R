# PS4b_Winkle.R
# Load libraries
library(sparklyr) # error - unavailable in current oscer r version 
library(tidyverse)

# 6.4: Set up a connection to Spark
sc <- spark_connect(master = "local")

# 6.5: Create a tibble called df1 that loads the iris data
df1 <- as_tibble(iris)

# 6.6: Copy this tibble into Spark, calling it df
df <- copy_to(sc, df1)

# 6.7: Verify the classes of both objects
print("--- Class of df1 (Local Tibble) ---")
print(class(df1))

print("--- Class of df (Spark DataFrame) ---")
print(class(df))
