# Extract raw data from SQL connection
library(tidyverse)
library(odbc)
library(DBI)

con <- dbConnect(odbc::odbc(), dsn="odbc_driver17", timeout = 10)


MPIFixedCause <- DBI::dbGetQuery(
  con,
  "
SELECT *
FROM [MESH_MORTALITY].[Mortality_1] 
WHERE REG_DATE_OF_DEATH>='2008-04-01' 
  and REG_DATE_OF_DEATH<='2025-08-27'
  and ((CCG_OF_RESIDENCE_CODE not like '7%' or CCG_OF_RESIDENCE_CODE is null) --england resi whether can find CCG or not
       or (CCG_OF_RESIDENCE_CODE like '7%'and (DER_NHAIS_CCG_OF_REGISTRATION is not null or CCG_OF_REGISTRATION_CODE is not null))) --wales resi but eng ccg resp
  and CANCELLED_FLAG<>'Y' --only 37 records
  and DER_PSEUDO_NHS_NUMBER is not null
  "
) |>
  tibble() |>
  janitor::clean_names()

write_csv(MPIFixedCause, "MPIFixedCause.csv")


