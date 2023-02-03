

dir_data <- file.path("../comix/data")
dir_cnts <- file.path("../comix/data/contacts.qs")
dir_part <- file.path("../comix/data/part_min.qs")


## Filepaths for origin of spss files
## CIJ
if(Sys.info()["nodename"] == "DESKTOP-OKJFGKO"){
  #dir_data_spss <- "data/spss"
  parent_path <- '~/../Filr/Net Folders/EPH Shared/Comix_survey'
  dir_data_spss <- file.path(parent_path, 'data/spss')
  dir_data_clean <- file.path(parent_path, 'data/clean')
  dir_data_archive <- file.path(parent_path, 'data/clean/archive')
  dir_data_validate <- file.path(parent_path, 'data/validated')
  dir_data_valid_archive <- file.path(parent_path, 'data/validated/archive')
  ## Filepaths for temp processing files
  dir_data_process <- "data/processing"
  dir_data_local <- file.path('../comix/data/')
}