library(fs)

create_dir_structure <- function(){
  
  dir_names <- c(
    "00_Data",
    "00_Scripts",
    "01_Recruiting",
    "02_Engagement",
    "03_Remuneration",
    "04_Performance",
    "05_Safety"
  )
  
  dir_create(dir_names)
  
  dir_ls()
}

create_dir_structure()
