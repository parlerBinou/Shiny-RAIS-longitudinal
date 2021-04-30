download_data <- function(table_number, coord_names, coord_prefix="dim", file_name=NULL) {
  # download the data from StatCan public website and prepare it
  
  # by design, the first dimension of COORDINATE is GEO.
  # DON'T need to include the dimension. GEO will be augmented inside of
  # this function.
  coord_names_with_geo <- c("geo", coord_names)
  ind_coord <- paste0(coord_prefix, "_", coord_names_with_geo)
    
  temp <- tempfile()
  url <- paste0("https://www150.statcan.gc.ca/n1/tbl/csv/", table_number,
                "-eng.zip", sep="")
  download.file(url, temp, mode="wb")
  
  orig <- unz(temp, paste(table_number, ".csv", sep = "")) %>%
    read_csv(
      col_types = cols_only(
        REF_DATE = col_integer(),
        COORDINATE = col_character(),
        VALUE = col_number(),
        STATUS = col_character(),
        SYMBOL = col_character())
    ) %>%
    filter(COORDINATE != "")
  unlink(temp)
  
  # split the COORDINATE into separate variables
  df <- orig %>%
    separate(col=COORDINATE, into=ind_coord)
  df[, ind_coord] <- sapply(df[, ind_coord], as.integer)
  
  
  if (!is.null(file_name)) {
    write_csv(df, file=paste0("data/",file_name,".csv"))
  }
  
  return(df)
}
