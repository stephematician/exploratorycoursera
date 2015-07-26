# A helper script for downloading the Course Project data

pm25_data_url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
pm25_local_archive <- './data/NEI_data.zip'

if (!file.exists('data')) {
    dir.create('data')
}

if (!file.exists(pm25_local_archive)) {
    download.file(pm25_data_url,
                  pm25_local_archive,
                  method='curl')
    sink('data/download_date.txt',
         append=FALSE,
         split=FALSE)
    print(date())
    sink()
}

unzip(pm25_local_archive,
      overwrite = TRUE,
      junkpaths = FALSE,
      exdir = "./data/")