retail_time <- read.csv("/Users/lauren/Documents/School/CMU/Box Sync/Classes/2017 Spring/Systems/data/Dec2016/cleaned/locations/retail_times2.csv",
                        header=T, sep=",")

hist(retail_time$total_sales[retail_time$total_sales > 50000])
hist(retail_time$total_sales[retail_time$total_sales < 50000])
hist(retail_time$total_sales[retail_time$total_sales < 10000])
hist(retail_time$total_sales[retail_time$total_sales < 5000])
hist(retail_time$total_sales[retail_time$total_sales < 1000])
hist(retail_time$total_sales[retail_time$total_sales < 500])

retail_time$price_cat <- ifelse(retail_time$total_sales > 250000, "> $250k",
                                ifelse(retail_time$total_sales > 150000, "$150-250k",
                                       ifelse(retail_time$total_sales > 50000, "$50-150k",
                                              ifelse(retail_time$total_sales > 10000, "10-50k",
                                                     "$0-$10k"))))

retail_time <- select(retail_time, location, Date, total_sales, city, status, loclatitude, loclongitude, locationtypeNames, price_cat)

write.table(retail_time, file="../data/Dec2016/cleaned/locations/retail_time2.csv", row.names = F,
            col.names = T, sep=",")
