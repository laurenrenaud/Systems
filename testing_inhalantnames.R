retail_oct <- readr::read_csv("../data/Dec2016/cleaned/testing/oct_inhalant_prodnamesonly.csv")

retail_oct <- select(retail_oct, retail_prodname = productname)

# get a df that is for each unique inhalant name, the boolean values for categorizing
retail_oct <- retail_oct %>%
  dplyr::rowwise() %>%
  # boolean for cartridge
  dplyr::mutate(cartridge = grepl("cart|vap|vc|pen|refill", retail_prodname, ignore.case = T),
                # extraction method
                extraction = ifelse(grepl("BHO|butane", retail_prodname, ignore.case = T), "BHO",
                                    ifelse(grepl("CO2", retail_prodname, ignore.case = T), "CO2",
                                           "unknown"
                                    )),
                # type
                type = ifelse(grepl("cart|vap|vc|pen|refill", retail_prodname, ignore.case=T), "cartridge",
                              ifelse(grepl("oil", retail_prodname, ignore.case=T), "oil",
                                     ifelse(grepl("wax", retail_prodname, ignore.case=T), "wax",
                                            ifelse(grepl("hash", retail_prodname, ignore.case = T), "hash",
                                                   ifelse(grepl("kief|keif", retail_prodname, ignore.case = T), "kief",
                                                          ifelse(grepl("shatter", retail_prodname, ignore.case = T), "shatter",
                                                                 ifelse(grepl("dab", retail_prodname, ignore.case = T), "dab",
                                                                        ifelse(grepl("resin", retail_prodname, ignore.case = T), "resin",
                                                                               ifelse(grepl("bubble", retail_prodname, ignore.case = T), "bubble",
                                                                                      "unknown")))))))
                              )),
                # consumption method
                consumption = ifelse(grepl("cart|vap|vc|pen|refill", retail_prodname, ignore.case = T), "vaping",
                                     ifelse(grepl("wax|shatter|dab|resin", retail_prodname, ignore.case = T), "dabbing",
                                            ifelse(grepl("kief|keif|hash", retail_prodname, ignore.case = T), "smoking",
                                                   ifelse(grepl("oil", retail_prodname, ignore.case = T), "oil",
                                                          "unknown"))))
  )
