library(dplyr)

solvents <- readr::read_csv("../data/Dec2016/cleaned/samples/retail_solvents.csv")
solvents$value <- as.numeric(solvents$value)

solvents$solvent_tested <- !is.na(solvents$value)

table(solvents$solvent_tested)



# first get list/df of inhalantnames
inhalantnames <- as.data.frame(unique(solvents$productname[solvents$inv_type_name=="Marijuana Extract for Inhalation"]))
# rename column so we can call it
colnames(inhalantnames) <- "retail_prodname"

# get a df that is for each unique inhalant name, the boolean values for categorizing
inhalantnames <- inhalantnames %>%
  dplyr::rowwise() %>%
  # boolean for cartridge
  dplyr::mutate(cartridge = grepl("cart|vap|vc|pen|refill|juju|joint", retail_prodname, ignore.case = T),
                # extraction method
                extraction = ifelse(grepl("BHO|butane", retail_prodname, ignore.case = T), "BHO",
                                    ifelse(grepl("CO2", retail_prodname, ignore.case = T), "CO2",
                                           "unknown"
                                    )),
                # type
                type = ifelse(grepl("cart|vap|vc|pen|refill|juju|joint", retail_prodname, ignore.case=T), "cartridge",
                              ifelse(grepl("oil|RSO|ESO", retail_prodname, ignore.case=T), "oil",
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
                consumption = ifelse(grepl("cart|vap|vc|pen|refill|juju|joint", retail_prodname, ignore.case = T), "vaping",
                                     ifelse(grepl("wax|shatter|dab|resin", retail_prodname, ignore.case = T), "dabbing",
                                            ifelse(grepl("kief|keif|hash", retail_prodname, ignore.case = T), "smoking",
                                                   ifelse(grepl("oil|ESO|RSO", retail_prodname, ignore.case = T), "oil",
                                                          "unknown"))))
  )

write.table(inhalantnames, file="../data/Dec2016/cleaned/testing/inhalant_prodnames_dec2016.csv", row.names=F, sep=",")

# join classified inhalantnames back to dispening df
solvents$productname <- as.factor(solvents$productname)
solvents <- left_join(solvents, inhalantnames, by=c("productname" = "retail_prodname"))

table(solvents$solvent_tested, solvents$extraction)

table(solvents$solvent_tested, solvents$type)

concentrates <- filter(solvents, type!="kief", type!="hash", type!="bubble")

table(concentrates$solvent_tested, concentrates$extraction)

# trying to group on product name
# to get idea if that particular product is eligible / required for solvent test
prod_names <- solvents %>%
  dplyr::group_by(productname) %>%
  dplyr::summarise(ever_solvent_test = sum(solvent_tested) > 0,
                   prods_sold = n(),
                   cartridge = cartridge[1],
                   extraction = extraction[1],
                   type = type[1],
                   consumption = consumption[1]
                   ) %>%
                   #perc_inhalants = prods_sold / nrow(solvents)) %>%
  dplyr::mutate(inhalant_marketrank = rank(prods_sold)/ length(prods_sold)) %>%
  # dplyr::mutate(cartridge = grepl("cart|vap|vc|pen|refill", productname, ignore.case = T),
  #               # extraction method
  #               extraction = ifelse(grepl("BHO|butane", productname, ignore.case = T), "BHO",
  #                                   ifelse(grepl("CO2", productname, ignore.case = T), "CO2",
  #                                          "unknown"
  #                                   )),
  #               # type
  #               type = ifelse(grepl("cart|vap|vc|pen|refill", productname, ignore.case=T), "cartridge",
  #                             ifelse(grepl("oil", productname, ignore.case=T), "oil",
  #                                    ifelse(grepl("wax", productname, ignore.case=T), "wax",
  #                                           ifelse(grepl("hash", productname, ignore.case = T), "hash",
  #                                                  ifelse(grepl("kief|keif", productname, ignore.case = T), "kief",
  #                                                         ifelse(grepl("shatter", productname, ignore.case = T), "shatter",
  #                                                                ifelse(grepl("dab", productname, ignore.case = T), "dab",
  #                                                                       ifelse(grepl("resin", productname, ignore.case = T), "resin",
  #                                                                              ifelse(grepl("bubble", productname, ignore.case = T), "bubble",
  #                                                                                     "unknown")))))))
  #                             )),
  #               # consumption method
  #               consumption = ifelse(grepl("cart|vap|vc|pen|refill", productname, ignore.case = T), "vaping",
  #                                    ifelse(grepl("wax|shatter|dab|resin", productname, ignore.case = T), "dabbing",
  #                                           ifelse(grepl("kief|keif|hash", productname, ignore.case = T), "smoking",
  #                                                  ifelse(grepl("oil", productname, ignore.case = T), "oil",
  #                                                         "unknown"))))
  # ) %>%
  arrange(extraction, ever_solvent_test, desc(inhalant_marketrank))
  
table(prod_names$ever_solvent_test, prod_names$consumption)
  
write.table(prod_names, file="../data/Dec2016/cleaned/testing/inhalant_names_solvents.csv", sep=",", row.names = F)
