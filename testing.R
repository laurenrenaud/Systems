testing <- retail %>%
  filter(saledate >= "2016-03-01" & saledate <= "2016-03-31") %>%
  select(retail_transactionid, dispensingid, retail_prodname, retail_usableweight,
         retail_weight, saledate) %>%
  group_by(retail_transactionid, retail_prodname) %>%
  mutate(num_dupes = n()) %>%
  filter(num_dupes > 1) %>%
  arrange(retail_transactionid, retail_prodname)
