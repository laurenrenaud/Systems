library(dplyr)
library(scales)
dispensing$location <- as.factor(dispensing$location)

pullman_store_sales <- dispensing %>%
  dplyr::filter(location %in% c(1751, 1761, 457, 1874, 910)) %>%
  dplyr::group_by(location, saletime) %>%
  dplyr::summarise(daily_revenue = sum(price, na.rm=T))
write.table(pullman_store_sales, file="../data/Dec2016/cleaned/samples/pullman_store_sales.csv",
            row.names=F, sep=",")


pullman_store_sales %>%
  group_by(saletime) %>%
  mutate(daily_sales_total = sum(daily_revenue, na.rm=T),
         location = factor(location, levels=c(1761, 457, 910, 1751, 1874))) %>%
  group_by(location, saletime) %>%
  summarise(sales_proportion = daily_revenue / daily_sales_total) %>%
  ggplot(aes(x=saletime, y=sales_proportion, fill=location)) +
  geom_area() +
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#d69b1d", "#d69b1d", "#0f9e78", "#a1afac", "#32bee5")) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(y="Proportion of Sales",
       x="Date")
