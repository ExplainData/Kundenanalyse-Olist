##################################
# Laden und Bereinigen der Daten #
##################################
# Installieren und Laden der benötigten Pakete
install.packages(c("readr", "dplyr", "ggplot2", "ggrepel", "lubridate", "arules", "arulesViz", "data.table", "Matrix", "tidyr", "rgl", "pheatmap", "factoextra", "NbClust", "GGally", "forecast", "zoo", "plotly"))
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(data.table)
library(arules)
library(arulesViz)
library(Matrix)
library(pheatmap)
library(tidyverse)
library(factoextra)
library(NbClust)
library(GGally)
library(lubridate)
library(forecast)
library(zoo)
library(plotly)

# Daten einlesen
customers <- read_csv("olist_customers_dataset.csv")
orders <- read_csv("olist_orders_dataset.csv")
order_items <- read_csv("olist_order_items_dataset.csv")
order_payments <- read_csv("olist_order_payments_dataset.csv")
order_reviews <- read_csv("olist_order_reviews_dataset.csv")
products <- read_csv("olist_products_dataset.csv")

# Datenzusammenführung
orders_customers <- left_join(orders, customers, by = "customer_id")
orders_customers_items <- left_join(orders_customers, order_items, by = "order_id")
orders_customers_items_payments <- left_join(orders_customers_items, order_payments, by = "order_id")
orders_customers_items_payments_reviews <- left_join(orders_customers_items_payments, order_reviews, by = "order_id")
complete_data <- left_join(orders_customers_items_payments_reviews, products, by = "product_id")

# Duplikate entfernen
complete_data <- distinct(complete_data)

# Fehlende Werte behandeln
missing_values <- sapply(complete_data, function(x) sum(is.na(x)))
print(missing_values)

orders_customers_items_payments_reviews <- orders_customers_items_payments_reviews %>%
  mutate(order_delivered_carrier_date = case_when(
    is.na(order_delivered_carrier_date) ~ order_purchase_timestamp,
    TRUE ~ order_delivered_carrier_date))

orders_customers_items_payments_reviews <- orders_customers_items_payments_reviews %>%
  mutate(order_delivered_customer_date = case_when(
    is.na(order_delivered_customer_date) ~ order_estimated_delivery_date,
    TRUE ~ order_delivered_customer_date))

orders_customers_items_payments_reviews <- orders_customers_items_payments_reviews %>% 
  filter(!is.na(order_item_id) & !is.na(review_id))

products <- products %>%
  filter(!is.na(product_category_name))

orders_customers_items_payments_reviews$review_comment_title[is.na(orders_customers_items_payments_reviews$review_comment_title)] <- "Kein Kommentar"
orders_customers_items_payments_reviews$review_comment_message[is.na(orders_customers_items_payments_reviews$review_comment_message)] <- "Kein Kommentar"

# Datenzusammenführung
complete_data <- left_join(orders_customers_items_payments_reviews, products, by = "product_id")
complete_data <- complete_data %>%
  filter(!is.na(product_category_name))


#######################
# Feature-Engineering #
#######################

# Differenz zwischen Bestelldatum und Lieferdatum
complete_data <- complete_data %>%
  mutate(delivery_time = difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days"))

# Daten auf Kundenebene zu aggregieren, um die Anzahl der Bestellungen und den durchschnittlichen Bestellwert pro Kunde zu ermitteln
customer_orders_agg <- orders_customers_items_payments_reviews %>%
  group_by(customer_unique_id) %>%
  summarise(num_orders = n_distinct(order_id),
            total_spent = sum(payment_value),
            avg_order_value = total_spent / num_orders) %>%
  arrange(desc(num_orders), desc(total_spent))

complete_data <- complete_data %>%
  left_join(customer_orders_agg, by = "customer_unique_id")

# Datei speichern
write_csv(complete_data, "complete_data.csv")

########################
# geografische Analyse #
########################

# Datenaggregation auf Stadtebene
city_level_data <- complete_data %>%
  group_by(customer_city) %>%
  summarise(num_customers = n_distinct(customer_id),
            num_orders = n(),
            total_revenue = sum(price + freight_value),
            avg_order_value = mean(price + freight_value)) %>%
  arrange(desc(total_revenue))

# Datenaggregation auf Bundestaatsebene
state_level_data <- complete_data %>%
  group_by(customer_state) %>%
  summarise(num_customers = n_distinct(customer_id),
            num_orders = n(),
            total_revenue = sum(price + freight_value),
            avg_order_value = mean(price + freight_value)) %>%
  arrange(desc(total_revenue))

# Regionale Unterschiede
top_states <- state_level_data %>%
  arrange(desc(num_customers)) %>%
  head(10)

top_cities <- city_level_data %>%
  arrange(desc(num_customers)) %>%
  head(10)

# Bevölkerungsdaten
population_data <- data.frame(
  region = c("SP", "RJ", "MG", "DF", "PR", "RS", "BA", "ES", "SC", "GO", "sao paulo", "rio de janeiro", "brasilia", "belo horizonte", "salvador", "curitiba", "campinas", "porto alegre", "guarulhos", "sao bernardo do campo"),
  region_type = c(rep("Bundesstaat", 10), rep("Stadt", 10)),
  population = c(45000000, 17200000, 21000000, 3050000, 11400000, 11400000, 15000000, 4000000, 7000000, 7000000, 12252023, 6718903, 3055149, 2521564, 2872347, 1948626, 1221979, 1488252, 1392131, 844483))

# Daten zusammenfügen
state_population_data <- top_states %>%
  left_join(population_data, by = c("customer_state" = "region"))

city_population_data <- top_cities %>%
  left_join(population_data, by = c("customer_city" = "region"))

# XY-Diagramme mit Kreisen, deren Größe proportional zur Bevölkerung ist, erstellen
state_plot_population <- ggplot(state_population_data, aes(x = num_orders, y = total_revenue, label = customer_state)) +
  geom_point(aes(size = population), alpha = 0.5) +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(range = c(3, 15)) + # Anpassung der Größe der Kreise
  labs(x = "Anzahl der Bestellungen (logarithmische Skala)", y = "Gesamtumsatz (logarithmische Skala)", title = "Bundesstaaten: Bestellungen vs. Umsatz (Kreisgröße = Bevölkerung)") +
  theme_minimal()

city_plot_population <- ggplot(city_population_data, aes(x = num_orders, y = total_revenue, label = customer_city)) +
  geom_point(aes(size = population), alpha = 0.5) +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(range = c(3, 15)) + 
  labs(x = "Anzahl der Bestellungen (logarithmische Skala)", y = "Gesamtumsatz (logarithmische Skala)", title = "Städte: Bestellungen vs. Umsatz (Kreisgröße = Bevölkerung)") +
  theme_minimal()

# ggplot-Objekt als Jpeg im Arbeitsverzeichnis speichern
ggsave("state_plot_population.jpg", plot = state_plot_population, dpi = 300)
ggsave("city_plot_population.jpg", plot = city_plot_population, dpi = 300)


#########################
# Kaufverhaltensanalyse #
#########################

# Häufigkeit der Käufe pro Kunde
purchase_frequency <- complete_data %>%
  group_by(customer_id) %>%
  summarise(num_orders = n()) %>%
  ungroup()

# Häufigkeitstabelle 
table(purchase_frequency$num_orders) 

# Da die meisten Kunden nur einmal gekauft haben,
# werden wir uns auf andere Aspekte des Kaufverhaltens konzentrieren,
# um Trends und Muster zu identifizieren

# Durchschnittlicher Bestellwert
average_order_value <- complete_data %>%
  summarise(avg_order_value = mean(price + freight_value))


# Verteilung von Bestellungen nach Wochentagen
  orders_by_weekday <- complete_data %>%
  mutate(weekday = weekdays(order_purchase_timestamp, abbreviate = TRUE)) %>%
  group_by(weekday) %>%
  summarise(num_orders = n()) %>%
  ungroup() %>%
  arrange(desc(num_orders))

# Verteilung von Bestellungen nach Tageszeiten
orders_by_hour <- complete_data %>%
  mutate(hour = hour(order_purchase_timestamp)) %>%
  group_by(hour) %>%
  summarise(num_orders = n()) %>%
  ungroup() %>%
  arrange(desc(num_orders))

# Visualisierung der Bestellverteilung nach Wochentagen
plot_orders_by_weekday <- ggplot(orders_by_weekday, aes(x = reorder(weekday, -num_orders), y = num_orders)) +
                          geom_col(fill = "steelblue") +
                          labs(title = "Verteilung von Bestellungen nach Wochentagen",
                           x = "Wochentag",
                           y = "Anzahl der Bestellungen") +
                          theme_minimal()

# Visualisierung der Bestellverteilung nach Tageszeiten
plot_orders_by_hour <- ggplot(orders_by_hour, aes(x = hour, y = num_orders)) +
                       geom_col(fill = "steelblue") +
                       labs(title = "Verteilung von Bestellungen nach Tageszeiten",
                        x = "Stunde des Tages",
                        y = "Anzahl der Bestellungen") +
                      theme_minimal()


# gplot-Objekt als Jpeg im Arbeitsverzeichnis speichern
ggsave("plot_orders_by_weekday.jpg", plot = plot_orders_by_weekday, dpi = 300)
ggsave("plot_orders_by_hour.jpg", plot = plot_orders_by_hour, dpi = 300)


# Analyse der meistverkauften Produktkategorien
top_product_categories <- complete_data %>%
  group_by(product_category_name) %>%
  summarise(num_orders = n()) %>%
  ungroup() %>%
  arrange(desc(num_orders))%>%
  head(20)

# Übersetzung der Produktkategorien
top_product_categories_translated <- top_product_categories %>%
  left_join(product_category_name_translation, by = "product_category_name") %>%
  select(product_category_name_english, num_orders)

#  Balkendiagramms der meistverkauften Produktkategorien (übersetzte Namen) erstellen
top_product_categories_plot <- ggplot(top_product_categories_translated, aes(x = reorder(product_category_name_english, -num_orders), y = num_orders)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8)) +
  labs(x = "Produktkategorie", y = "Anzahl der Bestellungen", title = "Meistverkaufte Produktkategorien")

ggsave("top_product_categories_plot.jpg", plot = top_product_categories_plot, dpi = 300)

# Analyse von wiederkehrenden Kunden
repeat_customers <- complete_data %>%
  group_by(customer_id) %>%
  summarise(num_orders = n()) %>%
  filter(num_orders > 1) %>%
  ungroup()%>%
  arrange(desc(num_orders))

# Analyse von Kunden mit hohen Bestellwerten
high_value_customers <- complete_data %>%
  group_by(customer_id) %>%
  summarise(total_order_value = sum(price + freight_value)) %>%
  filter(total_order_value > quantile(total_order_value, 0.9)) %>%
  ungroup()

#####################
# Warenkorb-Analyse #
#####################
# die Top-20-Produktkategorien basierend auf der Anzahl der Transaktionen auswählen
top_categories <- complete_data %>%
  count(product_category_name, sort = TRUE) %>%
  head(20) %>%
  .$product_category_name

# Transaktionen, um nur die Top-20-Produktkategorien zu behalten, filtern
filtered_transactions_data <- complete_data %>%
  dplyr::select(order_id, product_category_name) %>%
  data.table::dcast(., order_id ~ product_category_name, fun.aggregate = length, value.var = "product_category_name") %>%
  dplyr::select(-order_id) %>%
  dplyr::mutate_all(~ifelse(. == 0, FALSE, TRUE))

filtered_transactions <- as(filtered_transactions_data, "transactions")

# Warenkorbanalyse mit den gefilterten Transaktionen durchführen
rules <- apriori(filtered_transactions, parameter = list(supp = 0.0004, conf = 0.1, maxlen = 4))
rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_sorted, n = 10))

#################
# Segmentierung #
#################

# RFM-Metriken berechnen
rfm_data <- complete_data %>%
  mutate(order_purchase_timestamp = as.Date(order_purchase_timestamp)) %>%
  group_by(customer_unique_id) %>%
  summarise(Recency = as.numeric(min(Sys.Date()) - max(order_purchase_timestamp)),
                                 Frequency = n(),
                                 MonetaryValue = sum(payment_value)) %>%
                                ungroup()
            
# Kunden in Gruppen einteilen
  rfm_data <- rfm_data %>%
              drop_na() %>%
              mutate(R_Quartile = ntile(Recency, 4),
                     F_Quartile = ntile(Frequency, 4),
                     M_Quartile = ntile(MonetaryValue, 4)) %>%
              mutate(RFM_Score = paste(R_Quartile, F_Quartile, M_Quartile, sep = ''))
  
# Segmentgröße und Kennzahlen berechnen
segment_summary <- rfm_data %>%
    group_by(RFM_Score) %>%
    summarise(Count = n(),
              Avg_Recency = mean(Recency),
              Avg_Frequency = mean(Frequency),
              Avg_MonetaryValue = mean(MonetaryValue)) %>%
    ungroup()

# Häufigkeit zum RFM_Score hinzufügen
rfm_data_count <- rfm_data %>%
  group_by(RFM_Score) %>%
  mutate(count = n()) %>%
  ungroup()

# Jitter zu den Daten hinzufügen
summary_data_jitter <-  rfm_data_count %>%
  mutate(R_jitter = jitter(R_Quartile, factor = 0.3),
         F_jitter = jitter(F_Quartile, factor = 0.3),
         M_jitter = jitter(M_Quartile, factor = 0.3))

# Farbenskala erstellen
color_scale <- colorRampPalette(c("red", "yellow", "green"))


# 3D Streuungsdiagramm erstellen
scatter_3d <- plot_ly(summary_data_jitter, x = ~R_jitter, y = ~F_jitter, z = ~M_jitter,
                      type = "scatter3d", mode = "markers",
                      marker = list(size = 5, color = ~count, colorscale = "Jet", opacity = 0.8,
                                    colorbar = list(title = "Frequency"))) %>%
  layout(scene = list(xaxis = list(title = "Recency Quartile"),
                      yaxis = list(title = "Frequency Quartile"),
                      zaxis = list(title = "Monetary Value Quartile")))


######################
# Clustering-Analyse #
######################
# Daten normalisieren
normalized_rfm_data <- rfm_data %>%
  select(Recency, Frequency, MonetaryValue) %>%
  scale()

# eine Stichprobe von 10000 Kunden extrahieren
sample_size <- 10000
sample_rfm_data <- data.frame(normalized_rfm_data) %>%
  sample_n(size = sample_size)

# Elbow-Methode
fviz_nbclust <- fviz_nbclust(sample_rfm_data, kmeans, method = "wss")

# Silhouette-Koeffizient
nb_clusters <- NbClust(sample_rfm_data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")

# Schwellenwerte für jede Spalte bestimmen
lower_threshold_recency <- quantile(sample_rfm_data$Recency, 0.001)
upper_threshold_recency <- quantile(sample_rfm_data$Recency, 0.999)

lower_threshold_frequency <- quantile(sample_rfm_data$Frequency, 0.001)
upper_threshold_frequency <- quantile(sample_rfm_data$Frequency, 0.999)

lower_threshold_monetary <- quantile(sample_rfm_data$MonetaryValue, 0.001)
upper_threshold_monetary <- quantile(sample_rfm_data$MonetaryValue, 0.999)

# Ausreißer für alle Spalten entfernen
trimmed_rfm_data <- sample_rfm_data %>%
  filter(Recency >= lower_threshold_recency & Recency <= upper_threshold_recency,
         Frequency >= lower_threshold_frequency & Frequency <= upper_threshold_frequency,
         MonetaryValue >= lower_threshold_monetary & MonetaryValue <= upper_threshold_monetary)

# K-Means mit der optimalen Anzahl von Clustern ausführen
optimal_clusters <- nb_clusters$Best.nc
kmeans_result <- kmeans(trimmed_rfm_data, centers = 3)

# Clusterzugehörigkeit zu den ursprünglichen RFM-Daten hinzufügen
rfm_data_with_clusters <- trimmed_rfm_data %>%
  mutate(cluster = as.numeric(kmeans_result$cluster))

# Durchschnittswerte für jedes Cluster berechnen
cluster_summary <- rfm_data_with_clusters %>%
  group_by(cluster) %>%
  summarise(Recency = mean(Recency),
            Frequency = mean(Frequency),
            MonetaryValue = mean(MonetaryValue))

# Clusterzusammenfassung anzeigen
cluster_summary


# Benutzerdefinierte Funktion, um Korrelationskoeffizienten und P-Werte zu berechnen
correlation_with_p <- function(x, y) {
  cor_test <- cor.test(x, y)
  return(list(cor = cor_test$estimate, p_value = cor_test$p.value))
}

# Korrelationskoeffizienten und P-Werte für jedes Cluster berechnen 
correlation_summary <- rfm_data_with_clusters %>%
  group_by(cluster) %>%
  summarise(
    cor_recency_frequency = list(correlation_with_p(Recency, Frequency)),
    cor_recency_monetary = list(correlation_with_p(Recency, MonetaryValue)),
    cor_frequency_monetary = list(correlation_with_p(Frequency, MonetaryValue))
  ) %>%
  unnest(cols = c(cor_recency_frequency, cor_recency_monetary, cor_frequency_monetary))


# einen Paarplot erstellen
pair_plot <- ggpairs(rfm_data_with_clusters, columns = 1:3, ggplot2::aes(color = factor(cluster))) +
  theme_bw() +
  ggplot2::scale_color_discrete(name = "Cluster")

# Paarplot anzeigen
pair_plot

# Paarplot speichern
ggsave("pair_plot.jpg", plot = pair_plot, dpi = 600)

#####################
# Zeitreihenanalyse #
#####################

# die monatlichen Verkaufszahlen berechnen
monthly_sales <- complete_data %>%
  mutate(month = floor_date(order_purchase_timestamp, "month")) %>%
  group_by(month) %>%
  summarise(sales = sum(price, na.rm = TRUE))

# ein Zeitreihen-Objekt erstellen
ts_sales <- ts(monthly_sales$sales, start = c(year(min(monthly_sales$month)), month(min(monthly_sales$month))), frequency = 12)

# die Zeitreihe in Trend-, Saison- und zyklische Komponenten zerlegen
decomposed_sales <- decompose(ts_sales)

# die letzte Beobachtung aus jeder Zeitreihe in der Liste entferneb
decomposed_sales_no_last <- lapply(decomposed_sales, function(x) {
  if (is.ts(x)) {
    return(ts(x[1:(length(x) - 1)], start = start(x), frequency = frequency(x)))
  } else {
    return(x)
  }
})

# Ergebnis zurück in eine Liste der Klasse 'decomposed.ts' konvertieren
decomposed_sales_no_last <- structure(decomposed_sales_no_last, class = "decomposed.ts")

# decomposed_sales_no_last-Liste in einen Dataframe konvertieren
df_decomposed <- data.frame(
  date = time(decomposed_sales_no_last$x),
  sales = as.vector(decomposed_sales_no_last$x),
  seasonal = as.vector(decomposed_sales_no_last$seasonal),
  trend = as.vector(decomposed_sales_no_last$trend),
  random = as.vector(decomposed_sales_no_last$random)
)

# ataframe für ggplot2 vorbereiten
df_decomposed_long <- df_decomposed %>%
  gather(key = "series", value = "value", -date)

df_decomposed <- df_decomposed%>%
  mutate(date = as.Date(as.yearmon(date), frac = 1))

# Datum in ein richtiges Datumsformat fornatieren
df_decomposed_long <- df_decomposed_long %>%
  mutate(date = as.Date(as.yearmon(date), frac = 1))

# ggplot2-Graphen erstellen
df_decomposed_plot <- ggplot(df_decomposed_long, aes(x = date, y = value, color = series)) +
                      geom_line(linewidth = 1) +
                      scale_x_date(name = "Zeit", labels = scales::date_format("%m/%Y"), breaks = scales::date_breaks("months")) +
                      ylab("Umsatz") +
                      ggtitle("Trend-, Saison- und zyklische Komponenten der Verkaufszahlen") +
                      theme_minimal() +
                      facet_wrap(~series, ncol = 1, scales = "free_y") +
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("df_decomposed_plot.jpg", plot = df_decomposed_plot, dpi = 300)

# saisonbereinigte Zeitreihe berechnen
seasonally_adjusted <- ts_sales[0:(length(ts_sales)-1)] - decomposed_sales_no_last$seasonal

# Daten in ein Dataframe konvertieren
df_seasonally_adjusted <- data.frame(
  date = time(seasonally_adjusted),
  value = as.vector(seasonally_adjusted))

# Datum formatieren
df_seasonally_adjusted$date <- as.Date(as.yearmon(df_seasonally_adjusted$date), frac = 1)

# ggplot2-Graphen erstellen
df_seasonally_adjusted_plot <- ggplot(df_seasonally_adjusted, aes(x = date, y = value)) +
                                geom_line(size = 1, color = "blue") +
                                scale_x_date(name = "Zeit", labels = scales::date_format("%m/%Y"), breaks = scales::date_breaks("months")) +
                                ylab("Umsatz") +
                                ggtitle("Saisonbereinigte Zeitreihe") +
                                theme_minimal() +
                                theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("df_seasonally_adjusted_plot.jpg", plot = df_seasonally_adjusted_plot, dpi = 300)


