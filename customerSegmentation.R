library(dplyr)
library(lubridate)
library(ggplot2)
library(factoextra)
library(dplyr)
library(purrr)
library(scales)

df <- read.csv("online_retail_II.csv", stringsAsFactors = FALSE)
dim(df)

colnames(df) <- make.names(colnames(df))

#cleaning
#date
df$InvoiceDate <- as.POSIXct(df$InvoiceDate, format="%Y-%m-%d %H:%M:%S")
df <- df %>% filter(!is.na(Customer.ID))

#remove cancellations (Invoice starting with C)
df <- df %>% filter(!grepl("^C", Invoice))

#Remove invalid values
df <- df %>% filter(Quantity > 0, Price > 0)

#creating total price
df <- df %>% mutate(TotalPrice = Quantity * Price)

#snapshot date , 'today'
snapshot_date <- max(df$InvoiceDate) + 1

#RFM calculation
rfm <- df %>%
  group_by(Customer.ID) %>%
  summarise(
    Recency = as.numeric(snapshot_date - max(InvoiceDate)),
    Frequency = n_distinct(Invoice),
    Monetary = sum(TotalPrice)
  )

#scaling
rfm_scaled <- scale(rfm[, -1])

fviz_nbclust(rfm_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method")

fviz_nbclust(rfm_scaled, kmeans, method = "silhouette") +
  ggtitle("Silhouette Score")

set.seed(123)
kmeans_model <- kmeans(rfm_scaled, centers = 4, nstart = 25)

rfm$Cluster <- as.factor(kmeans_model$cluster)
# Cluster summary
rfm %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Count = n()
  )
#View(df)
#dim(df)

#plotting rfm
ggplot(rfm, aes(x = Recency, y = Monetary, color = Cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal()





aggregate(cbind(Recency, Frequency, Monetary) ~ Cluster, data = rfm, mean)
df <- merge(df, rfm[, c("Customer.ID", "Cluster")], by = "Customer.ID")

segment_revenue <- df %>%
  group_by(Cluster) %>%
  summarise(Revenue = sum(TotalPrice))


#estimating price elasticity (base)
df_model <- df %>%
  filter(Quantity > 0, Price > 0) %>%
  mutate(
    logQ = log(Quantity),
    logP = log(Price)
  )

model <- lm(logQ ~ logP, data = df_model)
summary(model)

#estimating price elasticity by cluster
elasticity_results <- df_model %>%
  group_by(Cluster) %>%
  summarise(
    model = list(lm(logQ ~ logP, data = pick(everything())))
  ) %>%
  mutate(
    beta = map_dbl(model, ~ coef(.x)[2])
  )

elasticity_results

#cannot run segmentation by product. hits memory limit

#setting changes in prices of cluster 1 and 2, where P1 is 
#price for Cluster 1, P2 is price for Cluster 2

#since |E| < 1, we can increase prices,
#for exmaple, let's take a 2% increase for P1 and 5% for P2 
delP1 <- 0.02
delP2 <- 0.05
df <- df %>%
  mutate(
    PriceChange = case_when(
      Cluster == 1 ~ delP1, 
      Cluster == 2 ~ delP2,
      TRUE ~ 0
    ),
    NewPrice = Price * (1 + PriceChange),
    NewRevenue = Quantity * NewPrice
  )

original_revenue <- sum(df$TotalPrice)
new_revenue <- sum(df$NewRevenue)

change <- (new_revenue - original_revenue) / original_revenue
change

#implementing price elasticity
ped1 <- -0.55 
ped2 <- -0.515
df <- df %>%
  mutate(
    AdjustedQuantity = case_when(
      Cluster == 2 ~ Quantity * (1 + ped2*delP2),  
      Cluster == 1 ~ Quantity * (1 + ped1*delP1), 
      TRUE ~ Quantity
    ),
    NewRevenue = AdjustedQuantity * NewPrice
  )
original_revenue <- sum(df$TotalPrice)
new_revenue <- sum(df$NewRevenue)

change <- (new_revenue - original_revenue) / original_revenue
change

ped1 <- -0.55 
ped2 <- -0.515
price_changes <- seq(0, 0.30, by = 0.01)  # 0% to 10%

results <- data.frame()


for (dp in price_changes) {
  #taking effective ped because elasticity will likely change with higher price)
  effective_ped1 <- ped1 - 2*dp
  effective_ped2 <- ped2 - 2*dp

  temp_df <- df %>%
    mutate(
      PriceChange = case_when(
        Cluster == 1 ~ dp,
        Cluster == 2 ~ dp,
        TRUE ~ 0
      ),
      NewPrice = Price * (1 + PriceChange),
      
      AdjustedQuantity = case_when(
        Cluster == 1 ~ Quantity * (1 + effective_ped1 * dp),
        Cluster == 2 ~ Quantity * (1 + effective_ped2 * dp),
        TRUE ~ Quantity
      ),
      
      NewRevenue = AdjustedQuantity * NewPrice
    )
  
  new_rev <- sum(temp_df$NewRevenue)
  
  results <- rbind(results, data.frame(
    PriceChange = dp,
    Revenue = new_rev
  ))
}


ggplot(results, aes(x = PriceChange, y = Revenue)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Revenue vs Price Increase",
    x = "Price Change",
    y = "Total Revenue"
  )
optimal <- results[which.max(results$Revenue), ]
optimal
