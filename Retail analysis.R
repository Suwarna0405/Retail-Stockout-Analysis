install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(reshape2)

# Read the dataset
df <- read.csv("E:/Project/datasets/sales_data.csv", stringsAsFactors = FALSE)

# Stockout: Inventory Level == 0
df$stockout <- ifelse(df$Inventory.Level == 0, 1, 0)

# Overall stockout rate
total_stockouts <- sum(df$stockout)
total_rows <- nrow(df)
overall_rate <- (total_stockouts / total_rows) * 100

cat("Total Stockouts:", total_stockouts, "\n")
cat("Total Rows:", total_rows, "\n")
cat("Overall Stockout Rate:", round(overall_rate, 4), "%\n")

# Group by Product ID and Store ID
product_store_rate <- aggregate(stockout ~ Product.ID + Store.ID, data = df, mean)
product_store_rate$stockout <- product_store_rate$stockout * 100
head(product_store_rate)

# Total stockouts by store
store_stockouts <- aggregate(stockout ~ Store.ID, data = df, sum)
store_stockouts <- store_stockouts[order(-store_stockouts$stockout), ]
print(store_stockouts)

# Pivot using reshape2
heatmap_data <- dcast(df, Store.ID ~ Product.ID, value.var = "stockout", fun.aggregate = mean)
rownames(heatmap_data) <- heatmap_data$Store.ID
heatmap_matrix <- as.matrix(heatmap_data[ , -1])

heatmap(heatmap_matrix, col = heat.colors(256),
        main = "Stockout Rate Heatmap (Store x Product)",
        xlab = "Product ID", ylab = "Store ID")

df$Store_Product <- paste(df$Store.ID, df$Product.ID, sep = "-")
stockout_sum <- aggregate(stockout ~ Store_Product, data = df, sum)

ggplot(stockout_sum, aes(x = Store_Product, y = stockout)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Total Stockouts per Store-Product") +
  theme(axis.text.x = element_text(angle = 90, size = 6, hjust = 1)) +
  xlab("Store-Product ID") + ylab("Total Stockouts")

# Sales by Season
season_sales <- aggregate(Units.Sold ~ Seasonality, data = df, sum)
ggplot(season_sales, aes(x = Seasonality, y = Units.Sold, fill = Seasonality)) +
  geom_bar(stat = "identity") + ggtitle("Units Sold by Season")

# Stockouts by Season
season_stockouts <- aggregate(stockout ~ Seasonality, data = df, sum)
ggplot(season_stockouts, aes(x = Seasonality, y = stockout, fill = Seasonality)) +
  geom_bar(stat = "identity") + ggtitle("Stockouts by Season")
   

# Sales by Promotion
promo_sales <- aggregate(Units.Sold ~ Promotion, data = df, sum)
ggplot(promo_sales, aes(x = factor(Promotion), y = Units.Sold, fill = factor(Promotion))) +
  geom_bar(stat = "identity") + ggtitle("Units Sold by Promotion") +
  xlab("Promotion (0=No, 1=Yes)")

# Stockouts by Promotion
promo_stockouts <- aggregate(stockout ~ Promotion, data = df, sum)
ggplot(promo_stockouts, aes(x = factor(Promotion), y = stockout, fill = factor(Promotion))) +
  geom_bar(stat = "identity") + ggtitle("Stockouts by Promotion") +
  xlab("Promotion (0=No, 1=Yes)") 

cat_stockouts <- aggregate(stockout ~ Category, data = df, sum)
cat_stockouts <- cat_stockouts[order(-cat_stockouts$stockout), ]
cat_stockouts$cumulative <- cumsum(cat_stockouts$stockout)
cat_stockouts$cum_pct <- cat_stockouts$cumulative / sum(cat_stockouts$stockout) * 100

ggplot(cat_stockouts, aes(x = reorder(Category, -stockout), y = stockout)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_line(aes(y = cum_pct * max(stockout) / 100, group = 1), color = "red") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 100 / max(cat_stockouts$stockout), name = "Cumulative %")) +
  labs(title = "Stockouts by Category (Pareto)", x = "Category", y = "Stockouts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
