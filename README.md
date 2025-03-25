# Disable scientific notation 
options(scipen=999) 
# Import the dataset 
ndvi_data <- read.csv("newall_NDVI.csv",stringsAsFactors = T)
 # Convert date column to Date format
 ndvi_data$Date <- as.Date(ndvi_data$Date,format="%d/%m/%Y") 
# Set up plot window 
par(mfrow = c(1,1)) 
# Create time series plot 
plot(ndvi_data$MEAN ~ ndvi_data$Date, type = "l", xlab = "Date", ylab = "NDVI Value", main = "NDVI Time Series") 
# Create the model 
smooth_model <- loess(ndvi_data$MEAN ~ as.numeric(ndvi_data$Date)) 
# add the predicted values lines
(ndvi_data$Date, predict(smooth_model),col = "blue",lwd = 2)
Set up plot window
par(mfrow = c(1,1))
Create plot with first line
plot(ndvi_data$Date, ndvi_data$MEAN, type = "l", col = "blue", xlab = "Date", ylab = "NDVI Values", main = "Temporal NDVI Variation")
Add additional lines
lines(ndvi_data$Date, ndvi_data$MAX, col = "red") 
lines(ndvi_data$Date, ndvi_data$MIN, col = "green")
Add legend
legend ("topright", legend = c("Mean NDVI", "Max NDVI", "Min NDVI"), col = c("blue", "red", "green"), lty = 1)
2. Seasonal Analysis
# Create Season column*
ndvi_data$Season <- ifelse(month(ndvi_data$Date) %in% c(6,7,8,9), "Dry", "Wet")
# Calculate seasonal statistics*
dry_data <- subset(ndvi_data, Season == "Dry")
wet_data <- subset(ndvi_data, Season == "Wet")
seasonal_stats <- data.frame( *Season* = c("Dry", "Wet"), *mean_ndvi* = c(mean(dry_data$MEAN), mean(wet_data$MEAN)), *sd_ndvi* = c(sd(dry_data$MEAN), sd(wet_data$MEAN)))
*# Seasonal boxplot*
boxplot(MEAN ~ Season, *data* = ndvi_data, *col* = c("red", "blue"), *xlab* = "Season", *ylab* = "NDVI Value", *main* = "NDVI Distribution by Season")
3. Statistical Analysis
*# 1. T-test between seasons*
season_ttest <- t.test(MEAN ~ Season, *data* = ndvi_data)
*# 2. Monthly ANOVA*
ndvi_data$Month <- factor(month(ndvi_data$Date))monthly_anova <- aov(MEAN ~ Month, *data* = ndvi_data)
*# 3. Yearly trend analysis*
*# Create year column*
ndvi_data$Year <- as.numeric(format(ndvi_data$Date, "%Y"))
*# Calculate yearly means*
yearly_means <- tapply(ndvi_data$MEAN, ndvi_data$Year, mean)
yearly_ndvi <- data.frame( *Year* = as.numeric(names(yearly_means)), *mean_ndvi* = as.numeric(yearly_means))
*# Linear regression for yearly trend*
yearly_model <- lm(mean_ndvi ~ Year, *data* = yearly_ndvi)
*# Print statistical results*
cat("T-test Results (Dry vs Wet Seasons):\\n")
print(season_ttest)
cat("\\nMonthly ANOVA Results:\\n")
print(summary(monthly_anova))
cat("\\nYearly Trend Analysis:\\n")
print(summary(yearly_model))
*# Post-hoc test for monthly differences*
tukey_results <- TukeyHSD(monthly_anova)
cat("\\nMonthly Comparisons (Tukey HSD):\\n")
print(tukey_results)
4. Yearly NDVI Trend Plot
*# Create yearly NDVI trend plot
plot(yearly_ndvi$Year, yearly_ndvi$mean_ndvi, *type* = "o", *xlab* = "Year", *ylab* = "Mean NDVI Value", *main* = "Yearly NDVI Trend")*# Add regression line*abline(yearly_model, *col* = "blue")
# Calculate year-over-year changes*
yearly_ndvi$ndvi_change <- c(NA, diff(yearly_ndvi$mean_ndvi))*
# Plot year-over-year changes
barplot(yearly_ndvi$ndvi_change[-1], *names.arg* = yearly_ndvi$Year[-1], *col* = "steelblue", *xlab* = "Year", *ylab* = "NDVI Change from Previous Year", *main* = "Year-over-Year NDVI Changes")
5. Additional Analysis
*# Mann-Kendall test (requires Kendall package)
# install.packages("Kendall")*
library(Kendall)
mk_test <- MannKendall(yearly_ndvi$mean_ndvi)
print(mk_test)
*# Time series analysis
# Create time series object*
ts_data <- ts(ndvi_data$MEAN, *frequency* = 12) *
# Assuming monthly data*decomp <- decompose(ts_data)plot(decomp) *
# Plots trend, seasonal, and random components
# Monthly means*monthly_means <- tapply(ndvi_data$MEAN, list(factor(month(ndvi_data$Date), *levels*=1:12, *labels*=month.abb)), mean)monthly_sds <- tapply(ndvi_data$MEAN, list(factor(month(ndvi_data$Date), *levels*=1:12, *labels*=month.abb)), sd)monthly_patterns <- data.frame( *Month* = factor(month.abb, *levels* = month.abb), *mean_ndvi* = as.numeric(monthly_means), *sd_ndvi* = as.numeric(monthly_sds))*# Plot Monthly Patterns*plot(1:12, monthly_patterns$mean_ndvi, *type* = "o", *xlab* = "Month", *ylab* = "Mean NDVI Value", *main* = "Monthly NDVI Patterns", *xaxt* = "n")axis(1, *at* = 1:12, *labels* = month.abb)*# Add error bars*arrows(1:12, monthly_patterns$mean_ndvi - monthly_patterns$sd_ndvi, 1:12, monthly_patterns$mean_ndvi + monthly_patterns$sd_ndvi, *length* = 0.05, *angle* = 90, *code* = 3)
6. 2024 Spike Investigation
*# Compare 2024 with previous years*ndvi_data$is_2024 <- ndvi_data$Year == 2024*# T-test comparing 2024 vs previous years*spike_test <- t.test(MEAN ~ is_2024, *data* = ndvi_data)print(spike_test)*# Calculate yearly statistics*yearly_stats <- aggregate(MEAN ~ Year, *data* = ndvi_data, FUN = function(x) c(*mean* = mean(x), *sd* = sd(x)))yearly_stats <- data.frame( *Year* = yearly_stats$Year, *mean_ndvi* = yearly_stats$MEAN[,1], *sd_ndvi* = yearly_stats$MEAN[,2])*# Visualize the spike*plot(yearly_stats$Year, yearly_stats$mean_ndvi, *type* = "o", *xlab* = "Year", *ylab* = "Mean NDVI Value", *main* = "Yearly NDVI Trends with 2024 Spike")*# Add error bars*arrows(yearly_stats$Year, yearly_stats$mean_ndvi - yearly_stats$sd_ndvi, yearly_stats$Year, yearly_stats$mean_ndvi + yearly_stats$sd_ndvi, *length* = 0.05, *angle* = 90, *code* = 3)
