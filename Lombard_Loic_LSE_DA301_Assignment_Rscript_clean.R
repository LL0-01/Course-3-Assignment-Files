## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages(tidyverse)
library(tidyverse)

# Import the turtle_sales.csv data set.
sa <- read.csv(choose.files(), header=TRUE)

# Print & explore the data frame.
view(sa)
head(sa)
as_tibble(sa)
summary(sa)
str(sa)
dim(sa)

## calculate residual sales
sa <- sa %>%
  mutate(Residual_Sales = Global_Sales - NA_Sales - EU_Sales) %>%
  select(Ranking, Product, Platform, Year, Genre, Publisher, NA_Sales, EU_Sales, 
         Residual_Sales, Global_Sales)

## view the new df
view(sa)

# note: there are negative residual sales in 2002-04 - rounding errors likely
#Round sales numbers
sa$Residual_Sales <- round(sa$Residual_Sales, 2)
sa$NA_Sales <- round(sa$NA_Sales, 2)
sa$EU_Sales <- round(sa$EU_Sales, 2)

## rename columns to lowercase
names(sa) <- tolower(names(sa))

## rename product ID
names(sa)[2] <- "product_id"

## check for NA
sa_na_bar <- is.na(sa)
unique(sa_na_bar)

# remove observations with NA values in Year
sa <- na.omit(sa)

# check if NA items are removed
unique(is.na(sa))

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sa_num <- select(sa, -ranking, -year,-genre,-publisher, -platform)

## check for na values
sa_num_na <- sa_num %>% filter(rowSums(is.na(.)) > 0)

## view the filtered data
sa_num_na

# View the data frame.
head(sa_num)

# View the descriptive statistics.
summary(sa_num)


## for future reference: Summary of the 'why' action were taken: 
# dataset was explored to get a high level grasp for it
# dataset was reduced to relevant data (sales and platform)
# set was checked for NA values, that could complicate analysis - there were none
# the column names lowercase due to easier handling in analysis
# duplicate values need not be removed as there aren't any based from earlier analysis


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
# compare NA and EU sales
ggplot(data=sa_num, mapping=aes(x=na_sales, y=eu_sales)) +
  geom_point() +
  geom_smooth(method=lm)


## 2b) Histograms
# Create histograms.

# Convert data to pivot format
sa_num_hist <- sa_num %>%
  pivot_longer(cols = c(na_sales, eu_sales, residual_sales, global_sales),
               names_to = "variable",
               values_to = "value")

# Create faceted histogram
ggplot(sa_num_hist, aes(x = value)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "white") +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  xlab("Sales") +
  ylab("Frequency") +
  ggtitle("Distribution of Sales by Variable") +
  theme_minimal()

## 2c) Boxplots
# Create boxplots.

# convert data frame to clean format
sa_num_box <- sa_num %>%
  pivot_longer(cols = c(na_sales, eu_sales, residual_sales),
               names_to = "region",
               values_to = "value")

# reorder levels of region factor based on sales values
sa_num_box$region <- factor(sa_num_box$region, levels = c("na_sales", "eu_sales", "residual_sales"),
                         ordered = TRUE, 
                         labels = c("NA Sales", "EU Sales", "Residual Sales"))

# create boxplot and cut off the outliers at 15
ggplot(sa_num_box, aes(x = region, y = value)) +
  geom_boxplot() +
  xlab("Sales Region") +
  ylab("Sales") +
  ggtitle("Sales by Region") +
  theme_minimal() +
  guides(fill=guide_legend(title="Region")) +
  coord_cartesian(ylim = c(0, 15))
 

## 2d) compare all sales in a barchart per year

# install the relevant package/library
install.packages("reshape2")
library(reshape2)

# Melt the data to long format
sa_melt <- melt(sa, id.vars = "year", measure.vars = c("na_sales", "eu_sales", "residual_sales"))

# convert into interactive plot for better viewing
install.packages(plotly)
library(plotly)

# create plot
sa_bar_int <- ggplot(sa_melt, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Region", x = "Year", y = "Sales", fill = 'Region') +
  scale_x_continuous(name = "Year", breaks = seq(min(sa_melt$year), max(sa_melt$year), by = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 

# plot the plot
ggplotly(sa_bar_int)

# save the plot for the report
ggsave("sales_total.png", sa_bar_int , dpi = 150, width = 10, height = 7)


# bar chart of the title count per platforms
# Order by count in ascending order
sa_count <- sa %>% 
  count(platform) %>% 
  arrange(n)

# Plot the bar chart
ggplot(sa_count, aes(x = reorder(platform, n), y = n)) +
  geom_bar(fill = "lightblue", stat = "identity") +
  labs(title = "Range of Platforms and Titles", x = "Platform", y = "Games Released") +
  coord_flip() +
  theme_classic() +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.1)



###############################################################################

# 3. Observations and insights

## 
# - 2 observations with NA have been removed, looks like data entry issues
# - NA sales lead most of the time, making it the most important market.
#   perhaps a good idea to segment sales into the different countries 
#   of units corresponding to how our marketing splits the market. This enables 
#   more granular insight. 
# - Very variable sales, one year great another less so likely due to "Blockbuster" 
#   nature of the business corresponding to popular games. 
#   See visualizations of barplots and histograms.
# - In some years ('03-'04) EU & NA are larger than global sales, sales 
#   department should have a closer look at the rounding practice, it seems 
#   not well rounded. 
# - 2016 seems to be an year with incomplete data, likely snapshot taken early 
#   on in Jan/feb.
####
# First impression suggestions to improve the business:
# 1.  To generate a more predictable revenue stream, which would help in 
#     running the business more smoothly
#     Marketing and Product could think about a game subscription model via 
#     online distribution ("netflixiszation") to open up a blockbuster 
#     isolated revenue stream. Alternatively creating platforms for competitive 
#     gaming online. big change in business though
# 2.  Irrespective of the cyclical nature of the business, a good addition 
#     could be merchandise of blockbusters to upsell on popular titles.
# 3.  Platform wise we seem to be selling most titles on the 5-10 popular 
#     and recent ones. Product department should consider when to cut an older 
#     title from inventory, keeping in mind a large secondary market on internet 
#     platforms.


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
view(sa)
head(sa)

# Check output: Determine the min, max, and mean values.
summary(sa)

# View the descriptive statistics.
library(psych)
describe(sa)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sa_top_sellers <- sa %>%
  group_by(product_id) %>%
  summarise(total_na_sales = sum(na_sales),
            total_eu_sales = sum(eu_sales),
            total_residual_sales = sum(residual_sales),
            total_global_sales = sum(global_sales)) %>%
  arrange(desc(total_global_sales))


## Group data based on Platform and determine the sum per platform
sa_top_platform <- sa %>%
  group_by(platform) %>%
  summarise(total_na_sales = sum(na_sales),
            total_eu_sales = sum(eu_sales),
            total_residual_sales = sum(residual_sales),
            total_global_sales = sum(global_sales)) %>%
  arrange(desc(total_global_sales))

# view top platforms
view(sa_top_platform)

# View the first 10 entries of top sellers data frame.
sa_top_sellers


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

p <- ggplot(sa, aes(x = eu_sales, y = na_sales, color = residual_sales)) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_gradient(low = "blue", high = "red", na.value = "grey") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "darkblue", size = 0.5) +
  labs(title = "EU Sales vs. NA Sales by Residual Sales", x = "EU Sales", y = "NA Sales", color = "Residual Sales") +
  theme(legend.title = element_text(size = 12),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

# Show plot
ggplotly(p)

# save the plot for the report
ggsave("sales_correlation.png",p, dpi = 150, width = 10, height = 7)


# Create histograms.
ggplot(data=sa_top_sellers, mapping=aes(x = total_global_sales)) +
  geom_histogram(fill = "lightblue", color = 'black')

# Histogram of total_na_sales
ggplot(data=sa_top_sellers, mapping=aes(x=total_na_sales)) +
  geom_histogram(fill = "darkorange", color = 'black') +
  labs(title="Total NA Sales Histogram")

# Histogram of total_eu_sales
ggplot(data=sa_top_sellers, mapping=aes(x=total_eu_sales)) +
  geom_histogram(fill = "green", color = 'black') +
  labs(title="Total EU Sales Histogram")

# Histogram of total_residual_sales
ggplot(data=sa_top_sellers, mapping=aes(x=total_residual_sales)) +
  geom_histogram(fill = "blue", color = 'black') +
  labs(title="Total Residual Sales Histogram")

# Histogram of total_global_sales
ggplot(data=sa_top_sellers, mapping=aes(x=total_global_sales)) +
  geom_histogram(fill = "lightblue", color = 'black') +
  labs(title="Total Global Sales Histogram")


# Create boxplots.
ggplot(data=sa_top_sellers, 
            mapping=aes(x=product_id, y=total_na_sales)) +
  geom_boxplot(fill = "darkorange", color = 'black') +
  labs(title="Total NA Sales")

ggplot(data=sa_top_sellers, 
            mapping=aes(x=product_id, y=total_eu_sales)) +
  geom_boxplot(fill = "green", color = 'black') +
  labs(title="Total EU Sales")

ggplot(data=sa_top_sellers, 
            mapping=aes(x=product_id, y=total_residual_sales)) +
  geom_boxplot(fill = "blue", color = 'black') +
  labs(title="Total Residual Sales")

ggplot(data=sa_top_sellers, 
            mapping=aes(x=product_id, y=total_global_sales)) +
  geom_boxplot(fill = "lightblue", color = 'black') +
  labs(title="Total Global Sales")



###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
ggplot(data = sa_top_sellers, mapping = aes(sample = total_na_sales)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Total NA Sales QQ Plot") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 16))

ggplot(data = sa_top_sellers, mapping = aes(sample = total_eu_sales)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Total EU Sales QQ Plot") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 16))

ggplot(data = sa_top_sellers, mapping = aes(sample = total_residual_sales)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Total Residual Sales QQ Plot") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 16))

ggplot(data = sa_top_sellers, mapping = aes(sample = total_global_sales)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Total Global Sales QQ Plot") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 16))

## scatter plot is the most apealing that provides most value. 
## histograms tell how sales are distributed
## boxplots show the - only positive - outliers
## data does not seem normal

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages(moments)
library(moments)

# Perform Shapiro-Wilk test.
# H0 = Data is not normally distributed (p<0.05)
# H1 = Data is normally distributed (p>0.05)

# Shapiro-Wilk test for total_na_sales
shapiro.test(sa_top_sellers$total_na_sales)

# Shapiro-Wilk test for total_eu_sales
shapiro.test(sa_top_sellers$total_eu_sales)

# Shapiro-Wilk test for total_residual_sales
shapiro.test(sa_top_sellers$total_residual_sales)

# Shapiro-Wilk test for total_global_sales
shapiro.test(sa_top_sellers$total_global_sales)

## all p-values are very very low, the data of all variables is not normally 
# distributed

## Shapiro-Wilk test for non-aggregated data
shapiro.test(sa_num$na_sales)
shapiro.test(sa_num$eu_sales)
shapiro.test(sa_num$residual_sales)
shapiro.test(sa_num$global_sales)

## same as above, all non-normal. 


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
describe(sa_top_sellers)
describe(sa_num)

## Skewness: 
# all relevant variables have large positive skew, indicating that the data 
# skews to the right, this is likely due to the positive outliers

## Kurtosis: 
# most variables can be considered leptokurtic, except residual_sales. 
# This is likely again due to outliers which are most pronounced in variables 
# with high kurtosis levels


## 3d) Determine correlation
# Determine correlation.

cor(sa_top_sellers)
cor(sa_num)

##
# Sales correlate positively, indicating that a hit in one 
# region of the world will be one in another too. 



###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

## scatter plot is the best way to check the correlation between the sales data 
## of the different regions. 

p <- ggplot(sa, aes(x = eu_sales, y = na_sales, color = residual_sales)) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_gradient(low = "blue", high = "red", na.value = "grey") +
  labs(title = "EU Sales vs. NA Sales by Residual Sales", x = "EU Sales", y = "NA Sales") +
  geom_smooth(method = "lm", se = FALSE)

# Show plot
plot(p)

## checking plot as well for the aggregated data of the df sa_top_sellers
## scatter plot is the best to check the correlation between the sales data of 
# the different regions. 

p <- ggplot(sa_top_sellers, aes(x = total_eu_sales, y = total_na_sales, color = total_residual_sales)) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_gradient(low = "blue", high = "red", na.value = "grey") +
  labs(title = "EU Sales vs. NA Sales by Residual Sales", x = "EU Sales", y = "NA Sales") +
  geom_smooth(method = "lm", se = FALSE)

# Show plot
plot(p)

## Same picture as with non-aggregated data. Decision to focus on non-aggregated 
## data (sa) for the linear regression part. -> due to more data points

# a linear regression model could inform how see closely they are correlated 
# Fit linear regression model
lm_model <- lm(na_sales ~ eu_sales, data = sa)
summary(lm_model)

# There is some significant correlation between the sales regions with R2=0.5


###############################################################################

# 5. Observations and insights
##
# The data is not be trusted 100%, tests performed have revealed irregularities. 
# The sales data is  not distributed normally across the regions. The outliers 
# underline the blockbuster nature of the video games business, this happens 
# globally although not always simultaneously 
# This also informs us that our marketing and promotion can drive campaigns 
# and invest in titles that sold well in one region, because they will likely 
# do well in another too, having more simultaneous multi-regional releases, 
# its becomes more rare 
##


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
head(sa)

# Determine a summary of the data frame.
describe(sa)
str(sa)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
lm_model <- lm(na_sales ~ eu_sales, data = sa)
summary(lm_model)


## 2b) Create a plot (simple linear regression)
# Create a scatter plot with the regression line
ggplot(sa, aes(x = eu_sales, y = na_sales)) +
  geom_point(alpha = 0.7, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "NA Sales vs. EU Sales", x = "EU Sales", y = "NA Sales")


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
# Create a multiple linear regression model
lm_model2 <- lm(global_sales ~ na_sales + eu_sales + ranking,
                data = sa)

# Multiple linear regression model.
summary(lm_model2)

# omitt ranking variable
lm_model3 <- lm(global_sales ~ na_sales + eu_sales,
                data = sa)

# Multiple linear regression model.
summary(lm_model3)

# model 2 seems to be more accurate
# check for multicollinearity
# Load the car package
library(car)

## factors impacting the accuracy of statistical multivariable model
## multicollinearity
## Calculate VIF for each variable
vif(lm_model2)

## VIF values are all below 3, multicollienarity should not be an 
## issue in this data set

##
# model 2 seems to be the most accurate one. Crucial info: residual 
# sales is omitted on purpose, its a calculated value and will close the model 
# gap to an R2 of 1 and it would not work with the next part of the assignment...
## 
# It is counter intuitive that the variable: Ranking is statistically not very relevant,
# usually consumers - and especially - data driven gamers/nerds rely on such info
# when making a purchase. It remains included because it marginally improves the model.
## 

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# new df with test data
sa_test <- data.frame(
  na_sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
  eu_sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Compare dfs before prediction
head(sa_test)
head(sa)

# Create a new object and specify the predict function.
predictTest = predict(lm_model3, newdata=sa_test,
                      interval='confidence')

# Print the object.
predictTest

# predictions: 
#####
### fit       lwr       upr         Actual  Accurate?
# 1 71.460784 70.150426 72.771143¦  67.85   no
# 2  6.858034  6.719488  6.996579¦  6.04    no
# 3  4.250722  4.103385  4.398059¦  4.32    YES!
# 4  4.136789  4.010304  4.263274¦  3.53    no
# 5 26.437646 25.414290 27.461002¦  23.21   no

# Not a great model as this prediction test shows

# Test for heteroscedasticity
# import the right library
library(lmtest)

# run the Beusch-pagan test for  heteroscedasticity
bptest(lm_model3)
# p<0.05 indicates heteroscedasticity

# Create a QQ plot of the residuals
qqnorm(lm_model3$residuals)
qqline(lm_model3$residuals)
# not a good fit

# white test
coeftest(lm_model)

# as with the Beusch-pagan test a p<0.05 indicates heteroscedasticity

# Data is indeed heteroscedastic, another reason our linear model is inaccurate. 
# Causes for heteroscedasticity: 
# Non-normal data distribution, likely due to the large outliers and large
# variety in data

### Fixes against heteroscedasticity: 
## More data: 
# Sell more games and get better & mainly more data going forward.
## Removing outliers:
# This was done below and does not eliminate skew or heteroscedasticity 
# to the degree needed.
## Box-Cox transformation:
# A BC transformation is a tool to normalize the sales data, this can be  
# attempted with after discussion of details with the marketing department.
# Analysis outcomes of heavily wrangled data needs a reality check, 
# perhaps the reduction of such a large portion of noise render 
# the finding useless? Decision to not pursue this for the time being. 

# create a new df for normalisation attempt
sa_num_norm<-sa_num

# Create a boxplot of the dataset to identify outliers
boxplot(sa_num_norm$global_sales)

# Determine the threshold for outliers on gobal sales
q1 <- quantile(sa_num_norm$global_sales, 0.25)
q3 <- quantile(sa_num_norm$global_sales, 0.75)
iqr <- q3 - q1
outlier_threshold <- q3 + 1.5 * iqr

# Identify outliers in the dataset
outliers <- sa_num_norm$global_sales > outlier_threshold

# Create a new dataset without outliers
sa_num_norm <- sa_num_norm[!outliers, ]


# Shapiro-Wilk test for sa_num_norm
shapiro.test(sa_num_norm$global_sales)

## removing outliers makes it a bit more normally distributed, but its 
## still a very skewed dataset and not normally distributed


###############################################################################

# 5. Observations and insights
## OBSERVATIONS ADN INSIGHTS 
# Predictive Linear regression models have a set of conditions that need to be 
# fulfilled to predict with accuracy. In this case two conditions are not met.
# The data is not normally distributed (1) and displays heteroscedasticity (2).
# Consequences are, that significance of tests being either too high or too low, 
# Standard errors are biased and OLS estimators are not useful either.
# This can lead to inaccurate predictions and confidence intervals.
# This is evident in the attempt at predicting values that even have helped to 
# shape the model. The predictions are close but far from reliable. 
## Why doesn't it work?
# THE OUTLIERS. Due to the blockbuster nature of the games business a breakout hit 
# will skew the data considerably impacting a predictive linear model. 
# LITTLE DATA. Outliers in combination with the turtle games sales strategy to only 
# focus on a selected few of titles, will amplify the outlier issue and skew 
# business data even more for it to display unpredictable, even erratic patterns.
# This is because outliers gain even more weight in a smaller data set.
# NOISE: Data displays a lot of noise and variance. 
## FIXES:
# MORE DATA: Since inception of Turtle games an average of 500 video games 
# are released yearly (Wikipedia), Turtle games has released 350 in total. 
# There would be potential for more titles - and better (more) data going forward.
# it will also lead to more revenue, however, profitability is another matter 
# that needs to be considered. 
# FIX DATA: Box-Cox transformation, a BC transformation is a tool to normalize 
# the global_sales data, the could work and can be explored further on request
# REMOVING OUTLIERS: This was done and doesn't impact the data sets distribution enough to 
# enable linear regression


###############################################################################
###############################################################################


# Notes: a short evaluation on the review Dataset in R

# Import the review.csv data set.
rw <- read.csv(choose.files(), header=TRUE)


names(rw)
# rename columns
rw <- rename(rw, spending_score = spending_score..1.100., remuneration = remuneration..k..)

# Print & explore the data frame.
view(re)
head(rw)
as_tibble(rw)

## reduce to numerical values
rw_num <- rw [, c('spending_score', 'remuneration','loyalty_points', 'age')]


# Shapiro-Wilk test for Rw data to check normal distribution
shapiro.test(rw_num$spending_score)
shapiro.test(rw_num$remuneration)
shapiro.test(rw_num$loyalty_points)

#check statistics
library(psych)
describe(rw_num)
# no significant skewness or kurtosis
describe(sa)

# multiple linear regression loyalty vs. spending & renumeration
lm_modela <- lm(loyalty_points ~ spending_score + remuneration,
                data = rw_num)

# multiple linear regression loyalty vs. renumeration
lm_modelb <- lm(loyalty_points ~ remuneration,
                data = rw_num)

# multiple linear regression loyalty vs. spending 
lm_modelc <- lm(loyalty_points ~ spending_score,
                data = rw_num)

# check Linear regression models outcome - age omitted cause no correlation 
# found in Python analysis
summary(lm_modela)
summary(lm_modelb)
summary(lm_modelc)

#import necessary library
library(lmtest)

# run the Beusch-pagan test for  heteroscedasticity
bptest(lm_modela)
bptest(lm_modelb)
bptest(lm_modelc)
# all are heteroscedastic -> limited data viability for predictive regression. 

# check scatterplot if age impacts remuneration 
ggplot(rw, aes(x = remuneration , y = loyalty_points )) +
  geom_point(aes(color = age)) +
  labs(x = "Remuneration", y = "Loyalty", color = "Age") +
  theme_classic()

# it doesnt

#check relation between spending and remuneration -> Clusters
ggplot(rw, aes(x = remuneration , y = spending_score )) +
  geom_point(aes(color = age)) +
  theme_classic()




