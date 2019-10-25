library("readxl")
library(sqldf)
library("PerformanceAnalytics")
library(car)

#Import two data sets
sales <- read_excel('/Users/travisnakano/Downloads/Handset Sales.xls')
promo <- read_excel('/Users/travisnakano/Downloads/Handset (11).xls')

#rename columns for use with SQLDF package
colnames(promo)[colnames(promo)=="Start Date"] <- "start_date"
colnames(promo)[colnames(promo)=="End Date"] <- "end_date"
colnames(promo)[colnames(promo)=="Promo Type"] <- "promo_type"
promo$Carrier <- as.character(promo$Carrier)
promo$Carrier[promo$Carrier == "Verizon Wireless"] <- "Verizon"
promo$Carrier[promo$Carrier == "U.S. Cellular"] <- "US Cellular"
colnames(sales)[colnames(sales)=="Week Starting"] <- "week_starting"
colnames(sales)[colnames(sales)=="Online Price (After All Discounts)"] <- "MSRP"
colnames(sales)[colnames(sales)=="Weeks Since Launch At Operator"] <- "weeks_since_launch"
colnames(sales)[colnames(sales)=="Calender Week #"] <- "week_num"

#Merge two data sets together to align with week starting format

data <- sqldf('SELECT *
              FROM sales a
              LEFT JOIN promo b
              ON a.week_starting BETWEEN b.start_date AND b.end_date
              AND a.Model = b.Model
              AND a.Operator = b.carrier
              WHERE a.year IN (2016, 2017, 2018, 2019)
              AND a.Country = "US"')

#replace NA with 0 to correct errors in promo value calculation
data[is.na(data)] <- 0

#Split BOGO value in half since two devices are required for promo to be valid and trade ins by 5 to account for smaller opportunity size in market.
data2 <- sqldf('SELECT
               Year,
               week_starting,
               weeks_since_launch,
               week_num,
               Operator, 
               Manufacturer, 
               Model, 
               OS, 
               ROUND(Quantity, 2) AS quantity,
               promo_type,
               MSRP,
               CASE
                  WHEN promo_type = "BOGO" THEN Value/2
                  WHEN promo_type = "Trade-in" THEN value/5
                  ELSE Value END AS value
               FROM data')

#Create discount percentage metric to normalize discounts
data3 <- sqldf('SELECT
                Year, 
                week_starting,
                weeks_since_launch,
                week_num,
                Operator,
                Manufacturer,
                Model,
                OS,
                quantity,
                promo_type,
                CASE WHEN MSRP <300 THEN "Low"
                     WHEN MSRP <=600 AND MSRP >= 300 THEN "Mid"
                     ELSE "High" END AS price_tier,
                round(value/MSRP, 2) as discount_percentage
                FROM data2')

#wouldn't let me use case statement on Samsung Motorola or Google, joining to table to get around error.
data7 <- sqldf('SELECT Year, week_starting, weeks_since_launch, week_num, Operator, price_tier, Manufacturer, avg(discount_percentage) as discount_percentage
               FROM data3
               WHERE Manufacturer = "Samsung"
               GROUP BY 1,2,3,4,5')

data8 <- sqldf('SELECT Year, week_starting, week_num, weeks_since_launch, Operator, price_tier, Manufacturer, avg(discount_percentage) as discount_percentage
               FROM data3
               WHERE Manufacturer = "Motorola"
               GROUP BY 1,2,3,4,5')

data9 <- sqldf('SELECT Year, week_starting, week_num, weeks_since_launch, Operator, price_tier, Manufacturer, avg(discount_percentage) as discount_percentage
               FROM data3
               WHERE Manufacturer = "Google"
               GROUP BY 1,2,3,4,5')

data10 <- sqldf('SELECT Year, week_starting, weeks_since_launch, week_num, Operator, price_tier, Manufacturer, avg(discount_percentage) as discount_percentage
               FROM data3
               WHERE Manufacturer = "LG"
               GROUP BY 1,2,3,4,5')

#assign discount values into columns by manufacturer
data4 <- sqldf('SELECT
               a.Year,
               a.week_starting,
               a.weeks_since_launch,
               a.week_num,
               a.Operator,
               a.price_tier,
               b.discount_percentage as samsung_promo,
               c.discount_percentage as motorola_promo,
               d.discount_percentage as google_promo,
               e.discount_percentage as lg_promo,
               CASE WHEN a.Manufacturer = "Apple" THEN avg(a.discount_percentage) ELSE 0 END AS apple_promo,
               CASE WHEN a.Manufacturer NOT IN ("Google", "Apple", "Samsung", "Motorola") THEN avg(a.discount_percentage) ELSE 0 END AS other_promo 
               FROM data3 a
               JOIN data7 b
               ON a.Year = b.Year AND a.week_starting = b.week_starting AND a.Operator = b.Operator AND a.price_tier = b.price_tier
               JOIN data8 c
               ON a.Year = c.Year AND a.week_starting = c.week_starting AND a.Operator = c.Operator AND a.price_tier = c.price_tier
               JOIN data9 d
               ON a.Year = d.Year AND a.week_starting = d.week_starting AND a.Operator = d.Operator AND a.price_tier = d.price_tier
               JOIN data10 e
               ON a.Year = e.Year AND a.week_starting = e.week_starting AND a.Operator = e.Operator AND a.price_tier = e.price_tier
               WHERE a.Year IN (2019, 2018)
               GROUP BY 1,2,3,4,5,6,7')

#Section off dependent variable
data5 <- sqldf('SELECT
               Year,
               week_starting,
               weeks_since_launch,
               week_num,
               Operator,
               price_tier,
               quantity
               FROM data3
               WHERE manufacturer = "Google"')

#merge dependent and independent variables into single DF
data6 <- sqldf('SELECT a.week_starting,
               a.weeks_since_launch,
               a.week_num,
               a.price_tier,
               a.Year,
               a.quantity,
               a.Operator,
               b.apple_promo,
               b.samsung_promo,
               b.google_promo,
               b.other_promo,
               b.lg_promo,
               b.motorola_promo
               FROM data5 a
               JOIN data4 b
               ON a.Year = b.Year
               AND a.week_starting = b.week_starting
               AND a.Operator = b.Operator
               AND a.price_tier = b.price_tier')

#replace NA with 0 for MLR model
data6[is.na(data6)] <- 0

#run data for all pricing tiers
all_price_tiers <- subset(data6, Operator == "Verizon")
all_price_tiers <- all_price_tiers[, c('quantity', 'apple_promo', 'samsung_promo', 'google_promo', 'motorola_promo', 'other_promo')]

#issue with motorola and google promotions move on since limited reach on impact.
chart.Correlation(all_price_tiers[sapply(all_price_tiers, function(x) !is.factor(x))])

#poor results
lm_all_price_tiers = lm(quantity~samsung_promo+other_promo, data = all_price_tiers)
summary(lm_all_price_tiers)
confint(lm_all_price_tiers, conf.level=0.95)
vif(lm_all_price_tiers)

#break down into smaller categories
high_tier <- subset(data6, price_tier == "High")
high_tier <- subset(high_tier, Operator == "Verizon")

#add weeks since launch and week num as regressors
high_tier <- high_tier[, c('quantity', 'weeks_since_launch', 'week_num', 'samsung_promo', 'other_promo')]
chart.Correlation(high_tier[sapply(high_tier, function(x) !is.factor(x))])
lm_high_tier = lm(quantity~weeks_since_launch+week_num+samsung_promo+other_promo, data = high_tier)

#much better results
summary(lm_high_tier)
confint(lm_high_tier, conf.level=0.95)
#low multicolinerarity
vif(lm_high_tier)