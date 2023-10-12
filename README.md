Project_2: Contacting an API
================
Keren Vivas
2023-10-12

- <a href="#introduction" id="toc-introduction">Introduction</a>
- <a href="#required-packages" id="toc-required-packages">Required
  Packages</a>
- <a href="#functions-to-interact-with-the-api"
  id="toc-functions-to-interact-with-the-api">Functions to Interact with
  the API</a>
  - <a href="#1-product_type-function" id="toc-1-product_type-function">1.
    product_type function</a>
  - <a href="#2-product_currency-function"
    id="toc-2-product_currency-function">2. product_currency function</a>
  - <a href="#3-product_tag-function" id="toc-3-product_tag-function">3.
    product_tag function</a>
  - <a href="#4-product_brand_type-function"
    id="toc-4-product_brand_type-function">4. product_brand_type
    function</a>
  - <a href="#5-6-product_brand_type_price-function"
    id="toc-5-6-product_brand_type_price-function">5-6.
    product_brand_type_price function</a>
- <a href="#exploratory-data-analysis-eda-summarizing-data-from-an-api"
  id="toc-exploratory-data-analysis-eda-summarizing-data-from-an-api">Exploratory
  Data Analysis (EDA): Summarizing data from an API</a>
  - <a href="#one_way-contingency-table"
    id="toc-one_way-contingency-table">One_way contingency table</a>
  - <a href="#two_way-contingency-table"
    id="toc-two_way-contingency-table">Two_way contingency table</a>
  - <a href="#two_way-contingency-table-filtering-using-function"
    id="toc-two_way-contingency-table-filtering-using-function">Two_way
    contingency table filtering using function</a>
  - <a href="#stacked_bar_plot"
    id="toc-stacked_bar_plot">Stacked_bar_plot</a>
  - <a href="#violin-plot" id="toc-violin-plot">Violin plot</a>
  - <a href="#histogram" id="toc-histogram">Histogram</a>

# Introduction

This vignette provides a hands-on demonstration of data exploration
using an API. In this case, I have utilized the “Makeup” API database to
gain insights into the makeup market of various brands in the United
States, Canada, and the United Kingdom. To make this exploration
effective, I have created a set of custom functions that help to fetch
data from the API and select relevant information. With this data in
hand, I embarked on a journey of data exploration, examining how makeup
product prices vary across different markets, brands, and types of
makeup products. Let’s dive into the details and uncover the insights!

# Required Packages

I have made use of the below list set of packages to interact with the
Makeup API and to manipulate and summarize the data.

``` r
library(jsonlite)  # To interact with API
library(tidyverse) # To manipulate data
library(dplyr)     # To manipulate data
library(ggplot2)   # To plot summaries
```

# Functions to Interact with the API

Querying the selected API, **Makeup**!

``` r
query_API <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")
```

*Let’s begin by defining the set of functions created for interacting
with the Makeup API!*

## 1. product_type function

This function is designed to retrieve all rows of products where the
product type matches the user’s input called “type” and has “all” value
as a default. This function also considers the need for excluding all
products where product type is missing, it means, product type = NA.

``` r
product_type <- function(type="all") {
  
    #Retrieve API
    query_API <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")
    
    #Filter products by the specified product_type removing NA values
    filtered_products <- query_API[!is.na(query_API$product_type) & query_API$product_type == type, ]
    
    #Output
    return(filtered_products)
}
```

To test the product_type function, I requested the function to provide
all products related to mascara, foundation, and nail polish, each
separately.

``` r
mascara <- product_type("mascara")
foundation <- product_type("foundation")
nail_polish <- product_type("nail_polish")
```

## 2. product_currency function

This function is designed to retrieve all rows of products where the
currency of product matches the user’s input called “currency” and has
“all” value as a default. This function also considers the need for
excluding all products where product currency is missing, it means,
product type = NA.

``` r
product_currency <- function(currency="all") {
    
    #Retrieve API
    query_API <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")
    
    #Filter products by the specified currency removing NA values
    filtered_products <- query_API[!is.na(query_API$currency) & query_API$currency == currency, ]
    
    #Output
    return(filtered_products)
}
```

To test the product_currency function, I requested the function to
provide all products related to CAD, USD and GBP currency, each
separately.

``` r
CAD_prices <- product_currency("CAD")
USD_prices <- product_currency("USD")
GBP_prices <- product_currency("GBP")
```

## 3. product_tag function

This function is designed to first split the tag_list vector (all tags
available on product package), and retrieve all rows of products where
any of the new columns with product tag matches the user’s input called
“tag” and has “all” value as a default.

``` r
product_tag <- function(tag="all") {
  
  #Retrieve API
  query_API <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")

  # Split the tag_list into separate columns
  query_API2 <- query_API %>%
    separate(tag_list, into = c("Tag1", "Tag2", "Tag3", "Tag4", "Tag5", "Tag6", "Tag7", "Tag8"), sep = ",")

  # Filter rows where any of the columns contains the specified tag
  filtered_products <- query_API2 %>%
    filter_at(vars(starts_with("Tag")), any_vars(. == tag))
  
  #Output
  return(filtered_products)
}
```

To test the product_tag function, I requested the function to provide
all products related to Vegan, Canadian, and Natural product tag, each
separately.

``` r
Vegan_products <- product_tag("Vegan")
Canadian_products <- product_tag("Canadian")
Natural_products <- product_tag("Natural")
```

## 4. product_brand_type function

This function is designed to retrieve all rows of products where brand
and product type matches the user’s inputs called “brand” and “type”.
Both inputs with value “all” as default.

``` r
product_brand_type <- function(brand="all", type="all") {
    
    #Retrieve API
    query_API <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")
    
    #Filter products by the specified brand and type
    filtered_products <- query_API[!is.na(query_API$brand) & query_API$brand == brand & !is.na(query_API$product_type) & query_API$product_type == type, ]
    
    #Output
    return(filtered_products)
}
```

To test the product_brand_type function, I requested the function to
provide all foundations by deciem, mascaras by nyx, blushes by
mabelline, each separately.

``` r
deciem_foundation <- product_brand_type("deciem", "foundation" )
nyx_mascara <- product_brand_type("nyx", "mascara")
mabelline_blush <- product_brand_type("maybelline", "blush")
```

## 5-6. product_brand_type_price function

This function is designed to retrieve all rows of products where brand,
product type and price matches the user’s inputs called “brand”, “type”,
“price_operator”, and “price_value”. Price_operator can be greater or
lower to identify products with price greater/lower and equal to the
specified price_value. Brand and type are inputs with value “all” as
default.

``` r
product_brand_type_price <- function(brand= "all", type="all", price_operator, price_value) {
    
    #Retrieve API
    query_API <- fromJSON("http://makeup-api.herokuapp.com/api/v1/products.json")

    #Filter products by the specified brand, product_type, price_operator and price_value removing NA values
    filtered_data <- query_API[!is.na(query_API$brand) & query_API$brand == brand & !is.na(query_API$product_type) & query_API$product_type == type, ]
        if (price_operator == 'greater') {
    filtered_data <- filtered_data[!is.na(filtered_data$price) & filtered_data$price >= as.numeric(price_value), ]
     }
    else if (price_operator == 'lower') {
    filtered_data <- filtered_data[!is.na(filtered_data$price) & filtered_data$price <= as.numeric(price_value), ]
    }
  
    #Output
  return(filtered_data)
}
```

To test the product_brand_type_price function, I requested the function
to provide all lipstick by colourpop with price greater and equal to 5.

``` r
colourpop_lipstick_price <- product_brand_type_price("colourpop", "lipstick", "greater", "5")
```

# Exploratory Data Analysis (EDA): Summarizing data from an API

I wanted to try to find insights/answers to the following questions:

1.  What are the type of make-up products and brand with more options?
2.  What are the type of make-up products more expensive?
3.  What about within USA market and others?

To achieve this, the following summaries plot were deployed

## One_way contingency table

This contingency table aims to know what are those products that have
more options in the market. Based on the output, it is noticed that
foundations and lipstick head the product types followed by eyeliner,
mascara, etc.

``` r
# Select database
all_product_type <- query_API

# Select columns for the contingency table
contigency_data <- all_product_type[,"product_type"]

# Create a contingency table
contigency_table <- table(contigency_data)

# Print the contingency table
print(contigency_table)
```

    ## contigency_data
    ##       blush     bronzer     eyebrow    eyeliner   eyeshadow  foundation   lip_liner 
    ##          78          69          49         148          86         166          29 
    ##    lipstick     mascara nail_polish 
    ##         154          92          60

## Two_way contingency table

Now, I was curious to find out which brands have the most extensive
range of products and are leading the market with a significant number
of options. Based on the output, it is noticed that nyx is the brand
with more products available in the market follow by clinique,
maybelline and dior.

``` r
# Select database
all_product_type <- query_API

# Select columns for the contingency table
contigency_data <- all_product_type[,c("brand","product_type")]

# Create a contingency table
contigency_table <- table(contigency_data)

# Print the contingency table
print(contigency_table)
```

    ##                           product_type
    ## brand                      blush bronzer eyebrow eyeliner eyeshadow foundation lip_liner
    ##   almay                        1       1       0        3         2          3         0
    ##   alva                         0       0       0        0         1          0         0
    ##   anna sui                     1       0       0        3         0          0         0
    ##   annabelle                    1       1       0        7         0          1         1
    ##   benefit                      0       6      11        3         0          2         0
    ##   boosh                        0       0       0        0         0          0         0
    ##   burt's bees                  0       0       0        0         0          0         0
    ##   butter london                0       0       0        0         0          0         0
    ##   c'est moi                    0       0       0        1         0          0         1
    ##   cargo cosmetics              3       9       0        2         1          2         1
    ##   china glaze                  0       0       0        0         0          0         0
    ##   clinique                     8       5       6        9        12         34         2
    ##   coastal classic creation     0       0       0        0         0          0         0
    ##   colourpop                    0       0       0        0         0          1         1
    ##   covergirl                    7       2       0       12         5         10         1
    ##   dalish                       0       0       0        0         0          0         0
    ##   deciem                       0       0       0        0         0          2         0
    ##   dior                         5       1       5        6        11          9         0
    ##   dr. hauschka                 0       3       0        2         2          2         0
    ##   e.l.f.                       2       5       0        4         6          3         1
    ##   essie                        0       0       0        0         0          0         0
    ##   fenty                        0       0       0        1         0          2         0
    ##   glossier                     1       0       0        0         0          4         0
    ##   green people                 0       0       0        0         0          0         0
    ##   iman                         1       0       1        1         2          4         0
    ##   l'oreal                      2       1       0        9         1          7         2
    ##   lotus cosmetics usa          1       0       0        0         5          0         0
    ##   maia's mineral galaxy        0       0       1        0         0          0         0
    ##   marcelle                     1       2       0        4         1          2         3
    ##   marienatie                   1       0       0        2         2          1         0
    ##   maybelline                   4       3       0        8         7         10         1
    ##   milani                       2       1       0        5         1          1         1
    ##   mineral fusion               1       1       0        1         0          2         0
    ##   misa                         0       0       0        0         0          0         0
    ##   mistura                      0       0       0        0         1          0         0
    ##   moov                         0       0       0        0         0          0         0
    ##   nudus                        0       0       0        0         0          0         0
    ##   nyx                         12       9      18       32         1         32        10
    ##   orly                         0       0       0        0         0          0         0
    ##   pacifica                     1       1       0        1         4          0         1
    ##   penny lane organics          0       0       0        0         0          0         0
    ##   physicians formula           8       9       0        9         3          8         0
    ##   piggy paint                  0       0       0        0         0          0         0
    ##   pure anada                   3       1       0        1         2          4         0
    ##   rejuva minerals              1       2       0        0         2          1         0
    ##   revlon                       2       0       0        4         1          7         1
    ##   sally b's skin yummies       0       0       0        0         1          0         0
    ##   salon perfect                0       0       0        0         0          0         0
    ##   sante                        1       0       0        1         1          1         1
    ##   sinful colours               0       0       0        0         0          0         0
    ##   smashbox                     3       2       7        6         7          9         0
    ##   stila                        2       1       0        1         0          0         0
    ##   suncoat                      0       0       0        1         0          0         0
    ##   w3llpeople                   0       0       0        0         0          1         0
    ##   wet n wild                   0       0       0        5         3          0         0
    ##   zorah                        0       0       0        1         0          0         0
    ##   zorah biocosmetiques         0       0       0        1         1          0         0
    ##                           product_type
    ## brand                      lipstick mascara nail_polish
    ##   almay                           1       3           0
    ##   alva                            0       0           0
    ##   anna sui                        1       0           1
    ##   annabelle                       0       0           0
    ##   benefit                        13       6           0
    ##   boosh                           1       0           0
    ##   burt's bees                     2       0           0
    ##   butter london                   1       0           1
    ##   c'est moi                       1       1           0
    ##   cargo cosmetics                 2       0           0
    ##   china glaze                     0       0           1
    ##   clinique                       17       0           0
    ##   coastal classic creation        1       0           0
    ##   colourpop                       2       0           0
    ##   covergirl                       4      12           1
    ##   dalish                          1       0           0
    ##   deciem                          0       0           0
    ##   dior                           15       8          14
    ##   dr. hauschka                    2       1           0
    ##   e.l.f.                          3       3           0
    ##   essie                           0       0           4
    ##   fenty                           2       0           0
    ##   glossier                        1       0           0
    ##   green people                    0       1           0
    ##   iman                            1       3           0
    ##   l'oreal                         7       8           9
    ##   lotus cosmetics usa             0       1           0
    ##   maia's mineral galaxy           0       0           0
    ##   marcelle                        1       1           0
    ##   marienatie                      2       1           0
    ##   maybelline                      7      11           3
    ##   milani                          2       0           0
    ##   mineral fusion                  1       1           1
    ##   misa                            0       0           1
    ##   mistura                         0       0           0
    ##   moov                            0       0           3
    ##   nudus                           1       0           0
    ##   nyx                            39      11           0
    ##   orly                            0       0           4
    ##   pacifica                        1       2           2
    ##   penny lane organics             1       0           0
    ##   physicians formula              0       6           0
    ##   piggy paint                     0       0           1
    ##   pure anada                      1       1           3
    ##   rejuva minerals                 0       0           0
    ##   revlon                         11       0           3
    ##   sally b's skin yummies          1       0           0
    ##   salon perfect                   0       0           1
    ##   sante                           0       0           1
    ##   sinful colours                  0       0           1
    ##   smashbox                        6       6           0
    ##   stila                           0       0           0
    ##   suncoat                         0       1           4
    ##   w3llpeople                      0       0           0
    ##   wet n wild                      2       1           1
    ##   zorah                           0       1           0
    ##   zorah biocosmetiques            0       0           0

## Two_way contingency table filtering using function

By doing the same two_way contingency table but now filtering USD
market, it is noticed that in USD market the leading brands are nyx and
clinique for all product type including the most available in the
market.

``` r
# Select columns for the contingency table (brand and tag columns)
all_USD_product <- product_currency("USD")
contingency_data <- all_USD_product[, c("product_type", "brand")]

# Create a contingency table
contingency_table <- table(contingency_data)

# Print the contingency table
print(contingency_table)
```

    ##             brand
    ## product_type alva c'est moi clinique coastal classic creation fenty glossier green people
    ##   blush         0         0        7                        0     0        1            0
    ##   bronzer       0         0        5                        0     0        0            0
    ##   eyebrow       0         0        6                        0     0        0            0
    ##   eyeliner      0         1        9                        0     1        0            0
    ##   eyeshadow     1         0       12                        0     0        0            0
    ##   foundation    0         0       34                        0     2        4            0
    ##   lip_liner     0         1        2                        0     0        0            0
    ##   lipstick      0         1       17                        1     2        1            0
    ##   mascara       0         1        0                        0     0        0            1
    ##             brand
    ## product_type lotus cosmetics usa maia's mineral galaxy marienatie nudus nyx
    ##   blush                        1                     0          1     0   9
    ##   bronzer                      0                     0          0     0   7
    ##   eyebrow                      0                     1          0     0  18
    ##   eyeliner                     0                     0          2     0  31
    ##   eyeshadow                    5                     0          2     0   0
    ##   foundation                   0                     0          1     0  29
    ##   lip_liner                    0                     0          0     0   6
    ##   lipstick                     0                     0          2     1  36
    ##   mascara                      1                     0          1     0  11
    ##             brand
    ## product_type penny lane organics rejuva minerals sally b's skin yummies w3llpeople
    ##   blush                        0               1                      0          0
    ##   bronzer                      0               2                      0          0
    ##   eyebrow                      0               0                      0          0
    ##   eyeliner                     0               0                      0          0
    ##   eyeshadow                    0               2                      1          0
    ##   foundation                   0               1                      0          1
    ##   lip_liner                    0               0                      0          0
    ##   lipstick                     1               0                      1          0
    ##   mascara                      0               0                      0          0
    ##             brand
    ## product_type zorah biocosmetiques
    ##   blush                         0
    ##   bronzer                       0
    ##   eyebrow                       0
    ##   eyeliner                      1
    ##   eyeshadow                     1
    ##   foundation                    0
    ##   lip_liner                     0
    ##   lipstick                      0
    ##   mascara                       0

## Stacked_bar_plot

To understand the contribution of each market in the Makeup API, it was
important to identify how it is the distribution of product types across
Canada, United States, and United Kingdom. To achieve this, a stacked
bar plot were performed to acount for all products depending on their
type and currency. It is noticed that the biggest contributions
corresponding to USD products, followed by United Kingdom and lastly
Canada.

``` r
# Filter data to avoid NA values within currency and product_type
my_data_filtered <- query_API[!is.na(query_API$currency) & !is.na(query_API$product_type), ]

# Create a stacked bar plot with a custom color palette
stacked_bar_plot <- ggplot(my_data_filtered, aes(x = product_type, fill = currency)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Product Type Counts by Currency",
       x = "Product Type", y = "Count") +
  scale_fill_brewer(palette = "Set1", type = "qual", direction = 1)  # Choose "Set1" with three colors

# Print the stacked bar plot
print(stacked_bar_plot)
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Violin plot

Regarding price, I wanted to know what are the products that have the
highest price and how the price is distributed. To achieve this, I
performed a violin plot of prices for each of the product type available
in the market. The results show the products with higher prices have a
more spread distribution when compared with product with product with
lower price. To ilustrate, bronzer vs. lipstick. I could conclude that
eyeshadow, bronzer, eyebrow, and foundation are the product types with
more costly options.

``` r
# Filter data to avoid NA values within price and product_type
my_data_filtered <- query_API[!is.na(query_API$price) & !is.na(query_API$product_type), ]

# Create a violin plot with a custom color palette
violin_plot <- ggplot(my_data_filtered, aes(x = product_type, y = as.numeric(price), fill = product_type)) +
  geom_violin() +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "navy", "yellow", "brown", "darkgreen"))+  # Specify custom colors
  labs(title = "Violin Plot of Prices by Product type",
       x = "Product type", y = "Price")

# Print the violin plot
print(violin_plot)
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Histogram

To see the last results in a different way, I performed a histogram of
prices for each of the product type available in the market. Looking
these results, the order seems to be clearer, bronzer has the most
expensive options in the market followed by eyeshadom, eyebrow and
lastly foundation.

``` r
# Filter data to avoid NA values within price and product_type
my_data_filtered <- query_API[!is.na(query_API$price) & !is.na(query_API$product_type), ]

  # Create a single boxplot for prices without faceting
  histogram_plot <- ggplot(my_data_filtered, aes(x = as.numeric(price), fill = product_type)) +
  geom_histogram() +
     scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "navy", "yellow", "brown", "darkgreen"))+  # Specify custom colors
 labs(title = "Histogram of Prices Faceted by Product Type", x = "Price", y= "Counts")+
   facet_wrap(~product_type)
 
 # Print the histogram
print(histogram_plot)
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- --> \# End of
the Project 2
