Location Effect on Price and Sales
================
Shea Conaway

``` r
# packages
library(tidyverse)
library(maps)
library(scales)
library(ggmap)
```

``` r
# import data
data <- read_csv('../data_clean/cleaned_data.csv', show_col_types = FALSE)
```

``` r
# basic nashville map

# calculate median values and map locations
data_zip <-
  data %>%
  mutate(address_zipcode = replace(address_zipcode,address_zipcode == 37219,37201)) %>%
  group_by(address_zipcode) %>%
  summarize(latitude = mean(latitude)
            ,longitude = mean(longitude)
            ,med_price_sqft = median(price_sqft)
            ,med_price = median(price))

# pull map
davidson_county <- map_data('county') %>% filter(region == 'tennessee') %>% filter(subregion == 'davidson')

# map plot grouped by zip code
ggplot() +
  geom_polygon(data=davidson_county, aes(x=long, y=lat, group=group), color='black', fill='light gray', size = .3 ) +
  geom_point(data = data_zip, aes(x=longitude, y=latitude, color = med_price_sqft, size = med_price_sqft)) +
  coord_quickmap() +
  scale_color_continuous(name = 'Price per \nSquare Foot',type = 'viridis', labels = dollar_format()) +
  scale_size(name = 'Price per \nSquare Foot',range = c(2,10),labels = dollar_format()) +
  labs(title = 'Median Price per Square Foot by Location'
       ,x = ''
       ,y = '') +
  theme_void()
```

![](location_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# register google API (saved in separate private file)
register_google(key = "AIzaSyAsMaYVa64Pg7BQdOhEx7D-plwRjInKqO8")

# load better nashville map
nashville_map <- qmap('Nashville, TN', zoom = 11, maptype = 'toner-lite', source = 'stamen')
```

``` r
# better map plot grouped by zip code
  nashville_map +
  geom_point(data = data_zip, aes(x=longitude, y=latitude, color = med_price_sqft, size = med_price_sqft), alpha = 0.75) +
  scale_color_continuous(name = 'Price per \nSquare Foot',type = 'viridis', labels = dollar_format()) +
  scale_size(name = 'Price per \nSquare Foot',range = c(2,10),labels = dollar_format()) +
  labs(title = 'Median Price per Square Foot by Location\n'
       ,x = ''
       ,y = '') +
  theme_void()
```

![](location_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# downtown distance scatterplot

# factorize zip code
data$address_zipcode <- as.factor(data$address_zipcode)

# downtown price correlation
corr_dt <- round(cor(-data$downtown_dist,data$price_sqft),2)

# downtown distance scatterplot
ggplot(data = data, mapping = aes(x = downtown_dist
                                      ,y = price_sqft)) +
  geom_point(aes(color = address_zipcode),alpha = 0.02, show.legend = FALSE) +
  geom_smooth(formula = y ~ x, method = 'lm', se = FALSE, color = 'gray28', size = 1) +
  labs(title = 'Price per Square Foot by Distance from Downtown'
       ,subtitle = 'Colored by zip code'
       ,x = 'Distance from Downtown'
       ,y = 'Price per Square Foot') +
  annotate('text',label = paste('Correlation Coefficient', corr_dt, sep = '\n'), x = 12.5, y = 400) +
  scale_color_discrete(type = "viridis") +
  theme_minimal()
```

![](location_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# heat map plot
  nashville_map +
  stat_summary_2d(data = data,aes(x=longitude, y=latitude, z = price_sqft),fun = median, bins = 50, alpha = 0.75) +
  scale_fill_continuous(name = 'Price per \nSquare Foot',type = 'viridis', labels = dollar_format()) +
  scale_size(name = 'Price per \nSquare Foot',range = c(2,10),labels = dollar_format()) +
  labs(title = 'Median Price per Square Foot by Location\n'
       ,x = ''
       ,y = '') +
  theme_void()
```

![](location_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
