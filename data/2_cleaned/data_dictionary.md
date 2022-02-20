# Data dictionary

## General description
This is data collected from Zillow.com for the prior three years of house sales in Nashville. The data was collected on 11 November 2021. Each observation is a property or house and its sale information.

## Variables

**zpid** (numeric)
- Unique identifier for each house listing on Zillow

**price** (numeric)
- The most recent sales price

**price_sqft** (numeric)
- The price per square foot (price / living_area)

**bedrooms** (numeric)
- The number of bedrooms

**bathrooms** (numeric)
- The number of bathrooms. Half bathrooms are a sink and toilet without a shower/bath

**living_area** (numeric)
- The livable area in square feet

**home_type** (categorical)
- The home type of the property (e.g. single family home, townhouse, condo)

**date_sold** (date)
- The most recent sale date

**date_listed** (date)
- The most recent date the property was listed for sale prior to selling

**days_on_market** (numeric)
- The number of days the property was listed for sale before selling (i.e. the number of days between date_listed and date_sold)

**date_sold_previous** (date)
- The most recent sale date prior to date_sold (i.e. the second most recent sale)

**year_built** (year date)
- The year the house was built

**age** (numeric)
- The age in years of the house at the time of sale (i.e. the years between years_built and date_sold)

**description** (text)
- A text description that appears on the Zillow.com listing

**photo_count** (numeric)
- A count of the number of photos that appear on the Zillow.com listing

**lot_size** (numeric)
- The land area of the property the house is located on. Note, the land area for condos is equal to the living area of the condo

**downtown_dist** (numeric)
- Distance from the property to downtown in miles as the crow flies. Location of Tennessee State Capitol building was subjectively chosen as the central point of downtown

**longitude** (numeric)
- The longitudinal location of the property

**latitude** (numeric)
- The latitudinal location of the property

**neighborhood** (text)
- A text name of the neighborhood or neighborhoods as determined by the zip code of the house

**address_state** (categorical)
- The state where the house is located (i.e. Tennessee)

**address_city** (categorical)
- The city where the house is located. Includes Nashville and other incorporated municipalities in Davidson County

**address_zipcode** (categorical)
- The zipcode where the house is located

**address_street** (text)
- The street address where the house is located

**parcel_id** (text)
- The parcel identifier of the property as registered with the Davidson County property assessor

**url** (text)
- The web address of the Zillow listing

**favorite_count** (numeric)
- The number of Zillow users who "favorited" the listing for later review

**page_view_count** (numeric)
- The number of listing views on Zillow.com

**home_status** (categorical)
- The current status of the property as determined by Zillow (e.g. Sold, For Sale)
