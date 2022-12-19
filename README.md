# Nashville Real Estate

House prices are a common project topic for data science learners. I've attempted to make this more interesting by collecting my own real-world data for my current city. This was an EDA-only project for an academic course, which I've since expanded with some predictive modeling.
- Local data was collected from Zillow using a custom scraper
- Extensive exploratory data analysis was performed
- Linear, tree-based, and novel transformer models have been applied and compared for price prediction

![zillow_search_area](readme_images/zillow_search_area.png)
![log_price](readme_images/log_price.png)
![xgboost](readme_images/xgboost.png)

## Data Collection and Preparation
-  [Web scraping](scrape.md)
-  [Cleaning](data/2_cleaned/clean_data.Rmd)

## Exploratory Data Analysis
- Understand data, visualize trends, and identify potential features
- [Brief EDA Presentation](brief_presentation.pdf)
- [Full EDA Report](final_report.pdf)

## Modeling
- How accurately can prices be predicted with these features?
- [Modeling Results](modeling/modeling.md)
