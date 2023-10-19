Data Science Midterm
================

The raw data used in this analysis consists of Change of Address (COA)
data from the US Postal Service for New York City between 2018 and 2022.
The COA data includes information on the total number of permanent
address changes going into and out of each ZIP code in New York City.
The initial section of the report focuses on importing, cleaning, and
ensuring data quality. This process results in a clean and structured
dataset, ready for analysis. The dataset’s characteristics, including
observations, unique ZIP codes, and neighborhoods, are described. Data
quality issues are also addressed. The subsequent section visualizes the
data with an analysis of address change trends, uncovering extreme
values, and plots that highlight neighborhood-level insights over the
five-year period. The report concludes with an acknowledgment of any
limitations of the dataset.

## Section 1 – Data import, cleaning, and quality control

``` r
zip_data = read_csv("zip_codes.csv")
```

    ## Rows: 324 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): County Name, County Code, File Date, Neighborhood
    ## dbl (3): State FIPS, County FIPS, ZipCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sheet_names <- c("2018", "2019", "2020", "2021", "2022")
coa_data <- map_dfr(sheet_names, ~ readxl::read_xlsx("USPS.xlsx", sheet = .x))

view(coa_data)

coa_data = coa_data %>% 
  janitor::clean_names() %>% 
  mutate(
    year = as.integer(str_sub(month, 1, 4)),
    net_change = `total_perm_in` - `total_perm_out`
  )
  
  view(coa_data)
```

Cleaning zipcode data and creating a borough variable

``` r
# Create a "borough" variable in the ZIP code data
zip_data <- zip_data %>%
  janitor::clean_names() %>% 
  rename(zipcode = zip_code) %>% 
  mutate(
    borough = case_when(
      county_name %in% c("Kings", "Queens") ~ "Brooklyn",
      county_name == "Bronx" ~ "Bronx",
      county_name == "New York" ~ "Manhattan",
      county_name == "Richmond" ~ "Staten Island"
    )
  )
view(zip_data)
```

Merging the zipcode and coa datasets

``` r
# Merge the COA and ZIP code data based on ZIP code
merged_data <- coa_data %>%
  left_join(zip_data, by = "zipcode")
```

    ## Warning in left_join(., zip_data, by = "zipcode"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 88 of `x` matches multiple rows in `y`.
    ## ℹ Row 79 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
# Select only the necessary variables for later visualizations
final_data <- merged_data %>%
  select(zipcode, neighborhood, borough, year, month, net_change)

# View the final dataset
view(final_data)
```
