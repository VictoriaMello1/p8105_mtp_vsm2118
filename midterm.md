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
      county_name %in% c("Kings") ~ "Brooklyn",
      county_name %in% c("Queens") ~ "Queens",
      county_name == "Bronx" ~ "Bronx",
      county_name == "New York" ~ "Manhattan",
      county_name == "Richmond" ~ "Staten Island"
    )
  )
view(zip_data)
```

Merging the zipcode and coa data sets

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
  select(zipcode, neighborhood, borough, year, month, net_change, city)

view(final_data)
```

Exploring combined final dataset:

``` r
summary_description <- final_data %>%
  summarise(
    Total_Observations = n(),
    Unique_ZIP_Codes = n_distinct(zipcode),
    Unique_Neighborhoods = n_distinct(neighborhood)
  )

print(summary_description)
```

    ## # A tibble: 1 × 3
    ##   Total_Observations Unique_ZIP_Codes Unique_Neighborhoods
    ##                <int>            <int>                <int>
    ## 1              12085              237                   43

Comparing city and borough:

``` r
# Filter the data for Manhattan and Queens
manhattan_data <- final_data %>%
  filter(borough == "Manhattan")

queens_data <- final_data %>%
  filter(borough == "Queens")

# Create tables for the most common cities in Manhattan and Queens
manhattan_table <- manhattan_data %>%
  count(city, sort = TRUE)

queens_table <- queens_data %>%
  count(city, sort = TRUE)

knitr::kable(manhattan_table, caption = "Manhattan most common city value")
```

| city             |    n |
|:-----------------|-----:|
| NEW YORK         | 3477 |
| BRONX            |   60 |
| BROOKLYN         |   59 |
| CANAL STREET     |    4 |
| ROOSEVELT ISL    |    4 |
| ROOSEVELT ISLAND |    4 |
| BOWLING GREEN    |    1 |
| BROOKLYN HEIGHTS |    1 |
| NYC              |    1 |
| SECHEDATY        |    1 |
| WALL STREET      |    1 |

Manhattan most common city value

``` r
knitr::kable(queens_table, caption = "Queens most common city value")
```

| city                |   n |
|:--------------------|----:|
| JAMAICA             | 372 |
| FLUSHING            | 309 |
| ASTORIA             | 230 |
| QUEENS VILLAGE      | 165 |
| BAYSIDE             | 135 |
| LONG ISLAND CITY    | 120 |
| EAST ELMHURST       | 117 |
| OZONE PARK          | 116 |
| FRESH MEADOWS       | 107 |
| FAR ROCKAWAY        | 102 |
| LITTLE NECK         |  79 |
| RIDGEWOOD           |  70 |
| FLORAL PARK         |  68 |
| ELMHURST            |  63 |
| KEW GARDENS         |  62 |
| BELLEROSE           |  60 |
| BROOKLYN            |  60 |
| COLLEGE POINT       |  60 |
| CORONA              |  60 |
| HOWARD BEACH        |  60 |
| REGO PARK           |  60 |
| FOREST HILLS        |  59 |
| HOLLIS              |  59 |
| ROSEDALE            |  59 |
| WOODHAVEN           |  59 |
| WOODSIDE            |  59 |
| CAMBRIA HEIGHTS     |  58 |
| MASPETH             |  58 |
| MIDDLE VILLAGE      |  58 |
| SAINT ALBANS        |  57 |
| ARVERNE             |  56 |
| JACKSON HEIGHTS     |  56 |
| WHITESTONE          |  55 |
| RICHMOND HILL       |  54 |
| GLEN OAKS           |  52 |
| SUNNYSIDE           |  51 |
| ROCKAWAY BEACH      |  49 |
| SOUTH OZONE PARK    |  49 |
| BREEZY POINT        |  47 |
| SOUTH RICHMOND HILL |  47 |
| ROCKAWAY PARK       |  40 |
| DOUGLASTON          |  39 |
| OAKLAND GARDENS     |  38 |
| SPRINGFIELD GARDENS |  30 |
| LAURELTON           |  28 |
| LONG IS CITY        |  19 |
| BRIARWOOD           |  16 |
| ROCKAWAY POINT      |  13 |
| BELLE HARBOR        |  11 |
| GLENDALE            |  11 |
| S RICHMOND HL       |   9 |
| QUEENS VLG          |   8 |
| S OZONE PARK        |   8 |
| BAYSIDE HILLS       |   6 |
| BROAD CHANNEL       |   5 |
| BEECHHURST          |   4 |
| KEW GARDENS HILLS   |   4 |
| NEPONSIT            |   4 |
| AUBURNDALE          |   2 |
| BELLEROSE MANOR     |   2 |
| SPRNGFLD GDNS       |   2 |
| CALVERTON           |   1 |
| CAMBRIA HTS         |   1 |
| JACKSON HTS         |   1 |
| MIDDLE VLG          |   1 |
| NEW YORK CITY       |   1 |
| ST ALBANS           |   1 |

Queens most common city value
