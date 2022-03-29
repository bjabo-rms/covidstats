

# get country level data --------------------------------------------------

#' Translate country ISO3 code to country name
#' @description The set_country_name function is used to translate ISO3 country
#' codes to country name. Country names are necessary sub-setting CSSE
#' data by country.
#' @param country_name ISO3 country code
#' @return A data frame
#' @examples
#' set_country_name("CHE")
set_country_name <- function(country_name) {
  ifelse(
    country_name == "USA", "US",
    countrycode::countrycode(country_name, origin = 'iso3c', destination = 'country.name')
  )
}


#' Retrieve daily COVID-19 data
#' @description The get_country_counts function is used to retrieve country
#' daily COVID-19 case and death counts.
#' @param country_name ISO3 country code
#' @return A data frame
#' @export
#' @examples
#'get_country_counts("CHE")
#' get_counts(deaths,deaths_url, c("USA"))
get_country_counts <- function(country_name) {
  url_cases <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  url_deaths <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  # metric <- rlang::enexpr(metric)
  # country_name <- deparse(substitute(country_name))
  country_name <- toupper(country_name)
  country_name <- try(set_country_name(country_name))
  country_name <- try(rlang::enexpr(country_name))
  # url <- dplyr::case_when({{metric}} == "cases" ~ url_cases, {{metric}} == "deaths" ~ url_deaths, TRUE ~ NA_character_)
  counts_cases <- readr::read_csv(url(url_cases))
  # counts <-
  counts_cases <- counts_cases |>
    dplyr::select(-c("Province/State", Lat, Long))|>
    tidyr::pivot_longer(-`Country/Region`, names_to = "date",
                        values_to = "total_cases") |>
    janitor::clean_names()|>
    dplyr::filter(country_region  %in% !!country_name)|>
    dplyr::mutate(
      date = lubridate::mdy(date),
      country_region =ifelse(country_region == "US", "United States", country_region)

      )

  counts_deaths <- readr::read_csv(url(url_deaths))
  # counts <-
  counts_deaths <- counts_deaths |>
    dplyr::select(-c("Province/State", Lat, Long))|>
    tidyr::pivot_longer(-`Country/Region`, names_to = "date",
                        values_to = "total_deaths") |>
    janitor::clean_names()|>
    dplyr::filter(country_region  %in% !!country_name)|>
    dplyr::mutate(
      date = lubridate::mdy(date),
      country_region =ifelse(country_region == "US", "United States", country_region)

    )|>
    dplyr::group_by(country_region, date)|>
    dplyr::summarise(total_deaths = sum(total_deaths))

  counts <- dplyr::left_join(counts_cases, counts_deaths, by = c("country_region", "date"))
  counts<-counts |>
    tidyr::replace_na(list(total_cases = 0, total_deaths = 0))|>
    dplyr::mutate(
      new_cases = total_cases - dplyr::lag(
        total_cases,
        n = 1,
        default = 0
      ),
      new_deaths = total_deaths - dplyr::lag(
        total_deaths,
        n = 1,
        default = 0
      ),
    )|>
    dplyr::mutate_if(is.numeric, function(x){ifelse(x <0, 0, x)})
  counts |>
    dplyr::group_by(country_region, date)|>
    dplyr::summarise(new_cases = sum(new_cases),
                     new_deaths = mean(new_deaths))
}


#' Retrieve daily COVID-19 vaccination rates
#' @description The get_country_vax function is used to retrieve country
#' daily COVID-19 vaccination rate.
#' @param country_name ISO3 country name
#' @return A data frame
#' @export
#' @examples
#' get_country_vax("CHE")
get_country_vax <- function(country_name) {
  # country_name <- rlang::enexpr(country_name)
  # country_name <- deparse(substitute(country_name))

  country_name <- toupper(country_name)
  df_url <-"https://covid.ourworldindata.org/data/owid-covid-data.csv"
  df_metrics <- readr::read_csv(df_url)
  df_metrics |>
    # dplyr::filter(iso_code %in% {{country_name}}) |>
    dplyr::filter(iso_code  %in% !!country_name)|>
    dplyr::select(
      iso_code,
      location,
      date,
      people_vaccinated_per_hundred,
      people_fully_vaccinated_per_hundred,
      total_boosters_per_hundred
    )
}



# get location specific data ----------------------------------------------

#' Retrieve country COVID-19 data
#' @description The get_us_counts function is used to retrieve US county
#' daily COVID-19 case and death counts.
#' @return A data frame
#' @export
#' @examples
#' get_us_counts(cases)
get_us_counts <- function() {
  url_cases <- "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv"
  url_deaths <- "https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv"
  # metric <- rlang::enexpr(metric)
  # url <- dplyr::case_when({{metric}} == "cases" ~ url_cases, {{metric}} == "deaths" ~ url_deaths, TRUE ~ NA_character_)
  counts_cases <- readr::read_csv(url(url_cases))
  counts_cases <- counts_cases |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("2"),
      names_to = "date",
      values_to = "total_cases"
      )|>janitor::clean_names()|>
    dplyr::mutate(
      date = lubridate::ymd(date))|>
    dplyr::select(-c("county_fips", "state_fips"))

  counts_deaths <- readr::read_csv(url(url_deaths))
  counts_deaths <- counts_deaths |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("2"),
      names_to = "date",
      values_to = "total_deaths"
    )|>janitor::clean_names()|>
    dplyr::mutate(
      date = lubridate::ymd(date)) |>
    dplyr::select(-c("county_fips", "state_fips"))

  counts <- dplyr::left_join(counts_cases, counts_deaths, by = c("state", "county_name", "date"))
  counts |>
    tidyr::replace_na(list(total_cases = 0, total_deaths = 0))|>
    dplyr::mutate(
      new_cases = total_cases- dplyr::lag(
        total_cases,
        n = 1,
        default = 0
      ),
      new_deaths = total_deaths-dplyr::lag(
        total_deaths,
        n = 1,
        default = 0
      ),
    )|>
    dplyr::mutate_if(is.numeric, function(x){ifelse(x <0, 0, x)})
}



#' Retrieve US Metropolitan statistical area(MSA) table
#' @description The get_us_msa function is used to retrieve the US Census
#' 2020 MSA table.
#' @return US MSA table
#' @export
#' @examples
#' get_us_msa()
get_us_msa <- function() {
  url_msa <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls"
  temp = tempfile(fileext = ".xls")
  download.file(url_msa, destfile=temp, mode='wb')
  readxl::read_excel(temp, sheet = "List 1", skip = 2)|>janitor::clean_names()
}


#' Retrieve US state vaccination rates
#' @description The get_us_vax is used to retrieve CDC US state daily COVID-19
#' vaccination rates.
#' @return A data frame
#' @export
#' @examples
#' get_us_vax()
get_us_vax <- function() {
  jsonlite::fromJSON("https://data.cdc.gov/resource/unsk-b7fc.json") |>
    dplyr::as_tibble()
}


#' Retrieve US state and demographic vaccination rates
#' @description The get_us_vx_demog function retrieves USFACTS state vaccination
#' rate by age.
#' @return A data frame
#' @export
#' @examples
#' get_us_vx_demog()
get_us_vx_demog <- function(){
  url_us_vx_demog <- "https://static.usafacts.org/public/data/covid-19/COVID19_Vaccination_Demographics.csv"
  readr::read_csv(url_us_vx_demog)
}



# get Zurich data ---------------------------------------------------------

#' Retrieve Zurich COVID-19 data.
#' @description The get_zurich_counts function is used to generate Zurich canton
#' daily COVID-19 case and death counts.
#' @return A data frame
#' @export
#' @examples
#' get_zurich_counts()
get_zurich_counts <- function() {
  url <- "https://github.com/openZH/covid_19/raw/master/COVID19_Fallzahlen_CH_total.csv"
  readr::read_csv(url(url)) |>
    janitor::clean_names()|>
    dplyr::filter(abbreviation_canton_and_fl == "ZH")|>
    dplyr::select(date, total_cases = ncumul_conf, total_deaths = ncumul_deceased)|>
    tidyr::replace_na(list(total_cases = 0, total_deaths = 0))|>
    dplyr::mutate(
      new_cases = total_cases- dplyr::lag(
        total_cases,
        n = 1,
        default = 0
      ),
      new_deaths = total_deaths-dplyr::lag(
        total_deaths,
        n = 1,
        default = 0
      ),
    )
}


# get london data ---------------------------------------------------------

#' Retrieve London COVID-19 data
#' @description The get_london_counts function is used to generate London daily
#' COVID-19 case and death counts.
#' @return A data frame
#' @export
#' @examples
#' get_london_counts()
get_london_counts <- function() {
  url_cases <- "https://data.london.gov.uk/download/coronavirus--covid-19--cases/151e497c-a16e-414e-9e03-9e428f555ae9/phe_cases_london_boroughs.csv"
  url_deaths <- "https://data.london.gov.uk/download/coronavirus--covid-19--cases/ff60cf44-852e-425e-960c-869920dcdd0d/phe_deaths_london_boroughs.csv"
  # metric <- rlang::enexpr(metric)
  # url <- dplyr::case_when({{metric}} == "cases" ~ url_cases, {{metric}} == "deaths" ~ url_deaths, TRUE ~ NA_character_)
  counts_cases <- readr::read_csv(url(url_cases))
  counts_deaths <- readr::read_csv(url(url_deaths))
  counts <- dplyr::left_join(counts_cases, counts_deaths, by = c("area_name", "area_code", "date"))
  counts |>
    tidyr::replace_na(list(new_cases = 0, total_cases = 0, new_deaths = 0, total_deaths = 0))|>
    dplyr::group_by(date)|>
    dplyr::summarise(
      new_cases = sum(new_cases),
      total_cases = sum(total_cases),
      new_deaths = sum(new_deaths),
      total_deaths = sum(total_deaths)
      )|>
    dplyr::mutate(location = "London Metropolitan Area")

  }

#' Retrieve London vaccination rates
#' @description The get_london_vx_demog is used to retrieve London daily
#' COVID-19 vaccination rate by age.
#' @return A data frame
#' @export
#' @examples
#' get_london_vx_demog()
get_london_vx_demog <- function() {
  url_london_vx_demog <- "https://data.london.gov.uk/download/coronavirus--covid-19--cases/ae4d5fc9-5448-49a6-810f-910f7cbc9fd2/phe_vaccines_age_london_boroughs.csv"
  df_london_vx_demog <- readr::read_csv(url_london_vx_demog)
  df_london_vx_demog

}
