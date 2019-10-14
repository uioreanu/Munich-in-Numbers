Munich in numbers
================
CU
10/6/2019

  - [Brief exploratory analysis](#brief-exploratory-analysis)
      - [Data](#data)
  - [General Trends (monthly)](#general-trends-monthly)
      - [Cinema visitors](#cinema-visitors)
      - [weather trends](#weather-trends)
      - [Tourists](#tourists)
      - [Population](#population)
      - [Religion](#religion)
      - [Unemployment (gender &
        nationality)](#unemployment-gender-nationality)
  - [Counting NAs (missing data)](#counting-nas-missing-data)
  - [Explore correlations](#explore-correlations)
      - [simple use of cor()](#simple-use-of-cor)
      - [the correlate package](#the-correlate-package)
  - [Next steps](#next-steps)
      - [Highly Correlated variables](#highly-correlated-variables)
      - [linear models](#linear-models)

# Brief exploratory analysis

This is a brief EDA of Munich stats.

## Data

### source

Thanks to **Statistische Amt München** for making the data available on
a monthly basis. They make way more data available, these are solely
main KPIs. Source of data:
<http://www.mstatistik-muenchen.de/monatszahlenmonitoring/export/export.php>.
Official Data exploration portal:
<http://www.mstatistik-muenchen.de/monatszahlenmonitoring/atlas.html?indicator=i158&date=Jan&select=20,19&select2=JAHR&indicator2=i0>.
Data here is extracted from Excel, processed into data.frames and
correlation charts

### raw

The raw data is a list of data-frames

``` r
as.data.frame(lapply(dat, dim)) %>% 
  rownames_to_column() %>%
  kable()
```

<table>

<thead>

<tr>

<th style="text-align:left;">

rowname

</th>

<th style="text-align:right;">

INHALTSÜBERSICHT

</th>

<th style="text-align:right;">

ARBEITSMARKT

</th>

<th style="text-align:right;">

BAUEN

</th>

<th style="text-align:right;">

BEVÖLKERUNG

</th>

<th style="text-align:right;">

EINBÜRGERUNGEN

</th>

<th style="text-align:right;">

FEUERWEHR.MÜNCHEN

</th>

<th style="text-align:right;">

FLUGVERKEHR

</th>

<th style="text-align:right;">

FREIZEIT

</th>

<th style="text-align:right;">

KFZ.Bestand

</th>

<th style="text-align:right;">

KFZ.Neuzulassungen

</th>

<th style="text-align:right;">

KINOS

</th>

<th style="text-align:right;">

MUSEEN

</th>

<th style="text-align:right;">

ORCHESTER

</th>

<th style="text-align:right;">

SOZIALE.LEISTUNGEN

</th>

<th style="text-align:right;">

THEATER

</th>

<th style="text-align:right;">

TOURISMUS

</th>

<th style="text-align:right;">

VERKEHRSUNFÄLLE

</th>

<th style="text-align:right;">

WIRTSCHAFT

</th>

<th style="text-align:right;">

WITTERUNG

</th>

<th style="text-align:right;">

IMPRESSUM

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

1

</td>

<td style="text-align:right;">

288

</td>

<td style="text-align:right;">

2119

</td>

<td style="text-align:right;">

720

</td>

<td style="text-align:right;">

19109

</td>

<td style="text-align:right;">

660

</td>

<td style="text-align:right;">

1914

</td>

<td style="text-align:right;">

1880

</td>

<td style="text-align:right;">

1639

</td>

<td style="text-align:right;">

4984

</td>

<td style="text-align:right;">

2592

</td>

<td style="text-align:right;">

228

</td>

<td style="text-align:right;">

2068

</td>

<td style="text-align:right;">

1416

</td>

<td style="text-align:right;">

2055

</td>

<td style="text-align:right;">

4056

</td>

<td style="text-align:right;">

954

</td>

<td style="text-align:right;">

1596

</td>

<td style="text-align:right;">

4462

</td>

<td style="text-align:right;">

1638

</td>

<td style="text-align:right;">

13

</td>

</tr>

<tr>

<td style="text-align:left;">

2

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

1

</td>

</tr>

</tbody>

</table>

### aggregated

We’ll now compress the **original disparated data** in one compact
data.frame For that, we create dynamic column names and aggregate the
monthly values The final result will be a large matrix (\~300 columns)
where each column represents a variable and the rows are the monthly
values (20 yrs x 12 months)

``` r
##########################
# looping through all the Excel sheets
dsFinal <- data.frame()
for (i in 1:length(names(dat))) {
  SECTIONNAME <- names(dat)[i]
  dtmp <- (dat[SECTIONNAME])[[1]]
  # if the current section has values in it
  if ('WERT' %in% names(dtmp)) {
    # creating dynamic column names and assign monthly values as cols
    dtmp_mo <- dtmp %>%
      select(MONATSZAHL, AUSPRAEGUNG, MONAT, WERT) %>%
      mutate(colname=paste(SECTIONNAME, MONATSZAHL, AUSPRAEGUNG, sep = " - ")) %>%
      select(MONAT, colname, WERT) %>%
      spread(MONAT, WERT) 
    rownames(dtmp_mo) <- dtmp_mo$colname
    dtmp_mo$colname <- NULL
    # transpose monthly values to rows
    dtmp_mo <- t(dtmp_mo)
    # back from tibble to dataframe
    dtmp_mo <-as.data.frame(dtmp_mo)
    dtmp_mo$month <- rownames(dtmp_mo)
    
    if (nrow(dsFinal)==0){
      # if the final construct doesn't exist, simply create it
      dsFinal <- dtmp_mo
    } else {
      # if we have values in it already, merge them (full join)  using the month column
      dsFinal <- dtmp_mo %>%
        full_join(dsFinal, by = "month")
    }
  }
}
# cleanup
rm(dtmp, dtmp_mo, i, SECTIONNAME)

rownames(dsFinal) <- dsFinal$month
dsFinal$month <- NULL
dim(dsFinal)
```

    ## [1] 236 288

``` r
# dsFinal[1:10,1:2] %>% kable()
# colMeans(dsFinal, na.rm = T) %>% kable()
# str(dsFinal)
##########################
```

# General Trends (monthly)

## Cinema visitors

``` r
##########################
# exploratory charts
dat$KINOS %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
    geom_line() + 
    facet_wrap(~AUSPRAEGUNG) +
    theme_classic()+ 
    geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("Cinema Visitors")
```

![](Munich_numbers_files/figure-gfm/charts_kinos-1.jpeg)<!-- -->

## weather trends

### Sun & temperature

``` r
dat$WITTERUNG %>%
  filter(MONATSZAHL %in% c("Sonnenschein", "Lufttemperatur")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free") +
  theme_classic()+ 
  geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("monthly weather trends")
```

![](Munich_numbers_files/figure-gfm/charts_weather-1.jpeg)<!-- -->

### Rain

``` r
dat$WITTERUNG %>%
  filter(MONATSZAHL %in% c("Niederschlag")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free", nrow=2) +
  theme_classic()+ 
  geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("monthly weather trends")
```

![](Munich_numbers_files/figure-gfm/charts_weather_rain-1.jpeg)<!-- -->

## Tourists

``` r
dat$TOURISMUS %>%
  filter(MONATSZAHL %in% c("Gäste")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free", nrow=3) +
  theme_classic()+ 
  geom_smooth() + scale_y_continuous(position = "right", labels = scales::comma) + ggtitle("monthly trend - Turist guests")
```

![](Munich_numbers_files/figure-gfm/charts_tourism-1.jpeg)<!-- -->

## Population

### German population 2010-today

``` r
dsFinal %>%
  rownames_to_column() %>%
  select(rowname, `german population` =  `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Deutsche insgesamt`) %>%
  mutate(Month = as.Date(paste0(rowname,'01'), format = '%Y%m%d')) %>%
  filter(Month>='2010-01-01') %>%
  ggplot(aes(x = Month))  + 
    geom_line(aes(y = `german population`)) + 
    theme_minimal() + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
    scale_y_continuous(position = "right", labels = scales::comma) 
```

    ## Warning: Removed 1 rows containing missing values (geom_path).

![](Munich_numbers_files/figure-gfm/charts_population_germans_only-1.jpeg)<!-- -->

### Foreigners 2010-today

``` r
dsFinal %>%
  rownames_to_column() %>%
  select(rowname, `foreigners` = `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Ausländer/innen insgesamt`) %>%
  mutate(Month = as.Date(paste0(rowname,'01'), format = '%Y%m%d')) %>%
  filter(Month>='2010-01-01') %>%
  ggplot(aes(x = Month))  + 
    geom_line(aes(y = foreigners), linetype="twodash") + 
    theme_minimal() + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
    scale_y_continuous(position = "right", labels = scales::comma) 
```

    ## Warning: Removed 1 rows containing missing values (geom_path).

![](Munich_numbers_files/figure-gfm/charts_population_foreigners_only-1.jpeg)<!-- -->

### Inhabitants (all & gender)

``` r
dat$BEVÖLKERUNG %>%
  filter(MONATSZAHL %in% c("Geschlecht und Staatsangehörigkeit")) %>%
  filter(str_detect(AUSPRAEGUNG, 'Einwohner')) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  arrange(-WERT) %>% # sort
  mutate_at(vars(AUSPRAEGUNG), funs(factor(., levels=unique(.)))) %>% # convert to factor
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG) +
  theme_classic()+ 
  scale_y_continuous(labels = scales::comma) + ggtitle("Munich Inhabitants (male, female, all)")
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## please use list() instead
    ## 
    ##   # Before:
    ##   funs(name = f(.))
    ## 
    ##   # After: 
    ##   list(name = ~ f(.))
    ## This warning is displayed once per session.

![](Munich_numbers_files/figure-gfm/charts_population_all-1.jpeg)<!-- -->

### German inhabitants (all & gender)

``` r
dat$BEVÖLKERUNG %>%
  filter(MONATSZAHL %in% c("Geschlecht und Staatsangehörigkeit")) %>%
  filter(str_detect(AUSPRAEGUNG, 'Deutsche')) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  arrange(-WERT) %>% # sort
  mutate_at(vars(AUSPRAEGUNG), funs(factor(., levels=unique(.)))) %>% # convert to factor
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG) +
  theme_classic()+ 
  scale_y_continuous(labels = scales::comma) + ggtitle("Germans in Munich (male, female, all)")
```

![](Munich_numbers_files/figure-gfm/charts_population_german-1.jpeg)<!-- -->

### Foreign inhabitants (all & gender)

``` r
dat$BEVÖLKERUNG %>%
  filter(MONATSZAHL %in% c("Geschlecht und Staatsangehörigkeit")) %>%
  filter(str_detect(AUSPRAEGUNG, 'Ausländer')) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  arrange(-WERT) %>% # sort
  mutate_at(vars(AUSPRAEGUNG), funs(factor(., levels=unique(.)))) %>% # convert to factor
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG) +
  theme_classic()+ 
  scale_y_continuous(labels = scales::comma) + ggtitle("Foreigners in Munich (male, female, all)")
```

![](Munich_numbers_files/figure-gfm/charts_population_foreign-1.jpeg)<!-- -->

### Female share

#### among german nationals

``` r
dsFinal %>% 
  rownames_to_column() %>%
  mutate(
    year = substring(rowname, 0, 4), 
    'german_female' = `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Deutsche weiblich`,
    'foreign_female' = `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Ausländer weiblich`,
    'all_german' = `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Deutsche insgesamt`,
    'all_female' = `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner weiblich`
   ) %>%
  select(year, german_female, foreign_female, all_german, all_female) %>%
  group_by(year) %>%
  summarize(
            german_female = round(mean(german_female, na.rm = TRUE),0),
            foreign_female = round(mean(foreign_female, na.rm = TRUE),0),
            all_german=round(mean(all_german, na.rm = TRUE),0),
            all_female=round(mean(all_female, na.rm = TRUE),0)
            ) %>%
  mutate(
    female_share_among_germans = round(german_female/all_german, 2),
    german_female_share = round(german_female/all_female, 2)
    ) %>%
  kable(format.args = list(decimal.mark = ".", big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" ", "in thousands" = 4, "percentages" = 2))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="border-bottom:hidden" colspan="1">

</th>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

in
thousands

</div>

</th>

<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

percentages

</div>

</th>

</tr>

<tr>

<th style="text-align:left;">

year

</th>

<th style="text-align:right;">

german\_female

</th>

<th style="text-align:right;">

foreign\_female

</th>

<th style="text-align:right;">

all\_german

</th>

<th style="text-align:right;">

all\_female

</th>

<th style="text-align:right;">

female\_share\_among\_germans

</th>

<th style="text-align:right;">

german\_female\_share

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

2000

</td>

<td style="text-align:right;">

514,168

</td>

<td style="text-align:right;">

126,904

</td>

<td style="text-align:right;">

960,111

</td>

<td style="text-align:right;">

641,072

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.80

</td>

</tr>

<tr>

<td style="text-align:left;">

2001

</td>

<td style="text-align:right;">

517,712

</td>

<td style="text-align:right;">

130,238

</td>

<td style="text-align:right;">

970,183

</td>

<td style="text-align:right;">

647,950

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.80

</td>

</tr>

<tr>

<td style="text-align:left;">

2002

</td>

<td style="text-align:right;">

518,533

</td>

<td style="text-align:right;">

132,470

</td>

<td style="text-align:right;">

974,597

</td>

<td style="text-align:right;">

651,003

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.80

</td>

</tr>

<tr>

<td style="text-align:left;">

2003

</td>

<td style="text-align:right;">

517,904

</td>

<td style="text-align:right;">

135,331

</td>

<td style="text-align:right;">

975,670

</td>

<td style="text-align:right;">

653,236

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

2004

</td>

<td style="text-align:right;">

517,884

</td>

<td style="text-align:right;">

136,630

</td>

<td style="text-align:right;">

977,249

</td>

<td style="text-align:right;">

654,515

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

2005

</td>

<td style="text-align:right;">

520,386

</td>

<td style="text-align:right;">

140,568

</td>

<td style="text-align:right;">

984,594

</td>

<td style="text-align:right;">

660,954

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

2006

</td>

<td style="text-align:right;">

531,108

</td>

<td style="text-align:right;">

144,146

</td>

<td style="text-align:right;">

1,007,509

</td>

<td style="text-align:right;">

675,254

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

2007

</td>

<td style="text-align:right;">

541,959

</td>

<td style="text-align:right;">

147,222

</td>

<td style="text-align:right;">

1,031,287

</td>

<td style="text-align:right;">

689,182

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

2008

</td>

<td style="text-align:right;">

548,027

</td>

<td style="text-align:right;">

150,211

</td>

<td style="text-align:right;">

1,045,105

</td>

<td style="text-align:right;">

698,238

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.78

</td>

</tr>

<tr>

<td style="text-align:left;">

2009

</td>

<td style="text-align:right;">

550,959

</td>

<td style="text-align:right;">

148,599

</td>

<td style="text-align:right;">

1,052,103

</td>

<td style="text-align:right;">

699,558

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

2010

</td>

<td style="text-align:right;">

553,954

</td>

<td style="text-align:right;">

151,328

</td>

<td style="text-align:right;">

1,059,451

</td>

<td style="text-align:right;">

705,283

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

2011

</td>

<td style="text-align:right;">

558,116

</td>

<td style="text-align:right;">

157,464

</td>

<td style="text-align:right;">

1,070,394

</td>

<td style="text-align:right;">

715,580

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.78

</td>

</tr>

<tr>

<td style="text-align:left;">

2012

</td>

<td style="text-align:right;">

562,388

</td>

<td style="text-align:right;">

165,760

</td>

<td style="text-align:right;">

1,081,036

</td>

<td style="text-align:right;">

728,148

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.77

</td>

</tr>

<tr>

<td style="text-align:left;">

2013

</td>

<td style="text-align:right;">

565,732

</td>

<td style="text-align:right;">

174,331

</td>

<td style="text-align:right;">

1,088,920

</td>

<td style="text-align:right;">

740,063

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.76

</td>

</tr>

<tr>

<td style="text-align:left;">

2014

</td>

<td style="text-align:right;">

567,894

</td>

<td style="text-align:right;">

183,660

</td>

<td style="text-align:right;">

1,094,788

</td>

<td style="text-align:right;">

751,554

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.76

</td>

</tr>

<tr>

<td style="text-align:left;">

2015

</td>

<td style="text-align:right;">

569,554

</td>

<td style="text-align:right;">

193,766

</td>

<td style="text-align:right;">

1,099,356

</td>

<td style="text-align:right;">

763,320

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.75

</td>

</tr>

<tr>

<td style="text-align:left;">

2016

</td>

<td style="text-align:right;">

571,423

</td>

<td style="text-align:right;">

203,557

</td>

<td style="text-align:right;">

1,103,956

</td>

<td style="text-align:right;">

774,980

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.74

</td>

</tr>

<tr>

<td style="text-align:left;">

2017

</td>

<td style="text-align:right;">

572,479

</td>

<td style="text-align:right;">

206,579

</td>

<td style="text-align:right;">

1,106,840

</td>

<td style="text-align:right;">

779,058

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.73

</td>

</tr>

<tr>

<td style="text-align:left;">

2018

</td>

<td style="text-align:right;">

571,335

</td>

<td style="text-align:right;">

205,135

</td>

<td style="text-align:right;">

1,105,693

</td>

<td style="text-align:right;">

776,471

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.74

</td>

</tr>

<tr>

<td style="text-align:left;">

2019

</td>

<td style="text-align:right;">

573,240

</td>

<td style="text-align:right;">

208,963

</td>

<td style="text-align:right;">

1,110,235

</td>

<td style="text-align:right;">

782,203

</td>

<td style="text-align:right;">

0.52

</td>

<td style="text-align:right;">

0.73

</td>

</tr>

</tbody>

</table>

#### among foreigners

``` r
plot(
    rownames(dsFinal) 
    , dsFinal$`BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Ausländer weiblich`  / dsFinal$`BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Ausländer/innen insgesamt`
    , xlab = "month"
    , ylab = "% Female"
     , main="% Female among Foreigners", 
    type="o")
```

![](Munich_numbers_files/figure-gfm/female_share_foreigners-1.jpeg)<!-- -->

### Family status

``` r
dat$BEVÖLKERUNG %>%
  filter(MONATSZAHL %in% c("Familienstand")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  arrange(-WERT) %>% # sort
  mutate_at(vars(AUSPRAEGUNG), funs(factor(., levels=unique(.)))) %>% # convert to factor
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free") +
  theme_classic()+ 
  scale_y_continuous(labels = scales::comma) + ggtitle("monthly trend - population by family status")
```

![](Munich_numbers_files/figure-gfm/charts_population-1.jpeg)<!-- -->

``` r
#ggsave("monthly trend - population by family status.png", dpi=400, dev='png', height=4, width=5, units="in", scale = 2)
```

### Age groups

``` r
dat$BEVÖLKERUNG %>%
    filter(MONATSZAHL %in% c("Altersgruppen")) %>%
    mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
    arrange(-WERT) %>% # sort
    mutate_at(vars(AUSPRAEGUNG), funs(factor(., levels=unique(.)))) %>% # convert to factor
    ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
    geom_line() + 
    facet_wrap(~AUSPRAEGUNG, scales = "free") +
    theme_classic()+ 
    geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("Population by age group")
```

![](Munich_numbers_files/figure-gfm/charts_population_by_age-1.jpeg)<!-- -->

## Religion

``` r
dat$BEVÖLKERUNG %>%
    filter(MONATSZAHL %in% c("Religionszugehörigkeit")) %>%
    mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
    ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
    geom_line() + 
    facet_wrap(~AUSPRAEGUNG, scales = "free") +
    theme_classic()+ 
    geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("Religion in Munich")
```

![](Munich_numbers_files/figure-gfm/charts_population_religion-1.jpeg)<!-- -->

## Unemployment (gender & nationality)

### absolute numbers

``` r
dat$ARBEITSMARKT %>%
  filter(MONATSZAHL %in% c("Arbeitslose")) %>%
  filter(AUSPRAEGUNG %in% c("insgesamt","Frauen","Männer","Deutsche","Deutsche", "Ausländer/innen", "Langzeitarbeitslose")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  arrange(-WERT) %>% # sort
  mutate_at(vars(AUSPRAEGUNG), funs(factor(., levels=unique(.)))) %>% # convert to factor
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free") +
  theme_classic()+ 
  scale_y_continuous(labels = scales::comma) + ggtitle("Unemployed in Munich (#)")
```

![](Munich_numbers_files/figure-gfm/charts_unemployed_absolute-1.jpeg)<!-- -->

### as percentages

``` r
dat$ARBEITSMARKT %>%
  filter(MONATSZAHL %in% c("Arbeitslosenquote")) %>%
  filter(AUSPRAEGUNG %in% c("abh. ziv. Erwerbspersonen", "alle ziv. Erwerbspersonen")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  arrange(-WERT) %>% # sort
  mutate_at(vars(AUSPRAEGUNG), funs(factor(., levels=unique(.)))) %>% # convert to factor
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free") +
  theme_classic()+ 
  scale_y_continuous(labels = scales::comma) + ggtitle("Unemployment rate in Munich")
```

![](Munich_numbers_files/figure-gfm/charts_unemployed_percent-1.jpeg)<!-- -->

# Counting NAs (missing data)

``` r
##########################
# count NAs
# absolute number of missing values
nasCount <- dsFinal %>%
  summarise_all(funs(sum(is.na(.))))
# relative count
nasCount <- nasCount/nrow(dsFinal)
# transpose percent of missing values
nasCount <- as.data.frame(t(nasCount))
nasCount$field <- rownames(nasCount)
names(nasCount) <- c('NAs_proc', 'field_name')
# done
head(nasCount) %>% kable()
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

NAs\_proc

</th>

<th style="text-align:left;">

field\_name

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit

</td>

<td style="text-align:right;">

0.0084746

</td>

<td style="text-align:left;">

WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit

</td>

</tr>

<tr>

<td style="text-align:left;">

WITTERUNG - Lufttemperatur - Höchste Lufttemperatur

</td>

<td style="text-align:right;">

0.0084746

</td>

<td style="text-align:left;">

WITTERUNG - Lufttemperatur - Höchste Lufttemperatur

</td>

</tr>

<tr>

<td style="text-align:left;">

WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur

</td>

<td style="text-align:right;">

0.0084746

</td>

<td style="text-align:left;">

WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur

</td>

</tr>

<tr>

<td style="text-align:left;">

WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur

</td>

<td style="text-align:right;">

0.0084746

</td>

<td style="text-align:left;">

WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur

</td>

</tr>

<tr>

<td style="text-align:left;">

WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt

</td>

<td style="text-align:right;">

0.0084746

</td>

<td style="text-align:left;">

WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt

</td>

</tr>

<tr>

<td style="text-align:left;">

WITTERUNG - Niederschlag - Tage mit Niederschlägen

</td>

<td style="text-align:right;">

0.0084746

</td>

<td style="text-align:left;">

WITTERUNG - Niederschlag - Tage mit
Niederschlägen

</td>

</tr>

</tbody>

</table>

``` r
nasFinal <- data.frame("field_name" =  nasCount$field_name, "NAs_proc" = nasCount$NAs_proc)
plot(nasFinal$NAs_proc, main="Percent of missing values for all variables")
abline(h=0.1, col="blue")
```

![](Munich_numbers_files/figure-gfm/prepare_count_of_NAs-1.jpeg)<!-- -->

``` r
rm(nasCount)
##########################
```

we will **cut all variables filled below 10%** (so all above the blue
line).

# Explore correlations

## simple use of cor()

``` r
##########################
# explore correlation
dsCor <- cor(dsFinal, use = "pairwise.complete.obs")

dsCor[1:5, 1:5]
```

    ##                                                                   WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit
    ## WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit                                                        1.00000000
    ## WITTERUNG - Lufttemperatur - Höchste Lufttemperatur                                                                     -0.64793440
    ## WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur                                                                    -0.62228448
    ## WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur                                                                     -0.51313290
    ## WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt                                                                 -0.08243451
    ##                                                                   WITTERUNG - Lufttemperatur - Höchste Lufttemperatur
    ## WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit                                          -0.6479344
    ## WITTERUNG - Lufttemperatur - Höchste Lufttemperatur                                                         1.0000000
    ## WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur                                                        0.9586386
    ## WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur                                                         0.9037555
    ## WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt                                                     0.4353042
    ##                                                                   WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur
    ## WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit                                           -0.6222845
    ## WITTERUNG - Lufttemperatur - Höchste Lufttemperatur                                                          0.9586386
    ## WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur                                                         1.0000000
    ## WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur                                                          0.9581775
    ## WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt                                                      0.4822358
    ##                                                                   WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur
    ## WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit                                          -0.5131329
    ## WITTERUNG - Lufttemperatur - Höchste Lufttemperatur                                                         0.9037555
    ## WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur                                                        0.9581775
    ## WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur                                                         1.0000000
    ## WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt                                                     0.5078810
    ##                                                                   WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt
    ## WITTERUNG - Luftfeuchtigkeit - Mittlere relative Luftfeuchtigkeit                                             -0.08243451
    ## WITTERUNG - Lufttemperatur - Höchste Lufttemperatur                                                            0.43530416
    ## WITTERUNG - Lufttemperatur - Mittlere Lufttemperatur                                                           0.48223577
    ## WITTERUNG - Lufttemperatur - Tiefste Lufttemperatur                                                            0.50788102
    ## WITTERUNG - Niederschlag - Niederschlagsmenge insgesamt                                                        1.00000000

``` r
# explore correlations with R https://drsimonj.svbtle.com/exploring-correlations-in-r-with-corrr
diag(dsCor) <- NA
col_has_over_90 <- apply(dsCor, 2, function(x) any(x > .9))

length(col_has_over_90)
```

    ## [1] 288

``` r
#( dsCor[,col_has_over_90] )
```

## the correlate package

``` r
library(corrr)
correlate(dsFinal, use = "pairwise.complete.obs")
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 288 x 289
    ##    rowname `WITTERUNG - Lu~ `WITTERUNG - Lu~ `WITTERUNG - Lu~
    ##    <chr>              <dbl>            <dbl>            <dbl>
    ##  1 WITTER~          NA               -0.648         -0.622   
    ##  2 WITTER~          -0.648           NA              0.959   
    ##  3 WITTER~          -0.622            0.959         NA       
    ##  4 WITTER~          -0.513            0.904          0.958   
    ##  5 WITTER~          -0.0824           0.435          0.482   
    ##  6 WITTER~           0.260           -0.0987        -0.0835  
    ##  7 WITTER~          -0.823            0.830          0.845   
    ##  8 WIRTSC~           0.0399          -0.0270        -0.000122
    ##  9 WIRTSC~          -0.0729          -0.0955        -0.0721  
    ## 10 WIRTSC~          -0.161            0.221          0.250   
    ## # ... with 278 more rows, and 285 more variables: `WITTERUNG -
    ## #   Lufttemperatur - Tiefste Lufttemperatur` <dbl>, `WITTERUNG -
    ## #   Niederschlag - Niederschlagsmenge insgesamt` <dbl>, `WITTERUNG -
    ## #   Niederschlag - Tage mit Niederschlägen` <dbl>, `WITTERUNG -
    ## #   Sonnenschein - Sonnenscheindauer` <dbl>, `WIRTSCHAFT - Bauhauptgewerbe
    ## #   - Auftragseingänge` <dbl>, `WIRTSCHAFT - Bauhauptgewerbe -
    ## #   Betriebe` <dbl>, `WIRTSCHAFT - Bauhauptgewerbe - Entgelte` <dbl>,
    ## #   `WIRTSCHAFT - Bauhauptgewerbe - Geleistete Arbeitsstd. -
    ## #   insges.` <dbl>, `WIRTSCHAFT - Bauhauptgewerbe - Geleistete Arbeitsstd.
    ## #   - Wohnungsbau` <dbl>, `WIRTSCHAFT - Bauhauptgewerbe - Tätige
    ## #   Personen` <dbl>, `WIRTSCHAFT - Bauhauptgewerbe - Umsatz -
    ## #   insgesamt` <dbl>, `WIRTSCHAFT - Bauhauptgewerbe - Umsatz -
    ## #   Wohnungsbau` <dbl>, `WIRTSCHAFT - Gewerbean- und -abmeldungen -
    ## #   Gewerbeabmeldungen` <dbl>, `WIRTSCHAFT - Gewerbean- und -abmeldungen -
    ## #   Gewerbeanmeldungen` <dbl>, `WIRTSCHAFT - Handwerksbetriebe -
    ## #   Ausbaugewerbe` <dbl>, `WIRTSCHAFT - Handwerksbetriebe -
    ## #   Bauhauptgewerbe` <dbl>, `WIRTSCHAFT - Handwerksbetriebe -
    ## #   Gesamthandwerk insgesamt` <dbl>, `WIRTSCHAFT - Handwerksbetriebe -
    ## #   Gesundheitsgewerbe` <dbl>, `WIRTSCHAFT - Handwerksbetriebe - Handwerk
    ## #   f. den priv. Bedarf` <dbl>, `WIRTSCHAFT - Handwerksbetriebe - Handwerk
    ## #   f. gewerbl. Bedarf` <dbl>, `WIRTSCHAFT - Handwerksbetriebe -
    ## #   Kfz-Gewerbe` <dbl>, `WIRTSCHAFT - Handwerksbetriebe -
    ## #   Lebensmittelgewerbe` <dbl>, `WIRTSCHAFT - Steuern - Einkommensteuer
    ## #   (Gemeindeanteil)` <dbl>, `WIRTSCHAFT - Steuern - Gewerbesteuer` <dbl>,
    ## #   `WIRTSCHAFT - Steuern - Grunderwerbsteuer` <dbl>, `WIRTSCHAFT -
    ## #   Steuern - Umsatzsteuer (Gemeindeanteil)` <dbl>, `WIRTSCHAFT -
    ## #   Verarbeitendes Gewerbe - Auslandsumsatz` <dbl>, `WIRTSCHAFT -
    ## #   Verarbeitendes Gewerbe - Beschäftigte` <dbl>, `WIRTSCHAFT -
    ## #   Verarbeitendes Gewerbe - Betriebe` <dbl>, `WIRTSCHAFT - Verarbeitendes
    ## #   Gewerbe - Entgelte` <dbl>, `WIRTSCHAFT - Verarbeitendes Gewerbe -
    ## #   Geleistete Arbeitsstd.` <dbl>, `WIRTSCHAFT - Verarbeitendes Gewerbe -
    ## #   Gesamtumsatz` <dbl>, `VERKEHRSUNFÄLLE - Alkoholunfälle -
    ## #   insgesamt` <dbl>, `VERKEHRSUNFÄLLE - Alkoholunfälle - Verletzte und
    ## #   Getötete` <dbl>, `VERKEHRSUNFÄLLE - Fluchtunfälle - insgesamt` <dbl>,
    ## #   `VERKEHRSUNFÄLLE - Fluchtunfälle - Verletzte und Getötete` <dbl>,
    ## #   `VERKEHRSUNFÄLLE - Verkehrsunfälle - insgesamt` <dbl>,
    ## #   `VERKEHRSUNFÄLLE - Verkehrsunfälle - mit Personenschäden` <dbl>,
    ## #   `VERKEHRSUNFÄLLE - Verkehrsunfälle - Verletzte und Getötete` <dbl>,
    ## #   `TOURISMUS - Gäste - Ausland` <dbl>, `TOURISMUS - Gäste -
    ## #   Inland` <dbl>, `TOURISMUS - Gäste - insgesamt` <dbl>, `TOURISMUS -
    ## #   Übernachtungen - Ausland` <dbl>, `TOURISMUS - Übernachtungen -
    ## #   Inland` <dbl>, `TOURISMUS - Übernachtungen - insgesamt` <dbl>,
    ## #   `THEATER - Aufführungen - Münchner Kammerspiele` <dbl>, `THEATER -
    ## #   Aufführungen - Nationaltheater` <dbl>, `THEATER - Aufführungen -
    ## #   Prinzregententheater (Großes Haus)` <dbl>, `THEATER - Aufführungen -
    ## #   Residenztheater` <dbl>, `THEATER - Aufführungen - Schauburg - Theater
    ## #   für junges Publikum` <dbl>, `THEATER - Aufführungen - Theater am
    ## #   Gärtnerplatz` <dbl>, `THEATER - Besucher/innen - Münchner
    ## #   Kammerspiele` <dbl>, `THEATER - Besucher/innen -
    ## #   Nationaltheater` <dbl>, `THEATER - Besucher/innen -
    ## #   Prinzregententheater (Großes Haus)` <dbl>, `THEATER - Besucher/innen -
    ## #   Residenztheater` <dbl>, `THEATER - Besucher/innen - Schauburg -
    ## #   Theater für junges Publikum` <dbl>, `THEATER - Besucher/innen -
    ## #   Theater am Gärtnerplatz` <dbl>, `THEATER - Durchschnittl.
    ## #   Platzausnutzung - Münchner Kammerspiele` <dbl>, `THEATER -
    ## #   Durchschnittl. Platzausnutzung - Nationaltheater` <dbl>, `THEATER -
    ## #   Durchschnittl. Platzausnutzung - Prinzregententheater (Großes
    ## #   Haus)` <dbl>, `THEATER - Durchschnittl. Platzausnutzung -
    ## #   Residenztheater` <dbl>, `THEATER - Durchschnittl. Platzausnutzung -
    ## #   Schauburg - Theater für junges Publikum` <dbl>, `THEATER -
    ## #   Durchschnittl. Platzausnutzung - Theater am Gärtnerplatz` <dbl>,
    ## #   `SOZIALE LEISTUNGEN - Empfänger nach SGB XII - Ausländer/innen` <dbl>,
    ## #   `SOZIALE LEISTUNGEN - Empfänger nach SGB XII - Deutsche` <dbl>,
    ## #   `SOZIALE LEISTUNGEN - Empfänger nach SGB XII - Empfänger/innen
    ## #   insges.` <dbl>, `SOZIALE LEISTUNGEN - Empfänger nach SGB XII -
    ## #   Frauen` <dbl>, `SOZIALE LEISTUNGEN - Empfänger nach SGB XII -
    ## #   Männer` <dbl>, `SOZIALE LEISTUNGEN - Grundsicherung im Alter und bei
    ## #   Erwerbsminderung - Ausländer/innen` <dbl>, `SOZIALE LEISTUNGEN -
    ## #   Grundsicherung im Alter und bei Erwerbsminderung - Deutsche` <dbl>,
    ## #   `SOZIALE LEISTUNGEN - Grundsicherung im Alter und bei Erwerbsminderung
    ## #   - Empfänger/innen insges.` <dbl>, `SOZIALE LEISTUNGEN - Grundsicherung
    ## #   im Alter und bei Erwerbsminderung - Frauen` <dbl>, `SOZIALE LEISTUNGEN
    ## #   - Grundsicherung im Alter und bei Erwerbsminderung - Männer` <dbl>,
    ## #   `SOZIALE LEISTUNGEN - Hilfe zum Lebensunterhalt -
    ## #   Ausländer/innen` <dbl>, `SOZIALE LEISTUNGEN - Hilfe zum
    ## #   Lebensunterhalt - Deutsche` <dbl>, `SOZIALE LEISTUNGEN - Hilfe zum
    ## #   Lebensunterhalt - Empfänger/innen insges.` <dbl>, `SOZIALE LEISTUNGEN
    ## #   - Hilfe zum Lebensunterhalt - Frauen` <dbl>, `SOZIALE LEISTUNGEN -
    ## #   Hilfe zum Lebensunterhalt - Männer` <dbl>, `ORCHESTER - Besucher/innen
    ## #   - Bayerisches Staatsorchester` <dbl>, `ORCHESTER - Besucher/innen -
    ## #   Münchner Philharmoniker` <dbl>, `ORCHESTER - Durchschnittl.
    ## #   Platzausnutzung - Bayerisches Staatsorchester` <dbl>, `ORCHESTER -
    ## #   Durchschnittl. Platzausnutzung - Münchner Philharmoniker` <dbl>,
    ## #   `ORCHESTER - Konzerte - Bayerisches Staatsorchester` <dbl>, `ORCHESTER
    ## #   - Konzerte - Münchner Philharmoniker` <dbl>, `MUSEEN - Besucher/innen
    ## #   - Alte Pinakothek` <dbl>, `MUSEEN - Besucher/innen - Bayerisches
    ## #   Nationalmuseum` <dbl>, `MUSEEN - Besucher/innen - Deutsches Museum -
    ## #   Museumsinsel` <dbl>, `MUSEEN - Besucher/innen - Deutsches Museum -
    ## #   Verkehrszentrum` <dbl>, `MUSEEN - Besucher/innen - Münchner
    ## #   Stadtmuseum` <dbl>, `MUSEEN - Besucher/innen - Museum
    ## #   Brandhorst` <dbl>, `MUSEEN - Besucher/innen - Museum Mensch und
    ## #   Natur` <dbl>, `MUSEEN - Besucher/innen - Neue Pinakothek` <dbl>,
    ## #   `MUSEEN - Besucher/innen - Pinakothek der Moderne` <dbl>, `MUSEEN -
    ## #   Besucher/innen - Schackgalerie` <dbl>, `MUSEEN - Besucher/innen -
    ## #   Städtische Galerie im Lenbachhaus` <dbl>, `KINOS - Besucher/innen -
    ## #   insgesamt` <dbl>, `KFZ-Neuzulassungen - Fahrzeugtypen - Kraftfahrzeuge
    ## #   insgesamt` <dbl>, `KFZ-Neuzulassungen - Fahrzeugtypen -
    ## #   Personenkraftwagen Firmen` <dbl>, `KFZ-Neuzulassungen - Fahrzeugtypen
    ## #   - Personenkraftwagen gesamt` <dbl>, `KFZ-Neuzulassungen -
    ## #   Fahrzeugtypen - Personenkraftwagen Privat` <dbl>, ...

``` r
# dsFinal %>% 
#   correlate(use = "pairwise.complete.obs") %>% 
#   focus(3:5, mirror = TRUE) %>% 
#   network_plot()

# above 98% pearson correlation
dsFinal %>% 
  correlate(use = "pairwise.complete.obs") %>% 
  stretch() %>% 
  filter(abs(r)>0.98) %>% 
  head(20)
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 20 x 3
    ##    x                              y                                       r
    ##    <chr>                          <chr>                               <dbl>
    ##  1 WIRTSCHAFT - Handwerksbetrieb~ WIRTSCHAFT - Handwerksbetriebe - ~  0.991
    ##  2 WIRTSCHAFT - Handwerksbetrieb~ BEVÖLKERUNG - Haushalte nach Kind~ -0.982
    ##  3 WIRTSCHAFT - Handwerksbetrieb~ WIRTSCHAFT - Handwerksbetriebe - ~  0.991
    ##  4 WIRTSCHAFT - Handwerksbetrieb~ WIRTSCHAFT - Handwerksbetriebe - ~  0.990
    ##  5 WIRTSCHAFT - Handwerksbetrieb~ SOZIALE LEISTUNGEN - Empfänger na~  0.981
    ##  6 WIRTSCHAFT - Handwerksbetrieb~ SOZIALE LEISTUNGEN - Empfänger na~  0.982
    ##  7 WIRTSCHAFT - Handwerksbetrieb~ SOZIALE LEISTUNGEN - Grundsicheru~  0.984
    ##  8 WIRTSCHAFT - Handwerksbetrieb~ SOZIALE LEISTUNGEN - Grundsicheru~  0.982
    ##  9 WIRTSCHAFT - Handwerksbetrieb~ SOZIALE LEISTUNGEN - Grundsicheru~  0.984
    ## 10 WIRTSCHAFT - Handwerksbetrieb~ KFZ-Bestand - Pkw-Kraftstoffarten~  0.981
    ## 11 WIRTSCHAFT - Handwerksbetrieb~ BEVÖLKERUNG - EU-Nationalitäten -~  0.985
    ## 12 WIRTSCHAFT - Handwerksbetrieb~ BEVÖLKERUNG - Familienstand - led~  0.980
    ## 13 WIRTSCHAFT - Handwerksbetrieb~ BEVÖLKERUNG - Geschlecht und Staa~  0.981
    ## 14 WIRTSCHAFT - Handwerksbetrieb~ BEVÖLKERUNG - Geschlecht und Staa~  0.981
    ## 15 WIRTSCHAFT - Handwerksbetrieb~ BEVÖLKERUNG - Nicht-EU-Nationalit~ -0.982
    ## 16 WIRTSCHAFT - Handwerksbetrieb~ BEVÖLKERUNG - Religionszugehörigk~  0.981
    ## 17 WIRTSCHAFT - Handwerksbetrieb~ WIRTSCHAFT - Handwerksbetriebe - ~  0.990
    ## 18 WIRTSCHAFT - Verarbeitendes G~ WIRTSCHAFT - Verarbeitendes Gewer~  0.986
    ## 19 WIRTSCHAFT - Verarbeitendes G~ WIRTSCHAFT - Verarbeitendes Gewer~  0.986
    ## 20 VERKEHRSUNFÄLLE - Verkehrsunf~ VERKEHRSUNFÄLLE - Verkehrsunfälle~  0.981

``` r
# how many does each correlate with
dsFinal %>% 
  correlate(use = "pairwise.complete.obs") %>% 
  stretch() %>% 
  filter(abs(r)>0.98) %>%
  group_by(x) %>%
  count() %>% 
  arrange(-n) %>% 
  head(20)
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 20 x 2
    ## # Groups:   x [20]
    ##    x                                                                      n
    ##    <chr>                                                              <int>
    ##  1 BEVÖLKERUNG - Familienstand - ledig                                   29
    ##  2 BEVÖLKERUNG - Religionszugehörigkeit - sonstige/ ohne/ ohne Angabe    29
    ##  3 BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Ausländer weib~    28
    ##  4 BEVÖLKERUNG - Altersgruppen - Minderjährige (unter 18 J.)             27
    ##  5 BEVÖLKERUNG - EU-Nationalitäten - Bulgarien                           27
    ##  6 BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/inne~    27
    ##  7 BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner weib~    26
    ##  8 BEVÖLKERUNG - Altersgruppen - Noch nicht Schulpflichtige (unter 6~    25
    ##  9 SOZIALE LEISTUNGEN - Grundsicherung im Alter und bei Erwerbsminde~    25
    ## 10 SOZIALE LEISTUNGEN - Grundsicherung im Alter und bei Erwerbsminde~    24
    ## 11 BEVÖLKERUNG - Altersgruppen - Strafmündige (14 J. und älter)          23
    ## 12 BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner männ~    23
    ## 13 SOZIALE LEISTUNGEN - Grundsicherung im Alter und bei Erwerbsminde~    23
    ## 14 BEVÖLKERUNG - Altersgruppen - Erwerbsfähige (15- 64 J.)               22
    ## 15 BEVÖLKERUNG - Altersgruppen - Volljährige (18 J. und älter)           22
    ## 16 BEVÖLKERUNG - Haushalte nach Nationengruppe - deutsch - ausländis~    22
    ## 17 BEVÖLKERUNG - Altersgruppen - Schulpflichtige (6- 14 J.)              21
    ## 18 BEVÖLKERUNG - Haushalte nach Kinderzahl - mit 1 Kind                  21
    ## 19 BEVÖLKERUNG - Kontinente - Amerika                                    21
    ## 20 SOZIALE LEISTUNGEN - Empfänger nach SGB XII - Männer                  21

``` r
# split the correlated variables by section/subsections
dsFinal %>% 
  correlate(use = "pairwise.complete.obs") %>% 
  stretch() %>% 
  filter(abs(r)>0.98) %>%
  mutate(x_field = x, y_field = y) %>%
  separate(x, into = c("x_sec", "x_subsection", "x_subsubsection"), sep = " - ") %>%
  separate(y, into = c("y_sec", "y_subsection", "y_subsubsection"), sep = " - ") %>%
  filter(x_sec!=y_sec)-> dsAgg
```

    ## 
    ## Correlation method: 'pearson'
    ## Missing treated using: 'pairwise.complete.obs'

``` r
# main correlated sections
( table(dsAgg$x_sec, dsAgg$y_sec) )
```

    ##                     
    ##                      BEVÖLKERUNG KFZ-Bestand SOZIALE LEISTUNGEN WIRTSCHAFT
    ##   BEVÖLKERUNG                  0          33                 78          7
    ##   KFZ-Bestand                 33           0                  8          1
    ##   SOZIALE LEISTUNGEN          78           8                  0          5
    ##   WIRTSCHAFT                   7           1                  5          0

``` r
# dsFinal %>% 
#   correlate(use = "pairwise.complete.obs") %>% 
#   network_plot(min_cor = 0.98)

  

dsAgg %>%
  inner_join(nasFinal, by = c("x_field"="field_name")) %>%
  filter(NAs_proc <0.1) %>%
  select(x_field, y_field, r, NAs_proc) -> dsAggFiltered
(dsAggFiltered)
```

    ## # A tibble: 108 x 4
    ##    x_field                      y_field                          r NAs_proc
    ##    <chr>                        <chr>                        <dbl>    <dbl>
    ##  1 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Grunds~ 0.982  0.00847
    ##  2 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Grunds~ 0.981  0.00847
    ##  3 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Empfän~ 0.991  0.00847
    ##  4 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Empfän~ 0.981  0.00847
    ##  5 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Grunds~ 0.990  0.00847
    ##  6 BEVÖLKERUNG - Altersgruppen~ KFZ-Bestand - Fahrzeugtypen~ 0.985  0.00847
    ##  7 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Empfän~ 0.992  0.00847
    ##  8 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Empfän~ 0.981  0.00847
    ##  9 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Grunds~ 0.989  0.00847
    ## 10 BEVÖLKERUNG - Altersgruppen~ SOZIALE LEISTUNGEN - Empfän~ 0.981  0.00847
    ## # ... with 98 more rows

``` r
##########################
```

# Next steps

  - we look at cross-topic correlations (assuming that inter-topic
    correlations are logical: highest to lowest temperature f.e.)
  - explore some highly correlated numbers
  - **BEWARE OF CONFOUNDING VARIABLES**

## Highly Correlated variables

These are pure sample examples out of
\~100

``` r
plot(dsFinal$`BEVÖLKERUNG - Altersgruppen - Rentner/innen (65 J. und älter)`, dsFinal$`SOZIALE LEISTUNGEN - Empfänger nach SGB XII - Empfänger/innen insges.`,
     main="Old-age pensioner and social welfare recipients")
```

![](Munich_numbers_files/figure-gfm/prepare_correlation_charts-1.jpeg)<!-- -->

``` r
plot(dsFinal$`BEVÖLKERUNG - Familienstand - geschieden`, dsFinal$`KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt`,
     main="Divorces and Diesel cars")
```

![](Munich_numbers_files/figure-gfm/prepare_correlation_charts-2.jpeg)<!-- -->

## linear models

Handworkers by number of diesel cars and total
population

``` r
model.lm <- lm(`WIRTSCHAFT - Handwerksbetriebe - Handwerk f. den priv. Bedarf` ~ `KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt` + `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/innen insgesamt`, data = dsFinal)
summary(model.lm)
```

    ## 
    ## Call:
    ## lm(formula = `WIRTSCHAFT - Handwerksbetriebe - Handwerk f. den priv. Bedarf` ~ 
    ##     `KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt` + `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/innen insgesamt`, 
    ##     data = dsFinal)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -163.29  -75.10   23.08   65.65  140.01 
    ## 
    ## Coefficients:
    ##                                                                                  Estimate
    ## (Intercept)                                                                    -7.815e+02
    ## `KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt`                             1.416e-03
    ## `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/innen insgesamt`  3.978e-03
    ##                                                                                Std. Error
    ## (Intercept)                                                                     3.665e+02
    ## `KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt`                             6.676e-04
    ## `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/innen insgesamt`  3.615e-04
    ##                                                                                t value
    ## (Intercept)                                                                     -2.132
    ## `KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt`                              2.121
    ## `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/innen insgesamt`  11.006
    ##                                                                                Pr(>|t|)
    ## (Intercept)                                                                      0.0344
    ## `KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt`                              0.0354
    ## `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/innen insgesamt`   <2e-16
    ##                                                                                   
    ## (Intercept)                                                                    *  
    ## `KFZ-Bestand - Pkw-Kraftstoffarten - Diesel gesamt`                            *  
    ## `BEVÖLKERUNG - Geschlecht und Staatsangehörigkeit - Einwohner/innen insgesamt` ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 81.89 on 169 degrees of freedom
    ##   (64 observations deleted due to missingness)
    ## Multiple R-squared:  0.9617, Adjusted R-squared:  0.9613 
    ## F-statistic:  2123 on 2 and 169 DF,  p-value: < 2.2e-16

``` r
plot(model.lm)
```

![](Munich_numbers_files/figure-gfm/prepare_linear_models-1.jpeg)<!-- -->![](Munich_numbers_files/figure-gfm/prepare_linear_models-2.jpeg)<!-- -->![](Munich_numbers_files/figure-gfm/prepare_linear_models-3.jpeg)<!-- -->![](Munich_numbers_files/figure-gfm/prepare_linear_models-4.jpeg)<!-- -->

:)
