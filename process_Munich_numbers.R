#
rm(list=ls())
gc()

# url
url = 'http://www.mstatistik-muenchen.de/monatszahlenmonitoring/export/xlsx/mzm_export_alle_monatszahlen.xlsx'
destExcelFile = "file.xlsx"

library(RCurl)

download.file(url = url,
              destfile=destExcelFile,
              method="libcurl",
              mode = "wb")

library(readxl)
library(tidyverse)
path = destExcelFile
dat <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

rm(path, destExcelFile, url)

dat$KINOS %>%
  select(MONATSZAHL) %>%
  table()

dat$KINOS %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
    geom_line() + 
    facet_wrap(~AUSPRAEGUNG) +
    theme_classic()+ 
    geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("monthly trend - Cinema Visitors")
ggsave("monthly trend - Cinema Visitors.png", dpi=400, dev='png', height=4, width=5, units="in", scale = 2)


dat$WITTERUNG %>%
  filter(MONATSZAHL %in% c("Sonnenschein", "Lufttemperatur")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free") +
  theme_classic()+ 
  geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("monthly weather trends")
ggsave("monthly weather trends.png", dpi=400, dev='png', height=4, width=5, units="in", scale = 2)


dat$TOURISMUS %>%
  filter(MONATSZAHL %in% c("Gäste")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free") +
  theme_classic()+ 
  geom_smooth() + scale_y_continuous(labels = scales::comma) + ggtitle("monthly trend - Turist guests")
ggsave("monthly trend - Turist guests.png", dpi=400, dev='png', height=4, width=5, units="in", scale = 2)


dat$BEVÖLKERUNG %>%
  filter(MONATSZAHL %in% c("Familienstand")) %>%
  mutate(Month = as.Date(paste0(MONAT,'01'), format = '%Y%m%d')) %>%
  ggplot(., aes(x=Month, y = WERT, group=AUSPRAEGUNG)) + 
  geom_line() + 
  facet_wrap(~AUSPRAEGUNG, scales = "free") +
  theme_classic()+ 
  scale_y_continuous(labels = scales::comma) + ggtitle("monthly trend - population by family status")
ggsave("monthly trend - population by family status.png", dpi=400, dev='png', height=4, width=5, units="in", scale = 2)


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
str(dsFinal)

dsCor <- cor(dsFinal, use = "pairwise.complete.obs")

dsCor[1:5, 1:5]

# explore correlations with R https://drsimonj.svbtle.com/exploring-correlations-in-r-with-corrr
diag(dsCor) <- NA
col_has_over_90 <- apply(dsCor, 2, function(x) any(x > .9))
 
View( dsCor[,col_has_over_90] )

library(corrr)
correlate(dsFinal, use = "pairwise.complete.obs")
