## ---- message = FALSE----------------------------------------------------
# load the required packages
library(IDA)
library(knitr)

# filter for AR5 data
ar5 <- filter(idata, source_id == "AR5")

head(ar5)

## ---- message = FALSE----------------------------------------------------
kable(head(ar5), format = 'markdown')

## ------------------------------------------------------------------------
co2_glob <- filter(idata, source_id == "Joeri_1p5", variable == "Emissions|CO2|Total", spatial == "World")

## ---- echo = FALSE-------------------------------------------------------
library(ggplot2)

ggplot(co2_glob, aes(x = temporal, y = value, group = scenario)) +
  geom_line() +
  ggtitle(unique(co2_glob$variable)) +
  labs(x = "year", y = "Mt CO2/yr") +
  theme_bw()


## ---- message = FALSE----------------------------------------------------
co2_1p5_2060 <- filter(idata, source_id == "Joeri_1p5", temporal == 2060,
                       spatial != "World",
                       scenario == "myo_L15_BC_a",
                       variable == "Emissions|CO2|Fossil fuels and Industry",
                       value > 0)


## ---- echo = FALSE-------------------------------------------------------
kable(co2_1p5_2060, format = "markdown")

## ---- echo = FALSE-------------------------------------------------------
co2_2060 <-  filter(idata, source_id == "Joeri_1p5", temporal == 2060,
                    spatial != "World",
                    scenario %in% c("myo_L15_BC_a", "myo_L_BC_a"),
                    variable == "Emissions|CO2|Fossil fuels and Industry",
                    value > 0)

## ---- echo = FALSE-------------------------------------------------------
kable(co2_2060, format = "markdown")

