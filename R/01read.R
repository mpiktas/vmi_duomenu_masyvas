library(dplyr)
library(tidyr)
library(lubridate)

dt <- read.csv("DATA/DuomenuMasyvasCSV.csv", stringsAsFactors = FALSE)


idcols <- c("ID", "MMR.reg.metai", "PVM.reg.metai", "Statusas", "Darbuotojai.20200301",
            "Darbuotojai.20200401", "Darbo.apmokėjimo.suma.2019m", "EV.kodas",
            "EV.pavadinimas", "EV.skyrius", "EV.sekcija", "EV.skyriaus.kodas",
            "EV.sekcijos.kodas",
            "Mokėjimai.2019m",
            "PVMD.Apyvarta.2019m",
            "PVM.Prievolė.2019m"
            )
dtid <- dt[, idcols]

ldtm <- dt[ c("ID",setdiff(colnames(dt),idcols))] %>% pivot_longer(-ID, "type")

tp <- ldtm$type %>% unique


ltp <- tp %>% strsplit("." , fixed = TRUE) %>% lapply(function(x)gsub("([0-9]+)(.*)","\\1",x))

parse_name <- function(x) {
    nn <- na.omit(as.integer(x))
    nm <- paste(x[attributes(nn)$na.action], collapse = "_")
    mm <- paste(paste(x[-attributes(nn)$na.action], collapse = "-"),"01", sep = "-")
    data.frame(variable = nm, month = mm)
}

nice_names <- ltp %>% lapply(parse_name) %>% bind_rows

nice_names <- cbind(data.frame(type = tp, stringsAsFactors = FALSE), nice_names)

ldtm <- ldtm %>% inner_join(nice_names)

mokd <- ldtm %>% select(-type) %>% pivot_wider(c("ID","month"), "variable")

mokd %>% write.csv("DATA/monthly.csv", row.names = FALSE)
dtid %>% write.csv("DATA/idyearly.csv", row.names  = FALSE)



