ss <- mokd %>% inner_join(dtid %>% select(ID, EV.kodas)) %>% select(-ID) %>%
    group_by(EV.kodas, month) %>%
    summarize(n = n(),
              Mokejimai.q1 = quantile(Mokejimai, 0.25, na.rm = TRUE),
              Mokejimai.q2 = quantile(Mokejimai, 0.5, na.rm = TRUE),
              Mokejimai.q3 = quantile(Mokejimai, 0.75, na.rm = TRUE),
              Mokejimai = sum(Mokejimai, na.rm = TRUE),
              PVMD_Apyvarta.q1 = quantile(PVMD_Apyvarta, 0.25, na.rm = TRUE),
              PVMD_Apyvarta.q2 = quantile(PVMD_Apyvarta, 0.5, na.rm = TRUE),
              PVMD_Apyvarta.q3 = quantile(PVMD_Apyvarta, 0.75, na.rm = TRUE),
              PVMD_Apyvarta = sum(PVMD_Apyvarta, na.rm = TRUE),
              PVM_Prievolė.q1 = quantile(PVM_Prievolė, 0.25, na.rm = TRUE),
              PVM_Prievolė.q2 = quantile(PVM_Prievolė, 0.5, na.rm = TRUE),
              PVM_Prievolė.q3 = quantile(PVM_Prievolė, 0.75, na.rm = TRUE),
              PVM_Prievolė = sum(PVM_Prievolė, na.rm = TRUE))


#mokd %>% inner_join(dtid %>% select(ID, EV.kodas)) %>% select(-ID) %>% pivot_longer