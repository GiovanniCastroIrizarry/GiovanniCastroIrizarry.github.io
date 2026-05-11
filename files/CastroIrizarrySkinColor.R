# Recargar datos con party ID y nativity
anes2016_v2 <- read_dta(
  "Downloads/ANES/anes_timeseries_2016_dta/anes_timeseries_2016.dta",
  col_select = c(
    "V160501",    # Modo
    "V161310x",   # Raza summary
    "V162368",    # Skintone self
    "V161267",    # Edad
    "V161342",    # Genero
    "V161270",    # Educacion
    "V161361x",   # Ingreso
    "V160101",    # Peso
    "V161155",    # Party ID general
    "V161316"     # Nativity
  )
)

library(dplyr)

# Tabla descriptiva por modo y grupo racial
tabla_descriptiva <- todos_v2 %>%
  filter(!is.na(skintone_self)) %>%
  mutate(
    grupo = case_when(
      is.na(raza) ~ NA_character_,
      TRUE ~ raza
    )
  ) %>%
  bind_rows(
    todos_v2 %>%
      filter(!is.na(skintone_self)) %>%
      mutate(grupo = "Full Sample")
  ) %>%
  filter(!is.na(grupo)) %>%
  group_by(grupo, modo) %>%
  summarise(
    n = n(),
    mean = round(mean(skintone_self, na.rm = TRUE), 2),
    sd = round(sd(skintone_self, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    grupo = factor(grupo,
                   levels = c("Full Sample", "White",
                              "Latino", "Asian", "Black"))
  ) %>%
  arrange(grupo, modo)

print(tabla_descriptiva)

# T-test para cada grupo
grupos <- c("Full Sample", "White", "Latino", "Asian", "Black")

for (g in grupos) {
  if (g == "Full Sample") {
    dat <- todos_v2 %>% filter(!is.na(skintone_self))
  } else {
    dat <- todos_v2 %>% filter(raza == g, !is.na(skintone_self))
  }
  test <- t.test(skintone_self ~ modo, data = dat)
  cat(g, ": diff =", round(diff(test$estimate), 3),
      ", p =", round(test$p.value, 4), "\n")
}

library(modelsummary)
library(dplyr)

tabla1 <- tabla_descriptiva %>%
  mutate(
    cell = paste0(mean, " (", sd, ")\nN = ", n)
  ) %>%
  select(grupo, modo, cell) %>%
  tidyr::pivot_wider(names_from = modo, values_from = cell) %>%
  rename(
    "Racial Group" = grupo,
    "Live Interview" = FTF,
    "Self-Administered" = Internet
  )

# Añadir columna de diferencia y p-value
diffs <- data.frame(
  "Racial Group" = c("Full Sample", "White", "Latino", "Asian", "Black"),
  "Difference" = c("-0.645***", "-0.511***", "-0.927***", "-1.115**", "-0.393"),
  check.names = FALSE
)

tabla1_final <- left_join(tabla1, diffs, by = "Racial Group")

datasummary_df(tabla1_final,
               title = "Table 1. Self-Reported Skin Tone by Survey Mode and Racial Group",
               notes = "Mean skin tone (SD) and sample size by mode. Skin tone coded 1 (lightest) to 10 (darkest). Difference = Self-Administered minus Live Interview. ** p < 0.01, *** p < 0.001, based on two-sample t-tests.",
               output = "table1_descriptive.docx")

todos_v2 <- anes2016_v2 %>%
  mutate(
    modo = factor(V160501, levels = c(1, 2),
                  labels = c("FTF", "Internet")),
    skintone_self = ifelse(V162368 < 0, NA, V162368),
    edad = ifelse(V161267 < 0, NA, V161267),
    genero = ifelse(V161342 < 0, NA, V161342),
    educ = ifelse(V161270 < 0 | V161270 >= 90, NA, V161270),
    ingreso = ifelse(V161361x < 0, NA, V161361x),
    
    # Republicano = 1, todo lo demas = 0, missing para DK/refused
    republican = case_when(
      V161155 == 2 ~ 1,
      V161155 %in% c(0, 1, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),
    
    foreign_born = case_when(
      V161316 %in% c(1, 7) ~ 0,
      V161316 %in% c(2, 3, 4) ~ 1,
      TRUE ~ NA_real_
    ),
    raza = case_when(
      V161310x == 1 ~ "White",
      V161310x == 2 ~ "Black",
      V161310x == 3 ~ "Asian",
      V161310x == 5 ~ "Latino",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(raza))

# Modelo 1: Toda la muestra
m1 <- lm(skintone_self ~ modo + edad + genero + educ + ingreso + republican,
         data = todos_v2, weights = V160101)

# Modelo 2: Blancos
m2 <- lm(skintone_self ~ modo + edad + genero + educ + ingreso + republican,
         data = filter(todos_v2, raza == "White"),
         weights = V160101)

# Modelo 3: Negros
m3 <- lm(skintone_self ~ modo + edad + genero + educ + ingreso + republican,
         data = filter(todos_v2, raza == "Black"),
         weights = V160101)

# Modelo 4: Asiaticos
m4 <- lm(skintone_self ~ modo + edad + genero + educ + ingreso + republican,
         data = filter(todos_v2, raza == "Asian"),
         weights = V160101)

# Modelo 5: Latinos
m5 <- lm(skintone_self ~ modo + edad + genero + educ + ingreso + republican,
         data = filter(todos_v2, raza == "Latino"),
         weights = V160101)

# Modelo 6: Asiaticos con nativity
m6 <- lm(skintone_self ~ modo + edad + genero + educ + ingreso + republican + foreign_born,
         data = filter(todos_v2, raza == "Asian"),
         weights = V160101)

# Modelo 7: Latinos con nativity
m7 <- lm(skintone_self ~ modo + edad + genero + educ + ingreso + republican + foreign_born,
         data = filter(todos_v2, raza == "Latino"),
         weights = V160101)

# Tabla comparativa del efecto del modo en todos los modelos
library(broom)
library(purrr)
library(dplyr)

map_dfr(
  list(Todos = m1, White = m2, Black = m3,
       Asian = m4, Latino = m5,
       Asian_nativity = m6, Latino_nativity = m7),
  ~ tidy(.x, conf.int = TRUE) %>% filter(term == "modoInternet"),
  .id = "modelo"
) %>%
  select(modelo, estimate, std.error, statistic, p.value,
         conf.low, conf.high)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)


coef_df <- map_dfr(
  list(
    Full_Sample = m1,
    White = m2,
    Black = m3,
    Asian = m4,
    Latino = m5,
    Asian_nativity = m6,
    Latino_nativity = m7
  ),
  ~ tidy(.x, conf.int = TRUE) %>% filter(term == "modoInternet"),
  .id = "modelo"
) %>%
  mutate(
    modelo = recode(modelo,
                    "Full_Sample" = "All Respondents",
                    "Asian_nativity" = "Asian\n(+ nativity)",
                    "Latino_nativity" = "Latino\n(+ nativity)"
    ),
    modelo = factor(
      modelo,
      levels = c(
        "Latino\n(+ nativity)",
        "Latino",
        "Asian\n(+ nativity)",
        "Asian",
        "Black",
        "White",
        "All Respondents"
      )
    )
  )

ggplot(coef_df, aes(x = estimate, y = modelo,
                    color = modelo, shape = modelo)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, linewidth = 1) +
  geom_point(size = 3.5, stroke = 1) +
  scale_shape_manual(values = c(7, 6, 5, 4, 3, 2, 1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Effect of Self-Administered (vs. Live Interview Mode)",
    y = "",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

ggsave("~/Downloads/mode_effects_all_models.jpg",
       width = 5, height = 5,
       dpi = 300, bg = "white")

library(marginaleffects)

# Predicciones para toda la poblacion
preds_todos <- predictions(m1,
                           newdata = datagrid(modo = c("FTF", "Internet"))) %>%
  as.data.frame() %>%
  select(modo, estimate, conf.low, conf.high) %>%
  mutate(grupo = "All\nRespondents")

# Predicciones por grupo racial
preds_grupos <- map_dfr(
  list(
    "White\n(non-Hispanic)" = m2,
    "Black" = m3,
    "Asian" = m4,
    "Latino" = m5
  ),
  ~ predictions(.x,
                newdata = datagrid(modo = c("FTF", "Internet"))) %>%
    as.data.frame() %>%
    select(modo, estimate, conf.low, conf.high),
  .id = "grupo"
)

preds_completo <- bind_rows(preds_todos, preds_grupos) %>%
  mutate(
    grupo = factor(grupo,
                   levels = c("All\nRespondents",
                              "White\n(non-Hispanic)",
                              "Latino",
                              "Asian",
                              "Black")),
    modo = factor(modo, 
                  levels = c("FTF", "Internet"),
                  labels = c("Live\nInterview", "Self\nAdministered"))
  )

ggplot(preds_completo,
       aes(x = modo, y = estimate, group = 1)) +
  geom_point(size = 4, color = "#2C3E50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.12, linewidth = 0.8,
                color = "#2C3E50") +
  facet_wrap(~ grupo, scales = "fixed", nrow = 1) +
  labs(
    x = "Survey Mode",
    y = "Predicted Skin Tone (1=Lightest, 10=Darkest)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 0.8),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

ggsave("~/Downloads/predicted_skintone_by_mode.jpg",
       width = 9, height = 5.5,
       dpi = 300, bg = "white")

library(rmarkdown)
library(modelsummary)

modelsummary(
  list(
    "Full Sample" = m1,
    "White" = m2,
    "Black" = m3,
    "Asian" = m4,
    "Asian (+ nativity)" = m6,
    "Latino" = m5,
    "Latino (+ nativity)" = m7
  ),
  coef_rename = c(
    "modoInternet" = "Self-Administered mode",
    "edad"         = "Age",
    "genero"       = "Gender",
    "educ"         = "Education",
    "ingreso"      = "Income",
    "republican"   = "Republican",
    "foreign_born" = "Foreign born"
  ),
  coef_omit = "(Intercept)",
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = c("nobs", "r.squared"),
  notes = "OLS with survey weights. Controls include age, gender, education, income, and party identification. Models 6 and 7 add nativity control for Asian and Latino subsamples respectively. Standard errors in parentheses.",
  output = "table_models.docx"
)



library(tidyverse)

# Calcular proporcion de cada valor de skintone por modo
skin_dist <- todos_v2 %>%
  filter(!is.na(skintone_self)) %>%
  group_by(modo, skintone_self) %>%
  summarise(n = sum(V160101), .groups = "drop") %>%
  group_by(modo) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    modo = factor(modo,
                  levels = c("FTF", "Internet"),
                  labels = c("Live Interview", "Self-Administered"))
  )

ggplot(skin_dist, aes(x = skintone_self, y = prop,
                      color = modo, linetype = modo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:10,
                     labels = 1:10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Live Interview" = "#2C3E50",
                                "Self-Administered" = "#E74C3C")) +
  scale_linetype_manual(values = c("Live Interview" = "solid",
                                   "Self-Administered" = "dashed")) +
  labs(
    x = "Skin Tone (1 = Lightest, 10 = Darkest)",
    y = "Proportion of Respondents",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black")
  )

ggsave("~/Downloads/skintone_distribution_by_mode.jpg",
       width = 7, height = 5,
       dpi = 300, bg = "white")

library(dplyr)

# Calcular distribucion para toda la muestra
skin_dist_full <- todos_v2 %>%
  filter(!is.na(skintone_self)) %>%
  group_by(modo, skintone_self) %>%
  summarise(n = sum(V160101), .groups = "drop") %>%
  group_by(modo) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    modo = factor(modo,
                  levels = c("FTF", "Internet"),
                  labels = c("Live Interview", "Self-Administered")),
    grupo = "All Respondents"
  )

# Calcular distribucion por grupo racial
skin_dist_grupos <- todos_v2 %>%
  filter(!is.na(skintone_self), !is.na(raza)) %>%
  group_by(raza, modo, skintone_self) %>%
  summarise(n = sum(V160101), .groups = "drop") %>%
  group_by(raza, modo) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    modo = factor(modo,
                  levels = c("FTF", "Internet"),
                  labels = c("Live Interview", "Self-Administered")),
    grupo = raza
  ) %>%
  select(-raza)

# Combinar
skin_dist_all <- bind_rows(skin_dist_full, skin_dist_grupos) %>%
  mutate(
    grupo = factor(grupo,
                   levels = c("All Respondents", "White",
                              "Latino", "Asian", "Black"))
  )

# Grafica faceteada
ggplot(skin_dist_all, aes(x = skintone_self, y = prop,
                          color = modo, linetype = modo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Live Interview" = "#2C3E50",
                                "Self-Administered" = "#E74C3C")) +
  scale_linetype_manual(values = c("Live Interview" = "solid",
                                   "Self-Administered" = "dashed")) +
  facet_wrap(~ grupo, ncol = 1, scales = "free_y") +
  labs(
    x = "Skin Tone (1 = Lightest, 10 = Darkest)",
    y = "Proportion of Respondents",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("~/Downloads/skintone_distribution_by_mode_race.jpg",
       width = 7, height = 14,
       dpi = 300, bg = "white")

library(ggtext)

# Crear labels con HTML para italica solo en N
n_labels <- bind_rows(n_full, n_grupos) %>%
  mutate(
    grupo = factor(raza,
                   levels = c("All Respondents", "White",
                              "Latino", "Asian", "Black")),
    label = paste0(raza, " (*N* = ", n, ")")
  )

# Añadir labels al dataset
skin_dist_all <- skin_dist_all %>%
  select(-label) %>%
  left_join(n_labels %>% select(grupo, label), by = "grupo") %>%
  mutate(label = factor(label,
                        levels = n_labels$label[order(n_labels$grupo)]))

# Grafica
ggplot(skin_dist_all, aes(x = skintone_self, y = prop,
                          color = modo, linetype = modo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Live Interview" = "#2C3E50",
                                "Self-Administered" = "#E74C3C")) +
  scale_linetype_manual(values = c("Live Interview" = "solid",
                                   "Self-Administered" = "dashed")) +
  facet_wrap(~ label, ncol = 1, scales = "free_y") +
  labs(
    x = "Skin Tone (1 = Lightest, 10 = Darkest)",
    y = "Proportion of Respondents",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black"),
    strip.text = element_markdown(face = "plain", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("~/Downloads/skintone_distribution_by_mode_race.jpg",
       width = 7, height = 14,
       dpi = 300, bg = "white")

# Calcular N por grupo
n_grupos <- todos_v2 %>%
  filter(!is.na(skintone_self), !is.na(raza)) %>%
  group_by(raza) %>%
  summarise(n = n(), .groups = "drop")

n_full <- todos_v2 %>%
  filter(!is.na(skintone_self)) %>%
  summarise(n = n()) %>%
  mutate(raza = "All Respondents")

# Crear labels con HTML para italica solo en N
n_labels <- bind_rows(n_full, n_grupos) %>%
  mutate(
    grupo = factor(raza,
                   levels = c("All Respondents", "White",
                              "Latino", "Asian", "Black")),
    label = paste0(raza, " (*N* = ", format(n, big.mark = ","), ")")
  )

# Añadir labels al dataset
skin_dist_all <- skin_dist_all %>%
  select(-any_of("label")) %>%
  left_join(n_labels %>% select(grupo, label), by = "grupo") %>%
  mutate(label = factor(label,
                        levels = n_labels$label[order(n_labels$grupo)]))

# Grafica
ggplot(skin_dist_all, aes(x = skintone_self, y = prop,
                          color = modo, linetype = modo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Live Interview" = "#2C3E50",
                                "Self-Administered" = "#E74C3C")) +
  scale_linetype_manual(values = c("Live Interview" = "solid",
                                   "Self-Administered" = "dashed")) +
  facet_wrap(~ label, ncol = 1, scales = "free_y") +
  labs(
    x = "Skin Tone (1 = Lightest, 10 = Darkest)",
    y = "Proportion of Respondents",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(color = "black"),
    strip.text = element_markdown(face = "plain", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("~/Downloads/skintone_distribution_by_mode_race.jpg",
       width = 6.5, height = 9,
       dpi = 300, bg = "white")

todos_v2 %>%
  filter(raza == "Latino", !is.na(skintone_self)) %>%
  group_by(modo, skintone_self) %>%
  summarise(n = sum(V160101), .groups = "drop") %>%
  group_by(modo) %>%
  mutate(prop = round(n / sum(n) * 100, 1)) %>%
  arrange(modo, skintone_self)

todos_v2 %>%
  filter(raza == "Asian", !is.na(skintone_self)) %>%
  group_by(modo, skintone_self) %>%
  summarise(n = sum(V160101), .groups = "drop") %>%
  group_by(modo) %>%
  mutate(prop = round(n / sum(n) * 100, 1)) %>%
  arrange(modo, skintone_self)

todos_v2 %>%
  filter(!is.na(skintone_self)) %>%
  mutate(grupo = ifelse(is.na(raza), "All Respondents", raza)) %>%
  bind_rows(
    todos_v2 %>%
      filter(!is.na(skintone_self)) %>%
      mutate(grupo = "All Respondents")
  ) %>%
  filter(grupo %in% c("All Respondents", "White", "Latino", "Asian", "Black")) %>%
  group_by(grupo, modo, skintone_self) %>%
  summarise(n = sum(V160101), .groups = "drop") %>%
  group_by(grupo, modo) %>%
  mutate(prop = round(n / sum(n) * 100, 1)) %>%
  arrange(grupo, modo, skintone_self) %>%
  print(n = 100)