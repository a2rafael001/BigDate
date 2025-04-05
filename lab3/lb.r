library(tidyverse)
library(patchwork)
library(ggrepel)
library(scales)
library(ggthemes)



data <- read.csv("C:/Users/rafae/Downloads/Telegram Desktop/summer.csv", 
                 stringsAsFactors = FALSE)


usa_tennis <- data %>%
  filter(Country == "USA", Sport == "Tennis") %>%
  mutate(Gender = ifelse(Gender == "Men", "Мужчины", "Женщины"))

# 1. Столбчатая диаграмма по количеству медалей 1-3 места США (теннис)
usa_tennis %>%
  count(Year, Medal) %>%
  ggplot(aes(x = factor(Year), y = n, fill = Medal)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "gray80", "Bronze" = "darkorange")) +
  labs(title = "Медали США в теннисе (по годам)",
       x = "Год Олимпиады", y = "Количество медалей", fill = "Тип медали") +
  theme_minimal()

# 2. Круговая диаграмма по золотым медалям США
usa_gold <- usa_tennis %>% filter(Medal == "Gold") %>% count(Year)

usa_gold <- usa_tennis %>% 
  filter(Medal == "Gold") %>% 
  count(Year) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 1),
    Label = paste0( "\n", n, " (", Percent, "%)")
  )

ggplot(usa_gold, aes(x = "", y = n, fill = factor(Year))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Золотые медали США по годам (теннис)", fill = "Год") +
  theme_void()


#3 === ТОП-10 теннисистов США по количеству медалей ===
top10_athletes <- read.csv("C:/Users/rafae/Downloads/Telegram Desktop/summer.csv") %>% 
  filter(Country == "USA", Sport == "Tennis") %>%
  count(Athlete, Medal) %>%
  pivot_wider(names_from = Medal, values_from = n, values_fill = 0) %>%
  mutate(Total = Gold + Silver + Bronze) %>%
  arrange(desc(Total)) %>%
  head(10) %>%
  pivot_longer(cols = c(Gold, Silver, Bronze), 
               names_to = "MedalType", 
               values_to = "Count") %>%
  mutate(MedalType = factor(MedalType, levels = c("Bronze", "Silver", "Gold")))

ggplot(top10_athletes, aes(x = reorder(Athlete, Total), y = Count, fill = MedalType)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), 
            position = position_stack(vjust = 0.5, reverse = TRUE),
            color = "white", size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Gold" = "#FFD700", 
                               "Silver" = "#C0C0C0", 
                               "Bronze" = "#CD7F32")) +
  labs(title = "ТОП-10 АМЕРИКАНСКИХ ТЕННИСИСТОВ",
       subtitle = "По количеству олимпийских медалей",
       x = NULL,
       y = "Количество медалей",
       fill = "Тип медали",
       caption = "Данные: Олимпийские игры (исторические данные)") +
  theme_economist(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(color = "gray50", size = 10)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))



# 4. Функциональный график (тенденция призовых мест за последние 30 лет по полу)
last_30_years <- usa_tennis %>% filter(Year >= (max(Year) - 30))

last_30_years %>%
  count(Year, Gender) %>%
  ggplot(aes(x = Year, y = n, color = Gender)) +
  geom_line(size = 1) + geom_point(size = 2) +
  scale_color_manual(values = c("Мужчины" = "blue", "Женщины" = "red")) +
  labs(title = "Тенденция призовых мест США по полу (последние 30 лет)",
       x = "Год", y = "Количество медалей") +
  theme_minimal()

# 5. Графики изменения достижений по топ-7 странам за последние 6 Олимпиад
last_6_olympics <- data %>%
  filter(Sport == "Tennis", Year >= (max(Year) - 24))


# Фильтрация данных по теннису за последние 6 Олимпиад
tennis_data <- data %>% filter(Sport == "Tennis")
last_6_years <- sort(unique(tennis_data$Year), decreasing = TRUE)[1:6]
last_6_data <- tennis_data %>% filter(Year %in% last_6_years)

# 6. Топ-7 стран по золотым медалям (за весь период)
top7_gold_countries <- last_6_data %>%
  filter(Medal == "Gold") %>%
  count(Country, sort = TRUE) %>%
  head(7) %>%
  pull(Country)

# Подготовка данных для графика
gold_medals <- last_6_data %>%
  filter(Medal == "Gold", Country %in% top7_gold_countries) %>%
  count(Year, Country, name = "Gold_Count") %>%
  complete(Year = last_6_years, Country = top7_gold_countries, fill = list(Gold_Count = 0))

# Создание графика с улучшенным отображением
p_gold <- ggplot(gold_medals, aes(x = Year, y = Gold_Count, color = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = ifelse(Gold_Count > 0, Gold_Count, "")), 
                  size = 4, show.legend = FALSE) +
  scale_x_continuous(breaks = last_6_years) +
  scale_y_continuous(limits = c(0, max(gold_medals$Gold_Count) + 1)) +
  labs(title = "Динамика золотых медалей по теннису (Топ-7 стран)",
       subtitle = "Последние 6 Олимпиад",
       x = "Год", y = "Количество золотых медалей",
       color = "Страна") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                                "#FF7F00", "#A65628", "#F781BF"))
# . Топ-7 стран по всем призовым местам (за весь период)

top7_prize_countries <- last_6_data %>%
  count(Country, sort = TRUE) %>%
  head(7) %>%
  pull(Country)

# Подготовка данных с заполнением нулями
all_medals <- last_6_data %>%
  filter(Country %in% top7_prize_countries) %>%
  count(Year, Country, name = "Total_Medals") %>%
  complete(Year = last_6_years, Country = top7_prize_countries, 
           fill = list(Total_Medals = 0))

# Создание единой цветовой схемы
medal_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                  "#9467bd", "#8c564b", "#e377c2")


p_total <- ggplot(all_medals, aes(x = Year, y = Total_Medals, 
                                  color = Country, group = Country)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  geom_label_repel(
    aes(label = ifelse(Total_Medals > 0, Total_Medals, "")),
    size = 3.5,
    box.padding = 0.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = last_6_years,
    labels = last_6_years,
    expand = expansion(mult = 0.1)
  ) +
  scale_y_continuous(
    limits = c(0, max(all_medals$Total_Medals) + 1),
    breaks = pretty_breaks()
  ) +
  scale_color_manual(values = medal_colors) +
  labs(
    title = "Динамика всех медалей в теннисе",
    subtitle = "Топ-7 стран по общему количеству наград",
    x = "Олимпийский год",
    y = "Количество медалей",
    color = "Страна"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    axis.title = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 1))



# Для объединения с графиком по золотым медалям
p_combined <- p_gold / p_total +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

p_combined <- p_combined + plot_annotation(
  #title = "Достижения топ-7 стран в теннисе",
  #subtitle = "Золотые и призовые медали за последние 6 Олимпиад",
  tag_levels = 'A',
  theme = theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey40")
  )
)

print(p_combined)

# 7. Динамика призовых мест мужчин и женщин США за последние 6 Олимпиад
usa_last_6 <- usa_tennis %>% filter(Year >= (max(Year) - 24))

p_gender_dyn <- usa_last_6 %>%
  count(Year, Gender) %>%
  ggplot(aes(x = Year, y = n, color = Gender)) +
  geom_line(size = 1) + geom_point(size = 2.5) +
  scale_color_manual(values = c("Мужчины" = "blue", "Женщины" = "red")) +
  labs(title = "Динамика призовых мест США (теннис, мужчины и женщины)",
       x = "Год", y = "Количество медалей") +
  theme_minimal()

# Круговая диаграмма призовых мест мужчин и женщин США за последние 6 Олимпиад
usa_last_6_pie <- usa_last_6 %>%
  count(Gender)

usa_last_6_pie %>%
  ggplot(aes(x = "", y = n, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("Мужчины" = "blue", "Женщины" = "red")) +
  labs(title = "Распределение призовых мест США по полу (теннис, последние 6 Олимпиад)") +
  theme_void()



