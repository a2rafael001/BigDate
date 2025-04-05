library(tidyverse)
library(readr)



summer_data <- read.csv("C:/Users/rafae/Downloads/Telegram Desktop/summer.csv") 

usa_tennis_data <- summer_data %>%
  filter(tolower(Sport) == "tennis", toupper(Country) == "USA")


df_summary <- usa_tennis_data %>%
  group_by(Year) %>%
  summarise(Total = n())


y_breaks <- seq(0, max(df_summary$Total)+1, by=2)

barplot(df_summary$Total, names.arg = df_summary$Year,
        main = "Количество мест 1-8 США по теннису",
        xlab = "Год Олимпиады", ylab = "Количество мест",
        col = "steelblue", yaxt = "n")
axis(2, at=y_breaks, labels = y_breaks)

gold_medals <- usa_tennis_data %>% filter(Medal == "Gold")
medals_by_year <- table(gold_medals$Year)

pie(medals_by_year,
    main="Золотые медали США по теннису на Олимпиадах",
    labels=paste(medals_by_year, "медалей"),
    col=rainbow(length(medals_by_year)),
    clockwise=TRUE
)
legend("topright",
       legend = names(medals_by_year),
       fill = rainbow(length(medals_by_year)),
       title="Год Олимпиады"
)


create_medal_trend_plot <- function(medal_data, gender_name) {
  medal_counts <- medal_data %>%
    count(Year, Medal)
  
  gold <- filter(medal_counts, Medal == "Gold")
  silver <- filter(medal_counts, Medal == "Silver")
  bronze <- filter(medal_counts, Medal == "Bronze")
  
  y_max <- max(c(gold$n, silver$n, bronze$n), na.rm = TRUE)
  
  plot(NULL, xlim = range(medal_counts$Year), ylim = c(0, y_max),
       xlab = "Год Олимпиады", ylab = "Количество наград",
       main = paste("Динамика наград среди", gender_name, "(теннис)"))
  
  if (nrow(gold) > 0) lines(gold$Year, gold$n, type="o", col="gold", lwd=3, pch=19)
  if (nrow(silver) > 0) lines(silver$Year, silver$n, type="o", col="grey", lwd=3, pch=19)
  if (nrow(bronze) > 0) lines(bronze$Year, bronze$n, type="o", col="brown", lwd=3, pch=19)
  
  legend("topright", c("Золото", "Серебро", "Бронза"), col=c("gold", "grey", "brown"), lwd=3, pch=19)
}

recent_30_years <- max(usa_tennis_data$Year) - 30
men_medals <- usa_tennis_data %>% filter(Gender == "Men", Year >= recent_30_years)
women_medals <- usa_tennis_data %>% filter(Gender == "Women", Year >= recent_30_years)

par(mfrow=c(2,1))
create_medal_trend_plot(men_medals, "мужчин")
create_medal_trend_plot(women_medals, "женщин")




last_6_olympics <- tail(sort(unique(summer_data$Year)), 6)
df_last_6_olympics <- summer_data %>%
  filter(Year %in% last_6_olympics, Sport == "Tennis")

top_countries <- df_last_6_olympics %>%
  count(Country) %>%
  top_n(7, n) %>%
  pull(Country)

top_countries_data <- df_last_6_olympics %>%
  filter(Country %in% top_countries)


gold_summary <- top_countries_data %>%
  filter(Medal == "Gold") %>%
  count(Year, Country, name = "GoldCount") %>%
  complete(Year = last_6_olympics, Country = top_countries, fill = list(GoldCount = 0))


ggplot(gold_summary, aes(x = Year, y = GoldCount, color = Country, group = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = 0:max(gold_summary$GoldCount)) +
  labs(
    title = "Золотые медали по теннису (последние 6 Олимпиад)",
    x = "Год Олимпиады",
    y = "Количество золотых медалей",
    color = "Страна"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


usa_gender_summary <- usa_tennis_data %>%
  filter(Year %in% last_6_olympics) %>%
  count(Year, Gender)

ggplot(usa_gender_summary, aes(x=Year, y=n, color=Gender)) +
  geom_line(size=1.2) + geom_point(size=3) +
  labs(title="Динамика медалей США по теннису по полу (последние 6 Олимпиад)",
       x="Год Олимпиады", y="Количество медалей")
