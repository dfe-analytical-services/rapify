
library(dplyr)

df_2025 <- read.csv("attendance_persistent_absence_2025_week29.csv")
df_2026 <- read.csv("attendance_persistent_absence_2026_week4.csv")

df_joined <- bind_rows(df_2026, df_2025)
df_2026
df_2025

# View the result
head(df_joined)
View(df_joined)     


write.csv(df_joined, "persistent_absence_2026_week4.csv", row.names = FALSE)
