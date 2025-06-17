# Gerekli paketleri yüklüyoruz
library(modeldata)
library(tidymodels)
library(ggplot2)
library(broom)
library(dplyr)


# AMES verisini yüklüyoruz
data("ames")

head(ames)


# Basit lineer regresyon modeli
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames)

# Model özeti
summary(model1)

coef(model1)["Gr_Liv_Area"] * 100


# Artıkları ve tahminleri elde et
model1_aug <- augment(model1)

# Veriyi regresyon doğrusu ile çzimi
ggplot(model1_aug, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(title = "Gr_Liv_Area vs Sale_Price", x = "Yaşam Alanı (sqft)", y = "Satış Fiyatı ($)")

# Artıkların grafiği
ggplot(model1_aug, aes(x = Gr_Liv_Area, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "green", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Artıklar vs Gr_Liv_Area", x = "Yaşam Alanı (sqft)", y = "Artıklar")


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ------------------------------GÖREV-2--------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# Çoklu regresyon modeli
model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames)

# Model özeti
summary(model2)


set.seed(123)
cv_control <- trainControl(method = "cv", number = 10)

cv_model2 <- train(
  Sale_Price ~ Gr_Liv_Area + Year_Built,
  data = ames,
  method = "lm",
  trControl = cv_control
)

cv_model2$results




# Model - 1 için de aynısını yapalım.
cv_model1 <- train(
  Sale_Price ~ Gr_Liv_Area,
  data = ames,
  method = "lm",
  trControl = cv_control
)

cv_model1$results



# Broom ile artıklar
model1_aug <- augment(model1)
model2_aug <- augment(model2)

ggplot(model1_aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Model 1: Artıklar vs Tahmin",
    x = "Tahmin Edilen Değer",
    y = "Artık"
  ) +
  theme_minimal()

ggplot(model2_aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Model 2: Artıklar vs Tahmin",
    x = "Tahmin Edilen Değer",
    y = "Artık"
  ) +
  theme_minimal()


