model1 <- lm(mpg ~ 1, data = mtcars)
# model2 <- lm(mpg ~ disp, data = mtcars)
model3 <- lm(mpg ~ disp + hp, data = mtcars)
# model4 <- lm(mpg ~ disp * hp, data = mtcars)
model5 <- lme4::lmer(mpg ~ 1 + (1 | gear), data = mtcars)
# model6 <- lme4::lmer(mpg ~ hp + (1 | gear), data = mtcars)
model7 <- lme4::lmer(mpg ~ hp + (1 | gear) + (1 | carb), data = mtcars)

