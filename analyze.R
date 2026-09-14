library(datarium)
data(marketing)

cat("=== DATA STRUCTURE ===\n")
str(marketing)
cat("\n=== DIMENSIONS ===\n")
dim(marketing)
cat("\n=== FIRST 6 ROWS ===\n")
print(head(marketing))
cat("\n=== MISSING VALUES ===\n")
print(colSums(is.na(marketing)))
cat("\n=== DUPLICATE ROWS ===\n")
print(sum(duplicated(marketing)))

cat("\n=== SUMMARY STATISTICS ===\n")
print(summary(marketing))
cat("\n=== STANDARD DEVIATIONS ===\n")
print(sapply(marketing, sd))
cat("\n=== CORRELATION MATRIX ===\n")
cor_mat <- cor(marketing)
print(round(cor_mat, 3))
# Save correlation plot
png("/home/workdir/artifacts/analysis/correlation_pairs.png", width=800, height=800)
pairs(marketing, main="Scatterplot Matrix of Marketing Variables",
 pch=19, col=rgb(0,0.4,0.7,0.5))
dev.off()

png("/home/workdir/artifacts/analysis/histograms.png", width=1000, height=800)
par(mfrow=c(2,2))
hist(marketing$youtube, main="YouTube Budget", xlab="Budget ($000)", col="steelblue", border="white")
hist(marketing$facebook, main="Facebook Budget", xlab="Budget ($000)", col="steelblue", border="white")
hist(marketing$newspaper, main="Newspaper Budget", xlab="Budget ($000)", col="steelblue", border="white")
hist(marketing$sales, main="Sales", xlab="Sales (000 units)", col="darkgreen", border="white")
dev.off()

png("/home/workdir/artifacts/analysis/boxplots.png", width=1000, height=600)
par(mfrow=c(1,4))
boxplot(marketing$youtube, main="YouTube", col="steelblue")
boxplot(marketing$facebook, main="Facebook", col="steelblue")
boxplot(marketing$newspaper, main="Newspaper", col="steelblue")
boxplot(marketing$sales, main="Sales", col="darkgreen")
dev.off()


model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
cat("\n=== MULTIPLE LINEAR REGRESSION RESULTS ===\n")
print(summary(model))
cat("\n=== ANOVA TABLE ===\n")
print(anova(model))
# Confidence intervals
cat("\n=== 95% CONFIDENCE INTERVALS FOR COEFFICIENTS ===\n")
print(confint(model, level=0.95))

png("/home/workdir/artifacts/analysis/residual_plots.png", width=1000, height=800)
par(mfrow=c(2,2))
plot(model)
dev.off()

cat("\n=== SHAPIRO-WILK TEST FOR RESIDUAL NORMALITY ===\n")
sw <- shapiro.test(residuals(model))
print(sw)

cat("\n=== RESIDUALS VS FITTED CORRELATION (approx heteroscedasticity check) ===\n")
print(cor(fitted(model), abs(residuals(model))))

vif_calc <- function(model) {
 X <- model.matrix(model)[,-1]
 vifs <- numeric(ncol(X))
 names(vifs) <- colnames(X)
 for(i in 1:ncol(X)) {
 r2 <- summary(lm(X[,i] ~ X[,-i]))$r.squared
 vifs[i] <- 1 / (1 - r2)
 }
 vifs
}
cat("\n=== VARIANCE INFLATION FACTORS (VIF) ===\n")
print(vif_calc(model))

model2 <- lm(sales ~ youtube + facebook, data = marketing)
cat("\n=== REDUCED MODEL (youtube + facebook) ===\n")
print(summary(model2))
cat("\n=== MODEL COMPARISON (ANOVA) ===\n")
print(anova(model2, model))

# inteeractive model
model_int <- lm(sales ~ youtube * facebook + newspaper, data = marketing)
cat("\n=== MODEL WITH YOUTUBE x FACEBOOK INTERACTION ===\n")
print(summary(model_int))
model_final <- lm(sales ~ youtube * facebook, data = marketing)
cat("\n=== FINAL MODEL: youtube * facebook ===\n")
print(summary(model_final))
cat("\n=== 95% CI FINAL MODEL ===\n")
print(confint(model_final))

png("/home/workdir/artifacts/analysis/residual_plots_final.png", width=1000, height=800)
par(mfrow=c(2,2))
plot(model_final)
dev.off()
cat("\n=== SHAPIRO-WILK FINAL MODEL ===\n")
print(shapiro.test(residuals(model_final)))

cat("\n=== STANDARDIZED COEFFICIENTS (approx) ===\n")

std_coef <- function(model) {
 b <- coef(model)[-1]
 sdy <- sd(model$model[[1]])
 sdx <- apply(model$model[-1], 2, sd)
 b * sdx / sdy
}
print(std_coef(model2))

sink("/home/workdir/artifacts/analysis/full_output.txt")
cat("=== FULL ANALYSIS OUTPUT ===\n\n")
cat("DATA STRUCTURE\n")
str(marketing)
cat("\nSUMMARY\n")
print(summary(marketing))
cat("\nCORRELATION\n")
print(round(cor(marketing),3))
cat("\nFULL MODEL\n")
print(summary(model))
cat("\nREDUCED MODEL\n")
print(summary(model2))
cat("\nINTERACTION MODEL\n")
print(summary(model_final))
cat("\nCONFINT FINAL\n")
print(confint(model_final))
cat("\nVIF FULL\n")
print(vif_calc(model))
cat("\nSHAPIRO FULL\n")
print(shapiro.test(residuals(model)))
cat("\nSHAPIRO FINAL\n")
print(shapiro.test(residuals(model_final)))
sink()
cat("\nAnalysis complete. Plots and output saved.\n")