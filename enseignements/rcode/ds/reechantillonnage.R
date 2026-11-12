library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. SIMULATION MONTE-CARLO DES COURBES (B = 50 ré-échantillonnages)
# ==============================================================================
set.seed(123)
theta_vrai <- 3.5
B <- 50 # Nombre de courbes ré-échantillonnées par taille n
tailles <- c(10, 100, 1000)
theta_grid <- seq(2.0, 5.2, length.out = 400)

res_courbes <- list()
res_emv <- list()
compteur <- 1

for (n in tailles) {
  info_theo <- n / theta_vrai
  label_n <- sprintf("n = %d\nFisher théorique I(θ₀) = %.1f", n, info_theo)
  
  for (b in 1:B) {
    # Tirage d'un nouvel échantillon de taille n
    x <- rpois(n, lambda = theta_vrai)
    sum_x <- sum(x)
    theta_mv <- mean(x)
    
    # Log-vraisemblance relative (sommet à 0 pour comparer la forme et la position)
    ll <- -n * theta_grid + sum_x * log(theta_grid)
    ll_max <- -n * theta_mv + sum_x * log(theta_mv)
    ll_rel <- ll - ll_max
    
    # Fonction Score S(theta) = l'(theta) = -n + sum(x)/theta
    score <- -n + sum_x / theta_grid
    
    # Valeur exacte du Score au VRAI paramètre theta_0 = 3.5
    score_en_theta0 <- -n + sum_x / theta_vrai
    
    res_courbes[[compteur]] <- data.frame(
      theta = theta_grid,
      ll_rel = ll_rel,
      score = score,
      sim_id = b,
      n_val = n,
      groupe = label_n
    )
    
    res_emv[[compteur]] <- data.frame(
      sim_id = b,
      n_val = n,
      groupe = label_n,
      theta_mv = theta_mv,
      score_theta0 = score_en_theta0
    )
    compteur <- compteur + 1
  }
}

df_courbes <- do.call(rbind, res_courbes)
df_emv <- do.call(rbind, res_emv)

# Ordonner les facettes dans l'ordre n = 10, 100, 1000
niveaux <- unique(df_courbes$groupe)
df_courbes$groupe <- factor(df_courbes$groupe, levels = niveaux)
df_emv$groupe <- factor(df_emv$groupe, levels = niveaux)

# Calcul des statistiques empiriques sur les 50 ré-échantillonnages pour annotation
stats_emp <- do.call(rbind, lapply( split(df_emv, df_emv$groupe), function(d) {
  n <- unique(d$n_val)
  data.frame(
    groupe = unique(d$groupe),
    var_emv_emp = var(d$theta_mv),
    var_emv_theo = theta_vrai / n,
    var_score_emp = var(d$score_theta0),
    fisher_theo = n / theta_vrai
  )
}))

# ==============================================================================
# 2. GRAPHIQUE 1 : RÉ-ÉCHANTILLONNAGE DES LOG-VRAISEMBLANCES & DISPERSION DES EMV
# ==============================================================================
p1 <- ggplot() +
  # Les 50 courbes de log-vraisemblance ré-échantillonnées
  geom_line(data = df_courbes, aes(x = theta, y = ll_rel, group = sim_id),
            color = "#1f78b4", alpha = 0.22, linewidth = 0.6) +
  # Vrai paramètre theta_0 = 3.5
  geom_vline(xintercept = theta_vrai, linetype = "dashed", color = "black", linewidth = 0.8) +
  # Les 50 sommets (Maximums de vraisemblance) alignés en y = 0
  geom_jitter(data = df_emv, aes(x = theta_mv, y = 0),
              color = "#e31a1c", alpha = 0.7, size = 2, height = 0.15, width = 0) +
  # Annotation de la dispersion horizontale des EMV
  geom_label(data = stats_emp,
             aes(x = 3.5, y = -8.5,
                 label = sprintf("Dispersion des sommets rouges :\nVar(θ_MV) ≈ 1 / I(θ₀) = %.4f\n(Observée sur 50 tirages : %.4f)",
                                 var_emv_theo, var_emv_emp)),
             size = 3, fill = "white", color = "#b10026", fontface = "bold") +
  coord_cartesian(ylim = c(-10, 0.8)) +
  facet_wrap(~ groupe, ncol = 3) +
  labs(
    title = "1. Ré-échantillonnage de la Log-Vraisemblance (50 tirages) : Courbure et dispersion du Maximum",
    subtitle = "Points rouges = les 50 Maximums de Vraisemblance (θ_MV). Plus la courbure (Fisher) est forte, moins les sommets bougent horizontalement.",
    x = NULL, y = "Log-vraisemblance relative l(θ) - l(θ_MV)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10.5))

# ==============================================================================
# 3. GRAPHIQUE 2 : RÉ-ÉCHANTILLONNAGE DU SCORE & IDENTITÉ DE FISHER Var(S(θ₀)) = I(θ₀)
# ==============================================================================
p2 <- ggplot() +
  # Les 50 courbes de Score S(theta)
  geom_line(data = df_courbes, aes(x = theta, y = score, group = sim_id),
            color = "#33a02c", alpha = 0.22, linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_vline(xintercept = theta_vrai, linetype = "dashed", color = "black", linewidth = 0.8) +
  # Points rouges : où chaque courbe coupe Score = 0 (c'est l'EMV !)
  geom_point(data = df_emv, aes(x = theta_mv, y = 0),
             color = "#e31a1c", alpha = 0.6, size = 1.8) +
  # Points violets : valeurs du Score en theta_0 = 3.5 (dispersion verticale = Fisher !)
  geom_point(data = df_emv, aes(x = theta_vrai, y = score_theta0),
             color = "#6a3d9a", alpha = 0.75, size = 2.2) +
  # Annotation de la variance verticale du Score
  geom_label(data = stats_emp,
             aes(x = 3.5, y = -95,
                 label = sprintf("Dispersion verticale des points violets :\nVar[ S(θ₀) ] = I(θ₀) = %.1f\n(Observée sur 50 tirages : %.1f)",
                                 fisher_theo, var_score_emp)),
             size = 3, fill = "white", color = "#6a3d9a", fontface = "bold") +
  coord_cartesian(ylim = c(-120, 120)) +
  facet_wrap(~ groupe, ncol = 3) +
  labs(
    title = "2. Ré-échantillonnage du Score S(θ) = l'(θ) : L'Information de Fisher est la variance du Score en θ₀",
    subtitle = "Points rouges (sur y = 0) : passage par zéro = θ_MV  |  Points violets (sur x = 3.5) : Score au vrai paramètre S(θ₀), dont la variance vaut I(θ₀)",
    x = "Paramètre θ (Ligne pointillée verticale = vrai paramètre θ₀ = 3.5)",
    y = "Fonction Score S(θ)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10.5))

# Affichage combiné
p1 / p2