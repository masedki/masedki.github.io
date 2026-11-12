library(ggplot2)
library(patchwork)

# ==============================================================================
# 1. SIMULATION DES TROIS ÉCHANTILLONS (n = 10, 100, 1000)
# ==============================================================================
set.seed(42)
theta_vrai <- 3.5

tailles <- c(10, 100, 1000)
echantillons <- lapply(tailles, function(n) rpois(n, lambda = theta_vrai))

# Grille resserrée pour bien voir l'aiguille de n = 1000 tout en gardant n = 10
theta_grid <- seq(2.2, 4.8, length.out = 1000)

calculer_courbes <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  theta_mv <- mean(x)
  
  # Information de Fisher évaluée au maximum de vraisemblance : I = n / theta_mv
  info_mv <- n / theta_mv
  var_mv  <- 1 / info_mv
  se_mv   <- sqrt(var_mv)
  
  # 1. Log-vraisemblance relative (sommet aligné à 0)
  log_lik <- -n * theta_grid + sum_x * log(theta_grid) - sum(lgamma(x + 1))
  ll_max  <- -n * theta_mv + sum_x * log(theta_mv) - sum(lgamma(x + 1))
  log_lik_rel <- log_lik - ll_max
  
  # Parabole de Taylor (montre que Fisher = courbure exacte au sommet)
  parabole_fisher <- -0.5 * info_mv * (theta_grid - theta_mv)^2
  
  # 2. Score S(theta) = l'(theta)
  score <- -n + sum_x / theta_grid
  
  # 3. Distribution asymptotique de l'EMV : N(theta_mv, 1 / I(theta_mv))
  densite_emv <- dnorm(theta_grid, mean = theta_mv, sd = se_mv)
  
  label_groupe <- sprintf("n = %d  |  Fisher I = %.1f  |  Erreur-type = %.2f", 
                          n, info_mv, se_mv)
  
  data.frame(
    theta = theta_grid,
    log_lik_rel = log_lik_rel,
    parabole_fisher = parabole_fisher,
    score = score,
    densite_emv = densite_emv,
    n_val = factor(n),
    groupe = label_groupe,
    theta_mv = theta_mv,
    info_mv = info_mv,
    se_mv = se_mv
  )
}

df_list <- lapply(echantillons, calculer_courbes)
df_all  <- do.call(rbind, df_list)

# Ordonner les facteurs dans l'ordre 10, 100, 1000
niveaux_groupe <- unique(df_all$groupe)
df_all$groupe <- factor(df_all$groupe, levels = niveaux_groupe)
emv_summary <- unique(df_all[, c("n_val", "groupe", "theta_mv", "info_mv", "se_mv")])

# Calcul de la "chute" de log-vraisemblance pour un pas de côté de Delta = +0.3
delta_pas <- 0.3
emv_summary$theta_pas <- emv_summary$theta_mv + delta_pas
emv_summary$chute_fisher <- -0.5 * emv_summary$info_mv * (delta_pas^2)

couleurs <- setNames(c("#e66101", "#1f78b4", "#33a02c"), niveaux_groupe)

# ==============================================================================
# 2. CRÉATION DES GRAPHIQUES PÉDAGOGIQUES
# ==============================================================================

# --- Graphique 1 : Log-Vraisemblance relative et Courbure de Fisher ---
p1 <- ggplot(df_all, aes(x = theta, color = groupe)) +
  # Courbe exacte de log-vraisemblance relative
  geom_line(aes(y = log_lik_rel), linewidth = 1.2) +
  # Seuil d'intervalle de confiance à 95% (-1.92)
  geom_hline(yintercept = -1.92, linetype = "dashed", color = "gray30") +
  annotate("text", x = 2.22, y = -1.5, 
           label = "Seuil de plausibilité 95% (y = -1.92) : plus Fisher est grand, plus le pic est étroit", 
           hjust = 0, size = 3.2, fontface = "italic", color = "gray20") +
  # Illustration de la chute verticale pour un écart de +0.3 autour de l'EMV
  geom_segment(data = emv_summary,
               aes(x = theta_pas, xend = theta_pas, y = 0, yend = chute_fisher, color = groupe),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.9, show.legend = FALSE) +
  geom_point(data = emv_summary, aes(x = theta_mv, y = 0), size = 3) +
  coord_cartesian(ylim = c(-8, 0.8)) +
  scale_color_manual(name = "Échantillon & Information de Fisher :", values = couleurs) +
  labs(
    title = "1. Log-Vraisemblance relative : L'Information de Fisher est la COURBURE du pic",
    subtitle = "Flèches verticales : perte de vraisemblance quand on s'écarte de +0.3 du sommet (Chute ≈ -1/2 × I × 0.3²)",
    x = NULL, y = "l(θ) - l(θ_MV)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.text = element_text(size = 9.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )

# --- Graphique 2 : Fonction Score S(theta) (La pente = -Fisher) ---
p2 <- ggplot(df_all, aes(x = theta, y = score, color = groupe)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_point(data = emv_summary, aes(x = theta_mv, y = 0), size = 3) +
  # Zoom sur [-60, 60] pour voir la cassure de pente entre n=10, 100 et 1000
  coord_cartesian(ylim = c(-60, 60)) +
  scale_color_manual(values = couleurs, guide = "none") +
  labs(
    title = "2. Fonction Score S(θ) = l'(θ) : L'Information de Fisher est la RAIDEUR de la pente en 0",
    subtitle = "À n = 1000 (vert), la droite est quasi verticale : le moindre écart autour de l'EMV crée un score gigantesque",
    x = NULL, y = "Score S(θ)"
  ) +
  theme_minimal()

# --- Graphique 3 : Conséquence concrète de Fisher = Précision de l'estimateur ---
p3 <- ggplot(df_all, aes(x = theta, y = densite_emv, color = groupe, fill = groupe)) +
  geom_area(alpha = 0.2, position = "identity", linewidth = 1.1) +
  geom_vline(xintercept = theta_vrai, linetype = "longdash", color = "black", linewidth = 0.8) +
  annotate("text", x = theta_vrai + 0.05, y = max(df_all$densite_emv) * 0.85,
           label = sprintf("Vrai θ = %.1f", theta_vrai), hjust = 0, fontface = "bold", size = 3.5) +
  scale_color_manual(values = couleurs, guide = "none") +
  scale_fill_manual(values = couleurs, guide = "none") +
  labs(
    title = "3. Rôle concret de Fisher : Précision de l'estimateur  θ_MV ~ N(θ,  1 / I(θ))",
    subtitle = "L'inverse de l'Information de Fisher (1/I) donne la variance de la cloche d'incertitude autour de l'EMV",
    x = "Paramètre θ", y = "Densité de l'EMV"
  ) +
  theme_minimal()

# Assemblage des 3 panneaux
p1 / p2 / p3