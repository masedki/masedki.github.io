library(ggplot2)
library(gganimate)
library(gifski)

# ==============================================================================
# 1. SIMULATION DES B ÉCHANTILLONS (n = 10, 100, 1000)
# ==============================================================================
set.seed(123)
theta_vrai <- 3.5
B <- 40 # Nombre de tirages successifs animés
tailles <- c(10, 100, 1000)
theta_grid <- seq(2.2, 4.8, length.out = 200)

res_courbes <- list()
res_points  <- list()
k <- 1

for (n in tailles) {
  info_theo <- n / theta_vrai
  label_n <- sprintf("n = %d  (Fisher I = %.1f)", n, info_theo)
  
  for (b in 1:B) {
    x <- rpois(n, lambda = theta_vrai)
    sum_x <- sum(x)
    theta_mv <- mean(x)
    
    # 1. Log-vraisemblance relative (au lieu de mettre NA, on bloque à -12 et on utilise coord_cartesian)
    ll_rel <- (-n * theta_grid + sum_x * log(theta_grid)) - 
      (-n * theta_mv + sum_x * log(theta_mv))
    ll_rel <- pmax(ll_rel, -12)
    
    # 2. Fonction Score S(theta) = l'(theta)
    score <- -n + sum_x / theta_grid
    score <- pmin(pmax(score, -110), 110)
    
    # Valeur du score en theta_0 = 3.5
    score_theta0 <- -n + sum_x / theta_vrai
    
    res_courbes[[k]] <- rbind(
      data.frame(
        theta = theta_grid, valeur = ll_rel,
        type = "1. Log-vraisemblance relative l(θ) - l(θ_MV)",
        groupe = label_n, sim_id = b
      ),
      data.frame(
        theta = theta_grid, valeur = score,
        type = "2. Fonction Score S(θ) = l'(θ)",
        groupe = label_n, sim_id = b
      )
    )
    
    res_points[[k]] <- rbind(
      data.frame(
        theta = theta_mv, valeur = 0,
        type = "1. Log-vraisemblance relative l(θ) - l(θ_MV)",
        nature = "EMV (θ_MV : Sommet / Score = 0)",
        groupe = label_n, sim_id = b
      ),
      data.frame(
        theta = theta_mv, valeur = 0,
        type = "2. Fonction Score S(θ) = l'(θ)",
        nature = "EMV (θ_MV : Sommet / Score = 0)",
        groupe = label_n, sim_id = b
      ),
      data.frame(
        theta = theta_vrai, valeur = score_theta0,
        type = "2. Fonction Score S(θ) = l'(θ)",
        nature = "Score en θ₀ : S(θ₀) [Variance = Fisher]",
        groupe = label_n, sim_id = b
      )
    )
    k <- k + 1
  }
}

df_courbes_base <- do.call(rbind, res_courbes)
df_points_base  <- do.call(rbind, res_points)

niveaux_n <- unique(df_courbes_base$groupe)
df_courbes_base$groupe <- factor(df_courbes_base$groupe, levels = niveaux_n)
df_points_base$groupe  <- factor(df_points_base$groupe, levels = niveaux_n)

# ==============================================================================
# 2. CONSTRUCTION DE L'HISTORIQUE CUMULÉ (Contourne le bug de shadow_mark)
# ==============================================================================
# À chaque image f (de 1 à B), on garde tous les tirages b <= f
df_courbes_anim <- do.call(rbind, lapply(1:B, function(f) {
  sub_c <- df_courbes_base[df_courbes_base$sim_id <= f, ]
  sub_c$frame_id <- f
  sub_c$est_actif <- ifelse(sub_c$sim_id == f, "Actif", "Passé")
  sub_c
}))

df_points_anim <- do.call(rbind, lapply(1:B, function(f) {
  sub_p <- df_points_base[df_points_base$sim_id <= f, ]
  sub_p$frame_id <- f
  sub_p$est_actif <- ifelse(sub_p$sim_id == f, "Actif", "Passé")
  sub_p
}))

df_zero <- data.frame(
  type = factor("2. Fonction Score S(θ) = l'(θ)", levels = unique(df_courbes_base$type)),
  yintercept = 0
)

# ==============================================================================
# 3. CRÉATION DU GRAPHIQUE ANIME (Sans shadow_mark)
# ==============================================================================
p_anim <- ggplot() +
  geom_hline(data = df_zero, aes(yintercept = yintercept), color = "gray40", linewidth = 0.5) +
  geom_vline(xintercept = theta_vrai, linetype = "dashed", color = "black", linewidth = 0.7) +
  
  # 1. Courbes passées (en bleu clair transparent)
  geom_line(
    data = subset(df_courbes_anim, est_actif == "Passé"),
    aes(x = theta, y = valeur, group = sim_id),
    color = "#1f78b4", linewidth = 0.5, alpha = 0.20
  ) +
  # 2. Courbe du tirage actif (en bleu foncé épais au premier plan)
  geom_line(
    data = subset(df_courbes_anim, est_actif == "Actif"),
    aes(x = theta, y = valeur, group = sim_id),
    color = "#08306b", linewidth = 1.2, alpha = 1
  ) +
  # 3. Points passés (accumulés)
  geom_point(
    data = subset(df_points_anim, est_actif == "Passé"),
    aes(x = theta, y = valeur, color = nature),
    size = 2.0, alpha = 0.35
  ) +
  # 4. Points du tirage actif (bien visibles)
  geom_point(
    data = subset(df_points_anim, est_actif == "Actif"),
    aes(x = theta, y = valeur, color = nature),
    size = 3.8, alpha = 1
  ) +
  scale_color_manual(
    name = "Points accumulés :",
    values = c(
      "EMV (θ_MV : Sommet / Score = 0)" = "#e31a1c",
      "Score en θ₀ : S(θ₀) [Variance = Fisher]" = "#6a3d9a"
    )
  ) +
  facet_grid(type ~ groupe, scales = "free_y") +
  labs(
    title = "Ré-échantillonnage séquentiel : Échantillon #{current_frame} sur 40",
    subtitle = "Courbe foncée = tirage actif  |  Courbes claires = mémoire des tirages précédents",
    x = "Paramètre θ (Ligne pointillée = vrai paramètre θ₀ = 3.5)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10.5),
    panel.spacing = unit(1, "lines")
  ) +
  # On anime simplement sur notre colonne frame_id !
  transition_manual(frame_id)

# ==============================================================================
# 4. RENDU DU GIF
# ==============================================================================
anim <- animate(
  p_anim,
  nframes = B,
  fps = 4,
  width = 1050,
  height = 680,
  res = 110,
  renderer = gifski_renderer()
)

anim