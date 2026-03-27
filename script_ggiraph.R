# Instale os pacotes caso não os tenha:
# install.packages(c("ggplot2", "ggthemes", "sf", "ggiraph", "dplyr", "patchwork", "htmltools"))
# install.packages("geobr") # Essencial para mapas do Brasil

library(ggplot2)
library(ggthemes)
library(sf)
library(ggiraph)
library(dplyr)
library(patchwork) # Necessário para o inset_element() e plot_layout()
library(geobr)

# 1. OBTER O MAPA DOS BAIRROS DO RIO DE JANEIRO
# Vamos baixar os bairros do estado do RJ e filtrar apenas o município do Rio
atlas <- read_neighborhood(year = 2010, showProgress = FALSE) |>
  filter(name_muni == "Rio De Janeiro")

# 2. SIMULAR DADOS DE IDH E POPULAÇÃO (Substitua isso pelos seus dados reais depois)
set.seed(42) # Para garantir que os dados aleatórios sejam os mesmos
atlas <- atlas |>
  mutate(
    HDI = runif(n(), 0.60, 0.98), # Gera IDH aleatório entre 0.60 e 0.98
    pop = floor(runif(n(), 10000, 150000)) # Gera população aleatória
  )

# 3. PREPARAÇÃO DOS DADOS (Agrupamento e cálculos)
pop_hdi <- atlas |>
  st_drop_geometry() |>
  mutate(
    # Cria os intervalos de IDH da mesma forma que o seu exemplo
    group_hdi = findInterval(HDI, seq(0.65, 0.95, 0.05), left.open = FALSE),
    group_hdi = factor(group_hdi)
  ) |>
  group_by(group_hdi) |>
  summarise(score = sum(pop, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = score / sum(score) * 100) |>
  na.omit() |>
  mutate(
    y_text = if_else(group_hdi %in% c(0, 7), share + 3, share - 3),
    label = paste0(round(share, 1), "%"),
    data_id = as.character(group_hdi) 
  )

atlas <- atlas |>
  mutate(group_hdi = findInterval(HDI, seq(0.65, 0.95, 0.05), left.open = FALSE))

# 4. CRIANDO O MAPA (pmap)
pmap <- ggplot(atlas) +
  geom_sf_interactive(aes(fill = HDI, data_id = group_hdi, tooltip = paste("Bairro:", name_neighborhood, "<br>HDI:", round(HDI, 3))), 
                      lwd = 0.05, color = "white") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(0.65, 0.95, 0.05),
    direction = 1,
    palette = "YlGnBu"
  ) +
  labs(
    title = "IDH nos Bairros do Rio de Janeiro",
    subtitle = "Município do Rio de Janeiro (Dados Simulados)",
    caption = "Geometria: geobr | Dados: Simulados"
  ) +
  theme_map() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 5. CRIANDO O GRÁFICO DE BARRAS (pcol)
x_labels <- c(
  "0.650 or less", "0.650 to 0.699", "0.700 to 0.749", "0.750 to 0.799",
  "0.800 to 0.849", "0.850 to 0.899", "0.900 to 0.949", "0.950 or more"
)

pcol <- ggplot(pop_hdi, aes(group_hdi, share, fill = group_hdi)) +
  geom_col_interactive(aes(data_id = data_id, tooltip = paste("Share:", label))) +
  geom_hline(yintercept = 0) +
  geom_text_interactive(
    aes(y = y_text, label = label, color = group_hdi, data_id = data_id),
    size = 2.5
  ) +
  coord_flip() +
  scale_x_discrete(labels = x_labels) +
  scale_fill_brewer(palette = "YlGnBu") +
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  guides(fill = "none", color = "none") +
  labs(title = "", x = NULL, y = NULL) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_blank(),
    aspect.ratio = 1.5
  )

# 6. JUNTANDO OS GRÁFICOS (Patchwork)
# Inserindo o gráfico de barras dentro do mapa, no canto inferior direito
p_hdi_atlas <- pmap + inset_element(pcol, left = 0.5, bottom = 0, right = 1, top = 0.5)

# 7. GERANDO A INTERATIVIDADE HTML (ggiraph)
interactive_plot <- girafe(
  ggobj = p_hdi_atlas,
  options = list(
    opts_hover(css = "fill:orange; stroke:black; stroke-width:0.5px;"),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_selection(type = "single", only_shiny = FALSE)
  )
)

# Renderizar no RStudio Viewer
interactive_plot

# Salvar como HTML
# htmltools::save_html(interactive_plot, "mapa_rio_interativo.html")