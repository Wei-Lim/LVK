library(tidyverse)
library(ggplot2)

# Extracting from lpm-file
# dir_path <- choose.dir(
# 	"H:/Projekte/RStudio/LVK",
# 	#"T:/Leuchten",
# 	caption = "Ordner mit *.txt EMV-Dateien auswählen."
# )
dir_path <- "H:/Projekte/RStudio/LVK"
files_path <- list.files(
	path = dir_path, 
	pattern = c(".ldt$"), 
	full.names = TRUE, 
	recursive = TRUE,
	ignore.case = TRUE
)

file <- files_path[4]

lines <- readLines(file)
header <- lines[1:42]
data <- lines[-(1:42)]

# Art der Symmetrie Isym
Isym <- header[3]
# Anzahl Mc der C-Ebenen zwischen 0° und 360°
Mc <- header[4] %>% as.numeric()
# Winkelintervall Dc zwischen den C-Ebenen
#   (Dc = 0 für nicht-äquidistante C-Ebenen)
Dc <- header[5] %>% as.numeric()
# Anzahl Ng der Lichtstärkewerte in jeder C-Ebene
Ng <- header[6] %>% as.numeric()
# Winkelintervall Dg <- zwischen den Lichtstärke werten einer C-Ebene
#   (Dg = 0 für nicht-äquidistante Lichtstärkewerte in einer C-Ebene)
Dg <- header[7] %>% as.numeric()

flux_all <- header[29] %>% as.numeric()
power <- header[32] %>% as.numeric()

C_angle <- data[1:Mc] %>% as.numeric()
data <- data[-(1:Mc)]
G_angle <- data[1:Ng] %>% as.numeric()
data <- data[-(1:Ng)]

switch(
	Isym, 
	`0` = Icol <- Mc,
	`1` = Icol <- 1,
	`2` = Icol <- Mc / 2 + 1,
	`3` = Icol <- Mc / 2 + 1,
	`4` = print("Script wasn't tested on this symmetry type!")
)

df_I <- data.frame(gamma = numeric(), C = numeric(), I = numeric())
data_sub <- data
for (angle in C_angle[1:Icol]) {
	gamma <- G_angle
	C <- if_else(Isym != "3", angle, angle + 90)
	I <- data_sub[1:Ng] %>% as.numeric()
	data_sub <- data_sub[-(1:Ng)]
	df_I <- data.frame(gamma, C, I) %>% 
		bind_rows(df_I, .)
}

# data transformations
df_sub <- df_I
switch(
	Isym,
	`0` = {
		print(paste("keine Symmetrie =", Isym))
	},
	`1` = {
		print(paste("Symmetrie zur vertikalen Achse =", Isym))
		for (angle in seq(Dc, 360, Dc)) {
			df_I <- df_sub %>% 
				mutate(C = angle) %>% 
				bind_rows(df_I, .)
		}
	},
	`2` = {
		print(paste("Symmetrie zur Ebene C0-C180 =", Isym))
		df_I <- df_sub %>% 
			mutate(C = 360 - C) %>% 
			bind_rows(df_I, .) %>% 
			arrange(C) %>% 
			filter(C != 360)
	},
	`3` = {
		print(paste("Symmetrie zur Ebene C90-C270 =", Isym))
		df_I <- df_sub %>% 
			mutate(
				C = case_when(
					C <= 180 ~ 180 - C,
					C >  180 ~ 90 + C
				)
			) %>% 
			bind_rows(df_I, .) %>% 
			#mutate(C = case_when(C >= 180 ~ C - 180, C < 180 ~ C + 180)) %>% 
			arrange(C)
		
		# C_ang <- df_I$C
		# df_I <- df_I %>% 
		# 	mutate(C = case_when(C >= 180 ~ C_ang - 180, C < 180 ~ C_ang + 180)) %>%
		# 	arrange(C)
	},
	`4` = {
		print(paste("Symmetrie zu den Ebenen C0-C180 und C90-C270 =", Isym))
	}
)


dat <- df_I %>% 
	filter(C == 0 | C == 90 | C == 180 | C == 270) %>% 
	mutate(
		gamma = case_when(
			C == 0   | C == 90  ~ 360 - gamma, # transfrom for plotting
			C == 180 | C == 270 ~ gamma, 
		),
		C = case_when(
			C == 0  | C == 180 ~ "C0/180",
			C == 90 | C == 270 ~ "C90/270"
		),
		C = factor(C, levels = c("C0/180", "C90/270"))
	)

# Plotting polar graph
pracht_green <- rgb(188, 207, 3, max = 255)
palette1 <- rep(pracht_green, times = length(unique(df_I$C)))

ggplot(dat, aes(x = gamma, y = I, linetype = C)) +
	geom_line(color = pracht_green, size = 1) +
	theme_light() + 
	scale_x_continuous(
		limits = c(0, 360),
		breaks = seq(0, 330, 30),
		labels = c("0", "30", "60", "90", "120", "150", "180", "150", "120", "90",
							 "60", "30")
	) +
	coord_polar(start = pi) +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.box.spacing = unit(0, "mm")
	) +
	labs(
		x = expression(gamma ~ "in °"),
		y = expression(paste("I in cd/1000 lm"))
	)

