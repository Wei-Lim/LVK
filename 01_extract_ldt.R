### EXTRACT LDT INFORMATIONS ----

# 1.0 LIBRARIES ----
library(tidyverse)


# 2.0 IMPORT LDT-DATA ----

# 2.1 FILE IMPORT ----
files_path <- list.files(
	path        = "00_data/ldt_test/", 
	pattern     = c(".ldt$"), 
	full.names  = TRUE, 
	recursive   = TRUE,
	ignore.case = TRUE
)

# Select one file to test function
file <- files_path[4]

# 2.2 DATA EXTRACTION ----

extract_angle_data <- function(C, gamma, Isym, data_sub) {
	
	C        <- if_else(Isym != "3", C, C + 90)
	I        <- data_sub[1:Ng] %>% as.numeric()
	# data_sub <- data_sub[-(1:Ng)]
	tbl      <- tibble(gamma, C, I)
	
	return(tbl)
}

ldt <- files_path[1]

extract_ldt_data <- function(ldt) {
	
	filename <- basename(ldt) %>% tools::file_path_sans_ext()
	lines    <- readLines(ldt)
	header   <- lines[1:42]
	data     <- lines[-(1:42)]
	
	# 2.2.1 Header data ----
	# EULUMDAT description see https://de.wikipedia.org/wiki/EULUMDAT
	# Art der Symmetrie Isym
	Isym <- header[3]
	# Anzahl Mc der C-Ebenen zwischen 0° und 360°
	Mc   <- header[4] %>% as.numeric()
	# Winkelintervall Dc zwischen den C-Ebenen
	#   (Dc = 0 für nicht-äquidistante C-Ebenen)
	Dc   <- header[5] %>% as.numeric()
	# Anzahl Ng der Lichtstärkewerte in jeder C-Ebene
	Ng   <- header[6] %>% as.numeric()
	# Winkelintervall Dg <- zwischen den Lichtstärke werten einer C-Ebene
	#   (Dg = 0 für nicht-äquidistante Lichtstärkewerte in einer C-Ebene)
	Dg   <- header[7] %>% as.numeric()
	
	flux_total <- header[29] %>% as.numeric()
	power      <- header[32] %>% as.numeric()
	
	# 2.2.2 Body data ----
	# * Extracting angle definitions ----
	C_angle <- data[1:Mc] %>% as.numeric()
	data    <- data[-(1:Mc)]
	G_angle <- data[1:Ng] %>% as.numeric()
	data    <- data[-(1:Ng)]
	
	
	
	# * Extracting light intensity ----
	
	
	# Icol defined by LDT symmetry
	switch(
		Isym, 
		`0` = {
			print(paste(Isym, ": keine Symmetrie"))
			
			Icol <- Mc
			plot_tbl <-  tibble(C_angle = C_angle[1:Icol]) %>% 
				# Iterate over C_angle
				mutate(tbl = map(C_angle, extract_angle_data, gamma = G_angle, Isym = Isym, data_sub = data)) %>% 
				unnest(tbl) %>% 
				select(-C_angle)
		},
		`1` = {
			print(paste(Isym, ": Symetrie zur vertikalen Achse"))
			
			Icol <- 1
			sub_tbl <-  tibble(C_angle = C_angle[1:Icol]) %>% 
				# Iterate over C_angle
				mutate(tbl = map(C_angle, extract_angle_data, gamma = G_angle, Isym = Isym, data_sub = data)) %>% 
				unnest(tbl) %>% 
				select(-C_angle)
			
			for (angle in seq(Dc, 360, Dc)) {
				plot_tbl <- sub_tbl %>% 
					mutate(C = angle) %>% 
					bind_rows(sub_tbl, .)
			}
		},
		`2` = {
			print(paste(Isym, ": Symmetrie zur Ebene C0-C180" =", Isym"))
			
			Icol <- Mc / 2 + 1
			sub_tbl <-  tibble(C_angle = C_angle[1:Icol]) %>% 
				# Iterate over C_angle
				mutate(tbl = map(C_angle, extract_angle_data, gamma = G_angle, Isym = Isym, data_sub = data)) %>% 
				unnest(tbl) %>% 
				select(-C_angle)
			
			plot_tbl <- sub_tbl %>% 
				mutate(C = 360 - C) %>% 
				bind_rows(sub_tbl, .) %>% 
				arrange(C) %>% 
				filter(C != 360)
		},
		`3` = {
			print(paste(Isym, ": Symmetrie zur Ebene C90-C270"))
			
			Icol <- Mc / 2 + 1
			sub_tbl <-  tibble(C_angle = C_angle[1:Icol]) %>% 
				# Iterate over C_angle
				mutate(tbl = map(C_angle, extract_angle_data, gamma = G_angle, Isym = Isym, data_sub = data)) %>% 
				unnest(tbl) %>% 
				select(-C_angle)
			
			plot_tbl <- sub_tbl %>% 
				mutate(
					C = case_when(
						C <= 180 ~ 180 - C,
						C >  180 ~ 270 + (270 - C)
					)
				) %>%
				bind_rows(df_I, .) %>%
				arrange(C)
		},
		`4` = {
			print(paste(Isym, ": Symmetrie zu den Ebenen C0-C180 und C90-C270"))
			
			Icol <- Mc / 4 + 1
			sub_tbl <-  tibble(C_angle = C_angle[1:Icol]) %>% 
				# Iterate over C_angle
				mutate(tbl = map(C_angle, extract_angle_data, gamma = G_angle, Isym = Isym, data_sub = data)) %>% 
				unnest(tbl) %>% 
				select(-C_angle)
			
			sub_tbl <- sub_tbl %>% 
				mutate(C = 180 - C) %>%
				bind_rows(sub_tbl) %>%
				arrange(C)
			
			plot_tbl <- sub_tbl %>% 
				mutate(C = 360 - C) %>% 
				bind_rows(sub_tbl) %>% 
				arrange(C) %>% 
				filter(C != 360)
		}
		
	)
	
	return(plot_tbl)
	
}

# * Plotting polar graph ----

plot_polar_graph <- function(data, line_size = 1, title = filename) {
	
	# Define PRACHT colors
	pracht_green <- rgb(188, 207, 3, max = 255)
	
	data %>% 
		
		# Data wrangling
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
		) %>% 
		rename(gamma_plot = gamma) %>% 
		
		# Plot
		ggplot(aes(x = gamma_plot, y = I, linetype = C)) +
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
			legend.position = "bottom",
			legend.box.spacing = unit(0, "mm")
		) +
		labs(
			title = title,
			x = expression(gamma ~ "in °"),
			y = expression(paste("I in cd/1000 lm"))
		)
	
}

# * Perform functions ----

extract_ldt_data(files_path[2]) %>% 
	plot_polar_graph()


# 2.3 CREATE SVG-FILES ----

svg("test.svg")
plot_tbl %>% plot_polar_graph()
dev.off()






