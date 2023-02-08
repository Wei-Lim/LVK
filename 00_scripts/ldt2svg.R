extract_c_values <-
function(.C, .n, .gamma, .Ng, .data) {
	
	I   <- .data[((.n - 1) * .Ng + 1):(.n * .Ng)] %>% as.numeric()
	tbl <- tibble(.C, .gamma, I) %>% 
		rename(C = .C, gamma = .gamma)
	
	return(tbl)
}
extract_ldt_data <-
function(.ldt) {
	
	lines    <- readLines(.ldt)
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
			C <- seq(0, 360 - Dc, Dc)
			
			c_tbl <- tibble(C, n = 1:length(C))
			
			data_raw_tbl <- c_tbl %>% 
				mutate(tbl = future_map2(C, n, extract_c_values, .gamma = G_angle, .Ng = Ng, .data = data)) %>% 
				select(-C, -n) %>% 
				unnest(tbl)
			
		},
		`1` = {
			print(paste(Isym, ": Symmetrie zur vertikalen Achse"))
			C <- 0
			
			c_tbl <- tibble(C, n = 1:length(C))
			
			data_raw_tbl_1 <- c_tbl %>% 
				mutate(tbl = future_map2(C, n, extract_c_values, .gamma = G_angle, .Ng = Ng, .data = data)) %>% 
				select(-C, -n) %>% 
				unnest(tbl)
			
			data_raw_tbl <- bind_rows(
				data_raw_tbl_1, 
				# Hinzufügen von Daten anhand der Symmetrie zur vertikalen Achse
				data_raw_tbl_1 %>% 
					mutate(C = 180)
			)
			
		},
		`2` = {
			print(paste(Isym, ": Symmetrie zur Ebene C0-C180" =", Isym"))
			
			C <- seq(0, 180, Dc)
			
			c_tbl <- tibble(C, n = 1:length(C))
			
			data_raw_tbl_1 <- c_tbl %>% 
				mutate(tbl = future_map2(C, n, extract_c_values, .gamma = G_angle, .Ng = Ng, .data = data)) %>% 
				select(-C, -n) %>% 
				unnest(tbl)
			
			data_raw_tbl <- bind_rows(
				data_raw_tbl_1,
				# Hinzufügen von Daten anhand der Symmetrie zur C0-C180-Ebene
				data_raw_tbl_1 %>% 
					mutate(C = rev(C) + 180) %>% 
					filter(C != 180, C != 360)
			)

		},
		`3` = {
			print(paste(Isym, ": Symmetrie zur Ebene C90-C270"))
			
			C <- seq(90, 270, Dc)
			
			c_tbl <- tibble(C, n = 1:length(C))
			
			data_raw_tbl_1 <- c_tbl %>% 
				mutate(tbl = future_map2(C, n, extract_c_values, .gamma = G_angle, .Ng = Ng, .data = data)) %>% 
				select(-C, -n) %>% 
				unnest(tbl)
			
			data_raw_tbl <- data_raw_tbl_1 %>% 
				bind_rows(
					data_raw_tbl_1,
					# Hinzufügen von Daten anhand der Symmetrie zur C90-C270-Ebene
					data_raw_tbl_1 %>% 
						mutate(C = rev(C + 180) ) %>% 
						mutate(C = if_else(C >= 360, C - 360, C)) %>% 
						filter(C != 90)
				) %>% 
				arrange(C)
			
		},
		`4` = {
			print(paste(Isym, ": Symmetrie zu den Ebenen C0-C180 und C90-C270"))
			
			C <- seq( 0,  90, Dc)

			c_tbl <- tibble(C, n = 1:length(C))
			
			data_raw_tbl_1 <- c_tbl %>% 
				mutate(tbl = future_map2(C, n, extract_c_values, .gamma = G_angle, .Ng = Ng, .data = data)) %>% 
				select(-C, -n) %>% 
				unnest(tbl)

			data_raw_tbl_2 <- bind_rows(
				data_raw_tbl_1, 
				# Hinzufügen von Daten anhand der Symmetrie zur C90-C270-Ebene
				data_raw_tbl_1 %>% 
					mutate(C = rev(C + 90))
			) %>% 
				arrange(C)
			
			data_raw_tbl <- bind_rows(
				data_raw_tbl_2,
				# Hinzufügen von Daten anhand der Symmetrie zur C0-C180-Ebene
				data_raw_tbl_2 %>% 
					mutate(C = rev(C) + 180) %>% 
					filter(C != 180, C != 360)
			)
			
		}
		
	)
	
	return(data_raw_tbl)
	
}
prepare_polar_data <-
function(.data) {
	
	.data %>% 
		
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
		rename(gamma_plot = gamma)
	
}
plot_polar_graph <-
function(.data, .line_size = 1.5, .title = filename, .line_color = "#BCCF03") {
	
	# Plot
	g <- ggplot(.data, aes(x = gamma_plot, y = I, linetype = C)) +
		geom_line(color = .line_color, size = .line_size) +
		theme_light() + 
		scale_x_continuous(
			limits = c(0, 360),
			breaks = seq(0, 330, 30),
			labels = c("0", "30", "60", "90", "120", "150", "180", "150", "120", "90",
					   "60", "30")
		) +
		coord_polar(start = pi) +
		theme(
			plot.title         = element_text(size = 18),
			panel.grid.minor   = element_line(size = 0.5),
			panel.grid.major   = element_line(size = 0.5),
			axis.text          = element_text(size = 12),
			axis.title.y       = element_text(vjust = 4),
			axis.title         = element_text(size = 16),
			legend.text        = element_text(size = 12),
			legend.title       = element_blank(),
			legend.position    = "bottom",
			legend.box.spacing = unit(0, "mm")
		) +
		labs(
			title = .title,
			x = expression("Drehwinkel" ~ gamma ~ "in °"),
			y = expression(paste("Normierte Lichtstärke I in cd/1000 lm"))
		)
	
	return(g)
	
}
ldt2ggplot <-
function(.ldt_filepath, .filename) {
	
	name <- tools::file_path_sans_ext(.filename)
	
	g <- extract_ldt_data(.ldt_filepath) %>% 
		prepare_polar_data() %>% 
		plot_polar_graph(.title = name)
	
	return(g)
	
}
