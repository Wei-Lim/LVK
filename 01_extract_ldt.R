### EXTRACT LDT INFORMATIONS ----

# LIBRARIES ----
library(tidyverse)
library(svglite)
library(furrr)

# 1.FILES IMPORT ----
filepath <- list.files(
	path        = "00_data/ldt_test/", 
	pattern     = c(".ldt$"), 
	full.names  = TRUE, 
	recursive   = TRUE,
	ignore.case = TRUE
)


# Select one file to test function
file <- filepath[6]

# 2 FUNCTIONS ----

# 2.1 Extract luminous intensity ----
extract_lum_intensity <- function(C, i, gamma, Ng, data) {
	
	I   <- data[((i - 1) * Ng + 1):(i * Ng)] %>% as.numeric()
	tbl <- tibble::tibble(C, gamma, I)
	
	return(tbl)
}

# 2.2 Read LDT data ----
read_ldt <- function(file) {
	
	# Read txt-lines 
	lines    <- readLines(file)
	header   <- lines[1:42]
	data     <- lines[-(1:42)]
	
	ld_list = list(
		
		## Filepath definitions
		filepath  = file,
		file_name = basename(file) %>% tools::file_path_sans_ext(),
		
		## * Header ----
		
		# EULUMDAT description 
		# see https://de.wikipedia.org/wiki/EULUMDAT
		# see https://docs.agi32.com/PhotometricToolbox/Content/Open_Tool/eulumdat_file_format.htm
		
		# Company identification/databank/version/format identification
		company = header[1],
		
		# Ityp - Type indicator
		Ityp = header[2],
		
		# Isym - Symmetry indicator
		Isym = header[3],
		
		# Mc - Number of C-planes between 0 and 360 degrees 
		Mc   = header[4] %>% as.numeric(),
		
		# Dc - Distance between C-planes
		Dc   = header[5] %>% as.numeric(),
		
		# Ng - Number of luminous intensities in each C-plane
		Ng   = header[6] %>% as.numeric(),
		
		# Dg - Distance between luminous intensities per C-plane
		Dg   = header[7] %>% as.numeric(),
		
		# Measurement report number
		report_no      = header[8],
		
		# Luminaire name
		luminaire_name = header[9],
		
		# Luminaire number
		luminaire_no   = header[10],
		
		# File name
		file_name_ldt  = header[11],
		
		# Date/user
		date_user      = header[12],
		
		# Length/diameter of luminaire (mm)
		length = header[13] %>% as.numeric(),
		
		# b - Width of luminaire (mm) (b = 0 for circular luminaire)
		width  = header[14] %>% as.numeric(),
		
		# Height of luminaire (mm)
		height = header[15] %>% as.numeric(),
		
		# Length/diameter of luminous area (mm)
		length_lum      = header[16] %>% as.numeric(),
		
		# b1 - Width of luminous area (mm) (b1 = 0 for circular luminous area of luminaire)
		width_lum       = header[17] %>% as.numeric(),
		
		# Height of luminous area C0-plane (mm)
		height_lum_C0   = header[18] %>% as.numeric(),
		
		# Height of luminous area C90-plane (mm)
		height_lum_C90  = header[19] %>% as.numeric(),
		
		# Height of luminous area C180-plane (mm)
		height_lum_C180 = header[20] %>% as.numeric(),
		
		# Height of luminous area C270-plane (mm)
		height_lum_C270 = header[21] %>% as.numeric(),
		
		# DFF - Downward flux fraction (%)
		DFF  = header[22] %>% as.numeric(),
		
		# LORL - Light output ratio luminaire (%)
		LORL = header[23] %>% as.numeric(),
		
		# Conversion factor for luminous intensities (depending on measurement)
		cf   = header[24] %>% as.numeric(),
		
		# Tilt of luminaire during measurement (road lighting luminaires)
		tilt = header[25] %>% as.numeric(),
		
		# n - Number of standard sets of lamps (optional, also extendable on company-specific basis)
		# For absolute photometry, this value is 1
		lamp_standard_sets_no = header[26] %>% as.numeric(),
		
		# Number of lamps
		# For absolute photometry, number is negative
		lamp_no               = header[27] %>% as.numeric(),
		
		# Type of lamps
		lamp_type             = header[28],
		
		# Total luminous flux of lamps (lm)
		# For absolute photometry, this field is Total Luminous Flux of Luminaire
		lum_flux = header[29] %>% as.numeric(),
		
		# Color appearance / color temperature of lamps
		cct      = header[30],
		
		# Color rendering group / color rendering index
		cri      = header[31],
		
		# Wattage including ballast (W)
		power    = header[32] %>% as.numeric(),
		
		# DR - Direct ratios for room indices k = 0.6 ... 5 (for determination of luminaire numbers according to utilization factor method)
		DR       = header[33:42] %>% as.numeric()
	)
	
	
	## * Angles definitions ----
	Mc <- ld_list$Mc
	Ng <- ld_list$Ng
	Dc <- ld_list$Dc
	Isym <- ld_list$Isym
	
	# Angles C (beginning with 0 degrees)
	angle_C <- data[1:Mc] %>% as.numeric()
	data    <- data[-(1:Mc)] # removing angles from data
	
	# Angles G (beginning with 0 degrees)
	angle_G <- data[1:Ng] %>% as.numeric()
	data    <- data[-(1:Ng)] # removing angles from data
	
	## * Iteration to extracting light intensity data according to C and G angles ----
	# luminous intensity dependent on LDT symmetry
	switch(
		Isym, 
		   
	   # 0 - no symmetry
	   `0` = {
		   	C     <- seq(0, 360 - Dc, Dc)
		   	c_tbl <- tibble::tibble(C, i = 1:length(C))
	   },
	   
	   # 1 - symmetry about the vertical axis
	   `1` = {
		   	C     <- 0
		   	c_tbl <- tibble::tibble(C, i = 1:length(C))
	   },
	   
	   # 2 - symmetry to plane C0-C180
	   `2` = {
		   	C     <- seq(0, 180, Dc)
		   	c_tbl <- tibble::tibble(C, i = 1:length(C))
	   },
	   
	   # 3 - symmetry to plane C90-C270
	   `3` = {
		   	C     <- seq(90, 270, Dc)
		   	c_tbl <- tibble::tibble(C, i = 1:length(C))
	   },
	   
	   # 4 - symmetry to plane C0-C180 and to plane C90-C270
	   `4` = {
		   	C     <- seq( 0,  90, Dc)
		   	c_tbl <- tibble::tibble(C, i = 1:length(C))
	   }
	)
	
	# Luminous intensity distribution (cd/1000 lumens)
	lum_int_tbl <- c_tbl %>% 
		dplyr::mutate(tbl = future_map2(C, i, extract_lum_intensity, gamma = angle_G, Ng = Ng, data = data)) %>% 
		dplyr::select(-C, -i) %>% 
		tidyr::unnest(tbl) %>% 
		tidyr::pivot_wider(names_from = C, names_prefix = "C", values_from = I)
	
	## * Extend luminous intensity data for plotting and calculating ----
	tbl <- lum_int_tbl %>% 
		tidyr::pivot_longer(-gamma, "C", names_prefix = "C", names_transform = as.numeric, values_to = "I")
	
	switch(
		Isym, 
		
		# 0 - no symmetry
		`0` = lum_int_extended_tbl <- tbl,
		
		# 1 - symmetry about the vertical axis
		`1` = lum_int_extended_tbl <- tbl %>% 
			dplyr::bind_rows( 
				tbl %>% dplyr::mutate(C = 180) 
			),
		
		# 2 - symmetry to plane C0-C180
		`2` = lum_int_extended_tbl <- tbl %>% 
			dplyr::bind_rows( 
				tbl %>% 
					dplyr::mutate(C = rev(C) + 180) %>%
					dplyr::filter(C != 180, C != 360)
			) %>% 
			dplyr::arrange(C),
		
		# 3 - symmetry to plane C90-C270
		`3` = lum_int_extended_tbl <- tbl %>% 
			dplyr::bind_rows(
				tbl %>% 
					dplyr::mutate(C = rev(C + 180) ) %>% 
					dplyr::mutate(C = if_else(C >= 360, C - 360, C)) %>% 
					dplyr::filter(C != 90, C != 270)
			) %>% 
			dplyr::arrange(C),
		
		# 4 - symmetry to plane C0-C180 and to plane C90-C270
		`4` = {
			tbl2 <- tbl %>% 
				dplyr::bind_rows(
					tbl %>% 
						dplyr::mutate(C = rev(C + 90)) %>% 
						dplyr::filter(C != 90)
				) %>% 
				dplyr::arrange(C) 
			
			lum_int_extended_tbl <- tbl2 %>% 
				dplyr::bind_rows(
					tbl2 %>% 
						dplyr::mutate(C = rev(C) + 180) %>% 
						dplyr::filter(C != 180, C != 360)
				) %>% 
				dplyr::arrange(C)
		}
	)
	
	lum_int_extended_tbl <- lum_int_extended_tbl %>% 
		tidyr::pivot_wider(names_from = C, names_prefix = "C", values_from = I) 
	
	
	# * Appending angles and luminous intensity tables to LDT-list ----
	ld_list <- ld_list %>% 
		append(list(
			
			angleC               = angle_C,
			angleG               = angle_G,
			lum_int_tbl          = lum_int_tbl,
			lum_int_extended_tbl = lum_int_extended_tbl,
			
			## IES additional definitions
			# Photometric testing laboratory
			test_lab = "-",
			
			# Photometry type: 1: C, 2: B, 3: C
			photometry_type = "1",
			
			# ballast factor
			ballast_factor = 1
		))
	
	return(ld_list)
}

# * Test function ----
read_ldt(filepath[1])
read_ldt(filepath[4])
read_ldt(filepath[6])
read_ldt(filepath[7])
read_ldt(filepath[8])
	

# 2.3 Plot light distribution graph ----
ld_list <- read_ldt(filepath[1])
lum_int_extended_tbl <- ld_list$lum_int_extended_tbl
line_color <- "#BCCF03"
line_size  <- 1.5 
title <- "filename"

plot_light_distribution <- function(
		lum_int_extended_tbl, 
		line_color = "#BCCF03", 
		line_size = 1.5,
		title = "", 
		x_lab = expression("Drehwinkel" ~ gamma ~ "in °"),
		y_lab = expression(paste("Normierte Lichtstärke I in cd/1000 lm"))
	) {
	
	lum_int_extended_tbl %>% 
		
		# Data wrangling
		pivot_longer(-gamma, "C", names_prefix = "C", names_transform = as.numeric, values_to = "I") %>% 
		filter(C == 0 | C == 90 | C == 180 | C == 270) %>% 
		mutate(
			gamma = case_when(
				# transform gamma for plotting
				C == 0   | C == 90  ~ 360 - gamma,
				C == 180 | C == 270 ~ gamma,
			),
			C = case_when(
				C == 0  | C == 180 ~ "C0/180",
				C == 90 | C == 270 ~ "C90/270"
			),
			C = factor(C, levels = c("C0/180", "C90/270"))
		) %>% 
		
		# Plotting
		ggplot(aes(x = gamma, y = I, linetype = C)) +
		
		# Create polar graph
		geom_line(color = line_color, size = line_size) +
		coord_polar(start = pi) +
		
		# Define special x-axis for light distribution (see gamma transformation)
		scale_x_continuous(
			limits = c(0, 360),
			breaks = seq(0, 330, 30),
			labels = c("0", "30", "60", "90", "120", "150", "180", "150", "120", "90",
					   "60", "30")
		) +
		
		# Themes and Labels
		theme_light() + 
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
			title = title,
			x = expression("Drehwinkel" ~ gamma ~ "in °"),
			y = expression(paste("Normierte Lichtstärke I in cd/1000 lm"))
		)
	
}

ld_list$lum_int_extended_tbl %>% 
	plot_light_distribution()


# 2.4 LD: Add plot to list ----

future("multisession")
tictoc::tic()
file_list <- filepath %>% map(read_ldt)
tictoc::toc()

tictoc::tic()
file_list <- filepath %>% future_map(read_ldt)
tictoc::toc()

ld_list <- file_list[[1]]

ld_add_light_distribution <- function(
		ld_list,
		line_color = "#BCCF03", 
		line_size  = 1.5,
		title      = "", 
		x_lab      = expression("Drehwinkel" ~ gamma ~ "in °"),
		y_lab      = expression(paste("Normierte Lichtstärke I in cd/1000 lm"))
) {
	
	lum_int_extended_tbl <- ld_list$lum_int_extended_tbl
	
	ld_list$plot <- plot_light_distribution(
		lum_int_extended_tbl,
		line_color = line_color, 
		line_size  = line_size,
		title      = ld_list$file_name, 
		x_lab      = x_lab,
		y_lab      = y_lab
	)
	
	return(ld_list)
}

ld_add_light_distribution(file_list[[2]])


tictoc::tic()
file_list <- map(file_list, ld_add_light_distribution, line_color = "#000000")
tictoc::toc()

file_list[[3]]$plot

# 2.5 LD: Export to SVG ----
ld_list <- file_list[[1]]

ld_write_svg <- function(ld_list, dir_path = "00_data/export/") {
	
	file_name <- ld_list$file_name
	
	svglite(str_c(dir_path, file_name, ".svg"))
	print(ld_list$plot)
	dev.off()
	
}

file_list[[1]] %>% ld_write_svg()

tictoc::tic()
map(file_list, ld_write_svg)
tictoc::toc()

tictoc::tic()
future_map(file_list, ld_write_svg)
tictoc::toc()

# 2.6 LD: Write LDT ----
ld_list <- file_list[[1]]

ld_write_ldt <- function(ld_list, dir_path = "00_data/export/", user = "") {
	
	ld_list$file_name_ldt <- str_c(ld_list$file_name, ".ldt")
	ld_list$date_user <- str_c( Sys.time() %>% format("%Y-%m-%d"), user, sep = ", " )
	
	# Select ldt header features
	ldt_header <- c(
		"company", "Ityp", "Isym", "Mc", "Dc", "Ng", "Dg", 
		"report_no", "luminaire_name", "luminaire_no", "file_name_ldt", "date_user", 
		"length", "width", "height", 
		"length_lum", "width_lum", "height_lum_C0", "height_lum_C90", "height_lum_C180", "height_lum_C270", 
		"DFF", "LORL", "cf", "tilt", 
		"lamp_standard_sets_no", "lamp_no", "lamp_type", 
		"lum_flux", "cct", "cri", "power"
	)
	
	# Convert to character array for writing lines
	ldt_export_chr <- c(
		ld_list[ldt_header] %>% as.character(),
		ld_list$DR,
		ld_list$angleC,
		ld_list$angleG,
		ld_list$lum_int_tbl %>% 
			pivot_longer(-gamma, "C", names_prefix = "C", names_transform = as.numeric, values_to = "I") %>% 
			arrange(C) %>% 
			pull(I)
	)
	
	writeLines(ldt_export_chr, str_c(dir_path, ld_list$file_name, ".ldt"))
	
	return(invisible(NULL))
	
}

tictoc::tic()
map(file_list, ld_write_ldt, user = "PIT")
tictoc::toc()

tictoc::tic()
future_map(file_list, ld_write_ldt)
tictoc::toc()



# 2.7 LD: Write IES ----
# IES file format description after ANSI/IESNA LM-63-2002

ld_list <- file_list[[1]]
dir_path <- "00_data/export/"

ld_write_ies_lm63_2002 <- function(ld_list, dir_path = "00_data/export/") {
	
	# Luminous intensity in long table format
	lum_int_extended_long_dt <- ld_list$lum_int_extended_tbl %>% 
		
		# use of data.table for better computing speed in filtering and pivot_longer
		data.table::setDT() %>% 
		
		# Pivot longer
		data.table::melt(
			measure.vars  = patterns("C"),
			variable.name = "C",
			value.name    = "I"
		)
	
	# Removing prefix from variable "C" names
	lum_int_extended_long_dt[, C := str_remove(C, "C")]
	
	# TILT: Marker for end of keywords
	tilt <- "TILT=NONE"
	
	# * Data descriptions ----
	lamp_no             <- ld_list$lamp_no
	lumens_per_lamp     <- ld_list$lum_flux / lamp_no
	candela_multiplier  <- ld_list$LORL / 100
	angle_vertical_no   <- ld_list$lum_int_extended_tbl %>% nrow()
	angle_horizontal_no <- ld_list$lum_int_extended_tbl %>% select(-gamma) %>% ncol()
	photometric_type    <- ld_list$photometry_type
	
	# units_type: luminous dimensions in feet (1) or in meters (2)
	units_type <- 2 
	width_lum  <- ld_list$width_lum
	length_lum <- ld_list$length_lum
	height_lum <- ld_list$height_lum_C0
	
	ballast_factor   <- ld_list$ballast_factor
	future_use       <- "1"
	input_watts      <- ld_list$power
	angle_vertical   <- ld_list$lum_int_extended_tbl$gamma
	angle_horizontal <- lum_int_extended_long_dt %>% 
		distinct(C) %>% 
		pull(C)
	
	candela_values <- angle_horizontal %>% 
		map_chr( function(x) {
			lum_int_extended_long_dt[C == x, .(I)][[1]] %>% 
				str_c(collapse = " ")
		})
	
	# * Checks ----
	
	# Tilted luminaire definition
	# Note: conversion to TILT= INCLUDE is not implemented
	if (ld_list$tilt != 0) tilt <- "TILT=INCLUDE"
	
	# Absolute photometry
	if (ld_list$lamp_no == "-1") lumens_per_lamp <- lamp_no
	
	# Dimensions luminous shape
	
	# Circular or vertical cylindrical
	if (ld_list$width_lum == 0) {
		width_lum  <- -ld_list$length_lum
		length_lum <- -ld_list$length_lum
	}
	
	# Luminous height definition
	# Not entirely correct solution
	if ( ld_list$height_lum_C0   != 0 | ld_list$height_lum_C90  != 0 | 
		 ld_list$height_lum_C180 != 0 | ld_list$height_lum_C270 != 0 ) {
		height_lum <- max(ld_list$height_lum_C0, ld_list$height_lum_C90, ld_list$height_lum_C180, ld_list$height_lum_C270)
	}
	
	ies_export_chr <- c(
		
		# First line distinguishes from other ies-formats
		"IESNA:LM-63-2002",
		
		# * Keywords ----
		str_c("[TEST]",      ld_list$report_no, sep = " "),
		str_c("[TESTLAB]",   ld_list$test_lab, sep = " "),
		str_c("[ISSUEDATE]", Sys.time() %>% format("%Y-%m-%d"), sep = " "),
		str_c("[MANUFAC]",   ld_list$company, sep = " "),
		str_c("[LUMCAT]",    ld_list$luminaire_no, sep = " "),
		str_c("[LUMINAIRE]", ld_list$luminaire_name, sep = " "),
		"[FILEGENINFO] created by R package lighting",
		
		tilt,
		# End of keywords
		
		str_c(lamp_no, lumens_per_lamp, candela_multiplier, 
			  angle_vertical_no, angle_horizontal_no, photometric_type, 
			  units_type, width_lum, length_lum, height_lum, sep = " "),
		str_c(ballast_factor, future_use, input_watts, sep = " "),
		str_c(angle_vertical, collapse = " "),
		str_c(angle_horizontal, collapse = " "),
		candela_values
	)
	
	writeLines(ies_export_chr, str_c(dir_path, ld_list$file_name, ".ies"))
	
}



tictoc::tic()
map(file_list, ld_write_ies_lm63_2002)
tictoc::toc()



# 2.8 LD: Update LD with myview ----

# 3.0 DUMP FUNCTIONS ----
dump(
	c(
		"extract_lum_intensity", 
		"read_ldt", 
		"plot_light_distribution",
		"ld_add_light_distribution",
		"ld_write_svg",
		"ld_write_ies_lm53_2002"
	), 
	file = "00_scripts/light_distribution.R"
)
