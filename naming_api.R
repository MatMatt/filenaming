library(plumber)

# Function to generate Sentinel-like filenames
generate_sentinel_filename <- function(satellite, mode = NA, product_type, resolution = NA, 
                                       level = NA, sensing_start, sensing_end = NA, 
                                       orbit_number, tile_id = NA, data_take_id = NA, 
                                       cycle_number = NA, processing_center = NA, 
                                       processing_baseline, product_discriminator, instrument = NA) {
  
  satellite <- toupper(satellite)
  product_type <- toupper(product_type)
  level <- if (!is.na(level)) toupper(level) else NA
  mode <- if (!is.na(mode)) toupper(mode) else NA
  resolution <- if (!is.na(resolution)) toupper(resolution) else NA
  instrument <- if (!is.na(instrument)) toupper(instrument) else NA
  orbit_number <- sprintf("R%03d", as.integer(orbit_number))
  cycle_number <- if (!is.na(cycle_number)) sprintf("C%03d", as.integer(cycle_number)) else NA
  processing_baseline <- toupper(processing_baseline)
  processing_center <- if (!is.na(processing_center)) toupper(processing_center) else NA
  tile_id <- if (!is.na(tile_id)) toupper(tile_id) else NA
  data_take_id <- if (!is.na(data_take_id)) toupper(data_take_id) else NA
  
  sensing_start <- format(as.POSIXct(sensing_start, format="%Y-%m-%d %H:%M:%S"), "%Y%m%dT%H%M%S")
  if (!is.na(sensing_end)) {
    sensing_end <- format(as.POSIXct(sensing_end, format="%Y-%m-%d %H:%M:%S"), "%Y%m%dT%H%M%S")
  }
  
  if (satellite %in% c("S1A", "S1B")) {
    filename <- paste0(satellite, "_", mode, "_", product_type, "_", resolution, "_", 
                       sensing_start, "_", orbit_number, "_", data_take_id, "_", 
                       product_discriminator)
  } else if (satellite %in% c("S2A", "S2B")) {
    filename <- paste0(satellite, "_", level, "_", product_type, "_", sensing_start, "_", 
                       processing_baseline, "_", orbit_number, "_", tile_id, "_", 
                       product_discriminator, ".SAFE")
  } else if (satellite %in% c("S3A", "S3B")) {
    filename <- paste0(satellite, "_", instrument, "_", level, "_", product_type, "_", 
                       sensing_start, "_", orbit_number, "_", cycle_number, "_", 
                       processing_center, "_", processing_baseline, "_", 
                       product_discriminator)
  } else if (satellite == "S5P") {
    filename <- paste0(satellite, "_", level, "_", product_type, "_", orbit_number, "_", 
                       sensing_start, "_", sensing_end, "_", processing_baseline, "_", 
                       product_discriminator)
  } else {
    stop("Unsupported satellite!")
  }
  
  return(filename)
}

# Verbose Function to parse a Sentinel-like filename
parse_sentinel_filename_verbose <- function(filename) {
  filename <- toupper(filename)
  
  # Sentinel-1
  if (grepl("^S1[AB]_.*", filename)) {
    parts <- strsplit(filename, "_")[[1]]
    if (length(parts) == 8) {
      return(list(
        Satellite = paste("Satellite:", parts[1], "(Sentinel-1)"),
        Mode = paste("Mode:", parts[2], "(Acquisition Mode, e.g., IW for Interferometric Wide Swath)"),
        Product_Type = paste("Product Type:", parts[3], "(e.g., GRDH for Ground Range Detected, High Resolution)"),
        Resolution = paste("Resolution:", parts[4], "(Spatial Resolution, e.g., HIGH for high resolution)"),
        Sensing_Start = paste("Sensing Start Time:", parts[5], "(Start time of the sensing operation)"),
        Orbit_Number = paste("Orbit Number:", parts[6], "(Relative orbit number, e.g.,", parts[6], ")"),
        Data_Take_ID = paste("Data Take ID:", parts[7], "(Unique identifier for the data take)"),
        Product_Discriminator = paste("Product Discriminator:", parts[8], "(Further identifies the product, e.g., processing details)")
      ))
    } else {
      return("The filename does not have the correct number of parts for Sentinel-1.")
    }
  }
  
  # Sentinel-2
  if (grepl("^S2[AB]_.*", filename)) {
    parts <- strsplit(gsub(".SAFE$", "", filename), "_")[[1]]
    if (length(parts) == 8) {
      return(list(
        Satellite = paste("Satellite:", parts[1], "(Sentinel-2)"),
        Level = paste("Product Level:", parts[2], "(e.g., MSIL1C for Level-1C top-of-atmosphere reflectances)"),
        Product_Type = paste("Product Type:", parts[3], "(e.g., TOA for top-of-atmosphere reflectance)"),
        Sensing_Start = paste("Sensing Start Time:", parts[4], "(Start time of the sensing operation)"),
        Processing_Baseline = paste("Processing Baseline:", parts[5], "(Baseline number of the processing software)"),
        Orbit_Number = paste("Orbit Number:", parts[6], "(Relative orbit number, e.g.,", parts[6], ")"),
        Tile_ID = paste("Tile Identifier:", parts[7], "(Geographical tile ID, e.g., T31TCJ)"),
        Product_Discriminator = paste("Product Discriminator:", parts[8], "(Further identifies the product, e.g., processing details)")
      ))
    } else {
      return("The filename does not have the correct number of parts for Sentinel-2.")
    }
  }
  
  # Sentinel-3
  if (grepl("^S3[AB]_.*", filename)) {
    parts <- strsplit(filename, "_")[[1]]
    if (length(parts) == 10) {
      return(list(
        Satellite = paste("Satellite:", parts[1], "(Sentinel-3)"),
        Instrument = paste("Instrument:", parts[2], "(e.g., OLCI for Ocean and Land Colour Instrument)"),
        Level = paste("Product Level:", parts[3], "(Processing level, e.g., L1 for Level 1)"),
        Product_Type = paste("Product Type:", parts[4], "(e.g., EFR for Full Resolution)"),
        Sensing_Start = paste("Sensing Start Time:", parts[5], "(Start time of the sensing operation)"),
        Orbit_Number = paste("Orbit Number:", parts[6], "(Relative orbit number, e.g.,", parts[6], ")"),
        Cycle_Number = paste("Cycle Number:", parts[7], "(Orbit cycle number, e.g.,", parts[7], ")"),
        Processing_Center = paste("Processing Center:", parts[8], "(Center where the product was processed, e.g., ESA)"),
        Processing_Baseline = paste("Processing Baseline:", parts[9], "(Baseline number of the processing software)"),
        Product_Discriminator = paste("Product Discriminator:", parts[10], "(Further identifies the product, e.g., processing details)")
      ))
    } else {
      return("The filename does not have the correct number of parts for Sentinel-3.")
    }
  }
  
  # Sentinel-5P
  if (grepl("^S5P_.*", filename)) {
    parts <- strsplit(filename, "_")[[1]]
    if (length(parts) == 8) {
      return(list(
        Satellite = paste("Satellite:", parts[1], "(Sentinel-5P)"),
        Level = paste("Product Level:", parts[2], "(e.g., L2 for Level 2)"),
        Product_Type = paste("Product Type:", parts[3], "(e.g., SO2 for sulfur dioxide)"),
        Orbit_Number = paste("Orbit Number:", parts[4], "(Relative orbit number, e.g.,", parts[4], ")"),
        Sensing_Start = paste("Sensing Start Time:", parts[5], "(Start time of the sensing operation)"),
        Sensing_End = paste("Sensing End Time:", parts[6], "(End time of the sensing operation)"),
        Processing_Baseline = paste("Processing Baseline:", parts[7], "(Baseline number of the processing software)"),
        Product_Discriminator = paste("Product Discriminator:", parts[8], "(Further identifies the product, e.g., processing details)")
      ))
    } else {
      return("The filename does not have the correct number of parts for Sentinel-5P.")
    }
  }
  
  return("Invalid or unsupported filename format.")
}

#* Generate Sentinel Filename
#* @param satellite The satellite identifier (e.g., "S1A")
#* @param mode Acquisition mode (e.g., "IW")
#* @param product_type Product type (e.g., "GRDH")
#* @param resolution Resolution (e.g., "HIGH")
#* @param level Product level (e.g., "MSIL1C")
#* @param sensing_start Sensing start time in "YYYY-MM-DD HH:MM:SS"
#* @param sensing_end Sensing end time in "YYYY-MM-DD HH:MM:SS" (only for Sentinel-5P)
#* @param orbit_number Orbit number
#* @param tile_id Tile identifier (e.g., "T31TCJ")
#* @param data_take_id Data take ID (e.g., "0001D5")
#* @param cycle_number Cycle number
#* @param processing_center Processing center (e.g., "ESA")
#* @param processing_baseline Processing baseline (e.g., "N0209")
#* @param product_discriminator Product discriminator
#* @get /generate
function(satellite, mode = NA, product_type, resolution = NA, level = NA, sensing_start, sensing_end = NA, 
         orbit_number, tile_id = NA, data_take_id = NA, cycle_number = NA, processing_center = NA, 
         processing_baseline, product_discriminator, instrument = NA) {
  
  filename <- generate_sentinel_filename(satellite, mode, product_type, resolution, level, sensing_start, 
                                         sensing_end, orbit_number, tile_id, data_take_id, cycle_number, 
                                         processing_center, processing_baseline, product_discriminator, instrument)
  return(list(filename = filename))
}

#* Parse Sentinel Filename
#* @param filename The Sentinel filename to parse
#* @get /parse
function(filename) {
  parsed <- parse_sentinel_filename_verbose(filename)
  return(parsed)
}

