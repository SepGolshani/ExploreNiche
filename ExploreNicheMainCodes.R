# Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(ggplot2)     # For creating visualizations
library(Matrix)      # For matrix operations, potentially for use in models
library(lme4)        # For fitting linear mixed-effects models
library(dplyr)       # For data manipulation
library(gridExtra)   # For arranging multiple plots
library(readxl)      # For reading Excel files
library(svglite)
library(shiny)
library(ggplot2)
library(plotly)



# Load the dataset
merged_data_clean <- read.csv("/Users/sepidehgolshani/Desktop/PhD.ExploreNiche/ExploreNiche/merged_data_clean.csv")
data <- merged_data_clean



# logarithmic regression and generate plots for a given site----
perform_log_regression <- function(site) {
  # Add a new column for logarithmic depth
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for the specified site in the 'Exploratory' column
  site_data <- subset(data, grepl(site, data$Exploratory))
  
  # Create an empty list to store plots
  plots <- list()
  
  # Loop over unique Plot.IDs within the site data
  for (plot_id in unique(site_data$Plot.ID)) {
    plot_data <- subset(site_data, Plot.ID == plot_id)  # Data for current Plot.ID
    model <- lm(d18O ~ LogDepth, data = plot_data)  # Fit the linear model
    
    # Create prediction data
    prediction_data <- data.frame(LogDepth = seq(min(plot_data$LogDepth), max(plot_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Generate the plot
    p <- ggplot(plot_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +
      geom_line(data = prediction_data, aes(x = d18O, y = exp(LogDepth)), color = "red") +
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Plot ID:", plot_id),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    plots[[plot_id]] <- p  # Store the plot in the list
  }
  
  # Output plots
  for (plot_id in names(plots)) {
    print(plots[[plot_id]])
    # Uncomment to save each plot as a separate file
    # ggsave(filename = paste("plot_", site, "_", plot_id, ".png", sep = ""), plot = plots[[plot_id]], width = 10, height = 8)
  }
}

# Perform the analysis for each site
perform_log_regression("SCH")
perform_log_regression("HAI")
perform_log_regression("ALB")




#Single plot codes ---
# --- Single plot analysis ---

# Adding LogDepth to the dataframe; ensure the column name 'Depth..cm.' matches your dataset's actual column name
data$LogDepth <- log(data$Depth..cm.)

# Filter data for 'HEG48' in the 'Plot.ID' column
plot_data <- subset(data, Plot.ID == "HEG48")

# Check if plot_data is not empty to proceed
if (nrow(plot_data) > 0) {
  
  # Fit the linear model with logarithmic transformation
  model <- lm(d18O ~ LogDepth, data = plot_data)
  
  # Create a new data frame for predictions
  prediction_data <- data.frame(LogDepth = seq(min(plot_data$LogDepth), max(plot_data$LogDepth), length.out = 100))
  prediction_data$d18O <- predict(model, newdata = prediction_data)
  
  # Calculate R-squared value
  r_squared <- summary(model)$r.squared
  
  # Create the plot using ggplot2
  library(ggplot2)
  p <- ggplot(plot_data, aes(x = d18O, y = Depth..cm.)) +
    geom_point() +  # Add data points
    geom_line(data = prediction_data, aes(x = d18O, y = exp(LogDepth)), color = "red") +  # Add the regression line
    annotate("text", x = max(plot_data$d18O), y = 0, hjust = 1.1, vjust = -0.1,
             label = paste("R^2 =", round(r_squared, 2)),  # Display R-squared value on the plot
             size = 5, parse = TRUE) +
    theme_minimal() +
    labs(title = "Plot ID: HEG48",  # Customize the plot title
         x = expression(delta^18*O ~ '(%o)'),  # Customize x-axis label with isotopic notation
         y = "Depth (cm)")  # Customize y-axis label
  
  # Print the plot
  print(p)
} else {
  print("No data available for Plot ID: HEG48")
}






# --- R_Square calculations ----

# Ensure the Depth column is correctly named (replace 'Depth' with the actual column name in your dataset)
data$LogDepth <- log(data$Depth)

# Initialize an empty dataframe to store R-squared values
r_squared_df <- data.frame(Exploratory = character(),
                           Plot.ID = character(),
                           R.Squared = numeric(),
                           stringsAsFactors = FALSE)

# Function to process exploratory data
process_exploratory_data <- function(data, exploratory_name) {
  # Filter data for the specific exploratory
  exp_data <- subset(data, grepl(exploratory_name, data$Exploratory))
  
  for (plot_id in unique(exp_data$Plot.ID)) {
    # Subset the data for the current Plot.ID
    plot_data <- subset(exp_data, Plot.ID == plot_id)
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = plot_data)
    
    # Calculate R-squared value and add to the dataframe
    r_squared_df <<- rbind(r_squared_df, data.frame(Exploratory = exploratory_name,
                                                    Plot.ID = plot_id,
                                                    R.Squared = summary(model)$r.squared))
  }
}

# Process each exploratory
exploratories <- c("SCH", "HAI", "ALB")
for (exploratory in exploratories) {
  process_exploratory_data(data, exploratory)
}

# Display the R-squared values dataframe
print(r_squared_df)

# --- Plot the R squares ---


ggplot(r_squared_df, aes(x = Plot.ID, y = R.Squared, fill = R.Squared)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", R.Squared)), nudge_y = 0.02, size = 3) +
  scale_fill_gradient(low = "blue", high = "red", name = "R-squared Value") +
  labs(title = "R-squared Values by Plot ID", x = "Plot ID", y = "R-squared Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(size = 10))

# --- Data power using observation ---

# Count the number of observations for each Plot.ID
data_counts <- data %>%
  group_by(Plot.ID) %>%
  summarise(Count = n())

# Preview the aggregated data
print(data_counts)

ggplot(data_counts, aes(x = Plot.ID, y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), nudge_y = 0.02, size = 3) +
  scale_fill_gradient(low = "blue", high = "red", name = "Observation Count") +
  labs(title = "Number of Observations by Plot ID", x = "Plot ID", y = "Number of Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(size = 10))

# --- Observation effect on R square ---

# First, ensure that r_squared_df has a matching number of observations per plot
# This can be done by joining or ensuring both data frames are aligned

# Join the data frames on 'Plot.ID'
combined_df <- merge(data_counts, r_squared_df, by = "Plot.ID")

correlation_coefficient <- cor(combined_df$Count, combined_df$R.Squared)
print(correlation_coefficient)

# Plot the overlapping
ggplot(combined_df, aes(x = Count, y = R.Squared)) +
  geom_point(aes(color = R.Squared), size = 3) +  # Color points by R.Squared value for additional insight
  geom_smooth(method = "lm", color = "blue") +   # Add a linear regression line
  scale_color_gradient(low = "blue", high = "red", name = "R-squared Value") +
  labs(title = "Effect of Data Power on R-squared Values", x = "Number of Observations (Data Power)", y = "R-squared Value") +
  theme_minimal()



# 50% Determine how many plots have an R-squared value over 50%----
over_50 <- r_squared_df %>% filter(R.Squared > 0.5)
print(over_50$Plot.ID)
print(nrow(over_50))

# Group by 'Exploratory', filter for R.Squared > 0.4, and then count the number of plots per exploratory
counts_over_50 <- r_squared_df %>%
  group_by(Exploratory) %>%
  filter(R.Squared > 0.5) %>%
  summarise(Count = n())

# Print the counts for each region
print(counts_over_50)


  
  #PLANTS ----
plant_data_clean <- read.csv("/Users/sepidehgolshani/Desktop/PhD.ExploreNiche/ExploreNiche/plant_data_clean.csv")
plant_data <- plant_data_clean

# Filter plant data for 'HEG48'
plant_data_HEG48 <- subset(plant_data, Plot.ID == "HEG48")

# Check if there is data for HEG48 to proceed
if (nrow(plant_data_HEG48) > 0) {
  
  # Generate a dense range of LogDepth values from the soil data for prediction
  dense_log_depth <- seq(min(data$LogDepth), max(data$LogDepth), length.out = 1000)
  prediction_data <- data.frame(LogDepth = dense_log_depth)
  prediction_data$d18O <- predict(model, newdata = prediction_data)
  
  # For each plant d18O value, find the closest predicted d18O and its corresponding LogDepth
  predicted_depths <- sapply(plant_data_HEG48$d18O, function(x) {
    closest_index <- which.min(abs(prediction_data$d18O - x))
    return(exp(prediction_data$LogDepth[closest_index]))  # Convert LogDepth back to actual Depth
  })
  
  # Add the predicted depths to the plant dataset
  plant_data_HEG48$PredictedDepth <- predicted_depths
  
  # Plotting predicted water uptake depth for each species
  library(ggplot2)
  p <- ggplot(plant_data_HEG48, aes(x = Species, y = PredictedDepth, fill = Species)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    labs(title = "Predicted Water Uptake Depth for Species in HEG48",
         x = "Species",
         y = "Predicted Water Uptake Depth (cm)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
} else {
  message("No plant data found for HEG48")
}

  
  
  
  # Single Plant / Soil model----
  
  # Adding LogDepth to the dataframe
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for 'HEG48' in the 'Plot.ID' column
  plot_data <- subset(data, Plot.ID == "HEG48")
  
  # Fit the linear model with logarithmic transformation
  model <- lm(d18O ~ LogDepth, data = plot_data)
  
  # Prepare a prediction frame for plotting the regression line
  prediction_data <- data.frame(LogDepth = seq(min(plot_data$LogDepth), max(plot_data$LogDepth), length.out = 100))
  prediction_data$d18O <- predict(model, newdata = prediction_data)
  
  # Calculate R-squared value for the soil model
  r_squared <- summary(model)$r.squared
  
  # Predict plant water uptake depths based on model
  plant_data_HEG48$PredictedLogDepth <- sapply(plant_data_HEG48$d18O, function(x) {
    closest <- which.min(abs(prediction_data$d18O - x))
    return(prediction_data$LogDepth[closest])
  })
  
  # Convert predicted LogDepth back to Depth for plotting
  plant_data_HEG48$PredictedDepth <- exp(plant_data_HEG48$PredictedLogDepth)
  
  # Check the number of unique species and assign colors
  unique_species <- unique(plant_data_HEG48$Species)
  num_species <- length(unique_species)
  colors <- hcl.colors(num_species, "Set3")  # 'Set3' provides distinct colors
  
  # Create the plot
  ggplot() +
    geom_point(data = plot_data, aes(x = d18O, y = Depth..cm.), color = "gray") +  # Existing soil data points
    geom_line(data = prediction_data, aes(x = d18O, y = exp(LogDepth)), color = "red") +  # Soil model regression line
    geom_point(data = plant_data_HEG48, aes(x = d18O, y = PredictedDepth, color = Species), size = 4) +  # Plant data points
    geom_text(data = plant_data_HEG48, aes(x = d18O, y = PredictedDepth, label = Species), vjust = -1, hjust = -0.1) +  # Labels
    scale_y_reverse() +  # Reverse the y-axis
    scale_color_manual(values = colors) +  # Dynamic color assignment
    annotate("text", x = min(plot_data$d18O), y = 0, label = sprintf("R² = %.2f", r_squared), hjust = 0, vjust = -1, size = 4) +  # Add R-squared annotation
    theme_minimal() +
    labs(title = "Integration of Plant Water Uptake Depths with Soil Model",
         x = expression(delta^18*O ~ '(%o)'),
         y = "Depth (cm)") +
    theme(legend.title = element_blank())
  
  
  
  
  
  
  #All HEG plant/ soil models----
  
  # --- Soil and Plant Model Integration ---
  
  # Prepare the LogDepth column in the dataset
  data$LogDepth <- log(data$Depth..cm.)
  
  # Identify all unique HEG plots
  HEG_plots <- unique(data$Plot.ID[grep("^HEG", data$Plot.ID)])
  
  # Loop through each HEG plot
  for (plot_id in HEG_plots) {
    plot_data <- subset(data, Plot.ID == plot_id)
    
    if (nrow(plot_data) > 0) {
      # Fit the linear model with logarithmic transformation
      model <- lm(d18O ~ LogDepth, data = plot_data)
      prediction_data <- data.frame(LogDepth = seq(min(plot_data$LogDepth), max(plot_data$LogDepth), length.out = 100))
      prediction_data$d18O <- predict(model, newdata = prediction_data)
      r_squared <- summary(model)$r.squared
      
      plant_data_current <- subset(plant_data_clean, Plot.ID == plot_id)
      
      if (nrow(plant_data_current) > 0) {
        # Predict and convert LogDepth back to Depth
        plant_data_current$PredictedLogDepth <- sapply(plant_data_current$d18O, function(x) {
          closest <- which.min(abs(prediction_data$d18O - x))
          return(prediction_data$LogDepth[closest])
        })
        plant_data_current$PredictedDepth <- exp(plant_data_current$PredictedLogDepth)
        
        # Assign colors for species visualization
        colors <- hcl.colors(length(unique(plant_data_current$Species)), "Spectral")
        
        # Create and print the plot
        p <- ggplot() +
          geom_point(data = plot_data, aes(x = d18O, y = Depth..cm.), color = "gray") +
          geom_line(data = prediction_data, aes(x = d18O, y = exp(LogDepth)), color = "darkgreen") +
          geom_point(data = plant_data_current, aes(x = d18O, y = PredictedDepth, color = Species), size = 4) +
          scale_y_reverse() +
          scale_color_manual(values = colors) +
          theme_minimal() +
          labs(title = paste("Plant Water Uptake Depths with Soil Model -", plot_id),
               x = expression(delta^18*O ~ '(%o)'),
               y = "Depth (cm)")
        
        print(p)
      } else {
        message(paste("No plant data found for", plot_id))
      }
    } else {
      message(paste("No data found for", plot_id))
    }
  }
  
  
  
  
  #Interactive plant/ Soil----


  # Assuming 'data' and 'plant_data_clean' are loaded globally
  
  # Adding LogDepth to the dataframe
  data$LogDepth <- log(data$Depth..cm.)
  
  ui <- fluidPage(
    titlePanel("Interactive Soil and Plant Depth Model"),
    sidebarLayout(
      sidebarPanel(
        selectInput("plotID", "Choose a Plot ID:", choices = unique(data$Plot.ID)),
        actionButton("goButton", "Generate Graph"),
        downloadButton("downloadSVG", "Download SVG")  # Button for downloading SVG
      ),
      mainPanel(
        plotOutput("depthPlot")
      )
    )
  )
  
  
  server <- function(input, output) {
    plot_data <- reactive({
      subset(data, Plot.ID == input$plotID)
    })
    
    plot_model <- reactive({
      if (nrow(plot_data()) > 0) {
        model <- lm(d18O ~ LogDepth, data = plot_data())
        prediction_data <- data.frame(LogDepth = seq(min(plot_data()$LogDepth), max(plot_data()$LogDepth), length.out = 100))
        prediction_data$d18O <- predict(model, newdata = prediction_data)
        return(list(model = model, prediction_data = prediction_data))
      } else {
        NULL
      }
    })
    
    # Create a reactive expression for the plot
    plot_output <- reactive({
      req(plot_model())  # Ensure plot_model is not NULL
      plant_data <- subset(plant_data_clean, Plot.ID == input$plotID)
      r_squared <- summary(plot_model()$model)$r.squared
      
      if (nrow(plant_data) > 0) {
        plant_data$PredictedLogDepth <- sapply(plant_data$d18O, function(x) {
          closest <- which.min(abs(plot_model()$prediction_data$d18O - x))
          return(plot_model()$prediction_data$LogDepth[closest])
        })
        plant_data$PredictedDepth <- exp(plant_data$PredictedLogDepth)
        
        unique_species <- unique(plant_data$Species)
        colors <- hcl.colors(length(unique_species), "Spectral")
        
        ggplot() +
          geom_point(data = plot_data(), aes(x = d18O, y = Depth..cm.), color = "darkgray") +
          geom_line(data = plot_model()$prediction_data, aes(x = d18O, y = exp(LogDepth)), color = "darkblue", size= 1) +
          geom_point(data = plant_data, aes(x = d18O, y = PredictedDepth, color = Species), size = 4) +
          scale_y_reverse() +
          scale_color_manual(values = colors) +
          annotate("text", x = min(plot_data()$d18O), y = 0, label = sprintf("R² = %.2f", r_squared), hjust = 0, vjust = -1, size = 4) +
          theme_minimal() +
          labs(title = paste("Integration of Plant Water Uptake Depths with Soil Model -", input$plotID),
               x = expression(delta^18*O ~ '(%o)'),
               y = "Depth (cm)") +
          theme(legend.title = element_blank())
      } else {
        ggplot() + 
          labs(title = "No plant data found for the selected plot", x = "", y = "") +
          theme_void()
      }
    })
    
    output$depthPlot <- renderPlot({
      plot_output()  # Render the plot
    })
    
    output$downloadSVG <- downloadHandler(
      filename = function() {
        paste("plot-", input$plotID, ".svg", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_output(), device = "svg", width = 8.53, height = 9.03)
      }
    )
  }
  # Run the application
  shinyApp(ui = ui, server = server)  
  
  
  
  
  
  #SPECIES----
  
#All species from all plots----
  # Function to simulate plot_model for an entire dataset (global model for simplification)
  global_plot_model <- function(data) {
    data$LogDepth <- log(data$`Depth..cm.`)  # Using correct column name
    model <- lm(d18O ~ LogDepth, data = data)
    prediction_data <- data.frame(LogDepth = seq(min(data$LogDepth), max(data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    return(list(model = model, prediction_data = prediction_data))
  }
  
  # Compute global model
  all_predictions <- global_plot_model(data)
  
  # Calculate PredictedDepth for all plant data
  plant_data_clean$PredictedLogDepth <- sapply(plant_data_clean$d18O, function(x) {
    closest <- which.min(abs(all_predictions$prediction_data$d18O - x))
    return(all_predictions$prediction_data$LogDepth[closest])
  })
  plant_data_clean$PredictedDepth <- exp(plant_data_clean$PredictedLogDepth)
  
  
  # Calculate mean PredictedDepth and count for each species
  species_summary <- plant_data_clean %>%
    group_by(Species) %>%
    summarise(
      MeanPredictedDepth = mean(PredictedDepth, na.rm = TRUE),
      Count = n()  # Count the number of observations per species
    )
  
  
  # Plot with counts in the legend
  ggplot(species_summary, aes(x = Species, y = MeanPredictedDepth, fill = paste(Species, Count, sep = " (n="))) +
    geom_col() +
    labs(title = "Average Predicted Water Uptake Depth by Species",
         x = "Species",
         y = "Mean Predicted Water Uptake Depth (cm)") +
    scale_fill_discrete(name = "Species (Count)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  #filter 50% R-squares of all plots ----
  
  
  # Define the specific plot numbers to include
  selected_plots <- c("SEG18", "SEG22", "SEG31", "SEG32", "SEG33", "SEG37", "SEG38", "SEG43", "SEG46", "SEG5", 
                      "SEG8", "HEG1", "HEG10", "HEG11", "HEG26", "HEG27", "HEG28", "HEG3", "HEG30", "HEG33",
                      "HEG4", "HEG40", "HEG43", "HEG44", "HEG48", "HEG5", "HEG50", "HEG7", "HEG8", "HEG9", 
                      "AEG1", "AEG10", "AEG2", "AEG20", "AEG21", "AEG22", "AEG26", "AEG3", "AEG30", "AEG31",
                      "AEG33", "AEG38", "AEG39", "AEG4", "AEG45", "AEG46", "AEG49", "AEG5", "AEG7", "AEG8", "AEG9")
  
  
  # Assuming data and plant_data_clean are already loaded
  # Filter both datasets to include only selected plots
  filtered_data <- data %>% 
    filter(Plot.ID %in% selected_plots)
  
  filtered_plant_data <- plant_data_clean %>% 
    filter(Plot.ID %in% selected_plots)
  
  # Function to simulate plot_model using the filtered data
  global_plot_model <- function(data) {
    data$LogDepth <- log(data$`Depth..cm.`)
    model <- lm(d18O ~ LogDepth, data = data)
    prediction_data <- data.frame(LogDepth = seq(min(data$LogDepth), max(data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    return(list(model = model, prediction_data = prediction_data))
  }
  
  # Compute global model for the filtered data
  all_predictions <- global_plot_model(filtered_data)
  
  # Calculate PredictedDepth for the filtered plant data
  filtered_plant_data$PredictedLogDepth <- sapply(filtered_plant_data$d18O, function(x) {
    closest <- which.min(abs(all_predictions$prediction_data$d18O - x))
    return(all_predictions$prediction_data$LogDepth[closest])
  })
  filtered_plant_data$PredictedDepth <- exp(filtered_plant_data$PredictedLogDepth)
  
  
  # Calculate mean PredictedDepth and count for each species in the filtered plant data
  species_summary <- filtered_plant_data %>%
    group_by(Species) %>%
    summarise(
      MeanPredictedDepth = mean(PredictedDepth, na.rm = TRUE),
      Count = n()
    )
  
  # Plot the results
  ggplot(species_summary, aes(x = Species, y = MeanPredictedDepth, fill = Species)) +
    geom_col() +
    geom_text(aes(label = paste("n =", Count)), vjust = -0.5, color = "black") +
    labs(title = "Average Predicted Water Uptake Depth by Species (Filtered Plots)",
         x = "Species",
         y = "Mean Predicted Water Uptake Depth (cm)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  
  
  # SEG plots more than 50%----
  selected_plots <- c("SEG18", "SEG22", "SEG31", "SEG32", "SEG33", "SEG37", "SEG38", "SEG43", "SEG46", "SEG5", 
                      "SEG8")
  
  # Assuming 'data' and 'plant_data_clean' contain 'Plot.ID'
  # Filter the data to only include plots that start with 'SEG' and are in the selected plots
  filtered_data <- data %>%
    filter(grepl("^SEG", Plot.ID), Plot.ID %in% selected_plots)
  
  filtered_plant_data <- plant_data_clean %>%
    filter(grepl("^SEG", Plot.ID), Plot.ID %in% selected_plots)
  
  # Function to simulate plot_model using the filtered data
  global_plot_model <- function(data) {
    data$LogDepth <- log(data$`Depth..cm.`)
    model <- lm(d18O ~ LogDepth, data = data)
    prediction_data <- data.frame(LogDepth = seq(min(data$LogDepth), max(data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    return(list(model = model, prediction_data = prediction_data))
  }
  
  # Compute global model for the filtered data
  all_predictions <- global_plot_odel(filtered_data)
  
  # Calculate PredictedDepth for the filtered plant data
  filtered_plant_data$PredictedLogDepth <- sapply(filtered_plant_data$d18O, function(x) {
    closest <- which.min(abs(all_predictions$prediction_data$d18O - x))
    return(all_predictions$prediction_data$LogDepth[closest])
  })
  filtered_plant_data$PredictedDepth <- exp(filtered_plant_data$PredictedLogDepth)
  
  # Calculate mean PredictedDepth and count for each species in the filtered plant data
  species_summary <- filtered_plant_data %>%
    group_by(Species) %>%
    summarise(
      MeanPredictedDepth = mean(PredictedDepth, na.rm = TRUE),
      Count = n()
    )
  
  # Plot the results
  ggplot(species_summary, aes(x = Species, y = MeanPredictedDepth, fill = Species)) +
    geom_col() +
    geom_text(aes(label = paste("n =", Count)), vjust = -0.5, color = "black") +
    labs(title = "Average Predicted Water Uptake Depth by Species (SCH Plots)",
         x = "Species",
         y = "Mean Predicted Water Uptake Depth (cm)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  # HEG plots more than 50%----
  # Define the specific plot numbers to include, focusing on 'HEG'
  selected_plots <- c("HEG1", "HEG10", "HEG11", "HEG26", "HEG27", "HEG28", "HEG3", "HEG30", "HEG33",
                      "HEG4", "HEG40", "HEG43", "HEG44", "HEG48", "HEG5", "HEG50", "HEG7", "HEG8", "HEG9")
  
  # Filter the data to only include plots that start with 'HEG' and are in the selected plots
  filtered_data <- data %>%
    filter(grepl("^HEG", Plot.ID), Plot.ID %in% selected_plots)
  
  filtered_plant_data <- plant_data_clean %>%
    filter(grepl("^HEG", Plot.ID), Plot.ID %in% selected_plots)
  
  # Function to simulate plot_model using the filtered data
  global_plot_model <- function(data) {
    data$LogDepth <- log(data$`Depth..cm.`)
    model <- lm(d18O ~ LogDepth, data = data)
    prediction_data <- data.frame(LogDepth = seq(min(data$LogDepth), max(data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_to_data)
    return(list(model = model, prediction_data = prediction_data))
  }
  
  # Compute global model for the filtered data
  all_predictions <- global_plot_model(filtered_data)
  
  # Calculate PredictedDepth for the filtered plant data
  filtered_plant_data$PredictedLogDepth <- sapply(filtered_plant_data$d18O, function(x) {
    closest <- which.min(abs(all_predictions$prediction_data$d18O - x))
    return(all_predictions$prediction_data$LogDepth[closest])
  })
  filtered_plant_data$PredictedDepth <- exp(filtered_plant_data$PredictedLogDepth)
  
  # Calculate mean PredictedDepth and count for each species in the filtered plant data
  species_summary <- filtered_plant_data %>%
    group_by(Species) %>%
    summarise(
      MeanPredictedDepth = mean(PredictedDepth, na.rm = TRUE),
      Count = n()
    )
  
  # Plot the results
  ggplot(species_summary, aes(x = Species, y = MeanPredictedDepth, fill = Species)) +
    geom_col() +
    geom_text(aes(label = paste("n =", Count)), vjust = -0.5, color = "black") +
    labs(title = "Average Predicted Water Uptake Depth by Species (Filtered HEG Plots)",
         x = "Species",
         y = "Mean Predicted Water Uptake Depth (cm)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  # AEG plots more than 50%----
  # Define the specific plot numbers to include, focusing on 'ALB'
  selected_plots <- c("AEG1", "AEG10", "AEG11", "AEG26", "AEG27", "AEG28", "AEG3", "AEG30", "AEG33",
                      "AEG4", "AEG40", "AEG43", "AEG44", "AEG48", "AEG5", "AEG50", "AEG7", "AEG8", "AEG9")
  
  # Assuming 'data' and 'plant_data_clean' contain 'Plot.ID'
  # Filter the data to only include plots that start with 'ALB' and are in the selected plots
  filtered_data <- data %>%
    filter(grepl("^AEG", Plot.ID), Plot.ID %in% selected_plots)
  
  filtered_plant_data <- plant_data_clean %>%
    filter(grepl("^AEG", Plot.ID), Plot.ID %in% selected_plots)
  
  
  
  # Function to simulate plot_model using the filtered data
  global_plot_model <- function(data) {
    data$LogDepth <- log(data$`Depth..cm.`)
    model <- lm(d18O ~ LogDepth, data = data)
    prediction_data <- data.frame(LogDepth = seq(min(data$LogDepth), max(data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    return(list(model = model, prediction_data = prediction_data))
  }
  
  # Compute global model for the filtered data
  all_predictions <- global_plot_model(filtered_data)
  
  # Calculate PredictedDepth for the filtered plant data
  filtered_plant_data$PredictedLogDepth <- sapply(filtered_plant_data$d18O, function(x) {
    closest <- which.min(abs(all_predictions$prediction_data$d18O - x))
    return(all_predictions$prediction_data$LogDepth[closest])
  })
  filtered_plant_data$PredictedDepth <- exp(filtered_plant_data$PredictedLogDepth)
  
  # Calculate mean PredictedDepth and count for each species in the filtered plant data
  species_summary <- filtered_plant_data %>%
    group_by(Species) %>%
    summarise(
      MeanPredictedDepth = mean(PredictedDepth, na.rm = TRUE),
      Count = n()
    )
  
  # Plot the results
  ggplot(species_summary, aes(x = Species, y = MeanPredictedDepth, fill = Species)) +
    geom_col() +
    geom_text(aes(label = paste("n =", Count)), vjust = -0.5, color = "black") +
    labs(title = "Average Predicted Water Uptake Depth by Species (Filtered ALB Plots)",
         x = "Species",
         y = "Mean Predicted Water Uptake Depth (cm)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  
  
  
  
  
  
  
  #Density interactive graph----
  
  
  # Assuming the filtered_plant_data and species_summary have already been computed
  
  # Filter out species with fewer than two data points
  filtered_plant_data <- filtered_plant_data %>%
    group_by(Species) %>%
    filter(n() >= 2) %>%
    ungroup()
  
  # Create the density plot with ggplot2
  density_plot <- ggplot(filtered_plant_data, aes(x = PredictedDepth, fill = Species)) +
    geom_density(alpha = 0.6) +
    labs(title = "Density Plot of Predicted Water Uptake Depth by Species",
         x = "Predicted Water Uptake Depth (cm)",
         y = "Density") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert the ggplot to an interactive plotly plot
  interactive_plot <- ggplotly(density_plot)
  
  # Display the interactive plot
  interactive_plot
  
  
  
  
  
  
  # Filter out species with fewer than two data points
  filtered_plant_data <- filtered_plant_data %>%
    group_by(Species) %>%
    filter(n() >= 2) %>%
    ungroup()
  
  # Calculate density for each species
  densities <- lapply(split(filtered_plant_data, filtered_plant_data$Species), function(data) {
    dens <- density(data$PredictedDepth)
    data.frame(PredictedDepth = dens$x, Density = dens$y, Species = unique(data$Species))
  })
  
  # Combine all densities into one data frame
  density_data <- do.call(rbind, densities)
  
  # Create a plotly plot
  # Create a plotly plot with lines only
  interactive_plot <- plot_ly(density_data, x = ~Density, y = ~PredictedDepth, color = ~Species, 
                              type = 'scatter', mode = 'lines', 
                              line = list(width = 2), 
                              hoverinfo = 'text',
                              text = ~paste("Depth:", PredictedDepth, "Density:", Density, "Species:", Species))
  
  # Layout adjustments
  interactive_plot <- interactive_plot %>%
    layout(title = "Density Plot of Predicted Water Uptake Depth by Species",
           xaxis = list(title = "Density"),
           yaxis = list(title = "Predicted Water Uptake Depth (cm)"))
  
  # Display the interactive plot
  interactive_plot
  
  
  
  
  
  
  
  
  
  
  #test clustering data----
  library(ggplot2)
  library(dplyr)
  
  
  # Filter out values of d18O under -11
  data <- data %>% filter(d18O >= -11)
  
  # Assuming 'data' is your dataframe and 'Exploratories' is the name of the column containing the site information
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for rows with 'SCH' in the 'Exploratories' column
  sch_data <- data %>% filter(grepl("SCH", Exploratory))
  
  # Apply clustering based on Longitude and Latitude (assuming these columns exist in your data)
  set.seed(123) # for reproducibility
  sch_data <- sch_data %>%
    mutate(Cluster = kmeans(cbind(Longitude, Latitude), centers = 5, nstart = 25)$cluster)
  
  # Fit the linear model with logarithmic transformation and plot for each cluster
  for (cluster_id in unique(sch_data$Cluster)) {
    # Subset the data for the current cluster
    cluster_data <- sch_data[sch_data$Cluster == cluster_id,]
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = cluster_data)
    
    # Create a new data frame for predictions
    prediction_data <- data.frame(LogDepth = seq(min(cluster_data$LogDepth), max(cluster_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    prediction_data$Depth..cm. <- exp(prediction_data$LogDepth)  # Back-transform
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Create the plot
    p <- ggplot(cluster_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +  # Add points
      geom_line(data = prediction_data, aes(x = d18O, y = Depth..cm.), color = "red") +  # Add the regression line
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Cluster:", cluster_id),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    # Explicitly print the plot within the loop
    print(p)
  }
  
  
  
  
  
  # Filter out values of d18O under -11
  data <- data %>% filter(d18O >= -11)
  
  # Assuming 'data' is your dataframe and 'Exploratories' is the name of the column containing the site information
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for rows with 'HAI' in the 'Exploratories' column
  hai_data <- data %>% filter(grepl("HAI", Exploratory))
  
  # Apply clustering based on Longitude and Latitude (assuming these columns exist in your data)
  set.seed(123) # for reproducibility
  hai_data <- hai_data %>%
    mutate(Cluster = kmeans(cbind(Longitude, Latitude), centers = 5, nstart = 25)$cluster)
  
  # Fit the linear model with logarithmic transformation and plot for each cluster
  for (cluster_id in unique(hai_data$Cluster)) {
    # Subset the data for the current cluster
    cluster_data <- hai_data[hai_data$Cluster == cluster_id,]
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = cluster_data)
    
    # Create a new data frame for predictions
    prediction_data <- data.frame(LogDepth = seq(min(cluster_data$LogDepth), max(cluster_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    prediction_data$Depth..cm. <- exp(prediction_data$LogDepth)  # Back-transform
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Create the plot
    p <- ggplot(cluster_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +  # Add points
      geom_line(data = prediction_data, aes(x = d18O, y = Depth..cm.), color = "red") +  # Add the regression line
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Cluster:", cluster_id),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    # Explicitly print the plot within the loop
    print(p)
  }
  
  
  
  
  # Filter out values of d18O under -11
  data <- data %>% filter(d18O >= -11)
  
  # Assuming 'data' is your dataframe and 'Exploratories' is the name of the column containing the site information
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for rows with 'ALB' in the 'Exploratories' column
  alb_data <- data %>% filter(grepl("ALB", Exploratory))
  
  # Apply clustering based on Longitude and Latitude (assuming these columns exist in your data)
  set.seed(123) # for reproducibility
  alb_data <- alb_data %>%
    mutate(Cluster = kmeans(cbind(Longitude, Latitude), centers = 5, nstart = 25)$cluster)
  
  # Fit the linear model with logarithmic transformation and plot for each cluster
  for (cluster_id in unique(alb_data$Cluster)) {
    # Subset the data for the current cluster
    cluster_data <- alb_data[alb_data$Cluster == cluster_id,]
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = cluster_data)
    
    # Create a new data frame for predictions
    prediction_data <- data.frame(LogDepth = seq(min(cluster_data$LogDepth), max(cluster_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    prediction_data$Depth..cm. <- exp(prediction_data$LogDepth)  # Back-transform
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Create the plot
    p <- ggplot(cluster_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +  # Add points
      geom_line(data = prediction_data, aes(x = d18O, y = Depth..cm.), color = "red") +  # Add the regression line
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Cluster:", cluster_id),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    # Explicitly print the plot within the loop
    print(p)
  }
  
  
  
  
  
  
  
  library(ggplot2)
  library(dplyr)
  
  
  # Filter out values of d18O under -11
  data <- data %>% filter(d18O >= -11)
  
  # Assuming 'data' is your dataframe and 'Exploratories' is the name of the column containing the site information
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for rows with 'SCH' in the 'Exploratories' column
  sch_data <- data %>% filter(grepl("SCH", Exploratory))
  
  # Analyze by Soil Type instead of Clusters
  # Loop over each unique soil type found in the dataset
  for (soil_type in unique(sch_data$Soil_Type_WRB)) {
    # Subset the data for the current soil type
    soil_data <- sch_data[sch_data$Soil_Type_WRB == soil_type,]
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = soil_data)
    
    # Create a new data frame for predictions
    prediction_data <- data.frame(LogDepth = seq(min(soil_data$LogDepth), max(soil_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    prediction_data$Depth..cm. <- exp(prediction_data$LogDepth)  # Back-transform
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Create the plot
    p <- ggplot(soil_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +  # Add points
      geom_line(data = prediction_data, aes(x = d18O, y = Depth..cm.), color = "red") +  # Add the regression line
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Soil Type:", soil_type),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    # Explicitly print the plot within the loop
    print(p)
  }
  
  
  
  library(ggplot2)
  library(dplyr)
  
  
  # Filter out values of d18O under -11
  data <- data %>% filter(d18O >= -11)
  
  # Assuming 'data' is your dataframe and 'Exploratories' is the name of the column containing the site information
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for rows with 'HAI' in the 'Exploratories' column
  hai_data <- data %>% filter(grepl("HAI", Exploratory))
  
  # Analyze by Soil Type instead of Clusters
  # Loop over each unique soil type found in the dataset
  for (soil_type in unique(hai_data$Soil_Type_WRB)) {
    # Subset the data for the current soil type
    soil_data <- hai_data[hai_data$Soil_Type_WRB == soil_type,]
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = soil_data)
    
    # Create a new data frame for predictions
    prediction_data <- data.frame(LogDepth = seq(min(soil_data$LogDepth), max(soil_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    prediction_data$Depth..cm. <- exp(prediction_data$LogDepth)  # Back-transform
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Create the plot
    p <- ggplot(soil_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +  # Add points
      geom_line(data = prediction_data, aes(x = d18O, y = Depth..cm.), color = "red") +  # Add the regression line
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Soil Type:", soil_type),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    # Explicitly print the plot within the loop
    print(p)
  }
  
  
  library(ggplot2)
  library(dplyr)
  
  
  # Filter out values of d18O under -11
  data <- data %>% filter(d18O >= -11)
  
  # Assuming 'data' is your dataframe and 'Exploratories' is the name of the column containing the site information
  data$LogDepth <- log(data$Depth..cm.)
  
  # Filter data for rows with 'ALB' in the 'Exploratories' column
  alb_data <- data %>% filter(grepl("ALB", Exploratory))
  
  # Analyze by Soil Type instead of Clusters
  # Loop over each unique soil type found in the dataset
  for (soil_type in unique(alb_data$Soil_Type_WRB)) {
    # Subset the data for the current soil type
    soil_data <- alb_data[alb_data$Soil_Type_WRB == soil_type,]
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = soil_data)
    
    # Create a new data frame for predictions
    prediction_data <- data.frame(LogDepth = seq(min(soil_data$LogDepth), max(soil_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    prediction_data$Depth..cm. <- exp(prediction_data$LogDepth)  # Back-transform
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Create the plot
    p <- ggplot(soil_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +  # Add points
      geom_line(data = prediction_data, aes(x = d18O, y = Depth..cm.), color = "red") +  # Add the regression line
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Soil Type:", soil_type),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    # Explicitly print the plot within the loop
    print(p)
  }
  
  
  
  
  
  
  
  # Define the specific plot numbers to exclude
  excluded_plots <- c("SEG18", "SEG22", "SEG31", "SEG32", "SEG33", "SEG37", "SEG38", "SEG43", "SEG46", "SEG5", 
                      "SEG8", "HEG1", "HEG10", "HEG11", "HEG26", "HEG27", "HEG28", "HEG3", "HEG30", "HEG33",
                      "HEG4", "HEG40", "HEG43", "HEG44", "HEG48", "HEG5", "HEG50", "HEG7", "HEG8", "HEG9", 
                      "AEG1", "AEG10", "AEG2", "AEG20", "AEG21", "AEG22", "AEG26", "AEG3", "AEG30", "AEG31",
                      "AEG33", "AEG38", "AEG39", "AEG4", "AEG45", "AEG46", "AEG49", "AEG5", "AEG7", "AEG8", "AEG9")
  
  # Filter out these plots and values of d18O under -11
  data <- data %>% 
    filter(!Plot.ID %in% excluded_plots, d18O >= -11)
  
  # Assuming 'data' is your dataframe and 'Exploratories' is the name of the column containing the site information
  data$LogDepth <- log(data$Depth..cm.)
  
  # Loop through each unique exploratory found in the dataset
  for (exploratory in unique(data$Exploratory)) {
    # Subset the data for the current exploratory
    exploratory_data <- data[data$Exploratory == exploratory,]
    
    # Fit the linear model with logarithmic transformation
    model <- lm(d18O ~ LogDepth, data = exploratory_data)
    
    # Create a new data frame for predictions
    prediction_data <- data.frame(LogDepth = seq(min(exploratory_data$LogDepth), max(exploratory_data$LogDepth), length.out = 100))
    prediction_data$d18O <- predict(model, newdata = prediction_data)
    prediction_data$Depth..cm. <- exp(prediction_data$LogDepth)  # Back-transform
    
    # Calculate R-squared value
    r_squared <- summary(model)$r.squared
    
    # Create the plot
    p <- ggplot(exploratory_data, aes(x = d18O, y = Depth..cm.)) +
      geom_point() +  # Add points
      geom_line(data = prediction_data, aes(x = d18O, y = Depth..cm.), color = "red") +  # Add the regression line
      annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
               label = paste("R^2 = ", round(r_squared, 2), sep = ""),
               size = 5, parse = TRUE) +
      theme_minimal() +
      labs(title = paste("Exploratory:", exploratory),
           x = expression(delta^18*O ~ '(%o)'),
           y = "Depth (cm)")
    
    # Explicitly print the plot within the loop
    print(p)
  }
  
  
  
  