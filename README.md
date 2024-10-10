# Leads-Shiny-Dashboard

## Overview
The Leads Dashboard is an interactive web application built using R and Shiny. It provides a comprehensive solution for tracking and visualizing leads data. The dashboard allows users to upload leads data, view summary statistics, and generate various visualizations to gain insights into leads patterns.

## Features
- **Summary Statistics**: View key statistics such as total leads, average leads, and conversion rates.
- **Visualizations**: Generate interactive charts and graphs to visualize leads trends over time.
- **Filtering**: Filter data by date range, lead source, or other relevant criteria.
- **Export**: Export visualizations and summary statistics for reporting purposes.

![alt text](<Screenshot 2024-10-10 223833.png>)

![alt text](<Screenshot 2024-10-10 223939.png>)

![alt text](<Screenshot 2024-10-10 224023.png>)

## Installation
To run the Attendance Shiny Dashboard locally, follow these steps:

1. **Clone the repository**:
    ```sh
    git clone https://github.com/robin-ochieng/Leads-Dashboard-.git
    cd Leads-Dashboard-
    ```


2. **Install required packages**:
    Open R or RStudio and run the following command to install the necessary packages:
    ```R
    install.packages(c("shiny", "ggplot2", "dplyr", "readr", "readxl", "lubridate", "DT", "scales", "bs4Dash", "bslib", "fresh", "plotly", "shinycssloaders", "shinyjs"))
    ```

3. **Run the Shiny app**:
    In R or RStudio, run the following command to start the Shiny app:
    ```R
    shiny::runApp()
    ```

## Usage
1. **Upload Data**: Click on the "Upload Data" tab and select your attendance CSV file.
2. **View Summary**: Navigate to the "Motor" and Health tab to view key attendance statistics.
3. **Generate Visualizations**: Use the "Visualizations" tab to create interactive charts and graphs.
4. **Filter Data**: Apply filters to narrow down the data based on specific criteria.
5. **Export**: Export the visualizations and summary statistics for further analysis or reporting.

## Contributing
Contributions are welcome! If you have any suggestions or improvements, please create an issue or submit a pull request.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## Contact
For any questions or inquiries, please contact [Robin Ochieng](robinochieng@gmail.com).