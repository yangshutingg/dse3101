# Benchmarking Economic Models with Shiny

## Introduction
This Shiny application is designed for benchmarking different economic forecasting models with real-time data. Users can select start and end quarters, set forecast horizons, and choose between various economic model types such as AR, ADL, and more. The application then visualizes the forecasted GDP growth rate and compares the performance of the selected models.

## Installation

Before running the application, ensure you have R installed on your system. The application requires several R packages which can be installed with the following commands:

```R
install.packages(c("readxl", "tidyverse", "zoo", "sandwich", "lsei", "shiny", "shinyWidgets", "shinythemes","RSelenium","plotly"))

```

## Deployment

To run the app locally using RStudio, follow these steps:
1.	Clone the Repository:
Begin by cloning the project repository from GitHub. Open RStudio and navigate to the directory where you want to store the project. Then, click on `File` in the top menu, select `New Project`, and choose `Version Control`, followed by `Git`. Enter the URL of the repository on GitHub and click `Create Project`.
2.	Install packages:
Once the project is set up in RStudio, you may need to install any required packages if they have not been installed before.  install and load all the required packages, you can install the pacman package if you have not installed it before, and run the pacman::p_load code at the start of the ‘backend_models.R’ file.
3.	Run the App:
With the packages installed, you can now run the app. First, open the `src` folder which contains all the source code. Then, open and run the R script `backend_models.R`, which contains all the models and backend functions. Next, open and run the R script `server_reactive_version.R`, which contains the server function. Lastly, open and run the R script `ui.R`, which contains the frontend code for user interface, together with the command to launch the app in RStudio.
4.	Explore the App:
Congratulations! You’ve successfully launched the app locally using RStudio. Take some time to explore its features and functionality. If you encounter any issues during the process, refer to this project technical manual or reach out to the project maintainers for assistance.
5.	Run with latest data:
Now you’ve successfully launched the app locally, using the data updated in 2024 quarter 1. If you wish to run the app with the latest data available at the time, you can first delete all files in the `data` folder. Then, open and run the R script `get_data.R`, which will automatically visit the websites and download necessary data to the `data` folder. However, this feature is currently only available for MacBook users. If you are using a Windows machine, you might encounter an error when running the file.


## Built With

* [R](https://www.r-project.org/) - R

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Contributors

* Choo Jin Yi
* Toh Kai Lin
* Wang Tingyu Kelly
* Yang Shu Ting

## Acknowledgments

* Prof. Denis & Prof. Huang for their guidance on this project
