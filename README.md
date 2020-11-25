# R application using shiny package

The application predicts a ticket price and chances of survival for Titanic cruise based on user's input.
Predictive models were trained on records from file titanic.csv, which contains information about 891 passengers.
Each record(passenger) is characterized by such traits like age, sex and ticket class.

Training of models was performed thanks to caret library and process of learning is presented in file pomoc_titanic.Rmd.

In order to run application, open this project in RStudio and type in its console:

shiny::runApp()

