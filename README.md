# 🏫 Madrid School Admission Tiebreaker

An R Shiny application to help families understand the tiebreaker process for school admissions in the **Comunidad de Madrid** for the 2026/2027 academic year.

-----

## ⚠️ Important Disclaimer

> This tool is an **orientative interpretation** of the tiebreaker methodology described in the [Resolución de 26 de noviembre de 2025](https://www.bocm.es/boletin/CM_Orden_BOCM/2025/12/09/BOCM-20251209-19.PDF) of the Viceconsejería de Política y Organización Educativa (BOCM núm. 293, 9 de diciembre de 2025) and the [Resultado del Sorteo Público of 8 April 2026](https://www.comunidad.madrid/sites/default/files/resultado_sorteo_publico_posibles_empates_3.pdf).
> 
> Calculations are performed automatically based on user-provided data and **do not constitute official confirmation** that any child will or will not be admitted to a specific school. The definitive allocation of school places is the sole responsibility of the **Comunidad de Madrid through the Sistema Integral de Gestión Educativa Raíces**.
> 
> If in doubt, contact the school directly or the relevant **Dirección de Área Territorial (DAT)**.

-----

## 📋 What does this app do?

When multiple children have the same score in the school admission process, a **public lottery** is held to determine the order in which places are assigned. This app:

- Takes the list of applicants and their scores as input
- Applies the official tiebreaker formula from **Instrucción Novena** of the BOCM resolution
- Shows a **step-by-step explanation** of why a specific child enters or does not enter within the top N positions
- Displays the full ranked list with a visual cutoff line

-----

## 🔢 The tiebreaker methodology

As described in **Instrucción Novena** of the Resolución de 26 de noviembre de 2025:

1. All tied applicants are listed in **alphabetical order** and numbered 1 to N.
1. A **starting position** is calculated as:

```
P = floor(lottery_number × N + 1)
```

1. Places are assigned **starting from position P**, continuing in ascending order.
1. The list is **circular**: after the last position, it wraps back to position 1.

The official lottery number for 2026/2027 is **0.1283**, drawn publicly on **8 April 2026**.

-----

## 🚀 Getting started

### Prerequisites

Make sure you have R installed along with the following packages:

```r
install.packages(c("shiny", "dplyr", "stringr", "DT"))
```

### Run locally

```r
shiny::runApp("app.R")
```

### Deploy to shinyapps.io

```r
install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(name = "YOUR_ACCOUNT", token = "YOUR_TOKEN", secret = "YOUR_SECRET")
rsconnect::deployApp("app.R")
```

-----

## 📥 Input format

The app accepts a CSV table (comma or semicolon separated) with two columns:

|Nombre           |Puntuacion|
|-----------------|----------|
|Ortega Soto Vera |13        |
|Pando Leal Bruno |13        |
|Benito Vidal Omar|72        |

You can paste the CSV directly into the text area in the app.

-----

## 🖥️ Features

- 🔢 Configurable lottery number and cutoff position
- 🔍 Search any child by name to see their result
- 📖 Step-by-step reasoning panel explaining each calculation
- 📊 Full ranked list with cutoff line and highlighted search result
- ⚠️ Built-in disclaimer displayed prominently

-----

## 📚 Legal references

- **Resolución de 26 de noviembre de 2025**, Viceconsejería de Política y Organización Educativa — [BOCM núm. 293, 9 de diciembre de 2025](https://www.bocm.es/boletin/CM_Orden_BOCM/2025/12/09/BOCM-20251209-19.PDF)
- **Resultado del Sorteo Público para resolución de empates**, 8 de abril de 2026 — [Comunidad de Madrid](https://www.comunidad.madrid/sites/default/files/resultado_sorteo_publico_posibles_empates_3.pdf)
- **Decreto 29/2013**, de 11 de abril, del Consejo de Gobierno, de libertad de elección de centro escolar en la Comunidad de Madrid
- **Orden 1240/2013**, de 17 de abril, de la Consejería de Educación, Juventud y Deporte

-----

## 🤝 Contributing

Contributions are welcome. If you find an error in the tiebreaker logic or want to improve the UI, please open an issue or submit a pull request.

-----

## 📄 License

MIT License — free to use, modify and distribute with attribution.
