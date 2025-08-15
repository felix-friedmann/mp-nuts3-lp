## Heterogene Effekte geldpolitische Schocks in deutschen Regionen

`src/data_preparation.R` - Datenextraktion und -transformation.

`src/analysis.R` - Grundlegende Schätzung der Koeffizienten und Differenzen zwischen Quantilen mithilfe des [fixest-packages](https://cran.r-project.org/web/packages/fixest/index.html).

`src/wcb.R` - Schätzung von Konfidenzintervallen und p-Werten mit Wild Cluster Bootstrap über das Package [fwildclusterboot](https://github.com/s3alfisc/fwildclusterboot).

`scr/dak.R` - Schätzung von Konfidenzintervallen und p-Werten mit Driscoll and Kraay Standardfehlern über das fixest-package.

---

### Daten

- **ARDECO-Variablen** - Zugriff über das gleichnamige [R-Package](https://cran.r-project.org/web/packages/ARDECO/index.html).  

- **Heterogenitätsvariablen** (`data/struktur.csv`) - Zugriff über den [INKAR-Online-Atlas](https://www.inkar.de) des BBSR Bonn. [^1] 

- **Geldpolitische Schocks** (`data/shocks_ecb_mpd_me_m.csv`) - Die Daten stammen aus dem Repository von [Marek Jarociński](https://github.com/marekjarocinski/jkshocks_update_ecb). [^2]

**Hinweis:** Die Originaldaten sind nicht Teil dieses Repositories und müssen aus den genannten Quellen bezogen werden.

### Reproduzierbare Umgebung

Dieses Projekt verwendet [renv](https://rstudio.github.io/renv/) zur Verwaltung der R-Paketversionen.  
Alle erforderlichen Pakete und deren Versionen sind in der Datei `renv.lock` dokumentiert.  
Um die Projektumgebung wiederherzustellen, im Projektordner in R ausführen:

```r
install.packages("renv")
renv::restore()
```

[^1]: Bundesinstitut für Bau-, Stadt- und Raumforschung (BBSR). (2022). _INKAR: Indikatoren und Karten zur Raum- und Stadtentwicklung_ \[Online-Datenbank. BBSR Bonn\]
[^2]: Jarociński, M., & Karadi, P. (2020). Deconstructing Monetary Policy Surprises - The Role of Information Shocks. _American Economic Journal: Macroeconomics, 12(2)_. DOI: [10.1257/mac.20180090](http://doi.org/10.1257/mac.20180090)
