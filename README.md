# EpiPlot

EpiPlot est une application web permettant de mettre en forme des données de mouvements de patients afin de produire un graphique de suivi des mouvements interactif ainsi que de construire un réseau. Un fichier avec les prélèvements effectués pour chaque patient (positifs et négatifs) peut être ajouté afin de visualiser les prélèvements sur le graphique et de trier les patients selon le génotype de leur souche.

## Installation

R et Rstudio sont requis pour utiliser l'application en local. La procédure est décrite sur <https://posit.co/download/rstudio-desktop/>.

L'installation des librairies nécessaires se fait à l'aide du script install_packages.R

```         
# Install libraries
Rscript install_packages.R
```

Il est également possible d'ouvrir le script dans Rstudio et d'exécuter chaque ligne.  

## Exemples

Un fichier de mouvements et un fichier de prélèvements sont disponibles afin de tester l'application (Mouvements_exemple.xlsx et Prelevements_exemple.xlsx)