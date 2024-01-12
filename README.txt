Application R Shiny : Production Électrique Annuelle 

Aperçu :
Cette application R Shiny offre une analyse interactive des données de production électrique annuelle. Elle propose une interface complète pour filtrer et visualiser les données de production électrique selon différentes dimensions telles que l'année, la région, le département et la commune. Les données utilisées dans cette application proviennent du fichier "production.xlsx" et couvrent différents types de production comme le photovoltaïque, l'éolien, l'hydraulique, la bioénergie, la cogénération, et d'autres secteurs.

Fonctionnalités :
- Filtrage des données par année, région, département et commune.
- Visualisations interactives pour l'analyse de la production électrique.
- Onglets séparés pour différents types de production et informations géographiques détaillées.
- Fonctionnalité de téléchargement pour les données filtrées.

Prérequis :
Pour exécuter cette application, vous devez avoir R installé sur votre système ainsi que les packages R suivants :
- shiny
- shinydashboard
- shinythemes
- scales
- forcats
- wrapr
- gridExtra
- ggplot2movies
- tidyverse
- rlang
- DT
- plyr
- ggplot2
- ggnewscale
- stringr
- ggh4x
- dplyr
- readxl
- ggsci
- leaflet
- plotly

Installation :
1. Clonez ou téléchargez ce dépôt sur votre machine locale.	
2. Ouvrez le script R (app.R ou similaire) dans votre IDE R (comme RStudio).
3. Installez-les packages manquants en utilisant `install.packages(" nom_du_package ") `.
4. Définissez le répertoire de travail sur le dossier contenant le script et le fichier de données en utilisant `Sed(" chemin/vers/le/dossier ") `.
5. Exécutez l'application en lançant le script.

 Utilisation :
1. Lancez l'application dans R.
2. Utilisez la barre latérale pour sélectionner les filtres pour l'année, la région, le département et la commune.
3. Naviguez à travers les onglets pour explorer différentes visualisations.
4. Interagissez avec les graphiques pour des insights détaillés.
5. Téléchargez les données filtrées si nécessaire en utilisant l'onglet de téléchargement.

 Données :
Les données doivent être dans un fichier Excel nommé "production.xlsx". Assurez-vous que le fichier est dans le bon format et placé dans le même répertoire que le script de l'application.

 Contribution :
N'hésitez pas à forker le dépôt et à soumettre des pull requêtes. Vous pouvez également ouvrir des issues pour des bugs trouvés ou des demandes de fonctionnalités.

Assurez-vous de personnaliser ce modèle selon les caractéristiques et fonctionnalités spécifiques de votre application. Si votre application comporte des étapes de configuration supplémentaires ou des notes importantes, incluez-les dans les sections respectives. Application R Shiny : Production Électrique Annuelle
