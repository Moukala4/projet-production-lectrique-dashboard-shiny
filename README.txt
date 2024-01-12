Application R Shiny : Production �lectrique Annuelle 

Aper�u :
Cette application R Shiny offre une analyse interactive des donn�es de production �lectrique annuelle. Elle propose une interface compl�te pour filtrer et visualiser les donn�es de production �lectrique selon diff�rentes dimensions telles que l'ann�e, la r�gion, le d�partement et la commune. Les donn�es utilis�es dans cette application proviennent du fichier "production.xlsx" et couvrent diff�rents types de production comme le photovolta�que, l'�olien, l'hydraulique, la bio�nergie, la cog�n�ration, et d'autres secteurs.

Fonctionnalit�s :
- Filtrage des donn�es par ann�e, r�gion, d�partement et commune.
- Visualisations interactives pour l'analyse de la production �lectrique.
- Onglets s�par�s pour diff�rents types de production et informations g�ographiques d�taill�es.
- Fonctionnalit� de t�l�chargement pour les donn�es filtr�es.

Pr�requis :
Pour ex�cuter cette application, vous devez avoir R install� sur votre syst�me ainsi que les packages R suivants :
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
1. Clonez ou t�l�chargez ce d�p�t sur votre machine locale.	
2. Ouvrez le script R (app.R ou similaire) dans votre IDE R (comme RStudio).
3. Installez-les packages manquants en utilisant `install.packages(" nom_du_package ") `.
4. D�finissez le r�pertoire de travail sur le dossier contenant le script et le fichier de donn�es en utilisant `Sed(" chemin/vers/le/dossier ") `.
5. Ex�cutez l'application en lan�ant le script.

 Utilisation :
1. Lancez l'application dans R.
2. Utilisez la barre lat�rale pour s�lectionner les filtres pour l'ann�e, la r�gion, le d�partement et la commune.
3. Naviguez � travers les onglets pour explorer diff�rentes visualisations.
4. Interagissez avec les graphiques pour des insights d�taill�s.
5. T�l�chargez les donn�es filtr�es si n�cessaire en utilisant l'onglet de t�l�chargement.

 Donn�es :
Les donn�es doivent �tre dans un fichier Excel nomm� "production.xlsx". Assurez-vous que le fichier est dans le bon format et plac� dans le m�me r�pertoire que le script de l'application.

 Contribution :
N'h�sitez pas � forker le d�p�t et � soumettre des pull requ�tes. Vous pouvez �galement ouvrir des issues pour des bugs trouv�s ou des demandes de fonctionnalit�s.

Assurez-vous de personnaliser ce mod�le selon les caract�ristiques et fonctionnalit�s sp�cifiques de votre application. Si votre application comporte des �tapes de configuration suppl�mentaires ou des notes importantes, incluez-les dans les sections respectives. Application R Shiny : Production �lectrique Annuelle
