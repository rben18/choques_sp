# choques_sp
Este es el repositorio para analizar choques en el municipio de San Pedro. Aquí se puede encontrar el codigo de una "Shiny App" para visualizar en un mapa de OpenStreetMap la cantidad de choques que hubo en 20 distintos cruces en San Pedro. También se puede ver información de otros accidentes que sucedieron en otros años. 

Los choques para los cruces ocurrieron entre Enero del 2014 y Septiembre del 2016. La información de los accidentes son entre Enero del 2016 y Septiembre del 2018. Los datos vienen de la página de "Datos Abiertos" en la página del municipio de San Pedro Garza García: http://datosabiertos.sanpedro.gob.mx/pagesda/Default.aspx?

- Tab "Accidentes en Cruceros":

Este tab demuestra la cantidad de choques que hubo en 20 distintos cruces de San Pedro, por año. Si le damos "click" a los distintos circulos en el mapa, podemos ver las calles que componen el cruce junto con la cantidad de choques que hubo en el periodo elegido. Puedes filtrar los años que quieras ver en el "checkbox" del lado y después darle "click" al botón de ejecutar.

- Tab "Resultados de Accidentes"

Este tab visualiza en forma de histograma 4 visualizaciones. El municipio catalogó distintos choques por tipo de accidente y público las cantidades a nivel mensual. El visualizador explora los tipos de accidente que son "Lesionado", "Alcohol", "Menores", "Muertes". Puedes ajustar las fechas que quieras visualizar con el deslizador a la izquierda y automáticamente se actualizarán las gráficas. 

- Prerequisitos:

Instalar R versión 3.6.2 con las librerias de shiny, tidyverse, leaflet, sf, DT y zoo.

- Disclaimer

Un pequeño "disclaimer" sobre esta herramienta es que ésta es más para fines exploratorios que para fines conclusivos. El objetivo de este proyecto es demostrar como se pueden visualizar datos de distintas maneras utilizando las herramientas open source de R y Shiny. 

Hay poca información sobre que exactamente representan los datos ni como se recopilaron. Se mandó un correo al departamento de Datos Abiertos de San Pedro para saber más sobre los datos y no he recibio respuesta.

Dentro de la información de "Resultados de Accidentes", el significado de que es un accidente puede variar. A mi interpretación un caso de "accidente" es un accidente vial, ya que estos datos estaban en la sección de "Seguridad y Vialidad".
A su vez, en un accidente, no está claro si un caso de "menores" significa que el conductor era un/una menor o si sólo había algún menor involucrado (pudiera ser como pasajero). Además, no sabemos si algunos estos casos pueden ser del mismo accidente. Es decir, no sabemos si un caso de un menor alcoholizado sea contado como un caso en "Menores" y en "Alcohol" o en sóla una de estas categorías. 

- Siguientes pasos y preguntas:
1) Estos datos no se actualizan de manera frecuente. Sería bueno que el gobierno los actualizara de manera más frecuente y con un catálogo e información sobre las distintas variables.
2) A su vez, más información sobre los distintos casos estaría bueno. Tener los distintos casos desglosados para después ser agrupados de distintas maneras (por caso, por semana, mes, ubicación, etc.).
3) ¿Cómo se pudieran integrar estos datos con los de otras fuentes, como las aseguradoras?
4) Estos son los casos que San Pedro tiene registrados, ¿cuantos se "resolveran" de manera informal sin involucrar a las autoridades?
5) Con esta información, ¿qué se puede hacer al respecto? Por ejemplo, si vemos que hay muchos casos de "Alcohol" en todo el año, quizá ser más estricto con las antialcoholicas por más tiempo aparte de la época Navideña. A su vez, promocionar de una distinta manera el hecho de no manejar y tomar. Si hay muchos casos de menores chocando, ¿habrá que mejorar los cursos de manejo? Estas son las preguntas que pueden salir con un análisis de estos datos. 

Visualizar datos puede desatar a la curiosidad y traer grandes cambios. Tener datos es la mitad de la batalla, también hay que saber usarlos.



