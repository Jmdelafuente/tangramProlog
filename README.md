# tangramProlog
Implementación de heurísticas para el juego Tangram en Ciao Prolog.
Para más detalle revisar el pdf adjunto.
El mismo representa una entrega de la materia Inteligencia Artificial de la Facultad de Informática de la Universidad Nacional del Comahue.
Sientase libre de descargar el código, revisarlo y utilizarlo -¡y mejorarlo!-.
Por favor mantenga la autoría, la licencia se encuentra adjunta en el repositorio, recurra a ella antes de cualquier otra acción.

Utilización:

init(M) instancia la matriz del estado inicial en la variable M.
asearch1(M,Path) para calcular el camino segun A*.

Si se quiere modificar el estado inicial es necesario cambiar el predicado: 'matrizI' y 'listaFichasI' considerando que cada ficha que se remueva de 'matrizI' debe ser añadida en 'listaFichasI'.

Se pueden generar nuevas fichas añadiendo un predicado que establezca su insercion llamado con el nombre del color de la ficha, añadiendo los valores de la ficha en 'recuperar_ficha' segun lo documentado en los comentarios.

Este mismo codigo puede ser descargado desde GitHub:
https://github.com/jmdelafuente22/tangramProlog
