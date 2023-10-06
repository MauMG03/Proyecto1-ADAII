# Proyecto 1 - Análisis y Diseño de Algoritmos II (2023-II).
## Integrantes
* Mauricio Muñoz Gutierrez
* Paul Rodrigo Rojas Guerrero
* Juan Sebastian Getial Getial
* Nicolas Fernando Huertas Cadavid

## Archivos entregados.

* Se entrega un PDF donde se describe todo el trabajo realizado.
* Se entrega codigo fuente, los archivos de pruebas se encuentran en src/test, los archivos con los codigos de las soluciones, los generadores de prueba, la interfaz y demás se encuentran en src/main.

## Instrucciones de uso.

Para efectos prácticos, se obtará por ejecutar la aplicación directamente desde la interfaz de consola de sbt. Dentro de la carpeta donde se encuentra el archivo .sbt, correr el siguiente comando. Notar que es necesario tener sbt instalado https://www.scala-sbt.org/

> sbt

Una vez se carguen las dependencia correr el siguiente comando:

> ~run

Se preparan las dependencias de la aplicación. Una vez se solicite qué clase se va a cargar, elegir la opción Main (opcion 2).

A partir de ahi podra seleccionar el archivo de entrada, asi como optar por verlos antes de ejecutar el algoritmo que seleccione. Luego debe seleccionar un directorio donde se guardará el archivo de salida, cuando el algoritmo arroje una respuesta se desplegará una ventana donde podra ver la solución cálculada.


Solución de Problemas.

Si la aplicación es cerrada y nuevamente se intenta abrir con el comando '~run' es posible que se lance un error por intentar importar modulos que ya están cargados, y es que estos permanecen así durante la sesión de la consola sbt. Sin embargo, cerrar y volver a abrir la consola basta para corregir esto, ya sea presionando ctrl+c o escribiendo el comando 'exit'. 
