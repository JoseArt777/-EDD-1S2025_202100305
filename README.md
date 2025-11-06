# Proyecto de Demostración: Visión por Computadora

Este repositorio agrupa todos los entregables solicitados para la materia y, en
particular, incluye un proyecto listo para clonar con una canalización completa
de visión por computadora. El objetivo es que puedas descargar el repositorio y
contar inmediatamente con un ejemplo funcional para explicar conceptos clave de
procesamiento de imágenes.

## ¿Qué contiene?

- `vision_demo/`: paquete en Python con la demo de visión por computadora.
- `vision_demo/assets/`: imagen sintética de ejemplo para ejecutar la canalización
  sin necesidad de archivos adicionales.
- `vision_demo/output/`: carpeta donde se guardan los resultados generados.
- `vision_demo/src/vision_pipeline.py`: implementación del flujo completo.
- Directorios históricos (`Fase1/`, `Tareas/`) que conservan otras entregas del
  curso y que no interfieren con la demo.

## Requisitos rápidos

1. Clona el repositorio:

   ```bash
   git clone https://github.com/tu-usuario/vision-demo-edd.git
   cd vision-demo-edd
   ```

2. (Opcional) Crea y activa un entorno virtual.

3. Instala las dependencias:

   ```bash
   pip install -r requirements.txt
   ```

4. Ejecuta la canalización con la imagen incluida:

   ```bash
   python -m vision_demo.src.vision_pipeline \
       --image vision_demo/assets/sample_scene.ppm \
       --output vision_demo/output
   ```

Si no se indica la ruta de salida, el script creará automáticamente la carpeta
`vision_demo/output/`. Puedes ajustar el número de clusters utilizados en la
segmentación añadiendo el parámetro `--clusters` (por ejemplo, `--clusters 4`).

## Resultados generados

Al finalizar la ejecución se generan los siguientes archivos en la carpeta de
salida:

- `01_grayscale.png` – Imagen convertida a escala de grises.
- `02_clahe.png` – Contraste mejorado mediante CLAHE.
- `03_edges.png` – Bordes detectados con Canny.
- `04_segmentation.png` – Segmentación basada en K-Means.
- `05_contours.png` – Contornos superpuestos a la imagen original.
- `06_histogram.png` – Histograma de color BGR.
- `metadata.json` – Archivo con datos de resolución, número de contornos,
  paleta detectada y clusters solicitados.

¡Listo! Con solo clonar este repositorio podrás mostrar de inmediato una demo
local de visión por computadora.
