# Demo de Visión por Computadora

Este proyecto crea una canalización completa de visión por computadora pensada
para propósitos educativos. Permite demostrar conceptos fundamentales como:

- Lectura y preprocesamiento de imágenes.
- Mejora de contraste con **CLAHE**.
- Detección de bordes con **Canny**.
- Segmentación no supervisada mediante **K-Means**.
- Detección y visualización de contornos.
- Análisis estadístico básico a través de histogramas de color y metadatos.

La solución funciona de forma local y genera artefactos listos para su análisis
posterior.

## Requisitos

- Python 3.10 o superior.
- Se recomienda crear un entorno virtual con `venv` o `conda`.

Instala las dependencias ejecutando:

```bash
pip install -r requirements.txt
```

## Estructura del proyecto

```
vision_demo/
├── assets/
│   └── sample_scene.ppm        # Imagen sintética para pruebas inmediatas
├── output/                     # Carpeta donde se almacenan los resultados
├── src/
│   ├── __init__.py
│   └── vision_pipeline.py      # Implementación de la canalización CV
└── README.md                   # Este documento
```

## Uso

```bash
python -m vision_demo.src.vision_pipeline --image vision_demo/assets/sample_scene.ppm \
    --output vision_demo/output
```

Parámetros opcionales:

- `--image`: ruta a una imagen propia en formato compatible con OpenCV
  (JPG, PNG, BMP, PPM, entre otros).
- `--output`: carpeta donde se guardarán los resultados.
- `--clusters`: número de grupos para la segmentación por K-Means (3 por defecto).

El script imprimirá en consola la lista de archivos generados. En la carpeta de
salida encontrarás:

- `01_grayscale.png`: conversión a escala de grises.
- `02_clahe.png`: imagen con contraste mejorado mediante CLAHE.
- `03_edges.png`: bordes detectados mediante Canny.
- `04_segmentation.png`: segmentación por K-Means.
- `05_contours.png`: contornos detectados dibujados sobre la imagen original.
- `06_histogram.png`: histograma de color BGR.
- `metadata.json`: metadatos con resolución, número de contornos, paleta de la
  segmentación y clusters solicitados/encontrados.

## Extensiones posibles

- Sustituir la imagen de ejemplo por capturas reales o frames de vídeo.
- Incorporar detección de rostros con clasificadores Haar o modelos DNN.
- Añadir seguimiento de objetos usando algoritmos como CamShift o SORT.
- Exportar los resultados a un dashboard interactivo (por ejemplo, con Streamlit).

¡Listo! Con este proyecto tienes un punto de partida completo para explicar los
componentes básicos de la visión por computadora moderna.
