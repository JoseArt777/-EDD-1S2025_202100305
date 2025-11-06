"""Pipeline de visión por computadora para demostración educativa.

Este módulo ofrece un flujo de procesamiento que cubre tareas comunes
como la carga de imágenes, preprocesamiento, detección de bordes,
segmentación y generación de reportes visuales.
"""
from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List

import cv2
import matplotlib.pyplot as plt
import numpy as np


@dataclass
class PipelineResults:
    """Estructura para almacenar la ruta de los artefactos generados."""

    grayscale_path: Path
    clahe_path: Path
    edges_path: Path
    segmentation_path: Path
    contours_path: Path
    histogram_path: Path
    metadata_path: Path


class VisionPipeline:
    """Implementa un flujo completo de visión por computadora."""

    def __init__(self, image_path: Path, output_dir: Path) -> None:
        self.image_path = image_path
        self.output_dir = output_dir
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.original = self._load_image(image_path)

    @staticmethod
    def _load_image(path: Path) -> np.ndarray:
        if not path.exists():
            raise FileNotFoundError(f"La imagen {path} no existe")
        image = cv2.imread(str(path))
        if image is None:
            raise ValueError(
                "OpenCV no pudo cargar la imagen. Comprueba el formato o permisos."
            )
        return image

    def _save_image(self, array: np.ndarray, filename: str) -> Path:
        destination = self.output_dir / filename
        cv2.imwrite(str(destination), array)
        return destination

    def to_grayscale(self) -> Path:
        gray = cv2.cvtColor(self.original, cv2.COLOR_BGR2GRAY)
        return self._save_image(gray, "01_grayscale.png")

    def enhance_contrast(self, gray_path: Path) -> Path:
        gray = cv2.imread(str(gray_path), cv2.IMREAD_GRAYSCALE)
        clahe = cv2.createCLAHE(clipLimit=2.0, tileGridSize=(8, 8))
        enhanced = clahe.apply(gray)
        return self._save_image(enhanced, "02_clahe.png")

    def detect_edges(self, clahe_path: Path) -> Path:
        clahe = cv2.imread(str(clahe_path), cv2.IMREAD_GRAYSCALE)
        blurred = cv2.GaussianBlur(clahe, (5, 5), 0)
        edges = cv2.Canny(blurred, threshold1=80, threshold2=160)
        return self._save_image(edges, "03_edges.png")

    def segment_with_kmeans(self, k: int = 3) -> Path:
        data = self.original.reshape((-1, 3)).astype(np.float32)
        criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 40, 0.2)
        _, labels, centers = cv2.kmeans(
            data, k, None, criteria, attempts=10, flags=cv2.KMEANS_RANDOM_CENTERS
        )
        centers = np.uint8(centers)
        segmented = centers[labels.flatten()].reshape(self.original.shape)
        return self._save_image(segmented, "04_segmentation.png")

    def draw_contours(self, edges_path: Path) -> Path:
        edges = cv2.imread(str(edges_path), cv2.IMREAD_GRAYSCALE)
        contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        contour_img = self.original.copy()
        cv2.drawContours(contour_img, contours, -1, (0, 0, 255), 2)
        return self._save_image(contour_img, "05_contours.png")

    def save_histogram(self) -> Path:
        colors = ("b", "g", "r")
        plt.figure(figsize=(8, 4))
        for idx, color in enumerate(colors):
            hist = cv2.calcHist([self.original], [idx], None, [256], [0, 256])
            plt.plot(hist, color=color)
            plt.xlim([0, 256])
        plt.title("Histograma de color (BGR)")
        plt.xlabel("Intensidad")
        plt.ylabel("Número de pixeles")
        histogram_path = self.output_dir / "06_histogram.png"
        plt.tight_layout()
        plt.savefig(histogram_path)
        plt.close()
        return histogram_path

    def save_metadata(
        self, edges_path: Path, segmentation_path: Path, clusters: int
    ) -> Path:
        edges = cv2.imread(str(edges_path), cv2.IMREAD_GRAYSCALE)
        contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

        segmented = cv2.imread(str(segmentation_path))
        palette: List[List[int]] = np.unique(segmented.reshape(-1, 3), axis=0).tolist()

        metadata: Dict[str, object] = {
            "archivo_origen": str(self.image_path),
            "resolucion": {
                "ancho": int(self.original.shape[1]),
                "alto": int(self.original.shape[0]),
                "canales": int(self.original.shape[2]),
            },
            "numero_contornos": len(contours),
            "paleta_segmentacion": palette,
            "clusters_solicitados": clusters,
            "clusters_encontrados": len(palette),
        }
        metadata_path = self.output_dir / "metadata.json"
        metadata_path.write_text(json.dumps(metadata, indent=2, ensure_ascii=False))
        return metadata_path

    def run(self, clusters: int = 3) -> PipelineResults:
        """Ejecuta la canalización completa con el número de clusters indicado."""

        grayscale = self.to_grayscale()
        clahe = self.enhance_contrast(grayscale)
        edges = self.detect_edges(clahe)
        segmentation = self.segment_with_kmeans(k=clusters)
        contours = self.draw_contours(edges)
        histogram = self.save_histogram()
        metadata = self.save_metadata(edges, segmentation, clusters=clusters)
        return PipelineResults(
            grayscale_path=grayscale,
            clahe_path=clahe,
            edges_path=edges,
            segmentation_path=segmentation,
            contours_path=contours,
            histogram_path=histogram,
            metadata_path=metadata,
        )


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Ejecuta una canalización de visión por computadora que incluye "
            "preprocesamiento, detección de bordes, segmentación y generación "
            "de reportes gráficos."
        )
    )
    parser.add_argument(
        "--image",
        type=Path,
        default=Path(__file__).resolve().parent.parent / "assets" / "sample_scene.ppm",
        help="Ruta a la imagen que se desea procesar.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).resolve().parent.parent / "output",
        help="Directorio donde se almacenarán los resultados.",
    )
    parser.add_argument(
        "--clusters",
        type=int,
        default=3,
        help="Número de clusters para la segmentación por k-means.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_arguments()
    pipeline = VisionPipeline(args.image, args.output)
    results = pipeline.run(clusters=args.clusters)

    print("Procesamiento completado. Archivos generados:")
    for field, value in results.__dict__.items():
        print(f"- {field}: {value}")


if __name__ == "__main__":
    main()
