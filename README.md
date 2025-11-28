# EDDMail – Sistema de Correo Electrónico académico en Pascal

## Descripción del Proyecto

EDDMail es una aplicación de escritorio desarrollada en Pascal que implementa un sistema de correo electrónico, integrando estructuras de datos avanzadas para la gestión eficiente de usuarios, contactos, mensajes y comunidades. La arquitectura demuestra la aplicación práctica de listas, árboles, grafos y blockchain en la organización y visualización de información.

## Características Principales

- Registro y autenticación de usuarios.
- Envío, recepción y almacenamiento de correos electrónicos.
- Administración y gestión avanzada de contactos y comunidades.
- Visualización de mensajes y relaciones de contacto mediante grafos y árboles binarios.
- Generación de reportes y gráficos (blockchain de mensajes, grafo de contactos).
- Interfaz gráfica con Lazarus/LCL (GTK).
- Importación/exportación de datos en formato JSON y binario.

## Tecnologías Utilizadas

- **Lenguaje:** Pascal
- **Framework:** Lazarus IDE / Free Pascal Compiler (FPC)
- **Librerías:** fpjson (manejo de JSON), LCL, componentes estándar
- **Persistencia:** Archivos binarios y JSON
- **Herramientas complementarias:** Graphviz para visualización

## Requisitos Previos

- Lazarus IDE y/o Free Pascal Compiler instalados
- Graphviz (opcional, para generación de reportes gráficos)
- Sistema operativo: Windows, Linux o macOS
- Conocimientos básicos en programación estructurada

## Instrucciones de Instalación y Ejecución

1. **Clona el repositorio**
   ```bash
   git clone https://github.com/JoseArt777/-EDD-1S2025_202100305.git
   ```
2. **Abre el proyecto**
   - Desde Lazarus IDE, abre `Fase1/src/EDDMail.lpr`.
3. **Compila y ejecuta**
   - Compila el proyecto o ejecuta con Free Pascal Compiler.
   - Ingresa con las credenciales de ejemplo:
     ```
     Email: root@edd.com
     Password: root123
     ```
4. **Importa datos si lo deseas:** Utiliza las opciones de la interfaz para cargar información desde archivos JSON.

## Estructura del Proyecto

- `Fase1/src/EDDMail.lpr`: Entrada principal de la aplicación.
- `Fase1/src/backup/EDDMail.lpr`: Backup del sistema principal.
- `Fase1/src/estructurasdatos.pas`: Implementación de estructuras de datos (listas, árboles, grafos, blockchain).
- `Fase1/src/InterfazGTK.pas`: Interfaz gráfica.
- `Fase1/src/CorreoManager.pas`: Lógica de correo electrónico.
- `Tareas/`: Ejercicios y componentes del curso.
- Otros archivos `.pas`, `.lpi`, `.lpr`: Módulos y recursos auxiliares.

## Endpoints

Este sistema no expone endpoints HTTP/REST; funciona como aplicación de escritorio con interacción mediante interfaz gráfica y consola.

## Capturas o Ejemplos de Uso

### Ejemplo: Envío de correo
```pascal
CorreoManager.EnviarCorreo('origen@edd.com', 'destino@edd.com', 'Asunto', 'Mensaje de prueba');
```

### Ejemplo: Generación de gráficos de contactos
```pascal
EDDMailSystem.GenerarReporteGrafoContactos('contactos.dot');
// Visualiza el resultado con Graphviz
```

### Pantalla de bienvenida
```
=================================
    EDDMail - Sistema de Correo
    Estructuras de Datos - USAC
=================================
Iniciando aplicación con Lazarus...

Credenciales por defecto:
Email: root@edd.com
Password: root123
```

## Buenas Prácticas Implementadas

- Modularidad: separación por responsabilidades.
- Gestión eficiente de memoria.
- Comentarios descriptivos y documentación interna.
- Nomenclatura profesional y estructurada.
- Control de flujo robusto y manejo de errores.

## Aprendizajes Obtenidos

El desarrollo de EDDMail presenta la aplicación de estructuras de datos en la gestión de información, el uso de formatos de persistencia, la integración de gráficos y la programación de interfaces de usuario en Pascal. Se exploró el diseño y optimización funcional de un sistema integral de comunicaciones.

## Posibles Mejoras Futuras

- Implementación de protocolos de red simulados (SMTP/POP/IMAP).
- Optimización y ampliación de la interfaz gráfica.
- Cifrado de información sensible.
- Multiusuario y roles.
- Búsqueda avanzada y filtros dinámicos.
- Pruebas automatizadas.
- Internacionalización de la interfaz.

