#!/bin/bash

echo "=== EDDMail - Compilador con GTK Corregido ==="
echo ""

# Rutas GTK confirmadas para Ubuntu 24.04
GTK_PATH="/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/gtk2"

# Verificar que la ruta GTK existe
if [ ! -d "$GTK_PATH" ]; then
    echo "ERROR: No se encuentra la ruta GTK en $GTK_PATH"
    exit 1
fi

echo "✓ GTK Path encontrado: $GTK_PATH"
echo ""

# Limpiar compilación anterior
echo "🧹 Limpiando archivos previos..."
rm -f units/*.o units/*.ppu
rm -f ui/*.o ui/*.ppu
rm -f *.o *.ppu test_gtk
rm -rf ../lib ../bin

echo "📁 Creando directorios..."
mkdir -p ../lib ../bin
echo ""

# Compilar unidades principales
echo "=== COMPILANDO UNIDADES PRINCIPALES ==="

units=(
    "DataStructures"
    "BasicOperations"
    "SystemCore"
    "UserManager"
    "EmailManager"
    "ContactManager"
    "CommunityManager"
    "FileManager"
    "ReportGenerator"
)

for unit in "${units[@]}"; do
    echo "Compilando $unit..."
    if ! fpc -Mobjfpc -FE../lib -FU../lib units/$unit.pas; then
        echo "❌ Error compilando $unit"
        exit 1
    fi
done

echo "✅ Unidades principales compiladas exitosamente"
echo ""

# Compilar interfaces GTK
echo "=== COMPILANDO INTERFACES GTK ==="

# Parámetros de compilación con GTK
COMPILE_OPTS="-Mobjfpc -FE../lib -FU../lib -Fi../lib -Fu$GTK_PATH"

ui_units=(
    "UIBase"
    "LoginWindow"
    "RootWindow"
    "UserWindow"
    "InboxWindow"
    "ComposeWindow"
    "ContactWindow"
    "TrashWindow"
    "ScheduleWindow"
    "ScheduledWindow"
    "ProfileWindow"
    "ReportsWindow"
)

failed_units=0

for unit in "${ui_units[@]}"; do
    echo "Compilando $unit..."
    if fpc $COMPILE_OPTS ui/$unit.pas; then
        echo "✓ $unit compilado exitosamente"
    else
        echo "⚠ Error compilando $unit"
        failed_units=$((failed_units + 1))
    fi
done

if [ $failed_units -gt 0 ]; then
    echo ""
    echo "⚠ $failed_units interfaces fallaron. Continuando con programa principal..."
else
    echo "✅ Todas las interfaces GTK compiladas exitosamente"
fi

echo ""

# Compilar programa principal
echo "=== COMPILANDO PROGRAMA PRINCIPAL ==="
if fpc $COMPILE_OPTS -o../bin/eddmail EDDMail.pas; then
    echo ""
    echo "🎉 ¡COMPILACIÓN EXITOSA!"
    echo "📁 Ejecutable creado en: ../bin/eddmail"
    echo ""
    echo "🚀 Para ejecutar:"
    echo "   cd .."
    echo "   ./bin/eddmail"
    echo ""
    echo "🔍 Para depurar:"
    echo "   cd .."
    echo "   gdb ./bin/eddmail"
    echo ""
else
    echo ""
    echo "❌ ERROR en la compilación del programa principal"
    echo ""
    echo "💡 Creando versión de consola como alternativa..."
    
    # Compilar versión de consola si falla la GUI
    if [ -f "test_console.pas" ]; then
        if fpc -Mobjfpc -FE../bin -FU../lib -Fi../lib -o../bin/eddmail_console test_console.pas; then
            echo "✅ Versión de consola creada en: ../bin/eddmail_console"
            echo "   Ejecutar con: ../bin/eddmail_console"
        fi
    fi
    
    exit 1
fi
