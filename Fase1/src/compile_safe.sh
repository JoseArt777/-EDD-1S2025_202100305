#!/bin/bash

echo "=== EDDMail - Compilador SEGURO ==="
echo ""

# Configuración GTK
GTK_PATH="/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/gtk2"

if [ ! -d "$GTK_PATH" ]; then
    echo "ERROR: No se encuentra GTK en $GTK_PATH"
    exit 1
fi

# Limpiar
echo "🧹 Limpiando..."
rm -f units/*.o units/*.ppu ui/*.o ui/*.ppu *.o *.ppu
rm -rf ../lib ../bin
mkdir -p ../lib ../bin

# Compilar solo las unidades básicas primero
echo "📦 Compilando unidades básicas..."
basic_units=(
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

for unit in "${basic_units[@]}"; do
    echo "  Compilando $unit..."
    if ! fpc -Mobjfpc -FE../lib -FU../lib units/$unit.pas; then
        echo "❌ Error en $unit"
        exit 1
    fi
done

echo "✅ Unidades básicas OK"

# Compilar UIBase primero (crítico)
echo "🖼️ Compilando UIBase..."
if fpc -Mobjfpc -FE../lib -FU../lib -Fi../lib -Fu$GTK_PATH ui/UIBase.pas; then
    echo "✅ UIBase compilado"
else
    echo "❌ Error crítico en UIBase"
    exit 1
fi

# Compilar interfaces una por una
echo "🖥️ Compilando interfaces..."
ui_files=(
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

COMPILE_OPTS="-Mobjfpc -FE../lib -FU../lib -Fi../lib -Fu$GTK_PATH"
compiled_count=0
total_count=${#ui_files[@]}

for ui_file in "${ui_files[@]}"; do
    echo "  Compilando $ui_file..."
    if fpc $COMPILE_OPTS ui/$ui_file.pas; then
        echo "    ✅ $ui_file OK"
        compiled_count=$((compiled_count + 1))
    else
        echo "    ⚠️ $ui_file falló - continuando..."
    fi
done

echo ""
echo "📊 Resumen interfaces: $compiled_count/$total_count compiladas"

# Compilar programa principal
echo "🚀 Compilando programa principal..."
if fpc $COMPILE_OPTS -o../bin/eddmail EDDMail.pas; then
    echo ""
    echo "🎉 ¡COMPILACIÓN EXITOSA!"
    echo ""
    echo "📁 Ejecutable: ../bin/eddmail"
    echo "🔧 Para ejecutar: cd .. && ./bin/eddmail"
    echo ""
    echo "📋 Estadísticas:"
    echo "   - Unidades básicas: ${#basic_units[@]}/${#basic_units[@]} ✅"
    echo "   - Interfaces UI: $compiled_count/$total_count"
    echo "   - Programa principal: ✅"
    echo ""
else
    echo ""
    echo "❌ Error en programa principal"
    echo ""
    echo "🔍 Verificar estos archivos:"
    echo "   - EDDMail.pas"
    echo "   - ui/UIBase.pas"
    echo "   - ui/RootWindow.pas"
    echo ""
    exit 1
fi
