#!/bin/bash

echo "=== EDDMail - Compilación con ERRORES CORREGIDOS ==="
echo ""

# Configuración GTK
GTK_PATH="/usr/lib/x86_64-linux-gnu/fpc/3.2.2/units/x86_64-linux/gtk2"

if [ ! -d "$GTK_PATH" ]; then
    echo "❌ ERROR: No se encuentra GTK en $GTK_PATH"
    exit 1
fi

echo "✅ GTK Path: $GTK_PATH"

# Limpiar completamente
echo ""
echo "🧹 Limpiando archivos de compilación..."
rm -f units/*.o units/*.ppu ui/*.o ui/*.ppu *.o *.ppu
rm -rf ../lib ../bin
mkdir -p ../lib ../bin

# Compilar unidades básicas
echo ""
echo "📦 Compilando unidades principales..."
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
    echo "  📄 Compilando $unit..."
    if fpc -Mobjfpc -FE../lib -FU../lib units/$unit.pas >/dev/null 2>&1; then
        echo "    ✅ $unit OK"
    else
        echo "    ❌ $unit FALLÓ"
        echo "       Ejecutando con detalles:"
        fpc -Mobjfpc -FE../lib -FU../lib units/$unit.pas
        exit 1
    fi
done

echo "✅ Todas las unidades básicas compiladas"

# Compilar interfaces UI
echo ""
echo "🖥️ Compilando interfaces UI..."
COMPILE_OPTS="-Mobjfpc -FE../lib -FU../lib -Fi../lib -Fu$GTK_PATH"

# Compilar UIBase PRIMERO (crítico)
echo "  🎯 Compilando UIBase (crítico)..."
if fpc $COMPILE_OPTS ui/UIBase.pas >/dev/null 2>&1; then
    echo "    ✅ UIBase OK"
else
    echo "    ❌ UIBase FALLÓ - CRÍTICO"
    fpc $COMPILE_OPTS ui/UIBase.pas
    exit 1
fi

# Compilar el resto de interfaces
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

compiled=0
failed=0

for ui_file in "${ui_files[@]}"; do
    echo "  📄 Compilando $ui_file..."
    if fpc $COMPILE_OPTS ui/$ui_file.pas >/dev/null 2>&1; then
        echo "    ✅ $ui_file OK"
        compiled=$((compiled + 1))
    else
        echo "    ⚠️ $ui_file FALLÓ"
        failed=$((failed + 1))
    fi
done

total=$((compiled + failed))
echo ""
echo "📊 Resumen UI: $compiled/$total compiladas, $failed fallidas"

# Compilar programa principal
echo ""
echo "🚀 Compilando programa principal..."
if fpc $COMPILE_OPTS -o../bin/eddmail EDDMail.pas; then
    echo ""
    echo "🎉 ¡COMPILACIÓN EXITOSA!"
    echo ""
    echo "📁 Ejecutable: ../bin/eddmail"
    echo "🏃 Para ejecutar: cd .. && ./bin/eddmail"
    echo ""
    echo "📈 Estadísticas finales:"
    echo "   ✅ Unidades básicas: ${#basic_units[@]}/${#basic_units[@]}"
    echo "   ✅ UIBase: 1/1"
    echo "   📊 Interfaces UI: $compiled/$total"
    echo "   🎯 Programa principal: ✅"
    echo ""
    if [ $failed -gt 0 ]; then
        echo "⚠️ Nota: $failed interfaces fallaron pero el programa principal compiló correctamente"
    fi
else
    echo ""
    echo "❌ ERROR en el programa principal"
    echo ""
    echo "🔍 Verificando dependencias críticas..."
    
    # Verificar archivos críticos
    critical_files=("ui/UIBase.pas" "ui/LoginWindow.pas" "EDDMail.pas")
    for file in "${critical_files[@]}"; do
        if [ -f "$file" ]; then
            echo "  ✅ $file existe"
        else
            echo "  ❌ $file FALTA"
        fi
    done
    
    exit 1
fi
