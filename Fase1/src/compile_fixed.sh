#!/bin/bash

echo "=== EDDMail - Compilador Corregido ==="

# Limpiar archivos previos
find . -name "*.o" -delete
find . -name "*.ppu" -delete

# Compilar unidades principales (sin errores)
echo "Compilando unidades principales..."
cd units
for unit in DataStructures BasicOperations SystemCore UserManager EmailManager ContactManager FileManager; do
    echo "  - $unit..."
    fpc -Mobjfpc -c $unit.pas
done
cd ..

# Compilar interfaces GTK (solo las que funcionan)
echo "Compilando interfaces GTK..."
cd ui
for ui_unit in UIBase InboxWindow ComposeWindow ContactWindow TrashWindow ScheduleWindow ScheduledWindow ProfileWindow ReportsWindow; do
    echo "  - $ui_unit..."
    fpc -Mobjfpc -WG -Fu../units -c $ui_unit.pas 2>/dev/null || echo "    ⚠ $ui_unit falló"
done
cd ..

# Intentar compilar programa principal
echo "Compilando programa principal..."
if [ -f "EDDMail.pas" ]; then
    fpc -Mobjfpc -WG -Fu./units -Fu./ui EDDMail.pas -o EDDMail
elif [ -f "test_simple.pas" ]; then
    fpc -Mobjfpc -WG -Fu./units test_simple.pas -o EDDMail
else
    echo "❌ No se encontró archivo principal"
fi

echo "Compilación terminada."
