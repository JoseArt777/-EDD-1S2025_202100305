#!/bin/bash

echo "=== Diagnóstico de archivos EDDMail ==="
echo ""

critical_files=(
    "ui/UIBase.pas"
    "ui/RootWindow.pas"
    "ui/LoginWindow.pas"
    "EDDMail.pas"
)

echo "🔍 Verificando archivos críticos:"
for file in "${critical_files[@]}"; do
    if [ -f "$file" ]; then
        lines=$(wc -l < "$file")
        echo "  ✅ $file ($lines líneas)"
    else
        echo "  ❌ $file - NO ENCONTRADO"
    fi
done

echo ""
echo "🔍 Verificando errores de sintaxis comunes:"

# Verificar imports duplicados
echo "📋 Imports duplicados:"
for file in ui/*.pas; do
    if [ -f "$file" ]; then
        duplicates=$(grep -o "UIBase" "$file" | wc -l)
        if [ $duplicates -gt 1 ]; then
            echo "  ⚠️ $(basename $file): UIBase aparece $duplicates veces"
        fi
    fi
done

# Verificar problemas de String/PChar
echo ""
echo "📋 Problemas String/PChar:"
for file in ui/*.pas; do
    if [ -f "$file" ]; then
        if grep -q "gtk_combo_box_get_active_text" "$file"; then
            if ! grep -q "PChar(gtk_combo_box_get_active_text" "$file"; then
                echo "  ⚠️ $(basename $file): falta conversión PChar"
            fi
        fi
    fi
done

# Verificar GLib2
echo ""
echo "📋 Imports GLib2:"
for file in ui/*.pas; do
    if [ -f "$file" ]; then
        if grep -q "GTK2" "$file" && ! grep -q "GLib2" "$file"; then
            echo "  ⚠️ $(basename $file): falta GLib2"
        fi
    fi
done

echo ""
echo "✅ Diagnóstico completado"
