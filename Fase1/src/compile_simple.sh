#!/bin/bash

echo "Creando directorios..."
mkdir -p ../lib ../bin

echo "Compilando en directorio actual..."
fpc -Mobjfpc units/DataStructures.pas
if [ $? -ne 0 ]; then
    echo "Error compilando DataStructures"
    exit 1
fi

fpc -Mobjfpc units/BasicOperations.pas
fpc -Mobjfpc -Fu. units/SystemCore.pas
fpc -Mobjfpc -Fu. units/UserManager.pas
fpc -Mobjfpc -Fu. units/EmailManager.pas
fpc -Mobjfpc -Fu. units/ContactManager.pas
fpc -Mobjfpc -Fu. units/CommunityManager.pas
fpc -Mobjfpc -Fu. units/FileManager.pas
fpc -Mobjfpc -Fu. units/ReportGenerator.pas

echo "Compilando interfaces..."
fpc -Mobjfpc -Fu. ui/UIBase.pas
fpc -Mobjfpc -Fu. ui/LoginWindow.pas
fpc -Mobjfpc -Fu. ui/RootWindow.pas
fpc -Mobjfpc -Fu. ui/UserWindow.pas
fpc -Mobjfpc -Fu. ui/InboxWindow.pas
fpc -Mobjfpc -Fu. ui/ComposeWindow.pas
fpc -Mobjfpc -Fu. ui/ContactWindow.pas
fpc -Mobjfpc -Fu. ui/TrashWindow.pas
fpc -Mobjfpc -Fu. ui/ScheduleWindow.pas
fpc -Mobjfpc -Fu. ui/ScheduledWindow.pas
fpc -Mobjfpc -Fu. ui/ProfileWindow.pas
fpc -Mobjfpc -Fu. ui/ReportsWindow.pas

echo "Compilando programa principal..."
fpc -Mobjfpc -Fu. -o../bin/eddmail EDDMail.pas

echo "¡Listo! Ejecutar con: ../bin/eddmail"
