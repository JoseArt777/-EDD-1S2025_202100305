program EDDMail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  EstructurasDatos,
  CorreoManager,
  InterfazGTK;

var
  Aplicacion: TInterfazEDDMail;

begin

  // Configurar aplicación
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;

  WriteLn('=================================');
  WriteLn('    EDDMail - Sistema de Correo');
  WriteLn('    Estructuras de Datos - USAC');
  WriteLn('=================================');
  WriteLn('Iniciando aplicación con Lazarus...');
  WriteLn('');
  WriteLn('Credenciales por defecto:');
  WriteLn('Email: root@edd.com');
  WriteLn('Password: root123');
  WriteLn('');

  try
    // Crear directorio de reportes
    if not DirectoryExists('Root-Reportes') then
      CreateDir('Root-Reportes');

    Aplicacion := TInterfazEDDMail.Create;
    try
      WriteLn('Sistema inicializado correctamente.');
      WriteLn('Mostrando interfaz gráfica...');
      WriteLn('');

      // Ejecutar aplicación
      Aplicacion.Ejecutar;

    finally
      Aplicacion.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error al ejecutar la aplicación: ', E.Message);
      WriteLn('Presione Enter para continuar...');
      ReadLn;
      Halt(1);
    end;
  end;

  WriteLn('Aplicación terminada correctamente.');
end.
