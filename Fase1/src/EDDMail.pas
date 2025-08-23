program EDDMail;

{$mode objfpc}{$H+}

uses
  SysUtils, glib2, gdk2, gtk2,
  EstructurasDatos, InterfazGTK;

var
  Aplicacion: TInterfazEDDMail;

begin
  WriteLn('=================================');
  WriteLn('    EDDMail - Sistema de Correo');
  WriteLn('=================================');
  WriteLn('Iniciando aplicación...');
  
  try
    Aplicacion := TInterfazEDDMail.Create;
    try
      WriteLn('Interfaz creada exitosamente.');
      WriteLn('Ejecutando interfaz GTK...');
      Aplicacion.Ejecutar;
    finally
      Aplicacion.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error al ejecutar la aplicación: ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn('Aplicación terminada.');
end.
