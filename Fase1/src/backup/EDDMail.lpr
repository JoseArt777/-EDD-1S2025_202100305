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
  Forms, SysUtils,
  EstructurasDatos, InterfazGTK
  { you can add units after this };

{$R *.res}

var
  Aplicacion: TInterfazEDDMail;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  WriteLn('=================================');
  WriteLn('    EDDMail - Sistema de Correo');
  WriteLn('    Estructuras de Datos - USAC');
  WriteLn('=================================');
  WriteLn('Iniciando aplicación...');
  WriteLn('');
  WriteLn('Credenciales por defecto:');
  WriteLn('Email: root@edd.com');
  WriteLn('Password: root123');
  WriteLn('');

  try
    if not DirectoryExists('Root-Reportes') then
      CreateDir('Root-Reportes');

    Aplicacion := TInterfazEDDMail.Create;
    try
      WriteLn('Sistema inicializado correctamente.');
      Aplicacion.Ejecutar;
    finally
      Aplicacion.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ReadLn;
      Halt(1);
    end;
  end;
end.
