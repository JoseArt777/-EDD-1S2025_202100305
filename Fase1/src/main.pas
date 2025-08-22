program EDDMail;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, glib2, gtk2,
  UStructures, ULogin, URoot, UUser,
  login_interface, user_interface, root_interface;

var
  LoginWindow: TLoginInterface;

begin
  // Inicializar GTK
  gtk_init(@argc, @argv);

  WriteLn('=== INICIANDO EDDMAIL ===');
  WriteLn('Sistema de correo electrónico');
  WriteLn('Estructuras de Datos - USAC');
  WriteLn('');

  try
    // Crear y mostrar ventana de login
    LoginWindow := TLoginInterface.Create;
    LoginWindow.Show;

    // Iniciar loop principal de GTK
    gtk_main;

  except
    on E: Exception do
    begin
      WriteLn('Error fatal: ', E.Message);
    end;
  end;

  WriteLn('');
  WriteLn('=== FINALIZANDO EDDMAIL ===');
end.

