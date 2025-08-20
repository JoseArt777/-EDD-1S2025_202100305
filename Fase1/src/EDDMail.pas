program EDDMail;

{$mode objfpc}{$H+}

uses
  GTK2, GDK2, SystemCore, DataStructures, LoginWindow, RootWindow, UserWindow,
  ReportGenerator;

begin
  // Inicializar GTK
  gtk_init(@argc, @argv);

  // Crear ventana de login
  LoginWin := TLoginWindow.Create;

  // Mostrar ventana de login
  LoginWin.Show;

  // Inicializar bucle principal de GTK
  gtk_main;

  // Limpiar recursos
  if LoginWin <> nil then
    LoginWin.Free;
  if RootWin <> nil then
    RootWin.Free;
  if UserWin <> nil then
    UserWin.Free;
end.

