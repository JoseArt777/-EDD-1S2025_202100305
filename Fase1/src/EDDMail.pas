program EDDMail;

{$mode objfpc}{$H+}

uses
  GTK2, GDK2, SysUtils, Classes,
  DataStructures, SystemCore, UserManager, EmailManager, ContactManager,
  CommunityManager, FileManager, BasicOperations, ReportGenerator,
  LoginWindow, RootWindow, UserWindow;

var
  LoginWin: TLoginWindow;
  RootWin: TRootWindow;
  UserWin: TUserWindow;

begin
  // Inicializar GTK
  gtk_init(@argc, @argv);

  // Inicializar el sistema core
  SystemCore.Initialize;

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

  // Finalizar sistema core
  SystemCore.Finalize;
end.
