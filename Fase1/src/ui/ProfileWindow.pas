unit ProfileWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DataStructures, SystemCore, UIBase, UserManager;

type
  TProfileWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FNameEntry: PGtkWidget;
    FUserEntry: PGtkWidget;
    FEmailLabel: PGtkWidget;
    FPhoneEntry: PGtkWidget;
    FUpdateButton: PGtkWidget;
    FCancelButton: PGtkWidget;
    FStatusLabel: PGtkWidget;

    function ValidateForm: Boolean;
    procedure ClearForm;

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create(AParent: PGtkWidget = nil);
    destructor Destroy; override;
    procedure LoadCurrentUser;
    procedure UpdateProfile;
  end;

// Callbacks
procedure OnUpdateClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnCancelClicked(widget: PGtkWidget; data: gpointer); cdecl;

implementation

constructor TProfileWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Actualizar Perfil', 450, 350, AParent);
  LoadCurrentUser;
end;

destructor TProfileWindow.Destroy;
begin
  inherited;
end;

procedure TProfileWindow.SetupComponents;
var
  Table: PGtkWidget;
  ButtonsHBox: PGtkWidget;
  Label1: PGtkWidget;
  InfoLabel: PGtkWidget;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(15);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 20);

  // Título
  Label1 := TUIUtils.CreateLabel('Actualizar Perfil de Usuario', True);
  gtk_label_set_markup(GTK_LABEL(Label1), '<span size="large" weight="bold">Actualizar Perfil de Usuario</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 10);

  // Información
  InfoLabel := TUIUtils.CreateLabel('Puedes actualizar tu nombre de usuario y número de teléfono.');
  gtk_label_set_markup(GTK_LABEL(InfoLabel), '<span style="italic">Puedes actualizar tu nombre de usuario y número de teléfono.</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), InfoLabel, False, False, 5);

  // Tabla para organizar campos
  Table := TUIUtils.CreateTable(4, 2);
  gtk_box_pack_start(GTK_BOX(FMainVBox), Table, True, True, 15);

  // Campo Nombre Completo
  Label1 := TUIUtils.CreateLabel('Nombre Completo:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 5, 8);

  FNameEntry := TUIUtils.CreateEntry;
  gtk_table_attach(GTK_TABLE(Table), FNameEntry, 1, 2, 0, 1,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 8);

  // Campo Nombre de Usuario
  Label1 := TUIUtils.CreateLabel('Nombre de Usuario:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 1, 2, GTK_FILL, GTK_FILL, 5, 8);

  FUserEntry := TUIUtils.CreateEntry;
  gtk_table_attach(GTK_TABLE(Table), FUserEntry, 1, 2, 1, 2,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 8);

  // Campo Email (solo lectura)
  Label1 := TUIUtils.CreateLabel('Email:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 2, 3, GTK_FILL, GTK_FILL, 5, 8);

  FEmailLabel := TUIUtils.CreateLabel('');
  gtk_label_set_markup(GTK_LABEL(FEmailLabel), '<span foreground="gray">No modificable</span>');
  gtk_misc_set_alignment(GTK_MISC(FEmailLabel), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(Table), FEmailLabel, 1, 2, 2, 3,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 8);

  // Campo Teléfono
  Label1 := TUIUtils.CreateLabel('Teléfono:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 3, 4, GTK_FILL, GTK_FILL, 5, 8);

  FPhoneEntry := TUIUtils.CreateEntry;
  gtk_table_attach(GTK_TABLE(Table), FPhoneEntry, 1, 2, 3, 4,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 8);

  // Botones
  ButtonsHBox := TUIUtils.CreateHBox(15);
  gtk_box_pack_start(GTK_BOX(FMainVBox), ButtonsHBox, False, False, 15);

  FUpdateButton := TUIUtils.CreateButton('💾 Actualizar', @OnUpdateClicked, Self);
  FCancelButton := TUIUtils.CreateButton('❌ Cancelar', @OnCancelClicked, Self);

  gtk_widget_set_size_request(FUpdateButton, 120, 35);
  gtk_widget_set_size_request(FCancelButton, 120, 35);

  gtk_box_pack_start(GTK_BOX(ButtonsHBox), FUpdateButton, True, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsHBox), FCancelButton, True, False, 5);

  // Label de estado
  FStatusLabel := TUIUtils.CreateLabel('');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusLabel, False, False, 10);
end;

procedure TProfileWindow.ConnectSignals;
begin
  inherited;
end;

procedure TProfileWindow.LoadCurrentUser;
begin
  if not IsUserLoggedIn then
  begin
    ClearForm;
    Exit;
  end;

  gtk_entry_set_text(GTK_ENTRY(FNameEntry), PChar(CurrentUser^.Nombre));
  gtk_entry_set_text(GTK_ENTRY(FUserEntry), PChar(CurrentUser^.Usuario));
  gtk_label_set_text(GTK_LABEL(FEmailLabel), PChar(CurrentUser^.Email));
  gtk_entry_set_text(GTK_ENTRY(FPhoneEntry), PChar(CurrentUser^.Telefono));

  gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Información cargada desde el perfil actual');
end;

procedure TProfileWindow.ClearForm;
begin
  gtk_entry_set_text(GTK_ENTRY(FNameEntry), '');
  gtk_entry_set_text(GTK_ENTRY(FUserEntry), '');
  gtk_label_set_text(GTK_LABEL(FEmailLabel), '');
  gtk_entry_set_text(GTK_ENTRY(FPhoneEntry), '');
  gtk_label_set_text(GTK_LABEL(FStatusLabel), '');
end;

function TProfileWindow.ValidateForm: Boolean;
var
  Name, User, Phone: String;
begin
  Result := False;

  Name := gtk_entry_get_text(GTK_ENTRY(FNameEntry));
  User := gtk_entry_get_text(GTK_ENTRY(FUserEntry));
  Phone := gtk_entry_get_text(GTK_ENTRY(FPhoneEntry));

  if Length(Trim(Name)) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese su nombre completo');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Nombre requerido');
    Exit;
  end;

  if Length(Trim(User)) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese su nombre de usuario');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Nombre de usuario requerido');
    Exit;
  end;

  if Length(Trim(Phone)) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese su número de teléfono');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Teléfono requerido');
    Exit;
  end;

  // Validar longitud del teléfono
  if Length(Phone) < 8 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'El número de teléfono debe tener al menos 8 dígitos');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Teléfono muy corto');
    Exit;
  end;

  // Validar que el teléfono contenga solo números
  // Línea corregida
  for i := 1 to Length(Phone) do
  begin
    if not (Phone[i] in ['0'..'9']) then
    begin
      TUIUtils.ShowErrorMessage(FWindow, 'El número de teléfono debe contener solo dígitos');
      gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Teléfono con caracteres inválidos');
      Exit;
    end;
  end;

  Result := True;
end;

procedure TProfileWindow.UpdateProfile;
var
  Name, User, Phone: String;
  OldName, OldUser, OldPhone: String;
begin
  if not IsUserLoggedIn then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Error: No hay usuario logueado');
    Exit;
  end;

  if not ValidateForm then Exit;

  Name := Trim(gtk_entry_get_text(GTK_ENTRY(FNameEntry)));
  User := Trim(gtk_entry_get_text(GTK_ENTRY(FUserEntry)));
  Phone := Trim(gtk_entry_get_text(GTK_ENTRY(FPhoneEntry)));

  // Guardar valores anteriores para comparación
  OldName := CurrentUser^.Nombre;
  OldUser := CurrentUser^.Usuario;
  OldPhone := CurrentUser^.Telefono;

  // Verificar si hay cambios
  if (Name = OldName) and (User = OldUser) and (Phone = OldPhone) then
  begin
    TUIUtils.ShowInfoMessage(FWindow, 'No se detectaron cambios en el perfil');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Sin cambios detectados');
    Exit;
  end;

  // Confirmar actualización
  if not TUIUtils.ShowConfirmDialog(FWindow, 'Confirmar Actualización',
                                   '¿Está seguro que desea actualizar su perfil con esta información?') then
  begin
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Actualización cancelada');
    Exit;
  end;

  // Actualizar perfil
  if UpdateUserProfile(CurrentUser^.Email, Name, User, Phone) then
  begin
    TUIUtils.ShowInfoMessage(FWindow, 'Perfil actualizado exitosamente');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Perfil actualizado correctamente');

    // Mostrar resumen de cambios
    var ChangesText: String;
    ChangesText := 'Cambios realizados:' + LineEnding;

    if Name <> OldName then
      ChangesText := ChangesText + '• Nombre: ' + OldName + ' → ' + Name + LineEnding;
    if User <> OldUser then
      ChangesText := ChangesText + '• Usuario: ' + OldUser + ' → ' + User + LineEnding;
    if Phone <> OldPhone then
      ChangesText := ChangesText + '• Teléfono: ' + OldPhone + ' → ' + Phone + LineEnding;

    TUIUtils.ShowInfoMessage(FWindow, ChangesText);
  end
  else
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Error al actualizar el perfil');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error en la actualización');

    // Recargar información original
    LoadCurrentUser;
  end;
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnUpdateClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ProfileWindow: TProfileWindow;
begin
  ProfileWindow := TProfileWindow(data);
  ProfileWindow.UpdateProfile;
end;

procedure OnCancelClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ProfileWindow: TProfileWindow;
begin
  ProfileWindow := TProfileWindow(data);

  if TUIUtils.ShowConfirmDialog(ProfileWindow.Window, 'Cancelar Cambios',
                               '¿Está seguro que desea cancelar? Se perderán los cambios no guardados.') then
  begin
    ProfileWindow.LoadCurrentUser; // Recargar datos originales
    ProfileWindow.Hide;
  end;
end;

end.

