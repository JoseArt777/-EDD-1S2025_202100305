unit ProfileWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, UIBase, DataStructures, SystemCore;

type
  TProfileWindow = class(TBaseWindow)
  private
    FMainVBox   : PGtkWidget;
    FFormTable  : PGtkWidget;
    FButtonBox  : PGtkWidget;

    // Controles tipados (no PGtkWidget genérico)
    FTitleLabel : PGtkLabel;
    FEmailLabel : PGtkLabel;

    FNameEntry  : PGtkEntry;   // Nombre visible
    FUserEntry  : PGtkEntry;   // Usuario (username)
    FPhoneEntry : PGtkEntry;   // Teléfono

    FSaveButton   : PGtkWidget;
    FCancelButton : PGtkWidget;

    procedure BuildForm;
    procedure LoadFromUser;
    procedure SaveToUser;
  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;
  public
    constructor Create(AParent: PGtkWidget);
    procedure LoadCurrentUser;
  end;

// Callbacks de botones
procedure OnProfileSaveClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnProfileCancelClicked(widget: PGtkWidget; data: gpointer); cdecl;

implementation

// ============================================================================
// TProfileWindow
// ============================================================================

constructor TProfileWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Actualizar Perfil', 450, 300, AParent);
end;

procedure TProfileWindow.SetupComponents;
begin
  // VBox principal
  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 12);

  // Título
  FTitleLabel := PGtkLabel(TUIUtils.CreateLabel('<b>Editar Perfil</b>', True));
  gtk_label_set_use_markup(FTitleLabel, True);
  gtk_box_pack_start(GTK_BOX(FMainVBox), PGtkWidget(FTitleLabel), False, False, 0);

  // Formulario
  BuildForm;

  // Botonera
  FButtonBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(FMainVBox), FButtonBox, False, False, 0);

  FSaveButton   := TUIUtils.CreateButton('💾 Guardar', @OnProfileSaveClicked, Self);
  FCancelButton := TUIUtils.CreateButton('✖ Cancelar', @OnProfileCancelClicked, Self);

  gtk_widget_set_size_request(FSaveButton,   100, 34);
  gtk_widget_set_size_request(FCancelButton, 100, 34);

  gtk_box_pack_end(GTK_BOX(FButtonBox), FCancelButton, False, False, 0);
  gtk_box_pack_end(GTK_BOX(FButtonBox), FSaveButton,   False, False, 0);
end;

procedure TProfileWindow.ConnectSignals;
begin
  inherited;
  // (Las señales de botones ya se conectaron al crearlos con CreateButton)
end;

procedure TProfileWindow.BuildForm;
var
  L1, L2, L3, L4: PGtkWidget;
begin
  FFormTable := TUIUtils.CreateTable(4, 2);
  gtk_box_pack_start(GTK_BOX(FMainVBox), FFormTable, True, True, 0);

  // Labels
  L1 := TUIUtils.CreateLabel('Nombre:');
  L2 := TUIUtils.CreateLabel('Usuario:');
  L3 := TUIUtils.CreateLabel('Teléfono:');
  L4 := TUIUtils.CreateLabel('Email:');

  // Entradas (creadas tipadas)
  FNameEntry  := PGtkEntry(gtk_entry_new);
  FUserEntry  := PGtkEntry(gtk_entry_new);
  FPhoneEntry := PGtkEntry(gtk_entry_new);

  // Email solo lectura (label)
  FEmailLabel := PGtkLabel(TUIUtils.CreateLabel(''));
  gtk_label_set_selectable(FEmailLabel, True);

  // Attach en tabla
  // Fila 0: Nombre
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), L1, 0, 1, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), PGtkWidget(FNameEntry), 1, 2, 0, 1);

  // Fila 1: Usuario
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), L2, 0, 1, 1, 2);
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), PGtkWidget(FUserEntry), 1, 2, 1, 2);

  // Fila 2: Teléfono
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), L3, 0, 1, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), PGtkWidget(FPhoneEntry), 1, 2, 2, 3);

  // Fila 3: Email (label)
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), L4, 0, 1, 3, 4);
  gtk_table_attach_defaults(GTK_TABLE(FFormTable), PGtkWidget(FEmailLabel), 1, 2, 3, 4);
end;

procedure TProfileWindow.LoadFromUser;
begin
  if (CurrentUser = nil) then Exit;

  // Cargar datos con validaciones GTK_IS_*
  if (FNameEntry <> nil) and GTK_IS_ENTRY(FNameEntry) then
    gtk_entry_set_text(FNameEntry, PChar(UTF8Encode(CurrentUser^.Nombre)));

  if (FUserEntry <> nil) and GTK_IS_ENTRY(FUserEntry) then
    gtk_entry_set_text(FUserEntry, PChar(UTF8Encode(CurrentUser^.Usuario)));

  if (FPhoneEntry <> nil) and GTK_IS_ENTRY(FPhoneEntry) then
    gtk_entry_set_text(FPhoneEntry, PChar(UTF8Encode(CurrentUser^.Telefono)));

  if (FEmailLabel <> nil) and GTK_IS_LABEL(FEmailLabel) then
    gtk_label_set_text(FEmailLabel, PChar(UTF8Encode(CurrentUser^.Email)));
end;

procedure TProfileWindow.SaveToUser;
var
  SNombre, SUsuario, STelefono: AnsiString;
begin
  if (CurrentUser = nil) then Exit;

  // Leer valores con validaciones
  if (FNameEntry <> nil) and GTK_IS_ENTRY(FNameEntry) then
    SNombre := gtk_entry_get_text(FNameEntry)
  else
    SNombre := CurrentUser^.Nombre;

  if (FUserEntry <> nil) and GTK_IS_ENTRY(FUserEntry) then
    SUsuario := gtk_entry_get_text(FUserEntry)
  else
    SUsuario := CurrentUser^.Usuario;

  if (FPhoneEntry <> nil) and GTK_IS_ENTRY(FPhoneEntry) then
    STelefono := gtk_entry_get_text(FPhoneEntry)
  else
    STelefono := CurrentUser^.Telefono;

  // Actualizar solo los campos permitidos (usuario, teléfono; y opcionalmente nombre visible)
  CurrentUser^.Nombre   := UTF8Decode(SNombre);
  CurrentUser^.Usuario  := UTF8Decode(SUsuario);
  CurrentUser^.Telefono := UTF8Decode(STelefono);

  // Persistir en tu sistema si aplica (ej: SaveUsers/Flush, etc.)
  try
    // Si tienes alguna rutina de persistencia, llámala aquí
    // SaveUsers();  // <- ejemplo opcional según tu arquitectura
  except
    on E: Exception do
      TUIUtils.ShowErrorMessage(Window, 'No se pudieron guardar los cambios: ' + E.Message);
  end;

  TUIUtils.ShowInfoMessage(Window, 'Perfil actualizado exitosamente');
end;

procedure TProfileWindow.LoadCurrentUser;
begin
  LoadFromUser;
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnProfileSaveClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  Win: TProfileWindow;
begin
  Win := TProfileWindow(data);
  if Win <> nil then
  begin
    Win.SaveToUser;
    // Mantener la ventana abierta para que el usuario vea los cambios
    if (Win.Window <> nil) and GTK_IS_WIDGET(Win.Window) then
      gtk_widget_show_all(Win.Window);
  end;
end;

procedure OnProfileCancelClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  Win: TProfileWindow;
begin
  Win := TProfileWindow(data);
  if (Win <> nil) and (Win.Window <> nil) and GTK_IS_WIDGET(Win.Window) then
    gtk_widget_hide(Win.Window); // Solo ocultar (no destruir), consistente con UIBase
end;

end.

