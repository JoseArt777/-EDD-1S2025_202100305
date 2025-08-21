unit RootWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DataStructures, SystemCore, UIBase,
  UserManager, EmailManager, ContactManager, CommunityManager, ReportGenerator;

type
  TRootWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FNotebook: PGtkWidget;
    FStatusLabel: PGtkWidget;
    FLoadStatusLabel: PGtkWidget;

    // Página de carga masiva
    FFileEntry: PGtkWidget;
    FSelectFileButton: PGtkWidget;
    FLoadButton: PGtkWidget;

    // Página de comunidades
    FCommunityNameEntry: PGtkWidget;
    FCreateCommunityButton: PGtkWidget;
    FCommunityCombo: PGtkWidget;
    FUserEmailEntry: PGtkWidget;
    FAddUserButton: PGtkWidget;
    FCommunityStatusLabel: PGtkWidget;
    FLogoutButton: PGtkWidget;

    // Página de reportes
    FUsersReportButton: PGtkWidget;
    FRelationsReportButton: PGtkWidget;
    FReportsStatusLabel: PGtkWidget;

    procedure SetupNotebook;
    procedure SetupLoadMassivePage;
    procedure SetupCommunitiesPage;
    procedure SetupReportsPage;
    procedure RefreshCommunityCombo;
    procedure ClearComboBox(ComboBox: PGtkWidget);

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Show; reintroduce;
    procedure Hide; reintroduce;
  end;

// Callbacks
procedure OnSelectFileClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnLoadFileClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnCreateCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnAddUserToCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnUsersReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRelationsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRootWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRootLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;

var
  RootWin: TRootWindow;

implementation

// Declaración forward para evitar dependencia circular
procedure RestartApplication; forward;

constructor TRootWindow.Create;
begin
  inherited Create('EDDMail - Administrador Root', 600, 500);
end;

destructor TRootWindow.Destroy;
begin
  inherited;
end;

procedure TRootWindow.SetupComponents;
begin
  WriteLn('DEBUG: SetupComponents iniciado');

  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);

  SetupNotebook;

  // Botón de logout
  FLogoutButton := TUIUtils.CreateButton('Cerrar Sesión', @OnRootLogoutClicked, Self);
  gtk_box_pack_end(GTK_BOX(FMainVBox), FLogoutButton, False, False, 5);

  // Status bar
  FStatusLabel := TUIUtils.CreateLabel('Listo', False);
  gtk_box_pack_end(GTK_BOX(FMainVBox), FStatusLabel, False, False, 0);

  WriteLn('DEBUG: SetupComponents completado');
end;

procedure TRootWindow.ConnectSignals;
begin
  inherited;
  g_signal_connect(G_OBJECT(FWindow), 'destroy', G_CALLBACK(@OnRootWindowDestroy), Self);
end;

procedure TRootWindow.SetupNotebook;
begin
  FNotebook := gtk_notebook_new;
  gtk_box_pack_start(GTK_BOX(FMainVBox), FNotebook, True, True, 0);

  SetupLoadMassivePage;
  SetupCommunitiesPage;
  SetupReportsPage;
end;

procedure TRootWindow.SetupLoadMassivePage;
var
  PageVBox, HBox: PGtkWidget;
  Label1: PGtkWidget;
begin
  PageVBox := TUIUtils.CreateVBox(10);
  gtk_container_set_border_width(GTK_CONTAINER(PageVBox), 10);

  Label1 := TUIUtils.CreateLabel('Cargar Usuarios Masivamente', True);
  gtk_box_pack_start(GTK_BOX(PageVBox), Label1, False, False, 0);

  HBox := TUIUtils.CreateHBox(5);
  FFileEntry := TUIUtils.CreateEntry('Seleccionar archivo JSON...');
  FSelectFileButton := TUIUtils.CreateButton('Examinar', @OnSelectFileClicked, Self);

  gtk_box_pack_start(GTK_BOX(HBox), FFileEntry, True, True, 0);
  gtk_box_pack_start(GTK_BOX(HBox), FSelectFileButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(PageVBox), HBox, False, False, 0);

  FLoadButton := TUIUtils.CreateButton('Cargar Usuarios', @OnLoadFileClicked, Self);
  gtk_box_pack_start(GTK_BOX(PageVBox), FLoadButton, False, False, 0);

  FLoadStatusLabel := TUIUtils.CreateLabel('', False);
  gtk_box_pack_start(GTK_BOX(PageVBox), FLoadStatusLabel, False, False, 0);

  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), PageVBox,
                           gtk_label_new('Carga Masiva'));
end;

procedure TRootWindow.SetupCommunitiesPage;
var
  PageVBox, HBox: PGtkWidget;
  Label1, Label2: PGtkWidget;
begin
  PageVBox := TUIUtils.CreateVBox(10);
  gtk_container_set_border_width(GTK_CONTAINER(PageVBox), 10);

  Label1 := TUIUtils.CreateLabel('Gestión de Comunidades', True);
  gtk_box_pack_start(GTK_BOX(PageVBox), Label1, False, False, 0);

  // Crear comunidad
  HBox := TUIUtils.CreateHBox(5);
  FCommunityNameEntry := TUIUtils.CreateEntry('Nombre de la comunidad');
  FCreateCommunityButton := TUIUtils.CreateButton('Crear', @OnCreateCommunityClicked, Self);

  gtk_box_pack_start(GTK_BOX(HBox), FCommunityNameEntry, True, True, 0);
  gtk_box_pack_start(GTK_BOX(HBox), FCreateCommunityButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(PageVBox), HBox, False, False, 0);

  // Agregar usuario a comunidad
  Label2 := TUIUtils.CreateLabel('Agregar Usuario a Comunidad:', False);
  gtk_box_pack_start(GTK_BOX(PageVBox), Label2, False, False, 0);

  FCommunityCombo := TUIUtils.CreateComboBox;
  gtk_box_pack_start(GTK_BOX(PageVBox), FCommunityCombo, False, False, 0);

  HBox := TUIUtils.CreateHBox(5);
  FUserEmailEntry := TUIUtils.CreateEntry('Email del usuario');
  FAddUserButton := TUIUtils.CreateButton('Agregar', @OnAddUserToCommunityClicked, Self);

  gtk_box_pack_start(GTK_BOX(HBox), FUserEmailEntry, True, True, 0);
  gtk_box_pack_start(GTK_BOX(HBox), FAddUserButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(PageVBox), HBox, False, False, 0);

  FCommunityStatusLabel := TUIUtils.CreateLabel('', False);
  gtk_box_pack_start(GTK_BOX(PageVBox), FCommunityStatusLabel, False, False, 0);

  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), PageVBox,
                           gtk_label_new('Comunidades'));
end;

procedure TRootWindow.SetupReportsPage;
var
  PageVBox: PGtkWidget;
  Label1: PGtkWidget;
begin
  PageVBox := TUIUtils.CreateVBox(10);
  gtk_container_set_border_width(GTK_CONTAINER(PageVBox), 10);

  Label1 := TUIUtils.CreateLabel('Generar Reportes', True);
  gtk_box_pack_start(GTK_BOX(PageVBox), Label1, False, False, 0);

  FUsersReportButton := TUIUtils.CreateButton('Reporte de Usuarios', @OnUsersReportClicked, Self);
  gtk_box_pack_start(GTK_BOX(PageVBox), FUsersReportButton, False, False, 0);

  FRelationsReportButton := TUIUtils.CreateButton('Reporte de Relaciones', @OnRelationsReportClicked, Self);
  gtk_box_pack_start(GTK_BOX(PageVBox), FRelationsReportButton, False, False, 0);

  FReportsStatusLabel := TUIUtils.CreateLabel('', False);
  gtk_box_pack_start(GTK_BOX(PageVBox), FReportsStatusLabel, False, False, 0);

  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), PageVBox,
                           gtk_label_new('Reportes'));
end;

procedure TRootWindow.ClearComboBox(ComboBox: PGtkWidget);
var
  Model: PGtkTreeModel;
  Count: Integer;
  i: Integer;
begin
  Model := gtk_combo_box_get_model(GTK_COMBO_BOX(ComboBox));
  if Model <> nil then
  begin
    Count := gtk_tree_model_iter_n_children(Model, nil);
    for i := Count - 1 downto 0 do
      gtk_combo_box_remove_text(GTK_COMBO_BOX(ComboBox), i);
  end;
end;

procedure TRootWindow.RefreshCommunityCombo;
begin
  // Temporalmente vacía para evitar errores
  WriteLn('DEBUG: RefreshCommunityCombo omitido por seguridad');
end;

procedure TRootWindow.Show;
begin
  gtk_widget_show_all(FWindow);
end;

procedure TRootWindow.Hide;
begin
  gtk_widget_hide(FWindow);
end;

// ============================================================================
// Callbacks Implementation
// ============================================================================

procedure OnSelectFileClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  CurrentPath: String;
begin
  try
    RootWin := TRootWindow(data);

    WriteLn('DEBUG: Iniciando selector de archivo simplificado');

    // Obtener directorio actual
    CurrentPath := GetCurrentDir;
    WriteLn('DEBUG: Directorio actual: ', CurrentPath);

    // Simplemente poner una ruta de ejemplo en el campo
    gtk_entry_set_text(GTK_ENTRY(RootWin.FFileEntry),
                       PChar(CurrentPath + '/usuarios_prueba.json'));

    // Actualizar label de status usando métodos directos GTK
    gtk_label_set_text(GTK_LABEL(RootWin.FLoadStatusLabel),
                       'Ruta de ejemplo cargada. Modifique si es necesario.');

    WriteLn('DEBUG: Selector simplificado completado');

  except
    on E: Exception do
    begin
      WriteLn('Error crítico en OnSelectFileClicked: ', E.Message);
      WriteLn('Tipo de error: ', E.ClassName);
    end;
  end;
end;

procedure OnLoadFileClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  Filename: String;
  Result: Boolean;
begin
  try
    RootWin := TRootWindow(data);
    Filename := gtk_entry_get_text(GTK_ENTRY(RootWin.FFileEntry));

    WriteLn('DEBUG: OnLoadFileClicked iniciado');

    if Filename = '' then
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FLoadStatusLabel), 'Error: Especifique archivo');
      WriteLn('ERROR: Archivo no especificado');
      Exit;
    end;

    if not FileExists(Filename) then
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FLoadStatusLabel), 'Error: Archivo no encontrado');
      WriteLn('ERROR: Archivo no encontrado');
      Exit;
    end;

    Result := LoadUsersFromJSON(Filename);

    if Result then
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FLoadStatusLabel), 'Usuarios cargados exitosamente');
      WriteLn('ÉXITO: Usuarios cargados');
    end
    else
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FLoadStatusLabel), 'Error al cargar usuarios');
      WriteLn('ERROR: Fallo en carga');
    end;

    WriteLn('DEBUG: OnLoadFileClicked completado');

  except
    on E: Exception do
      WriteLn('Error en OnLoadFileClicked: ', E.Message);
  end;
end;

procedure OnCreateCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  CommunityName: String;
  Result: Boolean;
begin
  try
    RootWin := TRootWindow(data);
    CommunityName := gtk_entry_get_text(GTK_ENTRY(RootWin.FCommunityNameEntry));

    if CommunityName = '' then
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'Error: Ingrese nombre');
      Exit;
    end;

    Result := CreateCommunity(CommunityName);

    if Result then
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'Comunidad creada exitosamente');
      gtk_entry_set_text(GTK_ENTRY(RootWin.FCommunityNameEntry), '');
    end
    else
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'Error al crear comunidad');
    end;
  except
    on E: Exception do
      WriteLn('Error en OnCreateCommunityClicked: ', E.Message);
  end;
end;

procedure OnAddUserToCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  UserEmail, ComboText: String;
  CommunityId: Integer;
  SpacePos: Integer;
  ComboPC: PChar;
begin
  try
    RootWin := TRootWindow(data);
    UserEmail := gtk_entry_get_text(GTK_ENTRY(RootWin.FUserEmailEntry));

    ComboPC := gtk_combo_box_get_active_text(GTK_COMBO_BOX(RootWin.FCommunityCombo));
    if ComboPC <> nil then
    begin
      ComboText := String(ComboPC);
      g_free(ComboPC);
    end
    else
      ComboText := '';

    if (UserEmail = '') or (ComboText = '') then
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'Complete todos los campos');
      Exit;
    end;

    SpacePos := Pos(' ', ComboText);
    if SpacePos > 0 then
    begin
      try
        CommunityId := StrToInt(Copy(ComboText, 1, SpacePos - 1));

        if AddUserToCommunity(CommunityId, UserEmail) then
        begin
          gtk_entry_set_text(GTK_ENTRY(RootWin.FUserEmailEntry), '');
          gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'Usuario agregado exitosamente');
        end
        else
        begin
          gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'Error al agregar usuario');
        end;
      except
        gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'ID de comunidad inválido');
      end;
    end
    else
    begin
      gtk_label_set_text(GTK_LABEL(RootWin.FCommunityStatusLabel), 'Formato de comunidad inválido');
    end;
  except
    on E: Exception do
      WriteLn('Error en OnAddUserToCommunityClicked: ', E.Message);
  end;
end;

procedure OnUsersReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
begin
  try
    RootWin := TRootWindow(data);

    if SaveUsersReport then
      gtk_label_set_text(GTK_LABEL(RootWin.FReportsStatusLabel), 'Reporte de usuarios generado')
    else
      gtk_label_set_text(GTK_LABEL(RootWin.FReportsStatusLabel), 'Error al generar reporte');
  except
    on E: Exception do
      WriteLn('Error en OnUsersReportClicked: ', E.Message);
  end;
end;

procedure OnRelationsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
begin
  try
    RootWin := TRootWindow(data);

    if SaveRelationsReport then
      gtk_label_set_text(GTK_LABEL(RootWin.FReportsStatusLabel), 'Reporte de relaciones generado')
    else
      gtk_label_set_text(GTK_LABEL(RootWin.FReportsStatusLabel), 'Error al generar reporte');
  except
    on E: Exception do
      WriteLn('Error en OnRelationsReportClicked: ', E.Message);
  end;
end;

procedure RestartApplication;
begin
  WriteLn('INFORMACIÓN: Cierre la aplicación y vuelva a ejecutarla.');
  WriteLn('Los usuarios han sido cargados y estarán disponibles en la próxima sesión.');
  WriteLn('Puede hacer login como: juan@test.com / 123456');
  gtk_main_quit;
end;

procedure OnRootWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  WriteLn('DEBUG: Ventana de root cerrada');
  LogoutUser;
  RestartApplication;
end;

procedure OnRootLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
begin
  try
    RootWin := TRootWindow(data);
    WriteLn('DEBUG: Logout manual solicitado');
    LogoutUser;
    RootWin.Hide;
    RestartApplication;
  except
    on E: Exception do
      WriteLn('Error en OnRootLogoutClicked: ', E.Message);
  end;
end;

end.
