unit RootWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, UIBase, SysUtils, Classes, DataStructures, SystemCore,
  UserManager, EmailManager, ReportGenerator;

type
  TRootWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FNotebook: PGtkWidget;
    FStatusLabel: PGtkWidget;
    FLoadStatusLabel: PGtkWidget;
    FLoadFileButton: PGtkWidget;
    FUsersReportButton: PGtkWidget;
    FRelationsReportButton: PGtkWidget;
    FReportsStatusLabel: PGtkWidget;

  protected
    procedure SetupComponents; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  end;

// Callbacks
procedure OnLoadFileClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnUsersReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRelationsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRootWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

var
  RootWin: TRootWindow;

implementation

constructor TRootWindow.Create;
begin
  inherited Create('EDDMail - Administrador Root', 600, 400);
end;

destructor TRootWindow.Destroy;
begin
  inherited;
end;

procedure TRootWindow.SetupComponents;
var
  VBox, HBox: PGtkWidget;
  Label1: PGtkWidget;
begin
  // Contenedor principal
  VBox := gtk_vbox_new(False, 20);
  gtk_container_add(GTK_CONTAINER(FWindow), VBox);
  gtk_container_set_border_width(GTK_CONTAINER(VBox), 20);

  // Título
  Label1 := gtk_label_new('Panel de Administración Root');
  gtk_label_set_markup(GTK_LABEL(Label1), '<span size="large" weight="bold">Panel de Administración Root</span>');
  gtk_box_pack_start(GTK_BOX(VBox), Label1, False, False, 20);

  // Sección carga masiva
  Label1 := gtk_label_new('Carga Masiva de Usuarios:');
  gtk_box_pack_start(GTK_BOX(VBox), Label1, False, False, 10);

  FLoadFileButton := gtk_button_new_with_label('Seleccionar Archivo JSON');
  g_signal_connect(G_OBJECT(FLoadFileButton), 'clicked', G_CALLBACK(@OnLoadFileClicked), Self);
  gtk_box_pack_start(GTK_BOX(VBox), FLoadFileButton, False, False, 5);

  FLoadStatusLabel := gtk_label_new('');
  gtk_box_pack_start(GTK_BOX(VBox), FLoadStatusLabel, False, False, 5);

  // Sección reportes
  Label1 := gtk_label_new('Generar Reportes:');
  gtk_box_pack_start(GTK_BOX(VBox), Label1, False, False, 10);

  HBox := gtk_hbox_new(True, 10);
  gtk_box_pack_start(GTK_BOX(VBox), HBox, False, False, 5);

  FUsersReportButton := gtk_button_new_with_label('Reporte Usuarios');
  g_signal_connect(G_OBJECT(FUsersReportButton), 'clicked', G_CALLBACK(@OnUsersReportClicked), Self);
  gtk_box_pack_start(GTK_BOX(HBox), FUsersReportButton, True, True, 5);

  FRelationsReportButton := gtk_button_new_with_label('Reporte Relaciones');
  g_signal_connect(G_OBJECT(FRelationsReportButton), 'clicked', G_CALLBACK(@OnRelationsReportClicked), Self);
  gtk_box_pack_start(GTK_BOX(HBox), FRelationsReportButton, True, True, 5);

  FReportsStatusLabel := gtk_label_new('');
  gtk_box_pack_start(GTK_BOX(VBox), FReportsStatusLabel, False, False, 5);

  // Conectar señal de destrucción
  g_signal_connect(G_OBJECT(FWindow), 'destroy', G_CALLBACK(@OnRootWindowDestroy), nil);
end;

procedure TRootWindow.Show;
begin
  gtk_widget_show_all(FWindow);
end;

procedure TRootWindow.Hide;
begin
  gtk_widget_hide(FWindow);
end;

// Callbacks
procedure OnLoadFileClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWindow: TRootWindow;
  Dialog: PGtkWidget;
  FileName: String;
  Response: gint;
begin
  RootWindow := TRootWindow(data);

  Dialog := gtk_file_chooser_dialog_new(
    'Seleccionar archivo JSON',
    GTK_WINDOW(RootWindow.FWindow),
    GTK_FILE_CHOOSER_ACTION_OPEN,
    GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
    GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
    nil
  );

  Response := gtk_dialog_run(GTK_DIALOG(Dialog));

  if Response = GTK_RESPONSE_ACCEPT then
  begin
    FileName := gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(Dialog));
    gtk_label_set_text(GTK_LABEL(RootWindow.FLoadStatusLabel), PChar('Archivo seleccionado: ' + ExtractFileName(FileName)));
  end;

  gtk_widget_destroy(Dialog);
end;

procedure OnUsersReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWindow: TRootWindow;
begin
  RootWindow := TRootWindow(data);
  gtk_label_set_text(GTK_LABEL(RootWindow.FReportsStatusLabel), 'Generando reporte de usuarios...');
end;

procedure OnRelationsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWindow: TRootWindow;
begin
  RootWindow := TRootWindow(data);
  gtk_label_set_text(GTK_LABEL(RootWindow.FReportsStatusLabel), 'Generando reporte de relaciones...');
end;

procedure OnRootWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

end.
