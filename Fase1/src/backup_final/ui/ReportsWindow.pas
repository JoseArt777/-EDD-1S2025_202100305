unit ReportsWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, GLib2, SysUtils, Classes, DataStructures, SystemCore, UIBase,
  ReportGenerator, FileManager;

type
  TReportsWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FReportsFrame: PGtkWidget;
    FPreviewFrame: PGtkWidget;

    // Botones de reportes
    FInboxReportButton: PGtkWidget;
    FTrashReportButton: PGtkWidget;
    FScheduledReportButton: PGtkWidget;
    FContactsReportButton: PGtkWidget;
    FGenerateAllButton: PGtkWidget;

    // Preview del reporte
    FPreviewTextView: PGtkWidget;
    FScrolledPreview: PGtkWidget;

    // Estado
    FStatusLabel: PGtkWidget;
    FCurrentReportType: String;

    procedure ShowReportPreview(ReportTitle, ReportContent: String);
    procedure GenerateAllReports;

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create(AParent: PGtkWidget = nil);
    destructor Destroy; override;
    procedure GenerateInboxReport;
    procedure GenerateTrashReport;
    procedure GenerateScheduledReport;
    procedure GenerateContactsReport;
  end;

// Callbacks
procedure OnInboxReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnTrashReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnScheduledReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnContactsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnGenerateAllClicked(widget: PGtkWidget; data: gpointer); cdecl;

implementation

constructor TReportsWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Generar Reportes', 800, 600, AParent);
  FCurrentReportType := '';
end;

destructor TReportsWindow.Destroy;
begin
  inherited;
end;

procedure TReportsWindow.SetupComponents;
var
  MainHBox: PGtkWidget;
  ButtonsVBox: PGtkWidget;
  Label1: PGtkWidget;
  Buffer: PGtkTextBuffer;
  WelcomeText: String;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 15);

  // Título
  Label1 := TUIUtils.CreateLabel('Generador de Reportes de Usuario', True);
  gtk_label_set_markup(GTK_LABEL(Label1), '<span size="large" weight="bold">Generador de Reportes de Usuario</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 10);

  // Información
  Label1 := TUIUtils.CreateLabel('Los reportes se guardarán en la carpeta: ' + GetUserReportsPath(CurrentUser^.Email));
  gtk_label_set_markup(GTK_LABEL(Label1), '<span style="italic">Los reportes se guardarán en la carpeta: ' + GetUserReportsPath(CurrentUser^.Email) + '</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 5);

  // Contenedor horizontal para botones y preview
  MainHBox := TUIUtils.CreateHBox(15);
  gtk_box_pack_start(GTK_BOX(FMainVBox), MainHBox, True, True, 10);

  // Frame para botones de reportes
  FReportsFrame := TUIUtils.CreateFrame('Tipos de Reportes');
  gtk_widget_set_size_request(FReportsFrame, 250, -1);
  gtk_box_pack_start(GTK_BOX(MainHBox), FReportsFrame, False, False, 5);

  ButtonsVBox := TUIUtils.CreateVBox(15);
  gtk_container_add(GTK_CONTAINER(FReportsFrame), ButtonsVBox);
  gtk_container_set_border_width(GTK_CONTAINER(ButtonsVBox), 15);

  // Botones de reportes individuales
  FInboxReportButton := TUIUtils.CreateButton('📥 Correos Recibidos', @OnInboxReportClicked, Self);
  FTrashReportButton := TUIUtils.CreateButton('🗑️ Papelera', @OnTrashReportClicked, Self);
  FScheduledReportButton := TUIUtils.CreateButton('📅 Correos Programados', @OnScheduledReportClicked, Self);
  FContactsReportButton := TUIUtils.CreateButton('👥 Contactos', @OnContactsReportClicked, Self);
  FGenerateAllButton := TUIUtils.CreateButton('📊 Generar Todos', @OnGenerateAllClicked, Self);

  gtk_widget_set_size_request(FInboxReportButton, 200, 40);
  gtk_widget_set_size_request(FTrashReportButton, 200, 40);
  gtk_widget_set_size_request(FScheduledReportButton, 200, 40);
  gtk_widget_set_size_request(FContactsReportButton, 200, 40);
  gtk_widget_set_size_request(FGenerateAllButton, 200, 50);

  gtk_box_pack_start(GTK_BOX(ButtonsVBox), FInboxReportButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox), FTrashReportButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox), FScheduledReportButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox), FContactsReportButton, False, False, 5);

  // Separador
  Label1 := gtk_hseparator_new;
  gtk_box_pack_start(GTK_BOX(ButtonsVBox), Label1, False, False, 10);

  gtk_box_pack_start(GTK_BOX(ButtonsVBox), FGenerateAllButton, False, False, 10);

  // Frame para preview del reporte
  FPreviewFrame := TUIUtils.CreateFrame('Vista Previa del Reporte');
  gtk_box_pack_start(GTK_BOX(MainHBox), FPreviewFrame, True, True, 5);

  FScrolledPreview := TUIUtils.CreateScrolledWindow;
  gtk_container_add(GTK_CONTAINER(FPreviewFrame), FScrolledPreview);
  gtk_container_set_border_width(GTK_CONTAINER(FScrolledPreview), 10);

  FPreviewTextView := TUIUtils.CreateTextView;
  gtk_text_view_set_editable(GTK_TEXT_VIEW(FPreviewTextView), False);
  gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(FPreviewTextView), False);

  // Configurar fuente monospace para mejor visualización
  var FontDesc: PPangoFontDescription;
  FontDesc := pango_font_description_from_string('Monospace 10');
  gtk_widget_modify_font(FPreviewTextView, FontDesc);
  pango_font_description_free(FontDesc);

  gtk_container_add(GTK_CONTAINER(FScrolledPreview), FPreviewTextView);

  // Texto de bienvenida
  WelcomeText := 'GENERADOR DE REPORTES' + LineEnding + LineEnding +
                 'Seleccione un tipo de reporte para generar:' + LineEnding + LineEnding +
                 '📥 Correos Recibidos - Lista todos los emails en tu bandeja de entrada' + LineEnding +
                 '🗑️ Papelera - Lista todos los emails eliminados' + LineEnding +
                 '📅 Correos Programados - Lista todos los emails pendientes de envío' + LineEnding +
                 '👥 Contactos - Lista todos tus contactos' + LineEnding + LineEnding +
                 '📊 Generar Todos - Crea todos los reportes de una vez' + LineEnding + LineEnding +
                 'Los reportes se guardarán automáticamente en tu carpeta de reportes.';

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FPreviewTextView));
  gtk_text_buffer_set_text(Buffer, PChar(WelcomeText), -1);

  // Label de estado
  FStatusLabel := TUIUtils.CreateLabel('Listo para generar reportes');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusLabel, False, False, 5);
end;

procedure TReportsWindow.ConnectSignals;
begin
  inherited;
end;

procedure TReportsWindow.ShowReportPreview(ReportTitle, ReportContent: String);
var
  Buffer: PGtkTextBuffer;
  PreviewText: String;
begin
  FCurrentReportType := ReportTitle;

  PreviewText := 'VISTA PREVIA: ' + ReportTitle + LineEnding +
                 StringOfChar('=', Length('VISTA PREVIA: ' + ReportTitle)) + LineEnding + LineEnding +
                 ReportContent + LineEnding + LineEnding +
                 'Nota: Esta es solo una vista previa. El reporte completo se ha guardado en archivo.';

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FPreviewTextView));
  gtk_text_buffer_set_text(Buffer, PChar(PreviewText), -1);

  // Hacer scroll al inicio
  var StartIter: TGtkTextIter;
  gtk_text_buffer_get_start_iter(Buffer, @StartIter);
  gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(FPreviewTextView), @StartIter, 0.0, False, 0.0, 0.0);
end;

procedure TReportsWindow.GenerateInboxReport;
var
  Report: String;
  Success: Boolean;
begin
  if not IsUserLoggedIn then Exit;

  gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Generando reporte de correos recibidos...');

  Report := ReportGenerator.GenerateUserInboxReport(CurrentUser^.Email);
  Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Correos_Recibidos', Report);

  if Success then
  begin
    ShowReportPreview('REPORTE DE CORREOS RECIBIDOS', Report);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Reporte de correos recibidos generado exitosamente');
    TUIUtils.ShowInfoMessage(FWindow, 'Reporte de correos recibidos generado y guardado exitosamente');
  end
  else
  begin
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error al generar reporte de correos recibidos');
    TUIUtils.ShowErrorMessage(FWindow, 'Error al guardar el reporte de correos recibidos');
  end;
end;

procedure TReportsWindow.GenerateTrashReport;
var
  Report: String;
  Success: Boolean;
begin
  if not IsUserLoggedIn then Exit;

  gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Generando reporte de papelera...');

  Report := ReportGenerator.GenerateUserTrashReport(CurrentUser^.Email);
  Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Papelera', Report);

  if Success then
  begin
    ShowReportPreview('REPORTE DE PAPELERA', Report);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Reporte de papelera generado exitosamente');
    TUIUtils.ShowInfoMessage(FWindow, 'Reporte de papelera generado y guardado exitosamente');
  end
  else
  begin
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error al generar reporte de papelera');
    TUIUtils.ShowErrorMessage(FWindow, 'Error al guardar el reporte de papelera');
  end;
end;

procedure TReportsWindow.GenerateScheduledReport;
var
  Report: String;
  Success: Boolean;
begin
  if not IsUserLoggedIn then Exit;

  gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Generando reporte de correos programados...');

  Report := ReportGenerator.GenerateUserScheduledReport(CurrentUser^.Email);
  Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Correos_Programados', Report);

  if Success then
  begin
    ShowReportPreview('REPORTE DE CORREOS PROGRAMADOS', Report);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Reporte de correos programados generado exitosamente');
    TUIUtils.ShowInfoMessage(FWindow, 'Reporte de correos programados generado y guardado exitosamente');
  end
  else
  begin
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error al generar reporte de correos programados');
    TUIUtils.ShowErrorMessage(FWindow, 'Error al guardar el reporte de correos programados');
  end;
end;

procedure TReportsWindow.GenerateContactsReport;
var
  Report: String;
  Success: Boolean;
begin
  if not IsUserLoggedIn then Exit;

  gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Generando reporte de contactos...');

  Report := ReportGenerator.GenerateUserContactsReport(CurrentUser^.Email);
  Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Contactos', Report);

  if Success then
  begin
    ShowReportPreview('REPORTE DE CONTACTOS', Report);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Reporte de contactos generado exitosamente');
    TUIUtils.ShowInfoMessage(FWindow, 'Reporte de contactos generado y guardado exitosamente');
  end
  else
  begin
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error al generar reporte de contactos');
    TUIUtils.ShowErrorMessage(FWindow, 'Error al guardar el reporte de contactos');
  end;
end;

procedure TReportsWindow.GenerateAllReports;
var
  Success: Boolean;
  SuccessCount: Integer;
  ErrorCount: Integer;
  ResultMessage: String;
begin
  if not IsUserLoggedIn then Exit;

  if not TUIUtils.ShowConfirmDialog(FWindow, 'Generar Todos los Reportes',
                                   '¿Está seguro que desea generar todos los reportes? ' +
                                   'Esto creará 4 archivos en su carpeta de reportes.') then
  begin
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Generación de reportes cancelada');
    Exit;
  end;

  gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Generando todos los reportes...');
  SuccessCount := 0;
  ErrorCount := 0;

  // Generar reporte de correos recibidos
  try
    Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Correos_Recibidos',
                             ReportGenerator.GenerateUserInboxReport(CurrentUser^.Email));
    if Success then Inc(SuccessCount) else Inc(ErrorCount);
  except
    Inc(ErrorCount);
  end;

  // Generar reporte de papelera
  try
    Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Papelera',
                             ReportGenerator.GenerateUserTrashReport(CurrentUser^.Email));
    if Success then Inc(SuccessCount) else Inc(ErrorCount);
  except
    Inc(ErrorCount);
  end;

  // Generar reporte de correos programados
  try
    Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Correos_Programados',
                             ReportGenerator.GenerateUserScheduledReport(CurrentUser^.Email));
    if Success then Inc(SuccessCount) else Inc(ErrorCount);
  except
    Inc(ErrorCount);
  end;

  // Generar reporte de contactos
  try
    Success := SaveUserReport(CurrentUser^.Email, 'Reporte_Contactos',
                             ReportGenerator.GenerateUserContactsReport(CurrentUser^.Email));
    if Success then Inc(SuccessCount) else Inc(ErrorCount);
  except
    Inc(ErrorCount);
  end;

  // Mostrar resumen de la generación
  if ErrorCount = 0 then
  begin
    ResultMessage := Format('✅ Todos los reportes generados exitosamente!' + LineEnding +
                           'Se crearon %d archivos en la carpeta: %s',
                           [SuccessCount, GetUserReportsPath(CurrentUser^.Email)]);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Todos los reportes generados exitosamente');
  end
  else if SuccessCount > 0 then
  begin
    ResultMessage := Format('⚠️ Generación parcial completada' + LineEnding +
                           'Exitosos: %d | Errores: %d' + LineEnding +
                           'Revise la carpeta: %s',
                           [SuccessCount, ErrorCount, GetUserReportsPath(CurrentUser^.Email)]);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), Format('Generación parcial: %d exitosos, %d errores', [SuccessCount, ErrorCount]));
  end
  else
  begin
    ResultMessage := Format('❌ Error en la generación de reportes' + LineEnding +
                           'No se pudo generar ningún reporte.' + LineEnding +
                           'Verifique los permisos de la carpeta: %s',
                           [GetUserReportsPath(CurrentUser^.Email)]);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: No se pudo generar ningún reporte');
  end;

  TUIUtils.ShowInfoMessage(FWindow, ResultMessage);

  // Mostrar vista previa del resumen
  var SummaryReport: String;
  SummaryReport := 'RESUMEN DE GENERACIÓN DE REPORTES' + LineEnding +
                   '=================================' + LineEnding +
                   'Usuario: ' + CurrentUser^.Email + LineEnding +
                   'Fecha: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now) + LineEnding + LineEnding +
                   Format('Reportes generados exitosamente: %d', [SuccessCount]) + LineEnding +
                   Format('Errores encontrados: %d', [ErrorCount]) + LineEnding + LineEnding +
                   'Ubicación de archivos: ' + GetUserReportsPath(CurrentUser^.Email) + LineEnding + LineEnding +
                   'Archivos generados:' + LineEnding;

  if SuccessCount > 0 then
  begin
    SummaryReport := SummaryReport +
                    '• Reporte_Correos_Recibidos.txt' + LineEnding +
                    '• Reporte_Papelera.txt' + LineEnding +
                    '• Reporte_Correos_Programados.txt' + LineEnding +
                    '• Reporte_Contactos.txt' + LineEnding;
  end;

  ShowReportPreview('RESUMEN DE GENERACIÓN', SummaryReport);
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnInboxReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ReportsWindow: TReportsWindow;
begin
  ReportsWindow := TReportsWindow(data);
  ReportsWindow.GenerateInboxReport;
end;

procedure OnTrashReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ReportsWindow: TReportsWindow;
begin
  ReportsWindow := TReportsWindow(data);
  ReportsWindow.GenerateTrashReport;
end;

procedure OnScheduledReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ReportsWindow: TReportsWindow;
begin
  ReportsWindow := TReportsWindow(data);
  ReportsWindow.GenerateScheduledReport;
end;

procedure OnContactsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ReportsWindow: TReportsWindow;
begin
  ReportsWindow := TReportsWindow(data);
  ReportsWindow.GenerateContactsReport;
end;

procedure OnGenerateAllClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ReportsWindow: TReportsWindow;
begin
  ReportsWindow := TReportsWindow(data);
  ReportsWindow.GenerateAllReports;
end;

end.

