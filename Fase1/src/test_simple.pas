program TestSimple;

{$mode objfpc}{$H+}

uses
  GTK2, GDK2, GLib2, SysUtils,
  DataStructures, SystemCore, UserManager;

var
  Window: PGtkWidget;
  Button: PGtkWidget;
  VBox: PGtkWidget;

procedure OnButtonClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  writeln('¡EDDMail funcionando!');
  gtk_main_quit;
end;

procedure OnDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

begin
  // Inicializar GTK
  gtk_init(@argc, @argv);
  
  // Crear ventana principal
  Window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(Window), 'EDDMail - Prueba Simple');
  gtk_window_set_default_size(GTK_WINDOW(Window), 400, 300);
  gtk_window_set_position(GTK_WINDOW(Window), GTK_WIN_POS_CENTER);
  
  // Conectar señal de cierre
  g_signal_connect(G_OBJECT(Window), 'destroy', G_CALLBACK(@OnDestroy), nil);
  
  // Crear contenedor
  VBox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(Window), VBox);
  gtk_container_set_border_width(GTK_CONTAINER(VBox), 20);
  
  // Crear botón de prueba
  Button := gtk_button_new_with_label('Probar EDDMail');
  g_signal_connect(G_OBJECT(Button), 'clicked', G_CALLBACK(@OnButtonClick), nil);
  gtk_box_pack_start(GTK_BOX(VBox), Button, False, False, 10);
  
  // Mostrar ventana
  gtk_widget_show_all(Window);
  
  writeln('EDDMail iniciado - Presiona el botón para probar');
  
  // Iniciar bucle principal
  gtk_main;
end.
