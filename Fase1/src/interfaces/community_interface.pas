unit community_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glib2, gtk2, UStructures;

type
  TCommunityInterface = class
  private
    window: PGtkWidget;
    communities_tree: PGtkWidget;

    procedure CreateWindow;
    procedure RefreshCommunities;
    procedure ShowMessage(const msg: String);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  end;

implementation

constructor TCommunityInterface.Create;
begin
  inherited Create;
  CreateWindow;
end;

destructor TCommunityInterface.Destroy;
begin
  if window <> nil then
    gtk_widget_destroy(window);
  inherited Destroy;
end;

procedure TCommunityInterface.CreateWindow;
var
  vbox: PGtkWidget;
  label: PGtkWidget;
  scrolled: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window), 'Visualizar Comunidades');
  gtk_window_set_default_size(GTK_WINDOW(window), 600, 400);
  gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);

  vbox := gtk_vbox_new(FALSE, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);
  gtk_container_add(GTK_CONTAINER(window), vbox);

  label := gtk_label_new('Lista de Comunidades Registradas');
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

  // Lista de comunidades
  communities_tree := gtk_tree_view_new();
  renderer := gtk_cell_renderer_text_new();

  column := gtk_tree_view_column_new_with_attributes('ID', renderer, 'text', 0, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(communities_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Nombre', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(communities_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Cantidad de Miembros', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(communities_tree), column);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(scrolled), communities_tree);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, TRUE, TRUE, 0);

  RefreshCommunities;

  g_signal_connect(G_OBJECT(window), 'destroy', G_CALLBACK(@gtk_widget_hide), window);
end;

procedure TCommunityInterface.RefreshCommunities;
var
  currentCommunity: PCommunity;
  currentMember: PMember;
  model: PGtkListStore;
  iter: TGtkTreeIter;
  member_count: Integer;
begin
  model := gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_INT);

  currentCommunity := CommunityList.head;
  while currentCommunity <> nil do
  begin
    // Contar miembros
    member_count := 0;
    currentMember := currentCommunity^.members;
    while currentMember <> nil do
    begin
      Inc(member_count);
      currentMember := currentMember^.next;
    end;

    gtk_list_store_append(model, @iter);
    gtk_list_store_set(model, @iter,
      0, currentCommunity^.id,
      1, PChar(currentCommunity^.nombre),
      2, member_count,
      -1);

    currentCommunity := currentCommunity^.next;
  end;

  gtk_tree_view_set_model(GTK_TREE_VIEW(communities_tree), GTK_TREE_MODEL(model));
end;

procedure TCommunityInterface.ShowMessage(const msg: String);
var
  dialog: PGtkWidget;
begin
  dialog := gtk_message_dialog_new(
    GTK_WINDOW(window),
    GTK_DIALOG_MODAL,
    GTK_MESSAGE_INFO,
    GTK_BUTTONS_OK,
    PChar(msg)
  );

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
end;

procedure TCommunityInterface.Show;
begin
  if window <> nil then
  begin
    RefreshCommunities;
    gtk_widget_show_all(window);
  end;
end;

procedure TCommunityInterface.Hide;
begin
  if window <> nil then
    gtk_widget_hide(window);
end;

end.

