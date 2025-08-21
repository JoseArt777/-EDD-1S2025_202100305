unit ContactWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DataStructures, SystemCore, UIBase,
  ContactManager;

type
  TContactWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FNavigationHBox: PGtkWidget;
    FContactFrame: PGtkWidget;
    FAddFrame: PGtkWidget;

    // Navegación
    FPrevButton: PGtkWidget;
    FNextButton: PGtkWidget;
    FPositionLabel: PGtkWidget;

    // Información del contacto actual
    FNameLabel: PGtkWidget;
    FUserLabel: PGtkWidget;
    FEmailLabel: PGtkWidget;
    FPhoneLabel: PGtkWidget;

    // Agregar contacto
    FAddEmailEntry: PGtkWidget;
    FAddButton: PGtkWidget;
    FStatusLabel: PGtkWidget;

    // Control de navegación circular
    FCurrentContact: PContact;
    FContactsCount: Integer;
    FCurrentPosition: Integer;

    procedure LoadContacts;
    procedure UpdateContactDisplay;
    procedure UpdateNavigationButtons;
    procedure ShowNextContact;
    procedure ShowPrevContact;
    procedure AddNewContact;
    procedure RefreshContactsList;

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create(AParent: PGtkWidget = nil);
    destructor Destroy; override;
    procedure RefreshContacts;
  end;

// Callbacks
procedure OnPrevClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnNextClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnAddContactClicked(widget: PGtkWidget; data: gpointer); cdecl;

implementation

constructor TContactWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Gestión de Contactos', 500, 400, AParent);
  FCurrentContact := nil;
  FContactsCount := 0;
  FCurrentPosition := 0;
  LoadContacts;
end;

destructor TContactWindow.Destroy;
begin
  inherited;
end;

procedure TContactWindow.SetupComponents;
var
  ContactInfoVBox, AddContactVBox: PGtkWidget;
  Table: PGtkWidget;
  Label1: PGtkWidget;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(15);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 15);

  // Título
  Label1 := TUIUtils.CreateLabel('Gestión de Contactos', True);
  gtk_label_set_markup(GTK_LABEL(Label1), '<span size="large" weight="bold">Gestión de Contactos</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 10);

  // Frame para información del contacto
  FContactFrame := TUIUtils.CreateFrame('Contacto Actual');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FContactFrame, True, True, 5);

  ContactInfoVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FContactFrame), ContactInfoVBox);
  gtk_container_set_border_width(GTK_CONTAINER(ContactInfoVBox), 10);

  // Navegación
  FNavigationHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(ContactInfoVBox), FNavigationHBox, False, False, 5);

  FPrevButton := TUIUtils.CreateButton('◀ Anterior', @OnPrevClicked, Self);
  FNextButton := TUIUtils.CreateButton('Siguiente ▶', @OnNextClicked, Self);
  FPositionLabel := TUIUtils.CreateLabel('0 / 0');

  gtk_box_pack_start(GTK_BOX(FNavigationHBox), FPrevButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(FNavigationHBox), FPositionLabel, True, True, 10);
  gtk_box_pack_start(GTK_BOX(FNavigationHBox), FNextButton, False, False, 5);

  // Tabla para información del contacto
  Table := TUIUtils.CreateTable(4, 2);
  gtk_box_pack_start(GTK_BOX(ContactInfoVBox), Table, True, True, 10);

  // Campos de información
  Label1 := TUIUtils.CreateLabel('Nombre:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 5, 5);
  FNameLabel := TUIUtils.CreateLabel('');
  gtk_table_attach(GTK_TABLE(Table), FNameLabel, 1, 2, 0, 1, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  Label1 := TUIUtils.CreateLabel('Usuario:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 1, 2, GTK_FILL, GTK_FILL, 5, 5);
  FUserLabel := TUIUtils.CreateLabel('');
  gtk_table_attach(GTK_TABLE(Table), FUserLabel, 1, 2, 1, 2, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  Label1 := TUIUtils.CreateLabel('Email:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 2, 3, GTK_FILL, GTK_FILL, 5, 5);
  FEmailLabel := TUIUtils.CreateLabel('');
  gtk_table_attach(GTK_TABLE(Table), FEmailLabel, 1, 2, 2, 3, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  Label1 := TUIUtils.CreateLabel('Teléfono:', True);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 3, 4, GTK_FILL, GTK_FILL, 5, 5);
  FPhoneLabel := TUIUtils.CreateLabel('');
  gtk_table_attach(GTK_TABLE(Table), FPhoneLabel, 1, 2, 3, 4, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  // Frame para agregar contacto
  FAddFrame := TUIUtils.CreateFrame('Agregar Nuevo Contacto');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FAddFrame, False, False, 5);

  AddContactVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FAddFrame), AddContactVBox);
  gtk_container_set_border_width(GTK_CONTAINER(AddContactVBox), 10);

  // Campo para email del nuevo contacto
  Table := TUIUtils.CreateTable(1, 3);
  gtk_box_pack_start(GTK_BOX(AddContactVBox), Table, False, False, 5);

  Label1 := TUIUtils.CreateLabel('Email del contacto:');
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 5, 5);

  FAddEmailEntry := TUIUtils.CreateEntry;
  gtk_table_attach(GTK_TABLE(Table), FAddEmailEntry, 1, 2, 0, 1,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  FAddButton := TUIUtils.CreateButton('➕ Agregar', @OnAddContactClicked, Self);
  gtk_table_attach(GTK_TABLE(Table), FAddButton, 2, 3, 0, 1, GTK_FILL, GTK_FILL, 5, 5);

  // Label de estado
  FStatusLabel := TUIUtils.CreateLabel('');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusLabel, False, False, 5);

  UpdateContactDisplay;
  UpdateNavigationButtons;
end;

procedure TContactWindow.ConnectSignals;
begin
  inherited;
end;

procedure TContactWindow.LoadContacts;
var
  UserContacts: TContactList;
begin
  if not IsUserLoggedIn then Exit;

  UserContacts := ContactManager.UserContactManager.GetUserContacts(CurrentUser^.Email);
  FContactsCount := UserContacts.GetCount;

  if FContactsCount > 0 then
  begin
    FCurrentContact := UserContacts.GetFirst;
    FCurrentPosition := 1;
  end
  else
  begin
    FCurrentContact := nil;
    FCurrentPosition := 0;
  end;
end;

procedure TContactWindow.UpdateContactDisplay;
begin
  if FCurrentContact = nil then
  begin
    gtk_label_set_text(GTK_LABEL(FNameLabel), 'Sin contactos');
    gtk_label_set_text(GTK_LABEL(FUserLabel), '');
    gtk_label_set_text(GTK_LABEL(FEmailLabel), '');
    gtk_label_set_text(GTK_LABEL(FPhoneLabel), '');
    gtk_label_set_text(GTK_LABEL(FPositionLabel), '0 / 0');
  end
  else
  begin
    gtk_label_set_text(GTK_LABEL(FNameLabel), PChar(FCurrentContact^.Nombre));
    gtk_label_set_text(GTK_LABEL(FUserLabel), PChar(FCurrentContact^.Usuario));
    gtk_label_set_text(GTK_LABEL(FEmailLabel), PChar(FCurrentContact^.Email));
    gtk_label_set_text(GTK_LABEL(FPhoneLabel), PChar(FCurrentContact^.Telefono));
    gtk_label_set_text(GTK_LABEL(FPositionLabel),
                      PChar(Format('%d / %d', [FCurrentPosition, FContactsCount])));
  end;
end;

procedure TContactWindow.UpdateNavigationButtons;
begin
  gtk_widget_set_sensitive(FPrevButton, (FContactsCount > 1));
  gtk_widget_set_sensitive(FNextButton, (FContactsCount > 1));
end;

procedure TContactWindow.ShowNextContact;
var
  UserContacts: TContactList;
begin
  if (FCurrentContact = nil) or (FContactsCount <= 1) then Exit;

  UserContacts := ContactManager.UserContactManager.GetUserContacts(CurrentUser^.Email);
  FCurrentContact := UserContacts.GetNext(FCurrentContact);

  Inc(FCurrentPosition);
  if FCurrentPosition > FContactsCount then
    FCurrentPosition := 1;

  UpdateContactDisplay;
end;

procedure TContactWindow.ShowPrevContact;
var
  UserContacts: TContactList;
begin
  if (FCurrentContact = nil) or (FContactsCount <= 1) then Exit;

  UserContacts := ContactManager.UserContactManager.GetUserContacts(CurrentUser^.Email);
  FCurrentContact := UserContacts.GetPrev(FCurrentContact);

  Dec(FCurrentPosition);
  if FCurrentPosition < 1 then
    FCurrentPosition := FContactsCount;

  UpdateContactDisplay;
end;

procedure TContactWindow.AddNewContact;
var
  ContactEmail: String;
begin
  ContactEmail := gtk_entry_get_text(GTK_ENTRY(FAddEmailEntry));

  if Length(ContactEmail) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese el email del contacto');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Email requerido');
    Exit;
  end;

  if ContactManager.AddContact(ContactEmail) then
  begin
    gtk_entry_set_text(GTK_ENTRY(FAddEmailEntry), '');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Contacto agregado: ' + ContactEmail);
    RefreshContactsList;
    TUIUtils.ShowInfoMessage(FWindow, 'Contacto agregado exitosamente');
  end
  else
  begin
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error al agregar contacto');
  end;
end;

procedure TContactWindow.RefreshContactsList;
var
  OldPosition: Integer;
  i: Integer;
begin
  OldPosition := FCurrentPosition;
  LoadContacts;

  // Intentar mantener la posición actual si es posible
  if (FContactsCount > 0) and (OldPosition <= FContactsCount) then
  begin
    FCurrentPosition := 1;
    for i := 2 to OldPosition do
      ShowNextContact;
  end;

  UpdateContactDisplay;
  UpdateNavigationButtons;
end;

procedure TContactWindow.RefreshContacts;
begin
  RefreshContactsList;
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnPrevClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ContactWindow: TContactWindow;
begin
  ContactWindow := TContactWindow(data);
  ContactWindow.ShowPrevContact;
end;

procedure OnNextClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ContactWindow: TContactWindow;
begin
  ContactWindow := TContactWindow(data);
  ContactWindow.ShowNextContact;
end;

procedure OnAddContactClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ContactWindow: TContactWindow;
begin
  ContactWindow := TContactWindow(data);
  ContactWindow.AddNewContact;
end;

end.

