unit ReportGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, DataStructures, SystemCore, UserManager,
  EmailManager, ContactManager, CommunityManager, FileManager;

// Funciones de generación de reportes
function GenerateUsersReport: String;
function GenerateRelationsReport: String;
function GenerateUserInboxReport(UserEmail: String): String;
function GenerateUserTrashReport(UserEmail: String): String;
function GenerateUserScheduledReport(UserEmail: String): String;
function GenerateUserContactsReport(UserEmail: String): String;
function GenerateCommunitiesGraphReport: String;

// Funciones para guardar reportes
function SaveUsersReport: Boolean;
function SaveRelationsReport: Boolean;
function SaveUserReports(UserEmail: String): Boolean;

// Funciones de generación de gráficos Graphviz
function GenerateUsersGraphviz: String;
function GenerateRelationsGraphviz: String;
function GenerateContactsGraphviz(UserEmail: String): String;
function GenerateCommunitiesGraphviz: String;

implementation

function GenerateUsersReport: String;
var
  Report: String;
  Current: PUser;
  Count: Integer;
begin
  Report := 'REPORTE DE USUARIOS REGISTRADOS' + LineEnding;
  Report := Report + '===============================' + LineEnding;
  Report := Report + 'Fecha: ' + DateTimeToStr(Now) + LineEnding + LineEnding;

  Count := 0;
  Current := UserList.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Usuario #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  Nombre: ' + Current^.Nombre + LineEnding;
    Report := Report + '  Usuario: ' + Current^.Usuario + LineEnding;
    Report := Report + '  Email: ' + Current^.Email + LineEnding;
    Report := Report + '  Teléfono: ' + Current^.Telefono + LineEnding;
    Report := Report + '  ----------------------' + LineEnding;

    Current := Current^.Next;
  end;

  Report := Report + LineEnding + 'RESUMEN:' + LineEnding;
  Report := Report + 'Total de usuarios: ' + IntToStr(UserList.GetCount) + LineEnding;

  Result := Report;
end;

function GenerateRelationsReport: String;
var
  Report: String;
  SenderUser, ReceiverUser: PUser;
  i, j, EmailCount: Integer;
  TotalEmails: Integer;
begin
  Report := 'REPORTE DE RELACIONES EMISOR-RECEPTOR' + LineEnding;
  Report := Report + '=====================================' + LineEnding;
  Report := Report + 'Fecha: ' + DateTimeToStr(Now) + LineEnding + LineEnding;

  Report := Report + 'Matriz de correos enviados:' + LineEnding;
  Report := Report + '(Filas: Emisores, Columnas: Receptores)' + LineEnding + LineEnding;

  TotalEmails := 0;

  // Encabezados de columnas
  Report := Report + 'EMISOR -> RECEPTOR: CANTIDAD' + LineEnding;
  Report := Report + '=============================' + LineEnding;

  // Recorrer todos los usuarios como emisores
  SenderUser := UserList.GetFirst;
  while SenderUser <> nil do
  begin
    // Recorrer todos los usuarios como receptores
    ReceiverUser := UserList.GetFirst;
    while ReceiverUser <> nil do
    begin
      if SenderUser^.Id <> ReceiverUser^.Id then
      begin
        EmailCount := EmailRelationMatrix.GetValue(SenderUser^.Id, ReceiverUser^.Id);
        if EmailCount > 0 then
        begin
          Report := Report + SenderUser^.Email + ' -> ' + ReceiverUser^.Email + ': ' + IntToStr(EmailCount) + LineEnding;
          TotalEmails := TotalEmails + EmailCount;
        end;
      end;
      ReceiverUser := ReceiverUser^.Next;
    end;
    SenderUser := SenderUser^.Next;
  end;

  Report := Report + LineEnding + 'RESUMEN:' + LineEnding;
  Report := Report + 'Total de correos enviados: ' + IntToStr(TotalEmails) + LineEnding;

  Result := Report;
end;

function GenerateUserInboxReport(UserEmail: String): String;
var
  Report: String;
  UserInbox: TEmailList;
  Current: PEmail;
  Count, UnreadCount: Integer;
begin
  Report := 'REPORTE DE CORREOS RECIBIDOS' + LineEnding;
  Report := Report + '============================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding;
  Report := Report + 'Fecha: ' + DateTimeToStr(Now) + LineEnding + LineEnding;

  UserInbox := GetUserInbox(UserEmail);

  Count := 0;
  UnreadCount := 0;
  Current := UserInbox.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    if Current^.Estado = 'NL' then
      Inc(UnreadCount);

    Report := Report + 'Email #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  De: ' + Current^.Remitente + LineEnding;
    Report := Report + '  Asunto: ' + Current^.Asunto + LineEnding;
    Report := Report + '  Fecha: ' + DateTimeToStr(Current^.Fecha) + LineEnding;
    Report := Report + '  Estado: ';
    if Current^.Estado = 'NL' then
      Report := Report + 'No Leído'
    else
      Report := Report + 'Leído';
    Report := Report + LineEnding;
    Report := Report + '  Mensaje: ' + Current^.Mensaje + LineEnding;
    Report := Report + '  ----------------------' + LineEnding;

    Current := Current^.Next;
  end;

  if Count = 0 then
    Report := Report + 'No hay correos en la bandeja de entrada.' + LineEnding;

  Report := Report + LineEnding + 'RESUMEN:' + LineEnding;
  Report := Report + 'Total de correos: ' + IntToStr(Count) + LineEnding;
  Report := Report + 'Correos no leídos: ' + IntToStr(UnreadCount) + LineEnding;
  Report := Report + 'Correos leídos: ' + IntToStr(Count - UnreadCount) + LineEnding;

  Result := Report;
end;

function GenerateUserTrashReport(UserEmail: String): String;
var
  Report: String;
  UserTrash: TEmailStack;
  Current: PEmail;
  Count: Integer;
begin
  Report := 'REPORTE DE PAPELERA' + LineEnding;
  Report := Report + '==================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding;
  Report := Report + 'Fecha: ' + DateTimeToStr(Now) + LineEnding + LineEnding;

  UserTrash := GetUserTrash(UserEmail);

  Count := 0;
  Current := UserTrash.GetTop;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Email Eliminado #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  De: ' + Current^.Remitente + LineEnding;
    Report := Report + '  Asunto: ' + Current^.Asunto + LineEnding;
    Report := Report + '  Fecha: ' + DateTimeToStr(Current^.Fecha) + LineEnding;
    Report := Report + '  Mensaje: ' + Current^.Mensaje + LineEnding;
    Report := Report + '  ----------------------' + LineEnding;

    Current := Current^.Next;
  end;

  if Count = 0 then
    Report := Report + 'La papelera está vacía.' + LineEnding;

  Report := Report + LineEnding + 'RESUMEN:' + LineEnding;
  Report := Report + 'Total de correos eliminados: ' + IntToStr(Count) + LineEnding;

  Result := Report;
end;

function GenerateUserScheduledReport(UserEmail: String): String;
var
  Report: String;
  UserScheduled: TEmailQueue;
  Current: PEmail;
  Count: Integer;
begin
  Report := 'REPORTE DE CORREOS PROGRAMADOS' + LineEnding;
  Report := Report + '==============================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding;
  Report := Report + 'Fecha: ' + DateTimeToStr(Now) + LineEnding + LineEnding;

  UserScheduled := GetUserScheduled(UserEmail);

  Count := 0;
  Current := UserScheduled.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Email Programado #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  Para: ' + Current^.Destinatario + LineEnding;
    Report := Report + '  Asunto: ' + Current^.Asunto + LineEnding;
    Report := Report + '  Fecha de envío: ' + DateTimeToStr(Current^.Fecha) + LineEnding;
    Report := Report + '  Mensaje: ' + Current^.Mensaje + LineEnding;
    Report := Report + '  ----------------------' + LineEnding;

    Current := Current^.Next;
  end;

  if Count = 0 then
    Report := Report + 'No hay correos programados.' + LineEnding;

  Report := Report + LineEnding + 'RESUMEN:' + LineEnding;
  Report := Report + 'Total de correos programados: ' + IntToStr(Count) + LineEnding;

  Result := Report;
end;

function GenerateUserContactsReport(UserEmail: String): String;
begin
  Result := ContactManager.GenerateContactsReport(UserEmail);
end;

function GenerateCommunitiesGraphReport: String;
begin
  Result := CommunityManager.GenerateCommunitiesReport;
end;

// Funciones para guardar reportes
function SaveUsersReport: Boolean;
var
  Report: String;
begin
  Report := GenerateUsersReport;
  Result := SaveRootReport('Reporte_Usuarios', Report);
end;

function SaveRelationsReport: Boolean;
var
  Report: String;
begin
  Report := GenerateRelationsReport;
  Result := SaveRootReport('Reporte_Relaciones', Report);
end;

function SaveUserReports(UserEmail: String): Boolean;
var
  InboxReport, TrashReport, ScheduledReport, ContactsReport: String;
  Success: Boolean;
begin
  Success := True;

  // Generar y guardar reporte de bandeja de entrada
  InboxReport := GenerateUserInboxReport(UserEmail);
  if not SaveUserReport(UserEmail, 'Reporte_Correos_Recibidos', InboxReport) then
    Success := False;

  // Generar y guardar reporte de papelera
  TrashReport := GenerateUserTrashReport(UserEmail);
  if not SaveUserReport(UserEmail, 'Reporte_Papelera', TrashReport) then
    Success := False;

  // Generar y guardar reporte de correos programados
  ScheduledReport := GenerateUserScheduledReport(UserEmail);
  if not SaveUserReport(UserEmail, 'Reporte_Correos_Programados', ScheduledReport) then
    Success := False;

  // Generar y guardar reporte de contactos
  ContactsReport := GenerateUserContactsReport(UserEmail);
  if not SaveUserReport(UserEmail, 'Reporte_Contactos', ContactsReport) then
    Success := False;

  Result := Success;
end;

// Funciones de generación de gráficos Graphviz
function GenerateUsersGraphviz: String;
var
  Graphviz: String;
  Current: PUser;
  Count: Integer;
begin
  Graphviz := 'digraph usuarios {' + LineEnding;
  Graphviz := Graphviz + '  rankdir=LR;' + LineEnding;
  Graphviz := Graphviz + '  node [shape=box, style=filled, fillcolor=lightblue];' + LineEnding;
  Graphviz := Graphviz + '  label="Lista de Usuarios Registrados";' + LineEnding + LineEnding;

  Count := 0;
  Current := UserList.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    Graphviz := Graphviz + '  user' + IntToStr(Current^.Id) + ' [label="ID: ' + IntToStr(Current^.Id) +
                '\\nNombre: ' + Current^.Nombre +
                '\\nUsuario: ' + Current^.Usuario +
                '\\nEmail: ' + Current^.Email +
                '\\nTeléfono: ' + Current^.Telefono + '"];' + LineEnding;

    if Current^.Next <> nil then
      Graphviz := Graphviz + '  user' + IntToStr(Current^.Id) + ' -> user' + IntToStr(Current^.Next^.Id) + ';' + LineEnding;

    Current := Current^.Next;
  end;

  Graphviz := Graphviz + '}' + LineEnding;
  Result := Graphviz;
end;

function GenerateRelationsGraphviz: String;
var
  Graphviz: String;
  SenderUser, ReceiverUser: PUser;
  EmailCount: Integer;
begin
  Graphviz := 'digraph relaciones {' + LineEnding;
  Graphviz := Graphviz + '  rankdir=LR;' + LineEnding;
  Graphviz := Graphviz + '  node [shape=ellipse, style=filled, fillcolor=lightgreen];' + LineEnding;
  Graphviz := Graphviz + '  label="Matriz de Relaciones Emisor-Receptor";' + LineEnding + LineEnding;

  // Crear nodos para cada usuario
  SenderUser := UserList.GetFirst;
  while SenderUser <> nil do
  begin
    Graphviz := Graphviz + '  "' + SenderUser^.Email + '" [label="' + SenderUser^.Email + '"];' + LineEnding;
    SenderUser := SenderUser^.Next;
  end;

  Graphviz := Graphviz + LineEnding;

  // Crear aristas con pesos
  SenderUser := UserList.GetFirst;
  while SenderUser <> nil do
  begin
    ReceiverUser := UserList.GetFirst;
    while ReceiverUser <> nil do
    begin
      if SenderUser^.Id <> ReceiverUser^.Id then
      begin
        EmailCount := EmailRelationMatrix.GetValue(SenderUser^.Id, ReceiverUser^.Id);
        if EmailCount > 0 then
        begin
          Graphviz := Graphviz + '  "' + SenderUser^.Email + '" -> "' + ReceiverUser^.Email +
                      '" [label="' + IntToStr(EmailCount) + '"];' + LineEnding;
        end;
      end;
      ReceiverUser := ReceiverUser^.Next;
    end;
    SenderUser := SenderUser^.Next;
  end;

  Graphviz := Graphviz + '}' + LineEnding;
  Result := Graphviz;
end;

function GenerateContactsGraphviz(UserEmail: String): String;
var
  Graphviz: String;
  UserContacts: TContactList;
  Current: PContact;
  FirstContact: PContact;
  Count: Integer;
begin
  Graphviz := 'digraph contactos {' + LineEnding;
  Graphviz := Graphviz + '  rankdir=LR;' + LineEnding;
  Graphviz := Graphviz + '  node [shape=box, style=filled, fillcolor=lightyellow];' + LineEnding;
  Graphviz := Graphviz + '  label="Lista Circular de Contactos - ' + UserEmail + '";' + LineEnding + LineEnding;

  UserContacts := ContactManager.UserContactManager.GetUserContacts(UserEmail);

  if UserContacts.GetCount = 0 then
  begin
    Graphviz := Graphviz + '  empty [label="Sin contactos"];' + LineEnding;
  end
  else
  begin
    Count := 0;
    FirstContact := UserContacts.GetFirst;
    Current := FirstContact;

    if Current <> nil then
    begin
      repeat
        Inc(Count);
        Graphviz := Graphviz + '  contact' + IntToStr(Current^.Id) + ' [label="ID: ' + IntToStr(Current^.Id) +
                    '\\nNombre: ' + Current^.Nombre +
                    '\\nUsuario: ' + Current^.Usuario +
                    '\\nEmail: ' + Current^.Email +
                    '\\nTeléfono: ' + Current^.Telefono + '"];' + LineEnding;

        Current := UserContacts.GetNext(Current);
      until (Current = FirstContact) or (Count >= UserContacts.GetCount);

      // Crear conexiones circulares
      Count := 0;
      Current := FirstContact;
      repeat
        Inc(Count);
        Graphviz := Graphviz + '  contact' + IntToStr(Current^.Id) + ' -> contact' +
                    IntToStr(UserContacts.GetNext(Current)^.Id) + ';' + LineEnding;

        Current := UserContacts.GetNext(Current);
      until (Current = FirstContact) or (Count >= UserContacts.GetCount);
    end;
  end;

  Graphviz := Graphviz + '}' + LineEnding;
  Result := Graphviz;
end;

function GenerateCommunitiesGraphviz: String;
var
  Graphviz: String;
  CurrentCommunity: PCommunity;
  CurrentUser: PUserCommunity;
  CommunityCount: Integer;
begin
  Graphviz := 'digraph comunidades {' + LineEnding;
  Graphviz := Graphviz + '  rankdir=TB;' + LineEnding;
  Graphviz := Graphviz + '  node [shape=box];' + LineEnding;
  Graphviz := Graphviz + '  label="Lista de Comunidades y Usuarios";' + LineEnding + LineEnding;

  CommunityCount := 0;
  CurrentCommunity := CommunityList.GetFirst;

  if CurrentCommunity = nil then
  begin
    Graphviz := Graphviz + '  empty [label="Sin comunidades", style=filled, fillcolor=lightgray];' + LineEnding;
  end
  else
  begin
    while CurrentCommunity <> nil do
    begin
      Inc(CommunityCount);

      // Nodo de la comunidad
      Graphviz := Graphviz + '  community' + IntToStr(CurrentCommunity^.Id) +
                  ' [label="' + CurrentCommunity^.Nombre + '", style=filled, fillcolor=lightblue];' + LineEnding;

      // Nodos de usuarios en la comunidad
      CurrentUser := CurrentCommunity^.Users;
      while CurrentUser <> nil do
      begin
        Graphviz := Graphviz + '  user_' + StringReplace(CurrentUser^.Email, '@', '_', [rfReplaceAll]) +
                    StringReplace('', '.', '_', [rfReplaceAll]) +
                    ' [label="' + CurrentUser^.Email + '", style=filled, fillcolor=lightgreen];' + LineEnding;

        // Conexión de comunidad a usuario
        Graphviz := Graphviz + '  community' + IntToStr(CurrentCommunity^.Id) + ' -> user_' +
                    StringReplace(CurrentUser^.Email, '@', '_', [rfReplaceAll]) +
                    StringReplace('', '.', '_', [rfReplaceAll]) + ';' + LineEnding;

        CurrentUser := CurrentUser^.Next;
      end;

      CurrentCommunity := CurrentCommunity^.Next;
    end;
  end;

  Graphviz := Graphviz + '}' + LineEnding;
  Result := Graphviz;
end;

end.
