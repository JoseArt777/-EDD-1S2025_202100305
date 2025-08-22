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

  // CORREGIDO: Usar SysUtils.DateTimeToStr explícitamente
  try
    Report := Report + 'Fecha: ' + SysUtils.DateTimeToStr(Now) + LineEnding + LineEnding;
  except
    Report := Report + 'Fecha: [Error obteniendo fecha]' + LineEnding + LineEnding;
  end;

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
    Report := Report + LineEnding;
    Current := Current^.Next;
  end;

  if Count = 0 then
    Report := Report + 'No hay usuarios registrados.' + LineEnding;

  Result := Report;
end;

function GenerateRelationsReport: String;
var
  Report: String;
  i, j: Integer;
  Value: Integer;
begin
  Report := 'REPORTE DE RELACIONES (MATRIZ DISPERSA)' + LineEnding;
  Report := Report + '=======================================' + LineEnding;

  // CORREGIDO: Usar SysUtils.DateTimeToStr explícitamente
  try
    Report := Report + 'Fecha: ' + SysUtils.DateTimeToStr(Now) + LineEnding + LineEnding;
  except
    Report := Report + 'Fecha: [Error obteniendo fecha]' + LineEnding + LineEnding;
  end;

  if EmailRelationMatrix = nil then
  begin
    Report := Report + 'Error: Matriz de relaciones no inicializada.' + LineEnding;
    Result := Report;
    Exit;
  end;

  Report := Report + 'Relaciones encontradas:' + LineEnding;

  // CORREGIDO: Proteger acceso a matriz con manejo de errores
  try
    for i := 0 to 99 do // Limitar búsqueda para evitar sobrecarga
    begin
      for j := 0 to 99 do
      begin
        Value := EmailRelationMatrix.GetValue(i, j);
        if Value > 0 then
        begin
          Report := Report + Format('  Usuario %d -> Usuario %d: %d emails', [i, j, Value]) + LineEnding;
        end;
      end;
    end;
  except
    on E: Exception do
      Report := Report + 'Error accediendo a la matriz: ' + E.Message + LineEnding;
  end;

  Result := Report;
end;

function GenerateUserInboxReport(UserEmail: String): String;
var
  Report: String;
  Emails: TEmailList;
  Current: PEmail;
  Count: Integer;
begin
  Report := 'REPORTE DE BANDEJA DE ENTRADA' + LineEnding;
  Report := Report + '=============================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding;

  // CORREGIDO: Usar SysUtils.DateTimeToStr explícitamente
  try
    Report := Report + 'Fecha: ' + SysUtils.DateTimeToStr(Now) + LineEnding + LineEnding;
  except
    Report := Report + 'Fecha: [Error obteniendo fecha]' + LineEnding + LineEnding;
  end;

  Emails := GetUserEmailsReceived(UserEmail);
  if Emails = nil then
  begin
    Report := Report + 'Error: No se pudieron obtener los emails.' + LineEnding;
    Result := Report;
    Exit;
  end;

  Count := 0;
  Current := Emails.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Email #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  De: ' + Current^.Remitente + LineEnding;
    Report := Report + '  Asunto: ' + Current^.Asunto + LineEnding;
    Report := Report + '  Estado: ' + Current^.Estado + LineEnding;

    // CORREGIDO: Formatear fecha con manejo de errores
    try
      if Current^.Fecha > 0 then
        Report := Report + '  Fecha: ' + SysUtils.DateTimeToStr(Current^.Fecha) + LineEnding
      else
        Report := Report + '  Fecha: [No especificada]' + LineEnding;
    except
      Report := Report + '  Fecha: [Error formato]' + LineEnding;
    end;

    Report := Report + '  Mensaje: ' + Copy(Current^.Mensaje, 1, 100);
    if Length(Current^.Mensaje) > 100 then
      Report := Report + '...';
    Report := Report + LineEnding + LineEnding;

    Current := Current^.Next;
  end;

  if Count = 0 then
    Report := Report + 'No hay emails en la bandeja de entrada.' + LineEnding;

  Result := Report;
end;

function GenerateUserTrashReport(UserEmail: String): String;
var
  Report: String;
  Emails: TEmailStack;
  Current: PEmail;
  Count: Integer;
begin
  Report := 'REPORTE DE PAPELERA' + LineEnding;
  Report := Report + '==================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding;

  // CORREGIDO: Usar SysUtils.DateTimeToStr explícitamente
  try
    Report := Report + 'Fecha: ' + SysUtils.DateTimeToStr(Now) + LineEnding + LineEnding;
  except
    Report := Report + 'Fecha: [Error obteniendo fecha]' + LineEnding + LineEnding;
  end;

  Emails := GetUserEmailsDeleted(UserEmail);
  if Emails = nil then
  begin
    Report := Report + 'Error: No se pudieron obtener los emails eliminados.' + LineEnding;
    Result := Report;
    Exit;
  end;

  Count := 0;
  Current := Emails.GetTop;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Email Eliminado #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  De: ' + Current^.Remitente + LineEnding;
    Report := Report + '  Asunto: ' + Current^.Asunto + LineEnding;

    // CORREGIDO: Formatear fecha con manejo de errores
    try
      if Current^.Fecha > 0 then
        Report := Report + '  Fecha: ' + SysUtils.DateTimeToStr(Current^.Fecha) + LineEnding
      else
        Report := Report + '  Fecha: [No especificada]' + LineEnding;
    except
      Report := Report + '  Fecha: [Error formato]' + LineEnding;
    end;

    Report := Report + '  Mensaje: ' + Copy(Current^.Mensaje, 1, 100);
    if Length(Current^.Mensaje) > 100 then
      Report := Report + '...';
    Report := Report + LineEnding + LineEnding;

    Current := Current^.Next;
  end;

  if Count = 0 then
    Report := Report + 'No hay emails en la papelera.' + LineEnding;

  Result := Report;
end;

function GenerateUserScheduledReport(UserEmail: String): String;
var
  Report: String;
  Emails: TEmailQueue;
  Current: PEmail;
  Count: Integer;
begin
  Report := 'REPORTE DE CORREOS PROGRAMADOS' + LineEnding;
  Report := Report + '==============================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding;

  // CORREGIDO: Usar SysUtils.DateTimeToStr explícitamente
  try
    Report := Report + 'Fecha: ' + SysUtils.DateTimeToStr(Now) + LineEnding + LineEnding;
  except
    Report := Report + 'Fecha: [Error obteniendo fecha]' + LineEnding + LineEnding;
  end;

  Emails := GetUserEmailsScheduled(UserEmail);
  if Emails = nil then
  begin
    Report := Report + 'Error: No se pudieron obtener los emails programados.' + LineEnding;
    Result := Report;
    Exit;
  end;

  Count := 0;
  Current := Emails.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Email Programado #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  Para: ' + Current^.Destinatario + LineEnding;
    Report := Report + '  Asunto: ' + Current^.Asunto + LineEnding;

    // CORREGIDO: Formatear fecha con manejo de errores
    try
      if Current^.Fecha > 0 then
        Report := Report + '  Fecha programada: ' + SysUtils.DateTimeToStr(Current^.Fecha) + LineEnding
      else
        Report := Report + '  Fecha programada: [No especificada]' + LineEnding;
    except
      Report := Report + '  Fecha programada: [Error formato]' + LineEnding;
    end;

    Report := Report + '  Mensaje: ' + Copy(Current^.Mensaje, 1, 100);
    if Length(Current^.Mensaje) > 100 then
      Report := Report + '...';
    Report := Report + LineEnding + LineEnding;

    Current := Current^.Next;
  end;

  if Count = 0 then
    Report := Report + 'No hay emails programados.' + LineEnding;

  Result := Report;
end;

function GenerateUserContactsReport(UserEmail: String): String;
var
  Report: String;
  Contacts: TContactList;
  Current: PContact;
  Count: Integer;
begin
  Report := 'REPORTE DE CONTACTOS' + LineEnding;
  Report := Report + '===================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding;

  // CORREGIDO: Usar SysUtils.DateTimeToStr explícitamente
  try
    Report := Report + 'Fecha: ' + SysUtils.DateTimeToStr(Now) + LineEnding + LineEnding;
  except
    Report := Report + 'Fecha: [Error obteniendo fecha]' + LineEnding + LineEnding;
  end;

  Contacts := GetUserContacts(UserEmail);
  if Contacts = nil then
  begin
    Report := Report + 'Error: No se pudieron obtener los contactos.' + LineEnding;
    Result := Report;
    Exit;
  end;

  Count := 0;
  Current := Contacts.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Contacto #' + IntToStr(Count) + LineEnding;
    Report := Report + '  ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + '  Nombre: ' + Current^.Nombre + LineEnding;
    Report := Report + '  Usuario: ' + Current^.Usuario + LineEnding;
    Report := Report + '  Email: ' + Current^.Email + LineEnding;
    Report := Report + '  Teléfono: ' + Current^.Telefono + LineEnding;
    Report := Report + LineEnding;

    Current := Current^.Next;

    // Protección contra bucles infinitos en lista circular
    if Count > 1000 then
    begin
      Report := Report + '[... más contactos no mostrados para evitar bucle infinito ...]' + LineEnding;
      break;
    end;
  end;

  if Count = 0 then
    Report := Report + 'No hay contactos registrados.' + LineEnding;

  Result := Report;
end;

function GenerateCommunitiesGraphReport: String;
var
  Report: String;
begin
  Report := 'REPORTE DE COMUNIDADES' + LineEnding;
  Report := Report + '=====================' + LineEnding;

  // CORREGIDO: Usar SysUtils.DateTimeToStr explícitamente
  try
    Report := Report + 'Fecha: ' + SysUtils.DateTimeToStr(Now) + LineEnding + LineEnding;
  except
    Report := Report + 'Fecha: [Error obteniendo fecha]' + LineEnding + LineEnding;
  end;

  // TODO: Implementar cuando las comunidades estén funcionales
  Report := Report + 'Funcionalidad de comunidades pendiente de implementación.' + LineEnding;

  Result := Report;
end;

// CORREGIDO: Funciones para guardar reportes usando las funciones correctas de FileManager
function SaveUsersReport: Boolean;
var
  Report: String;
  FileName: String;
begin
  Result := False;
  try
    Report := GenerateUsersReport;

    // CORREGIDO: Usar SaveRootReport en lugar de SaveReportToFile
    Result := SaveRootReport('Reporte_Usuarios', Report);
  except
    on E: Exception do
    begin
      WriteLn('Error guardando reporte de usuarios: ', E.Message);
      Result := False;
    end;
  end;
end;

function SaveRelationsReport: Boolean;
var
  Report: String;
  FileName: String;
begin
  Result := False;
  try
    Report := GenerateRelationsReport;

    // CORREGIDO: Usar SaveRootReport en lugar de SaveReportToFile
    Result := SaveRootReport('Reporte_Relaciones', Report);
  except
    on E: Exception do
    begin
      WriteLn('Error guardando reporte de relaciones: ', E.Message);
      Result := False;
    end;
  end;
end;

function SaveUserReports(UserEmail: String): Boolean;
var
  InboxReport, TrashReport, ScheduledReport, ContactsReport: String;
  Success: Boolean;
begin
  Success := True;
  try
    // Generar todos los reportes del usuario
    InboxReport := GenerateUserInboxReport(UserEmail);
    TrashReport := GenerateUserTrashReport(UserEmail);
    ScheduledReport := GenerateUserScheduledReport(UserEmail);
    ContactsReport := GenerateUserContactsReport(UserEmail);

    // CORREGIDO: Usar SaveUserReport en lugar de SaveReportToFile
    if not SaveUserReport(UserEmail, 'Reporte_Bandeja_Entrada', InboxReport) then
      Success := False;

    if not SaveUserReport(UserEmail, 'Reporte_Papelera', TrashReport) then
      Success := False;

    if not SaveUserReport(UserEmail, 'Reporte_Correos_Programados', ScheduledReport) then
      Success := False;

    if not SaveUserReport(UserEmail, 'Reporte_Contactos', ContactsReport) then
      Success := False;

    Result := Success;
  except
    on E: Exception do
    begin
      WriteLn('Error guardando reportes del usuario: ', E.Message);
      Result := False;
    end;
  end;
end;

// Funciones de generación de gráficos Graphviz
function GenerateUsersGraphviz: String;
var
  Graph: String;
  Current: PUser;
begin
  Graph := 'digraph UsersGraph {' + LineEnding;
  Graph := Graph + '  rankdir=LR;' + LineEnding;
  Graph := Graph + '  node [shape=box];' + LineEnding + LineEnding;

  Current := UserList.GetFirst;
  while Current <> nil do
  begin
    Graph := Graph + Format('  "User_%d" [label="%s\\n%s"];',
                           [Current^.Id, Current^.Nombre, Current^.Email]) + LineEnding;
    Current := Current^.Next;
  end;

  Graph := Graph + '}' + LineEnding;
  Result := Graph;
end;

function GenerateRelationsGraphviz: String;
var
  Graph: String;
begin
  Graph := 'digraph RelationsGraph {' + LineEnding;
  Graph := Graph + '  rankdir=LR;' + LineEnding;
  Graph := Graph + '  node [shape=circle];' + LineEnding + LineEnding;

  // TODO: Implementar basado en la matriz dispersa
  Graph := Graph + '  // Relaciones pendientes de implementación' + LineEnding;

  Graph := Graph + '}' + LineEnding;
  Result := Graph;
end;

function GenerateContactsGraphviz(UserEmail: String): String;
var
  Graph: String;
  Contacts: TContactList;
  Current: PContact;
  SafeEmail: String;
  Count: Integer;
begin
  SafeEmail := StringReplace(UserEmail, '@', '_AT_', [rfReplaceAll]);
  SafeEmail := StringReplace(SafeEmail, '.', '_DOT_', [rfReplaceAll]);

  Graph := 'digraph ContactsGraph {' + LineEnding;
  Graph := Graph + '  rankdir=LR;' + LineEnding;
  Graph := Graph + '  node [shape=ellipse];' + LineEnding + LineEnding;

  Graph := Graph + Format('  "%s" [shape=box, style=filled, fillcolor=lightblue];', [SafeEmail]) + LineEnding;

  Contacts := GetUserContacts(UserEmail);
  if Contacts <> nil then
  begin
    Count := 0;
    Current := Contacts.GetFirst;
    while (Current <> nil) and (Count < 100) do // Protección contra bucles infinitos
    begin
      Graph := Graph + Format('  "%s" -> "%s";',
                             [SafeEmail, StringReplace(Current^.Email, '@', '_AT_', [rfReplaceAll])]) + LineEnding;
      Current := Current^.Next;
      Inc(Count);
    end;
  end;

  Graph := Graph + '}' + LineEnding;
  Result := Graph;
end;

function GenerateCommunitiesGraphviz: String;
var
  Graph: String;
begin
  Graph := 'digraph CommunitiesGraph {' + LineEnding;
  Graph := Graph + '  rankdir=TB;' + LineEnding;
  Graph := Graph + '  node [shape=hexagon];' + LineEnding + LineEnding;

  // TODO: Implementar cuando las comunidades estén funcionales
  Graph := Graph + '  // Comunidades pendientes de implementación' + LineEnding;

  Graph := Graph + '}' + LineEnding;
  Result := Graph;
end;

end.
