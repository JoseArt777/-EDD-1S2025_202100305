unit ReportGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SystemCore, DataStructures;

type
  TReportGenerator = class
  public
    class function GenerateUserReport: Boolean;
    class function GenerateRelationsReport: Boolean;
    class function GenerateUserReports(UserName: String): Boolean;
    class function GenerateInboxReport(UserName: String): Boolean;
    class function GenerateTrashReport(UserName: String): Boolean;
    class function GenerateScheduledReport(UserName: String): Boolean;
    class function GenerateContactsReport(UserName: String): Boolean;
    class function GenerateCommunitiesReport: Boolean;

  private
    class function CreateDirectory(const DirName: String): Boolean;
    class function WriteToFile(const FileName, Content: String): Boolean;
    class function EscapeGraphvizString(const Str: String): String;
  end;

var
  ReportGenerator: TReportGenerator;

implementation

uses
  DateUtils;

class function TReportGenerator.CreateDirectory(const DirName: String): Boolean;
begin
  Result := True;
  if not DirectoryExists(DirName) then
  begin
    try
      MkDir(DirName);
    except
      Result := False;
    end;
  end;
end;

class function TReportGenerator.WriteToFile(const FileName, Content: String): Boolean;
var
  FileStream: TFileStream;
begin
  Result := True;
  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.WriteBuffer(Content[1], Length(Content));
    finally
      FileStream.Free;
    end;
  except
    Result := False;
  end;
end;

class function TReportGenerator.EscapeGraphvizString(const Str: String): String;
begin
  Result := StringReplace(Str, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '', [rfReplaceAll]);
end;

class function TReportGenerator.GenerateUserReport: Boolean;
var
  DirName, FileName, Content: String;
  User: PUser;
  NodeCount: Integer;
begin
  Result := False;

  DirName := 'Root-Reportes';
  if not CreateDirectory(DirName) then Exit;

  FileName := DirName + '/reporte_usuarios.dot';
  Content := 'digraph ReporteUsuarios {' + LineEnding +
             '    label="Reporte de Usuarios Registrados";' + LineEnding +
             '    labelloc="t";' + LineEnding +
             '    fontsize=16;' + LineEnding +
             '    node [shape=box, style=filled, fillcolor=lightblue];' + LineEnding +
             LineEnding;

  User := SystemCore.FUsers.GetFirst;
  NodeCount := 0;

  while User <> nil do
  begin
    Content := Content + Format('    user%d [label="ID: %d\nNombre: %s\nUsuario: %s\nEmail: %s\nTeléfono: %s"];' + LineEnding,
                               [NodeCount, User^.Id, EscapeGraphvizString(User^.Nombre),
                                EscapeGraphvizString(User^.Usuario), EscapeGraphvizString(User^.Email),
                                EscapeGraphvizString(User^.Telefono)]);

    if User^.Next <> nil then
      Content := Content + Format('    user%d -> user%d;' + LineEnding, [NodeCount, NodeCount + 1]);

    User := User^.Next;
    Inc(NodeCount);
  end;

  Content := Content + '}' + LineEnding;
  Result := WriteToFile(FileName, Content);
end;

class function TReportGenerator.GenerateRelationsReport: Boolean;
var
  DirName, FileName, Content: String;
  Matrix: TSparseMatrix;
  User1, User2: PUser;
  Value: Integer;
  UserArray: array of PUser;
  UserCount, i, j: Integer;
begin
  Result := False;

  DirName := 'Root-Reportes';
  if not CreateDirectory(DirName) then Exit;

  FileName := DirName + '/reporte_relaciones.dot';
  Content := 'digraph ReporteRelaciones {' + LineEnding +
             '    label="Matriz de Relaciones Emisor vs Destinatario";' + LineEnding +
             '    labelloc="t";' + LineEnding +
             '    fontsize=16;' + LineEnding +
             '    node [shape=circle, style=filled];' + LineEnding +
             LineEnding;

  // Crear array de usuarios para acceso fácil
  UserCount := SystemCore.FUsers.GetCount;
  SetLength(UserArray, UserCount);
  User1 := SystemCore.FUsers.GetFirst;
  i := 0;
  while (User1 <> nil) and (i < UserCount) do
  begin
    UserArray[i] := User1;
    User1 := User1^.Next;
    Inc(i);
  end;

  // Agregar nodos de usuarios
  for i := 0 to UserCount - 1 do
  begin
    Content := Content + Format('    "%s" [fillcolor=lightgreen];' + LineEnding,
                               [EscapeGraphvizString(UserArray[i]^.Email)]);
  end;

  Content := Content + LineEnding;

  // Agregar relaciones basadas en la matriz
  Matrix := SystemCore.GetEmailMatrix;
  for i := 0 to UserCount - 1 do
  begin
    for j := 0 to UserCount - 1 do
    begin
      if (i < Length(UserArray)) and (j < Length(UserArray)) then
      begin
        Value := Matrix.GetValue(UserArray[i]^.Id, UserArray[j]^.Id);
        if Value > 0 then
        begin
          Content := Content + Format('    "%s" -> "%s" [label="%d", fontcolor=red];' + LineEnding,
                                     [EscapeGraphvizString(UserArray[i]^.Email),
                                      EscapeGraphvizString(UserArray[j]^.Email), Value]);
        end;
      end;
    end;
  end;

  Content := Content + '}' + LineEnding;
  Result := WriteToFile(FileName, Content);
end;

class function TReportGenerator.GenerateUserReports(UserName: String): Boolean;
begin
  Result := GenerateInboxReport(UserName) and
            GenerateTrashReport(UserName) and
            GenerateScheduledReport(UserName) and
            GenerateContactsReport(UserName);
end;

class function TReportGenerator.GenerateInboxReport(UserName: String): Boolean;
var
  DirName, FileName, Content: String;
  User: PUser;
  Inbox: TEmailList;
  Email: PEmail;
  NodeCount: Integer;
begin
  Result := False;

  User := SystemCore.GetCurrentUser;
  if User = nil then Exit;

  DirName := UserName + '-Reportes';
  if not CreateDirectory(DirName) then Exit;

  FileName := DirName + '/reporte_correos_recibidos.dot';
  Content := 'digraph ReporteCorreosRecibidos {' + LineEnding +
             '    label="Reporte de Correos Recibidos";' + LineEnding +
             '    labelloc="t";' + LineEnding +
             '    fontsize=16;' + LineEnding +
             '    node [shape=box, style=filled];' + LineEnding +
             LineEnding;

  Inbox := SystemCore.GetUserInbox(User^.Email);
  if Inbox <> nil then
  begin
    Email := Inbox.GetFirst;
    NodeCount := 0;

    while Email <> nil do
    begin
      var fillcolor: String;
      if Email^.Estado = 'NL' then
        fillcolor := 'yellow'
      else
        fillcolor := 'lightgray';

      Content := Content + Format('    email%d [label="ID: %d\nRemitente: %s\nEstado: %s\nAsunto: %s\nFecha: %s", fillcolor=%s];' + LineEnding,
                                 [NodeCount, Email^.Id, EscapeGraphvizString(Email^.Remitente),
                                  Email^.Estado, EscapeGraphvizString(Email^.Asunto),
                                  FormatDateTime('dd/mm/yyyy hh:nn', Email^.Fecha), fillcolor]);

      if Email^.Next <> nil then
        Content := Content + Format('    email%d -> email%d;' + LineEnding, [NodeCount, NodeCount + 1]);

      Email := Email^.Next;
      Inc(NodeCount);
    end;
  end;

  if NodeCount = 0 then
    Content := Content + '    empty [label="No hay correos recibidos", fillcolor=lightgray];' + LineEnding;

  Content := Content + '}' + LineEnding;
  Result := WriteToFile(FileName, Content);
end;

class function TReportGenerator.GenerateTrashReport(UserName: String): Boolean;
var
  DirName, FileName, Content: String;
  User: PUser;
  Trash: TEmailStack;
  Email: PEmail;
  NodeCount: Integer;
begin
  Result := False;

  User := SystemCore.GetCurrentUser;
  if User = nil then Exit;

  DirName := UserName + '-Reportes';
  if not CreateDirectory(DirName) then Exit;

  FileName := DirName + '/reporte_papelera.dot';
  Content := 'digraph ReportePapelera {' + LineEnding +
             '    label="Reporte de Papelera";' + LineEnding +
             '    labelloc="t";' + LineEnding +
             '    fontsize=16;' + LineEnding +
             '    node [shape=box, style=filled, fillcolor=pink];' + LineEnding +
             '    rankdir=TB;' + LineEnding +
             LineEnding;

  Trash := SystemCore.GetUserTrash(User^.Email);
  if Trash <> nil then
  begin
    Email := Trash.GetTop;
    NodeCount := 0;

    while Email <> nil do
    begin
      Content := Content + Format('    trash%d [label="ID: %d\nRemitente: %s\nAsunto: %s\nFecha: %s"];' + LineEnding,
                                 [NodeCount, Email^.Id, EscapeGraphvizString(Email^.Remitente),
                                  EscapeGraphvizString(Email^.Asunto),
                                  FormatDateTime('dd/mm/yyyy hh:nn', Email^.Fecha)]);

      if Email^.Next <> nil then
        Content := Content + Format('    trash%d -> trash%d;' + LineEnding, [NodeCount, NodeCount + 1]);

      Email := Email^.Next;
      Inc(NodeCount);
    end;
  end;

  if NodeCount = 0 then
    Content := Content + '    empty [label="Papelera vacía", fillcolor=lightgray];' + LineEnding;

  Content := Content + '}' + LineEnding;
  Result := WriteToFile(FileName, Content);
end;

class function TReportGenerator.GenerateScheduledReport(UserName: String): Boolean;
var
  DirName, FileName, Content: String;
  Queue: TEmailQueue;
  Email: PEmail;
  NodeCount: Integer;
begin
  Result := False;

  DirName := UserName + '-Reportes';
  if not CreateDirectory(DirName) then Exit;

  FileName := DirName + '/reporte_correos_programados.dot';
  Content := 'digraph ReporteCorreosProgramados {' + LineEnding +
             '    label="Reporte de Correos Programados";' + LineEnding +
             '    labelloc="t";' + LineEnding +
             '    fontsize=16;' + LineEnding +
             '    node [shape=box, style=filled, fillcolor=lightcyan];' + LineEnding +
             LineEnding;

  Queue := SystemCore.GetScheduledEmailsQueue;
  if Queue <> nil then
  begin
    Email := Queue.GetFirst;
    NodeCount := 0;

    while Email <> nil do
    begin
      Content := Content + Format('    scheduled%d [label="ID: %d\nDestinatario: %s\nAsunto: %s\nFecha Programada: %s"];' + LineEnding,
                                 [NodeCount, Email^.Id, EscapeGraphvizString(Email^.Destinatario),
                                  EscapeGraphvizString(Email^.Asunto),
                                  FormatDateTime('dd/mm/yyyy hh:nn', Email^.Fecha)]);

      if Email^.Next <> nil then
        Content := Content + Format('    scheduled%d -> scheduled%d;' + LineEnding, [NodeCount, NodeCount + 1]);

      Email := Email^.Next;
      Inc(NodeCount);
    end;
  end;

  if NodeCount = 0 then
    Content := Content + '    empty [label="No hay correos programados", fillcolor=lightgray];' + LineEnding;

  Content := Content + '}' + LineEnding;
  Result := WriteToFile(FileName, Content);
end;

class function TReportGenerator.GenerateContactsReport(UserName: String): Boolean;
var
  DirName, FileName, Content: String;
  User: PUser;
  Contacts: TContactList;
  Contact: PContact;
  NodeCount, i: Integer;
begin
  Result := False;

  User := SystemCore.GetCurrentUser;
  if User = nil then Exit;

  DirName := UserName + '-Reportes';
  if not CreateDirectory(DirName) then Exit;

  FileName := DirName + '/reporte_contactos.dot';
  Content := 'digraph ReporteContactos {' + LineEnding +
             '    label="Reporte de Contactos (Lista Circular)";' + LineEnding +
             '    labelloc="t";' + LineEnding +
             '    fontsize=16;' + LineEnding +
             '    node [shape=ellipse, style=filled, fillcolor=lightgreen];' + LineEnding +
             LineEnding;

  Contacts := SystemCore.GetUserContacts(User^.Email);
  if (Contacts <> nil) and (Contacts.GetCount > 0) then
  begin
    Contact := Contacts.GetFirst;
    NodeCount := 0;

    // Agregar todos los nodos
    for i := 0 to Contacts.GetCount - 1 do
    begin
      Content := Content + Format('    contact%d [label="ID: %d\nNombre: %s\nUsuario: %s\nEmail: %s\nTeléfono: %s"];' + LineEnding,
                                 [NodeCount, Contact^.Id, EscapeGraphvizString(Contact^.Nombre),
                                  EscapeGraphvizString(Contact^.Usuario), EscapeGraphvizString(Contact^.Email),
                                  EscapeGraphvizString(Contact^.Telefono)]);
      Contact := Contacts.GetNext(Contact);
      Inc(NodeCount);
    end;

    Content := Content + LineEnding;

    // Agregar conexiones circulares
    for i := 0 to Contacts.GetCount - 1 do
    begin
      if i = Contacts.GetCount - 1 then
        Content := Content + Format('    contact%d -> contact0;' + LineEnding, [i])
      else
        Content := Content + Format('    contact%d -> contact%d;' + LineEnding, [i, i + 1]);
    end;
  end
  else
  begin
    Content := Content + '    empty [label="No hay contactos", fillcolor=lightgray];' + LineEnding;
  end;

  Content := Content + '}' + LineEnding;
  Result := WriteToFile(FileName, Content);
end;

class function TReportGenerator.GenerateCommunitiesReport: Boolean;
var
  DirName, FileName, Content: String;
  Community: PCommunity;
  UserCommunity: PUserCommunity;
  CommunityCount, UserCount: Integer;
begin
  Result := False;

  DirName := 'Root-Reportes';
  if not CreateDirectory(DirName) then Exit;

  FileName := DirName + '/reporte_comunidades.dot';
  Content := 'digraph ReporteComunidades {' + LineEnding +
             '    label="Reporte de Comunidades";' + LineEnding +
             '    labelloc="t";' + LineEnding +
             '    fontsize=16;' + LineEnding +
             '    node [style=filled];' + LineEnding +
             '    rankdir=TB;' + LineEnding +
             LineEnding;

  Community := SystemCore.GetCommunities.GetFirst;
  CommunityCount := 0;

  while Community <> nil do
  begin
    Content := Content + Format('    community%d [label="Comunidad: %s", shape=box, fillcolor=lightblue];' + LineEnding,
                               [CommunityCount, EscapeGraphvizString(Community^.Nombre)]);

    UserCommunity := Community^.Users;
    UserCount := 0;

    while UserCommunity <> nil do
    begin
      Content := Content + Format('    user%d_%d [label="%s", shape=ellipse, fillcolor=lightyellow];' + LineEnding,
                                 [CommunityCount, UserCount, EscapeGraphvizString(UserCommunity^.Email)]);
      Content := Content + Format('    community%d -> user%d_%d;' + LineEnding,
                                 [CommunityCount, CommunityCount, UserCount]);

      UserCommunity := UserCommunity^.Next;
      Inc(UserCount);
    end;

    if UserCount = 0 then
    begin
      Content := Content + Format('    empty%d [label="Sin usuarios", shape=ellipse, fillcolor=lightgray];' + LineEnding,
                                 [CommunityCount]);
      Content := Content + Format('    community%d -> empty%d;' + LineEnding,
                                 [CommunityCount, CommunityCount]);
    end;

    Community := Community^.Next;
    Inc(CommunityCount);
  end;

  if CommunityCount = 0 then
    Content := Content + '    empty [label="No hay comunidades", fillcolor=lightgray];' + LineEnding;

  Content := Content + '}' + LineEnding;
  Result := WriteToFile(FileName, Content);
end;

end.

