unit UUser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStructures, UReports;

type
  TUserManager = class
  private
    currentUser: PUser;
    userReportsDir: String;
    function EnsureUserReportsDirectory: Boolean;
    function GenerateEmailId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Gestión de sesión
    procedure SetCurrentUser(user: PUser);
    function GetCurrentUser: PUser;
    function GetCurrentUserEmail: String;
    
    // Gestión de correos
    function SendEmail(destinatario, asunto, mensaje: String): Boolean;
    function ScheduleEmail(destinatario, asunto, mensaje, fechaEnvio: String): Boolean;
    function ProcessScheduledEmails: Integer;
    procedure DeleteEmail(emailId: Integer);
    procedure MarkEmailAsRead(emailId: Integer);
    procedure SortEmailsBySubject;
    
    // Gestión de contactos
    function AddContact(email: String): Boolean;
    function RemoveContact(email: String): Boolean;
    function IsContact(email: String): Boolean;
    procedure NavigateContacts(direction: Integer); // 1=siguiente, -1=anterior
    function GetCurrentContact: PContact;
    
    // Gestión de papelera
    function SearchInTrash(keyword: String): PDeletedEmail;
    function DeleteFromTrash(emailId: Integer): Boolean;
    
    // Actualización de perfil
    function UpdateProfile(newUsername, newPhone: String): Boolean;
    
    // Reportes de usuario
    function GenerateInboxReport: Boolean;
    function GenerateTrashReport: Boolean;
    function GenerateScheduledEmailsReport: Boolean;
    function GenerateContactsReport: Boolean;
    function GenerateAllUserReports: Boolean;
    
    // Estadísticas
    function GetUserStats: String;
    procedure ShowUserInfo;
  end;

var
  UserManager: TUserManager;

implementation

constructor TUserManager.Create;
begin
  inherited Create;
  currentUser := nil;
  userReportsDir := '';
end;

destructor TUserManager.Destroy;
begin
  inherited Destroy;
end;

procedure TUserManager.SetCurrentUser(user: PUser);
begin
  currentUser := user;
  if user <> nil then
  begin
    userReportsDir := 'reports/' + user^.usuario + '-Reportes';
    EnsureUserReportsDirectory;
  end;
end;

function TUserManager.GetCurrentUser: PUser;
begin
  Result := currentUser;
end;

function TUserManager.GetCurrentUserEmail: String;
begin
  if currentUser <> nil then
    Result := currentUser^.email
  else
    Result := '';
end;

function TUserManager.EnsureUserReportsDirectory: Boolean;
begin
  Result := True;
  try
    if not DirectoryExists('reports') then
      CreateDir('reports');
    if not DirectoryExists(userReportsDir) then
      CreateDir(userReportsDir);
  except
    Result := False;
  end;
end;

function TUserManager.GenerateEmailId: Integer;
var
  maxId: Integer;
  current: PEmail;
begin
  maxId := 0;
  current := EmailList.head;
  
  while current <> nil do
  begin
    if current^.id > maxId then
      maxId := current^.id;
    current := current^.next;
  end;
  
  Result := maxId + 1;
end;

function TUserManager.SendEmail(destinatario, asunto, mensaje: String): Boolean;
var
  emailId: Integer;
  currentDateTime: String;
begin
  Result := False;
  
  if currentUser = nil then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;
  
  // Verificar que el destinatario es un contacto
  if not IsContact(destinatario) then
  begin
    WriteLn('Error: ', destinatario, ' no está en tu lista de contactos');
    Exit;
  end;
  
  try
    emailId := GenerateEmailId;
    DateTimeToString(currentDateTime, 'dd/mm/yy hh:nn', Now);
    
    // Agregar correo a la bandeja del destinatario
    EmailList.AddEmail(emailId, currentUser^.email, destinatario, asunto, mensaje, currentDateTime);
    
    // Registrar relación en la matriz dispersa
    RelationMatrix.AddRelation(currentUser^.email, destinatario);
    
    WriteLn('Correo enviado exitosamente a ', destinatario);
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al enviar correo: ', E.Message);
      Result := False;
    end;
  end;
end;

function TUserManager.ScheduleEmail(destinatario, asunto, mensaje, fechaEnvio: String): Boolean;
var
  emailId: Integer;
begin
  Result := False;
  
  if currentUser = nil then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;
  
  // Verificar que el destinatario es un contacto
  if not IsContact(destinatario) then
  begin
    WriteLn('Error: ', destinatario, ' no está en tu lista de contactos');
    Exit;
  end;
  
  try
    emailId := GenerateEmailId;
    EmailQueue.Enqueue(emailId, currentUser^.email, destinatario, asunto, mensaje, fechaEnvio);
    
    WriteLn('Correo programado para ', fechaEnvio, ' a ', destinatario);
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al programar correo: ', E.Message);
      Result := False;
    end;
  end;
end;

function TUserManager.ProcessScheduledEmails: Integer;
var
  processedCount: Integer;
  scheduledEmail: PScheduledEmail;
  currentDateTime: String;
begin
  processedCount := 0;
  DateTimeToString(currentDateTime, 'dd/mm/yy hh:nn', Now);
  
  // Procesar todos los correos en la cola (simulación de envío automático)
  while not EmailQueue.IsEmpty do
  begin
    scheduledEmail := EmailQueue.Dequeue;
    if scheduledEmail <> nil then
    begin
      // Agregar correo a la bandeja del destinatario
      EmailList.AddEmail(
        scheduledEmail^.id,
        scheduledEmail^.remitente,
        scheduledEmail^.destinatario,
        scheduledEmail^.asunto,
        scheduledEmail^.mensaje,
        currentDateTime,
        True,
        scheduledEmail^.fechaEnvio
      );
      
      // Registrar relación
      RelationMatrix.AddRelation(scheduledEmail^.remitente, scheduledEmail^.destinatario);
      
      WriteLn('Correo enviado automáticamente: ', scheduledEmail^.asunto, ' a ', scheduledEmail^.destinatario);
      
      Dispose(scheduledEmail);
      Inc(processedCount);
    end;
  end;
  
  Result := processedCount;
  WriteLn('Total de correos enviados automáticamente: ', processedCount);
end;

procedure TUserManager.DeleteEmail(emailId: Integer);
begin
  EmailList.DeleteEmail(emailId);
  WriteLn('Correo movido a la papelera');
end;

procedure TUserManager.MarkEmailAsRead(emailId: Integer);
begin
  EmailList.MarkAsRead(emailId);
  WriteLn('Correo marcado como leído');
end;

procedure TUserManager.SortEmailsBySubject;
begin
  EmailList.SortBySubject;
  WriteLn('Correos ordenados alfabéticamente por asunto');
end;

function TUserManager.AddContact(email: String): Boolean;
var
  user: PUser;
  contactId: Integer;
begin
  Result := False;
  
  if currentUser = nil then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;
  
  // Verificar que el usuario existe en el sistema
  user := UserList.FindUserByEmail(email);
  if user = nil then
  begin
    WriteLn('Error: El usuario ', email, ' no está registrado en el sistema');
    Exit;
  end;
  
  // Verificar que no es el mismo usuario
  if user^.email = currentUser^.email then
  begin
    WriteLn('Error: No puedes agregarte a ti mismo como contacto');
    Exit;
  end;
  
  // Verificar que no está ya en contactos
  if IsContact(email) then
  begin
    WriteLn('Error: ', email, ' ya está en tu lista de contactos');
    Exit;
  end;
  
  contactId := ContactList.GetContactCount + 1;
  ContactList.AddContact(contactId, user^.nombre, user^.usuario, user^.email, user^.telefono);
  
  WriteLn('Contacto ', email, ' agregado exitosamente');
  Result := True;
end;

function TUserManager.RemoveContact(email: String): Boolean;
begin
  Result := False;
  // Implementación simplificada - en una implementación completa necesitaríamos método de eliminación
  WriteLn('Funcionalidad de eliminar contacto no implementada en esta versión');
end;

function TUserManager.IsContact(email: String): Boolean;
begin
  Result := ContactList.FindContactByEmail(email) <> nil;
end;

procedure TUserManager.NavigateContacts(direction: Integer);
begin
  if direction > 0 then
    ContactList.GetNextContact
  else
    ContactList.GetPrevContact;
end;

function TUserManager.GetCurrentContact: PContact;
begin
  Result := ContactList.GetCurrentContact;
end;

function TUserManager.SearchInTrash(keyword: String): PDeletedEmail;
begin
  Result := EmailStack.SearchBySubject(keyword);
  if Result <> nil then
    WriteLn('Correo encontrado en papelera: ', Result^.asunto)
  else
    WriteLn('No se encontraron correos con la palabra clave: ', keyword);
end;

function TUserManager.DeleteFromTrash(emailId: Integer): Boolean;
var
  deleted: PDeletedEmail;
begin
  Result := False;
  deleted := EmailStack.Pop;
  if deleted <> nil then
  begin
    if deleted^.id = emailId then
    begin
      WriteLn('Correo eliminado permanentemente de la papelera');
      Dispose(deleted);
      Result := True;
    end
    else
    begin
      // Volver a poner en la pila si no es el correcto
      EmailStack.Push(deleted^.id, deleted^.remitente, deleted^.asunto, deleted^.mensaje, deleted^.fecha);
      Dispose(deleted);
      WriteLn('Correo no encontrado en la papelera');
    end;
  end
  else
    WriteLn('La papelera está vacía');
end;

function TUserManager.UpdateProfile(newUsername, newPhone: String): Boolean;
begin
  Result := False;
  
  if currentUser = nil then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;
  
  try
    // Verificar que el nuevo username no esté en uso
    if (newUsername <> currentUser^.usuario) and (UserList.FindUserByUsername(newUsername) <> nil) then
    begin
      WriteLn('Error: El nombre de usuario ', newUsername, ' ya está en uso');
      Exit;
    end;
    
    // Actualizar datos
    currentUser^.usuario := newUsername;
    currentUser^.telefono := newPhone;
    
    WriteLn('Perfil actualizado exitosamente');
    WriteLn('Nuevo usuario: ', newUsername);
    WriteLn('Nuevo teléfono: ', newPhone);
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al actualizar perfil: ', E.Message);
      Result := False;
    end;
  end;
end;

function TUserManager.GenerateInboxReport: Boolean;
var
  reportFileName: String;
  reportFile: TextFile;
  current: PEmail;
  emailCount: Integer;
begin
  Result := False;
  if currentUser = nil then Exit;
  
  reportFileName := userReportsDir + '/reporte_correos_recibidos.dot';
  
  try
    AssignFile(reportFile, reportFileName);
    Rewrite(reportFile);
    
    WriteLn(reportFile, 'digraph ReporteCorreosRecibidos {');
    WriteLn(reportFile, '  rankdir=LR;');
    WriteLn(reportFile, '  node [shape=box, style=filled];');
    WriteLn(reportFile, '  label="Correos Recibidos - ', currentUser^.usuario, '";');
    WriteLn(reportFile, '  labelloc="t";');
    WriteLn(reportFile, '');
    
    WriteLn(reportFile, '  title [label="Lista Doblemente Enlazada\\nCorreos Recibidos", fillcolor=lightblue];');
    
    current := EmailList.head;
    emailCount := 0;
    
    if current <> nil then
    begin
      // Filtrar correos del usuario actual
      while current <> nil do
      begin
        if current^.destinatario = currentUser^.email then
        begin
          WriteLn(reportFile, '  email', current^.id, ' [label="ID: ', current^.id, '\\nDe: ', current^.remitente, '\\nAsunto: ', current^.asunto, '\\nEstado: ', current^.estado, '\\nFecha: ', current^.fecha, '", fillcolor=');
          if current^.estado = 'NL' then
            WriteLn(reportFile, 'yellow];')
          else
            WriteLn(reportFile, 'lightgreen];');
          
          if emailCount = 0 then
            WriteLn(reportFile, '  title -> email', current^.id, ';')
          else
            WriteLn(reportFile, '  email', current^.prev^.id, ' -> email', current^.id, ';');
          
          Inc(emailCount);
        end;
        current := current^.next;
      end;
    end;
    
    if emailCount = 0 then
    begin
      WriteLn(reportFile, '  empty [label="No hay correos recibidos", fillcolor=red];');
      WriteLn(reportFile, '  title -> empty;');
    end;
    
    WriteLn(reportFile, '}');
    CloseFile(reportFile);
    
    // Generar imagen
    if FileExists('/usr/bin/dot') or FileExists('/usr/local/bin/dot') then
    begin
      ExecuteProcess('dot', ['-Tpng', reportFileName, '-o', userReportsDir + '/reporte_correos_recibidos.png']);
    end;
    
    WriteLn('Reporte de correos recibidos generado');
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al generar reporte de correos: ', E.Message);
      Result := False;
    end;
  end;
end;

function TUserManager.GenerateTrashReport: Boolean;
var
  reportFileName: String;
  reportFile: TextFile;
  current: PDeletedEmail;
begin
  Result := False;
  if currentUser = nil then Exit;
  
  reportFileName := userReportsDir + '/reporte_papelera.dot';
  
  try
    AssignFile(reportFile, reportFileName);
    Rewrite(reportFile);
    
    WriteLn(reportFile, 'digraph ReportePapelera {');
    WriteLn(reportFile, '  rankdir=TB;');
    WriteLn(reportFile, '  node [shape=box, style=filled, fillcolor=pink];');
    WriteLn(reportFile, '  label="Papelera - ', currentUser^.usuario, '";');
    WriteLn(reportFile, '  labelloc="t";');
    WriteLn(reportFile, '');
    
    WriteLn(reportFile, '  title [label="Pila\\nCorreos Eliminados", fillcolor=red];');
    
    current := EmailStack.top;
    if current <> nil then
    begin
      while current <> nil do
      begin
        WriteLn(reportFile, '  deleted', current^.id, ' [label="ID: ', current^.id, '\\nDe: ', current^.remitente, '\\nAsunto: ', current^.asunto, '\\nFecha: ', current^.fecha, '"];');
        if current = EmailStack.top then
          WriteLn(reportFile, '  title -> deleted', current^.id, ';')
        else
          WriteLn(reportFile, '  deleted', current^.next^.id, ' -> deleted', current^.id, ';');
        current := current^.next;
      end;
    end
    else
    begin
      WriteLn(reportFile, '  empty [label="Papelera vacía", fillcolor=lightgray];');
      WriteLn(reportFile, '  title -> empty;');
    end;
    
    WriteLn(reportFile, '}');
    CloseFile(reportFile);
    
    if FileExists('/usr/bin/dot') or FileExists('/usr/local/bin/dot') then
    begin
      ExecuteProcess('dot', ['-Tpng', reportFileName, '-o', userReportsDir + '/reporte_papelera.png']);
    end;
    
    WriteLn('Reporte de papelera generado');
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al generar reporte de papelera: ', E.Message);
      Result := False;
    end;
  end;
end;

function TUserManager.GenerateScheduledEmailsReport: Boolean;
var
  reportFileName: String;
  reportFile: TextFile;
  current: PScheduledEmail;
begin
  Result := False;
  if currentUser = nil then Exit;
  
  reportFileName := userReportsDir + '/reporte_correos_programados.dot';
  
  try
    AssignFile(reportFile, reportFileName);
    Rewrite(reportFile);
    
    WriteLn(reportFile, 'digraph ReporteCorreosProgramados {');
    WriteLn(reportFile, '  rankdir=LR;');
    WriteLn(reportFile, '  node [shape=box, style=filled, fillcolor=lightyellow];');
    WriteLn(reportFile, '  label="Correos Programados - ', currentUser^.usuario, '";');
    WriteLn(reportFile, '  labelloc="t";');
    WriteLn(reportFile, '');
    
    WriteLn(reportFile, '  title [label="Cola\\nCorreos Programados", fillcolor=orange];');
    
    current := EmailQueue.front;
    if current <> nil then
    begin
      while current <> nil do
      begin
        if current^.remitente = currentUser^.email then
        begin
          WriteLn(reportFile, '  scheduled', current^.id, ' [label="ID: ', current^.id, '\\nPara: ', current^.destinatario, '\\nAsunto: ', current^.asunto, '\\nEnvío: ', current^.fechaEnvio, '"];');
          if current = EmailQueue.front then
            WriteLn(reportFile, '  title -> scheduled', current^.id, ';');
        end;
        current := current^.next;
      end;
    end
    else
    begin
      WriteLn(reportFile, '  empty [label="No hay correos programados", fillcolor=lightgray];');
      WriteLn(reportFile, '  title -> empty;');
    end;
    
    WriteLn(reportFile, '}');
    CloseFile(reportFile);
    
    if FileExists('/usr/bin/dot') or FileExists('/usr/local/bin/dot') then
    begin
      ExecuteProcess('dot', ['-Tpng', reportFileName, '-o', userReportsDir + '/reporte_correos_programados.png']);
    end;
    
    WriteLn('Reporte de correos programados generado');
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al generar reporte de correos programados: ', E.Message);
      Result := False;
    end;
  end;
end;

function TUserManager.GenerateContactsReport: Boolean;
var
  reportFileName: String;
  reportFile: TextFile;
  current: PContact;
  i: Integer;
begin
  Result := False;
  if currentUser = nil then Exit;
  
  reportFileName := userReportsDir + '/reporte_contactos.dot';
  
  try
    AssignFile(reportFile, reportFileName);
    Rewrite(reportFile);
    
    WriteLn(reportFile, 'digraph ReporteContactos {');
    WriteLn(reportFile, '  rankdir=LR;');
    WriteLn(reportFile, '  node [shape=box, style=filled, fillcolor=lightcyan];');
    WriteLn(reportFile, '  label="Contactos - ', currentUser^.usuario, '";');
    WriteLn(reportFile, '  labelloc="t";');
    WriteLn(reportFile, '');
    
    WriteLn(reportFile, '  title [label="Lista Circular\\nContactos", fillcolor=cyan];');
    
    if ContactList.GetContactCount > 0 then
    begin
      current := ContactList.head;
      for i := 0 to ContactList.GetContactCount - 1 do
      begin
        WriteLn(reportFile, '  contact', current^.id, ' [label="ID: ', current^.id, '\\nNombre: ', current^.nombre, '\\nUsuario: ', current^.usuario, '\\nEmail: ', current^.email, '\\nTeléfono: ', current^.telefono, '"];');
        if i = 0 then
          WriteLn(reportFile, '  title -> contact', current^.id, ';');
        if current^.next <> ContactList.head then
          WriteLn(reportFile, '  contact', current^.id, ' -> contact', current^.next^.id, ';')
        else
          WriteLn(reportFile, '  contact', current^.id, ' -> contact', ContactList.head^.id, ' [color=red];');
        current := current^.next;
      end;
    end
    else
    begin
      WriteLn(reportFile, '  empty [label="No hay contactos", fillcolor=lightgray];');
      WriteLn(reportFile, '  title -> empty;');
    end;
    
    WriteLn(reportFile, '}');
    CloseFile(reportFile);
    
    if FileExists('/usr/bin/dot') or FileExists('/usr/local/bin/dot') then
    begin
      ExecuteProcess('dot', ['-Tpng', reportFileName, '-o', userReportsDir + '/reporte_contactos.png']);
    end;
    
    WriteLn('Reporte de contactos generado');
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al generar reporte de contactos: ', E.Message);
      Result := False;
    end;
  end;
end;

function TUserManager.GenerateAllUserReports: Boolean;
var
  success: Boolean;
begin
  success := True;
  
  WriteLn('=== GENERANDO REPORTES DE USUARIO ===');
  
  success := success and GenerateInboxReport;
  success := success and GenerateTrashReport;
  success := success and GenerateScheduledEmailsReport;
  success := success and GenerateContactsReport;
  
  if success then
    WriteLn('Todos los reportes de usuario generados exitosamente')
  else
    WriteLn('Algunos reportes no se pudieron generar');
    
  Result := success;
end;

function TUserManager.GetUserStats: String;
var
  stats: String;
  unreadCount, contactCount: Integer;
  currentEmail: PEmail;
begin
  if currentUser = nil then
  begin
    Result := 'No hay usuario logueado';
    Exit;
  end;
  
  // Contar correos no leídos del usuario actual
  unreadCount := 0;
  currentEmail := EmailList.head;
  while currentEmail <> nil do
  begin
    if (currentEmail^.destinatario = currentUser^.email) and (currentEmail^.estado = 'NL') then
      Inc(unreadCount);
    currentEmail := currentEmail^.next;
  end;
  
  contactCount := ContactList.GetContactCount;
  
  stats := '=== ESTADÍSTICAS DE ' + currentUser^.usuario + ' ===' + LineEnding;
  stats := stats + 'Nombre: ' + currentUser^.nombre + LineEnding;
  stats := stats + 'Email: ' + currentUser^.email + LineEnding;
  stats := stats + 'Teléfono: ' + currentUser^.telefono + LineEnding;
  stats := stats + 'Correos no leídos: ' + IntToStr(unreadCount) + LineEnding;
  stats := stats + 'Total de contactos: ' + IntToStr(contactCount) + LineEnding;
  stats := stats + 'Correos en papelera: ' + BoolToStr(not EmailStack.IsEmpty, 'Sí hay', 'No hay') + LineEnding;
  
  Result := stats;
end;

procedure TUserManager.ShowUserInfo;
begin
  WriteLn(GetUserStats);
end;

// Inicialización global
initialization
  UserManager := TUserManager.Create;

finalization
  UserManager.Free;

end.
