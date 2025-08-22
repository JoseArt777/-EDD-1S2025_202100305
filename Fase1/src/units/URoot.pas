unit URoot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStructures, ULogin, UReports;

type
  TRootManager = class
  private
    function EnsureReportsDirectory: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Carga masiva
    function LoadMassiveUsers(fileName: String): Boolean;
    
    // Reportes
    function GenerateUsersReport: Boolean;
    function GenerateRelationsReport: Boolean;
    
    // Gestión de comunidades
    function CreateCommunity(nombre: String): Integer;
    function AddUserToCommunity(communityId: Integer; userEmail: String): Boolean;
    function GenerateCommunitiesReport: Boolean;
    
    // Estadísticas del sistema
    function GetSystemStats: String;
    procedure ShowSystemInfo;
  end;

var
  RootManager: TRootManager;

implementation

constructor TRootManager.Create;
begin
  inherited Create;
  EnsureReportsDirectory;
end;

destructor TRootManager.Destroy;
begin
  inherited Destroy;
end;

function TRootManager.EnsureReportsDirectory: Boolean;
begin
  Result := True;
  try
    if not DirectoryExists('reports') then
      CreateDir('reports');
    if not DirectoryExists('reports/Root-Reportes') then
      CreateDir('reports/Root-Reportes');
  except
    Result := False;
  end;
end;

function TRootManager.LoadMassiveUsers(fileName: String): Boolean;
begin
  Result := LoginManager.LoadUsersFromJSON(fileName);
  if Result then
  begin
    WriteLn('=== CARGA MASIVA COMPLETADA ===');
    WriteLn('Usuarios cargados exitosamente');
    WriteLn('Total de usuarios en el sistema: ', UserList.GetUserCount);
  end
  else
  begin
    WriteLn('Error: No se pudieron cargar los usuarios');
  end;
end;

function TRootManager.GenerateUsersReport: Boolean;
var
  reportFileName: String;
  reportFile: TextFile;
  current: PUser;
  userCount: Integer;
begin
  Result := False;
  reportFileName := 'reports/Root-Reportes/reporte_usuarios.dot';
  
  try
    AssignFile(reportFile, reportFileName);
    Rewrite(reportFile);
    
    // Encabezado del archivo DOT para Graphviz
    WriteLn(reportFile, 'digraph ReporteUsuarios {');
    WriteLn(reportFile, '  rankdir=LR;');
    WriteLn(reportFile, '  node [shape=box, style=filled, fillcolor=lightblue];');
    WriteLn(reportFile, '  label="Reporte de Usuarios Registrados";');
    WriteLn(reportFile, '  labelloc="t";');
    WriteLn(reportFile, '');
    
    // Nodo de título
    WriteLn(reportFile, '  title [label="Lista Enlazada\\nUsuarios", fillcolor=orange];');
    
    current := UserList.head;
    userCount := 0;
    
    if current <> nil then
    begin
      // Primer nodo
      WriteLn(reportFile, '  user', current^.id, ' [label="ID: ', current^.id, '\\nNombre: ', current^.nombre, '\\nUsuario: ', current^.usuario, '\\nEmail: ', current^.email, '\\nTeléfono: ', current^.telefono, '"];');
      WriteLn(reportFile, '  title -> user', current^.id, ';');
      Inc(userCount);
      
      // Nodos restantes y conexiones
      while current^.next <> nil do
      begin
        WriteLn(reportFile, '  user', current^.next^.id, ' [label="ID: ', current^.next^.id, '\\nNombre: ', current^.next^.nombre, '\\nUsuario: ', current^.next^.usuario, '\\nEmail: ', current^.next^.email, '\\nTeléfono: ', current^.next^.telefono, '"];');
        WriteLn(reportFile, '  user', current^.id, ' -> user', current^.next^.id, ';');
        current := current^.next;
        Inc(userCount);
      end;
    end
    else
    begin
      WriteLn(reportFile, '  empty [label="No hay usuarios registrados", fillcolor=red];');
      WriteLn(reportFile, '  title -> empty;');
    end;
    
    WriteLn(reportFile, '}');
    CloseFile(reportFile);
    
    // Generar imagen PNG usando Graphviz
    if FileExists('/usr/bin/dot') or FileExists('/usr/local/bin/dot') then
    begin
      ExecuteProcess('dot', ['-Tpng', reportFileName, '-o', 'reports/Root-Reportes/reporte_usuarios.png']);
      WriteLn('Reporte de usuarios generado: reports/Root-Reportes/reporte_usuarios.png');
    end
    else
    begin
      WriteLn('Archivo DOT generado: ', reportFileName);
      WriteLn('Para generar la imagen, ejecute: dot -Tpng ', reportFileName, ' -o reports/Root-Reportes/reporte_usuarios.png');
    end;
    
    Result := True;
    WriteLn('Total de usuarios reportados: ', userCount);
    
  except
    on E: Exception do
    begin
      WriteLn('Error al generar reporte de usuarios: ', E.Message);
      Result := False;
    end;
  end;
end;

function TRootManager.GenerateRelationsReport: Boolean;
var
  reportFileName: String;
  reportFile: TextFile;
  current: PMatrixNode;
  i, j: Integer;
begin
  Result := False;
  reportFileName := 'reports/Root-Reportes/reporte_relaciones.dot';
  
  try
    AssignFile(reportFile, reportFileName);
    Rewrite(reportFile);
    
    // Encabezado del archivo DOT
    WriteLn(reportFile, 'digraph ReporteRelaciones {');
    WriteLn(reportFile, '  rankdir=TB;');
    WriteLn(reportFile, '  node [shape=box, style=filled];');
    WriteLn(reportFile, '  label="Matriz Dispersa - Relaciones Emisor vs Destinatario";');
    WriteLn(reportFile, '  labelloc="t";');
    WriteLn(reportFile, '');
    
    // Nodo de título
    WriteLn(reportFile, '  title [label="Matriz Dispersa\\nRelaciones", fillcolor=yellow];');
    
    current := RelationMatrix.head;
    if current <> nil then
    begin
      // Crear nodos para usuarios únicos
      for i := 0 to RelationMatrix.userEmails.Count - 1 do
      begin
        WriteLn(reportFile, '  user', i, ' [label="', RelationMatrix.userEmails[i], '", fillcolor=lightgreen];');
      end;
      
      // Crear nodos para las relaciones
      while current <> nil do
      begin
        WriteLn(reportFile, '  rel_', current^.row, '_', current^.col, ' [label="', current^.value, '", fillcolor=orange];');
        WriteLn(reportFile, '  user', current^.row, ' -> rel_', current^.row, '_', current^.col, ';');
        WriteLn(reportFile, '  rel_', current^.row, '_', current^.col, ' -> user', current^.col, ';');
        current := current^.nextRow;
      end;
    end
    else
    begin
      WriteLn(reportFile, '  empty [label="No hay relaciones registradas", fillcolor=red];');
      WriteLn(reportFile, '  title -> empty;');
    end;
    
    WriteLn(reportFile, '}');
    CloseFile(reportFile);
    
    // Generar imagen PNG
    if FileExists('/usr/bin/dot') or FileExists('/usr/local/bin/dot') then
    begin
      ExecuteProcess('dot', ['-Tpng', reportFileName, '-o', 'reports/Root-Reportes/reporte_relaciones.png']);
      WriteLn('Reporte de relaciones generado: reports/Root-Reportes/reporte_relaciones.png');
    end
    else
    begin
      WriteLn('Archivo DOT generado: ', reportFileName);
      WriteLn('Para generar la imagen, ejecute: dot -Tpng ', reportFileName, ' -o reports/Root-Reportes/reporte_relaciones.png');
    end;
    
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al generar reporte de relaciones: ', E.Message);
      Result := False;
    end;
  end;
end;

function TRootManager.CreateCommunity(nombre: String): Integer;
var
  communityId: Integer;
  current: PCommunity;
  maxId: Integer;
begin
  // Generar ID único para la comunidad
  maxId := 0;
  current := CommunityList.head;
  while current <> nil do
  begin
    if current^.id > maxId then
      maxId := current^.id;
    current := current^.next;
  end;
  
  communityId := maxId + 1;
  CommunityList.CreateCommunity(communityId, nombre);
  
  WriteLn('Comunidad "', nombre, '" creada con ID: ', communityId);
  Result := communityId;
end;

function TRootManager.AddUserToCommunity(communityId: Integer; userEmail: String): Boolean;
var
  user: PUser;
  community: PCommunity;
begin
  Result := False;
  
  // Verificar que el usuario existe
  user := UserList.FindUserByEmail(userEmail);
  if user = nil then
  begin
    WriteLn('Error: Usuario con email ', userEmail, ' no encontrado');
    Exit;
  end;
  
  // Verificar que la comunidad existe
  community := CommunityList.FindCommunity(communityId);
  if community = nil then
  begin
    WriteLn('Error: Comunidad con ID ', communityId, ' no encontrada');
    Exit;
  end;
  
  // Agregar usuario a la comunidad
  CommunityList.AddMemberToCommunity(communityId, userEmail);
  WriteLn('Usuario ', userEmail, ' agregado a la comunidad "', community^.nombre, '"');
  Result := True;
end;

function TRootManager.GenerateCommunitiesReport: Boolean;
var
  reportFileName: String;
  reportFile: TextFile;
  currentCommunity: PCommunity;
  currentMember: PMember;
  memberCount: Integer;
begin
  Result := False;
  reportFileName := 'reports/Root-Reportes/reporte_comunidades.dot';
  
  try
    AssignFile(reportFile, reportFileName);
    Rewrite(reportFile);
    
    WriteLn(reportFile, 'digraph ReporteComunidades {');
    WriteLn(reportFile, '  rankdir=TB;');
    WriteLn(reportFile, '  node [shape=box, style=filled];');
    WriteLn(reportFile, '  label="Reporte de Comunidades";');
    WriteLn(reportFile, '  labelloc="t";');
    WriteLn(reportFile, '');
    
    currentCommunity := CommunityList.head;
    if currentCommunity <> nil then
    begin
      while currentCommunity <> nil do
      begin
        // Nodo de la comunidad
        WriteLn(reportFile, '  community', currentCommunity^.id, ' [label="Comunidad\\n', currentCommunity^.nombre, '", fillcolor=lightblue];');
        
        // Nodos de los miembros
        currentMember := currentCommunity^.members;
        memberCount := 0;
        while currentMember <> nil do
        begin
          WriteLn(reportFile, '  member', currentCommunity^.id, '_', memberCount, ' [label="', currentMember^.email, '", fillcolor=lightgreen];');
          WriteLn(reportFile, '  community', currentCommunity^.id, ' -> member', currentCommunity^.id, '_', memberCount, ';');
          currentMember := currentMember^.next;
          Inc(memberCount);
        end;
        
        currentCommunity := currentCommunity^.next;
      end;
    end
    else
    begin
      WriteLn(reportFile, '  empty [label="No hay comunidades creadas", fillcolor=red];');
    end;
    
    WriteLn(reportFile, '}');
    CloseFile(reportFile);
    
    // Generar imagen
    if FileExists('/usr/bin/dot') or FileExists('/usr/local/bin/dot') then
    begin
      ExecuteProcess('dot', ['-Tpng', reportFileName, '-o', 'reports/Root-Reportes/reporte_comunidades.png']);
      WriteLn('Reporte de comunidades generado: reports/Root-Reportes/reporte_comunidades.png');
    end
    else
    begin
      WriteLn('Archivo DOT generado: ', reportFileName);
      WriteLn('Para generar la imagen, ejecute: dot -Tpng ', reportFileName, ' -o reports/Root-Reportes/reporte_comunidades.png');
    end;
    
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al generar reporte de comunidades: ', E.Message);
      Result := False;
    end;
  end;
end;

function TRootManager.GetSystemStats: String;
var
  stats: String;
begin
  stats := '=== ESTADÍSTICAS DEL SISTEMA ===' + LineEnding;
  stats := stats + 'Total de usuarios: ' + IntToStr(UserList.GetUserCount) + LineEnding;
  stats := stats + 'Total de correos en bandeja: ' + IntToStr(EmailList.GetUnreadCount) + ' no leídos' + LineEnding;
  stats := stats + 'Total de contactos: ' + IntToStr(ContactList.GetContactCount) + LineEnding;
  stats := stats + 'Correos programados: ' + BoolToStr(not EmailQueue.IsEmpty, 'Sí hay', 'No hay') + LineEnding;
  stats := stats + 'Correos en papelera: ' + BoolToStr(not EmailStack.IsEmpty, 'Sí hay', 'No hay') + LineEnding;
  Result := stats;
end;

procedure TRootManager.ShowSystemInfo;
begin
  WriteLn(GetSystemStats);
end;

// Inicialización global
initialization
  RootManager := TRootManager.Create;

finalization
  RootManager.Free;

end.
