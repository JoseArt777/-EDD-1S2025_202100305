unit EstructurasDatos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Tipos de punteros
  PUsuario = ^TUsuario;
  PCorreo = ^TCorreo;
  PContacto = ^TContacto;
  PComunidad = ^TComunidad;
  PUsuarioComunidad = ^TUsuarioComunidad;
  PMatrizDispersaFila = ^TMatrizDispersaFila;
  PMatrizDispersaColumna = ^TMatrizDispersaColumna;
  PMatrizDispersaNodo = ^TMatrizDispersaNodo;

  // Estructura Usuario (Lista Simple)
  TUsuario = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Password: String;
    Siguiente: PUsuario;
  end;

  // Estructura Correo (Lista Doblemente Enlazada para bandeja de entrada)
  TCorreo = record
    Id: Integer;
    Remitente: String;
    Destinatario: String;
    Estado: String; // 'NL' = No Leído, 'L' = Leído
    Programado: Boolean;
    Asunto: String;
    Fecha: String;
    Mensaje: String;
    FechaEnvio: String; // Para correos programados
    Anterior: PCorreo;
    Siguiente: PCorreo;
  end;

  // Estructura Contacto (Lista Circular)
  TContacto = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Siguiente: PContacto;
  end;

  // Estructura para Comunidades
  TUsuarioComunidad = record
    Email: String;
    Siguiente: PUsuarioComunidad;
  end;

  TComunidad = record
    Id: Integer;
    Nombre: String;
    UsuariosList: PUsuarioComunidad;
    Siguiente: PComunidad;
  end;

  // Matriz Dispersa para relaciones remitente-destinatario
  TMatrizDispersaNodo = record
    Fila: Integer;
    Columna: Integer;
    Cantidad: Integer;
    RemitenteEmail: String;
    DestinatarioEmail: String;
    Arriba: PMatrizDispersaNodo;
    Abajo: PMatrizDispersaNodo;
    Izquierda: PMatrizDispersaNodo;
    Derecha: PMatrizDispersaNodo;
  end;

  TMatrizDispersaFila = record
    Fila: Integer;
    Email: String;
    Primero: PMatrizDispersaNodo;
    Siguiente: PMatrizDispersaFila;
  end;

  TMatrizDispersaColumna = record
    Columna: Integer;
    Email: String;
    Primero: PMatrizDispersaNodo;
    Siguiente: PMatrizDispersaColumna;
  end;

  // Clase principal para manejar todas las estructuras
  TEDDMailSystem = class
  private
    // Listas principales
    FUsuarios: PUsuario;
    FComunidades: PComunidad;
    FMatrizFilas: PMatrizDispersaFila;
    FMatrizColumnas: PMatrizDispersaColumna;
    FUsuarioActual: PUsuario;

    // Funciones auxiliares para usuarios
    function BuscarUsuario(Email: String): PUsuario;
    function ValidarCredenciales(Email, Password: String): PUsuario;

    // Funciones auxiliares para correos
    function CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean = False): PCorreo;

    // Funciones auxiliares para contactos
    function BuscarContacto(Usuario: PUsuario; Email: String): PContacto;

    // Funciones auxiliares para matriz dispersa
    procedure ActualizarMatrizRelaciones(Remitente, Destinatario: String);
    function BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
    function BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;

  public
    constructor Create;
    destructor Destroy; override;

    // Funciones de usuario
    function RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
    function IniciarSesion(Email, Password: String): Boolean;
    procedure CerrarSesion;
    function GetUsuarioActual: PUsuario;
    function ListarComunidades: String;

    // Funciones para carga masiva (ROOT)
    procedure CargarUsuariosDesdeJSON(RutaArchivo: String);

    // Funciones de correo
    procedure EnviarCorreo(Destinatario, Asunto, Mensaje: String);
    procedure ProgramarCorreo(Destinatario, Asunto, Mensaje, FechaEnvio: String);
    procedure EliminarCorreo(Usuario: PUsuario; CorreoId: Integer);
    procedure MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer);
    function GetBandejaEntrada(Usuario: PUsuario): PCorreo;
    function GetPapelera(Usuario: PUsuario): PCorreo; // Pila
    function GetCorreosProgramados(Usuario: PUsuario): PCorreo; // Cola
    procedure ProcesarCorreosProgramados;

    // Funciones de contactos
    function AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
    function GetContactos(Usuario: PUsuario): PContacto;

    // Funciones de comunidades
    function CrearComunidad(Nombre: String): Boolean;
    function AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: String): Boolean;
    function GetComunidades: PComunidad;

    // Funciones de actualización de perfil
    procedure ActualizarPerfil(Usuario: PUsuario; NuevoNombre, NuevoUsuario, NuevoTelefono: String);

    // Funciones de reportes
    procedure GenerarReporteUsuarios(RutaCarpeta: String);
    procedure GenerarReporteRelaciones(RutaCarpeta: String);
    procedure GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReporteComunidades(RutaCarpeta: String);
  end;

implementation

uses
  fpjson, jsonparser, process;

constructor TEDDMailSystem.Create;
begin
  inherited Create;
  FUsuarios := nil;
  FComunidades := nil;
  FMatrizFilas := nil;
  FMatrizColumnas := nil;
  FUsuarioActual := nil;

  // Crear usuario root por defecto
  RegistrarUsuario('Root Admin', 'root', 'root@edd.com', '00000000', 'root123');
end;

destructor TEDDMailSystem.Destroy;
var
  TempUsuario: PUsuario;
  TempComunidad: PComunidad;
begin
  // Liberar memoria de usuarios
  while FUsuarios <> nil do
  begin
    TempUsuario := FUsuarios;
    FUsuarios := FUsuarios^.Siguiente;
    Dispose(TempUsuario);
  end;

  // Liberar memoria de comunidades
  while FComunidades <> nil do
  begin
    TempComunidad := FComunidades;
    FComunidades := FComunidades^.Siguiente;
    Dispose(TempComunidad);
  end;

  inherited Destroy;
end;

function TEDDMailSystem.BuscarUsuario(Email: String): PUsuario;
var
  Actual: PUsuario;
begin
  Result := nil;
  Actual := FUsuarios;
  while Actual <> nil do
  begin
    if Actual^.Email = Email then
    begin
      Result := Actual;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
end;
function TEDDMailSystem.ListarComunidades: String;
var
  Comunidad: PComunidad;
  UsuarioCom: PUsuarioComunidad;
begin
  Result := '';
  Comunidad := FComunidades;

  if Comunidad = nil then
  begin
    Result := 'No hay comunidades creadas.';
    Exit;
  end;

  while Comunidad <> nil do
  begin
    Result := Result + 'Comunidad: ' + Comunidad^.Nombre + LineEnding;
    Result := Result + 'Usuarios:' + LineEnding;

    UsuarioCom := Comunidad^.UsuariosList;
    if UsuarioCom = nil then
      Result := Result + '  (sin usuarios)' + LineEnding
    else
    begin
      while UsuarioCom <> nil do
      begin
        Result := Result + '  - ' + UsuarioCom^.Email + LineEnding;
        UsuarioCom := UsuarioCom^.Siguiente;
      end;
    end;

    Result := Result + LineEnding;
    Comunidad := Comunidad^.Siguiente;
  end;
end;
function TEDDMailSystem.ValidarCredenciales(Email, Password: String): PUsuario;
var
  Usuario: PUsuario;
begin
  Result := nil;
  Usuario := BuscarUsuario(Email);
  if (Usuario <> nil) and (Usuario^.Password = Password) then
    Result := Usuario;
end;

function TEDDMailSystem.RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
var
  NuevoUsuario, Ultimo: PUsuario;
  IdCounter: Integer;
begin
  Result := False;

  // Verificar que el email no exista
  if BuscarUsuario(Email) <> nil then
    Exit;

  // Crear nuevo usuario
  New(NuevoUsuario);

  // Calcular ID
  IdCounter := 1;
  Ultimo := FUsuarios;
  while Ultimo <> nil do
  begin
    if Ultimo^.Siguiente = nil then
      Break;
    IdCounter := IdCounter + 1;
    Ultimo := Ultimo^.Siguiente;
  end;

  // Asignar valores
  NuevoUsuario^.Id := IdCounter;
  NuevoUsuario^.Nombre := Nombre;
  NuevoUsuario^.Usuario := Usuario;
  NuevoUsuario^.Email := Email;
  NuevoUsuario^.Telefono := Telefono;
  NuevoUsuario^.Password := Password;
  NuevoUsuario^.Siguiente := nil;

  // Agregar a la lista
  if FUsuarios = nil then
    FUsuarios := NuevoUsuario
  else
    Ultimo^.Siguiente := NuevoUsuario;

  Result := True;
end;

function TEDDMailSystem.IniciarSesion(Email, Password: String): Boolean;
begin
  FUsuarioActual := ValidarCredenciales(Email, Password);
  Result := FUsuarioActual <> nil;
end;

procedure TEDDMailSystem.CerrarSesion;
begin
  FUsuarioActual := nil;
end;

function TEDDMailSystem.GetUsuarioActual: PUsuario;
begin
  Result := FUsuarioActual;
end;

procedure TEDDMailSystem.CargarUsuariosDesdeJSON(RutaArchivo: String);
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  UsuariosArray: TJSONArray;
  UsuarioObj: TJSONObject;
  FileStream: TFileStream;
  JsonString: String;
  i: Integer;
  PasswordUsuario: String;
begin
  try
    if not FileExists(RutaArchivo) then
    begin
      WriteLn('Error: Archivo JSON no existe: ', RutaArchivo);
      Exit;
    end;

    FileStream := TFileStream.Create(RutaArchivo, fmOpenRead);
    try
      SetLength(JsonString, FileStream.Size);
      if FileStream.Size > 0 then
        FileStream.ReadBuffer(JsonString[1], FileStream.Size);
    finally
      FileStream.Free;
    end;

    if JsonString = '' then
    begin
      WriteLn('Error: Archivo JSON está vacío');
      Exit;
    end;

    JsonData := GetJSON(JsonString);
    try
      JsonObj := JsonData as TJSONObject;
      UsuariosArray := JsonObj.Arrays['usuarios'];

      for i := 0 to UsuariosArray.Count - 1 do
      begin
        UsuarioObj := UsuariosArray.Objects[i];
              // Leer password del JSON si existe, sino usar genérico
      if UsuarioObj.Find('password') <> nil then
        PasswordUsuario := UsuarioObj.Strings['password']
      else
        PasswordUsuario := 'password123';
        if RegistrarUsuario(
          UsuarioObj.Strings['nombre'],
          UsuarioObj.Strings['usuario'],
          UsuarioObj.Strings['email'],
          UsuarioObj.Strings['telefono'],
          PasswordUsuario  // <- Password del JSON

        ) then
          WriteLn('Usuario cargado: ', UsuarioObj.Strings['email'])
        else
          WriteLn('Error al cargar usuario: ', UsuarioObj.Strings['email']);
      end;
    finally
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error al cargar JSON: ', E.Message);
  end;
end;

function TEDDMailSystem.CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean): PCorreo;
begin
  New(Result);
  Result^.Id := Random(9999) + 1;
  Result^.Remitente := Remitente;
  Result^.Destinatario := Destinatario;
  Result^.Estado := 'NL'; // No Leído
  Result^.Programado := Programado;
  Result^.Asunto := Asunto;
  Result^.Fecha := Fecha;
  Result^.Mensaje := Mensaje;
  Result^.Anterior := nil;
  Result^.Siguiente := nil;
end;

procedure TEDDMailSystem.EnviarCorreo(Destinatario, Asunto, Mensaje: String);
var
  NuevoCorreo: PCorreo;
  UsuarioDestino: PUsuario;
begin
  if FUsuarioActual = nil then
    Exit;

  UsuarioDestino := BuscarUsuario(Destinatario);
  if UsuarioDestino = nil then
    Exit;

  // Verificar que sea contacto
  if BuscarContacto(FUsuarioActual, Destinatario) = nil then
    Exit;

  NuevoCorreo := CrearCorreo(FUsuarioActual^.Email, Destinatario, Asunto, Mensaje,
                           FormatDateTime('dd/mm/yy hh:nn', Now));

  // Agregar a bandeja de entrada del destinatario (lista doblemente enlazada)
  // Aquí iría la lógica para agregar a la bandeja del destinatario

  // Actualizar matriz de relaciones
  ActualizarMatrizRelaciones(FUsuarioActual^.Email, Destinatario);
end;

function TEDDMailSystem.BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
begin
  Result := nil;
  // Implementar búsqueda en lista circular de contactos del usuario
  // Por ahora retorna nil para permitir que compile
end;

procedure TEDDMailSystem.ActualizarMatrizRelaciones(Remitente, Destinatario: String);
begin
  // Implementar lógica de matriz dispersa
  WriteLn('Actualizando matriz de relaciones: ', Remitente, ' -> ', Destinatario);
end;

function TEDDMailSystem.BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
begin
  Result := nil;
  // Implementar búsqueda en filas de matriz
end;

function TEDDMailSystem.BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;
begin
  Result := nil;
  // Implementar búsqueda en columnas de matriz
end;

// Implementar resto de métodos...
procedure TEDDMailSystem.ProgramarCorreo(Destinatario, Asunto, Mensaje, FechaEnvio: String);
begin
  // Implementar cola para correos programados
  WriteLn('Programando correo para: ', Destinatario, ' en fecha: ', FechaEnvio);
end;

procedure TEDDMailSystem.EliminarCorreo(Usuario: PUsuario; CorreoId: Integer);
begin
  // Implementar pila para correos eliminados
  WriteLn('Eliminando correo ID: ', CorreoId);
end;

procedure TEDDMailSystem.MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer);
begin
  // Cambiar estado de correo a 'L'
  WriteLn('Marcando correo como leído ID: ', CorreoId);
end;

function TEDDMailSystem.GetBandejaEntrada(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  // Retornar lista doblemente enlazada de correos del usuario
end;

function TEDDMailSystem.GetPapelera(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  // Retornar pila de correos eliminados
end;

function TEDDMailSystem.GetCorreosProgramados(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  // Retornar cola de correos programados
end;

procedure TEDDMailSystem.ProcesarCorreosProgramados;
begin
  // Procesar cola FIFO de correos programados
  WriteLn('Procesando correos programados...');
end;

function TEDDMailSystem.AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
begin
  Result := False;
  // Implementar lista circular de contactos
  if Usuario <> nil then
  begin
    WriteLn('Agregando contacto: ', Email, ' para usuario: ', Usuario^.Email);
    Result := True; // Simular éxito por ahora
  end;
end;

function TEDDMailSystem.GetContactos(Usuario: PUsuario): PContacto;
begin
  Result := nil;
  // Retornar lista circular de contactos
end;

function TEDDMailSystem.CrearComunidad(Nombre: String): Boolean;
var
  NuevaComunidad, Ultima: PComunidad;
  IdCounter: Integer;
begin
  Result := False;

  // Verificar que no exista
  Ultima := FComunidades;
  while Ultima <> nil do
  begin
    if Ultima^.Nombre = Nombre then
      Exit; // Ya existe
    if Ultima^.Siguiente = nil then
      Break;
    Ultima := Ultima^.Siguiente;
  end;

  // Crear nueva comunidad
  New(NuevaComunidad);
  IdCounter := 1;
  if FComunidades <> nil then
  begin
    Ultima := FComunidades;
    while Ultima^.Siguiente <> nil do
    begin
      Inc(IdCounter);
      Ultima := Ultima^.Siguiente;
    end;
  end;

  NuevaComunidad^.Id := IdCounter;
  NuevaComunidad^.Nombre := Nombre;
  NuevaComunidad^.UsuariosList := nil;
  NuevaComunidad^.Siguiente := nil;

  if FComunidades = nil then
    FComunidades := NuevaComunidad
  else
    Ultima^.Siguiente := NuevaComunidad;

  Result := True;
end;

function TEDDMailSystem.AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: String): Boolean;
var
  Comunidad: PComunidad;
  Usuario: PUsuario;
  NuevoUsuarioCom, UltimoUsuarioCom: PUsuarioComunidad;
begin
  Result := False;

  // Buscar comunidad
  Comunidad := FComunidades;
  while (Comunidad <> nil) and (Comunidad^.Nombre <> NombreComunidad) do
    Comunidad := Comunidad^.Siguiente;

  if Comunidad = nil then
    Exit;

  // Verificar que el usuario existe
  Usuario := BuscarUsuario(EmailUsuario);
  if Usuario = nil then
    Exit;

  // Verificar que no esté ya en la comunidad
  UltimoUsuarioCom := Comunidad^.UsuariosList;
  while UltimoUsuarioCom <> nil do
  begin
    if UltimoUsuarioCom^.Email = EmailUsuario then
      Exit; // Ya está en la comunidad
    if UltimoUsuarioCom^.Siguiente = nil then
      Break;
    UltimoUsuarioCom := UltimoUsuarioCom^.Siguiente;
  end;

  // Agregar usuario a la comunidad
  New(NuevoUsuarioCom);
  NuevoUsuarioCom^.Email := EmailUsuario;
  NuevoUsuarioCom^.Siguiente := nil;

  if Comunidad^.UsuariosList = nil then
    Comunidad^.UsuariosList := NuevoUsuarioCom
  else
    UltimoUsuarioCom^.Siguiente := NuevoUsuarioCom;

  Result := True;
end;

function TEDDMailSystem.GetComunidades: PComunidad;
begin
  Result := FComunidades;
end;

procedure TEDDMailSystem.ActualizarPerfil(Usuario: PUsuario; NuevoNombre, NuevoUsuario, NuevoTelefono: String);
begin
  if Usuario <> nil then
  begin
    Usuario^.Nombre := NuevoNombre;
    Usuario^.Usuario := NuevoUsuario;
    Usuario^.Telefono := NuevoTelefono;
    WriteLn('Perfil actualizado para: ', Usuario^.Email);
  end;
end;

// Funciones de reportes usando Graphviz
procedure TEDDMailSystem.GenerarReporteUsuarios(RutaCarpeta: String);
var
  Archivo: TextFile;
  Usuario: PUsuario;
  Process: TProcess;
begin
  try
    ForceDirectories(RutaCarpeta);

    AssignFile(Archivo, RutaCarpeta + '/usuarios.dot');
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    rankdir=LR;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(Archivo, '    label="Lista Simple de Usuarios";');
    WriteLn(Archivo, '    fontsize=16;');

    Usuario := FUsuarios;
    while Usuario <> nil do
    begin
      WriteLn(Archivo, Format('    user%d [label="ID: %d|Nombre: %s|Usuario: %s|Email: %s|Telefono: %s"];',
        [Usuario^.Id, Usuario^.Id, Usuario^.Nombre, Usuario^.Usuario, Usuario^.Email, Usuario^.Telefono]));

      if Usuario^.Siguiente <> nil then
        WriteLn(Archivo, Format('    user%d -> user%d;', [Usuario^.Id, Usuario^.Siguiente^.Id]));

      Usuario := Usuario^.Siguiente;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen usando Graphviz
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(RutaCarpeta + '/usuarios.dot');
        Process.Parameters.Add('-o');
        Process.Parameters.Add(RutaCarpeta + '/usuarios.png');
        Process.Options := Process.Options + [poWaitOnExit, poUsePipes];
        Process.Execute;
        WriteLn('Reporte de usuarios generado: ', RutaCarpeta, '/usuarios.png');
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen (¿Graphviz instalado?): ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de usuarios: ', E.Message);
  end;
end;

procedure TEDDMailSystem.GenerarReporteRelaciones(RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
begin
  try
    ForceDirectories(RutaCarpeta);

    AssignFile(Archivo, RutaCarpeta + '/relaciones.dot');
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Matriz Dispersa - Relaciones de Correos";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=box];');
    WriteLn(Archivo, '    matriz [label="Matriz Dispersa\n(Sin relaciones aún)", style=filled, fillcolor=lightyellow];');
    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(RutaCarpeta + '/relaciones.dot');
        Process.Parameters.Add('-o');
        Process.Parameters.Add(RutaCarpeta + '/relaciones.png');
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de relaciones generado: ', RutaCarpeta, '/relaciones.png');
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;
  except
    on E: Exception do
      WriteLn('Error al generar reporte de relaciones: ', E.Message);
  end;
end;

procedure TEDDMailSystem.GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de lista doblemente enlazada
  WriteLn('Generando reporte de correos recibidos para: ', Usuario^.Email);
end;

procedure TEDDMailSystem.GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de pila
  WriteLn('Generando reporte de papelera para: ', Usuario^.Email);
end;

procedure TEDDMailSystem.GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de cola
  WriteLn('Generando reporte de correos programados para: ', Usuario^.Email);
end;

procedure TEDDMailSystem.GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de lista circular
  WriteLn('Generando reporte de contactos para: ', Usuario^.Email);
end;

procedure TEDDMailSystem.GenerarReporteComunidades(RutaCarpeta: String);
var
  Archivo: TextFile;
  Comunidad: PComunidad;
  UsuarioCom: PUsuarioComunidad;
  Process: TProcess;
begin
  try
    ForceDirectories(RutaCarpeta);

    AssignFile(Archivo, RutaCarpeta + '/comunidades.dot');
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Lista de Listas - Comunidades";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=box];');

    if FComunidades = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin comunidades", style=filled, fillcolor=lightgray];');
    end
    else
    begin
      Comunidad := FComunidades;
      while Comunidad <> nil do
      begin
        WriteLn(Archivo, Format('    com%d [label="Comunidad: %s", style=filled, fillcolor=lightblue];',
          [Comunidad^.Id, Comunidad^.Nombre]));

        UsuarioCom := Comunidad^.UsuariosList;
        while UsuarioCom <> nil do
        begin
          WriteLn(Archivo, Format('    user_%s [label="%s", style=filled, fillcolor=lightyellow];',
            [StringReplace(UsuarioCom^.Email, '@', '_', [rfReplaceAll]), UsuarioCom^.Email]));
          WriteLn(Archivo, Format('    com%d -> user_%s;',
            [Comunidad^.Id, StringReplace(UsuarioCom^.Email, '@', '_', [rfReplaceAll])]));
          UsuarioCom := UsuarioCom^.Siguiente;
        end;

        Comunidad := Comunidad^.Siguiente;
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(RutaCarpeta + '/comunidades.dot');
        Process.Parameters.Add('-o');
        Process.Parameters.Add(RutaCarpeta + '/comunidades.png');
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de comunidades: ', E.Message);
  end;
end;

end.
