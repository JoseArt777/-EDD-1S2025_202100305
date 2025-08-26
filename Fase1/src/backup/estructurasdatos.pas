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
      ListaContactos: PContacto;  // Lista circular de contactos del usuario

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

      procedure AgregarContactoALista(var PrimerContacto: PContacto; NuevoContacto: PContacto); // <- AGREGAR


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
      function EliminarContacto(Usuario: PUsuario; Email: String): Boolean;

    // Funciones de contactos
    function AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
    function GetContactos(Usuario: PUsuario): PContacto;
    // Funciones auxiliares para contactos
     function BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
     function CrearContacto(Email: String): PContacto;

     function ContarContactos(PrimerContacto: PContacto): Integer;

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
  NuevoUsuario^.ListaContactos := nil; // Inicializar lista de contactos vacía


  // Agregar a la lista
  if FUsuarios = nil then
    FUsuarios := NuevoUsuario
  else
    Ultimo^.Siguiente := NuevoUsuario;

  Result := True;
end;
  // Implementación mejorada de GetContactos:
function TEDDMailSystem.GetContactos(Usuario: PUsuario): PContacto;
begin
  Result := nil;
  if Usuario <> nil then
    Result := Usuario^.ListaContactos;
end;

// Implementación mejorada de AgregarContacto:
function TEDDMailSystem.AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
var
  NuevoContacto: PContacto;
begin
  Result := False;

  if Usuario = nil then
    Exit;

  // Verificar que el contacto no sea el mismo usuario
  if Usuario^.Email = Email then
  begin
    WriteLn('Error: No puede agregarse a sí mismo como contacto');
    Exit;
  end;

  // Verificar que no esté ya en contactos
  if BuscarContacto(Usuario, Email) <> nil then
  begin
    WriteLn('Error: El contacto ya existe en la lista');
    Exit;
  end;

  // Crear el nuevo contacto
  NuevoContacto := CrearContacto(Email);
  if NuevoContacto = nil then
  begin
    WriteLn('Error: El usuario no existe en el sistema');
    Exit;
  end;

  // Agregar a la lista circular del usuario específico
  AgregarContactoALista(Usuario^.ListaContactos, NuevoContacto);

  WriteLn('Contacto agregado exitosamente: ', Email);
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

// Implementación mejorada de BuscarContacto:
function TEDDMailSystem.BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
var
  Actual: PContacto;
  PrimerContacto: PContacto;
  Contador: Integer;
begin
  Result := nil;
  if Usuario = nil then
    Exit;

  PrimerContacto := Usuario^.ListaContactos;
  if PrimerContacto = nil then
    Exit;

  Actual := PrimerContacto;
  Contador := 0;
  repeat
    if Actual^.Email = Email then
    begin
      Result := Actual;
      Exit;
    end;
    Actual := Actual^.Siguiente;
    Inc(Contador);
  until (Actual = PrimerContacto) or (Contador > 1000); // Prevenir bucle infinito
end;
function TEDDMailSystem.EliminarContacto(Usuario: PUsuario; Email: String): Boolean;
var
  Actual, Anterior: PContacto;
  PrimerContacto: PContacto;
  Contador: Integer;
begin
  Result := False;
  if Usuario = nil then
    Exit;

  PrimerContacto := Usuario^.ListaContactos;
  if PrimerContacto = nil then
    Exit;

  // Si solo hay un contacto
  if PrimerContacto^.Siguiente = PrimerContacto then
  begin
    if PrimerContacto^.Email = Email then
    begin
      Dispose(PrimerContacto);
      Usuario^.ListaContactos := nil;
      Result := True;
    end;
    Exit;
  end;

  // Buscar el contacto a eliminar
  Actual := PrimerContacto;
  Anterior := nil;
  Contador := 0;

  // Encontrar el anterior al primero
  repeat
    if Actual^.Siguiente = PrimerContacto then
    begin
      Anterior := Actual;
      Break;
    end;
    Actual := Actual^.Siguiente;
    Inc(Contador);
  until Contador > 1000;

  // Buscar el contacto específico
  Actual := PrimerContacto;
  Contador := 0;
  repeat
    if Actual^.Email = Email then
    begin
      // Encontrado, eliminar
      if Actual = PrimerContacto then
      begin
        // Es el primer elemento
        Usuario^.ListaContactos := Actual^.Siguiente;
        Anterior^.Siguiente := Usuario^.ListaContactos;
      end
      else
      begin
        // No es el primer elemento
        Anterior^.Siguiente := Actual^.Siguiente;
      end;

      Dispose(Actual);
      Result := True;
      Exit;
    end;

    Anterior := Actual;
    Actual := Actual^.Siguiente;
    Inc(Contador);
  until (Actual = PrimerContacto) or (Contador > 1000);
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

 function TEDDMailSystem.CrearContacto(Email: String): PContacto;
var
  UsuarioExistente: PUsuario;
begin
  Result := nil;

  // Verificar que el usuario existe en el sistema
  UsuarioExistente := BuscarUsuario(Email);
  if UsuarioExistente = nil then
    Exit;

  // Crear nuevo contacto
  New(Result);
  Result^.Id := Random(9999) + 1;
  Result^.Nombre := UsuarioExistente^.Nombre;
  Result^.Usuario := UsuarioExistente^.Usuario;
  Result^.Email := UsuarioExistente^.Email;
  Result^.Telefono := UsuarioExistente^.Telefono;
  Result^.Siguiente := nil;
end;
procedure TEDDMailSystem.AgregarContactoALista(var PrimerContacto: PContacto; NuevoContacto: PContacto);
var
  Actual: PContacto;
begin
  if PrimerContacto = nil then
  begin
    // Primera inserción - crear lista circular
    PrimerContacto := NuevoContacto;
    NuevoContacto^.Siguiente := NuevoContacto; // Apunta a sí mismo
  end
  else
  begin
    // Buscar el último nodo (que apunta al primero)
    Actual := PrimerContacto;
    while Actual^.Siguiente <> PrimerContacto do
      Actual := Actual^.Siguiente;

    // Insertar el nuevo contacto
    Actual^.Siguiente := NuevoContacto;
    NuevoContacto^.Siguiente := PrimerContacto;
  end;
end;
function TEDDMailSystem.ContarContactos(PrimerContacto: PContacto): Integer;
var
  Actual: PContacto;
begin
  Result := 0;
  if PrimerContacto = nil then
    Exit;

  Actual := PrimerContacto;
  repeat
    Inc(Result);
    Actual := Actual^.Siguiente;
  until Actual = PrimerContacto;
end;

procedure TEDDMailSystem.GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Contacto, PrimerContacto: PContacto;
  Process: TProcess;
  NombreArchivo: String;
  EmailLimpio: String;
  SigEmailLimpio: String;  // <- DECLARAR AQUÍ
  UltimoLimpio: String;    // <- DECLARAR AQUÍ
  PrimeroLimpio: String;   // <- DECLARAR AQUÍ
  UltimoEmail: String;     // <- DECLARAR AQUÍ
  Contador: Integer;
begin
  if Usuario = nil then
    Exit;

  try
    ForceDirectories(RutaCarpeta);

    NombreArchivo := RutaCarpeta + '/contactos_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Lista Circular de Contactos - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    rankdir=LR;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightblue];');

    PrimerContacto := GetContactos(Usuario);

    if PrimerContacto = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin contactos", style=filled, fillcolor=lightgray];');
    end
    else
    begin
      Contacto := PrimerContacto;
      Contador := 0;

      repeat
        EmailLimpio := StringReplace(Contacto^.Email, '@', '_at_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);

        WriteLn(Archivo, Format('    contact_%s [label="ID: %d|Nombre: %s|Usuario: %s|Email: %s|Tel: %s"];',
          [EmailLimpio, Contacto^.Id, Contacto^.Nombre, Contacto^.Usuario,
           Contacto^.Email, Contacto^.Telefono]));

        Contacto := Contacto^.Siguiente;
        Inc(Contador);
      until (Contacto = PrimerContacto) or (Contador > 100); // Prevenir bucle infinito

      // Generar las conexiones circulares
      Contacto := PrimerContacto;
      Contador := 0;
      repeat
        EmailLimpio := StringReplace(Contacto^.Email, '@', '_at_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);

        if Contacto^.Siguiente <> nil then
        begin

           SigEmailLimpio := StringReplace(Contacto^.Siguiente^.Email, '@', '_at_', [rfReplaceAll]);
          SigEmailLimpio := StringReplace(SigEmailLimpio, '.', '_', [rfReplaceAll]);
          SigEmailLimpio := StringReplace(SigEmailLimpio, '-', '_', [rfReplaceAll]);

          WriteLn(Archivo, Format('    contact_%s -> contact_%s;', [EmailLimpio, SigEmailLimpio]));
        end;

        Contacto := Contacto^.Siguiente;
        Inc(Contador);
      until (Contacto = PrimerContacto) or (Contador > 100);

      // Indicar la naturaleza circular
      WriteLn(Archivo, '    edge [color=red, style=dashed];');
      if PrimerContacto^.Siguiente <> PrimerContacto then
      begin
        UltimoEmail := PrimerContacto^.Email;
        while Contacto^.Siguiente <> PrimerContacto do
          Contacto := Contacto^.Siguiente;

        UltimoLimpio := StringReplace(Contacto^.Email, '@', '_at_', [rfReplaceAll]);
        UltimoLimpio := StringReplace(UltimoLimpio, '.', '_', [rfReplaceAll]);
        UltimoLimpio := StringReplace(UltimoLimpio, '-', '_', [rfReplaceAll]);

         PrimeroLimpio := StringReplace(PrimerContacto^.Email, '@', '_at_', [rfReplaceAll]);
        PrimeroLimpio := StringReplace(PrimeroLimpio, '.', '_', [rfReplaceAll]);
        PrimeroLimpio := StringReplace(PrimeroLimpio, '-', '_', [rfReplaceAll]);

        WriteLn(Archivo, Format('    contact_%s -> contact_%s [label="circular"];',
                [UltimoLimpio, PrimeroLimpio]));
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen usando Graphviz
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(NombreArchivo);
        Process.Parameters.Add('-o');
        Process.Parameters.Add(ChangeFileExt(NombreArchivo, '.png'));
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de contactos generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de contactos: ', E.Message);
  end;
end;
end.
