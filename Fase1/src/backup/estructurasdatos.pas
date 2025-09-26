unit EstructurasDatos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

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
  //Fase 2

    // Nodo para Árbol AVL (Borradores)
  PNodoAVL = ^TNodoAVL;
  TNodoAVL = record
    Correo: PCorreo;
    Altura: Integer;
    Izquierdo: PNodoAVL;
    Derecho: PNodoAVL;
  end;

  // Nodo para BST (Comunidades)
  PNodoBST = ^TNodoBST;
  PMensajeComunidad = ^TMensajeComunidad;

  TMensajeComunidad = record
    Correo: String;
    Mensaje: String;
    FechaPublicacion: String;
    Siguiente: PMensajeComunidad;
  end;

  TNodoBST = record
    NombreComunidad: String;
    FechaCreacion: String;
    NumeroMensajes: Integer;
    ListaMensajes: PMensajeComunidad;
    Izquierdo: PNodoBST;
    Derecho: PNodoBST;
  end;

  // Nodo para Árbol B (Favoritos)
  PNodoB = ^TNodoB;
  TNodoB = record
    NumClaves: Integer;
    Claves: array[0..3] of Integer; // Máximo 4 claves
    Correos: array[0..3] of PCorreo; // Correos asociados a las claves
    Hijos: array[0..4] of PNodoB; // Máximo 5 hijos
    EsHoja: Boolean;
  end;

  // Estructura Usuario (Lista Simple)
  TUsuario = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Password: String;
    Siguiente: PUsuario;
    ListaContactos: PContacto;    // Lista circular de contactos del usuario
    BandejaEntrada: PCorreo;      // Lista doblemente enlazada
    Papelera: PCorreo;            // Pila LIFO
    CorreosProgramados: PCorreo;  // Cola FIFO
    ArbolBorradores: PNodoAVL;      // Árbol AVL para borradores
    ArbolFavoritos: PNodoB;         // Árbol B para favoritos
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
    Anterior: PCorreo;  // enlace hacia el nodo previo
    Siguiente: PCorreo; // enlace hacia el nodo siguiente
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
      FArbolComunidades: PNodoBST;    // Árbol BST para comunidades

      // Funciones auxiliares para correos
      function CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean = False; IdFijo: Integer = -1): PCorreo;
      procedure AgregarContactoALista(var PrimerContacto: PContacto; NuevoContacto: PContacto);

      // Funciones auxiliares para matriz dispersa
      function BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
      function BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;
      function BuscarUsuarioPorId(IdBuscado: Integer): PUsuario;
      procedure Inbox_InsertTail(var Head: PCorreo; NewNode: PCorreo);

      // FASE 2 - Funciones para AVL
      function RotarDerecha(y: PNodoAVL): PNodoAVL;
      function RotarIzquierda(x: PNodoAVL): PNodoAVL;
      function ObtenerAltura(nodo: PNodoAVL): Integer;
      function ObtenerBalance(nodo: PNodoAVL): Integer;
      function InsertarAVL(nodo: PNodoAVL; correo: PCorreo): PNodoAVL;

      // Funciones para métodos de Fase 2
      function EliminarFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
      function BuscarCorreoEnAVL(nodo: PNodoAVL; CorreoId: Integer): PCorreo;

      procedure GenerarNodosAVL(var Archivo: TextFile; nodo: PNodoAVL);

      // Funciones para BST
      function InsertarBST(nodo: PNodoBST; nombreComunidad: String): PNodoBST;
      function BuscarComunidadBST(nodo: PNodoBST; nombre: String): PNodoBST;

      // Funciones para Árbol B
      function CrearNodoB: PNodoB;
      function InsertarB(raiz: PNodoB; correo: PCorreo): PNodoB;


      // Funciones auxiliares adicionales
      function BuscarCorreoEnBandeja(Usuario: PUsuario; CorreoId: Integer): PCorreo;

      // Recorridos del Árbol AVL
      procedure RecorridoInOrdenAVL(nodo: PNodoAVL; lista: TStringList);
      procedure RecorridoPreOrdenAVL(nodo: PNodoAVL; lista: TStringList);
      procedure RecorridoPostOrdenAVL(nodo: PNodoAVL; lista: TStringList);

      // MÉTODOS AUXILIARES FALTANTES PARA FASE 2:
      procedure GenerarNodosB(var Archivo: TextFile; nodo: PNodoB; nivel: Integer);
        procedure GenerarNodosBST(var Archivo: TextFile; nodo: PNodoBST);



    public
      constructor Create;
      destructor Destroy; override;

      // Funciones de usuario
      function RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String; IdFijo: Integer = -1): Boolean;
      function IniciarSesion(Email, Password: String): Boolean;
      procedure CerrarSesion;
      function GetUsuarioActual: PUsuario;
      function ListarComunidades: String;

      // Funciones para carga masiva (ROOT)
      procedure CargarUsuariosDesdeJSON(RutaArchivo: String);
      procedure CargarCorreosDesdeJSON(const RutaArchivo: String);

      // Funciones de correo
      procedure ActualizarMatrizRelaciones(Remitente, Destinatario: String);
      procedure EnviarCorreo(Destinatario, Asunto, Mensaje: String);
      procedure ProgramarCorreo(Destinatario, Asunto, Mensaje, FechaEnvio: String);
      procedure EliminarCorreo(Usuario: PUsuario; CorreoId: Integer);
      procedure MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer);
      function GetBandejaEntrada(Usuario: PUsuario): PCorreo;
      function GetPapelera(Usuario: PUsuario): PCorreo; // Pila
      function GetCorreosProgramados(Usuario: PUsuario): PCorreo; // Cola
      procedure ProcesarCorreosProgramados;

      // Funciones de usuarios y contactos
      function EliminarContacto(Usuario: PUsuario; Email: String): Boolean;
      function BuscarUsuario(Email: String): PUsuario;
      function ValidarCredenciales(Email, Password: String): PUsuario;
      function AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
      function GetContactos(Usuario: PUsuario): PContacto;
      function BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
      function CrearContacto(Email: String): PContacto;
      function ContarContactos(PrimerContacto: PContacto): Integer;

      // Funciones de comunidades (Fase 1)
      function CrearComunidad(Nombre: String): Boolean;
      function AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: String): Boolean;
      function GetComunidades: PComunidad;

      // Funciones de actualización de perfil
      procedure ActualizarPerfil(Usuario: PUsuario; NuevoNombre, NuevoUsuario, NuevoTelefono: String);

      // Funciones de reportes (Fase 1)
      procedure GenerarReporteUsuarios(RutaCarpeta: String);
      procedure GenerarReporteRelaciones(RutaCarpeta: String);
      procedure GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReporteComunidades(RutaCarpeta: String);

      // FASE 2 - Nuevas funciones públicas
      function GuardarBorrador(Usuario: PUsuario; Destinatario, Asunto, Mensaje: String): Boolean;
      function ObtenerBorradores(Usuario: PUsuario; tipoRecorrido: String): TStringList;
      function MarcarComoFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
      function CrearComunidadBST(nombreComunidad: String): Boolean;
      function PublicarMensajeAComunidad(nombreComunidad, correoUsuario, mensaje: String): Boolean;

      // MÉTODOS PÚBLICOS FALTANTES PARA FASE 2:
      procedure GenerarReporteComunidadesBST(RutaCarpeta: String);
      procedure GenerarReporteFavoritos(Usuario: PUsuario; RutaCarpeta: String);
      function ObtenerMensajesComunidad(nombreComunidad: String): String;
        procedure GenerarReporteBorradores(Usuario: PUsuario; RutaCarpeta: String);


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
    FArbolComunidades := nil;  // ← NUEVO: Inicializar árbol de comunidades


  // Crear usuario root por defecto (Id fijo = 0)
  RegistrarUsuario('Root Admin', 'root', 'root@edd.com', '00000000', 'root123', 0);
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
function TEDDMailSystem.BuscarComunidadBST(nodo: PNodoBST; nombre: String): PNodoBST;
begin
  Result := nil;
  if nodo = nil then Exit;

  if nombre = nodo^.NombreComunidad then
    Result := nodo
  else if nombre < nodo^.NombreComunidad then
    Result := BuscarComunidadBST(nodo^.Izquierdo, nombre)
  else
    Result := BuscarComunidadBST(nodo^.Derecho, nombre);
end;

function TEDDMailSystem.BuscarB(nodo: PNodoB; id: Integer): PCorreo;
var
  i: Integer;
begin
  Result := nil;
  if nodo = nil then Exit;

  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if nodo^.Claves[i] = id then
    begin
      Result := nodo^.Correos[i];
      Exit;
    end;
  end;
end;

function TEDDMailSystem.EliminarFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
begin
  Result := False;
  if Usuario = nil then Exit;

  // TODO: Implementar eliminación del árbol B
  WriteLn('Eliminar favorito ID: ', CorreoId);
  Result := True;
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

// Modificar el RegistrarUsuario para inicializar nuevas estructuras
function TEDDMailSystem.RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String; IdFijo: Integer): Boolean;
var
  NuevoUsuario, Cur: PUsuario;
  MaxId: Integer;
begin
  Result := False;

  // Verificar que el email no exista
  if BuscarUsuario(Email) <> nil then
    Exit;

  // Validar que el IdFijo (si viene) no esté duplicado
  if (IdFijo >= 0) and (BuscarUsuarioPorId(IdFijo) <> nil) then
  begin
    WriteLn('Error: id duplicado en JSON: ', IdFijo, ' (', Email, ')');
    Exit;
  end;

  // Crear nuevo usuario
  New(NuevoUsuario);

  // Calcular MaxId actual
  MaxId := 0;
  Cur := FUsuarios;
  while Cur <> nil do
  begin
    if Cur^.Id > MaxId then
      MaxId := Cur^.Id;
    Cur := Cur^.Siguiente;
  end;

  if IdFijo >= 0 then
    NuevoUsuario^.Id := IdFijo
  else
    NuevoUsuario^.Id := MaxId + 1;

  // Asignar valores
  NuevoUsuario^.Nombre := Nombre;
  NuevoUsuario^.Usuario := Usuario;
  NuevoUsuario^.Email := Email;
  NuevoUsuario^.Telefono := Telefono;
  NuevoUsuario^.Password := Password;
  NuevoUsuario^.Siguiente := nil;

  // INICIALIZAR ESTRUCTURAS EXISTENTES
  NuevoUsuario^.ListaContactos := nil;
  NuevoUsuario^.BandejaEntrada := nil;
  NuevoUsuario^.Papelera := nil;
  NuevoUsuario^.CorreosProgramados := nil;

  // ← NUEVO: INICIALIZAR NUEVAS ESTRUCTURAS FASE 2
  NuevoUsuario^.ArbolBorradores := nil;
  NuevoUsuario^.ArbolFavoritos := nil;

  // Agregar a la lista (al final)
  if FUsuarios = nil then
    FUsuarios := NuevoUsuario
  else
  begin
    Cur := FUsuarios;
    while Cur^.Siguiente <> nil do
      Cur := Cur^.Siguiente;
    Cur^.Siguiente := NuevoUsuario;
  end;

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
  IdJson: Integer;

begin
  JsonString := '';  // *** AGREGAR ESTA INICIALIZACIÓN ***
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

        // Leer id del JSON (si no está, -1)
        IdJson := UsuarioObj.Get('id', -1);

        if RegistrarUsuario(
          UsuarioObj.Strings['nombre'],
          UsuarioObj.Strings['usuario'],
          UsuarioObj.Strings['email'],
          UsuarioObj.Strings['telefono'],
          PasswordUsuario,  // Password del JSON o genérico
          IdJson            // Id del JSON (o -1)
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

function TEDDMailSystem.BuscarUsuarioPorId(IdBuscado: Integer): PUsuario;
var
  U: PUsuario;
begin
  Result := nil;
  U := FUsuarios;
  while U <> nil do
  begin
    if U^.Id = IdBuscado then
    begin
      Result := U;
      Exit;
    end;
    U := U^.Siguiente;
  end;
end;

procedure TEDDMailSystem.Inbox_InsertTail(var Head: PCorreo; NewNode: PCorreo);
var
  P: PCorreo;
begin
  if NewNode = nil then Exit;
  NewNode^.Anterior := nil;
  NewNode^.Siguiente := nil;

  if Head = nil then
  begin
    Head := NewNode;
    Exit;
  end;

  P := Head;
  while P^.Siguiente <> nil do
    P := P^.Siguiente;

  // enlazar doblemente
  P^.Siguiente := NewNode;
  NewNode^.Anterior := P;
end;

procedure TEDDMailSystem.CargarCorreosDesdeJSON(const RutaArchivo: String);
var
  JsonData: TJSONData;
  Root, ObjUsuario, MailObj: TJSONObject;
  ArrCorreos, Inbox: TJSONArray;
  i, k, UsuarioId: Integer;
  U: PUsuario;
  C: PCorreo;
  FS: TFileStream;
  S: String;
  estadoTxt, progTxt: String;
begin
    S := '';

  if not FileExists(RutaArchivo) then
  begin
    WriteLn('Error: Archivo JSON de correos no existe: ', RutaArchivo);
    Exit;
  end;

  // leer archivo completo
  FS := TFileStream.Create(RutaArchivo, fmOpenRead);
  try
    SetLength(S, FS.Size);
    if FS.Size > 0 then FS.ReadBuffer(S[1], FS.Size);
  finally
    FS.Free;
  end;

  if S = '' then
  begin
    WriteLn('Error: Archivo de correos vacío');
    Exit;
  end;

  JsonData := GetJSON(S);
  try
    Root := JsonData as TJSONObject;
    ArrCorreos := Root.Arrays['correos'];        // estructura esperada

    for i := 0 to ArrCorreos.Count - 1 do
    begin
      ObjUsuario := ArrCorreos.Objects[i];
      UsuarioId := ObjUsuario.Get('usuario_id', 0);

      U := BuscarUsuarioPorId(UsuarioId);
      if U = nil then
      begin
        WriteLn('Aviso: usuario_id ', UsuarioId, ' no existe. Se omite su bandeja.');
        Continue;
      end;

      Inbox := ObjUsuario.Arrays['bandeja_entrada'];
      for k := 0 to Inbox.Count - 1 do
      begin
        MailObj := Inbox.Objects[k];

        New(C);
        // mapear campos
        C^.Id          := MailObj.Get('id', 0);
        C^.Remitente   := MailObj.Get('remitente', '');
        C^.Destinatario:= U^.Email; // destinatario es el dueño de la bandeja
        estadoTxt      := LowerCase(MailObj.Get('estado',''));
        if (Pos('no', estadoTxt) > 0) then C^.Estado := 'NL' else C^.Estado := 'L';

        progTxt        := LowerCase(MailObj.Get('programado','no'));
        C^.Programado  := (progTxt = 'si') or (progTxt = 'sí');

        C^.Asunto      := MailObj.Get('asunto', '');
        C^.Fecha       := MailObj.Get('fecha', '');
        C^.Mensaje     := MailObj.Get('mensaje', '');
        C^.FechaEnvio  := ''; // solo se usa para programados salientes

        C^.Anterior := nil;
        C^.Siguiente := nil;

        // insertar al final de la bandeja (lista doble)
        Inbox_InsertTail(U^.BandejaEntrada, C);

        // actualizar matriz remitente->destinatario
        ActualizarMatrizRelaciones(C^.Remitente, U^.Email);
      end;
    end;

    WriteLn('Carga de correos completada desde: ', RutaArchivo);
  finally
    JsonData.Free;
  end;
end;


function TEDDMailSystem.CrearCorreo(
  Remitente, Destinatario, Asunto, Mensaje, Fecha: String;
  Programado: Boolean = False; IdFijo: Integer = -1
): PCorreo;
begin
  New(Result);

  if IdFijo >= 0 then
    Result^.Id := IdFijo
  else
    Result^.Id := Random(9999) + 1;

  Result^.Remitente    := Remitente;
  Result^.Destinatario := Destinatario;
  Result^.Estado       := 'NL';        // No leído por defecto
  Result^.Programado   := Programado;
  Result^.Asunto       := Asunto;
  Result^.Fecha        := Fecha;
  Result^.Mensaje      := Mensaje;

  if Programado then
    Result^.FechaEnvio := Fecha      // para programados usas la fecha indicada
  else
    Result^.FechaEnvio := FormatDateTime('dd/mm/yy hh:nn', Now); // enviado ahora

  Result^.Anterior := nil;
  Result^.Siguiente := nil;
end;


procedure TEDDMailSystem.EnviarCorreo(Destinatario, Asunto, Mensaje: String);
var
  NuevoCorreo: PCorreo;
  UsuarioDestino: PUsuario;
begin
  if FUsuarioActual = nil then Exit;

  UsuarioDestino := BuscarUsuario(Destinatario);
  if UsuarioDestino = nil then Exit;

  // Verificar que sea contacto
  if BuscarContacto(FUsuarioActual, Destinatario) = nil then Exit;

  NuevoCorreo := CrearCorreo(
    FUsuarioActual^.Email, Destinatario, Asunto, Mensaje,
    FormatDateTime('dd/mm/yy hh:nn', Now)
  );

  // INSERTAR en bandeja del destinatario (lista doble)
  Inbox_InsertTail(UsuarioDestino^.BandejaEntrada, NuevoCorreo);

  // Actualizar matriz de relaciones (remitente destinatario)
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

        Usuario^.ListaContactos := Actual^.Siguiente;
        Anterior^.Siguiente := Usuario^.ListaContactos;
      end
      else
      begin

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
var
  FilaHdr: PMatrizDispersaFila;
  ColHdr: PMatrizDispersaColumna;
  N, P: PMatrizDispersaNodo;
begin
  if (Remitente = '') or (Destinatario = '') then Exit;

  FilaHdr := BuscarFilaMatriz(Remitente);
  ColHdr  := BuscarColumnaMatriz(Destinatario);
  if (FilaHdr = nil) or (ColHdr = nil) then Exit;

  // 1) Buscar si ya existe nodo (misma fila/col) recorriendo por la lista de fila
  P := FilaHdr^.Primero;
  while P <> nil do
  begin
    if (P^.Columna = ColHdr^.Columna) then
    begin
      Inc(P^.Cantidad); // sumar 1 si ya existe
      Exit;
    end;
    P := P^.Derecha;
  end;

  // 2) Crear nuevo nodo
  New(N);
  N^.Fila := FilaHdr^.Fila;
  N^.Columna := ColHdr^.Columna;
  N^.Cantidad := 1;
  N^.RemitenteEmail := Remitente;
  N^.DestinatarioEmail := Destinatario;
  N^.Arriba := nil; N^.Abajo := nil; N^.Izquierda := nil; N^.Derecha := nil;

  // 3) Insertar en lista de la fila (ordenado por columna)
  if FilaHdr^.Primero = nil then
    FilaHdr^.Primero := N
  else
  begin
    P := FilaHdr^.Primero;
    // insertar ordenado por columna
    if N^.Columna < P^.Columna then
    begin
      N^.Derecha := P; P^.Izquierda := N;
      FilaHdr^.Primero := N;
    end
    else
    begin
      while (P^.Derecha <> nil) and (P^.Derecha^.Columna < N^.Columna) do
        P := P^.Derecha;
      N^.Derecha := P^.Derecha;
      if P^.Derecha <> nil then P^.Derecha^.Izquierda := N;
      P^.Derecha := N;
      N^.Izquierda := P;
    end;
  end;

  // 4) Insertar en lista de la columna (ordenado por fila)
  if ColHdr^.Primero = nil then
    ColHdr^.Primero := N
  else
  begin
    P := ColHdr^.Primero;
    if N^.Fila < P^.Fila then
    begin
      N^.Abajo := P; P^.Arriba := N;
      ColHdr^.Primero := N;
    end
    else
    begin
      while (P^.Abajo <> nil) and (P^.Abajo^.Fila < N^.Fila) do
        P := P^.Abajo;
      N^.Abajo := P^.Abajo;
      if P^.Abajo <> nil then P^.Abajo^.Arriba := N;
      P^.Abajo := N;
      N^.Arriba := P;
    end;
  end;
end;


function TEDDMailSystem.BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
var
  F, Ult: PMatrizDispersaFila;
  nextIdx: Integer;
begin
  // buscar existente
  F := FMatrizFilas; Ult := nil;
  while F <> nil do
  begin
    if F^.Email = Email then
    begin
      Result := F; Exit;
    end;
    Ult := F; F := F^.Siguiente;
  end;

  // crear nueva fila
  New(Result);
  Result^.Email := Email;
  Result^.Primero := nil;
  Result^.Siguiente := nil;

  // asignar índice
  if Ult = nil then
  begin
    Result^.Fila := 1;
    FMatrizFilas := Result;
  end
  else
  begin
    nextIdx := Ult^.Fila + 1;
    Result^.Fila := nextIdx;
    Ult^.Siguiente := Result;
  end;
end;

function TEDDMailSystem.BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;
var
  C, Ult: PMatrizDispersaColumna;
  nextIdx: Integer;
begin
  // buscar existente
  C := FMatrizColumnas; Ult := nil;
  while C <> nil do
  begin
    if C^.Email = Email then
    begin
      Result := C; Exit;
    end;
    Ult := C; C := C^.Siguiente;
  end;

  // crear nueva columna
  New(Result);
  Result^.Email := Email;
  Result^.Primero := nil;
  Result^.Siguiente := nil;

  // asignar índice
  if Ult = nil then
  begin
    Result^.Columna := 1;
    FMatrizColumnas := Result;
  end
  else
  begin
    nextIdx := Ult^.Columna + 1;
    Result^.Columna := nextIdx;
    Ult^.Siguiente := Result;
  end;
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
  if Usuario = nil then Exit(nil);
  Result := Usuario^.BandejaEntrada;
end;

function TEDDMailSystem.GetPapelera(Usuario: PUsuario): PCorreo;
begin
  if Usuario = nil then Exit(nil);
  Result := Usuario^.Papelera;
end;

function TEDDMailSystem.GetCorreosProgramados(Usuario: PUsuario): PCorreo;
begin
  if Usuario = nil then Exit(nil);
  Result := Usuario^.CorreosProgramados;
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
  F: PMatrizDispersaFila;
  N: PMatrizDispersaNodo;
    C: PMatrizDispersaColumna;  // ← declarar aquí

begin
  try
    ForceDirectories(RutaCarpeta);
    AssignFile(Archivo, RutaCarpeta + '/relaciones.dot');
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '  rankdir=LR;');
    WriteLn(Archivo, '  node [shape=box, style=filled, fillcolor=lightyellow];');
    WriteLn(Archivo, '  label="Relaciones Remitente → Destinatario (Matriz Dispersa)"; fontsize=16;');

    // declarar nodos por email para estética
    WriteLn(Archivo, '  subgraph cluster_remitentes { label="Remitentes"; color=lightgray;');
    F := FMatrizFilas;
    while F <> nil do
    begin
      WriteLn(Archivo, Format('  r_%d [label="%s", fillcolor=lightblue];', [F^.Fila, F^.Email]));
      F := F^.Siguiente;
    end;
    WriteLn(Archivo, '  }');

    WriteLn(Archivo, '  subgraph cluster_destinatarios { label="Destinatarios"; color=lightgray;');
    C := FMatrizColumnas;
    while C <> nil do
    begin
      WriteLn(Archivo, Format('  d_%d [label="%s", fillcolor=lightgreen];', [C^.Columna, C^.Email]));
      C := C^.Siguiente;
    end;
    WriteLn(Archivo, '  }');

    // aristas con cantidad
    F := FMatrizFilas;
    while F <> nil do
    begin
      N := F^.Primero;
      while N <> nil do
      begin
        WriteLn(Archivo, Format('  r_%d -> d_%d [label="%d"];', [N^.Fila, N^.Columna, N^.Cantidad]));
        N := N^.Derecha;
      end;
      F := F^.Siguiente;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // PNG con Graphviz
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

procedure TEDDMailSystem.GenerarNodosBST(var Archivo: TextFile; nodo: PNodoBST);
var
  NombreLimpio: String;
begin
  if nodo = nil then Exit;

  // Limpiar el nombre para usarlo como ID en Graphviz
  NombreLimpio := StringReplace(nodo^.NombreComunidad, ' ', '_', [rfReplaceAll]);
  NombreLimpio := StringReplace(NombreLimpio, '-', '_', [rfReplaceAll]);
  NombreLimpio := StringReplace(NombreLimpio, '.', '_', [rfReplaceAll]);

  // Generar el nodo actual
  WriteLn(Archivo, Format('    com_%s [label="Comunidad: %s|Creada: %s|Mensajes: %d"];',
    [NombreLimpio, nodo^.NombreComunidad, nodo^.FechaCreacion, nodo^.NumeroMensajes]));

  // Generar conexión y nodo izquierdo
  if nodo^.Izquierdo <> nil then
  begin
    WriteLn(Archivo, Format('    com_%s -> com_%s [label="<"];',
      [NombreLimpio, StringReplace(StringReplace(StringReplace(
        nodo^.Izquierdo^.NombreComunidad, ' ', '_', [rfReplaceAll]),
        '-', '_', [rfReplaceAll]), '.', '_', [rfReplaceAll])]));
    GenerarNodosBST(Archivo, nodo^.Izquierdo);
  end;

  // Generar conexión y nodo derecho
  if nodo^.Derecho <> nil then
  begin
    WriteLn(Archivo, Format('    com_%s -> com_%s [label=">"];',
      [NombreLimpio, StringReplace(StringReplace(StringReplace(
        nodo^.Derecho^.NombreComunidad, ' ', '_', [rfReplaceAll]),
        '-', '_', [rfReplaceAll]), '.', '_', [rfReplaceAll])]));
    GenerarNodosBST(Archivo, nodo^.Derecho);
  end;
end;

// 1. Implementación de GenerarNodosB
procedure TEDDMailSystem.GenerarNodosB(var Archivo: TextFile; nodo: PNodoB; nivel: Integer);
var
  i: Integer;
  NodoId: String;
begin
  if nodo = nil then Exit;

  NodoId := 'nodoB_' + IntToStr(nivel);

  // Generar el nodo con sus claves
  Write(Archivo, Format('    %s [label="', [NodoId]));
  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if i > 0 then Write(Archivo, '|');
    Write(Archivo, IntToStr(nodo^.Claves[i]));
  end;
  WriteLn(Archivo, '"];');

  // TODO: Agregar conexiones con hijos cuando se implemente completamente
end;

// 2. Implementación de GenerarReporteComunidadesBST
procedure TEDDMailSystem.GenerarReporteComunidadesBST(RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/comunidades_bst.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Árbol BST - Comunidades";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightgreen];');

    if FArbolComunidades = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin comunidades", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosBST(Archivo, FArbolComunidades);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen PNG
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
        WriteLn('Reporte BST generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte BST: ', E.Message);
  end;
end;

// 3. Implementación de GenerarReporteFavoritos
procedure TEDDMailSystem.GenerarReporteFavoritos(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  if Usuario = nil then Exit;

  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/favoritos_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Árbol B - Favoritos - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightyellow];');

    if Usuario^.ArbolFavoritos = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin favoritos", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosB(Archivo, Usuario^.ArbolFavoritos, 0);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen PNG
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
        WriteLn('Reporte favoritos generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte favoritos: ', E.Message);
  end;
end;

// 4. Implementación de ObtenerMensajesComunidad
function TEDDMailSystem.ObtenerMensajesComunidad(nombreComunidad: String): String;
var
  Comunidad: PNodoBST;
  Mensaje: PMensajeComunidad;
begin
  Result := '';
  Comunidad := BuscarComunidadBST(FArbolComunidades, nombreComunidad);

  if Comunidad = nil then
  begin
    Result := 'Comunidad no encontrada: ' + nombreComunidad;
    Exit;
  end;

  Result := 'Comunidad: ' + Comunidad^.NombreComunidad + LineEnding;
  Result := Result + 'Fecha de creación: ' + Comunidad^.FechaCreacion + LineEnding;
  Result := Result + 'Total de mensajes: ' + IntToStr(Comunidad^.NumeroMensajes) + LineEnding;
  Result := Result + '----------------------------------------' + LineEnding + LineEnding;

  Mensaje := Comunidad^.ListaMensajes;
  if Mensaje = nil then
  begin
    Result := Result + 'No hay mensajes publicados en esta comunidad.';
    Exit;
  end;

  while Mensaje <> nil do
  begin
    Result := Result + 'De: ' + Mensaje^.Correo + LineEnding;
    Result := Result + 'Fecha: ' + Mensaje^.FechaPublicacion + LineEnding;
    Result := Result + 'Mensaje: ' + Mensaje^.Mensaje + LineEnding;
    Result := Result + '----------------------------------------' + LineEnding;
    Mensaje := Mensaje^.Siguiente;
  end;
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
procedure TEDDMailSystem.GenerarReporteComunidades(RutaCarpeta: String);
var
  Archivo: TextFile;
  Comunidad: PComunidad;
  UsuarioCom, UltimoUsuario: PUsuarioComunidad;
  Process: TProcess;
  EmailLimpio: String;
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
      // 1. Crear nodos de comunidades
      Comunidad := FComunidades;
      while Comunidad <> nil do
      begin
        WriteLn(Archivo, Format('    com%d [label="Comunidad\n%s", style=filled, fillcolor=lightblue];',
          [Comunidad^.Id, Comunidad^.Nombre]));
        Comunidad := Comunidad^.Siguiente;
      end;


      WriteLn(Archivo, '    { rank=same; '); // Fuerza comunidades al mismo nivel
      Comunidad := FComunidades;
      while Comunidad <> nil do
      begin
        Write(Archivo, Format('com%d; ', [Comunidad^.Id]));
        Comunidad := Comunidad^.Siguiente;
      end;
      WriteLn(Archivo, ' }');


      Comunidad := FComunidades;
      while (Comunidad <> nil) and (Comunidad^.Siguiente <> nil) do
      begin
        WriteLn(Archivo, Format('    com%d -> com%d [color=black, constraint=false];',
          [Comunidad^.Id, Comunidad^.Siguiente^.Id]));
        Comunidad := Comunidad^.Siguiente;
      end;

      // 3. Crear usuarios y conectarlos verticalmente ABAJO de cada comunidad
      Comunidad := FComunidades;
      while Comunidad <> nil do
      begin
        UsuarioCom := Comunidad^.UsuariosList;
        UltimoUsuario := nil;

        while UsuarioCom <> nil do
        begin
          EmailLimpio := StringReplace(UsuarioCom^.Email, '@', '_', [rfReplaceAll]);
          EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);
          EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);

          WriteLn(Archivo, Format('    user_%d_%s [label="%s", style=filled, fillcolor=lightyellow];',
            [Comunidad^.Id, EmailLimpio, UsuarioCom^.Email]));

          // Conectar comunidad al PRIMER usuario
          if UltimoUsuario = nil then
          begin
            WriteLn(Archivo, Format('    com%d -> user_%d_%s [color=blue];',
              [Comunidad^.Id, Comunidad^.Id, EmailLimpio]));
          end
          else
          begin

            EmailLimpio := StringReplace(UltimoUsuario^.Email, '@', '_', [rfReplaceAll]);
            EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);
            EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);

            WriteLn(Archivo, Format('    user_%d_%s -> user_%d_%s [color=blue];',
              [Comunidad^.Id, EmailLimpio, Comunidad^.Id,
               StringReplace(StringReplace(StringReplace(UsuarioCom^.Email, '@', '_', [rfReplaceAll]), '-', '_', [rfReplaceAll]), '.', '_', [rfReplaceAll])]));
          end;

          UltimoUsuario := UsuarioCom;
          UsuarioCom := UsuarioCom^.Siguiente;
        end;

        Comunidad := Comunidad^.Siguiente;
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar PNG...
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
        WriteLn('Reporte de comunidades generado: ', RutaCarpeta, '/comunidades.png');
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

//Fase 2
// Agregar estas implementaciones en EstructurasDatos.pas

// =============== FUNCIONES PARA ÁRBOL AVL (BORRADORES) ===============
function TEDDMailSystem.ObtenerAltura(nodo: PNodoAVL): Integer;
begin
  if nodo = nil then
    Result := 0
  else
    Result := nodo^.Altura;
end;

function TEDDMailSystem.ObtenerBalance(nodo: PNodoAVL): Integer;
begin
  if nodo = nil then
    Result := 0
  else
    Result := ObtenerAltura(nodo^.Izquierdo) - ObtenerAltura(nodo^.Derecho);
end;

function TEDDMailSystem.RotarDerecha(y: PNodoAVL): PNodoAVL;
var
  x: PNodoAVL;
begin
  x := y^.Izquierdo;
  y^.Izquierdo := x^.Derecho;
  x^.Derecho := y;

  // Actualizar alturas
  y^.Altura := Max(ObtenerAltura(y^.Izquierdo), ObtenerAltura(y^.Derecho)) + 1;
  x^.Altura := Max(ObtenerAltura(x^.Izquierdo), ObtenerAltura(x^.Derecho)) + 1;

  Result := x;
end;

function TEDDMailSystem.RotarIzquierda(x: PNodoAVL): PNodoAVL;
var
  y: PNodoAVL;
begin
  y := x^.Derecho;
  x^.Derecho := y^.Izquierdo;
  y^.Izquierdo := x;

  // Actualizar alturas
  x^.Altura := Max(ObtenerAltura(x^.Izquierdo), ObtenerAltura(x^.Derecho)) + 1;
  y^.Altura := Max(ObtenerAltura(y^.Izquierdo), ObtenerAltura(y^.Derecho)) + 1;

  Result := y;
end;

function TEDDMailSystem.InsertarAVL(nodo: PNodoAVL; correo: PCorreo): PNodoAVL;
var
  balance: Integer;
begin
  // 1. Inserción normal BST
  if nodo = nil then
  begin
    New(Result);
    Result^.Correo := correo;
    Result^.Altura := 1;
    Result^.Izquierdo := nil;
    Result^.Derecho := nil;
    Exit;
  end;

  if correo^.Id < nodo^.Correo^.Id then
    nodo^.Izquierdo := InsertarAVL(nodo^.Izquierdo, correo)
  else if correo^.Id > nodo^.Correo^.Id then
    nodo^.Derecho := InsertarAVL(nodo^.Derecho, correo)
  else
  begin
    Result := nodo; // Igual, no insertar
    Exit;
  end;

  // 2. Actualizar altura
  nodo^.Altura := 1 + Max(ObtenerAltura(nodo^.Izquierdo), ObtenerAltura(nodo^.Derecho));

  // 3. Obtener balance
  balance := ObtenerBalance(nodo);

  // 4. Casos de rotación
  // Izquierda Izquierda
  if (balance > 1) and (correo^.Id < nodo^.Izquierdo^.Correo^.Id) then
  begin
    Result := RotarDerecha(nodo);
    Exit;
  end;

  // Derecha Derecha
  if (balance < -1) and (correo^.Id > nodo^.Derecho^.Correo^.Id) then
  begin
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  // Izquierda Derecha
  if (balance > 1) and (correo^.Id > nodo^.Izquierdo^.Correo^.Id) then
  begin
    nodo^.Izquierdo := RotarIzquierda(nodo^.Izquierdo);
    Result := RotarDerecha(nodo);
    Exit;
  end;

  // Derecha Izquierda
  if (balance < -1) and (correo^.Id < nodo^.Derecho^.Correo^.Id) then
  begin
    nodo^.Derecho := RotarDerecha(nodo^.Derecho);
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  Result := nodo;
end;

function TEDDMailSystem.GuardarBorrador(Usuario: PUsuario; Destinatario, Asunto, Mensaje: String): Boolean;
var
  NuevoCorreo: PCorreo;
begin
  Result := False;
  if Usuario = nil then Exit;

  NuevoCorreo := CrearCorreo(
    Usuario^.Email,
    Destinatario,
    Asunto,
    Mensaje,
    FormatDateTime('dd/mm/yy hh:nn', Now),
    False
  );

  Usuario^.ArbolBorradores := InsertarAVL(Usuario^.ArbolBorradores, NuevoCorreo);
  WriteLn('Borrador guardado para usuario: ', Usuario^.Email);
  Result := True;
end;

// =============== FUNCIONES PARA BST (COMUNIDADES) ===============
function TEDDMailSystem.InsertarBST(nodo: PNodoBST; nombreComunidad: String): PNodoBST;
begin
  if nodo = nil then
  begin
    New(Result);
    Result^.NombreComunidad := nombreComunidad;
    Result^.FechaCreacion := FormatDateTime('dd/mm/yyyy', Now);
    Result^.NumeroMensajes := 0;
    Result^.ListaMensajes := nil;
    Result^.Izquierdo := nil;
    Result^.Derecho := nil;
    Exit;
  end;

  if nombreComunidad < nodo^.NombreComunidad then
    nodo^.Izquierdo := InsertarBST(nodo^.Izquierdo, nombreComunidad)
  else if nombreComunidad > nodo^.NombreComunidad then
    nodo^.Derecho := InsertarBST(nodo^.Derecho, nombreComunidad)
  else
    Result := nodo; // Ya existe

  Result := nodo;
end;



function TEDDMailSystem.CrearComunidadBST(nombreComunidad: String): Boolean;
begin
  Result := False;
  if BuscarComunidadBST(FArbolComunidades, nombreComunidad) <> nil then
  begin
    WriteLn('Error: La comunidad ya existe');
    Exit;
  end;

  FArbolComunidades := InsertarBST(FArbolComunidades, nombreComunidad);
  WriteLn('Comunidad creada: ', nombreComunidad);
  Result := True;
end;

function TEDDMailSystem.PublicarMensajeAComunidad(nombreComunidad, correoUsuario, mensaje: String): Boolean;
var
  Comunidad: PNodoBST;
  NuevoMensaje: PMensajeComunidad;
begin
  Result := False;
  Comunidad := BuscarComunidadBST(FArbolComunidades, nombreComunidad);
  if Comunidad = nil then
  begin
    WriteLn('Error: La comunidad no existe');
    Exit;
  end;

  New(NuevoMensaje);
  NuevoMensaje^.Correo := correoUsuario;
  NuevoMensaje^.Mensaje := mensaje;
  NuevoMensaje^.FechaPublicacion := FormatDateTime('dd/mm/yyyy hh:nn', Now);
  NuevoMensaje^.Siguiente := Comunidad^.ListaMensajes;

  Comunidad^.ListaMensajes := NuevoMensaje;
  Inc(Comunidad^.NumeroMensajes);

  WriteLn('Mensaje publicado en comunidad: ', nombreComunidad);
  Result := True;
end;

// =============== FUNCIONES PARA ÁRBOL B (FAVORITOS) ===============
function TEDDMailSystem.CrearNodoB: PNodoB;
var
  i: Integer;
begin
  New(Result);
  Result^.NumClaves := 0;
  Result^.EsHoja := True;

  for i := 0 to 3 do
  begin
    Result^.Claves[i] := 0;
    Result^.Correos[i] := nil;
  end;

  for i := 0 to 4 do
    Result^.Hijos[i] := nil;
end;


function TEDDMailSystem.MarcarComoFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
var
  Correo: PCorreo;
begin
  Result := False;
  if Usuario = nil then Exit;

  // Buscar el correo en la bandeja de entrada
  Correo := BuscarCorreoEnBandeja(Usuario, CorreoId);
  if Correo = nil then
  begin
    WriteLn('Error: Correo no encontrado');
    Exit;
  end;

  // Agregar al árbol B de favoritos (implementación básica)
  Usuario^.ArbolFavoritos := InsertarB(Usuario^.ArbolFavoritos, Correo);
  WriteLn('Correo marcado como favorito: ID ', CorreoId);
  Result := True;
end;

// Función auxiliar para buscar correo en bandeja
function TEDDMailSystem.BuscarCorreoEnBandeja(Usuario: PUsuario; CorreoId: Integer): PCorreo;
var
  Correo: PCorreo;
begin
  Result := nil;
  if Usuario = nil then Exit;

  Correo := Usuario^.BandejaEntrada;
  while Correo <> nil do
  begin
    if Correo^.Id = CorreoId then
    begin
      Result := Correo;
      Exit;
    end;
    Correo := Correo^.Siguiente;
  end;
end;

// Función básica para obtener borradores (simplificada)
function TEDDMailSystem.ObtenerBorradores(Usuario: PUsuario; tipoRecorrido: String): TStringList;
begin
  Result := TStringList.Create;
  if Usuario = nil then Exit;

  // Implementar recorridos del árbol AVL
  case tipoRecorrido of
    'InOrden': RecorridoInOrdenAVL(Usuario^.ArbolBorradores, Result);
    'PreOrden': RecorridoPreOrdenAVL(Usuario^.ArbolBorradores, Result);
    'PostOrden': RecorridoPostOrdenAVL(Usuario^.ArbolBorradores, Result);
  end;
end;
// Implementación de los recorridos
procedure TEDDMailSystem.RecorridoInOrdenAVL(nodo: PNodoAVL; lista: TStringList);
var
  Display: String;
begin
  if nodo = nil then Exit;

  RecorridoInOrdenAVL(nodo^.Izquierdo, lista);

  Display := Format('[ID: %d] %s → %s | %s',
    [nodo^.Correo^.Id, nodo^.Correo^.Asunto, nodo^.Correo^.Destinatario, nodo^.Correo^.Fecha]);
  lista.AddObject(Display, TObject(PtrInt(nodo^.Correo^.Id)));

  RecorridoInOrdenAVL(nodo^.Derecho, lista);
end;

procedure TEDDMailSystem.RecorridoPreOrdenAVL(nodo: PNodoAVL; lista: TStringList);
var
  Display: String;
begin
  if nodo = nil then Exit;

  Display := Format('[ID: %d] %s → %s | %s',
    [nodo^.Correo^.Id, nodo^.Correo^.Asunto, nodo^.Correo^.Destinatario, nodo^.Correo^.Fecha]);
  lista.AddObject(Display, TObject(PtrInt(nodo^.Correo^.Id)));

  RecorridoPreOrdenAVL(nodo^.Izquierdo, lista);
  RecorridoPreOrdenAVL(nodo^.Derecho, lista);
end;

procedure TEDDMailSystem.RecorridoPostOrdenAVL(nodo: PNodoAVL; lista: TStringList);
var
  Display: String;
begin
  if nodo = nil then Exit;

  RecorridoPostOrdenAVL(nodo^.Izquierdo, lista);
  RecorridoPostOrdenAVL(nodo^.Derecho, lista);

  Display := Format('[ID: %d] %s → %s | %s',
    [nodo^.Correo^.Id, nodo^.Correo^.Asunto, nodo^.Correo^.Destinatario, nodo^.Correo^.Fecha]);
  lista.AddObject(Display, TObject(PtrInt(nodo^.Correo^.Id)));
end;



// Función básica para insertar en árbol B (simplificada)
function TEDDMailSystem.InsertarB(raiz: PNodoB; correo: PCorreo): PNodoB;
begin
  // Implementación básica - crear nodo si no existe
  if raiz = nil then
  begin
    Result := CrearNodoB;
    Result^.Claves[0] := correo^.Id;
    Result^.Correos[0] := correo;
    Result^.NumClaves := 1;
  end
  else
  begin
    // Para una implementación completa del árbol B, aquí iría
    // la lógica de inserción con splits de nodos
    Result := raiz;
    WriteLn('Insertar en árbol B: ID ', correo^.Id);
  end;
end;




// Agregar función auxiliar para buscar en AVL
function TEDDMailSystem.BuscarCorreoEnAVL(nodo: PNodoAVL; CorreoId: Integer): PCorreo;
begin
  Result := nil;
  if nodo = nil then Exit;

  if CorreoId = nodo^.Correo^.Id then
  begin
    Result := nodo^.Correo;
    Exit;
  end;

  if CorreoId < nodo^.Correo^.Id then
    Result := BuscarCorreoEnAVL(nodo^.Izquierdo, CorreoId)
  else
    Result := BuscarCorreoEnAVL(nodo^.Derecho, CorreoId);
end;


// Agregar función para generar reportes de nuevas estructuras
procedure TEDDMailSystem.GenerarReporteBorradores(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  if Usuario = nil then Exit;

  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/borradores_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Árbol AVL - Borradores - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightyellow];');

    if Usuario^.ArbolBorradores = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin borradores", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosAVL(Archivo, Usuario^.ArbolBorradores);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen
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
        WriteLn('Reporte de borradores generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de borradores: ', E.Message);
  end;
end;

// Función auxiliar para generar nodos del árbol AVL en Graphviz
procedure TEDDMailSystem.GenerarNodosAVL(var Archivo: TextFile; nodo: PNodoAVL);
begin
  if nodo = nil then Exit;

  WriteLn(Archivo, Format('    nodo_%d [label="ID: %d|Altura: %d|%s|%s"];',
    [nodo^.Correo^.Id, nodo^.Correo^.Id, nodo^.Altura,
     nodo^.Correo^.Asunto, nodo^.Correo^.Destinatario]));

  if nodo^.Izquierdo <> nil then
  begin
    WriteLn(Archivo, Format('    nodo_%d -> nodo_%d [label="L"];',
      [nodo^.Correo^.Id, nodo^.Izquierdo^.Correo^.Id]));
    GenerarNodosAVL(Archivo, nodo^.Izquierdo);
  end;

  if nodo^.Derecho <> nil then
  begin
    WriteLn(Archivo, Format('    nodo_%d -> nodo_%d [label="R"];',
      [nodo^.Correo^.Id, nodo^.Derecho^.Correo^.Id]));
    GenerarNodosAVL(Archivo, nodo^.Derecho);
  end;
end;

end.
