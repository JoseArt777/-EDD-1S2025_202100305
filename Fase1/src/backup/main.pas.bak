program EDDMail;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  // Importar todas las estructuras
  lista_simple in 'units/estructuras/lista_simple.pas',
  lista_doble in 'units/estructuras/lista_doble.pas',
  lista_circular in 'units/estructuras/lista_circular.pas',
  cola in 'units/estructuras/cola.pas',
  pila in 'units/estructuras/pila.pas',
  matriz_dispersa in 'units/estructuras/matriz_dispersa.pas';

type
  // Sistema principal que integra todas las estructuras
  TSistemaEDDMail = class
  private
    // Estructuras principales
    usuarios: TListaSimpleUsuarios;
    matriz_relaciones: TMatrizDispersa;

    // Usuario actualmente logueado
    usuario_actual: TUsuario;
    sesion_iniciada: Boolean;

    // Estructuras específicas por usuario (se cargan al hacer login)
    bandeja_entrada: TListaDobleCorreos;
    contactos: TListaCircularContactos;
    correos_programados: TColaCorreosProgramados;
    papelera: TPilaPapelera;

  public
    constructor Create;
    destructor Destroy; override;

    // Sistema de autenticación
    function IniciarSesion(email, password: string): Boolean;
    procedure CerrarSesion;
    function RegistrarUsuario(usuario: TUsuario): Boolean;

    // Menús principales
    procedure MenuPrincipal;
    procedure MenuRoot;
    procedure MenuUsuarioEstandar;

    // Funciones del Root
    procedure CargaMasiva;
    procedure GenerarReporteUsuarios;
    procedure GenerarReporteRelaciones;

    // Funciones del usuario estándar
    procedure MostrarBandejaEntrada;
    procedure EnviarCorreo;
    procedure ProgramarCorreo;
    procedure MostrarCorreosProgramados;
    procedure MostrarPapelera;
    procedure AgregarContacto;
    procedure MostrarContactos;
    procedure ActualizarPerfil;
    procedure GenerarReportesUsuario;

    // Utilidades
    procedure CargarDatosUsuario(email: string);
    procedure GuardarDatosUsuario;
    function ValidarEmail(email: string): Boolean;
    procedure MostrarMensaje(mensaje: string);
    function LeerOpcion: Integer;
    function LeerTexto(prompt: string): string;
  end;

// Variables globales
var
  sistema: TSistemaEDDMail;

constructor TSistemaEDDMail.Create;
begin
  // Inicializar estructuras principales
  usuarios := TListaSimpleUsuarios.Create;
  matriz_relaciones := TMatrizDispersa.Create;

  // Inicializar estructuras de usuario (vacías hasta login)
  bandeja_entrada := TListaDobleCorreos.Create;
  contactos := TListaCircularContactos.Create;
  correos_programados := TColaCorreosProgramados.Create;
  papelera := TPilaPapelera.Create;

  sesion_iniciada := False;

  // Crear usuario root por defecto
  var usuarioRoot: TUsuario;
  usuarioRoot.nombre := 'Administrador';
  usuarioRoot.usuario := 'root';
  usuarioRoot.email := 'root@edd.com';
  usuarioRoot.telefono := '00000000';
  usuarioRoot.password := 'root123';

  try
    usuarios.Insertar(usuarioRoot);
  except
    // El usuario root ya existe
  end;

  WriteLn('=== SISTEMA EDDMAIL INICIADO ===');
end;

destructor TSistemaEDDMail.Destroy;
begin
  usuarios.Free;
  matriz_relaciones.Free;
  bandeja_entrada.Free;
  contactos.Free;
  correos_programados.Free;
  papelera.Free;
  inherited Destroy;
end;

function TSistemaEDDMail.IniciarSesion(email, password: string): Boolean;
var
  nodoUsuario: PNodoUsuario;
begin
  Result := False;

  if usuarios.ValidarCredenciales(email, password) then
  begin
    nodoUsuario := usuarios.Buscar(email);
    if nodoUsuario <> nil then
    begin
      usuario_actual := nodoUsuario^.datos;
      sesion_iniciada := True;
      CargarDatosUsuario(email);
      Result := True;
      WriteLn('Sesión iniciada correctamente para: ', usuario_actual.nombre);
    end;
  end
  else
    WriteLn('Credenciales incorrectas');
end;

procedure TSistemaEDDMail.CerrarSesion;
begin
  if sesion_iniciada then
  begin
    GuardarDatosUsuario;
    sesion_iniciada := False;
    WriteLn('Sesión cerrada');
  end;
end;

function TSistemaEDDMail.RegistrarUsuario(usuario: TUsuario): Boolean;
begin
  Result := False;
  try
    usuarios.Insertar(usuario);
    Result := True;
    WriteLn('Usuario registrado exitosamente');
  except
    on E: Exception do
      WriteLn('Error al registrar usuario: ', E.Message);
  end;
end;

procedure TSistemaEDDMail.MenuPrincipal;
var
  opcion: Integer;
  email, password: string;
  nuevoUsuario: TUsuario;
begin
  repeat
    WriteLn;
    WriteLn('=== EDDMAIL - SISTEMA DE CORREOS ===');
    WriteLn('1. Iniciar Sesión');
    WriteLn('2. Crear Cuenta');
    WriteLn('3. Salir');
    Write('Seleccione una opción: ');
    opcion := LeerOpcion;

    case opcion of
      1: begin
        email := LeerTexto('Email: ');
        password := LeerTexto('Password: ');
        if IniciarSesion(email, password) then
        begin
          if usuario_actual.email = 'root@edd.com' then
            MenuRoot
          else
            MenuUsuarioEstandar;
        end;
      end;

      2: begin
        WriteLn('=== CREAR NUEVA CUENTA ===');
        nuevoUsuario.nombre := LeerTexto('Nombre: ');
        nuevoUsuario.usuario := LeerTexto('Usuario: ');
        nuevoUsuario.email := LeerTexto('Email: ');
        nuevoUsuario.telefono := LeerTexto('Teléfono: ');
        nuevoUsuario.password := LeerTexto('Password: ');
        RegistrarUsuario(nuevoUsuario);
      end;

      3: WriteLn('¡Gracias por usar EDDMail!');

      else
        WriteLn('Opción inválida');
    end;
  until opcion = 3;
end;

procedure TSistemaEDDMail.MenuRoot;
var
  opcion: Integer;
begin
  repeat
    WriteLn;
    WriteLn('=== MENÚ ADMINISTRADOR ===');
    WriteLn('1. Carga Masiva');
    WriteLn('2. Reporte de Usuarios');
    WriteLn('3. Reporte de Relaciones');
    WriteLn('4. Cerrar Sesión');
    Write('Seleccione una opción: ');
    opcion := LeerOpcion;

    case opcion of
      1: CargaMasiva;
      2: GenerarReporteUsuarios;
      3: GenerarReporteRelaciones;
      4: CerrarSesion;
      else
        WriteLn('Opción inválida');
    end;
  until opcion = 4;
end;

procedure TSistemaEDDMail.MenuUsuarioEstandar;
var
  opcion: Integer;
begin
  repeat
    WriteLn;
    WriteLn('=== MENÚ USUARIO: ', usuario_actual.nombre, ' ===');
    WriteLn('1. Bandeja de Entrada');
    WriteLn('2. Enviar Correo');
    WriteLn('3. Programar Correo');
    WriteLn('4. Correos Programados');
    WriteLn('5. Papelera');
    WriteLn('6. Agregar Contacto');
    WriteLn('7. Ver Contactos');
    WriteLn('8. Actualizar Perfil');
    WriteLn('9. Generar Reportes');
    WriteLn('10. Cerrar Sesión');
    Write('Seleccione una opción: ');
    opcion := LeerOpcion;

    case opcion of
      1: MostrarBandejaEntrada;
      2: EnviarCorreo;
      3: ProgramarCorreo;
      4: MostrarCorreosProgramados;
      5: MostrarPapelera;
      6: AgregarContacto;
      7: MostrarContactos;
      8: ActualizarPerfil;
      9: GenerarReportesUsuario;
      10: CerrarSesion;
      else
        WriteLn('Opción inválida');
    end;
  until opcion = 10;
end;

// Implementar todas las funciones restantes...

procedure TSistemaEDDMail.CargaMasiva;
begin
  WriteLn('=== CARGA MASIVA ===');
  WriteLn('Función en desarrollo...');
  // TODO: Implementar carga desde JSON
end;

procedure TSistemaEDDMail.GenerarReporteUsuarios;
var
  reporte: string;
  archivo: TextFile;
begin
  WriteLn('=== GENERANDO REPORTE DE USUARIOS ===');
  reporte := usuarios.GenerarReporte;

  // Crear directorio si no existe
  if not DirectoryExists('Root-Reportes') then
    CreateDir('Root-Reportes');

  // Guardar reporte
  AssignFile(archivo, 'Root-Reportes/reporte_usuarios.dot');
  Rewrite(archivo);
  WriteLn(archivo, reporte);
  CloseFile(archivo);

  WriteLn('Reporte guardado en Root-Reportes/reporte_usuarios.dot');
end;

procedure TSistemaEDDMail.GenerarReporteRelaciones;
var
  reporte: string;
  archivo: TextFile;
begin
  WriteLn('=== GENERANDO REPORTE DE RELACIONES ===');
  reporte := matriz_relaciones.GenerarReporteGraphviz;

  if not DirectoryExists('Root-Reportes') then
    CreateDir('Root-Reportes');

  AssignFile(archivo, 'Root-Reportes/reporte_relaciones.dot');
  Rewrite(archivo);
  WriteLn(archivo, reporte);
  CloseFile(archivo);

  WriteLn('Reporte guardado en Root-Reportes/reporte_relaciones.dot');
end;

procedure TSistemaEDDMail.MostrarBandejaEntrada;
begin
  WriteLn('=== BANDEJA DE ENTRADA ===');
  if bandeja_entrada.EstaVacia then
    WriteLn('No hay correos en la bandeja de entrada')
  else
  begin
    bandeja_entrada.MostrarTodos;
    WriteLn('Correos no leídos: ', bandeja_entrada.ContarNoLeidos);
  end;
end;

procedure TSistemaEDDMail.EnviarCorreo;
var
  destinatario, asunto, mensaje: string;
  correo: TCorreo;
begin
  WriteLn('=== ENVIAR CORREO ===');
  destinatario := LeerTexto('Para: ');

  // Verificar que el destinatario esté en contactos
  if not contactos.ExisteContacto(destinatario) then
  begin
    WriteLn('Error: El destinatario no está en sus contactos');
    Exit;
  end;

  asunto := LeerTexto('Asunto: ');
  mensaje := LeerTexto('Mensaje: ');

  // Crear correo
  correo.remitente := usuario_actual.email;
  correo.destinatario := destinatario;
  correo.asunto := asunto;
  correo.mensaje := mensaje;
  correo.fecha := Now;
  correo.estado := ecNoLeido;
  correo.programado := False;

  // TODO: Agregar a la bandeja del destinatario
  // Por ahora solo registramos la relación en la matriz
  matriz_relaciones.AgregarRelacion(usuario_actual.email, destinatario);

  WriteLn('Correo enviado exitosamente');
end;

// Continuar implementando el resto de funciones...

procedure TSistemaEDDMail.CargarDatosUsuario(email: string);
begin
  // TODO: Cargar datos específicos del usuario desde archivos
  WriteLn('Cargando datos del usuario: ', email);
end;

procedure TSistemaEDDMail.GuardarDatosUsuario;
begin
  // TODO: Guardar datos del usuario actual
  WriteLn('Guardando datos del usuario');
end;

function TSistemaEDDMail.ValidarEmail(email: string): Boolean;
begin
  Result := (Length(email) > 0) and (Pos('@', email) > 0);
end;

procedure TSistemaEDDMail.MostrarMensaje(mensaje: string);
begin
  WriteLn('>>> ', mensaje);
end;

function TSistemaEDDMail.LeerOpcion: Integer;
var
  input: string;
begin
  ReadLn(input);
  Result := StrToIntDef(input, 0);
end;

function TSistemaEDDMail.LeerTexto(prompt: string): string;
begin
  Write(prompt);
  ReadLn(Result);
end;

// Implementar funciones restantes...
procedure TSistemaEDDMail.ProgramarCorreo;
begin
  WriteLn('Función ProgramarCorreo - En desarrollo');
end;

procedure TSistemaEDDMail.MostrarCorreosProgramados;
begin
  WriteLn('Función MostrarCorreosProgramados - En desarrollo');
end;

procedure TSistemaEDDMail.MostrarPapelera;
begin
  WriteLn('Función MostrarPapelera - En desarrollo');
end;

procedure TSistemaEDDMail.AgregarContacto;
begin
  WriteLn('Función AgregarContacto - En desarrollo');
end;

procedure TSistemaEDDMail.MostrarContactos;
begin
  WriteLn('Función MostrarContactos - En desarrollo');
end;

procedure TSistemaEDDMail.ActualizarPerfil;
begin
  WriteLn('Función ActualizarPerfil - En desarrollo');
end;

procedure TSistemaEDDMail.GenerarReportesUsuario;
begin
  WriteLn('Función GenerarReportesUsuario - En desarrollo');
end;

// PROGRAMA PRINCIPAL
begin
  sistema := TSistemaEDDMail.Create;
  try
    sistema.MenuPrincipal;
  finally
    sistema.Free;
  end;
end.

