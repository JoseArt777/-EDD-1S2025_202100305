program test_estructuras;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils,
  lista_simple in 'units/estructuras/lista_simple.pas',
  lista_doble in 'units/estructuras/lista_doble.pas',
  lista_circular in 'units/estructuras/lista_circular.pas',
  cola in 'units/estructuras/cola.pas',
  pila in 'units/estructuras/pila.pas',
  matriz_dispersa in 'units/estructuras/matriz_dispersa.pas';

// Variables globales para testing
var
  usuarios: TListaSimpleUsuarios;
  correos: TListaDobleCorreos;
  contactos: TListaCircularContactos;
  programados: TColaCorreosProgramados;
  papelera: TPilaPapelera;
  matriz: TMatrizDispersa;

procedure MostrarSeparador(titulo: string);
begin
  WriteLn;
  WriteLn('================================');
  WriteLn('=== ', titulo, ' ===');
  WriteLn('================================');
end;

procedure TestListaSimpleUsuarios;
var
  usuario1, usuario2, usuario3: TUsuario;
  nodo: PNodoUsuario;
begin
  MostrarSeparador('TESTING LISTA SIMPLE - USUARIOS');
  
  usuarios := TListaSimpleUsuarios.Create;
  
  // Crear usuarios de prueba
  usuario1.nombre := 'Juan Pérez';
  usuario1.usuario := 'jperez';
  usuario1.email := 'juan.perez@edd.com';
  usuario1.telefono := '12345678';
  usuario1.password := 'juan123';
  
  usuario2.nombre := 'María García';
  usuario2.usuario := 'mgarcia';
  usuario2.email := 'maria.garcia@edd.com';
  usuario2.telefono := '87654321';
  usuario2.password := 'maria123';
  
  usuario3.nombre := 'Pedro López';
  usuario3.usuario := 'plopez';
  usuario3.email := 'pedro.lopez@edd.com';
  usuario3.telefono := '11223344';
  usuario3.password := 'pedro123';
  
  // Insertar usuarios
  WriteLn('Insertando usuarios...');
  try
    usuarios.Insertar(usuario1);
    usuarios.Insertar(usuario2);
    usuarios.Insertar(usuario3);
    WriteLn('✓ Usuarios insertados correctamente');
  except
    on E: Exception do
      WriteLn('✗ Error insertando usuarios: ', E.Message);
  end;
  
  // Mostrar todos los usuarios
  WriteLn;
  usuarios.Mostrar;
  
  // Probar búsquedas
  WriteLn;
  WriteLn('=== PRUEBAS DE BÚSQUEDA ===');
  
  nodo := usuarios.Buscar('juan.perez@edd.com');
  if nodo <> nil then
    WriteLn('✓ Usuario encontrado: ', nodo^.datos.nombre)
  else
    WriteLn('✗ Usuario no encontrado');
  
  // Probar validación de credenciales
  if usuarios.ValidarCredenciales('maria.garcia@edd.com', 'maria123') then
    WriteLn('✓ Credenciales válidas para María')
  else
    WriteLn('✗ Credenciales inválidas');
  
  if usuarios.ValidarCredenciales('pedro.lopez@edd.com', 'password_malo') then
    WriteLn('✗ Error en validación - debería ser inválida')
  else
    WriteLn('✓ Validación correcta - credenciales inválidas detectadas');
  
  // Probar duplicados
  WriteLn;
  WriteLn('=== PRUEBA DE DUPLICADOS ===');
  try
    usuarios.Insertar(usuario1); // Intentar insertar duplicado
    WriteLn('✗ Error - permitió duplicado');
  except
    WriteLn('✓ Duplicado rechazado correctamente');
  end;
  
  WriteLn('Total de usuarios: ', usuarios.ObtenerCantidad);
end;

procedure TestListaDobleCorreos;
var
  correo1, correo2, correo3: TCorreo;
begin
  MostrarSeparador('TESTING LISTA DOBLE - CORREOS');
  
  correos := TListaDobleCorreos.Create;
  
  // Crear correos de prueba
  correo1.remitente := 'juan.perez@edd.com';
  correo1.destinatario := 'maria.garcia@edd.com';
  correo1.asunto := 'Reunión de proyecto';
  correo1.mensaje := 'Hola María, necesitamos coordinar la reunión del viernes.';
  correo1.fecha := Now - 2;
  correo1.estado := ecNoLeido;
  
  correo2.remitente := 'pedro.lopez@edd.com';
  correo2.destinatario := 'maria.garcia@edd.com';
  correo2.asunto := 'Documentos urgentes';
  correo2.mensaje := 'Por favor revisa los documentos adjuntos.';
  correo2.fecha := Now - 1;
  correo2.estado := ecNoLeido;
  
  correo3.remitente := 'admin@edd.com';
  correo3.destinatario := 'maria.garcia@edd.com';
  correo3.asunto := 'Bienvenida al sistema';
  correo3.mensaje := 'Bienvenida a EDDMail!';
  correo3.fecha := Now;
  correo3.estado := ecLeido;
  
  // Insertar correos
  WriteLn('Insertando correos...');
  correos.InsertarOrdenado(correo1);
  correos.InsertarOrdenado(correo2);
  correos.InsertarOrdenado(correo3);
  WriteLn('✓ ', correos.ObtenerCantidad, ' correos insertados');
  
  // Mostrar bandeja
  WriteLn;
  correos.MostrarTodos;
  
  // Probar funciones específicas
  WriteLn;
  WriteLn('=== ESTADÍSTICAS ===');
  WriteLn('Correos no leídos: ', correos.ContarNoLeidos);
  
  // Marcar como leído
  if correos.ObtenerCantidad > 0 then
  begin
    correos.MarcarComoLeido(1);
    WriteLn('✓ Correo marcado como leído');
    WriteLn('Correos no leídos después: ', correos.ContarNoLeidos);
  end;
  
  // Probar ordenamiento
  WriteLn;
  WriteLn('=== ORDENAMIENTO POR ASUNTO ===');
  correos.OrdenarPorAsunto;
  correos.MostrarTodos;
end;

procedure TestListaCircularContactos;
var
  contacto1, contacto2, contacto3: TContacto;
  i: Integer;
begin
  MostrarSeparador('TESTING LISTA CIRCULAR - CONTACTOS');
  
  contactos := TListaCircularContactos.Create;
  
  // Crear contactos de prueba
  contacto1.nombre := 'Ana Rodríguez';
  contacto1.usuario := 'arodriguez';
  contacto1.email := 'ana.rodriguez@edd.com';
  contacto1.telefono := '55555555';
  
  contacto2.nombre := 'Carlos Mendoza';
  contacto2.usuario := 'cmendoza';
  contacto2.email := 'carlos.mendoza@edd.com';
  contacto2.telefono := '66666666';
  
  contacto3.nombre := 'Elena Vargas';
  contacto3.usuario := 'evargas';
  contacto3.email := 'elena.vargas@edd.com';
  contacto3.telefono := '77777777';
  
  // Agregar contactos
  WriteLn('Agregando contactos...');
  try
    contactos.Agregar(contacto1);
    contactos.Agregar(contacto2);
    contactos.Agregar(contacto3);
    WriteLn('✓ ', contactos.ObtenerCantidad, ' contactos agregados');
  except
    on E: Exception do
      WriteLn('✗ Error agregando contactos: ', E.Message);
  end;
  
  // Mostrar contactos
  WriteLn;
  contactos.MostrarTodos;
  
  // Probar navegación circular
  WriteLn;
  WriteLn('=== NAVEGACIÓN CIRCULAR ===');
  WriteLn('Navegando 5 pasos desde el contacto actual:');
  
  for i := 1 to 5 do
  begin
    contactos.SiguienteContacto;
    WriteLn('Paso ', i, ': ', contactos.ContactoActual^.datos.nombre);
  end;
  
  // Probar búsqueda
  WriteLn;
  WriteLn('=== BÚSQUEDAS ===');
  if contactos.ExisteContacto('carlos.mendoza@edd.com') then
    WriteLn('✓ Contacto Carlos encontrado')
  else
    WriteLn('✗ Contacto Carlos no encontrado');
end;

procedure TestColaCorreosProgramados;
var
  correo1, correo2, correo3: TCorreoProgramado;
  correosListos: TArray<TCorreoProgramado>;
  i: Integer;
begin
  MostrarSeparador('TESTING COLA - CORREOS PROGRAMADOS');
  
  programados := TColaCorreosProgramados.Create;
  
  // Crear correos programados
  correo1.remitente := 'sistema@edd.com';
  correo1.destinatario := 'juan.perez@edd.com';
  correo1.asunto := 'Recordatorio reunión';
  correo1.mensaje := 'Recuerda la reunión de mañana a las 10 AM.';
  correo1.fechaProgramada := Now + (1/24); // En 1 hora
  
  correo2.remitente := 'sistema@edd.com';
  correo2.destinatario := 'maria.garcia@edd.com';
  correo2.asunto := 'Reporte mensual';
  correo2.mensaje := 'Es hora de generar el reporte mensual.';
  correo2.fechaProgramada := Now + (2/24); // En 2 horas
  
  correo3.remitente := 'sistema@edd.com';
  correo3.destinatario := 'pedro.lopez@edd.com';
  correo3.asunto := 'Backup automático';
  correo3.mensaje := 'Iniciando backup automático del sistema.';
  correo3.fechaProgramada := Now - (1/24); // Hace 1 hora (debería estar listo)
  
  // Encollar correos
  WriteLn('Encolando correos programados...');
  try
    programados.Encollar(correo1);
    programados.Encollar(correo2);
    programados.Encollar(correo3);
    WriteLn('✓ ', programados.ObtenerCantidad, ' correos encolados');
  except
    on E: Exception do
      WriteLn('✗ Error encolando correos: ', E.Message);
  end;
  
  // Mostrar cola
  WriteLn;
  programados.MostrarTodos;
  
  // Procesar correos listos
  WriteLn;
  WriteLn('=== PROCESANDO CORREOS LISTOS ===');
  correosListos := programados.ProcesarCorreosListos;
  WriteLn('Correos listos para envío: ', Length(correosListos));
  
  for i := 0 to Length(correosListos) - 1 do
  begin
    WriteLn('- ', correosListos[i].asunto, ' para ', correosListos[i].destinatario);
  end;
  
  WriteLn('Correos restantes en cola: ', programados.ObtenerCantidad);
  
  // Mostrar estadísticas
  WriteLn;
  programados.MostrarEstadisticas;
end;

procedure TestPilaPapelera;
var
  correoElim1, correoElim2: TCorreoEliminado;
  correoRestaurado: TCorreoEliminado;
begin
  MostrarSeparador('TESTING PILA - PAPELERA');
  
  papelera := TPilaPapelera.Create;
  
  // Crear correos eliminados
  correoElim1.idOriginal := 1;
  correoElim1.remitente := 'spam@ejemplo.com';
  correoElim1.destinatario := 'usuario@edd.com';
  correoElim1.asunto := 'Oferta especial!!!';
  correoElim1.mensaje := 'Compra ahora y recibe descuentos increíbles...';
  correoElim1.fechaOriginal := Now - 5;
  correoElim1.motivoEliminacion := 'Correo spam';
  
  correoElim2.idOriginal := 2;
  correoElim2.remitente := 'marketing@empresa.com';
  correoElim2.destinatario := 'usuario@edd.com';
  correoElim2.asunto := 'Newsletter semanal';
  correoElim2.mensaje := 'Últimas noticias de nuestra empresa...';
  correoElim2.fechaOriginal := Now - 2;
  correoElim2.motivoEliminacion := 'No deseado';
  
  // Apilar correos
  WriteLn('Apilando correos eliminados...');
  try
    papelera.Apilar(correoElim1);
    papelera.Apilar(correoElim2);
    WriteLn('✓ ', papelera.ObtenerCantidad, ' correos en papelera');
  except
    on E: Exception do
      WriteLn('✗ Error apilando correos: ', E.Message);
  end;
  
  // Mostrar papelera
  WriteLn;
  papelera.MostrarTodos;
  
  // Mostrar estadísticas
  WriteLn;
  papelera.MostrarEstadisticas;
  
  // Probar restauración
  WriteLn;
  WriteLn('=== PROBANDO RESTAURACIÓN ===');
  if papelera.ObtenerCantidad > 0 then
  begin
    try
      correoRestaurado := papelera.RestaurarCorreo(papelera.Tope.id);
      WriteLn('✓ Correo restaurado: ', correoRestaurado.asunto);
      WriteLn('Correos restantes en papelera: ', papelera.ObtenerCantidad);
    except
      on E: Exception do
        WriteLn('✗ Error restaurando: ', E.Message);
    end;
  end;
end;

procedure TestMatrizDispersa;
var
  usuarios_matriz: TArray<string>;
  i, j: Integer;
  estadisticas: TArray<TEstadisticaUsuario>;
begin
  MostrarSeparador('TESTING MATRIZ DISPERSA - RELACIONES');
  
  matriz := TMatrizDispersa.Create;
  
  // Agregar algunas relaciones de prueba
  WriteLn('Agregando relaciones de correos...');
  try
    // Juan envía correos
    matriz.AgregarRelacion('juan.perez@edd.com', 'maria.garcia@edd.com');
    matriz.AgregarRelacion('juan.perez@edd.com', 'maria.garcia@edd.com'); // +1
    matriz.AgregarRelacion('juan.perez@edd.com', 'pedro.lopez@edd.com');
    
    // María envía correos
    matriz.AgregarRelacion('maria.garcia@edd.com', 'juan.perez@edd.com');
    matriz.AgregarRelacion('maria.garcia@edd.com', 'pedro.lopez@edd.com');
    matriz.AgregarRelacion('maria.garcia@edd.com', 'pedro.lopez@edd.com'); // +1
    matriz.AgregarRelacion('maria.garcia@edd.com', 'pedro.lopez@edd.com'); // +1
    
    // Pedro envía correos
    matriz.AgregarRelacion('pedro.lopez@edd.com', 'juan.perez@edd.com');
    matriz.AgregarRelacion('pedro.lopez@edd.com', 'maria.garcia@edd.com');
    
    WriteLn('✓ Relaciones agregadas correctamente');
  except
    on E: Exception do
      WriteLn('✗ Error agregando relaciones: ', E.Message);
  end;
  
  // Mostrar matriz
  WriteLn;
  matriz.MostrarMatriz;
  
  // Mostrar estadísticas
  WriteLn;
  matriz.MostrarEstadisticas;
  
  // Probar consultas específicas
  WriteLn;
  WriteLn('=== CONSULTAS ESPECÍFICAS ===');
  WriteLn('Correos de Juan a María: ', matriz.ObtenerCantidadCorreos('juan.perez@edd.com', 'maria.garcia@edd.com'));
  WriteLn('Correos de María a Pedro: ', matriz.ObtenerCantidadCorreos('maria.garcia@edd.com', 'pedro.lopez@edd.com'));
  WriteLn('Total correos enviados por Juan: ', matriz.ObtenerCorreosEnviados('juan.perez@edd.com'));
  WriteLn('Total correos recibidos por Pedro: ', matriz.ObtenerCorreosRecibidos('pedro.lopez@edd.com'));
  WriteLn('Usuario más activo: ', matriz.ObtenerUsuarioMasActivo);
  
  // Top remitentes
  WriteLn;
  WriteLn('=== TOP REMITENTES ===');
  estadisticas := matriz.ObtenerTopRemitentesByEnviados(3);
  for i := 0 to Length(estadisticas) - 1 do
  begin
    WriteLn((i+1), '. ', estadisticas[i].email, ' - ', estadisticas[i].enviados, ' enviados');
  end;
end;

procedure GenerarReportes;
var
  reporte: string;
  archivo: TextFile;
begin
  MostrarSeparador('GENERANDO REPORTES DE PRUEBA');
  
  // Crear directorio de reportes
  if not DirectoryExists('Test-Reportes') then
  begin
    CreateDir('Test-Reportes');
    WriteLn('✓ Directorio Test-Reportes creado');
  end;
  
  // Reporte de usuarios
  if usuarios <> nil then
  begin
    reporte := usuarios.GenerarReporte;
    AssignFile(archivo, 'Test-Reportes/usuarios.dot');
    Rewrite(archivo);
    WriteLn(archivo, reporte);
    CloseFile(archivo);
    WriteLn('✓ Reporte usuarios.dot generado');
  end;
  
  // Reporte de correos
  if correos <> nil then
  begin
    reporte := correos.GenerarReporte;
    AssignFile(archivo, 'Test-Reportes/correos.dot');
    Rewrite(archivo);
    WriteLn(archivo, reporte);
    CloseFile(archivo);
    WriteLn('✓ Reporte correos.dot generado');
  end;
  
  // Reporte de contactos
  if contactos <> nil then
  begin
    reporte := contactos.GenerarReporte;
    AssignFile(archivo, 'Test-Reportes/contactos.dot');
    Rewrite(archivo);
    WriteLn(archivo, reporte);
    CloseFile(archivo);
    WriteLn('✓ Reporte contactos.dot generado');
  end;
  
  // Reporte de cola
  if programados <> nil then
  begin
    reporte := programados.GenerarReporte;
    AssignFile(archivo, 'Test-Reportes/programados.dot');
    Rewrite(archivo);
    WriteLn(archivo, reporte);
    CloseFile(archivo);
    WriteLn('✓ Reporte programados.dot generado');
  end;
  
  // Reporte de pila
  if papelera <> nil then
  begin
    reporte := papelera.GenerarReporte;
    AssignFile(archivo, 'Test-Reportes/papelera.dot');
    Rewrite(archivo);
    WriteLn(archivo, reporte);
    CloseFile(archivo);
    WriteLn('✓ Reporte papelera.dot generado');
  end;
  
  // Reporte de matriz
  if matriz <> nil then
  begin
    reporte := matriz.GenerarReporteGraphviz;
    AssignFile(archivo, 'Test-Reportes/matriz.dot');
    Rewrite(archivo);
    WriteLn(archivo, reporte);
    CloseFile(archivo);
    WriteLn('✓ Reporte matriz.dot generado');
  end;
  
  WriteLn;
  WriteLn('Para visualizar los reportes:');
  WriteLn('dot -Tpng Test-Reportes/usuarios.dot -o Test-Reportes/usuarios.png');
  WriteLn('dot -Tpng Test-Reportes/matriz.dot -o Test-Reportes/matriz.png');
  WriteLn('(Y así para todos los demás archivos .dot)');
end;

procedure LiberarMemoria;
begin
  MostrarSeparador('LIBERANDO MEMORIA');
  
  if usuarios <> nil then
  begin
    usuarios.Free;
    WriteLn('✓ Lista de usuarios liberada');
  end;
  
  if correos <> nil then
  begin
    correos.Free;
    WriteLn('✓ Lista de correos liberada');
  end;
  
  if contactos <> nil then
  begin
    contactos.Free;
    WriteLn('✓ Lista circular liberada');
  end;
  
  if programados <> nil then
  begin
    programados.Free;
    WriteLn('✓ Cola liberada');
  end;
  
  if papelera <> nil then
  begin
    papelera.Free;
    WriteLn('✓ Pila liberada');
  end;
  
  if matriz <> nil then
  begin
    matriz.Free;
    WriteLn('✓ Matriz dispersa liberada');
  end;
end;

// PROGRAMA PRINCIPAL DE TESTING
begin
  WriteLn('==============================================');
  WriteLn('===     TESTING ESTRUCTURAS DE DATOS      ===');
  WriteLn('===           PROYECTO EDDMAIL             ===');
  WriteLn('==============================================');
  
  try
    // Ejecutar todas las pruebas
    TestListaSimpleUsuarios;
    TestListaDobleCorreos;
    TestListaCircularContactos;
    TestColaCorreosProgramados;
    TestPilaPapelera;
    TestMatrizDispersa;
    
    // Generar reportes
    GenerarReportes;
    
    // Liberar memoria
    LiberarMemoria;
    
    WriteLn;
    WriteLn('==============================================');
    WriteLn('===        TESTING COMPLETADO ✓           ===');
    WriteLn('==============================================');
    
  except
    on E: Exception do
    begin
      WriteLn('ERROR CRÍTICO: ', E.Message);
      LiberarMemoria;
    end;
  end;
  
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
