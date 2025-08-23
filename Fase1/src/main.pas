program EDDMail;
{$mode objfpc}{$H+}
uses
  SysUtils,
  edd_types in 'units/estructuras/edd_types.pas',
  lista_simple in 'units/estructuras/lista_simple.pas';

var
  usuarios: TListaUsuarios;
  ur: TUsuario;
begin
  LS_Init(usuarios);
  // Agregar usuario root
  ur.Id := 0; ur.Nombre := 'root'; ur.Usuario := 'root'; ur.Email := 'root@edd.com'; ur.Telefono := '00000000';
  LS_Append(usuarios, ur);
  Writeln('EDDMail listo. Usuarios cargados: ', usuarios.Count);
  LS_ToDot(usuarios, 'usuarios.dot');
  LS_Clear(usuarios);
end.

