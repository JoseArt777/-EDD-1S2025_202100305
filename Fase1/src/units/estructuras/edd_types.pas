unit edd_types;
{$mode objfpc}{$H+}
interface
uses
  SysUtils;

type
  PUsuario = ^TUsuario;
  TUsuario = record
    Id: LongInt;
    Nombre: AnsiString;
    Usuario: AnsiString;
    Email: AnsiString;
    Telefono: AnsiString;
  end;

  TEstadoCorreo = (ecNoLeido, ecLeido);

  TCorreo = record
    Id: LongInt;
    Remitente: AnsiString;     // email del remitente
    Destinatario: AnsiString;  // email del destinatario
    Estado: TEstadoCorreo;     // L / NL
    Programado: Boolean;
    Asunto: AnsiString;
    Fecha: TDateTime;
    Mensaje: AnsiString;
  end;

  TContacto = record
    Email: AnsiString;
  end;

implementation
end.
