unit edd_types;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  // ───────────────────────────
  // Usuario
  // ───────────────────────────
  PUsuario = ^TUsuario;
  TUsuario = record
    Id: LongInt;
    Nombre: AnsiString;
    Usuario: AnsiString;
    Email: AnsiString;
    Telefono: AnsiString;
    Password: AnsiString; // útil para login
  end;

  // ───────────────────────────
  // Correo
  // ───────────────────────────
  TEstadoCorreo = (ecNoLeido, ecLeido);

  TCorreo = record
    Id: LongInt;
    Remitente: AnsiString;
    Destinatario: AnsiString;
    Estado: TEstadoCorreo;
    Programado: Boolean;
    Asunto: AnsiString;
    Fecha: TDateTime;
    Mensaje: AnsiString;
  end;

  // ───────────────────────────
  // Contacto
  // ───────────────────────────
  TContacto = record
    Email: AnsiString;
  end;

  // ───────────────────────────
  // Comunidad (para integración por grupo)
  // ───────────────────────────
  PComunidad = ^TComunidad;
  TComunidad = record
    Id: LongInt;
    Nombre: AnsiString;
    // Podrías usar aquí una lista simple de usuarios
    // o solo almacenar emails/nombres según convenga.
  end;

implementation
end.

