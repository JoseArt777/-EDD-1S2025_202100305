unit lista_simple;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, edd_types;

type
  PNodeUsuario = ^TNodeUsuario;
  TNodeUsuario = record
    Data: TUsuario;
    Next: PNodeUsuario;
  end;

  TListaUsuarios = record
    Head: PNodeUsuario;
    Count: LongInt;
  end;

procedure LS_Init(var L: TListaUsuarios);
procedure LS_Clear(var L: TListaUsuarios);
procedure LS_Append(var L: TListaUsuarios; const U: TUsuario);
function LS_FindByEmail(var L: TListaUsuarios; const Email: AnsiString): PNodeUsuario;
function LS_UpdateByEmail(var L: TListaUsuarios; const Email: AnsiString; const NuevoUsuario: TUsuario): Boolean;
function LS_DeleteByEmail(var L: TListaUsuarios; const Email: AnsiString; out Eliminado: TUsuario): Boolean;
procedure LS_ToDot(const L: TListaUsuarios; const FilePath: AnsiString);

implementation

procedure LS_Init(var L: TListaUsuarios);
begin
  L.Head := nil; L.Count := 0;
end;

procedure LS_Clear(var L: TListaUsuarios);
var cur, tmp: PNodeUsuario;
begin
  cur := L.Head;
  while cur <> nil do
  begin
    tmp := cur^.Next;
    Dispose(cur);
    cur := tmp;
  end;
  L.Head := nil; L.Count := 0;
end;

procedure LS_Append(var L: TListaUsuarios; const U: TUsuario);
var n, cur: PNodeUsuario;
begin
  New(n);
  n^.Data := U;
  n^.Next := nil;
  if L.Head = nil then
    L.Head := n
  else
  begin
    cur := L.Head;
    while cur^.Next <> nil do cur := cur^.Next;
    cur^.Next := n;
  end;
  Inc(L.Count);
end;

function LS_FindByEmail(var L: TListaUsuarios; const Email: AnsiString): PNodeUsuario;
var cur: PNodeUsuario;
begin
  cur := L.Head;
  while cur <> nil do
  begin
    if SameText(cur^.Data.Email, Email) then
    begin
      Result := cur; Exit;
    end;
    cur := cur^.Next;
  end;
  Result := nil;
end;

function LS_UpdateByEmail(var L: TListaUsuarios; const Email: AnsiString; const NuevoUsuario: TUsuario): Boolean;
var p: PNodeUsuario;
begin
  p := LS_FindByEmail(L, Email);
  if p <> nil then
  begin
    p^.Data := NuevoUsuario; Result := True;
  end
  else Result := False;
end;

function LS_DeleteByEmail(var L: TListaUsuarios; const Email: AnsiString; out Eliminado: TUsuario): Boolean;
var cur, prev: PNodeUsuario;
begin
  Result := False;
  prev := nil; cur := L.Head;
  while cur <> nil do
  begin
    if SameText(cur^.Data.Email, Email) then
    begin
      Eliminado := cur^.Data;
      if prev = nil then L.Head := cur^.Next else prev^.Next := cur^.Next;
      Dispose(cur); Dec(L.Count);
      Exit(True);
    end;
    prev := cur; cur := cur^.Next;
  end;
end;

procedure LS_ToDot(const L: TListaUsuarios; const FilePath: AnsiString);
var f: Text; cur: PNodeUsuario;
begin
  Assign(f, FilePath); Rewrite(f);
  Writeln(f, 'digraph Usuarios {');
  Writeln(f, '  rankdir=LR; node [shape=record];');
  cur := L.Head;
  while cur <> nil do
  begin
    Writeln(f, '  u', PtrUInt(cur), ' [label="', cur^.Data.Nombre, '\n', cur^.Data.Email, '"];');
    if cur^.Next <> nil then
      Writeln(f, '  u', PtrUInt(cur), ' -> u', PtrUInt(cur^.Next), ';');
    cur := cur^.Next;
  end;
  Writeln(f, '}');
  Close(f);
end;

end.

