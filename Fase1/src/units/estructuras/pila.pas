unit pila;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, edd_types;

type
  PNodeStackCorreo = ^TNodeStackCorreo;
  TNodeStackCorreo = record
    Data: TCorreo;
    Next: PNodeStackCorreo;
  end;

  TPilaPapelera = record
    Top: PNodeStackCorreo;
    Count: LongInt;
  end;

procedure PP_Init(var P: TPilaPapelera);
procedure PP_Clear(var P: TPilaPapelera);
procedure PP_Push(var P: TPilaPapelera; const C: TCorreo);
function  PP_Pop(var P: TPilaPapelera; out C: TCorreo): Boolean;
function  PP_Peek(const P: TPilaPapelera; out C: TCorreo): Boolean;
function  PP_BuscarPorAsunto(const P: TPilaPapelera; const Palabra: AnsiString; out C: TCorreo): Boolean;
procedure PP_ToDot(const P: TPilaPapelera; const FilePath: AnsiString);

implementation

procedure PP_Init(var P: TPilaPapelera);
begin
  P.Top := nil; P.Count := 0;
end;

procedure PP_Clear(var P: TPilaPapelera);
var cur, tmp: PNodeStackCorreo;
begin
  cur := P.Top;
  while cur <> nil do
  begin
    tmp := cur^.Next;
    Dispose(cur);
    cur := tmp;
  end;
  P.Top := nil; P.Count := 0;
end;

procedure PP_Push(var P: TPilaPapelera; const C: TCorreo);
var n: PNodeStackCorreo;
begin
  New(n); n^.Data := C; n^.Next := P.Top; P.Top := n; Inc(P.Count);
end;

function PP_Pop(var P: TPilaPapelera; out C: TCorreo): Boolean;
var n: PNodeStackCorreo;
begin
  if P.Top = nil then Exit(False);
  n := P.Top; C := n^.Data; P.Top := n^.Next; Dispose(n); Dec(P.Count); Result := True;
end;

function PP_Peek(const P: TPilaPapelera; out C: TCorreo): Boolean;
begin
  if P.Top = nil then Exit(False);
  C := P.Top^.Data; Result := True;
end;

function PP_BuscarPorAsunto(const P: TPilaPapelera; const Palabra: AnsiString; out C: TCorreo): Boolean;
var cur: PNodeStackCorreo;
begin
  Result := False;
  cur := P.Top;
  while cur <> nil do
  begin
    if Pos(LowerCase(Palabra), LowerCase(cur^.Data.Asunto)) > 0 then
    begin
      C := cur^.Data; Exit(True);
    end;
    cur := cur^.Next;
  end;
end;

procedure PP_ToDot(const P: TPilaPapelera; const FilePath: AnsiString);
var f: Text; cur: PNodeStackCorreo;
begin
  Assign(f, FilePath); Rewrite(f);
  Writeln(f, 'digraph Papelera {');
  Writeln(f, '  rankdir=TB; node [shape=box];');
  cur := P.Top;
  while cur <> nil do
  begin
    Writeln(f, '  n', PtrUInt(cur), ' [label="', cur^.Data.Asunto, '"];');
    if cur^.Next <> nil then
      Writeln(f, '  n', PtrUInt(cur), ' -> n', PtrUInt(cur^.Next), ';');
    cur := cur^.Next;
  end;
  Writeln(f, '}');
  Close(f);
end;

end.

