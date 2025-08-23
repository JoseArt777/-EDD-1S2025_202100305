unit lista_doble;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, edd_types;

type
  PNodeCorreo = ^TNodeCorreo;
  TNodeCorreo = record
    Data: TCorreo;
    Prev, Next: PNodeCorreo;
  end;

  TListaDobleCorreos = record
    Head, Tail: PNodeCorreo;
    Count: LongInt;
    CountNoLeidos: LongInt;
  end;

procedure LD_Init(var L: TListaDobleCorreos);
procedure LD_Clear(var L: TListaDobleCorreos);
procedure LD_PushBack(var L: TListaDobleCorreos; const C: TCorreo);
function  LD_MarkAsRead(var L: TListaDobleCorreos; Id: LongInt): Boolean;
function  LD_DeleteById(var L: TListaDobleCorreos; Id: LongInt; out C: TCorreo): Boolean;
procedure LD_SortByAsuntoAZ(var L: TListaDobleCorreos);
procedure LD_ToDot(const L: TListaDobleCorreos; const FilePath: AnsiString);

implementation

procedure LD_Init(var L: TListaDobleCorreos);
begin
  L.Head := nil; L.Tail := nil; L.Count := 0; L.CountNoLeidos := 0;
end;

procedure LD_Clear(var L: TListaDobleCorreos);
var cur, tmp: PNodeCorreo;
begin
  cur := L.Head;
  while cur <> nil do
  begin
    tmp := cur^.Next;
    Dispose(cur);
    cur := tmp;
  end;
  L.Head := nil; L.Tail := nil; L.Count := 0; L.CountNoLeidos := 0;
end;

procedure LD_PushBack(var L: TListaDobleCorreos; const C: TCorreo);
var n: PNodeCorreo;
begin
  New(n); n^.Data := C; n^.Prev := L.Tail; n^.Next := nil;
  if L.Tail <> nil then L.Tail^.Next := n else L.Head := n;
  L.Tail := n;
  Inc(L.Count);
  if C.Estado = ecNoLeido then Inc(L.CountNoLeidos);
end;

function LD_MarkAsRead(var L: TListaDobleCorreos; Id: LongInt): Boolean;
var cur: PNodeCorreo;
begin
  Result := False;
  cur := L.Head;
  while cur <> nil do
  begin
    if cur^.Data.Id = Id then
    begin
      if cur^.Data.Estado = ecNoLeido then
      begin
        cur^.Data.Estado := ecLeido;
        if L.CountNoLeidos > 0 then Dec(L.CountNoLeidos);
      end;
      Exit(True);
    end;
    cur := cur^.Next;
  end;
end;

function LD_DeleteById(var L: TListaDobleCorreos; Id: LongInt; out C: TCorreo): Boolean;
var cur: PNodeCorreo;
begin
  Result := False;
  cur := L.Head;
  while cur <> nil do
  begin
    if cur^.Data.Id = Id then
    begin
      C := cur^.Data;
      if cur^.Prev <> nil then cur^.Prev^.Next := cur^.Next else L.Head := cur^.Next;
      if cur^.Next <> nil then cur^.Next^.Prev := cur^.Prev else L.Tail := cur^.Prev;
      if C.Estado = ecNoLeido then if L.CountNoLeidos > 0 then Dec(L.CountNoLeidos);
      Dispose(cur);
      Dec(L.Count);
      Exit(True);
    end;
    cur := cur^.Next;
  end;
end;

procedure LD_SortByAsuntoAZ(var L: TListaDobleCorreos);
var i, swapped: Boolean; cur: PNodeCorreo; tmp: TCorreo;
begin
  if (L.Head = nil) or (L.Head = L.Tail) then Exit;
  repeat
    swapped := False;
    cur := L.Head;
    while (cur <> nil) and (cur^.Next <> nil) do
    begin
      if AnsiCompareText(cur^.Data.Asunto, cur^.Next^.Data.Asunto) > 0 then
      begin
        // swap Data (simpler than node relinking)
        tmp := cur^.Data;
        cur^.Data := cur^.Next^.Data;
        cur^.Next^.Data := tmp;
        swapped := True;
      end;
      cur := cur^.Next;
    end;
  until not swapped;
end;

procedure LD_ToDot(const L: TListaDobleCorreos; const FilePath: AnsiString);
var f: Text; cur: PNodeCorreo; estado: AnsiString;
begin
  Assign(f, FilePath); Rewrite(f);
  Writeln(f, 'digraph Bandeja {');
  Writeln(f, '  rankdir=LR; node [shape=record];');
  cur := L.Head;
  while cur <> nil do
  begin
    if cur^.Data.Estado = ecLeido then estado := 'L' else estado := 'NL';
    Writeln(f, '  c', PtrUInt(cur), ' [label="', estado, ' | ', cur^.Data.Asunto, ' | ', cur^.Data.Remitente, '"];');
    if cur^.Next <> nil then
      Writeln(f, '  c', PtrUInt(cur), ' -> c', PtrUInt(cur^.Next), ' [dir=both];');
    cur := cur^.Next;
  end;
  Writeln(f, '}');
  Close(f);
end;

end.
