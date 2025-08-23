unit lista_circular;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, edd_types;

type
  PNodeContacto = ^TNodeContacto;
  TNodeContacto = record
    Data: TContacto;
    Next: PNodeContacto;
  end;

  TListaCircularContactos = record
    Tail: PNodeContacto; // tail->next = head
    Count: LongInt;
  end;

procedure LC_Init(var L: TListaCircularContactos);
procedure LC_Clear(var L: TListaCircularContactos);
procedure LC_Add(var L: TListaCircularContactos; const C: TContacto);
function  LC_RemoveByEmail(var L: TListaCircularContactos; const Email: AnsiString): Boolean;
function  LC_Find(var L: TListaCircularContactos; const Email: AnsiString): PNodeContacto;
function  LC_NextEmail(var L: TListaCircularContactos; const Email: AnsiString; out NextEmail: AnsiString): Boolean;
procedure LC_ToDot(const L: TListaCircularContactos; const FilePath: AnsiString);

implementation

procedure LC_Init(var L: TListaCircularContactos);
begin
  L.Tail := nil; L.Count := 0;
end;

procedure LC_Clear(var L: TListaCircularContactos);
var cur, head, tmp: PNodeContacto;
begin
  if L.Tail = nil then Exit;
  head := L.Tail^.Next;
  cur := head;
  repeat
    tmp := cur^.Next;
    Dispose(cur);
    cur := tmp;
  until cur = head;
  L.Tail := nil; L.Count := 0;
end;

procedure LC_Add(var L: TListaCircularContactos; const C: TContacto);
var n: PNodeContacto;
begin
  New(n); n^.Data := C;
  if L.Tail = nil then
  begin
    n^.Next := n;
    L.Tail := n;
  end
  else
  begin
    n^.Next := L.Tail^.Next; // new head
    L.Tail^.Next := n;
    L.Tail := n;
  end;
  Inc(L.Count);
end;

function LC_RemoveByEmail(var L: TListaCircularContactos; const Email: AnsiString): Boolean;
var prev, cur, head: PNodeContacto;
begin
  Result := False;
  if L.Tail = nil then Exit;
  head := L.Tail^.Next;
  prev := L.Tail; cur := head;
  repeat
    if SameText(cur^.Data.Email, Email) then
    begin
      if cur = prev then
      begin
        // only one element
        Dispose(cur); L.Tail := nil; L.Count := 0; Exit(True);
      end
      else
      begin
        prev^.Next := cur^.Next;
        if cur = L.Tail then L.Tail := prev;
        Dispose(cur); Dec(L.Count);
        Exit(True);
      end;
    end;
    prev := cur; cur := cur^.Next;
  until cur = head;
end;

function LC_Find(var L: TListaCircularContactos; const Email: AnsiString): PNodeContacto;
var cur, head: PNodeContacto;
begin
  Result := nil;
  if L.Tail = nil then Exit;
  head := L.Tail^.Next;
  cur := head;
  repeat
    if SameText(cur^.Data.Email, Email) then Exit(cur);
    cur := cur^.Next;
  until cur = head;
end;

function LC_NextEmail(var L: TListaCircularContactos; const Email: AnsiString; out NextEmail: AnsiString): Boolean;
var p: PNodeContacto;
begin
  p := LC_Find(L, Email);
  if p <> nil then
  begin
    NextEmail := p^.Next^.Data.Email; Exit(True);
  end;
  Result := False;
end;

procedure LC_ToDot(const L: TListaCircularContactos; const FilePath: AnsiString);
var f: Text; cur, head: PNodeContacto;
begin
  Assign(f, FilePath); Rewrite(f);
  Writeln(f, 'digraph Contactos {');
  Writeln(f, '  rankdir=LR; node [shape=circle];');
  if L.Tail <> nil then
  begin
    head := L.Tail^.Next;
    cur := head;
    repeat
      Writeln(f, '  n', PtrUInt(cur), ' [label="', cur^.Data.Email, '"];');
      Writeln(f, '  n', PtrUInt(cur), ' -> n', PtrUInt(cur^.Next), ';');
      cur := cur^.Next;
    until cur = head;
  end;
  Writeln(f, '}');
  Close(f);
end;

end.

