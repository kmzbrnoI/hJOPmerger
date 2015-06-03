unit OblastRizeni;

//deklarace struktur Oblasti rizeni

interface

uses Types, Generics.Collections;

const
  _MAX_OR  = 256;
  _MAX_OSV = 8;

type
  //1 osvetleni
  TOsv = record
   board:Byte;
   port:Byte;
   name:string;  //max 5 znaku
  end;

 //prava
 TORRights=record
  ModCasStart:Boolean;
  ModCasStop:Boolean;
  ModCasSet:Boolean;
 end;

 //pozice symbolu OR
 TPoss=record
  DK:TPoint;
  DKOr:byte;  //orientace DK (0,1)
  Queue:TPoint;
  Time:TPoint;
 end;

 //1 oR
 TOR=record
  str:string;
  Name:string;
  ShortName:string;
  id:string;
  Lichy:Byte;
  Rights:TORRights;
  Osvetleni:TList<TOsv>;
  Poss:TPoss;
 end;

 //vsechny OR
 TORs=record
  Data:array [0.._MAX_OR-1] of TOR;
  Cnt:Integer;
 end;


implementation

end.//unit
