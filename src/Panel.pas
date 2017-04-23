unit Panel;

//tato unita resi veskerou hlavni technologii programu - prace se slucovanim panelu
//tato unita z velke casti prebira kod ReliefObjects.pas z Editoru

//princip funkce:
//  nacist vice souboru *.opnl, vybrat z nich potrebana data pro server a tato data ulozit do souboru *.spnl

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, IniFiles,
  Forms, OblastRizeni, Generics.Collections;

const
 _MAX_SYMBOLS  = 256;
 _MAX_TECH_REL = 256;
 _MAX_PRJ_LEN  = 10;

 _FileVersion = '1.1';

type

////////////////////////////////////////////////////////////////////////////////

TBlkType = (usek, navestidlo, vyhybka, prejezd, popisek, uvazka, uvazka_spr, zamek, rozp);

// abstraktni trida, ze ktere se dedi graficke bloky
TGraphBlok = class
  OblRizeni:Integer;
end;

// graficky blok vyhybka
TVyhybka = class(TGraphBlok)
 PolohaPlus:Byte;
 Position:TPoint;
 SymbolID:Integer;
 obj:integer;                 // technologicke id useku, na kterem vyhybka je
end;//Navestidlo

  // patri useku
  TReliefSym=record
   Position:TPoint;
   SymbolID:Integer;
  end;

// graficky blok usek
TUsek = class(TGraphBlok)
 Symbols:record
   Data:array[0.._MAX_SYMBOLS] of TReliefSym;
   Count:Byte;
  end;//Symbols
  cislo_koleje:string;
  // zbytek neni potreba
end;

// graficky blok navestidlo
TNavestidlo = class(TGraphBlok)
 Blok:Integer;
 Position:TPoint;
 SymbolID:Integer;
 OblRizeni:Integer;

 UsekPred:Integer;    //technologicky usek
end;//Navestidlo

  // patri prejezdu
  TBlikPoint = record
   Pos:TPoint;
   TechUsek:Integer;     // jaky technologicky usek ma tato cast prejezdu
  end;

// graficky blok prejezd
TPrejezd = class(TGraphBlok)
 Blok:Integer;

 StaticPositions: record
  data:array [0.._MAX_PRJ_LEN] of TPoint;
  Count:Byte;
 end;

 BlikPositions: record
  data:array [0.._MAX_PRJ_LEN] of TBlikPoint;
  Count:Byte;
 end;

 OblRizeni:Integer;
end;//Navestidlo

// popisek
TPopisek = class(TGraphBlok)
 Text:string;
 Position:TPoint;
 Color:Integer;
end;//Text

// uvazka
TUvazka = class(TGraphBlok)
  Pos:TPoint;
  defalt_dir:Integer;
  OblRizeni:Integer;
end;

TUvazkaSprVertDir = (top = 0, bottom = 1);

//uvazka spr
TUvazkaSpr = class(TGraphBlok)
  Pos:TPoint;
  vertical_dir:TUvazkaSprVertDir;
  spr_cnt:Integer;
  OblRizeni:Integer;
end;

// zamek
TZamek = class(TGraphBlok)
 Pos:TPoint;
end;

// zamek
TRozp = class(TGraphBlok)
 Pos:TPoint;
end;

////////////////////////////////////////////////////////////////////////////////

// technologicky blok
// obsahuje reference na graficke bloky
TTechBlok = class
   typ:TBlkType;
   id:Integer;

   graph_blk:record
     data:array [0.._MAX_TECH_REL-1] of TGraphBlok;
     cnt:Integer;
   end;

  constructor Create(typ:TBlkType);
end;

////////////////////////////////////////////////////////////////////////////////

TRelief=class
  private
   // technologicke bloky
   TechBloky:record
    Data:array of TTechBlok;
    Count:Integer;
   end;


   ORs:TORs;                    //oblasti rizeni

   Errors:TStrings;             //sem se ulozi chyby pri nacitani a vrati se pri CheckValid()

    function FileLoad(const filename:string):Byte;
    function ORLoad(const ORs:TStrings):Byte;
    procedure Reset();          //reset data cnt (= := 0)

    function GetUsekPredID(UsekPos:TPoint; oblRizeni:Integer):Integer;  //porovnava pouze v dane oblasti rizeni
    // !!!   musi byt spusteno pred merge oblasti rizeni !!!

    function GetTechBlkOR(index:Integer):string;

    procedure AddGraphBlk(data:TGraphBlok; id:Integer; typ:TBlkType);

    procedure MergeOROsv(orig_or:Integer; new_or:Integer);        // dava osvetleni z orig_or do new_or
    procedure ReplaceBlkOR(orig_or:Integer; new_or:Integer);      // nahradi oblast rizeni u bloku s orig_or na new_or
    procedure DeleteOR(index:Integer);
          // pouzivano pri merge oblasti rizeni

    procedure FindSimilarORs(var or1:Integer; var or2:Integer);
    function CheckSimilarORs(or1:Integer; or2:Integer):Boolean;     // zkontroluje, jestli jsou OR opravdu stejne
        // vrati true, pokud ok, jinak false

  public
    constructor Create();
    destructor Destroy(); override;

    function FilesLoad(const filenames:TStrings):Cardinal;

    procedure Merge();                  //provede merge oblasti rizeni

    function CheckValid():TStrings;     //overi validitu dat a vrati chyby

    function ExportData(const filename:string):Byte;

    //spravny postup volani zvnejsku:
    // 1) FilesLoad
    // 2) Merge
    // 3) CheckValid
    // 4) pokud zadne chyby, ExportData, jinak znovu FilesLoad
end;//TPanelObjects

//Format objektoveho souboru reliefu:
//ini soubor
//
//'G'-globalni vlastnost
//    ver: verze souboru
//
//'P'-Panel: zakladni udaje
//    W:Width
//    H:Height
//    U:pocet Useku
//    N:pocet Navestidel
//    T:pocet Textu
//    P:pocet pomocnych objektu
//    V:pocet vyhybek
//    Uv:pocet uvazek
//    UvS: pocet textovych poli souprav k uvazkam
//    Z:pocet vyhybkovych zamku
//    vc: [0,1] 1 pokud jsou vetve spocitane, 0 pokud nejsou
//'U1'..'Un' - sekce useku
//  B= [blok technologie, jemuz je symbol prirazen]
//  S= [symboly] ulozeno textove
//    pevne delky: 0-2..souradnice X
//                 3-5..souradnice Y
//                 6-7..symbol
//  nasledujici data jsou v souboru ulzoena jen, pokud jsou k dispozici vetve:
//    VC= vetve count = [pocet vetvi]
//    V0...V(N-1)= [vetve] ulozeno textove
//      pevne delky: 0-2..1. vyhybka - index v poli vyhybek (nikoliv technologicky blok!)
//                   3-4..1. vyhybka : index dalsi vetve pro polohu "vodorovne"
//                   5-6..1. vyhybka: index dalsi vetve pro polohu "sikmo"

//                   7-9..2. vyhybka - index v poli vyhybek (nikoliv technologicky blok!)
//                 10-11..2. vyhybka : index dalsi vetve pro polohu "vodorovne"
//                 12-13..2. vyhybka: index dalsi vetve pro polohu "sikmo"
//
//      [-           7-9..souradnice X
//                 10-12..souradnice Y
//                 13-15..symbol  -]  <- tato cast se opakuje
//  C= [JCClick] ulozeno textove
//    pevne delky: 0-2..souradnice X
//                 3-5..souradnice Y
//  P= [KPopisek] ulozeno textove
//    pevne delky: 0-2..souradnice X
//                 3-5..souradnice Y
//  N= [nazev koleje] ulozeno textove
//  OR= [oblast rizeni] integer 0-n
//  R= [root, koren] 0-2..souradnice X
//                   3-5..souradnice Y
//
//'N1'..'Nn' - sekce navestidel
//  B= [asociovany blok technologie]
//  X= [pozice X]
//  Y= [pozice Y]
//  S= [symbol]
//     0..3 (4 typy navestidel)
//  OR= [oblast rizeni] integer 0-n
//
//'P0'..'Pn' - sekce pomocnych bloku
//  1 blok vzdy v 1 objektu
//  P= [pozice] - 3*X;3*Y;... (bez stredniku - pevne delky)
//  S= [symbol]
//
//'T1'..'Tn' - sekce textu
//  T= [text]
//  X= [pozice X]
//  Y= [pozice Y]
//  C= [barva]
//  B= [blok]
//  OR= [oblast rizeni] integer 0-n
//
//'V0'..'Vn' - sekce vyhybek
//  B= [asociovany blok technologie]
//  S= [symbol]
//  P= [poloha plus]
//  X= [pozice X]
//  Y= [pozice Y]
//  O= [objekt, kteremu vyhybka patri]
//  OR= [oblast rizeni] integer 0-n

//'PRJ0'..'PRJn' - sekce prejezdu
//  B= [asociovany blok technologie]
//  BP= [blik_pozice] - 3*X;3*Y;3*U... = X,T, tech_usek (bez stredniku - pevne delky)
//  SP= [static_pozice] - 3*X;3*Y;... (bez stredniku - pevne delky)
//  OR= [oblast rizeni] integer 0-n
//  U= [technologicky usek] - blok typu usek, ktery prejezd zobrazuje (prostredni usek prejezdu)

// 'Uv0'..'Uvn' - sekce uvazek
//  B= [asociovany blok technologie]
//  D= [zakladni smer]
//  X= [pozice X]
//  Y= [pozice Y]
//  OR= [oblast rizeni] integer 0-n

// 'UvS0'..'UvSn' - sekce seznamu souprav uvazek
//  B= [asociovany blok technologie]
//  VD= [vertikalni smer]
//  X= [pozice X]
//  Y= [pozice Y]
//  C= [pocet souprav]
//  OR= [oblast rizeni] integer 0-n

// 'Z0'..'Zn' - sekce vyhybkovych zamku
//  B= [asociovany blok technologie]
//  X= [pozice X]
//  Y= [pozice Y]
//  OR= [oblast rizeni] integer 0-n

implementation

//vytvoreni objektu
constructor TRelief.Create();
begin
 inherited Create();
 Self.Errors := TStringList.Create();
end;//constructor

destructor TRelief.Destroy;
var i:Integer;
begin
 for i := 0 to Self.ORs.Cnt-1 do
   Self.ORs.Data[i].Osvetleni.Free();

 Self.Reset();
 FreeAndNil(Self.Errors);

 inherited Destroy();
end;//destructor

//resetuje pocty
procedure TRelief.Reset();
var i, j:Integer;
begin
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-1 do
    FreeAndNil(Self.TechBloky.Data[i].graph_blk.data[j]);
   FreeAndNil(Self.TechBloky.Data[i]);
  end;//for i

 Self.ORs.Cnt         := 0;
 Self.TechBloky.Count := 0;

 Self.Errors.Clear();
end;//procedure

//nacitani vsech souboru
//vraci index v hi, chybovy kod v lo
function TRelief.FilesLoad(const filenames:TStrings):Cardinal;
var i:Integer;
    return:Byte;
begin
 Self.Reset();

 for i := 0 to filenames.Count-1 do
  begin
   return := Self.FileLoad(filenames[i]);
   if (return <> 0) then
    begin
     Result := (i shl 8)+return;
     Exit;
    end;
  end;//for i

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

//nacitani 1 souboru
function TRelief.FileLoad(const filename:string):Byte;
var i,j,return,id:Integer;
    inifile:TMemIniFile;
    Obj, ver:string;
    sect_str,obl_rizeni:TStrings;
    start_OR:Integer;
    w,h:Integer;
    blk:TGraphBlok;
    cnt:Integer;
begin
 //kontrola existence
 if (not FileExists(filename)) then
  begin
   Result := 1;
   Exit;
  end;

 //samotne nacitani dat
 try
   inifile := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   Result := 100;
   Exit;
 end;

 //kontrola verze
 ver := inifile.ReadString('G','ver',_FileVersion);
 if (_FileVersion <> ver) then
  begin
   if (Application.MessageBox(PChar(filename+#13#10+'Naèítáte soubor s verzí '+ver+#13#10+'Aplikace momentálnì podporuje verzi '+_FileVersion+#13#10+'Chcete pokraèovat?'), 'Varování', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
    begin
     Result := 2;
     Exit;
    end;
  end;

 DateTimeToString(Obj,'yyyy-mm-dd hh:nn:ss',Now);
 Self.Errors.Add('Loading validator: '+Obj+': '+ExtractFileName(filename));

 h := inifile.ReadInteger('P','H',0);
 w := inifile.ReadInteger('P','W',0);

 start_OR := Self.ORs.Cnt;

 //oblasti rizeni
 sect_str := TStringList.Create();
 obl_rizeni := TStringList.Create();
 inifile.ReadSection('OR',sect_str);
 for i := 0 to sect_str.Count-1 do
    obl_rizeni.Add(inifile.ReadString('OR',sect_str[i],''));

 return := Self.ORLoad(obl_rizeni);
 if (return <> 0) then
  begin
   Result := return+8;
   Exit;
  end;

 sect_str.Free();
 obl_rizeni.Free();

 //useky
 cnt := inifile.ReadInteger('P','U',0);
 for i := 0 to cnt-1 do
  begin
   blk := TUsek.Create();
   id := inifile.ReadInteger('U'+IntToStr(i), 'B', -1);
   if (id = -1) then
    Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': neni navaznost na technologicky blok');

   //OR
   blk.OblRizeni := inifile.ReadInteger('U'+IntToStr(i),'OR',-1)+start_OR;      //dodelat kontroly
   if (blk.OblRizeni = start_OR-1) then
    Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': neni navaznost na oblast rizeni');


   //Symbols
   obj := inifile.ReadString('U'+IntToStr(i),'S','');
   (blk as TUsek).Symbols.Count := (Length(obj) div 8);
   for j := 0 to (blk as TUsek).Symbols.Count - 1 do
    begin
     (blk as TUsek).Symbols.Data[j].Position.X := StrToIntDef(copy(obj, j*8+1, 3), 0);
     (blk as TUsek).Symbols.Data[j].Position.Y := StrToIntDef(copy(obj, j*8+4, 3), 0);
     (blk as TUsek).Symbols.Data[j].SymbolID   := StrToIntDef(copy(obj, j*8+7, 2), 0);

     if ((blk as TUsek).Symbols.Data[j].Position.X >= w) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': symbol presahuje sirku reliefu');
     if ((blk as TUsek).Symbols.Data[j].Position.Y >= h) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': symbol presahuje vysku reliefu');
    end;//for j
   if ((blk as TUsek).Symbols.Count = 0) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': neni zadny symbol');

   (blk as TUsek).cislo_koleje := inifile.ReadString('U'+IntToStr(i),'N','');

   Self.AddGraphBlk(blk, id, usek);
  end;//for i

 //navestidla
 cnt := inifile.ReadInteger('P','N',0);
 for i := 0 to cnt-1 do
  begin
   blk      := TNavestidlo.Create();
   id       := inifile.ReadInteger('N'+IntToStr(i),'B',-1);
   if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief navestidlo '+IntToStr(i)+': neni navaznost na technologicky blok');

   (blk as TNavestidlo).Position.X := inifile.ReadInteger('N'+IntToStr(i),'X',0);
   (blk as TNavestidlo).Position.Y := inifile.ReadInteger('N'+IntToStr(i),'Y',0);
   if ((blk as TNavestidlo).Position.X >= w) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief navestidlo '+IntToStr(i)+': symbol presahuje sirku reliefu');
   if ((blk as TNavestidlo).Position.Y >= h) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief navestidlo '+IntToStr(i)+': symbol presahuje vysku reliefu');

   (blk as TNavestidlo).SymbolID   := inifile.ReadInteger('N'+IntToStr(i),'S',0);
   blk.OblRizeni  := inifile.ReadInteger('N'+IntToStr(i),'OR',-1) + start_OR;
   if (blk.OblRizeni = start_OR-1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief navestidlo '+IntToStr(i)+': neni navaznost na oblast rizeni');

   case ((blk as TNavestidlo).SymbolID) of
    0,4 : (blk as TNavestidlo).UsekPred := Self.GetUsekPredID(Point((blk as TNavestidlo).Position.X-1, (blk as TNavestidlo).Position.Y), blk.OblRizeni);
    1,5 : (blk as TNavestidlo).UsekPred := Self.GetUsekPredID(Point((blk as TNavestidlo).Position.X+1, (blk as TNavestidlo).Position.Y), blk.OblRizeni);
   end;
   if ((blk as TNavestidlo).UsekPred = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief navestidlo '+IntToStr(i)+': pred navestidlem neni zadny usek');

   Self.AddGraphBlk(blk, id, navestidlo);
  end;//for i

 //pomocne symboly nejsou potreba

 //Vyhybky
 cnt := inifile.ReadInteger('P','V',0);
 for i := 0 to cnt-1 do
  begin
   blk := TVyhybka.Create();
   id        := inifile.ReadInteger('V'+IntToStr(i),'B',-1);
   if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief vyhybka '+IntToStr(i)+': neni navaznost na technologicky blok');

   (blk as TVyhybka).SymbolID    := inifile.ReadInteger('V'+IntToStr(i),'S',0);
   (blk as TVyhybka).PolohaPlus  := inifile.ReadInteger('V'+IntToStr(i),'P',0);
   (blk as TVyhybka).Position.X  := inifile.ReadInteger('V'+IntToStr(i),'X',0);
   (blk as TVyhybka).Position.Y  := inifile.ReadInteger('V'+IntToStr(i),'Y',0);
   if ((blk as TVyhybka).Position.X >= w) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief vyhybka '+IntToStr(i)+': symbol presahuje sirku reliefu');
   if ((blk as TVyhybka).Position.Y >= h) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief vyhybka '+IntToStr(i)+': symbol presahuje vysku reliefu');

   (blk as TVyhybka).obj         := inifile.ReadInteger('V'+IntToStr(i),'O',-1);
   (blk as TVyhybka).obj         := inifile.ReadInteger('U'+IntToStr((blk as TVyhybka).obj),'B',-1);

   blk.OblRizeni   := inifile.ReadInteger('V'+IntToStr(i),'OR',-1)+start_OR;
   if (blk.OblRizeni = start_OR-1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief vyhybka '+IntToStr(i)+': neni navaznost na oblast rizeni');

   Self.AddGraphBlk(blk, id, vyhybka);
  end;

 //Prejezdy
 cnt := inifile.ReadInteger('P','PRJ',0);
 for i := 0 to cnt-1 do
  begin
   blk := TPrejezd.Create();
   id  := inifile.ReadInteger('PRJ'+IntToStr(i), 'B', -1);
   if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief prejezd '+IntToStr(i)+': neni navaznost na technologicky blok');

   blk.OblRizeni   := inifile.ReadInteger('PRJ'+IntToStr(i), 'OR', -1)+start_OR;

   obj := inifile.ReadString('PRJ'+IntToStr(i), 'BP', '');
   (blk as TPrejezd).BlikPositions.Count := (Length(obj) div 9);
   for j := 0 to (blk as TPrejezd).BlikPositions.Count-1 do
    begin
     (blk as TPrejezd).BlikPositions.Data[j].Pos.X := StrToIntDef(copy(obj, j*9+1, 3), 0);
     (blk as TPrejezd).BlikPositions.Data[j].Pos.Y := StrToIntDef(copy(obj, j*9+4, 3), 0);
     (blk as TPrejezd).BlikPositions.Data[j].TechUsek := StrToIntDef(copy(obj, j*9+7, 3), 0);
    end;//for j

   obj := inifile.ReadString('PRJ'+IntToStr(i), 'SP', '');
   (blk as TPrejezd).StaticPositions.Count := (Length(obj) div 6);
   for j := 0 to (blk as TPrejezd).StaticPositions.Count-1 do
    begin
     (blk as TPrejezd).StaticPositions.Data[j].X := StrToIntDef(copy(obj, j*6+1, 3), 0);
     (blk as TPrejezd).StaticPositions.Data[j].Y := StrToIntDef(copy(obj, j*6+4, 3), 0);
    end;//for j

   Self.AddGraphBlk(blk, id, prejezd);
  end;//for i

 // popisky
 cnt := inifile.ReadInteger('P','T',0);
 for i := 0 to cnt-1 do
  begin
   blk := TPopisek.Create();
   id  := inifile.ReadInteger('T'+IntToStr(i),'B', -1);

   (blk as TPopisek).Text       := inifile.ReadString('T'+IntToStr(i),'T', 'text');
   (blk as TPopisek).Position.X := inifile.ReadInteger('T'+IntToStr(i),'X', 0);
   (blk as TPopisek).Position.Y := inifile.ReadInteger('T'+IntToStr(i),'Y', 0);
   (blk as TPopisek).Color      := inifile.ReadInteger('T'+IntToStr(i),'C', 0);
   blk.OblRizeni                := inifile.ReadInteger('T'+IntToStr(i),'OR', -1)+start_OR;

   if ((id = -1) and (Length((blk as TPopisek).Text) = 1)) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief popisek '+IntToStr(i)+': neni navaznost na technologicky blok');

   Self.AddGraphBlk(blk, id, popisek);
  end;//for i

 // uvazky
 cnt := inifile.ReadInteger('P', 'Uv', 0);
 for i := 0 to cnt-1 do
  begin
   blk      := TUvazka.Create();
   id       := inifile.ReadInteger('Uv'+IntToStr(i),'B', -1);

   (blk as TUvazka).Pos.X       := inifile.ReadInteger('Uv'+IntToStr(i), 'X', 0);
   (blk as TUvazka).Pos.Y       := inifile.ReadInteger('Uv'+IntToStr(i), 'Y', 0);
   (blk as TUvazka).defalt_dir  := inifile.ReadInteger('Uv'+IntToStr(i), 'D', 0);
   blk.OblRizeni                := inifile.ReadInteger('Uv'+IntToStr(i), 'OR', -1) + start_OR;

   if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief uvazka '+IntToStr(i)+': neni navaznost na technologicky blok');

   Self.AddGraphBlk(blk, id, uvazka);
  end;//for i

 // uvazky spr
 // uvazky spr se ve skutenocti nacitaji jako uavzky, aby doslo k merge
 // data o uvazky spr se tak skutecne nenactou, ale to nam nevadi - potrebujeme jen ID
 cnt := inifile.ReadInteger('P', 'UvS', 0);
 for i := 0 to cnt-1 do
  begin
   blk := TUvazka.Create();
   id       := inifile.ReadInteger('UvS'+IntToStr(i),'B', -1);

   blk.OblRizeni := inifile.ReadInteger('UvS'+IntToStr(i), 'OR', -1)+start_OR;

   if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief uvazka spr '+IntToStr(i)+': neni navaznost na technologicky blok');

   Self.AddGraphBlk(blk, id, uvazka);
  end;//for i

 // zamky
 cnt := inifile.ReadInteger('P','Z',0);
 for i := 0 to cnt-1 do
  begin
   blk := TZamek.Create();
   id  := inifile.ReadInteger('Z'+IntToStr(i), 'B', -1);
   blk.OblRizeni := inifile.ReadInteger('Z'+IntToStr(i),'OR', -1) + start_OR;

   (blk as TZamek).Pos.X := inifile.ReadInteger('Z'+IntToStr(i), 'X', 0);
   (blk as TZamek).Pos.Y := inifile.ReadInteger('Z'+IntToStr(i), 'Y', 0);

   if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief zamek '+IntToStr(i)+': neni navaznost na technologicky blok');

   Self.AddGraphBlk(blk, id, zamek);
  end;

 // rozpojovace
 cnt := inifile.ReadInteger('P','R',0);
 for i := 0 to cnt-1 do
  begin
   blk := TRozp.Create();
   id  := inifile.ReadInteger('R'+IntToStr(i), 'B', -1);
   blk.OblRizeni := inifile.ReadInteger('R'+IntToStr(i),'OR', -1) + start_OR;

   (blk as TRozp).Pos.X := inifile.ReadInteger('R'+IntToStr(i), 'X', 0);
   (blk as TRozp).Pos.Y := inifile.ReadInteger('R'+IntToStr(i), 'Y', 0);

   if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief rozpojovac '+IntToStr(i)+': neni navaznost na technologicky blok');

   Self.AddGraphBlk(blk, id, rozp);
  end;


 Self.Errors.Add('--- Loading check complete ---');

 inifile.Free;
 Result := 0;
end;//procedure LoadFile

////////////////////////////////////////////////////////////////////////////////

//na kazdem radku je ulozena jedna oblast rizeni ve formatu:
//  nazev;nazev_zkratka;id;lichy_smer(0,1);orientace_DK(0,1);ModCasStart(0,1);ModCasStop(0,1);ModCasSet(0,1);dkposx;dkposy;qposx;qposy;timeposx;timeposy;osv_mtb|osv_port|osv_name;
function TRelief.ORLoad(const ORs:TStrings):Byte;
var data_main,data_osv,data_osv2:TStrings;
    i,j:Integer;
    Osv:TOsv;
begin
 data_main := TStringList.Create();
 data_osv  := TStringList.Create();
 data_osv2 := TStringList.Create();

 for i := 0 to ORs.Count-1 do
  begin
   data_main.Clear();
   ExtractStrings([';'],[],PChar(ORs[i]),data_main);

   if (data_main.Count < 14) then
    begin
     Result := 2;
     Exit;
    end;

   Self.ORs.Cnt := Self.ORs.Cnt + 1;
   Self.ORs.Data[Self.ORs.Cnt-1].str := ORs[i];

   Self.ORs.Data[Self.ORs.Cnt-1].Name       := data_main[0];
   Self.ORs.Data[Self.ORs.Cnt-1].ShortName  := data_main[1];
   Self.ORs.Data[Self.ORs.Cnt-1].id         := data_main[2];
   Self.ORs.Data[Self.ORs.Cnt-1].Lichy      := StrToInt(data_main[3]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.DKOr  := StrToInt(data_main[4]);

   Self.ORs.Data[Self.ORs.Cnt-1].Rights.ModCasStart := StrToBool(data_main[5]);
   Self.ORs.Data[Self.ORs.Cnt-1].Rights.ModCasStop  := StrToBool(data_main[6]);
   Self.ORs.Data[Self.ORs.Cnt-1].Rights.ModCasSet   := StrToBool(data_main[7]);

   Self.ORs.Data[Self.ORs.Cnt-1].Poss.DK.X := StrToInt(data_main[8]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.DK.Y := StrToInt(data_main[9]);

   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Queue.X := StrToInt(data_main[10]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Queue.Y := StrToInt(data_main[11]);

   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Time.X := StrToInt(data_main[12]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Time.Y := StrToInt(data_main[13]);

   Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni := TList<TOsv>.Create();

   data_osv.Clear();
   if (data_main.Count < 15) then continue;

   ExtractStrings(['|'],[],PChar(data_main[14]),data_osv);
   for j := 0 to data_osv.Count-1 do
    begin
     data_osv2.Clear();
     ExtractStrings(['#'],[],PChar(data_osv[j]),data_osv2);

     if (data_osv2.Count < 2) then Exit(3);

     try
       Osv.board := StrToInt(data_osv2[0]);
       Osv.port  := StrToInt(data_osv2[1]);
       if (data_osv2.Count > 2) then Osv.name := data_osv2[2] else Osv.name := '';
       Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Add(Osv);
     except

     end;
    end;//for j
  end;//for i


 FreeAndNil(data_main);
 FreeAndNil(data_osv);
 FreeAndNil(data_osv2);
 Result := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// provede merge oblasti rizeni
procedure TRelief.Merge();
var orig,new:Integer;
begin
 while (true) do
  begin
   // hledame duplicity v unikatnich nazvech v poli oblasti rizeni
   Self.FindSimilarORs(orig, new);

   if (not Self.CheckSimilarORs(orig, new)) then
     Self.Errors.Add('ERROR: oblasti øízení s id ' + Self.ORs.Data[orig].id + ' se neshodují v názvu nebo ve zkratce názvu!');

   // pokud nenajdeme duplicitu, ukoncujeme merge
   if (orig = -1) then Exit;

   // duplicita nalezena -> merge
   // zachovavame orig, new mazeme
   Self.MergeOROsv(new, orig);
   Self.ReplaceBlkOR(new, orig);
   Self.DeleteOR(new);

   // a hledame znova....
 end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

//checking data valid
function TRelief.CheckValid():TStrings;
var str:string;
    i,j:Integer;
    nav, nav2:TNavestidlo;
    vyh:TVyhybka;
begin
 Self.Errors.Add('--- Spustena 2. faze validace: validator objektovych navaznosti ---');
 DateTimeToString(str,'yyyy-mm-dd hh:nn:ss',Now);
 Self.Errors.Add(str);

 //kontrola prirazeni oblasti rizeni
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-1 do
    begin
     if (Self.TechBloky.Data[i].graph_blk.data[j].OblRizeni >= Self.ORs.Cnt) then
      Self.Errors.Add('ERROR: Relief blok id '+IntToStr(Self.TechBloky.Data[i].id)+': definovana oblast rizeni, ktera neni v seznamu OR');
    end;//for j

   /////////////
   //kontrola stejneho cisla koleje
   if (Self.TechBloky.Data[i].typ = usek) then
    begin
     for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-2 do
      begin
       if (TUsek(Self.TechBloky.Data[i].graph_blk.data[j]).cislo_koleje <>
           TUsek(Self.TechBloky.Data[i].graph_blk.data[j+1]).cislo_koleje) then
          Self.Errors.Add('ERROR: Relief technologicky usek '+IntToStr(Self.TechBloky.Data[i].id)+
            ': ruzne cislo koleje v ruznych panelech!');
      end;
    end;

   /////////////
   //kontrola totoznosti orientace navestidel

   if (Self.TechBloky.Data[i].typ = navestidlo) then
    begin

     // navestidlo
     for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-2 do
      begin
       nav := (Self.TechBloky.Data[i].graph_blk.data[j] as TNavestidlo);
       nav2 := (Self.TechBloky.Data[i].graph_blk.data[j+1] as TNavestidlo);
       case (nav.SymbolID) of
        0,4:begin
             case (nav.SymbolID) of
              0,4:if ((Self.ORs.Data[nav.OblRizeni].Lichy xor 0) <> (Self.ORs.Data[nav2.OblRizeni].Lichy xor 0)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(Self.TechBloky.Data[i].id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs.Data[nav.OblRizeni].Name+', '+Self.ORs.Data[nav2.OblRizeni].Name);
              1,5:if ((Self.ORs.Data[nav.OblRizeni].Lichy xor 0) <> (Self.ORs.Data[nav2.OblRizeni].Lichy xor 1)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(Self.TechBloky.Data[i].id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs.Data[nav.OblRizeni].Name+', '+Self.ORs.Data[nav2.OblRizeni].Name);
             end;//case j+1
         end;//case 0,4
        1,5:begin
             case (nav.SymbolID) of
              0,4:if ((Self.ORs.Data[nav.OblRizeni].Lichy xor 1) <> (Self.ORs.Data[nav2.OblRizeni].Lichy xor 0)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(Self.TechBloky.Data[i].id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs.Data[nav.OblRizeni].Name+', '+Self.ORs.Data[nav2.OblRizeni].Name);
              1,5:if ((Self.ORs.Data[nav.OblRizeni].Lichy xor 1) <> (Self.ORs.Data[nav2.OblRizeni].Lichy xor 1)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(Self.TechBloky.Data[i].id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs.Data[nav.OblRizeni].Name+', '+Self.ORs.Data[nav2.OblRizeni].Name);
             end;//case j+1
         end;//case 1,5
       end;//case i
      end;// for j

    end;//if navestidlo

   /////////////
   //kontrola totoznosti navaznosti vyhybek

   if (Self.TechBloky.Data[i].typ = vyhybka) then
    begin
     for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-2 do
      begin
       vyh := (Self.TechBloky.Data[i].graph_blk.data[j] as TVyhybka);
       if (vyh.obj <> vyh.obj) then
        Self.Errors.Add('ERROR: Relief vyhybka '+IntToStr(i)+': vyhybka ma navaznost na jiny blok ve stanicich '+Self.ORs.Data[vyh.OblRizeni].Name+', '+Self.ORs.Data[vyh.OblRizeni].Name);
      end;//for j

    end;//vyhybka
  end;//for i

 Self.Errors.Add('--- Validace hotova. ---');
 Result := Self.Errors;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TRelief.ExportData(const filename:string):Byte;
var ini:TMemIniFile;
    i,j:Integer;
    str:string;
begin
 DeleteFile(filename);

 try
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   Result := 1;
   Exit;
 end;

 //ulozit OR
 for i := 0 to Self.ORs.Cnt-1 do
  begin
   // ukladame specificky upravena data, protoze smysl ma ukladat jen data u vsech OR stajna
   // = nazev, zkratka a unikatni nazev
   str := Self.ORs.Data[i].Name + ';' + Self.ORs.Data[i].ShortName + ';' + Self.ORs.Data[i].id + ';';
   for j := 0 to Self.ORs.Data[i].Osvetleni.Count-1 do
     str := str + '(' + IntToStr(Self.ORs.Data[i].Osvetleni[j].board) + '|' + IntToStr(Self.ORs.Data[i].Osvetleni[j].port) + '|' + Self.ORs.Data[i].Osvetleni[j].name + ')';
   str := str + ';';
   ini.WriteString('OR', IntToStr(i), str);
  end;

 //ulozit useky
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> usek) then continue;

   str := '';

   //sestaveni OR
   str := str + Self.GetTechBlkOR(i) + ';';

   if ((Self.TechBloky.Data[i].graph_blk.data[0] as TUsek).cislo_koleje <> '') then
    begin
     str := str + '1;' + TUsek(Self.TechBloky.Data[i].graph_blk.data[0]).cislo_koleje;
    end else
     str := str + '0';

   ini.WriteString('U', IntToStr(Self.TechBloky.Data[i].id), str);
  end;//for i

 //ulozit navestidla
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> navestidlo) then continue;

   str := '';

   //sestaveni OR
   str := str + Self.GetTechBlkOR(i) + ';';

   //typ symbolu
   case ((Self.TechBloky.Data[i].graph_blk.data[0] as TNavestidlo).SymbolID) of
    0,1:str := str + '0;';
    //2,3 jsou AB
    4,5:str := str + '1;';
   end;//case

  case (Self.ORs.Data[Self.TechBloky.Data[i].graph_blk.data[0].OblRizeni].Lichy xor ((Self.TechBloky.Data[i].graph_blk.data[0] as TNavestidlo).SymbolID mod 2)) of
   0:str := str + '0;';
   1:str := str + '1;';
  end;

   //usek pred id
   str := str + IntToStr((Self.TechBloky.Data[i].graph_blk.data[0] as TNavestidlo).UsekPred);

   ini.WriteString('N',IntToStr(Self.TechBloky.Data[i].id), str);
  end;//for i

 //ulozit vyhybky
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> vyhybka) then continue;

   str := '';

   //sestaveni OR
   str := str + Self.GetTechBlkOR(i) + ';';

   //navaznost na usek
   str := str + IntToStr((Self.TechBloky.Data[i].graph_blk.data[0] as TVyhybka).obj)+';';

   ini.WriteString('V',IntToStr(Self.TechBloky.Data[i].id),str);
  end;//for i

 //ulozit prejezdy
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> prejezd) then continue;

   str := '';

   //sestaveni OR
   str := str + Self.GetTechBlkOR(i) + ';';

   ini.WriteString('PRJ', IntToStr(Self.TechBloky.Data[i].id), str);
  end;//for i

 //ulozit popisky
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> popisek) then continue;

   if (Length((Self.TechBloky.Data[i].graph_blk.data[0] as TPopisek).Text) <> 1) then continue;

   str := '';

   //sestaveni OR
   str := str + Self.GetTechBlkOR(i) + ';';

   ini.WriteString('T', IntToStr(Self.TechBloky.Data[i].id), str);
  end;//for i

 //ulozit uvazky
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> uvazka) then continue;

   //sestaveni OR
   str := '';
   str := str + Self.GetTechBlkOR(i) + ';';

   ini.WriteString('Uv', IntToStr(Self.TechBloky.Data[i].id), str);
  end;//for i

 //ulozit zamky
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> zamek) then continue;

   //sestaveni OR
   str := '';
   str := str + Self.GetTechBlkOR(i) + ';';

   ini.WriteString('Z', IntToStr(Self.TechBloky.Data[i].id), str);
  end;//for i

 //ulozit rozpojovace
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> rozp) then continue;

   //sestaveni OR
   str := '';
   str := str + Self.GetTechBlkOR(i) + ';';

   ini.WriteString('R', IntToStr(Self.TechBloky.Data[i].id), str);
  end;//for i

 ini.UpdateFile();
 ini.Free();

 Result := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vraci technologicke ID bloku, ktery je na pozici UsekPos a v OR oblRizeni
// vyuzivano pri ziskavani bloku pred navestidly
function TRelief.GetUsekPredID(UsekPos:TPoint; oblRizeni:Integer):Integer;
var i,j,k:Integer;
begin
 // prijdeme vsechny technologicke useky
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if (Self.TechBloky.Data[i].typ <> usek) then continue;
   
   // projdeme vsechny useky na reliefu daneho tech. useku
   for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-1 do
    begin
     if (Self.TechBloky.Data[i].graph_blk.data[j].OblRizeni = oblRizeni) then
      begin
       // tady mame jisto, ze jsme ve spravne oblasti rizeni
       // prohleddame vsechny symboly bloku
       for k := 0 to (Self.TechBloky.Data[i].graph_blk.data[j] as TUsek).Symbols.Count-1 do
         if ((UsekPos.X = (Self.TechBloky.Data[i].graph_blk.data[j] as TUsek).Symbols.Data[k].Position.X) and (UsekPos.Y = (Self.TechBloky.Data[i].graph_blk.data[j] as TUsek).Symbols.Data[k].Position.Y)) then
           Exit(Self.TechBloky.Data[i].id);
      end;//if oblRizeni
    end;//for j
  end;//for i

 Result := -1;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TRelief.GetTechBlkOR(index:Integer):string;
var i,j:Integer;
    ret:array [0.._MAX_OR-1] of Integer;
    retcnt:Integer;
    found:boolean;
begin
 Result := '';
 if (index < 0) or (index >= Self.TechBloky.Count) then Exit;

 retcnt := 0;
 for i := 0 to Self.TechBloky.Data[index].graph_blk.cnt-1 do
  begin
   found := false;
   for j := 0 to retcnt-1 do if (ret[j] = Self.TechBloky.Data[index].graph_blk.data[i].OblRizeni) then found := true;

   if (not found) then
    begin
     retcnt := retcnt + 1;
     ret[retcnt-1] := Self.TechBloky.Data[index].graph_blk.data[i].OblRizeni;
    end;
  end;//for i

 for i := 0 to retcnt-1 do Result := Result + Self.ORs.Data[ret[i]].id + '|';
end;//function

////////////////////////////////////////////////////////////////////////////////
// pridava blok v parametru do technologickych bloku:

procedure TRelief.AddGraphBlk(data:TGraphBlok; id:Integer; typ:TBlkType);
var i, index:Integer;
begin
 index := Self.TechBloky.Count;
 for i := 0 to Self.TechBloky.Count-1 do
  begin
   if ((Self.TechBloky.Data[i].id = id) and (typ = Self.TechBloky.Data[i].typ)) then
    begin
     index := i;
     Break;
    end;
  end;

 if (index = Self.TechBloky.Count) then
  begin
   // pridavame novy technologicky blok
   Self.TechBloky.Count := Self.TechBloky.Count + 1;
   if (Length(Self.TechBloky.Data) < Self.TechBloky.Count) then
     SetLength(Self.TechBloky.Data, Length(Self.TechBloky.Data)+256);

   Self.TechBloky.Data[index] := TTechBlok.Create(typ);
   Self.TechBloky.Data[index].graph_blk.cnt     := 1;
   Self.TechBloky.Data[index].graph_blk.data[0] := data;
  end else begin
   // pridavame graficky blok do existujicicho tech. bloku (pripad, kdy se nectou uplne stejne bloky z vice panelu)
   Self.TechBloky.Data[index].graph_blk.cnt := Self.TechBloky.Data[index].graph_blk.cnt + 1;
   Self.TechBloky.Data[index].graph_blk.data[Self.TechBloky.Data[index].graph_blk.cnt-1] := data;
  end;

 Self.TechBloky.Data[index].id  := id;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// nahradi oblast rizeni u bloku s orig_or na new_or
// pouzivano pri merge oblasti rizeni
procedure TRelief.ReplaceBlkOR(orig_or:Integer; new_or:Integer);
var i, j:Integer;
begin
 for i := 0 to Self.TechBloky.Count-1 do
   for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-1 do
    if (Self.TechBloky.Data[i].graph_blk.data[j].OblRizeni = orig_or) then
      Self.TechBloky.Data[i].graph_blk.data[j].OblRizeni := new_or;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// maze oblast rizeni
// vyuzivano pri merge oblsti rizeni
procedure TRelief.DeleteOR(index:Integer);
var i, j:Integer;
begin
 for i := index to Self.ORs.Cnt-2 do
   Self.ORs.Data[i] := Self.ORs.Data[i+1];
 Self.ORs.Cnt := Self.ORs.Cnt - 1;

 // vsem blokum, ktere mely OR tuto a vesti musime dekrementovat OR
 for i := 0 to Self.TechBloky.Count-1 do
   for j := 0 to Self.TechBloky.Data[i].graph_blk.cnt-1 do
    if (Self.TechBloky.Data[i].graph_blk.data[j].OblRizeni >= index) then
      Self.TechBloky.Data[i].graph_blk.data[j].OblRizeni := Self.TechBloky.Data[i].graph_blk.data[j].OblRizeni-1;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

constructor TTechBlok.Create(typ:TBlkType);
begin
 Self.typ := typ;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.FindSimilarORs(var or1:Integer; var or2:Integer);
var i,j:Integer;
begin
 or1 := -1;
 or2 := -1;

 for i := 0 to Self.ORs.Cnt-1 do
  begin
   for j := 0 to Self.ORs.Cnt-1 do
    begin
     if (i <> j) and (Self.ORs.Data[i].id = Self.ORs.Data[j].id) then
      begin
       or1 := i;
       or2 := j;
       Exit;
      end;
    end;//for j
  end;//for i
end;//procedure

// porovnavame svechny potrebne atributy a podle nich zjistujeme, jestli jsou OR opravdu totozne
// pokud narazime na zmeny, pridame je do logu (Self.Errors)
function TRelief.CheckSimilarORs(or1:Integer; or2:Integer):Boolean;
begin
 // OR musi mit POUZE stejny nazev, zkratku nazvu a unikatni nazev
 //   predpoklada se, ze OR v argumentu uz maji stejny unikatni nazev
 //   (na zaklade toho probiha vyhodnoceni potencialne stejnych OR)
 // osvetleni a vse ostatni muze byt jine

 if ((Self.ORs.Data[or1].Name <> Self.ORs.Data[or2].Name) or
    (Self.ORs.Data[or1].ShortName <> Self.ORs.Data[or2].ShortName)) then
  begin
   Result := false;
  end else begin
   Result := true;
  end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.MergeOROsv(orig_or:Integer; new_or:Integer);
var i,j:Integer;
    found:boolean;
begin
 for i := 0 to Self.ORs.Data[orig_or].Osvetleni.Count-1 do
  begin
   found := false;
   for j := 0 to Self.ORs.Data[new_or].Osvetleni.Count-1 do
    if (Self.ORs.Data[new_or].Osvetleni[j].name = Self.ORs.Data[orig_or].Osvetleni[i].name) then
     found := true;
     // sem by mohla prijit kontrola konzistence MTB vstupu a vystupu

   if (not found) then
     Self.ORs.Data[new_or].Osvetleni.Add(Self.ORs.Data[orig_or].Osvetleni[i]);
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit

