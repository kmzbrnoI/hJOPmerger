unit Panel;

//tato unita resi veskerou hlavni technologii programu - prace se slucovanim panelu
//tato unita z velke casti prebira kod ReliefObjects.pas z Editoru

//princip funkce:
//  nacist vice souboru *.opnl, vybrat z nich potrebana data pro server a tato data ulozit do souboru *.spnl

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, IniFiles,
  Forms, OblastRizeni, Generics.Collections, StrUtils, Bloky;

const
  _FILEVERSION_10 = $0100;
  _FILEVERSION_11 = $0101;
  _FILEVERSION_12 = $0102;
  _FILEVERSION_13 = $0103;

  _FileVersion_accept : array[0..2] of string = (
     '1.1', '1.2', '1.3'
  );

type

// technologicky blok
// obsahuje reference na graficke bloky
TTechBlok = class
  typ:TBlkType;
  id:Integer;
  graph_blk:TObjectList<TGraphBlok>;

  constructor Create(typ:TBlkType);
  destructor Destroy(); override;
end;

////////////////////////////////////////////////////////////////////////////////

TRelief = class
  private
   // technologicke bloky
   TechBloky:TObjectDictionary<Integer, TTechBlok>;
   ORs:TObjectList<TOR>;
   Errors:TStrings; //sem se ulozi chyby pri nacitani a vrati se pri CheckValid()

    procedure FileLoad(const filename:string);
    procedure Reset();          //reset data cnt (= := 0)

    function GetUsekPredID(UsekPos:TPoint; oblRizeni:Integer):Integer;  //porovnava pouze v dane oblasti rizeni
    // !!!   musi byt spusteno pred merge oblasti rizeni !!!

    function GetTechBlkOR(tblk:TTechBlok):string;

    procedure AddGraphBlk(data:TGraphBlok; id:Integer; typ:TBlkType);

    procedure MergeOROsv(orig_or:TOR; new_or:TOR);        // dava osvetleni z orig_or do new_or
    procedure ReplaceBlkOR(orig_or:Integer; new_or:Integer);      // nahradi oblast rizeni u bloku s orig_or na new_or
    procedure DeleteOR(index:Integer);
          // pouzivano pri merge oblasti rizeni

    procedure FindSimilarORs(var or1:Integer; var or2:Integer);
    function CheckSimilarORs(or1:Integer; or2:Integer):Boolean;     // zkontroluje, jestli jsou OR opravdu stejne
        // vrati true, pokud ok, jinak false

  public
    constructor Create();
    destructor Destroy(); override;

    procedure FilesLoad(const filenames:TStrings);
    procedure Merge();                  //provede merge oblasti rizeni
    function CheckValid():TStrings;     //overi validitu dat a vrati chyby
    procedure ExportData(const filename:string);

    class function FileSupportedVersionsStr():string;

    //spravny postup volani zvnejsku:
    // 1) FilesLoad
    // 2) Merge
    // 3) CheckValid
    // 4) pokud zadne chyby, ExportData, jinak znovu FilesLoad
end;//TPanelObjects

implementation

//vytvoreni objektu
constructor TRelief.Create();
begin
 inherited;

 Self.Errors := TStringList.Create();
 Self.TechBloky := TObjectDictionary<Integer, TTechBlok>.Create();
 Self.ORs := TObjectList<TOR>.Create(TOr.NameComparer);
end;

destructor TRelief.Destroy;
begin
 Self.Reset();
 Self.ORs.Free();
 Self.TechBloky.Free();
 Self.Errors.Free();

 inherited;
end;

//resetuje pocty
procedure TRelief.Reset();
begin
 Self.TechBloky.Clear();
 Self.ORs.Clear();
 Self.Errors.Clear();
end;

//nacitani vsech souboru
//vraci index v hi, chybovy kod v lo
procedure TRelief.FilesLoad(const filenames:TStrings);
var filename:string;
begin
 Self.Reset();

 for filename in  filenames do
  begin
   try
     Self.FileLoad(filename);
   except
     on E:Exception do
       Self.Errors.Add(filename + ': ' + E.Message);
   end;
  end;


end;

////////////////////////////////////////////////////////////////////////////////

//nacitani 1 souboru
procedure TRelief.FileLoad(const filename:string);
var i,j,return,id:Integer;
    inifile:TMemIniFile;
    Obj, ver, key:string;
    verWord: Word;
    sect_str, strs:TStrings;
    start_OR:Integer;
    w,h:Integer;
    blk:TGraphBlok;
    cnt:Integer;
    versionOk:boolean;
    sym:TReliefSym;
    bp:TBlikPoint;
begin
 //kontrola existence
 if (not FileExists(filename)) then
   raise Exception.Create('Soubor panelu ' + filename + ' neexistuje!');

 inifile := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   //kontrola verze
   ver := inifile.ReadString('G', 'ver', 'invalid');
   versionOk := false;
   for i := 0 to Length(_FileVersion_accept)-1 do
    begin
     if (ver = _FileVersion_accept[i]) then
      begin
       versionOk := true;
       Break;
      end;
    end;

   if (not versionOk) then
    begin
     if (Application.MessageBox(PChar('Načítáte soubor s verzí '+ver+#13#10+
         'Aplikace momentálně podporuje verze '+Self.FileSupportedVersionsStr()+#13#10+'Chcete pokračovat?'),
         'Varování', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
       Exit();
    end;

   strs := TStringList.Create();
   try
     ExtractStrings(['.'], [], PChar(ver), strs);
     verWord := (StrToInt(strs[0]) shl 8) + StrToInt(strs[1]);
   finally
     strs.Free();
   end;

   DateTimeToString(Obj,'yyyy-mm-dd hh:nn:ss',Now);
   Self.Errors.Add('Loading validator: '+Obj+': '+ExtractFileName(filename));

   h := inifile.ReadInteger('P','H',0);
   w := inifile.ReadInteger('P','W',0);

   start_OR := Self.ORs.Count;

   // oblasti rizeni
   sect_str := TStringList.Create();
   try
     inifile.ReadSection('OR', sect_str);
     for key in sect_str do
       Self.ORs.Add(TOR.Create(inifile.ReadString('OR', key, '')));
     Self.ORs.Sort();
   finally
     sect_str.Free();
   end;

   // useky
   cnt := inifile.ReadInteger('P','U',0);
   for i := 0 to cnt-1 do
    begin
     id := inifile.ReadInteger('U'+IntToStr(i), 'B', -1);
     if (id = -1) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TUsek.Create();

     // OR
     blk.OblRizeni := inifile.ReadInteger('U'+IntToStr(i),'OR',-1)+start_OR;      //dodelat kontroly
     if (blk.OblRizeni = start_OR-1) then
      Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': neni navaznost na oblast rizeni');


     // symbols
     obj := inifile.ReadString('U'+IntToStr(i), 'S', '');
     (blk as TUsek).Symbols.Clear();
     for j := 0 to (Length(obj) div 8)-1 do
      begin
       sym.Position.X := StrToIntDef(copy(obj, j*8+1, 3), 0);
       sym.Position.Y := StrToIntDef(copy(obj, j*8+4, 3), 0);
       sym.SymbolID   := StrToIntDef(copy(obj, j*8+7, 2), 0);
       (blk as TUsek).Symbols.Add(sym);

       if (sym.Position.X >= w) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': symbol presahuje sirku reliefu');
       if (sym.Position.Y >= h) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': symbol presahuje vysku reliefu');
      end;//for j
     if ((blk as TUsek).Symbols.Count = 0) then Self.Errors.Add('WARNING: '+ExtractFileName(filename)+' Relief usek '+IntToStr(i)+': neni zadny symbol');

     (blk as TUsek).cislo_koleje := inifile.ReadString('U'+IntToStr(i),'N','');
     (blk as TUsek).spr_pos := (inifile.ReadString('U'+IntToStr(i), 'Spr', '') <> '');

     Self.AddGraphBlk(blk, id, usek);
    end;//for i

   // navestidla
   cnt := inifile.ReadInteger('P','N',0);
   for i := 0 to cnt-1 do
    begin
     id := inifile.ReadInteger('N'+IntToStr(i),'B',-1);

     if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief navestidlo '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TNavestidlo.Create();

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

   // pomocne symboly
   cnt := inifile.ReadInteger('P','P',0);
   for i := 0 to cnt-1 do
    begin
     id  := inifile.ReadInteger('P'+IntToStr(i),'B', -1);
     if (id > -1) then
      begin
       blk := TAC.Create();
       Self.AddGraphBlk(blk, id, TBlkType.ac);
      end;
    end;

   // vyhybky
   cnt := inifile.ReadInteger('P','V',0);
   for i := 0 to cnt-1 do
    begin
     id  := inifile.ReadInteger('V'+IntToStr(i),'B',-1);
     if (id = -1) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief vyhybka '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TVyhybka.Create();

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

   // prejezdy
   cnt := inifile.ReadInteger('P','PRJ',0);
   for i := 0 to cnt-1 do
    begin
     id  := inifile.ReadInteger('PRJ'+IntToStr(i), 'B', -1);
     if (id = -1) then Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief prejezd '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TPrejezd.Create();
     blk.OblRizeni := inifile.ReadInteger('PRJ'+IntToStr(i), 'OR', -1)+start_OR;

     obj := inifile.ReadString('PRJ'+IntToStr(i), 'BP', '');
     (blk as TPrejezd).BlikPositions.Clear();
     for j := 0 to (Length(obj) div 9)-1 do
      begin
       if (verWord >= _FILEVERSION_13) then
        begin
         bp.Pos.X := StrToIntDef(copy(obj, j*16+1, 3), 0);
         bp.Pos.Y := StrToIntDef(copy(obj, j*16+4, 3), 0);
         bp.PanelUsek := StrToIntDef(copy(obj, j*16+7, 10), 0);
        end else begin
         bp.Pos.X := StrToIntDef(copy(obj, j*9+1, 3), 0);
         bp.Pos.Y := StrToIntDef(copy(obj, j*9+4, 3), 0);
         bp.PanelUsek := StrToIntDef(copy(obj, j*9+7, 3), 0);
        end;

       (blk as TPrejezd).BlikPositions.Add(bp);
      end;//for j

     obj := inifile.ReadString('PRJ'+IntToStr(i), 'SP', '');
     (blk as TPrejezd).StaticPositions.Count := (Length(obj) div 6);
     for j := 0 to (blk as TPrejezd).StaticPositions.Count-1 do
      begin
       (blk as TPrejezd).StaticPositions.Add(Point(
         StrToIntDef(copy(obj, j*6+1, 3), 0),
         StrToIntDef(copy(obj, j*6+4, 3), 0)
       ))
      end;//for j

     Self.AddGraphBlk(blk, id, prejezd);
    end;//for i

   // popisky
   cnt := inifile.ReadInteger('P','T',0);
   for i := 0 to cnt-1 do
    begin
     id  := inifile.ReadInteger('T'+IntToStr(i),'B', -1);
     if (id < 0) then continue;

     blk := TPopisek.Create();

     (blk as TPopisek).Text       := inifile.ReadString('T'+IntToStr(i),'T', 'text');
     (blk as TPopisek).Position.X := inifile.ReadInteger('T'+IntToStr(i),'X', 0);
     (blk as TPopisek).Position.Y := inifile.ReadInteger('T'+IntToStr(i),'Y', 0);
     (blk as TPopisek).Color      := inifile.ReadInteger('T'+IntToStr(i),'C', 0);
     blk.OblRizeni                := inifile.ReadInteger('T'+IntToStr(i),'OR', -1)+start_OR;

     if ((id = -1) and (Length((blk as TPopisek).Text) = 1)) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief popisek '+IntToStr(i)+': neni navaznost na technologicky blok');
     Self.AddGraphBlk(blk, id, popisek);
    end;//for i

   // uvazky
   cnt := inifile.ReadInteger('P', 'Uv', 0);
   for i := 0 to cnt-1 do
    begin
     id := inifile.ReadInteger('Uv'+IntToStr(i),'B', -1);
     if (id = -1) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief uvazka '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TUvazka.Create();

     (blk as TUvazka).Pos.X       := inifile.ReadInteger('Uv'+IntToStr(i), 'X', 0);
     (blk as TUvazka).Pos.Y       := inifile.ReadInteger('Uv'+IntToStr(i), 'Y', 0);
     (blk as TUvazka).defalt_dir  := inifile.ReadInteger('Uv'+IntToStr(i), 'D', 0);
     blk.OblRizeni                := inifile.ReadInteger('Uv'+IntToStr(i), 'OR', -1) + start_OR;

     Self.AddGraphBlk(blk, id, uvazka);
    end;//for i

   // uvazky spr
   // uvazky spr se ve skutenocti nacitaji jako uavzky, aby doslo k merge
   // data o uvazky spr se tak skutecne nenactou, ale to nam nevadi - potrebujeme jen ID
   cnt := inifile.ReadInteger('P', 'UvS', 0);
   for i := 0 to cnt-1 do
    begin
     id := inifile.ReadInteger('UvS'+IntToStr(i),'B', -1);
     if (id = -1) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief uvazka spr '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TUvazka.Create();
     blk.OblRizeni := inifile.ReadInteger('UvS'+IntToStr(i), 'OR', -1)+start_OR;
     Self.AddGraphBlk(blk, id, uvazka);
    end;//for i

   // zamky
   cnt := inifile.ReadInteger('P','Z',0);
   for i := 0 to cnt-1 do
    begin
     id  := inifile.ReadInteger('Z'+IntToStr(i), 'B', -1);
     if (id = -1) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief zamek '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TZamek.Create();
     blk.OblRizeni := inifile.ReadInteger('Z'+IntToStr(i),'OR', -1) + start_OR;

     (blk as TZamek).Pos.X := inifile.ReadInteger('Z'+IntToStr(i), 'X', 0);
     (blk as TZamek).Pos.Y := inifile.ReadInteger('Z'+IntToStr(i), 'Y', 0);

     Self.AddGraphBlk(blk, id, zamek);
    end;

   // vykolejky
   cnt := inifile.ReadInteger('P', 'Vyk', 0);
   for i := 0 to cnt-1 do
    begin
     id  := inifile.ReadInteger('Vyk'+IntToStr(i), 'B', -1);
     if (id = -1) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief vykolejka '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TVykolejka.Create();
     blk.OblRizeni := inifile.ReadInteger('Vyk'+IntToStr(i),'OR', -1) + start_OR;

     (blk as TVykolejka).Pos.X := inifile.ReadInteger('Vyk'+IntToStr(i), 'X', 0);
     (blk as TVykolejka).Pos.Y := inifile.ReadInteger('Vyk'+IntToStr(i), 'Y', 0);
     (blk as TVykolejka).usek  := inifile.ReadInteger('Vyk'+IntToStr(i), 'O', -1);
     (blk as TVykolejka).usek  := inifile.ReadInteger('U'+IntToStr((blk as TVykolejka).usek), 'B', -1);

     Self.AddGraphBlk(blk, id, vykolejka);
    end;

   // rozpojovace
   cnt := inifile.ReadInteger('P','R',0);
   for i := 0 to cnt-1 do
    begin
     id  := inifile.ReadInteger('R'+IntToStr(i), 'B', -1);
     if (id = -1) then
       Self.Errors.Add('ERROR: '+ExtractFileName(filename)+' Relief rozpojovac '+IntToStr(i)+': neni navaznost na technologicky blok');
     if (id < 0) then continue;

     blk := TRozp.Create();
     blk.OblRizeni := inifile.ReadInteger('R'+IntToStr(i),'OR', -1) + start_OR;

     (blk as TRozp).Pos.X := inifile.ReadInteger('R'+IntToStr(i), 'X', 0);
     (blk as TRozp).Pos.Y := inifile.ReadInteger('R'+IntToStr(i), 'Y', 0);

     Self.AddGraphBlk(blk, id, rozp);
    end;

   Self.Errors.Add('--- Loading check complete ---');
 finally
   inifile.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// provede merge oblasti rizeni
procedure TRelief.Merge();
var orig, new:Integer;
begin
 while (true) do
  begin
   // hledame duplicity v unikatnich nazvech v poli oblasti rizeni
   Self.FindSimilarORs(orig, new);

   // pokud nenajdeme duplicitu, ukoncujeme merge
   if (orig = -1) then Exit;

   if (not Self.CheckSimilarORs(orig, new)) then
     Self.Errors.Add('ERROR: oblasti řízení s id ' + Self.ORs[orig].id + ' se neshodují v názvu nebo ve zkratce názvu!');

   // duplicita nalezena -> merge
   // zachovavame orig, new mazeme
   Self.MergeOROsv(Self.ORs[new], Self.ORs[orig]);
   Self.ReplaceBlkOR(new, orig);
   Self.DeleteOR(new);

   // a hledame znova....
 end;
end;

////////////////////////////////////////////////////////////////////////////////

//checking data valid
function TRelief.CheckValid():TStrings;
var str:string;
    j:Integer;
    nav, nav2:TNavestidlo;
    vyh:TVyhybka;
    blk:TGraphBlok;
    tblk:TTechBlok;
begin
 Self.Errors.Add('--- Spustena 2. faze validace: validator objektovych navaznosti ---');
 DateTimeToString(str,'yyyy-mm-dd hh:nn:ss',Now);
 Self.Errors.Add(str);

 //kontrola prirazeni oblasti rizeni
 for tblk in Self.TechBloky.Values do
  begin
   for blk in tblk.graph_blk do
     if (blk.OblRizeni >= Self.ORs.Count) then
       Self.Errors.Add('ERROR: Relief blok id '+IntToStr(tblk.id)+': definovana oblast rizeni, ktera neni v seznamu OR');

   /////////////
   //kontrola stejneho cisla koleje
   if (tblk.typ = usek) then
    begin
     for j := 0 to tblk.graph_blk.Count-2 do
      begin
       if (TUsek(tblk.graph_blk[j]).cislo_koleje <>
           TUsek(tblk.graph_blk[j+1]).cislo_koleje) then
          Self.Errors.Add('ERROR: Relief technologicky usek '+IntToStr(tblk.id)+
            ': ruzne cislo koleje v ruznych panelech!');
      end;
    end;

   /////////////
   //kontrola totoznosti orientace navestidel

   if (tblk.typ = navestidlo) then
    begin

     // navestidlo
     for j := 0 to tblk.graph_blk.Count-2 do
      begin
       nav := (tblk.graph_blk[j] as TNavestidlo);
       nav2 := (tblk.graph_blk[j+1] as TNavestidlo);
       case (nav.SymbolID) of
        0,4:begin
             case (nav.SymbolID) of
              0,4:if ((Self.ORs[nav.OblRizeni].Lichy xor 0) <> (Self.ORs[nav2.OblRizeni].Lichy xor 0)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(tblk.id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs[nav.OblRizeni].Name+', '+Self.ORs[nav2.OblRizeni].Name);
              1,5:if ((Self.ORs[nav.OblRizeni].Lichy xor 0) <> (Self.ORs[nav2.OblRizeni].Lichy xor 1)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(tblk.id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs[nav.OblRizeni].Name+', '+Self.ORs[nav2.OblRizeni].Name);
             end;//case j+1
         end;//case 0,4
        1,5:begin
             case (nav.SymbolID) of
              0,4:if ((Self.ORs[nav.OblRizeni].Lichy xor 1) <> (Self.ORs[nav2.OblRizeni].Lichy xor 0)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(tblk.id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs[nav.OblRizeni].Name+', '+Self.ORs[nav2.OblRizeni].Name);
              1,5:if ((Self.ORs[nav.OblRizeni].Lichy xor 1) <> (Self.ORs[nav2.OblRizeni].Lichy xor 1)) then
                    Self.Errors.Add('ERROR: Relief technologicke navestidlo '+IntToStr(tblk.id)+': navestidlo nema stejny smer ve stanicich '+Self.ORs[nav.OblRizeni].Name+', '+Self.ORs[nav2.OblRizeni].Name);
             end;//case j+1
         end;//case 1,5
       end;//case i
      end;// for j

    end;//if navestidlo

   /////////////
   //kontrola totoznosti navaznosti vyhybek

   if (tblk.typ = vyhybka) then
    begin
     for j := 0 to tblk.graph_blk.Count-2 do
      begin
       vyh := (tblk.graph_blk[j] as TVyhybka);
       if (vyh.obj <> vyh.obj) then
         Self.Errors.Add('ERROR: Relief technologicka vyhybka '+IntToStr(tblk.id)+': vyhybka ma navaznost na jiny blok ve stanicich '+Self.ORs[vyh.OblRizeni].Name+', '+Self.ORs[vyh.OblRizeni].Name);
      end;//for j

    end;//vyhybka
  end;//for i

 Self.Errors.Add('--- Validace hotova. ---');
 Result := Self.Errors;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.ExportData(const filename:string);
var ini:TMemIniFile;
    i,j:Integer;
    str:string;
    tblk:TTechBlok;
    oblr:TOR;
begin
 DeleteFile(filename);
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);

 try
   // OR
   for i := 0 to Self.ORs.Count-1 do
    begin
     oblr := Self.ORs[i];
     // ukladame specificky upravena data, protoze smysl ma ukladat jen data u vsech OR stajna
     // = nazev, zkratka a unikatni nazev
     str := oblr.Name + ';' + oblr.ShortName + ';' + oblr.id + ';';
     for j := 0 to oblr.Osvetleni.Count-1 do
       str := str + '(' + IntToStr(oblr.Osvetleni[j].board) + '|' + IntToStr(oblr.Osvetleni[j].port) + '|' + oblr.Osvetleni[j].name + ')';
     str := str + ';';
     ini.WriteString('OR', IntToStr(i), str);
    end;

   // useky
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> usek) then continue;

     str := '';

     //sestaveni OR
     str := str + Self.GetTechBlkOR(tblk) + ';';

     if ((tblk.graph_blk[0] as TUsek).cislo_koleje <> '') then
      str := str + '1;' + TUsek(tblk.graph_blk[0]).cislo_koleje + ';'
     else
      str := str + '0;;';

     if ((tblk.graph_blk[0] as TUsek).spr_pos) then
      str := str + '1;'
     else
      str := str + '0;';

     ini.WriteString('U', IntToStr(tblk.id), str);
    end;

   // navestidla
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> navestidlo) then continue;

     str := '';

     //sestaveni OR
     str := str + Self.GetTechBlkOR(tblk) + ';';

     //typ symbolu
     case ((tblk.graph_blk[0] as TNavestidlo).SymbolID) of
      0,1:str := str + '0;';
      //2,3 jsou AB
      4,5:str := str + '1;';
     end;//case

    case (Self.ORs[tblk.graph_blk[0].OblRizeni].Lichy xor ((tblk.graph_blk[0] as TNavestidlo).SymbolID mod 2)) of
     0:str := str + '0;';
     1:str := str + '1;';
    end;

     //usek pred id
     str := str + IntToStr((tblk.graph_blk[0] as TNavestidlo).UsekPred);

     ini.WriteString('N',IntToStr(tblk.id), str);
    end;

   // vyhybky
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> vyhybka) then continue;

     str := '';

     //sestaveni OR
     str := str + Self.GetTechBlkOR(tblk) + ';';

     //navaznost na usek
     str := str + IntToStr((tblk.graph_blk[0] as TVyhybka).obj)+';';

     ini.WriteString('V',IntToStr(tblk.id),str);
    end;

   // prejezdy
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> prejezd) then continue;

     str := '';

     //sestaveni OR
     str := str + Self.GetTechBlkOR(tblk) + ';';

     ini.WriteString('PRJ', IntToStr(tblk.id), str);
    end;

   // popisky
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> popisek) then continue;

     if (Length((tblk.graph_blk[0] as TPopisek).Text) <> 1) then continue;

     str := '';

     //sestaveni OR
     str := str + Self.GetTechBlkOR(tblk) + ';';

     ini.WriteString('T', IntToStr(tblk.id), str);
    end;

   // uvazky
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> uvazka) then continue;

     //sestaveni OR
     str := '';
     str := str + Self.GetTechBlkOR(tblk) + ';';

     ini.WriteString('Uv', IntToStr(tblk.id), str);
    end;

   // zamky
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> zamek) then continue;

     //sestaveni OR
     str := '';
     str := str + Self.GetTechBlkOR(tblk) + ';';

     ini.WriteString('Z', IntToStr(tblk.id), str);
    end;

   // vykolejky
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> vykolejka) then continue;

     //sestaveni OR
     str := '';
     str := str + Self.GetTechBlkOR(tblk) + ';';

     //navaznost na usek
     str := str + IntToStr((tblk.graph_blk[0] as TVykolejka).usek)+';';

     ini.WriteString('V', IntToStr(tblk.id), str); // ulozit jako vyhybky
    end;

   // rozpojovace
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> rozp) then continue;

     //sestaveni OR
     str := '';
     str := str + Self.GetTechBlkOR(tblk) + ';';

     ini.WriteString('R', IntToStr(tblk.id), str);
    end;

   // AC
   for tblk in Self.TechBloky.Values do
    begin
     if (tblk.typ <> AC) then continue;

     //sestaveni OR
     str := '';
     str := str + Self.GetTechBlkOR(tblk) + ';';

     ini.WriteString('AC', IntToStr(tblk.id), str);
    end;

 finally
   ini.UpdateFile();
   ini.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// vraci technologicke ID bloku, ktery je na pozici UsekPos a v OR oblRizeni
// vyuzivano pri ziskavani bloku pred navestidly
function TRelief.GetUsekPredID(UsekPos:TPoint; oblRizeni:Integer):Integer;
var blk:TGraphBlok;
    sym:TReliefSym;
    tblk:TTechBlok;
begin
 // prijdeme vsechny technologicke useky
 for tblk in Self.TechBloky.Values do
  begin
   if (tblk.typ <> usek) then continue;
   
   // projdeme vsechny useky na reliefu daneho tech. useku
   for blk in tblk.graph_blk do
    begin
     if (blk.OblRizeni = oblRizeni) then
      begin
       // tady mame jisto, ze jsme ve spravne oblasti rizeni
       // prohleddame vsechny symboly bloku
       for sym in (blk as TUsek).Symbols do
         if ((UsekPos.X = sym.Position.X) and (UsekPos.Y = sym.Position.Y)) then
           Exit(tblk.id);
      end;//if oblRizeni
    end;//for j
  end;//for i

 Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////

function TRelief.GetTechBlkOR(tblk:TTechBlok):string;
var i,j:Integer;
    ret:array [0.._MAX_OR-1] of Integer;
    retcnt:Integer;
    found:boolean;
    blk:TGraphBlok;
begin
 Result := '';

 retcnt := 0;
 for blk in tblk.graph_blk do
  begin
   found := false;
   for j := 0 to retcnt-1 do if (ret[j] = blk.OblRizeni) then found := true;

   if (not found) then
    begin
     retcnt := retcnt + 1;
     ret[retcnt-1] := blk.OblRizeni;
    end;
  end;//for i

 for i := 0 to retcnt-1 do
   Result := Result + Self.ORs[ret[i]].id + '|';
end;

////////////////////////////////////////////////////////////////////////////////
// pridava blok v parametru do technologickych bloku:

procedure TRelief.AddGraphBlk(data:TGraphBlok; id:Integer; typ:TBlkType);
begin
 if (not Self.TechBloky.ContainsKey(id)) then
  Self.TechBloky.Add(id, TTechBlok.Create(typ));
 Self.TechBloky[id].graph_blk.Add(data);
 Self.TechBloky[id].id := id;
end;

////////////////////////////////////////////////////////////////////////////////

// nahradi oblast rizeni u bloku s orig_or na new_or
// pouzivano pri merge oblasti rizeni
procedure TRelief.ReplaceBlkOR(orig_or:Integer; new_or:Integer);
var i, j:Integer;
begin
 for i := 0 to Self.TechBloky.Count-1 do
   for j := 0 to Self.TechBloky[i].graph_blk.Count-1 do
     if (Self.TechBloky[i].graph_blk[j].OblRizeni = orig_or) then
       Self.TechBloky[i].graph_blk[j].OblRizeni := new_or;
end;

////////////////////////////////////////////////////////////////////////////////

// maze oblast rizeni
// vyuzivano pri merge oblsti rizeni
procedure TRelief.DeleteOR(index:Integer);
var i, j:Integer;
begin
 Self.ORs.Delete(index);

 // vsem blokum, ktere mely OR tuto a vesti musime dekrementovat OR
 for i := 0 to Self.TechBloky.Count-1 do
   for j := 0 to Self.TechBloky[i].graph_blk.Count-1 do
     if (Self.TechBloky[i].graph_blk[j].OblRizeni >= index) then
       Self.TechBloky[i].graph_blk[j].OblRizeni := Self.TechBloky[i].graph_blk[j].OblRizeni-1;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TTechBlok.Create(typ:TBlkType);
begin
 inherited Create();
 Self.typ := typ;
 Self.graph_blk := TObjectList<TGraphBlok>.Create();
end;

destructor TTechBlok.Destroy();
begin
 Self.graph_blk.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.FindSimilarORs(var or1:Integer; var or2:Integer);
var i,j:Integer;
begin
 or1 := -1;
 or2 := -1;

 for i := 0 to Self.ORs.Count-1 do
  begin
   for j := 0 to Self.ORs.Count-1 do
    begin
     if (i <> j) and (Self.ORs[i].id = Self.ORs[j].id) then
      begin
       or1 := i;
       or2 := j;
       Exit();
      end;
    end;//for j
  end;//for i
end;

// porovnavame vsechny potrebne atributy a podle nich zjistujeme, jestli jsou OR opravdu totozne
// pokud narazime na zmeny, pridame je do logu (Self.Errors)
function TRelief.CheckSimilarORs(or1:Integer; or2:Integer):Boolean;
begin
 // OR musi mit POUZE stejny nazev, zkratku nazvu a unikatni nazev
 //   predpoklada se, ze OR v argumentu uz maji stejny unikatni nazev
 //   (na zaklade toho probiha vyhodnoceni potencialne stejnych OR)
 // osvetleni a vse ostatni muze byt jine

 Result := not ((Self.ORs[or1].Name <> Self.ORs[or2].Name) or
                (Self.ORs[or1].ShortName <> Self.ORs[or2].ShortName));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.MergeOROsv(orig_or:TOR; new_or:TOR);
var found:boolean;
    osvo, osvn: TOsv;
begin
 for osvo in orig_or.Osvetleni do
  begin
   found := false;
   for osvn in new_or.Osvetleni do
     if (osvn.name = osvo.name) then
       found := true;
       // sem by mohla prijit kontrola konzistence MTB vstupu a vystupu

   if (not found) then
     new_or.Osvetleni.Add(osvo);
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

class function TRelief.FileSupportedVersionsStr():string;
var i: Integer;
begin
 Result := '';
 for i := 0 to Length(_FileVersion_accept)-1 do
   Result := Result + _FileVersion_accept[i] + ', ';
 Result := LeftStr(Result, Length(Result)-2);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

