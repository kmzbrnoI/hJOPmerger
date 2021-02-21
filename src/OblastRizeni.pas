unit OblastRizeni;

// deklarace struktur Oblasti rizeni

interface

uses Types, Generics.Collections, Classes, SysUtils, Generics.Defaults;

const
  _MAX_OR = 256;
  _MAX_OSV = 8;

type
  // 1 osvetleni
  TOsv = record
    board: Byte;
    port: Byte;
    name: string; // max 5 znaku
  end;

  // prava
  TORRights = record
    ModCasStart: Boolean;
    ModCasStop: Boolean;
    ModCasSet: Boolean;
  end;

  // pozice symbolu OR
  TPoss = record
    DK: TPoint;
    DKOr: Byte; // orientace DK (0,1)
    Queue: TPoint;
    Time: TPoint;
  end;

  EInvalidData = class(Exception);

  // 1 oR
  TOR = class
    str: string;
    name: string;
    ShortName: string;
    id: string;
    LichyLtoR: Boolean;
    Rights: TORRights;
    Osvetleni: TList<TOsv>;
    Poss: TPoss;

    constructor Create(const str: string);
    destructor Destroy(); override;
    procedure Load(const str: string);

    class function NameComparer(): IComparer<TOR>;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TOR.Create(const str: string);
begin
  inherited Create();
  Self.Osvetleni := TList<TOsv>.Create();
  Self.Load(str);
end;

destructor TOR.Destroy();
begin
  Self.Osvetleni.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TOR.Load(const str: string);
var data_main, data_osv, data_osv2: TStrings;
  j: Integer;
  Osv: TOsv;
  Pos: TPoint;
begin
  data_main := TStringList.Create();
  data_osv := TStringList.Create();
  data_osv2 := TStringList.Create();

  try
    ExtractStrings([';'], [], PChar(str), data_main);

    if (data_main.Count < 14) then
      raise EInvalidData.Create('Prilis malo polozek v zaznamu oblasti rizeni!');

    Self.name := data_main[0];
    Self.ShortName := data_main[1];
    Self.id := data_main[2];
    Self.LichyLtoR := StrToBool(data_main[3]);
    Self.Poss.DKOr := StrToInt(data_main[4]);

    Self.Rights.ModCasStart := StrToBool(data_main[5]);
    Self.Rights.ModCasStop := StrToBool(data_main[6]);
    Self.Rights.ModCasSet := StrToBool(data_main[7]);

    Self.Poss.DK.X := StrToInt(data_main[8]);
    Self.Poss.DK.Y := StrToInt(data_main[9]);

    Pos.X := StrToInt(data_main[10]);
    Pos.Y := StrToInt(data_main[11]);

    Self.Poss.Time.X := StrToInt(data_main[12]);
    Self.Poss.Time.Y := StrToInt(data_main[13]);

    Self.Osvetleni.Clear();
    data_osv.Clear();
    if (data_main.Count >= 15) then
    begin
      ExtractStrings(['|'], [], PChar(data_main[14]), data_osv);
      for j := 0 to data_osv.Count - 1 do
      begin
        data_osv2.Clear();
        ExtractStrings(['#'], [], PChar(data_osv[j]), data_osv2);

        if (data_osv2.Count < 2) then
          continue;

        Osv.board := StrToInt(data_osv2[0]);
        Osv.port := StrToInt(data_osv2[1]);
        if (data_osv2.Count > 2) then
          Osv.name := data_osv2[2]
        else
          Osv.name := '';
        Self.Osvetleni.Add(Osv);
      end; // for j
    end; // .Count >= 15
  finally
    data_main.Free();
    data_osv.Free();
    data_osv2.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TOR.NameComparer(): IComparer<TOR>;
begin
  Result := TComparer<TOR>.Construct(
    function(const Left, Right: TOR): Integer
    begin
      Result := CompareStr(Left.name, Right.name, loUserLocale);
    end);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
