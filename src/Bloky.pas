unit Bloky;

interface

uses Types, Generics.Collections;

type

  TBlkType = (usek, navestidlo, vyhybka, prejezd, popisek, uvazka, uvazka_spr, zamek, vykolejka, rozp, pomocny, pst);

  // abstraktni trida, ze ktere dedi graficke bloky
  TGraphBlok = class
    OblRizeni: Integer;
  end;

  TVyhybka = class(TGraphBlok)
    PolohaPlus: Byte;
    Position: TPoint;
    SymbolID: Integer;
    obj: Integer; // technologicke id useku, na kterem vyhybka je
  end;

  TReliefSym = record
    Position: TPoint;
    SymbolID: Integer;
  end;

  TUsek = class(TGraphBlok)
    Symbols: TList<TReliefSym>;
    cislo_koleje: string;
    spr_pos: boolean;
    // zbytek neni potreba

    constructor Create();
    destructor Destroy(); override;
  end;

  // graficky blok navestidlo
  TNavestidlo = class(TGraphBlok)
    Blok: Integer;
    Position: TPoint;
    SymbolID: Integer;
    OblRizeni: Integer;
    Sudy: boolean;

    UsekPred: Integer; // technologicky usek

    function SymbolDirLeft(): boolean;
  end; // Navestidlo

  TBlikPoint = record
    Pos: TPoint;
    PanelUsek: Integer; // jaky usek panelu ma tato cast prejezdu
  end;

  TPrejezd = class(TGraphBlok)
    Blok: Integer;
    StaticPositions: TList<TPoint>;
    BlikPositions: TList<TBlikPoint>;
    OblRizeni: Integer;

    constructor Create();
    destructor Destroy(); override;
  end;

  TPopisek = class(TGraphBlok)
    Text: string;
    Position: TPoint;
    Color: Integer;
  end;

  TUvazka = class(TGraphBlok)
    Pos: TPoint;
    defalt_dir: Integer;
    OblRizeni: Integer;
  end;

  TUvazkaSprVertDir = (top = 0, bottom = 1);

  TUvazkaSpr = class(TGraphBlok)
    Pos: TPoint;
    vertical_dir: TUvazkaSprVertDir;
    spr_cnt: Integer;
    OblRizeni: Integer;
  end;

  TZamek = class(TGraphBlok)
    Pos: TPoint;
  end;

  TVykolejka = class(TGraphBlok)
    Blok: Integer;
    Pos: TPoint;
    usek: Integer; // index useku, na kterem je vykolejka
    // zbytek irelevantni
  end;

  TRozp = class(TGraphBlok)
    Pos: TPoint;
  end;

  TPomocny = class(TGraphBlok)
    Pos: TPoint;
  end;

  TPSt = class(TGraphBlok)
    Pos: TPoint;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TUsek.Create();
begin
  inherited;
  Self.Symbols := TList<TReliefSym>.Create();
end;

destructor TUsek.Destroy();
begin
  Self.Symbols.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPrejezd.Create();
begin
  inherited;
  Self.StaticPositions := TList<TPoint>.Create();
  Self.BlikPositions := TList<TBlikPoint>.Create();
end;

destructor TPrejezd.Destroy();
begin
  Self.StaticPositions.Free();
  Self.BlikPositions.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TNavestidlo.SymbolDirLeft(): boolean;
begin
  Result := (Self.SymbolID = 1) or (Self.SymbolID = 5);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
