unit fDataCheck;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StrUtils;

type
  TF_DataCheck = class(TForm)
    LV_Errors: TListView;
    procedure LV_ErrorsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private declarations }
  public
    procedure OpenForm(data:TStrings);
  end;

var
  F_DataCheck: TF_DataCheck;

implementation

{$R *.dfm}

procedure TF_DataCheck.LV_ErrorsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 Self.LV_Errors.Canvas.Brush.Color := $FFFFFF;

 if (LeftStr(Item.SubItems.Strings[0],5) = 'ERROR') then Self.LV_Errors.Canvas.Brush.Color := $CCCCFF;
 if (LeftStr(Item.SubItems.Strings[0],5) = 'WARNI') then Self.LV_Errors.Canvas.Brush.Color := $CCFFFF;
end;

procedure TF_DataCheck.OpenForm(data:TStrings);
var i:Integer;
    LI:TListItem;
begin
 Self.LV_Errors.Clear();

 for i := 0 to data.Count-1 do
  begin
   Li := Self.LV_Errors.Items.Add;
   LI.Caption := IntToStr(i);
   LI.SubItems.Add(data[i]);
  end;//for i

 Self.Show();
end;//procedure

end.//unit
