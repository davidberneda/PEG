unit Unit_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TeePeg;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    PEG : TPEG;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  TeePEG_Rules;

procedure AddTokens(const PEG:TPeg; const AStrings:TStrings);
var t : Integer;
    tmp : String;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;

    for t:=Low(Peg.Tokens) to High(Peg.Tokens) do
    begin
      if PEG.Tokens[t].Rule is TNamedRule then
         tmp:=TNamedRule(PEG.Tokens[t].Rule).Name
      else
         tmp:='';

      AStrings.Add(tmp+' -> '+PEG.Rules.TokenText(Peg.Tokens[t]));
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);

  function FirstLines(const Quantity:Integer):String;
  var t : Integer;
  begin
    result:='';

    for t:=0 to Quantity-1 do
    begin
      if t>0 then
         result:=result+#13#10;

      result:=result+Memo1.Lines[t];
    end;
  end;

var tmp : String;
begin
  PEG:=TPEG.Create;

  Memo2.Lines.Text:=PEG.Rules.AsString;

  tmp:=Memo1.Lines[21]; //FirstLines(21);

  PEG.Load(tmp);

  AddTokens(PEG, Memo2.Lines);


  PEG.Free;
end;

end.
