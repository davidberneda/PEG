unit Unit_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TeePeg, ComCtrls;

type
  TForm1 = class(TForm)
    Memo2: TMemo;
    Button1: TButton;
    Memo3: TMemo;
    PageControl1: TPageControl;
    TabPEG: TTabSheet;
    TabMath: TTabSheet;
    MathPEG: TMemo;
    PEGPEG: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

  procedure ShowStack(const AParser:TParser);

    procedure ShowRules(const AIdent:String; const ARules:Array of TRule; const APos:Integer);
    var t : Integer;
        tmp : String;
    begin
      for t:=0 to High(ARules) do
      begin
        if ARules[t] is TNamedRule then
           tmp:=TNamedRule(ARules[t]).Name
        else
           tmp:=ARules[t].ClassName;

        AStrings.Add(AIdent+tmp+' -> '+IntToStr(APos));
      end;
    end;

  var Ident : String;
      t : Integer;
  begin
    Ident:='';

    for t:=Low(AParser.Stack) to High(AParser.Stack) do
    begin
      ShowRules(Ident,AParser.Stack[t].Rules,AParser.Stack[t].Position);
      Ident:=Ident+' ';
    end;
  end;

begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;

    ShowStack(Peg.Rules.Parser);
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PEG_Log:=Memo3.Lines;

  PEG:=TPEG.Create;

  Memo2.Lines.Text:=PEG.Rules.AsString;
end;

procedure TForm1.Button1Click(Sender: TObject);

  function FirstLines(const Quantity:Integer):String;
  var t : Integer;
  begin
    result:='';

    for t:=0 to Quantity-1 do
    begin
      if t>0 then
         result:=result+#13#10;

      result:=result+PEGPEG.Lines[t];
    end;
  end;

var tmp : String;
begin
  PEG_Log.BeginUpdate;
  try
    PEG_Log.Clear;

    //tmp:=Memo1.Lines[21]; //FirstLines(22);

    // tmp:='11';

    if PageControl1.ActivePage=TabPEG then
       tmp:=PEGPEG.Text
    else
       tmp:=MathPEG.Text;

    //tmp:='\t';

    try
      PEG.Load(tmp{Memo1.Lines});
    finally
      AddTokens(PEG, Memo2.Lines);

      Memo2.Lines.Add('');
      Memo2.Lines.Add('Parsed: '+tmp);
    end;

  finally
    PEG_Log.EndUpdate;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  PEG.Free;
end;

end.
