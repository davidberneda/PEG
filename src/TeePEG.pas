// @davidberneda
// 2017
unit TeePEG;

{
  "Parsing Expression Grammars" (PEG)

  Implementation for: Mr. Bryan Ford baford@mit.edu
  http://bford.info/pub/lang/peg.pdf
}

interface

uses
  Classes, TeePEG_Rules;

type
  TRules=class(TChoice)
  private
    FParser : TParser;

    procedure Load; overload;
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Add(const ARule:TRule);
    function AsString:String; override;
    function Find:TRule;
    procedure Load(const S:String); overload;

    property Parser:TParser read FParser;
  end;

  TPEG=class
  public
    Rules : TRules;

    Constructor Create;
    Destructor Destroy; override;

    procedure Load(const S:String); overload;
    procedure Load(const S:TStrings); overload;
  end;

implementation

uses
  SysUtils, TeePEG_Grammar;

{ TRules }

Constructor TRules.Create;
begin
  inherited Create([]);
  FParser:=TParser.Create;
end;

Destructor TRules.Destroy;
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      Items[t].Free;

  FParser.Free;
  inherited;
end;

function TRules.AsString: String;
const
  LeftArrow=' <- ';

var t : Integer;
begin
  result:='';

  for t:=0 to High(Items) do
  begin
    if t>0 then
       result:=result+#13#10;

    if Items[t] is TNamedRule then
    begin
      result:=result+TNamedRule(Items[t]).Name;
      result:=result+LeftArrow+TOperator(Items[t]).Rule.AsString;
    end
    else
      result:=result+'Error:'+LeftArrow+Items[t].AsString;
  end;
end;

function TRules.Match(const AParser: TParser): Boolean;
begin
  result:=False;
end;

procedure TRules.Load(const S: String);
begin
  FParser.Text:=S;
  Load;
end;

type
  TRuleAccess=class(TRule);

function TRules.Find: TRule;
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      if TRuleAccess(Items[t]).Match(FParser) then
      begin
        result:=Items[t];
        Exit;
      end;

  result:=nil;
end;

procedure TRules.Load;
var Start : Integer;
    tmp : TRule;
begin
  FParser.Position:=1;

  repeat
    Start:=FParser.Position;

    tmp:=Find;

    if tmp=nil then
       raise Exception.Create('Invalid syntax at position: '+IntToStr(Start))
    else
    if FParser.Position=Start then
       raise Exception.Create('Inifite loop at position: '+IntToStr(Start));

  until FParser.EndOfFile;
end;

procedure TRules.Add(const ARule: TRule);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=ARule;
end;

{ TPEG }

Constructor TPEG.Create;
begin
  inherited Create;
  Rules:=TRules.Create;
  TGrammar.AddTo(Rules.Items);
end;

Destructor TPEG.Destroy;
begin
  Rules.Free;
  inherited;
end;

procedure TPEG.Load(const S: String);
begin
  Rules.Load(S);
end;

procedure TPEG.Load(const S: TStrings);
begin
  Load(S.Text);
end;

end.
