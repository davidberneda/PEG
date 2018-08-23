// @davidberneda
// 2017
unit TeePEG_Grammar;

{
  "Parsing Expression Grammars" (PEG)

  Implementation for: Mr. Bryan Ford baford@mit.edu
  http://bford.info/pub/lang/peg.pdf
}

interface

uses
  TeePEG_Rules;

type
  TGrammar=class(TPrioritized)
  private
    FParser : TParser;

    function Load:TGrammar; overload;
  protected
    Internal : TRuleArray;

    function AddInternal(const ARule:TRule):TRule;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Add(const ARule:TRule);
    function AsString:String; override;

    function From(const AItem:PSyntaxItem):TGrammar;

    function Load(const S:String):TGrammar; overload;

    property Parser:TParser read FParser;
  end;

implementation

uses
  SysUtils;

{ TGrammar }

Constructor TGrammar.Create;
begin
  inherited Create([]);
  FParser:=TParser.Create;
end;

Destructor TGrammar.Destroy;

  procedure FreeRules(const ARules:TRuleArray);
  var t : Integer;
  begin
    for t:=Low(ARules) to High(ARules) do
        ARules[t].Free;
  end;

begin
  FreeRules(Items);
  FreeRules(Internal);

  FParser.Free;
  inherited;
end;

function RulesFrom(const AParser:TParser; const AItems:PSyntaxItems):TRuleArray;
var t,L : Integer;
    tmpItem : PSyntaxItem;
    tmpName : String;
    tmpRule : TRule;
begin
  L:=Length(AItems);
  SetLength(result,L);

  for t:=0 to L-1 do
  begin
    tmpItem:=AItems[t].Items[0].Items[0].Items[0];
    tmpName:=AParser.Extract(tmpItem);

    result[t]:=TNamedRule.Create(tmpName,nil);
  end;

  // Inverse, create rules:
  for t:=L-1 downto 0 do
  begin
    tmpItem:=AItems[t].Items[0].Items[2]; // Expression

    tmpRule:=nil; // <-- use tmpItem !! ??

    TNamedRule(result[t]).Rule:=tmpRule;
  end;
end;

function TGrammar.From(const AItem: PSyntaxItem): TGrammar;
var tmp : PSyntaxItems;
begin
  result:=TGrammar.Create;

  tmp:=AItem.Items[1].Items;
  result.Items:=RulesFrom(Parser,tmp);
end;

// Internal array only purpose is to free memory leaks at Destroy
function TGrammar.AddInternal(const ARule: TRule):TRule;
var L : Integer;
begin
  L:=Length(Internal);
  SetLength(Internal,L+1);
  Internal[L]:=ARule;

  result:=ARule;
end;

function TGrammar.AsString: String;
const
  LeftArrow=' <- ';

var t : Integer;
    tmpRule : TRule;
begin
  result:='';

  for t:=0 to High(Items) do
  begin
    if t>0 then
       result:=result+#13#10;

    if Items[t] is TNamedRule then
    begin
      result:=result+TNamedRule(Items[t]).Name+LeftArrow;

      tmpRule:=TOperator(Items[t]).Rule;

      if tmpRule<>nil then
         result:=result+tmpRule.AsString;
    end
    else
      result:=result+'Error:'+LeftArrow+Items[t].AsString;
  end;
end;

function TGrammar.Load(const S: String):TGrammar;
begin
  FParser.Start(S);
  result:=Load;
end;

function TGrammar.Load:TGrammar;
var
  Start : Integer;

  procedure DoError(const S:String);
  begin
    raise Exception.Create(S+' At: '+IntToStr(Start));
  end;

var tmp : PSyntaxItem;
begin
  result:=nil;

  FParser.Position:=1;

  repeat
    Start:=FParser.Position;

    tmp:=Match(FParser);

    if tmp=nil then
       DoError('Invalid syntax')
    else
    if FParser.Position=Start then
       DoError('Infinite loop')
    else
    if tmp.Rule is TGrammar then
    begin
      tmp.Length:=0;

      FParser.Syntax:=tmp;
      result:=From(tmp.Items[0].Items[0]);
      Exit;
    end
    else
      DoError('Grammar expected. Found: '+tmp.Rule.ClassName);

  until FParser.EndOfFile;
end;

procedure TGrammar.Add(const ARule: TRule);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=ARule;
end;

end.
