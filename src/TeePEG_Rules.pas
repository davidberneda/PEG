// @davidberneda
// 2017
unit TeePEG_Rules;

{$DEFINE TRACE}

{
  "Parsing Expression Grammars" (PEG)

  Implementation for: Mr. Bryan Ford baford@mit.edu
  http://bford.info/pub/lang/peg.pdf
}

interface

uses
  Classes;

type
  TRule=class;

  TStackItem=record
    Position : Integer;
    Rules : Array of TRule;
  end;

  TParser=class
  private
    FPosition : Integer;
  protected
    procedure Match(const ARule:TRule; const ALength:Integer);
    function Push:Integer;
    procedure Pop;
  public
    Stack : Array of TStackItem;
    Text : String;

    function EndOfFile: Boolean; {$IFDEF INLINE}inline;{$ENDIF}
    property Position:Integer read FPosition write FPosition;
  end;

  TRule=class
  protected
    function Match(const AParser:TParser):Boolean; virtual; abstract;
  public
    function AsString:String; virtual; abstract;
  end;

  TOperator=class(TRule)
  public
    Rule : TRule;

    Constructor Create(const ARule:TRule);
    function AsString:String; override;
  end;

  TPredicate=class(TOperator)
  end;

  { &e }
  TAndPredicate=class(TPredicate)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  { !e }
  TNotPredicate=class(TPredicate)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  TCharacterRule=class(TRule)
  protected
    InSet : Boolean;
  end;

  { 'x' }
  TCharacter=class(TCharacterRule)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    Character : Char;

    Constructor Create(const AChar:Char);
    function AsString:String; override;
  end;

  { n-m }
  TCharacterRange=class(TCharacterRule)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    Start,
    Finish : Char;

    Constructor Create(const AStart,AFinish:Char);
    function AsString:String; override;
  end;

  { 'abc' }
  TString=class(TRule)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    Text : String;

    Constructor Create(const AText:String);
    function AsString:String; override;
  end;

  { . }
  TAnyCharacter=class(TRule)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  TRuleArray=Array of TRule;

  TChoice=class(TRule)
  private
    function ToString(const Separator:String):String;
  protected
    procedure Add(const AItems: array of TRule);
  public
    Items : TRuleArray;

    Constructor Create(const AItems:Array of TRule);
  end;

  { e1 e2 }
  TSequence=class(TChoice)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  { e1 / e2 }
  TPrioritized=class(TChoice)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  { [abcA-ZQRS_] }
  TCharacterSet=class(TPrioritized)
  protected
    constructor InnerCreate;
  public
    Constructor Create(const AItems: array of TCharacterRule);

    function AsString:String; override;
  end;

  { e* }
  TZeroOrMore=class(TOperator)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  { e+ }
  TOneOrMore=class(TOperator)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  { e? }
  TOptional=class(TOperator)  // Zero or One
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    function AsString:String; override;
  end;

  TNamedRule=class(TOperator)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    Name : String;

    Constructor Create(const AName:String; const ARule:TRule);
    function AsString:String; override;
  end;

var
  SingleQuote,
  DoubleQuote : TCharacter;

  {$IFDEF TRACE}
  PEG_Log : TStrings;
  {$ENDIF}

implementation

uses
  SysUtils;

procedure Trace(const ARule:TRule; const AStart:Integer; const AParser: TParser);
begin
  {$IFDEF TRACE}
  if PEG_Log<>nil then
     PEG_Log.Add(IntToStr(AParser.Position)+' { '+
          Copy(AParser.Text,AStart,AParser.Position-AStart)+' } '+
          ARule.ClassName+': '+ARule.AsString);
  {$ENDIF}
end;

{ TParser }

function TParser.EndOfFile: Boolean;
begin
  result:=FPosition>Length(Text);
end;

{ TSequenceRule }

function TSequence.AsString: String;
begin
  result:=ToString(' ');
end;

function TSequence.Match(const AParser:TParser): Boolean;
var t : Integer;
    tmp : Integer;
begin
  tmp:=AParser.Push;

  for t:=Low(Items) to High(Items) do
      if not Items[t].Match(AParser) then
      begin
        result:=False;
        AParser.Pop;
        Exit;
      end;

  result:=True;

  Trace(Self,tmp,AParser);
end;

procedure TParser.Match(const ARule: TRule; const ALength: Integer);
var L,tmp : Integer;
begin
  L:=High(Stack);

  tmp:=Length(Stack[L].Rules);
  SetLength(Stack[L].Rules,tmp+1);
  Stack[L].Rules[tmp]:=ARule;

  Inc(FPosition,ALength);
end;

procedure TParser.Pop;
var H : Integer;
begin
  H:=High(Stack);

  if H<0 then
     raise Exception.Create('Error: Parser Stack empty');

  FPosition:=Stack[H].Position;

  SetLength(Stack,H);
end;

function TParser.Push: Integer;
var L : Integer;
begin
  L:=Length(Stack);
  SetLength(Stack,L+1);

  Stack[L].Position:=Position;

  result:=Position;
end;

{ TOperator }

Constructor TOperator.Create(const ARule: TRule);
begin
  inherited Create;
  Rule:=ARule;
end;

function TOperator.AsString: String;
begin
  if (Rule is TChoice) and (Length(TChoice(Rule).Items)>1) then
     result:='('+Rule.AsString+')'
  else
     result:=Rule.AsString;
end;

{ TCharacter }

Constructor TCharacter.Create(const AChar: Char);
begin
  inherited Create;
  Character:=AChar;
end;

function ToString(const C:Char):String;
begin
  if C=SingleQuote.Character then
     result:=C
  else
  if C='[' then
     result:='\['
  else
  if C=']' then
     result:='\]'
  else
  if (C<' ') or (C>'z') then
     case C of
        #9: result:='\t';
       #10: result:='\n';
       #13: result:='\r';
     else
       result:='#'+IntToStr(Ord(C))
     end
  else
  if C='\' then
     result:='\\'
  else
     result:=C;
end;

function TCharacter.AsString: String;
begin
  if Character=SingleQuote.Character then
     if InSet then
        result:=SingleQuote.Character
     else
        result:='['+SingleQuote.Character+']'
  else
  if InSet then
     result:=ToString(Character)
  else
     result:=SingleQuote.Character+ToString(Character)+SingleQuote.Character;
end;

function TCharacter.Match(const AParser: TParser): Boolean;
begin
  result:=AParser.Text[AParser.Position]=Character;

  if result then
  begin
    AParser.Match(Self,1);
    Trace(Self,AParser.Position-1,AParser);
  end;
end;

{ TString }

Constructor TString.Create(const AText: String);
begin
  inherited Create;
  Text:=AText;
end;

function TString.AsString: String;
var t : Integer;
begin
  result:='’';

  for t:=1 to Length(Text) do
      result:=result+ToString(Text[t]);

  result:=result+'’';
end;

function TString.Match(const AParser: TParser): Boolean;
var t : Integer;
begin
  // if AParser.Remain<Length(Text) then
  //   result:=False
  //else
  begin
    for t:=1 to Length(Text) do
        if AParser.Text[AParser.FPosition+t-1]<>Text[t] then
        begin
          result:=False;
          Exit;
        end;

    result:=True;

    AParser.Match(Self,Length(Text));
    Trace(Self,AParser.Position-Length(Text),AParser);
  end;
end;

{ TChoice }

constructor TChoice.Create(const AItems: array of TRule);
begin
  inherited Create;
  Add(AItems);
end;

procedure TChoice.Add(const AItems: array of TRule);
var t : Integer;
begin
  SetLength(Items,Length(AItems));

  for t:=Low(AItems) to High(AItems) do
      Items[t-Low(AItems)]:=AItems[t];
end;

function TChoice.ToString(const Separator: String): String;
var L,
    t : Integer;
begin
  result:='';

  L:=Length(Items);

  for t:=Low(Items) to High(Items) do
  begin
    if t>Low(Items) then
       result:=result+Separator;

    if (Separator=' ') and (Items[t] is TChoice) and (L>1) then
       result:=result+'('+Items[t].AsString+')'
    else
       result:=result+Items[t].AsString;
  end;
end;

{ TCharacterRange }

constructor TCharacterRange.Create(const AStart, AFinish: Char);
begin
  inherited Create;

  Start:=AStart;
  Finish:=AFinish;
end;

function TCharacterRange.AsString: String;
begin
  if InSet then
     result:=''
  else
     result:='[';

  result:=result+Start+'-'+Finish;

  if not InSet then
     result:=result+']';
end;

function TCharacterRange.Match(const AParser: TParser): Boolean;
var tmp : Char;
begin
  tmp:=AParser.Text[AParser.Position];

  result:=(tmp>=Start) and (tmp<=Finish);

  if result then
  begin
    AParser.Match(Self,1);
    Trace(Self,AParser.Position-1,AParser);
  end;
end;

{ TAnyCharacter }

function TAnyCharacter.AsString: String;
begin
  result:='.';
end;

function TAnyCharacter.Match(const AParser: TParser): Boolean;
begin
  result:=not AParser.EndOfFile;

  if result then
  begin
    AParser.Match(Self,1);
    Trace(Self,AParser.Position-1,AParser);
  end;
end;

{ TZeroOrMore }

function TZeroOrMore.AsString: String;
begin
  result:=inherited AsString+'*';
end;

function TZeroOrMore.Match(const AParser: TParser): Boolean;
var tmp : Integer;
begin
  result:=True;

  tmp:=AParser.Position;

  repeat
  until not Rule.Match(AParser);

  Trace(Self,tmp,AParser);
end;

{ TOneOrMore }

function TOneOrMore.AsString: String;
begin
  result:=inherited AsString+'+';
end;

function TOneOrMore.Match(const AParser: TParser): Boolean;
var tmp : Integer;
begin
  tmp:=AParser.Position;

  result:=Rule.Match(AParser);

  if result then
  begin

    repeat
    until not Rule.Match(AParser);

    Trace(Self,tmp,AParser);
  end;
end;

{ TPrioritized }

function TPrioritized.AsString: String;
begin
  result:=ToString(' / ');
end;

function TPrioritized.Match(const AParser: TParser): Boolean;
var tmp : Integer;
    t : Integer;
begin
  for t:=Low(Items) to High(Items) do
  begin
    tmp:=AParser.Push;

    if Items[t].Match(AParser) then
    begin
      result:=True;
      Trace(Self,tmp,AParser);

      Exit;
    end
    else
      AParser.Pop;
  end;

  result:=False;
end;

{ TNotPredicate }

function TNotPredicate.AsString: String;
begin
  result:='!'+inherited AsString;
end;

function TNotPredicate.Match(const AParser: TParser): Boolean;
var tmp : Integer;
begin
  tmp:=AParser.Push;

  result:=not Rule.Match(AParser);

  if result then
     Trace(Self,tmp,AParser)
  else
     AParser.Pop;
end;

{ TAndPredicate }

function TAndPredicate.AsString: String;
begin
  result:='&'+inherited AsString;
end;

function TAndPredicate.Match(const AParser: TParser): Boolean;
var tmp : Integer;
begin
  tmp:=AParser.Push;

  result:=Rule.Match(AParser);

  Trace(Self,tmp,AParser);

  AParser.Pop;
end;

{ TNamedRule }

function TNamedRule.AsString: String;
begin
  result:=Name;
end;

constructor TNamedRule.Create(const AName: String; const ARule: TRule);
begin
  inherited Create(ARule);
  Name:=AName;
end;

function TNamedRule.Match(const AParser: TParser): Boolean;
var tmp : Integer;
begin
  tmp:=AParser.Position;

  result:=Rule.Match(AParser);

  if result then
     Trace(Self,tmp,AParser);
end;

{ TOptional }

function TOptional.AsString: String;
begin
  result:=inherited AsString+'?';
end;

function TOptional.Match(const AParser: TParser): Boolean;
var tmp : Integer;
begin
  tmp:=AParser.Position;

  result:=True;

  if Rule.Match(AParser) then
     Trace(Self,tmp,AParser);
end;

{ TCharacterSet }

function TCharacterSet.AsString: String;
begin
  result:='['+ToString('')+']';
end;

constructor TCharacterSet.InnerCreate;
begin
end;

constructor TCharacterSet.Create(const AItems: array of TCharacterRule);
var t : Integer;
begin
  InnerCreate;

  SetLength(Items,Length(AItems));

  for t:=Low(AItems) to High(AItems) do
  begin
    AItems[t].InSet:=True;

    Items[t-Low(AItems)]:=AItems[t];
  end;
end;

initialization
  // There are different single quotes:
  // ’ = #146
  // ´ = #180
  // ' = #39
  // ` = #96

  SingleQuote:=TCharacter.Create(#146);
  DoubleQuote:=TCharacter.Create('"');
finalization
  DoubleQuote.Free;
  SingleQuote.Free;
end.
