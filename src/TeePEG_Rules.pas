// @davidberneda
// 2017
unit TeePEG_Rules;

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

  PSyntaxItem=^TSyntaxItem;

  TSyntaxItem=record
    Position,
    Length : Integer;
    Rule : TRule;

    Items : Array of PSyntaxItem;
  end;

  TParser=class
  private
    FPosition : Integer;

    function Advance(const ARule:TRule; const ALength:Integer):PSyntaxItem;
    procedure Add(const AParent,AChild:PSyntaxItem);
    procedure Pop(const AItem:PSyntaxItem);
  protected
  public
    Syntax : PSyntaxItem;
    Text : String;

    function EndOfFile: Boolean; {$IFDEF INLINE}inline;{$ENDIF}
    procedure Start(const AText:String);

    property Position:Integer read FPosition write FPosition;
  end;

  TRule=class
  protected
    function Match(const AParser:TParser):PSyntaxItem; virtual; abstract;
    function Push(const APosition:Integer):PSyntaxItem;
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
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  { !e }
  TNotPredicate=class(TPredicate)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  TTextRule=class(TRule)
  protected
    function CharacterMatch(const A,B:Char):Boolean;
  public
    CaseInsensitive : Boolean;
  end;

  TCharacterRule=class(TTextRule)
  protected
    InSet : Boolean;
  end;

  { 'x' }
  TCharacter=class(TCharacterRule)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    Character : Char;

    Constructor Create(const AChar:Char);
    function AsString:String; override;
  end;

  { n-m }
  TCharacterRange=class(TCharacterRule)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    Start,
    Finish : Char;

    Constructor Create(const AStart,AFinish:Char);
    function AsString:String; override;
  end;

  { 'abc' }
  TString=class(TTextRule)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    Text : String;

    Constructor Create(const AText:String);
    function AsString:String; override;
  end;

  { . }
  TAnyCharacter=class(TRule)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  TRuleArray=Array of TRule;

  TChoice=class(TRule)
  private
    function Join(const Separator:String):String;
  protected
    procedure Add(const AItems: array of TRule);
  public
    Items : TRuleArray;

    Constructor Create(const AItems:Array of TRule);
  end;

  { e1 e2 }
  TSequence=class(TChoice)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  { e1 / e2 }
  TPrioritized=class(TChoice)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  { [abcA-ZQRS_] }
  TCharacterSet=class(TPrioritized)
  protected
    Constructor InnerCreate;
  public
    Constructor Create(const AItems: array of TCharacterRule);

    function AsString:String; override;
  end;

  { e* }
  TZeroOrMore=class(TOperator)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  { e+ }
  TOneOrMore=class(TOperator)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  { e? }
  TOptional=class(TOperator)  // Zero or One
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    function AsString:String; override;
  end;

  TNamedRule=class(TOperator)
  protected
    function Match(const AParser:TParser):PSyntaxItem; override;
  public
    Name : String;

    Constructor Create(const AName:String; const ARule:TRule);
    function AsString:String; override;
  end;

var
  SingleQuote,
  DoubleQuote : TCharacter;

  PEG_Log : TStrings;
  PEG_Log_AllRules : Boolean=False;

implementation

uses
  SysUtils;

procedure Log(const ARule:TRule; const AStart:Integer; const AParser: TParser);
begin
  if PEG_Log<>nil then
     if PEG_Log_AllRules or (ARule is TNamedRule) then

     PEG_Log.Add(IntToStr(AParser.Position)+' { '+
          Copy(AParser.Text,AStart,AParser.Position-AStart)+' } '+
          ARule.ClassName+': '+ARule.AsString);
end;

{ TParser }

function TParser.EndOfFile: Boolean;
begin
  result:=FPosition>Length(Text);
end;

{ TRule }

function TRule.Push(const APosition: Integer): PSyntaxItem;
begin
  New(result);
  result.Rule:=Self;
  result.Position:=APosition;
end;

{ TSequenceRule }

function TSequence.AsString: String;
begin
  result:=Join(' ');
end;

function TSequence.Match(const AParser:TParser): PSyntaxItem;
var t : Integer;
    tmp : PSyntaxItem;
begin
  result:=Push(AParser.Position);

  for t:=Low(Items) to High(Items) do
  begin
    tmp:=Items[t].Match(AParser);

    if tmp=nil then
    begin
      AParser.Pop(result);
      result:=nil;
      Exit;
    end
    else
      AParser.Add(result,tmp);
  end;

  result.Length:=AParser.Position-result.Position+1;

  Log(Self,result.Position,AParser);
end;

function TParser.Advance(const ARule:TRule; const ALength:Integer):PSyntaxItem;
begin
  result:=ARule.Push(FPosition);

  result.Length:=ALength;

  Log(ARule,FPosition,Self);

  Inc(FPosition,ALength);
end;

procedure TParser.Add(const AParent,AChild:PSyntaxItem);
var L : Integer;
begin
  L:=Length(AParent.Items);
  SetLength(AParent.Items,L+1);

  AParent.Items[L]:=AChild;
end;

procedure DisposeItem(const AItem:PSyntaxItem);
var t : Integer;
begin
  for t:=Low(AItem.Items) to High(AItem.Items) do
      DisposeItem(AItem.Items[t]);

  Dispose(AItem);
end;

procedure TParser.Pop(const AItem:PSyntaxItem);
begin
  FPosition:=AItem.Position;
  DisposeItem(AItem);
end;

procedure TParser.Start(const AText: String);
begin
  Text:=AText;

  if Syntax<>nil then
     DisposeItem(Syntax);

  New(Syntax);
  Syntax.Position:=0;
  Syntax.Rule:=nil;
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

{ TTextRule }

function TTextRule.CharacterMatch(const A,B:Char):Boolean;
begin
  if CaseInsensitive then
     result:=UpCase(A)=UpCase(B)
  else
     result:=A=B;
end;

{ TCharacter }

Constructor TCharacter.Create(const AChar: Char);
begin
  inherited Create;
  Character:=AChar;
end;

function CharToString(const C:Char):String;
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
     result:=CharToString(Character)
  else
     result:=SingleQuote.Character+CharToString(Character)+SingleQuote.Character;
end;

function TCharacter.Match(const AParser: TParser): PSyntaxItem;
begin
  if CharacterMatch(AParser.Text[AParser.Position],Character) then
     result:=AParser.Advance(Self,1)
  else
     result:=nil;
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
      result:=result+CharToString(Text[t]);

  result:=result+'’';
end;

function TString.Match(const AParser: TParser): PSyntaxItem;
var t : Integer;
begin
  for t:=1 to Length(Text) do
      if not CharacterMatch(AParser.Text[AParser.FPosition+t-1],Text[t]) then
      begin
        result:=nil;
        Exit;
      end;

  result:=AParser.Advance(Self,Length(Text));
end;

{ TChoice }

Constructor TChoice.Create(const AItems: array of TRule);
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

function TChoice.Join(const Separator: String): String;
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

function TCharacterRange.Match(const AParser: TParser): PSyntaxItem;
var tmp : Char;
    tmpMatch : Boolean;
begin
  tmp:=AParser.Text[AParser.Position];

  if CaseInsensitive then
  begin
    tmp:=UpCase(tmp);
    tmpMatch:=(tmp>=UpCase(Start)) and (tmp<=UpCase(Finish));
  end
  else
    tmpMatch:=(tmp>=Start) and (tmp<=Finish);

  if tmpMatch then
     result:=AParser.Advance(Self,1)
  else
     result:=nil;
end;

{ TAnyCharacter }

function TAnyCharacter.AsString: String;
begin
  result:='.';
end;

function TAnyCharacter.Match(const AParser: TParser): PSyntaxItem;
begin
  if AParser.EndOfFile then
     result:=nil
  else
     result:=AParser.Advance(Self,1);
end;

{ TZeroOrMore }

function TZeroOrMore.AsString: String;
begin
  result:=inherited AsString+'*';
end;

function TZeroOrMore.Match(const AParser: TParser): PSyntaxItem;
var tmp : PSyntaxItem;
begin
  result:=Push(AParser.Position);

  repeat
    tmp:=Rule.Match(AParser);

    if tmp<>nil then
       AParser.Add(result,tmp);

  until tmp=nil;

  if result.Items<>nil then
     Log(Self,result.Position,AParser);
end;

{ TOneOrMore }

function TOneOrMore.AsString: String;
begin
  result:=inherited AsString+'+';
end;

function TOneOrMore.Match(const AParser: TParser): PSyntaxItem;
var tmp : PSyntaxItem;
begin
  result:=Push(AParser.Position);

  tmp:=Rule.Match(AParser);

  if tmp=nil then
  begin
    AParser.Pop(result);
    result:=nil;
  end
  else
  begin
    AParser.Add(result,tmp);

    repeat
      tmp:=Rule.Match(AParser);

      if tmp<>nil then
         AParser.Add(result,tmp);

    until tmp=nil;

    result.Length:=AParser.Position-result.Position+1;

    Log(Self,result.Position,AParser);
  end;
end;

{ TPrioritized }

function TPrioritized.AsString: String;
begin
  result:=Join(' / ');
end;

function TPrioritized.Match(const AParser: TParser): PSyntaxItem;
var t : Integer;
    tmp : PSyntaxItem;
begin
  for t:=Low(Items) to High(Items) do
  begin
    result:=Push(AParser.Position);

    tmp:=Items[t].Match(AParser);

    if tmp<>nil then
    begin
      AParser.Add(result,tmp);

      //AParser.Match(result,Self,AParser.Position-result.Position);

      Log(Self,result.Position,AParser);

      Exit;
    end
    else
      AParser.Pop(result);
  end;

  result:=nil;
end;

{ TNotPredicate }

function TNotPredicate.AsString: String;
begin
  result:='!'+inherited AsString;
end;

function TNotPredicate.Match(const AParser: TParser): PSyntaxItem;
var tmp : PSyntaxItem;
begin
  result:=Push(AParser.Position);

  tmp:=Rule.Match(AParser);

  if tmp=nil then
     Log(Self,result.Position,AParser)
  else
  begin
    DisposeItem(tmp);

    AParser.Pop(result);

    result:=nil;
  end;
end;

{ TAndPredicate }

function TAndPredicate.AsString: String;
begin
  result:='&'+inherited AsString;
end;

function TAndPredicate.Match(const AParser: TParser): PSyntaxItem;
var tmp : PSyntaxItem;
begin
  tmp:=Push(AParser.Position);

  result:=Rule.Match(AParser);

  Log(Self,tmp.Position,AParser);

  AParser.Pop(tmp);
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

function TNamedRule.Match(const AParser: TParser): PSyntaxItem;
var tmp : PSyntaxItem;
begin
  result:=Push(AParser.Position);

  tmp:=Rule.Match(AParser);

  if tmp=nil then
  begin
    AParser.Pop(result);
    result:=nil;
  end
  else
  begin
    AParser.Add(result,tmp);
    //AParser.Match(result,Self,AParser.Position-result.Position);
    Log(Self,result.Position,AParser);
  end;
end;

{ TOptional }

function TOptional.AsString: String;
begin
  result:=inherited AsString+'?';
end;

function TOptional.Match(const AParser: TParser): PSyntaxItem;
var tmp : PSyntaxItem;
begin
  result:=Push(AParser.Position);

  tmp:=Rule.Match(AParser);

  if tmp<>nil then
  begin
    AParser.Add(result,tmp);
    Log(Self,result.Position,AParser);
  end;
end;

{ TCharacterSet }

Constructor TCharacterSet.InnerCreate;
begin
end;

Constructor TCharacterSet.Create(const AItems: array of TCharacterRule);
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

function TCharacterSet.AsString: String;
begin
  result:='['+Join('')+']';
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
