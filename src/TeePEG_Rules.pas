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
  TParser=class
  private
    FPos : Integer;
  public
    Text : String;

    function EndOfFile: Boolean; {$IFDEF INLINE}inline;{$ENDIF}
    property Position:Integer read FPos write FPos;
  end;

  TRule=class
  protected
    function Match(const AParser:TParser):Boolean; virtual; abstract;
  public
    function AsString:String; virtual; abstract;
  end;

  TToken=record
    Rule : TRule;
    Start,
    Length : Integer;
  end;

  TTokens=Array of TToken;

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

  { 'x' }
  TCharacter=class(TRule)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    Character : Char;

    Constructor Create(const AChar:Char);
    function AsString:String; override;
  end;

  { [a-zA-Z_] }
  TCharSet=set of Char;

  TCharacterSet=class(TRule)
  protected
    function Match(const AParser:TParser):Boolean; override;
  public
    CharacterSet : TCharSet;

    Constructor Create(const ASet:TCharSet);
  end;

  { [n-m] }
  TCharacterRange=class(TRule)
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
  TOptional=class(TOperator)
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

implementation

uses
  SysUtils;

{ TParser }

function TParser.EndOfFile: Boolean;
begin
  result:=FPos>Length(Text);
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
  tmp:=AParser.FPos;

  for t:=Low(Items) to High(Items) do
      if not Items[t].Match(AParser) then
      begin
        result:=False;
        AParser.FPos:=tmp;
        Exit;
      end;

  result:=True;
end;

{ TOperator }

constructor TOperator.Create(const ARule: TRule);
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
  if Character='’' then
     result:='[’]'
  else
     result:='’'+ToString(Character)+'’';
end;

function TCharacter.Match(const AParser: TParser): Boolean;
begin
  result:=AParser.Text[AParser.FPos]=Character;

  if result then
     Inc(AParser.FPos);
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
        if AParser.Text[AParser.FPos+t-1]<>Text[t] then
        begin
          result:=False;
          Exit;
        end;

    result:=True;
    Inc(AParser.FPos,Length(Text));
  end;
end;

{ TChoice }

constructor TChoice.Create(const AItems: array of TRule);
var t : Integer;
begin
  inherited Create;

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
  result:='['+Start+'-'+Finish+']';
end;

function TCharacterRange.Match(const AParser: TParser): Boolean;
var tmp : Char;
begin
  tmp:=AParser.Text[AParser.FPos];

  result:=(tmp>=Start) and (tmp<=Finish);

  if result then
     Inc(AParser.FPos);
end;

{ TCharacterSet }

constructor TCharacterSet.Create(const ASet: TCharSet);
begin
  inherited Create;
  CharacterSet:=ASet;
end;

function TCharacterSet.Match(const AParser: TParser): Boolean;
begin
  result:=AParser.Text[AParser.FPos] in CharacterSet;

  if result then
     Inc(AParser.FPos);
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
     Inc(AParser.FPos);
end;

{ TZeroOrMore }

function TZeroOrMore.AsString: String;
begin
  result:=inherited AsString+'*';
end;

function TZeroOrMore.Match(const AParser: TParser): Boolean;
begin
  result:=True;

  repeat
  until not Rule.Match(AParser);
end;

{ TOneOrMore }

function TOneOrMore.AsString: String;
begin
  result:=inherited AsString+'+';
end;

function TOneOrMore.Match(const AParser: TParser): Boolean;
begin
  result:=Rule.Match(AParser);

  if result then
     repeat
     until not Rule.Match(AParser);
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
  tmp:=AParser.FPos;

  for t:=Low(Items) to High(Items) do
  begin
    AParser.FPos:=tmp;

    if Items[t].Match(AParser) then
    begin
      result:=True;
      Exit;
    end;
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
  tmp:=AParser.FPos;

  result:=not Rule.Match(AParser);

  if not result then
     AParser.FPos:=tmp;
end;

{ TAndPredicate }

function TAndPredicate.AsString: String;
begin
  result:='&'+inherited AsString;
end;

function TAndPredicate.Match(const AParser: TParser): Boolean;
var tmp : Integer;
begin
  tmp:=AParser.FPos;

  result:=Rule.Match(AParser);

  AParser.FPos:=tmp;
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
begin
  result:=Rule.Match(AParser);
end;

{ TOptional }

function TOptional.AsString: String;
begin
  result:=inherited AsString+'?';
end;

function TOptional.Match(const AParser: TParser): Boolean;
begin
  result:=True;

  Rule.Match(AParser);
end;

end.
