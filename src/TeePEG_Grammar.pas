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
  TGrammar=class
  public
    class procedure AddTo(var Items:TRuleArray);
  end;

implementation

class procedure TGrammar.AddTo(var Items:TRuleArray);
var EndOfFile,
    AnyCharacter,
    Return,
    NewLine,
    LineFeed,
    EndOfLine,
    Space,
    Comment,
    Spacing,
    DOT, CLOSE, OPEN, PLUS, STAR, QUESTION, _NOT, _AND, SLASH, LEFTARROW,

    BackSlash, RightBracket,

    CharRule, Range, ClassRule,

    SingleQuote, DoubleQuote,

    Literal, IdentCont, IdentStart, Identifier,

    Primary, Suffix, Prefix, Sequence, Expression, Definition, Grammar

     : TRule;

  procedure Add(const ARule:TRule);
  var L : Integer;
  begin
    L:=Length(Items);
    SetLength(Items,L+1);
    Items[L]:=ARule;
  end;

  procedure AddRules;
  begin
    Add(Grammar);
    Add(Definition);

    Add(Expression);
    Add(Sequence);
    Add(Prefix);
    Add(Suffix);
    Add(Primary);
    Add(Identifier);
    Add(IdentStart);
    Add(IdentCont);
    Add(Literal);
    Add(ClassRule);
    Add(Range);
    Add(CharRule);

    Add(DOT);
    Add(CLOSE);
    Add(OPEN);
    Add(PLUS);
    Add(STAR);
    Add(QUESTION);
    Add(_NOT);
    Add(_AND);
    Add(SLASH);
    Add(LEFTARROW);

    Add(Spacing);
    Add(Comment);
    Add(Space);
    Add(EndOfLine);
    Add(EndOfFile);
  end;

  function CharSpacing(const AName:String; const AChar:Char):TRule;
  begin
    result:=TNamedRule.Create(AName,TSequence.Create([TCharacter.Create(AChar),Spacing]));
  end;

var
  ParentExpression : TSequence;

begin
  AnyCharacter:=TAnyCharacter.Create;

  EndOfFile:=TNamedRule.Create('EndOfFile',TNotPredicate.Create(AnyCharacter));

  Return:=TCharacter.Create(#13);
  NewLine:=TCharacter.Create(#10);
  LineFeed:=TString.Create(#13#10);

  EndOfLine:=TNamedRule.Create('EndOfLine',TPrioritized.Create([LineFeed,NewLine,Return]));

  Space:=TNamedRule.Create('Space',TPrioritized.Create([TCharacter.Create(' '),
                              TCharacter.Create(#9),
                              EndOfLine])
              );

  Comment:=TNamedRule.Create('Comment',
           TSequence.Create([TCharacter.Create('#'),
                             TZeroOrMore.Create(
                                 TSequence.Create(
                                      [ TNotPredicate.Create(EndOfLine),
                                        AnyCharacter ])
                             ),
                             TPrioritized.Create([
                                EndOfLine,
                                EndOfFile
                                ])
                             ])
              );

  Spacing:=TNamedRule.Create('Spacing',TZeroOrMore.Create(TPrioritized.Create([Space,Comment])));

  DOT:=CharSpacing('DOT','.');
  CLOSE:=CharSpacing('CLOSE',')');
  OPEN:=CharSpacing('OPEN','(');
  PLUS:=CharSpacing('PLUS','+');
  STAR:=CharSpacing('STAR','*');
  QUESTION:=CharSpacing('QUESTION','?');
  _NOT:=CharSpacing('NOT','!');
  _AND:=CharSpacing('AND','&');
  SLASH:=CharSpacing('SLASH','/');

  LEFTARROW:=TNamedRule.Create('LEFTARROW',TSequence.Create([TString.Create('<-'),Spacing]));

  BackSlash:=TCharacter.Create('\');

  CharRule:=TNamedRule.Create('Char',
                TPrioritized.Create([
                                   TSequence.Create([ BackSlash, TString.Create('"\') ]),
                                   TSequence.Create([ BackSlash,
                                                      TCharacterRange.Create('0','2'),
                                                      TCharacterRange.Create('0','7'),
                                                      TCharacterRange.Create('0','7') ]),
                                   TSequence.Create([ BackSlash,
                                                      TCharacterRange.Create('0','7'),
                                                      TOptional.Create(
                                                        TCharacterRange.Create('0','7')
                                                      )
                                                    ]),
                                   TSequence.Create([ TNotPredicate.Create(BackSlash), AnyCharacter ])

                                ])
                );

  Range:=TNamedRule.Create('Range',
           TPrioritized.Create([
               TSequence.Create([ CharRule, TCharacter.Create('-'), CharRule ]),
               CharRule
            ])
           );

  RightBracket:=TCharacter.Create(']');

  ClassRule:=TNamedRule.Create('Class',
               TSequence.Create([
                 TCharacter.Create('['),
                 TZeroOrMore.Create(
                    TSequence.Create(
                        [ TNotPredicate.Create(RightBracket), Range ]
                      )
                 ),
                 RightBracket,
                 Spacing
               ])
              );

  SingleQuote:=TCharacter.Create('’');
  DoubleQuote:=TCharacter.Create('"');

  Literal:=TNamedRule.Create('Literal',
             TPrioritized.Create([
               TSequence.Create([
                       SingleQuote,
                       TZeroOrMore.Create(
                            TSequence.Create([
                                TNotPredicate.Create(SingleQuote),
                                CharRule
                              ])
                          ),
                       SingleQuote,
                       Spacing
                    ]),
               TSequence.Create([
                       DoubleQuote,
                       TZeroOrMore.Create(
                            TSequence.Create([
                                TNotPredicate.Create(DoubleQuote),
                                CharRule
                              ])
                          ),
                       DoubleQuote,
                       Spacing
                    ])
             ])
           );

  IdentStart:=TNamedRule.Create('IdentStart',
               TPrioritized.Create([
                 TCharacterRange.Create('a','z'),
                 TCharacterRange.Create('A','Z'),
                 TCharacter.Create('_')
                 ])
              );

  IdentCont:=TNamedRule.Create('IdentCont',
                TPrioritized.Create([
                     IdentStart,
                     TCharacterRange.Create('0','9')
                  ])
                );

  Identifier:=TNamedRule.Create('Identifier',
                TSequence.Create([
                  IdentStart,
                  TZeroOrMore.Create(IdentCont),
                  Spacing
                ])
              );

  ParentExpression:=TSequence.Create([ OPEN, Expression, CLOSE] );

  Primary:=TNamedRule.Create('Primary',
              TPrioritized.Create([
                 TSequence.Create([ Identifier, TNotPredicate.Create(LEFTARROW) ]),
                 ParentExpression,
                 Literal,
                 ClassRule,
                 DOT
              ])
            );

  Suffix:=TNamedRule.Create('Suffix',
            TSequence.Create([
              Primary,
              TOptional.Create(
                TPrioritized.Create([
                     QUESTION,
                     STAR,
                     PLUS
                   ])
                )
            ])
          );

  Prefix:=TNamedRule.Create('Prefix',
            TSequence.Create([
              TOptional.Create( TPrioritized.Create([ _AND, _NOT ]) ),
              Suffix
            ])
          );

  Sequence:=TNamedRule.Create('Sequence',TZeroOrMore.Create(Prefix));

  Expression:=TNamedRule.Create('Expression',
               TSequence.Create([
                 Sequence,
                 TZeroOrMore.Create(
                    TSequence.Create([
                         SLASH, Sequence
                       ])
                   )
               ])
              );

  // Re-link "Expression"
  ParentExpression.Items[1]:=Expression;

  Definition:=TNamedRule.Create('Definition',
                 TSequence.Create([ Identifier, LEFTARROW, Expression ]));

  Grammar:=TNamedRule.Create('Grammar',
            TSequence.Create([
               Spacing,
               TOneOrMore.Create(Definition),
               EndOfFile
             ])
            );

  AddRules;
end;

end.
