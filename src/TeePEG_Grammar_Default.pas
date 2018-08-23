unit TeePEG_Grammar_Default;

interface

uses
  TeePEG_Grammar;

type
  TPEGGrammar=class(TGrammar)
  public
    Constructor Create;
  end;

implementation

uses
  TeePEG_Rules;

// Default PEG grammar, hardcoded

{ TPEGGrammar }

Constructor TPEGGrammar.Create;
var EndOfFile,
    AnyCharacter,
    LineFeed,
    EndOfLine,
    Space,
    Comment,
    SpaceOrComment,
    Spacing,
    NeedsSpacing,
    DOT, CLOSE, OPEN, PLUS, STAR, QUESTION, _NOT, _AND, SLASH, LEFTARROW,

    CharRule, Range, ClassRule,

    Literal, IdentCont, IdentStart, Identifier,

    Primary, Suffix, Prefix, Sequence, Expression, Definition, Grammar

     : TRule;

    SpaceChar,
    HashChar,
    Tab,
    Return,
    NewLine,
    BackSlash,
    LeftBracket,
    RightBracket : TCharacter;

    Range02,
    Range07 : TCharacterRange;

    NotEndOfLine : TNotPredicate;

    Escapable : TCharacterSet;

  procedure AddInternals;
  begin
    AddInternal(AnyCharacter);
    AddInternal(Return);
    AddInternal(NewLine);
    AddInternal(Tab);
    AddInternal(LineFeed);
    AddInternal(BackSlash);
    AddInternal(LeftBracket);
    AddInternal(RightBracket);
    AddInternal(SpaceChar);
    AddInternal(HashChar);
    AddInternal(Escapable);
    AddInternal(Range02);
    AddInternal(Range07);

    AddInternal(NotEndOfLine);
  end;

  function AddCharRule(const ARule:TCharacterRule):TCharacterRule;
  begin
    AddInternal(ARule);
    result:=ARule;
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

    Add(LEFTARROW);
    Add(SLASH);
    Add(_AND);
    Add(_NOT);
    Add(QUESTION);
    Add(STAR);
    Add(PLUS);
    Add(OPEN);
    Add(CLOSE);
    Add(DOT);

    Add(NeedsSpacing);
    Add(Spacing);
    Add(SpaceOrComment);
    Add(Comment);
    Add(Space);
    Add(EndOfLine);
    Add(EndOfFile);
  end;

  function NewChar(const AChar:Char):TCharacterRule;
  begin
    result:=AddCharRule(TCharacter.Create(AChar));
  end;

  function CharSpacing(const AName:String; const AChar:Char):TRule;
  begin
    result:=TNamedRule.Create(AName,TSequence.Create([NewChar(AChar),Spacing]));
  end;

var
  ParentExpression : TSequence;

begin
  inherited;

  AnyCharacter:=TAnyCharacter.Create;

  EndOfFile:=TNamedRule.Create('EndOfFile',TNotPredicate.Create(AnyCharacter));

  Return:=TCharacter.Create(#13);
  NewLine:=TCharacter.Create(#10);
  Tab:=TCharacter.Create(#9);
  LineFeed:=TString.Create(#13#10);
  SpaceChar:=TCharacter.Create(' ');
  HashChar:=TCharacter.Create('#');

  EndOfLine:=TNamedRule.Create('EndOfLine',TPrioritized.Create([LineFeed,NewLine,Return]));

  Space:=TNamedRule.Create('Space',TPrioritized.Create([SpaceChar,Tab,EndOfLine]));

  NotEndOfLine:=TNotPredicate.Create(EndOfLine);

  Comment:=TNamedRule.Create('Comment',
             TSequence.Create([HashChar,
                             AddInternal(
                               TZeroOrMore.Create(
                                 AddInternal(
                                   TSequence.Create(
                                      [ NotEndOfLine,
                                        AnyCharacter ])
                                   )
                               )
                             ),
                             AddInternal(
                               TPrioritized.Create([
                                EndOfLine,
                                EndOfFile
                                ])
                             )
                             ])
              );

  SpaceOrComment:=TNamedRule.Create('SpaceOrComment',TPrioritized.Create([Space,Comment]));

  Spacing:=TNamedRule.Create('Spacing',TZeroOrMore.Create(SpaceOrComment));

  NeedsSpacing:=TNamedRule.Create('NeedsSpacing',TOneOrMore.Create(SpaceOrComment));

  DOT:=CharSpacing('DOT','.');
  CLOSE:=CharSpacing('CLOSE',')');
  OPEN:=CharSpacing('OPEN','(');
  PLUS:=CharSpacing('PLUS','+');
  STAR:=CharSpacing('STAR','*');
  QUESTION:=CharSpacing('QUESTION','?');
  _NOT:=CharSpacing('NOT','!');
  _AND:=CharSpacing('AND','&');
  SLASH:=CharSpacing('SLASH','/');

  LEFTARROW:=TNamedRule.Create('LEFTARROW',
                  TSequence.Create(
                     [
                        AddInternal(TString.Create('<-')),
                        Spacing
                     ]));

  BackSlash:=TCharacter.Create('\');
  LeftBracket:=TCharacter.Create('[');
  RightBracket:=TCharacter.Create(']');

  Escapable:=TCharacterSet.Create([
               NewChar('n'),
               NewChar('r'),
               NewChar('t'),
               NewChar(SingleQuote.Character),
               NewChar(DoubleQuote.Character),
               NewChar(LeftBracket.Character),
               NewChar(RightBracket.Character),
               NewChar(BackSlash.Character)
             ]);

  Range02:=TCharacterRange.Create('0','2');
  Range07:=TCharacterRange.Create('0','7');

  CharRule:=TNamedRule.Create('Char',
                TPrioritized.Create([
                                   AddInternal(TSequence.Create([ BackSlash, Escapable ])),

                                   AddInternal(TSequence.Create([ BackSlash,
                                                       AddInternal(TCharacterSequence.Create(
                                                        [
                                                          Range02,Range07,Range07
                                                        ])
                                                        )
                                                     ])),

                                   AddInternal(TSequence.Create([ BackSlash,
                                                       AddInternal(TCharacterSequence.Create(
                                                        [
                                                          Range07,
                                                          AddInternal(TOptional.Create(Range07))
                                                        ])
                                                        )
                                                    ])),

                                   AddInternal(TSequence.Create([
                                       AddInternal(TNotPredicate.Create(BackSlash)), AnyCharacter
                                   ]))

                                ])
                );

  Range:=TNamedRule.Create('Range',
           TPrioritized.Create([
               AddInternal(
                 TSequence.Create(
                   [
                     CharRule,
                     NewChar('-'),
                     CharRule
                   ])
               ),
               CharRule
            ])
           );

  ClassRule:=TNamedRule.Create('Class',
               TSequence.Create([
                 LeftBracket,
                 AddInternal(
                  TZeroOrMore.Create(
                    AddInternal(
                       TSequence.Create(
                        [ AddInternal(TNotPredicate.Create(RightBracket)), Range ]
                      )
                    )
                  )
                 ),
                 RightBracket,
                 Spacing
               ])
              );

  Literal:=TNamedRule.Create('Literal',
             TPrioritized.Create([
               AddInternal(
                 TSequence.Create([
                       SingleQuote,
                       AddInternal(
                         TZeroOrMore.Create(
                            AddInternal(
                              TSequence.Create([
                                AddInternal(TNotPredicate.Create(SingleQuote)),
                                CharRule
                              ])
                            )
                         )
                       ),
                       SingleQuote,
                       Spacing
                 ])
               ),

               AddInternal(
                 TSequence.Create([
                       DoubleQuote,
                       AddInternal(
                         TZeroOrMore.Create(
                            AddInternal(
                              TSequence.Create([
                                AddInternal(TNotPredicate.Create(DoubleQuote)),
                                CharRule
                              ])
                            )
                         )
                       ),
                       DoubleQuote,
                       Spacing
                 ])
               )
             ])
           );

  IdentStart:=TNamedRule.Create('IdentStart',
                TCharacterSet.Create([
                 AddCharRule(TCharacterRange.Create('a','z')),
                 AddCharRule(TCharacterRange.Create('A','Z')),
                 NewChar('_')
                 ])
              );

  IdentCont:=TNamedRule.Create('IdentCont',
                TPrioritized.Create([
                     IdentStart,
                     AddInternal(TCharacterRange.Create('0','9'))
                  ])
                );

  Identifier:=TNamedRule.Create('Identifier',
                TSequence.Create([
                  IdentStart,
                  AddInternal(TZeroOrMore.Create(IdentCont)),
                  Spacing
                ])
              );

  ParentExpression:=TSequence.Create([OPEN, Expression, CLOSE] );

  Primary:=TNamedRule.Create('Primary',
              TPrioritized.Create([
                 AddInternal(
                   TSequence.Create([
                     Identifier,
                     AddInternal(TNotPredicate.Create(LEFTARROW))
                   ])
                 ),

                 AddInternal(ParentExpression),
                 Literal,
                 ClassRule,
                 DOT
              ])
            );

  Suffix:=TNamedRule.Create('Suffix',
            TSequence.Create([
              Primary,
              AddInternal(
               TOptional.Create(
                AddInternal(
                  TPrioritized.Create([
                     QUESTION,
                     STAR,
                     PLUS
                   ])
                )
                )
              )
            ])
          );

  Prefix:=TNamedRule.Create('Prefix',
            TSequence.Create([
              AddInternal(
                TOptional.Create(
                  AddInternal(
                    TPrioritized.Create([ _AND, _NOT ])
                    )
                  )
              ),
              Suffix
            ])
          );

  Sequence:=TNamedRule.Create('Sequence',
               TSequence.Create([
                 Prefix,
                 AddInternal(TZeroOrMore.Create(Prefix))
               ])
             );

  Expression:=TNamedRule.Create('Expression',
               TSequence.Create([
                 Sequence,
                 AddInternal(
                   TZeroOrMore.Create(
                    AddInternal(
                      TSequence.Create([
                         SLASH, Sequence
                       ])
                    )
                   )
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
               AddInternal(TOneOrMore.Create(Definition)),
               EndOfFile
             ])
            );

  AddRules;
  AddInternals;
end;

end.
