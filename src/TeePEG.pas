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
  Classes, TeePEG_Grammar;

type
  TPEG=class
  private
    FGrammar : TGrammar;
  public
    Constructor Create;
    Destructor Destroy; override;

    function Load(const S:String):TGrammar; overload;
    function Load(const S:TStrings):TGrammar; overload;

    property Grammar:TGrammar read FGrammar;
  end;

implementation

uses
  TeePEG_Grammar_Default;

{ TPEG }

Constructor TPEG.Create;
begin
  inherited Create;
  FGrammar:=TPEGGrammar.Create;
end;

Destructor TPEG.Destroy;
begin
  FGrammar.Free;
  inherited;
end;

function TPEG.Load(const S: String):TGrammar;
begin
  result:=Grammar.Load(S);
end;

function TPEG.Load(const S: TStrings):TGrammar;
begin
  result:=Load(S.Text);
end;

end.
