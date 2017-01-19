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
  public
    Grammar : TGrammar;

    Constructor Create;
    Destructor Destroy; override;

    function Load(const S:String):TGrammar; overload;
    function Load(const S:TStrings):TGrammar; overload;
  end;

implementation

{ TPEG }

Constructor TPEG.Create;
begin
  inherited Create;
  Grammar:=TPEGGrammar.Create;
end;

Destructor TPEG.Destroy;
begin
  Grammar.Free;
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
