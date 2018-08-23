unit Unit_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, 

  TeePeg, TeePeg_Grammar, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabPEG: TTabSheet;
    TabMath: TTabSheet;
    MathPEG: TMemo;
    PEGPEG: TMemo;
    CBTrace: TCheckBox;
    CBTokens: TCheckBox;
    PageControl2: TPageControl;
    TabRules: TTabSheet;
    MemoRules: TMemo;
    TabTrace: TTabSheet;
    MemoTrace: TMemo;
    TabSyntax: TTabSheet;
    TabEval: TTabSheet;
    Panel1: TPanel;
    MemoEval: TMemo;
    MemoOutput: TMemo;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBTraceClick(Sender: TObject);
    procedure MemoEvalChange(Sender: TObject);
  private
    { Private declarations }

    PEG : TPEG;
    Grammar : TGrammar;

    procedure VerifyDefaultGrammar;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  TeePEG_Rules, VCLPEG_Tree, Math;

procedure TForm1.CBTraceClick(Sender: TObject);
begin
  if CBTrace.Checked then
     PEG_Log:=MemoTrace.Lines
  else
     PEG_Log:=nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PEG:=TPEG.Create;

  MemoRules.Lines.Text:=PEG.Grammar.AsString;

  VerifyDefaultGrammar;
end;

procedure TForm1.Button1Click(Sender: TObject);

  function TestGrammar:String;
  begin
    if PageControl1.ActivePage=TabPEG then
    begin
      SingleQuote.Character:=#146;
      result:=PEGPEG.Text;
    end
    else
    begin
      SingleQuote.Character:='''';
      result:=MathPEG.Text;
    end;
  end;

begin
  if PEG_Log<>nil then
     PEG_Log.BeginUpdate;

  try
    if PEG_Log<>nil then
       PEG_Log.Clear;

    try
      Grammar.Free;
      Grammar:=nil;

      Grammar:=PEG.Load(TestGrammar);

    finally
      if CBTokens.Checked then
         TPEGTree.AddSyntax(PEG.Grammar.Parser, TreeView1);

      MemoRules.Lines.Text:=Grammar.AsString;
    end;

  finally
    if PEG_Log<>nil then
       PEG_Log.EndUpdate;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Grammar.Free;
  PEG.Free;
end;

procedure TForm1.MemoEvalChange(Sender: TObject);
begin
  if Grammar<>nil then
     MemoOutput.Text:=Grammar.Load(MemoEval.Text).AsString;
end;

procedure TForm1.VerifyDefaultGrammar;

  function CleanPEG:TStrings;
  var t : Integer;
      tmp : String;
  begin
    result:=TStringList.Create;

    for t:=0 to PEGPEG.Lines.Count-1 do
    begin
      tmp:=Trim(PEGPEG.Lines[t]);

      if (tmp<>'') and (Copy(tmp,1,1)<>'#') then
         result.Add(tmp)
    end;
  end;

var A,B : TStrings;
      t : Integer;
begin
  A:=CleanPEG;
  try
    B:=MemoRules.Lines;

    for t:=0 to Min(A.Count,B.Count)-1 do
    begin
      if A[t]<>B[t] then
         raise Exception.Create('Different line: '+IntToStr(t)+' '+A[t]);
    end;

    if A.Count<>B.Count then
       ShowMessage('Number of lines differ: '+IntToStr(A.Count)+' '+IntToStr(B.Count));
  finally
    A.Free;
  end;
end;

end.
