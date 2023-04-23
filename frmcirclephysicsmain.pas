unit frmCirclePhysicsMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  MaskEdit, ExtCtrls, TAGraph, TASeries, unBoardRenderer,
  unCirclePhysics, unTrajectoryPaths, BGRABitmap, bgPanel, unHelperInterfaces;

type

  { TForm1 }

  TForm1 = class(TForm, IBasicLogger)
    actTrigger: TAction;
    ActionList1: TActionList;
    btnTrigger: TButton;
    btnRenderFrame: TButton;
    btnTimeAdd: TButton;
    btnTimeSubtract: TButton;
    btnCalc: TButton;
    chaDistance: TChart;
    chaDisplacementLineSeries: TLineSeries;
    chaDistanceToBottomLineSeries: TLineSeries;
    chaDistanceToLeftLineSeries: TLineSeries;
    chaDistanceToRightLineSeries: TLineSeries;
    chaDistanceToTop: TChart;
    chaDistanceToBottom: TChart;
    chaDistanceToRight: TChart;
    chaDistanceToLeft: TChart;
    chaDistanceToTopLineSeries: TLineSeries;
    chaVelocity: TChart;
    edtDegrees: TEdit;
    edtOriginX: TEdit;
    edtOriginY: TEdit;
    edtTimeIncrement: TEdit;
    edtTime: TEdit;
    Label1: TLabel;
    lblSin: TLabel;
    lblRadians: TLabel;
    lblDegrees: TLabel;
    lblOriginX: TLabel;
    lblOriginY: TLabel;
    lblCos: TLabel;
    lblTimeIncrement: TLabel;
    lblFrameTime: TLabel;
    lstEvents: TListBox;
    VelocityLineSeries: TLineSeries;
    edtVelocity: TEdit;
    edtAngle: TEdit;
    ImageList1: TImageList;
    lblVelocity: TLabel;
    lblAngle: TLabel;
    AnimationTimer: TTimer;
    procedure actRenderExecute(Sender: TObject; const dTime: double);
    procedure actTriggerExecute(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);

    procedure btnCalcClick(Sender: TObject);
    procedure btnRenderFrameClick(Sender: TObject);
    procedure btnTimeAddClick(Sender: TObject);
    procedure btnTimeSubtractClick(Sender: TObject);

  private
    FBoardRenderer: TBoardRenderer;
    FBallVector: TBasicVector;
    FcStartAnimationTime: cardinal;
    FTrajectories: ITrajectoryPaths;
    procedure PlotTrajectories;
    procedure LogMessage(const sMessage: String);


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation


uses
  uncirclephysicsconstants;
{$R *.lfm}






{ TForm1 }

procedure TForm1.actRenderExecute(Sender: TObject; const dTime: double);
var
  BoardCanvas: TCanvas;
  dBallCenterX, dBallCenterY: double;
begin
  FBoardRenderer.Render;
  BoardCanvas := FBoardRenderer.Bitmap.Canvas;

  BoardCanvas.Brush.Color := clGray;
  BoardCanvas.Pen.Color := clWhite;
  dBallCenterX := FTrajectories.GetXAtTime(dTime);
  dBallCenterY := FTrajectories.GetYAtTime(dTime);
  BoardCanvas.Ellipse(Round(dBallCenterX - BALL_RADIUS),
    Round(dBallCenterY - BALL_RADIUS),
    round(dBallCenterX + BALL_RADIUS),
    ROUND(dBallCenterY + BALL_RADIUS));

  Canvas.Draw(10, 100, FBoardRenderer.Bitmap);
end;

procedure TForm1.actTriggerExecute(Sender: TObject);
begin
  actTrigger.Enabled := False;

  VelocityLineSeries.Clear;
  chaDisplacementLineSeries.Clear;
  chaDistanceToRightLineSeries.Clear;
  chaDistanceToLeftLineSeries.Clear;
  chaDistanceToBottomLineSeries.Clear;
  chaDistanceToTopLineSeries.Clear;

  PlotTrajectories;

  FcStartAnimationTime := GetTickCount64;
  AnimationTimer.Enabled := True;
end;

procedure TForm1.AnimationTimerTimer(Sender: TObject);
var
  cTimeSinceStart: cardinal;
  AVector: TBasicVector;
begin
  cTimeSinceStart := GetTickCount64 - FcStartAnimationTime;

  AVector := FTrajectories.GetVectorForTime(cTimeSinceStart);
  if (AVector = nil) then
  begin
    actTrigger.Enabled := True;
    AnimationTimer.Enabled := False;
  end
  else
  begin
    VelocityLineSeries.Add(TBasicMotion.GetVelocityAtTime(
      AVector.InitialVelocity, cTimeSinceStart));
    chaDisplacementLineSeries.Add(TBasicMotion.GetDistanceAtTime(
      AVector.InitialVelocity, cTimeSinceStart));

    chaDistanceToRightLineSeries.add(FBoardRenderer.Bitmap.Width -
      BALL_RADIUS - AVector.GetXAtTime(cTimeSinceStart));
    chaDistanceToLeftLineSeries.add(AVector.GetXAtTime(cTimeSinceStart) - BALL_RADIUS);
    chaDistanceToBottomLineSeries.add(FBoardRenderer.Bitmap.Height -
      BALL_RADIUS - AVector.GetYAtTime(cTimeSinceStart));
    chaDistanceToTopLineSeries.Add(AVector.GetYAtTime(cTimeSinceStart) - BALL_RADIUS);
    actRenderExecute(Self, cTimeSinceStart);
  end;

end;



procedure TForm1.btnCalcClick(Sender: TObject);
var
  dRadians: double;
begin
  dRadians := (pi / 180) * StrToFloatDef(edtDegrees.Text, 0.0);
  lblRadians.Caption := 'Rad ' + FloatToStr(dRadians);
  lblSin.Caption := 'Sin ' + FloatToStr(Sin(dRadians));
  lblCos.Caption := 'Cos ' + FloatToStr(Cos(dRadians));
end;

procedure TForm1.btnRenderFrameClick(Sender: TObject);
begin
  PlotTrajectories;
  actRenderExecute(Self, StrToFloatDef(edtTime.Text, 0.00));
end;

procedure TForm1.btnTimeAddClick(Sender: TObject);
begin
  edtTime.Text := FloatToStr(StrToFloatDef(edtTime.Text, 0.0) +
    StrToFloatDef(edtTimeIncrement.Text, 0.0));
  actRenderExecute(Self, StrToFloatDef(edtTime.Text, 0.0));
end;

procedure TForm1.btnTimeSubtractClick(Sender: TObject);
begin
  edtTime.Text := FloatToStr(StrToFloatDef(edtTime.Text, 0.0) -
    StrToFloatDef(edtTimeIncrement.Text, 0.0));
  actRenderExecute(Self, StrToFloatDef(edtTime.Text, 0.0));
end;


procedure TForm1.PlotTrajectories;
var
  APathPart: TBasicVector;
begin
  lstEvents.Clear;
  FBallVector.InitialVelocity := StrToFloatDef(edtVelocity.Text, 0.0);
  FBallVector.Angle := (Pi / 180) * StrToFloatDef(edtAngle.Text, 0.0);
  FBallVector.OriginX := StrToFloatDef(edtOriginX.Text, 0.0);
  FBallVector.OriginY := StrToFloatDef(edtOriginY.Text, 0.0);

  FTrajectories.Items.Clear;

  APathPart := TBasicVector.Create(FBallVector.OriginX, FBallVector.OriginY,
    FBallVector.InitialVelocity, FBallVector.Angle, 0);
  FTrajectories.Items.Add(APathPart);

  FTrajectories.CalculateTrajectories;

end;

procedure TForm1.LogMessage(const sMessage: String);
begin
  lstEvents.Items.Add(sMessage);
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoardRenderer := TBoardRenderer.Create(BOARD_WIDTH, BOARD_HEIGHT, clCream);
  FBallVector := TBasicVector.Create(300, 300, 1, 0.0, 0);
  FTrajectories := TTrajectoryPath.Create;
  (FTrajectories as IBasicLoggerClient).SetLogger(Self);
end;

destructor TForm1.Destroy;
begin
  FBoardRenderer.Free;
  FTrajectories := nil;
  FBallVector.Free;
  inherited Destroy;
end;

end.
