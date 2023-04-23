unit frmCirclePhysicsMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  MaskEdit, ExtCtrls, TAGraph, TASeries, unBoardRenderer,
  unCirclePhysics, unTrajectoryPaths;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FBoardRenderer: TBoardRenderer;
    FBallVector: TBasicVector;
    FcStartAnimationTime: cardinal;
    FTrajectories: TTrajectoryPath;
    procedure PlotTrajectories;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  RADIUS = 8;

type
  TEdgeHit = (ehNone, ehLeft, ehTop, ehRight, ehBottom);

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
  BoardCanvas.Ellipse(Round(dBallCenterX - RADIUS),
    Round(dBallCenterY - RADIUS),
    round(dBallCenterX + RADIUS),
    ROUND(dBallCenterY + RADIUS));

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
      RADIUS - AVector.GetXAtTime(cTimeSinceStart));
    chaDistanceToLeftLineSeries.add(AVector.GetXAtTime(cTimeSinceStart) - RADIUS);
    chaDistanceToBottomLineSeries.add(FBoardRenderer.Bitmap.Height -
      RADIUS - AVector.GetYAtTime(cTimeSinceStart));
    chaDistanceToTopLineSeries.Add(AVector.GetYAtTime(cTimeSinceStart) - RADIUS);
    actRenderExecute(Self, cTimeSinceStart);
  end;

end;

procedure TForm1.btnCalcClick(Sender: TObject);
var
  dRadians : double;
begin
  dRadians := (pi/180) * StrToFloatDef(edtDegrees.Text,0.0);
  lblRadians.Caption:= 'Rad ' + FloatToStr(dRadians);
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
  actRenderExecute(Self, StrToFloatDef(edtTime.Text,0.0));
end;

procedure TForm1.FormClick(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TForm1.PlotTrajectories;
var
  dTimeToStop, dXAtStop, dYatStop, dDeplacement, dEarliestHit, dHitTime: double;
  dXAtCollide, dyAtCollide, dVelAtCollide, dCollideTime, dPreCollisionAngle: Double;
  APathPart, aNextPathPart: TBasicVector;
  EdgeHit: TEdgeHit;
begin
  lstEvents.Clear;
  FBallVector.InitialVelocity := StrToFloatDef(edtVelocity.Text, 0.0);
  FBallVector.Angle := (Pi / 180) * StrToFloatDef(edtAngle.Text, 0.0);
  FBallVector.OriginX := StrToFloatDef(edtOriginX.Text, 0.0);
  FBallVector.OriginY := StrToFloatDef(edtOriginY.Text, 0.0);

  FTrajectories.Trajectories.Clear;



  APathPart := TBasicVector.Create(FBallVector.OriginX, FBallVector.OriginY,
    FBallVector.InitialVelocity, FBallVector.Angle, 0);
  FTrajectories.Trajectories.Add(APathPart);

  repeat
    EdgeHit := ehNone;

    dTimeToStop := TBasicMotion.GetTimeToStop(APathPart.InitialVelocity);
    dEarliestHit := dTimeToStop;

    dXAtStop := APathPart.GetXAtTime(dTimeToStop);
    dYatStop := APathPart.GetYAtTime(dTimeToStop);

    if (dXAtStop <= RADIUS) then
    begin
      dDeplacement := APathPart.OriginX - RADIUS;
      dHitTime := APathPart.GetTimeToXDeplacement(dDeplacement);
      if (dHitTime < dEarliestHit) and (dHitTime > 0) then
      begin
        EdgeHit := ehLeft;
        dEarliestHit := dHitTime;
      end;
    end;

    if (dYAtStop <= RADIUS) then
    begin
      dDeplacement := APathPart.OriginY - RADIUS;
      dHitTime := APathPart.GetTimeToYDeplacement(dDeplacement);
      if (dHitTime < dEarliestHit) and (dHitTime > 0) then
      begin
        EdgeHit := ehTop;
        dEarliestHit := dHitTime;
      end;
    end;

    if (dXAtStop >= (FBoardRenderer.Bitmap.Width - RADIUS)) then
    begin
      dDeplacement := FBoardRenderer.Bitmap.Width - RADIUS - APathPart.OriginX;
      dHitTime := APathPart.GetTimeToXDeplacement(dDeplacement);
      if (dHitTime < dEarliestHit) and (dHitTime > 0) then
      begin
        EdgeHit := ehRight;
        dEarliestHit := dHitTime;
      end;
    end;


    if (dYAtStop > (FBoardRenderer.Bitmap.Height - RADIUS)) then
    begin
      dDeplacement := FBoardRenderer.Bitmap.Height - RADIUS - APathPart.OriginY;
      dHitTime := APathPart.GetTimeToYDeplacement(dDeplacement);
      if (dHitTime < dEarliestHit) then
      begin
        EdgeHit := ehBottom;
        dEarliestHit := dHitTime;
      end;
    end;

    if (EdgeHit <> ehNone) then
    begin
      dXAtCollide := APathPart.GetXAtTime(dEarliestHit);
      dYAtCollide := APathPart.GetYAtTime(dEarliestHit);
      dVelAtCollide := TBasicMotion.GetVelocityAtTime(APathPart.InitialVelocity, dEarliestHit);
      dCollideTime := APathPart.StartTime + dEarliestHit;
      dPreCollisionAngle := APathPart.Angle;

      APathPart.EndTime:= dCollideTime;

      aNextPathPart := TBasicVector.Create(dXAtCollide, dyAtCollide,
       dVelAtCollide, dPreCollisionAngle, dCollideTime);

      FTrajectories.Trajectories.Add(aNextPathPart);

      APathPart := aNextPathPart;
      lstEvents.Items.Add(APathPart.ToString());
    end;


    case EdgeHit of
      ehNone: lstEvents.Items.Add('No Edge Hit');
      ehLeft:
      begin
        lstEvents.Items.Add('Hit left edge');
        APathPart.ReverseX;
      end;

      ehTop:
      begin
        lstEvents.Items.Add('Hit top edge');
        aNextPathPart.ReverseY();
      end;

      ehRight:
      begin
        lstEvents.Items.Add('Hit right edge');
        aNextPathPart.ReverseX();
      end;

      ehBottom:
      begin
        lstEvents.Items.Add('Hit bottom edge');
        aNextPathPart.ReverseY();
      end;

    end;
    if EdgeHit <> ehNone then
    begin
      lstEvents.Items.Add('End Time: ' + FloatToStr(APathPart.StartTime) +
        ', Angle: ' + FloatToStr(TBasicMotion.RadToDeg(APathPart.Angle)));

    end;

  until EdgeHit = ehNone;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoardRenderer := TBoardRenderer.Create(600, 600, clCream);
  FBallVector := TBasicVector.Create(300, 300, 1, 0.0, 0);
  FTrajectories := TTrajectoryPath.Create;
end;

destructor TForm1.Destroy;
begin
  FBoardRenderer.Free;
  FTrajectories.Free;
  FBallVector.Free;
  inherited Destroy;
end;

end.

