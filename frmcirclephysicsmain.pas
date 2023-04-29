unit frmCirclePhysicsMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  MaskEdit, ExtCtrls, ComCtrls, unGameBoard,
  unCirclePhysics, unTrajectoryPaths, BGRABitmap, unHelperInterfaces,
  unOtherCircles, unAngleSelectorControl;

type

  { TForm1 }

  TForm1 = class(TForm, IBasicLogger)
    actTrigger: TAction;
    ActionList1: TActionList;
    btnRenderFrame: TButton;
    btnTimeAdd: TButton;
    btnTimeSubtract: TButton;
    btnClearLog: TButton;
    btnTrigger: TButton;
    chkUpdatePosition: TCheckBox;
    edtTimeIncrement: TEdit;
    edtTime: TEdit;
    grpTakeShot: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblAngle: TLabel;
    lblTimeIncrement: TLabel;
    lblFrameTime: TLabel;
    lblVel: TLabel;
    lstEvents: TListBox;
    trkVelocity: TTrackBar;
    ImageList1: TImageList;
    AnimationTimer: TTimer;
    procedure actRenderExecute(Sender: TObject; const dTime: double);
    procedure actTriggerExecute(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);

    procedure btnClearLogClick(Sender: TObject);
    procedure btnRenderFrameClick(Sender: TObject);
    procedure btnTimeAddClick(Sender: TObject);
    procedure btnTimeSubtractClick(Sender: TObject);
    procedure trkVelocityChange(Sender: TObject);
  private
    FBoard: TCaromGameBoard;
    FBallVector: TBasicVector;
    FcStartAnimationTime: cardinal;
    FTrajectories: ITrajectoryPaths;
    FlstCircles: TCirclesList;
    FdVelocity: double;

    FAngleControl: TAngleControl;
    procedure PlotTrajectories;
    procedure LogMessage(const sMessage: string);
    procedure HandleBoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  public
    procedure DoAngleChanged(Sender: TObject);
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
  i: integer;
begin
  FBoard.Render;
  BoardCanvas := FBoard.BoardCanvas;

  BoardCanvas.Brush.Color := clGray;
  BoardCanvas.Pen.Color := clBlack;
  dBallCenterX := FTrajectories.GetXAtTime(dTime);
  dBallCenterY := FTrajectories.GetYAtTime(dTime);
  BoardCanvas.Ellipse(Round(dBallCenterX - PUCK_RADIUS),
    Round(dBallCenterY - PUCK_RADIUS),
    round(dBallCenterX + PUCK_RADIUS),
    ROUND(dBallCenterY + PUCK_RADIUS));

  for i := 0 to pred(FlstCircles.Count) do
  begin
    FlstCircles[i].Render(BoardCanvas);
  end;

  FBoard.Invalidate;
end;

procedure TForm1.actTriggerExecute(Sender: TObject);
var
  i: integer;
begin
  actTrigger.Enabled := False;

  for i := 0 to pred(FlstCircles.Count) do
    TCircle(FlstCircles.Items[i]).Stationary := True;

  PlotTrajectories;

  FcStartAnimationTime := GetTickCount64;
  AnimationTimer.Enabled := True;
end;

procedure TForm1.AnimationTimerTimer(Sender: TObject);
var
  cTimeSinceStart: cardinal;
  AVector: TBasicVector;

  dVectorDuration: double;
begin
  cTimeSinceStart := GetTickCount64 - FcStartAnimationTime;

  AVector := FTrajectories.GetVectorForTime(cTimeSinceStart);
  if (AVector = nil) then
  begin

    AnimationTimer.Enabled := False;


    if (FTrajectories.Count > 0) then
    begin
      AVector := FTrajectories.getItem(Pred(FTrajectories.GetCount));
      if (AVector <> nil) then
      begin
        edtTime.Text := FloatToStr(AVector.EndTime);
        dVectorDuration := AVector.EndTime - AVector.StartTime;
        FBallVector.OriginX := FBallVector.GetXAtTime(AVector.EndTime);
        FBallVector.Originy := FBallVector.GetyAtTime(AVector.EndTime);
      end;
    end;

  end
  else
  begin
    actRenderExecute(Self, cTimeSinceStart);
  end;

end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  FAngleControl.Repaint;
  lstEvents.Clear;
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

procedure TForm1.trkVelocityChange(Sender: TObject);
begin
  FdVelocity := (100 - trkVelocity.Position) / 100;
  lblVel.Caption := Format('Vel: %f', [FdVelocity]);
end;

procedure TForm1.PlotTrajectories;
var
  APathPart: TBasicVector;
begin
  lstEvents.Clear;
  FBallVector.InitialVelocity := FdVelocity;
  FBallVector.Angle := FAngleControl.Angle + pi;

  FTrajectories.Items.Clear;
  FTrajectories.SetCircles(FlstCircles);

  APathPart := TBasicVector.Create(FBallVector.OriginX, FBallVector.OriginY,
    FBallVector.InitialVelocity, FBallVector.Angle, 0);
  FTrajectories.Items.Add(APathPart);

  FTrajectories.CalculateTrajectories;

end;

procedure TForm1.LogMessage(const sMessage: string);
begin
  lstEvents.Items.Add(sMessage);
end;

procedure TForm1.HandleBoardMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if AnimationTimer.Enabled then exit;
  FBallVector.OriginX := x;
  FBallVector.OriginY := y;
  FBallVector.Angle := 0;
  FBallVector.InitialVelocity := 0;
  actTrigger.Enabled:=true;
  PlotTrajectories;
  actRenderExecute(Self, 0);
end;

procedure TForm1.DoAngleChanged(Sender: TObject);
var
  dAngle: double;
begin
  dAngle := FAngleControl.Angle + pi;
  if (dAngle > 2 * pi) then dAngle := dAngle - 2 * pi;
  dAngle := dAngle * (180 / pi);
  lblAngle.Caption := Format('Angle: %f', [dAngle]);
end;

constructor TForm1.Create(AOwner: TComponent);
var
  ACircle: TCircle;
begin
  inherited Create(AOwner);
  FBoard := TCaromGameBoard.Create(Self);
  InsertControl(FBoard);
  FBoard.Top := 100;
  FBoard.Left := 12;
  FBoard.OnMouseDown := @HandleBoardMouseDown;

  FBallVector := TBasicVector.Create(300, 300, 1, 0.0, 0);
  FTrajectories := TTrajectoryPath.Create;
  (FTrajectories as IBasicLoggerClient).SetLogger(Self);

  FlstCircles := TCirclesList.Create;
  ACircle := TCircle.Create(300, 300, TARGET_RADIUS);
  FlstCircles.Add(ACircle);

  FAngleControl := TAngleControl.Create(Self);
  grpTakeShot.InsertControl(FAngleControl);
  FAngleControl.Left := 10;
  FAngleControl.Top := 80;
  FAngleControl.OnChange := @Self.DoAngleChanged;
  FAngleControl.Angle := 0;

  FdVelocity := 0.5;

end;

destructor TForm1.Destroy;
begin
  RemoveControl(FBoard);
  FBoard.Free;
  FTrajectories := nil;
  FBallVector.Free;
  FlstCircles.Clear;
  FlstCircles.Free;
  grpTakeShot.RemoveControl(FAngleControl);
  FAngleControl.Free;

  inherited Destroy;
end;

end.
