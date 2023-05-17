unit frmCirclePhysicsMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  MaskEdit, ExtCtrls, ComCtrls, unGameBoard,
  unCirclePhysics, BGRABitmap, unHelperInterfaces,
  unOtherCircles, unAngleSelectorControl, unBallsInMotion;

type

  { TForm1 }

  TForm1 = class(TForm, IBasicLogger)
    actTrigger: TAction;
    ActionList1: TActionList;
    btnMoreAngle: TButton;
    btnRenderFrame: TButton;
    btnTimeAdd: TButton;
    btnTimeSubtract: TButton;
    btnClearLog: TButton;
    btnTrigger: TButton;
    btnDrawTrajectory: TButton;
    btnLessAngle: TButton;
    chkUpdatePosition: TCheckBox;
    edtTimeIncrement: TEdit;
    edtTime: TEdit;
    grpTakeShot: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblCos: TLabel;
    lblAngle: TLabel;
    lblSin: TLabel;
    lblTimeIncrement: TLabel;
    lblFrameTime: TLabel;
    lblVel: TLabel;
    lstEvents: TMemo;
    trkVelocity: TTrackBar;
    ImageList1: TImageList;
    AnimationTimer: TTimer;
    procedure actRenderExecute(Sender: TObject; const dTime: double);
    procedure actTriggerExecute(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);

    procedure btnClearLogClick(Sender: TObject);
    procedure btnDrawTrajectoryClick(Sender: TObject);
    procedure btnLessAngleClick(Sender: TObject);
    procedure btnMoreAngleClick(Sender: TObject);
    procedure btnRenderFrameClick(Sender: TObject);
    procedure btnTimeAddClick(Sender: TObject);
    procedure btnTimeSubtractClick(Sender: TObject);
    procedure trkVelocityChange(Sender: TObject);

  private
    FBoard: TCaromGameBoard;

    FcStartAnimationTime: cardinal;
    FlstCircles: ICirclesList;
    FbDraggingBall: boolean;

    FAngleControl: TAngleControl;

    FPuck: ICircle;

    FPathCalculator: IPathPlotter;

    FlstTimeslices: ITimesliceList;

    procedure DrawTrajectoryPaths;

    procedure LogMessage(const sMessage: string);

    procedure RenderCircle(const ACanvas: TCanvas; const intfCircle: ICircle);

    procedure HandleBoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleBoardMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure HandleBoardMouseUp(Sender: TObject; Button: TMouseButton;
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
  uncirclephysicsconstants, types;

{$R *.lfm}


{ TForm1 }

procedure TForm1.RenderCircle(const ACanvas: TCanvas; const intfCircle: ICircle);
var
  intfVectorAccess: IObjectWithVector;
  intfVector: IBasicVector;
  dRadius: double;
begin
  if not supports(intfCircle, IObjectWithVector, intfVectorAccess) then
    raise Exception.Create('Could not obtain IObjectWithVector')
  else
    intfVector := intfVectorAccess.GetBasicVector;

  ACanvas.Brush.color := intfCircle.BrushColor;
  ACanvas.Pen.Color := intfCircle.PenColor;
  dRadius := intfCircle.Radius;
  ACanvas.Ellipse(Round(intfVector.Origin.X - dRadius),
    Round(intfVector.Origin.Y - dRadius),
    Round(intfVector.Origin.X + dRadius), Round(intfVector.Origin.Y + dRadius));
end;

// Render the positions of all the circles for a given point in time
procedure TForm1.actRenderExecute(Sender: TObject; const dTime: double);
var
  BoardCanvas: TCanvas;
  dBallCenterX, dBallCenterY, dRadius: double;
  i: integer;
  lstCircles: IScreenCirclesList;
begin
  FBoard.Render;
  BoardCanvas := FBoard.BoardCanvas;

  BoardCanvas.Brush.Color := clGray;
  BoardCanvas.Pen.Color := clBlack;


  lstCircles := FPathCalculator.GetThePlotAtTime(dTime);

  if supports(lstCircles, IScreenCirclesList) then
    for i := 0 to pred(lstCircles.Count) do
    begin
      lstCircles[i].Render(BoardCanvas);
//      RenderCircle(BoardCanvas, lstCircles[i]);
    end;

  FBoard.Invalidate;
end;

// Start the animation to show the path of each of the circles
procedure TForm1.actTriggerExecute(Sender: TObject);
var
  i: integer;
begin
  actTrigger.Enabled := False;


     FPathCalculator.GainThePlot;
     FlstTimeslices := FPathCalculator.GetTimeslices;
  FcStartAnimationTime := GetTickCount64;

  AnimationTimer.Enabled := True;
end;

{ Called at fixed intervals - will update the display of circle positions for a
  given point in time }
procedure TForm1.AnimationTimerTimer(Sender: TObject);
var
  cTimeSinceStart: cardinal;
  AVector: IBasicVector;
begin
  cTimeSinceStart := GetTickCount64 - FcStartAnimationTime;
   {
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
        FBallVector.Origin := TPointF.Create(FBallVector.GetXAtTime(AVector.EndTime),
        FBallVector.GetyAtTime(AVector.EndTime));
      end;
    end;

  end
  else
  }
  begin
    actRenderExecute(Self, cTimeSinceStart);
  end;

end;

// Clear the event log (used for debugging)
procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  FAngleControl.Repaint;
  lstEvents.Clear;
end;

// Draw the trajectories of all objects
procedure TForm1.btnDrawTrajectoryClick(Sender: TObject);
begin
  DrawTrajectoryPaths;
end;

// Decrease the target angle by a small amount
procedure TForm1.btnLessAngleClick(Sender: TObject);
begin
  FAngleControl.Angle := FAngleControl.Angle - ANGLE_MICRO_CHANGE;
end;

// Increase the target angle by a small amount
procedure TForm1.btnMoreAngleClick(Sender: TObject);
begin
  FAngleControl.Angle := FAngleControl.Angle + ANGLE_MICRO_CHANGE;
end;

// Draw the position of all objects at a specific point in time (for debugging)
procedure TForm1.btnRenderFrameClick(Sender: TObject);
begin
  actRenderExecute(Self, StrToFloatDef(edtTime.Text, 0.00));
end;

// Advance the time for drawing a frame by a specified amount and draw it (for debugging)
procedure TForm1.btnTimeAddClick(Sender: TObject);
begin
  edtTime.Text := FloatToStr(StrToFloatDef(edtTime.Text, 0.0) +
    StrToFloatDef(edtTimeIncrement.Text, 0.0));
  actRenderExecute(Self, StrToFloatDef(edtTime.Text, 0.0));
end;

// Retard the time for drawing a frame by a specified amount and draw it (for debugging)
procedure TForm1.btnTimeSubtractClick(Sender: TObject);
begin
  edtTime.Text := FloatToStr(StrToFloatDef(edtTime.Text, 0.0) -
    StrToFloatDef(edtTimeIncrement.Text, 0.0));
  actRenderExecute(Self, StrToFloatDef(edtTime.Text, 0.0));
end;

// Change the intended velocity when the slider in the UI is changed
procedure TForm1.trkVelocityChange(Sender: TObject);
var
  intfVector: IBasicVector;
  dVelocity: double;
begin
  dVelocity := (100 - trkVelocity.Position) / 100;
  lblVel.Caption := Format('Vel: %f', [dVelocity]);

  intfVector := TCircleInterfaceAccess.GetVectorFromCircle(FPuck);
  intfVector.InitialVelocity := dVelocity;

  DrawTrajectoryPaths;
end;

// Draw the paths which will result for the chosen angle and velocity
procedure TForm1.DrawTrajectoryPaths;
var
  i, j: integer;
  AVector: IBasicVector;
  BoardCanvas: TCanvas;
  dBallCenterX, dBallCenterY: double;
  lstTimeslices: ITimesliceList;
  timeslice: ITimeslice;
  lstPathParts: IPathPartList;
  intfPathPart: IPathPart;
  intfVector: IBasicVector;
  dStartTime, dEndTime, dDuration: double;
begin
  lstEvents.Clear;
  FPathCalculator.GainThePlot;


  FBoard.Render;

  BoardCanvas := FBoard.BoardCanvas;
  BoardCanvas.Brush.Color := clGray;
  BoardCanvas.Pen.Color := clBlack;

  for i := 0 to pred(FlstCircles.Count) do
  begin
    RenderCircle(BoardCanvas, FlstCircles[i]);
  end;


  lstTimeslices := FPathCalculator.GetTimeslices;
  for i := 0 to pred(lstTimeslices.Count) do
  begin
    timeslice := lstTimeslices[i];
    dStartTime := timeslice.StartTime;
    dEndTime := timeslice.EndTime;
    dDuration := dEndTime - dStartTime;
    lstPathParts := timeslice.PathParts;
    for j := 0 to pred(lstPathParts.Count) do
    begin
      intfPathPart := lstPathParts[j];
      BoardCanvas.Pen.Color := intfPathPart.Circle.PenColor;
      BoardCanvas.Brush.Color := intfPathPart.Circle.BrushColor;
      BoardCanvas.Ellipse(Round(intfPathPart.Vector.Origin.X -
        intfPathPart.Circle.Radius),
        Round(intfPathPart.Vector.Origin.Y - intfPathPart.Circle.Radius),
        Round(intfPathPart.Vector.Origin.X + intfPathPart.Circle.Radius),
        Round(intfPathPart.Vector.Origin.Y + intfPathPart.Circle.Radius)
        );

      BoardCanvas.Pen.Color := intfPathPart.Circle.BrushColor;
      BoardCanvas.MoveTo(round(intfPathPart.Vector.Origin.X),
        round(intfPathPart.Vector.Origin.Y));
      BoardCanvas.LineTo(Round(intfPathPart.Vector.GetXAtTime(dDuration)),
        Round(intfPathPart.Vector.GetYAtTime(dDuration)));

    end;
  end;


  FBoard.Invalidate;
end;


// Add a message to the debug log
procedure TForm1.LogMessage(const sMessage: string);
begin
  lstEvents.Lines.Add(sMessage);
end;

// Move the position of the puck when the game board is clicked
procedure TForm1.HandleBoardMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  intfVector: IBasicVector;
begin
  if AnimationTimer.Enabled then exit;
  FbDraggingBall := True;
  actTrigger.Enabled := True;
  intfVector := TCircleInterfaceAccess.GetVectorFromCircle(FPuck);
  intfVector.Origin := TPointF.Create(x, y);
  intfVector.Angle := FAngleControl.Angle;
  intfVector.InitialVelocity := (100 - (trkVelocity.Position / 100)) / 100;
  actRenderExecute(Self, 0);
  DrawTrajectoryPaths;
end;

// Move the position of the puck when the left mouse button is held
procedure TForm1.HandleBoardMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  intfVector: IBasicVector;
begin
  if not FbDraggingBall then exit;
  intfVector := TCircleInterfaceAccess.GetVectorFromCircle(FPuck);
  intfVector.Origin := TPointF.Create(x, y);
  actRenderExecute(Self, 0);
  DrawTrajectoryPaths;

end;

// Finalise dragging the puck
procedure TForm1.HandleBoardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  intfVector: IBasicVector;
begin
  if not FbDraggingBall then exit;
  intfVector := TCircleInterfaceAccess.GetVectorFromCircle(FPuck);
  intfVector.Origin := TPointF.Create(x, y);
  //actRenderExecute(Self, 0);
    DrawTrajectoryPaths;
  FbDraggingBall := False;
end;

// Called when the user changes the selected angle
procedure TForm1.DoAngleChanged(Sender: TObject);
var
  dAngle: double;
  intfVector: IBasicVector;
begin
  dAngle := FAngleControl.Angle + pi;
  if (dAngle > 2 * pi) then dAngle := dAngle - 2 * pi;

  lblCos.Caption := Format('Cos: %f', [Cos(dAngle)]);

  lblSin.Caption := Format('Sin: %f', [Sin(dAngle)]);

  intfVector := TCircleInterfaceAccess.GetVectorFromCircle(FPuck);
  intfVector.Angle := dAngle;

  dAngle := TBasicMotion.RadToDeg(dAngle);
  lblAngle.Caption := Format('Angle: %f', [dAngle]);



  actRenderExecute(Self, 0);



  DrawTrajectoryPaths;
end;

constructor TForm1.Create(AOwner: TComponent);
var
  ACircle: ICircle;
  intfVector: IBasicVector;
  i: integer;
begin
  inherited Create(AOwner);
  FBoard := TCaromGameBoard.Create(Self);
  InsertControl(FBoard);
  FBoard.Top := 100;
  FBoard.Left := 12;
  FBoard.OnMouseDown := @HandleBoardMouseDown;
  FBoard.OnMouseMove := @HandleBoardMouseMove;
  FBoard.OnMouseUp := @HandleBoardMouseUp;


  FlstCircles := TCirclesList.Create;
  ACircle := TMovingCircle.Create(TPointF.Create(300, 300), PUCK_RADIUS-2, PUCK_MASS-0.2);
  ACircle.BrushColor := clAqua;
  ACircle.PenColor := clBlue;
  FlstCircles.Add(ACircle);
  FPuck := TMovingCircle.Create(TPointF.Create(200, 518), PUCK_RADIUS, PUCK_MASS);
  FPuck.BrushColor := clRed;
  FPuck.PenColor := clYellow;
  FlstCircles.Add(FPuck);

  FAngleControl := TAngleControl.Create(Self);
  grpTakeShot.InsertControl(FAngleControl);
  FAngleControl.Left := 10;
  FAngleControl.Top := 80;
  FAngleControl.OnChange := @Self.DoAngleChanged;
  FAngleControl.Angle := 0;

  intfVector := TCircleInterfaceAccess.GetVectorFromCircle(FPuck);
  intfVector.Angle := pi;
  intfVector.InitialVelocity := 0.5;

  FbDraggingBall := False;

  FPathCalculator := TCirclePathCalculator.Create;
  for i := 0 to pred(FlstCircles.Count) do
  begin
    FPathCalculator.AddCircle(FlstCircles[i]);
  end;
  (FPathCalculator as IBasicLoggerClient).SetLogger(Self);

end;

destructor TForm1.Destroy;
begin
  RemoveControl(FBoard);
  FBoard.Free;

  FlstCircles.Clear;
  FlstCircles := nil;
  grpTakeShot.RemoveControl(FAngleControl);
  FAngleControl.Free;

  FPathCalculator := nil;

  inherited Destroy;
end;

end.
