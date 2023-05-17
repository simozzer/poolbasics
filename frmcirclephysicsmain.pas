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

    FbDraggingBall: boolean;

    FAngleControl: TAngleControl;

    FiPuckID : Integer;

    FPathCalculator: IPathPlotter;

    procedure DrawTrajectoryPaths;

    procedure LogMessage(const sMessage: string);


    procedure HandleBoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleBoardMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure HandleBoardMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    function GetPuck: ICircle;

  public
    procedure DoAngleChanged(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation


uses
  uncirclephysicsconstants, types, unCircleUtils;

{$R *.lfm}


{ TForm1 }


// Render the positions of all the circles for a given point in time
procedure TForm1.actRenderExecute(Sender: TObject; const dTime: double);
var
  BoardCanvas: TCanvas;
  i: integer;
  intfTimeslice: ITimeslice;
  intfPathPart: IPathPart;
  ptPosition: TPointF;
  dRadius: double;
begin
  FBoard.Render;
  BoardCanvas := FBoard.BoardCanvas;

  intfTimeslice := FPathCalculator.GetThePlotAtTime(dTime);

  if supports(intfTimeslice, ITimeslice) then
    for i := 0 to pred(intfTimeslice.PathParts.Count) do
    begin
      intfPathPart := intfTimeslice.PathParts[i];


      ptPosition.X := intfPathPart.Vector.GetXAtTime(dTime - intfTimeslice.StartTime);
      ptPosition.Y := intfPathPart.Vector.GetYAtTime(dTime - intfTimeslice.StartTime);
      BoardCanvas.Pen.Color := intfPathPart.Circle.GetPenColor;
      BoardCanvas.Brush.Color := intfPathPart.Circle.GetBrushColor;
      dRadius := intfPathPart.Circle.Radius;
      BoardCanvas.Ellipse(round(ptPosition.X - dRadius),
        round(ptPosition.Y - dRadius),
        round(ptPosition.X + dRadius),
        round(ptPosition.Y + dRadius));

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
  FcStartAnimationTime := GetTickCount64;

  AnimationTimer.Enabled := True;
end;

{ Called at fixed intervals - will update the display of circle positions for a
  given point in time }
procedure TForm1.AnimationTimerTimer(Sender: TObject);
var
  cTimeSinceStart: cardinal;
  AVector: IBasicVector;
  dTotalVelocity: double;
  i: integer;
  intfCircle: ICircle;
  intfTimeslice: ITimeslice;
  lstCirclesAtStop: ICirclesList;
  intfPathPart: IPathPart;
  ptStop: TPointF;
begin
  cTimeSinceStart := GetTickCount64 - FcStartAnimationTime;

  intfTimeslice := FPathCalculator.GetThePlotAtTime(cTimeSinceStart);

  dTotalVelocity := 0;
  if supports(intfTimeslice, ITimeslice) then
    for i := 0 to pred(intfTimeslice.PathParts.Count) do
      dTotalVelocity += intfTimeslice.PathParts[i].Vector.GetVelocityAtTime(
        cTimeSinceStart - intfTimeslice.StartTime);


  if (dTotalVelocity = 0) then
  begin

    AnimationTimer.Enabled := False;
    actTrigger.Enabled := True;


    // TODO:: reduse origional circles (clone last
    FPathCalculator.Reinitialize;

  end
  else
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

  intfVector := TCircleUtils.GetPathPartForCircleID(FPathCalculator.Timeslices[0].PathParts,FiPuckID).Vector;
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

  {
  for i := 0 to pred(FlstCircles.Count) do
  begin
    RenderCircle(BoardCanvas, FlstCircles[i]);
  end;
  }


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

  if (Button = mbLeft) then
  begin
    FbDraggingBall := True;
    actTrigger.Enabled := True;
    intfVector := TCircleUtils.GetPathPartForCircleID(FPathCalculator.Timeslices[0].PathParts,FiPuckID).Vector;
    intfVector.Origin := TPointF.Create(x, y);
    intfVector.Angle := FAngleControl.Angle + pi;
    intfVector.InitialVelocity := (100 - (trkVelocity.Position / 100)) / 100;

    DrawTrajectoryPaths;
  end;
end;

// Move the position of the puck when the left mouse button is held
procedure TForm1.HandleBoardMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  intfVector: IBasicVector;
begin
  if  FbDraggingBall then
  begin
    intfVector := TCircleUtils.GetPathPartForCircleID(FPathCalculator.Timeslices[0].PathParts,FiPuckID).Vector;
    intfVector.Origin := TPointF.Create(x, y);

      DrawTrajectoryPaths;
  end;

end;

// Finalise dragging the puck
procedure TForm1.HandleBoardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  intfVector: IBasicVector;
begin
  if not FbDraggingBall then exit;
  intfVector := TCircleUtils.GetPathPartForCircleID(FPathCalculator.Timeslices[0].PathParts,FiPuckID).Vector;
  intfVector.Origin := TPointF.Create(x, y);

  DrawTrajectoryPaths;
  FbDraggingBall := False;
end;

function TForm1.GetPuck: ICircle;
begin
  RESULT := TCircleUtils.GetCircleFromPathParts(FPathCalculator.Timeslices[0].PathParts,FiPuckID);
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

  intfVector := TCircleUtils.GetPathPartForCircleID(FPathCalculator.Timeslices[0].PathParts,FiPuckID).Vector;
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
  pt, pt1 : TPointF;

  lstCircles : ICirclesList;
begin
  inherited Create(AOwner);
  FBoard := TCaromGameBoard.Create(Self);
  InsertControl(FBoard);
  FBoard.Top := 100;
  FBoard.Left := 12;
  FBoard.OnMouseDown := @HandleBoardMouseDown;
  FBoard.OnMouseMove := @HandleBoardMouseMove;
  FBoard.OnMouseUp := @HandleBoardMouseUp;


  FPathCalculator := TCirclePathCalculator.Create;

  ACircle := TBaseCircle.Create(TARGET_RADIUS, TARGET_MASS);
  ACircle.BrushColor:= clAqua;
  ACircle.PenColor:= clBlue;

  FPathCalculator.AddCircleWithPosition(ACircle, TPointF.Create(300,300));

  ACircle := TBaseCircle.Create(PUCK_RADIUS, PUCK_MASS);
  ACircle.BrushColor:= clGray;
  ACircle.PenColor := clBlack;

  FiPuckId := TCircleUtils.GetCircleId(ACircle);
  FPathCalculator.AddCircleWithPosition(ACircle, TPointF.Create(200,518));


  FAngleControl := TAngleControl.Create(Self);
  grpTakeShot.InsertControl(FAngleControl);
  FAngleControl.Left := 10;
  FAngleControl.Top := 80;
  FAngleControl.OnChange := @Self.DoAngleChanged;
  FAngleControl.Angle := 0;

  intfVector := TCircleUtils.GetPathPartForCircleID(FPathCalculator.Timeslices[0].PathParts,FiPuckID).Vector;
  intfVector.Angle := pi;
  intfVector.InitialVelocity := 0.5;

  FbDraggingBall := False;

  (FPathCalculator as IBasicLoggerClient).SetLogger(Self);

end;

destructor TForm1.Destroy;
begin
  RemoveControl(FBoard);
  FBoard.Free;


  grpTakeShot.RemoveControl(FAngleControl);
  FAngleControl.Free;

  FPathCalculator := nil;

  inherited Destroy;
end;

end.
