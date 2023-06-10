unit frmCirclePhysicsMain;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  MaskEdit, ExtCtrls, ComCtrls, unGameBoard,
  unCirclePhysics, BGRABitmap, unHelperInterfaces,
  unOtherCircles, unAngleSelectorControl, unBallsInMotion, Types;

type

  { TForm1 }

  TForm1 = class(TForm, IBasicLogger)
    actTrigger: TAction;
    ActionList1: TActionList;
    btnClearLog: TButton;
    btnLessAngle: TButton;
    btnMoreAngle: TButton;
    btnRenderFrame: TButton;
    btnTestMove: TButton;
    btnDrawRects: TButton;
    btnTimeAdd: TButton;
    btnTimeSubtract: TButton;
    btnTrigger: TButton;
    chkContinueRandom: TCheckBox;
    edtTimeIncrement: TEdit;
    edtTime: TEdit;
    grpTakeShot: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblTimeMs: TLabel;
    lblAngle: TLabel;
    lblTimeIncrement: TLabel;
    lblFrameTime: TLabel;
    lblVel: TLabel;
    lstEvents: TMemo;
    ImageList1: TImageList;
    AnimationTimer: TTimer;
    Panel1: TPanel;
    trkVelocity: TTrackBar;
    procedure actRenderExecute(Sender: TObject; const dTime: double);
    procedure actTriggerExecute(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);

    procedure btnClearLogClick(Sender: TObject);
    procedure btnDrawRectsClick(Sender: TObject);
    procedure btnLessAngleClick(Sender: TObject);
    procedure btnMoreAngleClick(Sender: TObject);
    procedure btnRenderFrameClick(Sender: TObject);
    procedure btnTimeAddClick(Sender: TObject);
    procedure btnTimeSubtractClick(Sender: TObject);
    procedure btnTestMoveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure trkVelocityChange(Sender: TObject);


  private
    FBoard: TCaromGameBoard;

    FcStartAnimationTime: cardinal;

    FbDraggingBall: boolean;
    FbDraggingDirection: boolean;

    FAngleControl: TAngleControl;

    FiPuckID: integer;

    FPathCalculator: IPathPlotter;

    FiLastRenderedTimesliceIndex: integer;

    procedure DrawTrajectoryPaths;

    procedure LogMessage(const sMessage: string);


    procedure HandleBoardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleBoardMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure HandleBoardMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    function GetPuck: ICircle;
    function GetVelocity: double;
    function AddTargetCircle(pt: TPointF; const sText: string;
      const clrBrush, clrPen: TColor): ICircle;

  public
    procedure DoAngleChanged(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation


uses
  uncirclephysicsconstants, unCircleUtils, Math;

{$R *.lfm}

type
  pRectF = ^TRectF;


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
begin
  actTrigger.Enabled := False;


  FiLastRenderedTimesliceIndex := -1;
  FPathCalculator.GainThePlot;
  //DrawTrajectoryPaths;
  FcStartAnimationTime := GetTickCount64;

  AnimationTimer.Enabled := True;
end;

{ Called at fixed intervals - will update the display of circle positions for a
  given point in time }
procedure TForm1.AnimationTimerTimer(Sender: TObject);
var
  cTimeSinceStart: cardinal;
  dTotalVelocity: double;
  i: integer;
  intfTimeslice: ITimeslice;
  intfVector: IBasicVector;
  APathPart: IPathPart;
begin
  cTimeSinceStart := GetTickCount64 - FcStartAnimationTime;

  // TODO: REMOVE (speed up for tests)
 // cTimeSinceStart:= cTimeSinceStart div 4;
  lblTimeMs.Caption := IntToStr(cTimeSinceStart);

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

    FPathCalculator.Reinitialize;

    if chkContinueRandom.Checked then
    begin
      APathPart := TCircleUtils.GetPathPartForCircleID(
        FPathCalculator.Timeslices[0].PathParts, FiPuckID);
      if supports(APathPart, IPathPart) then
      begin
        intfVector := APathPart.Vector;
        intfVector.Angle := Random * (2 * pi);
        intfVector.InitialVelocity := 0.5 + Random * 1.5;
        actTrigger.Enabled := True;
        actTriggerExecute(Self);
      end;

    end;

  end
  else
  begin
    actRenderExecute(Self, cTimeSinceStart);


    // Draw Trajectory at intervals
    {
    iTimesliceIndex := FPathCalculator.Timeslices.indexOf(intfTimeslice);
    if (iTimeSliceIndex >= FiLastRenderedTimesliceIndex) then
    begin
      dTimeInSlice:= cTimeSinceStart - intfTimeslice. StartTime;
      if (dTimeInSlice < 30) then
      begin
        DrawTrajectoryPaths;
        FiLastRenderedTimesliceIndex:= iTimeSliceIndex;
    end;
    }

  end;

end;

// Clear the event log (used for debugging)
procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  FAngleControl.Repaint;
  lstEvents.Clear;
end;


procedure TForm1.btnDrawRectsClick(Sender: TObject);


  function GetLimitRect(AVector: IBasicVector; ACircle: ICircle): TRectF;
  var
    dLeft, dRight, dTop, dBottom, dRadius, xAtSTop, yAtStop, originX, originY: double;
  begin
    dRadius := ACircle.Radius;
    xAtSTop := AVector.GetXAtStop;
    YAtStop := AVector.GetYAtStop;
    originX := AVector.Origin.X;
    OriginY := AVector.Origin.Y;

    if originX < XAtStop then
    begin
      // Moving Right
      dLeft := originX - dRadius;
      dRight := XAtStop + dRadius;
    end
    else
    begin
      // Moving Left;
      dLeft := XAtStop - dRadius;
      dRight := OriginX + dRadius;
    end;

    if YAtStop > originY then
    begin
      // Moving Down
      dTop := OriginY - dRadius;
      dBottom := YAtStop + dRadius;
    end
    else
    begin
      // Moving Up;
      dTop := YAtStop - dRadius;
      dBottom := OriginY + dRadius;
    end;

    Result.Left := dLeft;
    Result.Right := dRight;
    Result.Top := dTop;
    Result.Bottom := dBottom;
  end;


  function IntersectRectF(const R1, R2: TRectF; pInterectRect: pRectF): Boolean;
  var
    lRect: TRectF;
  begin
    lRect := R1;
    if R2.Left > R1.Left then
      lRect.Left := R2.Left;
    if R2.Top > R1.Top then
      lRect.Top := R2.Top;
    if R2.Right < R1.Right then
      lRect.Right := R2.Right;
    if R2.Bottom < R1.Bottom then
      lRect.Bottom := R2.Bottom;

    if (lRect.Left > lRect.Right) or (lRect.Top > lRect.Bottom) then
    begin
      Result := False;
    end
    else
    begin
      Result := True;
      if assigned(pInterectRect) then
      begin
        pInterectRect^.Left:= lRect.Left;
        pInterectRect^.Top:= lRect.Top;
        pInterectRect^.Right:= lRect.Right;
        pInterectRect^.Bottom:= lRect.Bottom;
      end;
    end;
  end;

var
  intfPathPart: IPathPart;
  lRect1,lRect2, IntersectRect: TRectF;

begin

  FBoard.Canvas.Brush.Color:= clRed;
  intfPathPart := FPathCalculator.Timeslices[0].PathParts[0];
  if supports(intfPathPart, IPathPart) then
  begin
    lRect1 := GetLimitRect(intfPathPart.Vector, intfPathPart.Circle);
    FBoard.Canvas.Rectangle(Round(lRect1.Left), Round(lRect1.Top), Round(
      lRect1.Right), Round(lRect1.Bottom));
  end;

  intfPathPart := FPathCalculator.Timeslices[0].PathParts[1];
  if supports(intfPathPart, IPathPart) then
  begin
    lRect2 := GetLimitRect(intfPathPart.Vector, intfPathPart.Circle);
    FBoard.Canvas.Rectangle(Round(lRect2.Left), Round(lRect2.Top), Round(
      lRect2.Right), Round(lRect2.Bottom));

    IntersectRect.Left := 0;
    IntersectRect.Top := 0;
    IntersectRect.Right := 0;
    IntersectRect.Bottom := 0;
    If IntersectRectF(lRect1,lRect2,@IntersectRect) then
    begin
        FBoard.Canvas.Brush.Color:= clWhite;
        FBoard.Canvas.Rectangle(Round(IntersectRect.Left), Round(IntersectRect.Top), Round(
      IntersectRect.Right), Round(IntersectRect.Bottom));
    end;
  end;

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

procedure TForm1.btnTestMoveClick(Sender: TObject);
var
  ACircle: ICircle;
  intfVector: IBasicVector;
  iCircleID: integer;
  dVelocity: double;
begin
  //  TODO
  FPathCalculator.Clear;

  dVelocity := 0.75;

  ACircle := AddTargetCircle(TPointF.Create(BOARD_WIDTH div 2,
    BOARD_HEIGHT - TARGET_RADIUS), 'PUCK', clYellow, clBlack);
  iCircleId := TCircleUtils.GetCircleId(ACircle);
  intfVector := TCircleUtils.GetPathPartForCircleID(
    FPathCalculator.Timeslices[0].PathParts, iCircleId).Vector;
  intfVector.Angle := -pi / 2;
  intfVector.InitialVelocity := dVelocity;

  ACircle := AddTargetCircle(TPointF.Create(BOARD_WIDTH -
    TARGET_RADIUS, BOARD_HEIGHT div 2), 'target', clBlue, clBlack);
  iCircleId := TCircleUtils.GetCircleId(ACircle);
  intfVector := TCircleUtils.GetPathPartForCircleID(
    FPathCalculator.Timeslices[0].PathParts, iCircleId).Vector;
  intfVector.Angle := pi + 0.01;
  intfVector.InitialVelocity := dVelocity;

  chkContinueRandom.Checked := False;

  actRenderExecute(Self, 0);
 //actTriggerExecute(Self);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  actRenderExecute(Self, 0);
end;

// Change the intended velocity when the slider in the UI is changed
procedure TForm1.trkVelocityChange(Sender: TObject);
var
  intfVector: IBasicVector;
  dVelocity: double;
begin
  dVelocity := GetVelocity;

  intfVector := TCircleUtils.GetPathPartForCircleID(
    FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
  intfVector.InitialVelocity := dVelocity;

  DrawTrajectoryPaths;
end;

// Draw the paths which will result for the chosen angle and velocity
procedure TForm1.DrawTrajectoryPaths;
var
  i, j: integer;
  BoardCanvas: TCanvas;
  lstTimeslices: ITimesliceList;
  timeslice: ITimeslice;
  lstPathParts: IPathPartList;
  intfPathPart: IPathPart;
  dStartTime, dEndTime, dDuration: double;
begin
  lstEvents.Clear;
  FPathCalculator.GainThePlot;


  FBoard.Render;

  BoardCanvas := FBoard.BoardCanvas;
  BoardCanvas.Brush.Color := clGray;
  BoardCanvas.Pen.Color := clBlack;

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
    intfVector := TCircleUtils.GetPathPartForCircleID(
      FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
    intfVector.Origin := TPointF.Create(x, y);
    intfVector.Angle := FAngleControl.Angle + pi;
    intfVector.InitialVelocity := GetVelocity;

    DrawTrajectoryPaths;
  end
  else if (Button = mbRight) then
  begin
    FbDraggingDirection := True;
    actTrigger.Enabled := True;
    intfVector := TCircleUtils.GetPathPartForCircleID(
      FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
    intfVector.Angle := arctan2(Y - intfVector.Origin.y, X - intfVector.Origin.X);
    intfVector.InitialVelocity := GetVelocity;
    FAngleControl.Angle := intfVector.Angle + pi;
    DrawTrajectoryPaths;
  end;
end;

// Move the position of the puck when the left mouse button is held
procedure TForm1.HandleBoardMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  intfVector: IBasicVector;
begin
  if FbDraggingBall then
  begin
    intfVector := TCircleUtils.GetPathPartForCircleID(
      FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
    intfVector.Origin := TPointF.Create(x, y);
    intfVector.InitialVelocity := GetVelocity;

    DrawTrajectoryPaths;
  end
  else if (FbDraggingDirection) then
  begin
    intfVector := TCircleUtils.GetPathPartForCircleID(
      FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
    intfVector.Angle := arctan2(Y - intfVector.Origin.y, X - intfVector.Origin.X);
    intfVector.InitialVelocity := GetVelocity;
    FAngleControl.Angle := intfVector.Angle + pi;
    DrawTrajectoryPaths;
  end;

end;

// Finalise dragging the puck
procedure TForm1.HandleBoardMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  intfVector: IBasicVector;
begin
  if FbDraggingBall then
  begin
    intfVector := TCircleUtils.GetPathPartForCircleID(
      FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
    intfVector.Origin := TPointF.Create(x, y);
    intfVector.InitialVelocity := GetVelocity;

    DrawTrajectoryPaths;
    FbDraggingBall := False;
  end
  else if FbDraggingDirection then
  begin
    intfVector := TCircleUtils.GetPathPartForCircleID(
      FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
    intfVector.Angle := arctan2(Y - intfVector.Origin.y, X - intfVector.Origin.X);
    intfVector.InitialVelocity := GetVelocity;
    FAngleControl.Angle := intfVector.Angle + pi;
    DrawTrajectoryPaths;
    FbDraggingDirection := False;
  end;
end;

function TForm1.GetPuck: ICircle;
begin
  Result := TCircleUtils.GetCircleFromPathParts(
    FPathCalculator.Timeslices[0].PathParts, FiPuckID);
end;

function TForm1.GetVelocity: double;
begin
  Result := (100 - trkVelocity.Position) / 50;
end;

// Called when the user changes the selected angle
procedure TForm1.DoAngleChanged(Sender: TObject);
var
  dAngle: double;
  intfVector: IBasicVector;
begin
  dAngle := FAngleControl.Angle + pi;
  if (dAngle > 2 * pi) then dAngle := dAngle - 2 * pi;


  intfVector := TCircleUtils.GetPathPartForCircleID(
    FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
  intfVector.InitialVelocity := GetVelocity;
  intfVector.Angle := dAngle;

  dAngle := TBasicMotion.RadToDeg(dAngle);
  lblAngle.Caption := Format('Angle: %f', [dAngle]);



  actRenderExecute(Self, 0);



  DrawTrajectoryPaths;
end;


function TForm1.AddTargetCircle(pt: TPointF; const sText: string;
  const clrBrush, clrPen: TColor): ICircle;
var
  intfCircle: ICircle;
begin
  intfCircle := TBaseCircle.Create(TARGET_RADIUS, TARGET_MASS);
  intfCircle.BrushColor := clrBrush;
  intfCircle.PenColor := clrPen;
{$IFDEF DEBUG}
  intfCircle.Text := sText;
{$ENDIF}
  // Add a small amount of random to the position for each circle
  pt.X := pt.X + Math.RandomRange(0, 3000) / 6000;
  pt.Y := pt.Y + Math.RandomRange(0, 3000) / 6000;
  FPathCalculator.AddCircleWithPosition(intfCircle, pt);
  Result := intfCircle;
end;


constructor TForm1.Create(AOwner: TComponent);
var
  ACircle: ICircle;
  intfVector: IBasicVector;
  dHalfWidth, dHalfHeight: double;
begin
  inherited Create(AOwner);
  FBoard := TCaromGameBoard.Create(Self);
  InsertControl(FBoard);
  FBoard.Top := 100;
  FBoard.Left := 12;
  FBoard.OnMouseDown := @HandleBoardMouseDown;
  FBoard.OnMouseMove := @HandleBoardMouseMove;
  FBoard.OnMouseUp := @HandleBoardMouseUp;


  dHalfHeight := BOARD_HEIGHT / 2;
  dHalfWidth := BOARD_WIDTH / 2;

  FPathCalculator := TCirclePathCalculator.Create;

  AddTargetCircle(TPointF.Create(dHalfWidth, dHalfHeight), 'Queen',
    clRed, clMaroon);
  AddTargetCircle(TPointF.Create(dHalfWidth, dHalfHeight -
    (2 * (TARGET_RADIUS + 0.5))), 'Black 1', clDkGray, clBlack);
  AddTargetCircle(TPointF.Create(300, dHalfHeight - (4 * (TARGET_RADIUS + 0.5))),
    'White1', clBlue, clBlack);
  AddTargetCircle(TPointF.Create(300, dHalfHeight + (2 * (TARGET_RADIUS + 0.5))),
    'White2', clBlue, clBlack);
  AddTargetCircle(TPointF.Create(300, dHalfHeight + (4 * (TARGET_RADIUS + 0.5))),
    'White3', clBlue, clBlack);


  AddTargetCircle(TPointF.Create(dHalfWidth - (TARGET_RADIUS * 2) +
    0.5, dHalfHeight - (3 * (TARGET_RADIUS + 0.5)))
    , 'Black2', clDkGray, clBlack);

  AddTargetCircle(TPointF.Create(dHalfWidth - (TARGET_RADIUS * 2) +
    0.5, dHalfHeight - (1 * (TARGET_RADIUS + 0.5))),
    'White4', clBlue, clBlack);

  AddTargetCircle(TPointF.Create(dHalfWidth - (TARGET_RADIUS * 2) +
    0.5, dHalfHeight - (-1 * (TARGET_RADIUS + 0.5))), 'Black4', clDkGray, clBlack);

  AddTargetCircle(TPointF.Create(dHalfWidth - (TARGET_RADIUS * 2) +
    0.5, dHalfHeight - (-3 * (TARGET_RADIUS + 0.5))), 'Black6', clDkGray, clBlack);




  AddTargetCircle(TPointF.Create(dHalfWidth + (TARGET_RADIUS * 2) -
    0.5, dHalfHeight - (3 * (TARGET_RADIUS + 0.5))),
    'Black3', clDkGray, clBlack);

  AddTargetCircle(
    TPointF.Create(dHalfWidth + (TARGET_RADIUS * 2) - 0.5, dHalfHeight -
    (1 * (TARGET_RADIUS + 0.5))),
    'White5', clBlue, clBlack);



  AddTargetCircle(TPointF.Create(dHalfWidth + (TARGET_RADIUS * 2) -
    0.5, dHalfHeight - (-1 * (TARGET_RADIUS + 0.5))), 'Black5', clDkGray, clBlack);


  AddTargetCircle(TPointF.Create(dHalfWidth + (TARGET_RADIUS * 2) -
    0.5, dHalfHeight - (-3 * (TARGET_RADIUS + 0.5))), 'Black6', clDkGray, clBlack);



  AddTargetCircle(TPointF.Create(dHalfWidth - (TARGET_RADIUS * 4) + 0.5, dHalfHeight),
    'Black7', clDkGray, clBlack);


  AddTargetCircle(TPointF.Create(dHalfWidth - (TARGET_RADIUS * 4) +
    0.5, dHalfHeight - 2 * (TARGET_RADIUS + 0.5)), 'White6', clBlue, clBlack);


  AddTargetCircle(TPointF.Create(dHalfWidth + (TARGET_RADIUS * 4) -
    0.5, dHalfHeight - 2 * (TARGET_RADIUS + 0.5)), 'White9', clBlue, clBlack);

  AddTargetCircle(TPointF.Create(dHalfWidth + (TARGET_RADIUS * 4) - 0.5, dHalfHeight),
    'Black8', clDkGray, clBlack);


  AddTargetCircle(TPointF.Create(dHalfWidth + (TARGET_RADIUS * 4) -
    0.5, dHalfHeight + 2 * (TARGET_RADIUS + 0.5)), 'White7', clBlue, clBlack);

  AddTargetCircle(TPointF.Create(dHalfWidth + (TARGET_RADIUS * 4) -
    0.5, dHalfHeight + 2 * (TARGET_RADIUS + 0.5)), 'White8', clBlue, clBlack);


  // bottom left
  AddTargetCircle(TPointF.Create(dHalfWidth - (TARGET_RADIUS * 4) -
    0.5, dHalfHeight + 2 * (TARGET_RADIUS + 0.5)), 'White8', clBlue, clBlack);



  {


  AddTargetCircle(TPointF.Create(326, 285), 'White8', clBlue, clBlack);

  ;
  }


  ACircle := TBaseCircle.Create(PUCK_RADIUS, PUCK_MASS);
  ACircle.BrushColor := clYellow;
  ACircle.PenColor := clBlack;
  {$IFDEF DEBUG}
  ACircle.Text := 'Puck';
  {$ENDIF}
  FiPuckId := TCircleUtils.GetCircleId(ACircle);
  FPathCalculator.AddCircleWithPosition(ACircle, TPointF.Create(200, 518));


  FAngleControl := TAngleControl.Create(Self);
  grpTakeShot.InsertControl(FAngleControl);
  FAngleControl.Left := 10;
  FAngleControl.Top := 80;
  FAngleControl.OnChange := @Self.DoAngleChanged;
  FAngleControl.Angle := 0;

  intfVector := TCircleUtils.GetPathPartForCircleID(
    FPathCalculator.Timeslices[0].PathParts, FiPuckID).Vector;
  intfVector.Angle := pi;
  intfVector.InitialVelocity := 0.5;

  FbDraggingBall := False;
  FbDraggingDirection := False;

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
