unit unBallsInMotion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unHelperInterfaces, uncollisiontypes, Graphics, Types;

type

  { TCirclePathCalculator }

  TCirclePathCalculator = class(TInterfacedObject, IPathPlotter, IBasicLoggerClient)
  private
    FintfLogger: IBasicLogger;

    FlstTimeslices: ITimesliceList;

    function GetMaxTimeFromPathPartList(const lstPathParts: IPathPartList): double;

    function GetPathPartsStateAtTime(const lstPathParts: IPathPartList;
      const dTime: double): IPathPartList;
  protected
    function GetMovingPathPartsAtTime(const lstPathParts: IPathPartList;
      const dTime: double): IPathPartList;
    function GetStationaryPartsAtTime(const lstPathParts: IPathPartList;
      const dTime: double): IPathPartList;
    function GetNextCollisionFromTime(const lstPathParts: IPathPartList;
      const dTime: double): TEdgeHitDetail;

    function GetTimeslices: ITimesliceList;
    procedure LogMessage(const sMessage: string);
    procedure SetLogger(const intfLogger: IBasicLogger);
    function GetThePlotAtTime(const dTime: double): ITimeslice;
    procedure Reinitialize;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCircleWithPosition(const ACircle: ICircle; const Position: TPointF);
    procedure Clear;
    procedure GainThePlot;
    property Timeslices: ITimesliceList read GetTimeslices;
  end;

implementation

uses
  unCollisionDetection, ComObj, unOtherCircles, unCirclePhysics,
  unPathPartImplementation, unTimesliceImpl, Math, unCircleUtils;


{ TCirclePathCalculator }

function TCirclePathCalculator.GetMaxTimeFromPathPartList(
  const lstPathParts: IPathPartList): double;
var
  dTime: double;
  i: integer;
begin
  Result := 0;
  for i := 0 to pred(lstPathParts.Count) do
  begin
    dTime := TBasicMotion.GetTimeToStop(lstPathParts[i].Vector.InitialVelocity);
    if (dTime > Result) then Result := dTime;
  end;
end;

function TCirclePathCalculator.GetPathPartsStateAtTime(
  const lstPathParts: IPathPartList;
  const dTime: double): IPathPartList;
var
  i: integer;
  ASourcePathPart, ATargetPathPart: IPathPart;
  ATargetVector: IBasicVector;
  APointAtTime: TPointF;
  dVelocityAtTime, dAngle: double;
begin
  Result := TPathPartList.Create;
  for i := 0 to pred(lstPathParts.Count) do
  begin
    ASourcePathPart := lstPathParts[i];

    APointAtTime := TPointF.Create(ASourcePathPart.Vector.GetXAtTime(dTime),
      ASourcePathPart.Vector.GetYAtTime(dTime));

    dVelocityAtTime := TBasicMotion.GetVelocityAtTime(
      ASourcePathPart.Vector.InitialVelocity, dTime);

    dAngle := ASourcePathPart.Vector.Angle;

    ATargetVector := TBasicVector.Create(APointAtTime, dVelocityAtTime, dAngle, 0);

    ATargetPathPart := TPathPart.Create(ASourcePathPart.Circle, ATargetVector);

    Result.add(ATargetPathPart);
  end;

end;

function TCirclePathCalculator.GetMovingPathPartsAtTime(
  const lstPathParts: IPathPartList; const dTime: double): IPathPartList;
var
  i: integer;
begin
  Result := TPathPartList.Create;
  for i := 0 to pred(lstPathParts.Count) do
  begin
    if lstPathParts[i].Vector.GetVelocityAtTime(dTime) > 0 then
      Result.add(lstPathParts[i]);
  end;
end;

function TCirclePathCalculator.GetStationaryPartsAtTime(
  const lstPathParts: IPathPartList; const dTime: double): IPathPartList;
var
  i: integer;
begin
  Result := TPathPartList.Create;
  for i := 0 to pred(lstPathParts.Count) do
  begin
    if lstPathParts[i].Vector.GetVelocityAtTime(dTime) <= 0 then
      Result.add(lstPathParts[i]);
  end;
end;

function TCirclePathCalculator.GetNextCollisionFromTime(
  const lstPathParts: IPathPartList; const dTime: double): TEdgeHitDetail;
var
  intfMovingPathParts: IPathPartList;
  intfStationaryPathParts: IPathPartList;
  i, j: integer;
  dEarliestHitTime: double;
  dEdgeHitTime: double;
  intfPathPart: IPathPart;
  AEdgeHit: TEdgeHit;
  intfEdgeHitPathPart: IPathPart;
  intfCircleCollisionResult: ICircleCollisionResult;
  intfStoreCircleCollisionResult: ICircleCollisionResult;
  iCircleId: integer;
begin
  Result.EdgeHit := ehNone;
  Result.HitTime := 0;
  Result.iCircleId := -1;
  AEdgeHit := ehNone;

  // for each moving circle find the next edge hit and return the detail from the 1st collision
  intfMovingPathParts := GetMovingPathPartsAtTime(lstPathParts, dTime);
  intfStationaryPathParts := GetStationaryPartsAtTime(lstPathParts, dTime);
  dEarliestHitTime := -1;
  for i := 0 to pred(intfMovingPathParts.Count) do
  begin

    intfPathPart := intfMovingPathParts[i];

    // Test if circle hits edge of game board
    dEdgeHitTime := dEarliestHitTime;
    AEdgeHit := ehNone;
    TCollisionDetection.DetectEdgeHits(intfPathPart, dEdgeHitTime, AEdgeHit);
    if (AEdgeHit <> ehNone) and ((dEarliestHitTime < 0) or
      (dEdgeHitTime < dEarliestHitTime)) then
    begin
      intfEdgeHitPathPart := intfPathPart;
      dEarliestHitTime := dEdgeHitTime;
      iCircleId := TCircleUtils.GetCircleId(intfPathPart.Circle);
    end;


    for j := 0 to pred(intfStationaryPathParts.Count) do
    begin
      intfCircleCollisionResult :=
        TCollisionDetection.DetectStationaryCircleHit(intfPathPart,
        intfStationaryPathParts[j]);
      if supports(intfCircleCollisionResult, ICircleCollisionResult) and
        ((intfCircleCollisionResult.HitTime < dEarliestHitTime) or
        (dEarliestHitTime < 0)) then
      begin
        // store circle collision result
        AEdgeHit := ehCircle;
        intfStoreCircleCollisionResult := intfCircleCollisionResult;
        dEarliestHitTime := intfCircleCollisionResult.HitTime;
      end;
    end;

  end;

  if AEdgeHit = ehCircle then
  begin
    Result.EdgeHit := ehCircle;
    Result.HitTime := intfCircleCollisionResult.HitTime;
    Result.intfDetails := intfStoreCircleCollisionResult;
  end
  else if (AEdgeHit <> ehNone) then
  begin
    Result.EdgeHit := AEdgeHit;
    Result.HitTime := dEarliestHitTime;
    Result.iCircleId := iCircleId;
    Result.intfDetails := intfEdgeHitPathPart;
  end
  else
  begin
    Result.EdgeHit := ehNone;
    Result.HitTime := -1;
    Result.intfDetails := nil;
  end;
end;


function TCirclePathCalculator.GetTimeslices: ITimesliceList;
begin
  Result := FlstTimeslices;
end;

procedure TCirclePathCalculator.LogMessage(const sMessage: string);
begin
  if supports(FintfLogger, IBasicLogger) then
    FintfLogger.LogMessage(sMessage);
end;

procedure TCirclePathCalculator.SetLogger(const intfLogger: IBasicLogger);
begin
  FintfLogger := intfLogger;
end;

function TCirclePathCalculator.GetThePlotAtTime(const dTime: double): ITimeslice;
var
  i: integer;
  intfTimeslice: ITimeslice;

begin
  i := 0;
  Result := nil;

  while (i < FlstTimeslices.Count) do
  begin
    intfTimeslice := FlstTimeslices[i];
    if (intfTimeslice.StartTime <= dTime) and
      ((intfTimeslice.EndTime >= dTime) or (i = pred(FlstTimeslices.Count))) then
    begin
      Result := intfTimeslice;
      exit;
    end
    else
      Inc(i);
  end;

end;

procedure TCirclePathCalculator.Reinitialize;
var
  intfLastTimeslice: ITimeslice;
  i: integer;
  intfCircle: ICircle;
  intfVector: IBasicVector;
  pt: TPointF;
  dDuration: double;

begin

  intfLastTimeslice := FlstTimeslices[pred(FlstTimeslices.Count)];
  FlstTimeslices.Clear;
  dDuration := intfLastTimeslice.EndTime - intfLastTimeslice.StartTime;
  for i := 0 to pred(intfLastTimeslice.PathParts.Count) do
  begin
    intfCircle := intfLastTimeslice.PathParts[i].Circle;
    intfVector := TCircleUtils.GetPathPartForCircleID(
      intfLastTimeSlice.PathParts, TCircleUtils.GetCircleId(intfCircle)).Vector;
    pt.X := intfVector.GetXAtTime(dDuration);
    pt.Y := intfVector.GetYAtTime(dDuration);
    intfVector.Origin := pt;
    intfVector.InitialVelocity := 0.2;
    AddCircleWithPosition(intfCircle, pt);
  end;

end;

constructor TCirclePathCalculator.Create;
begin
  // FlstCircles := TCirclesList.Create;
  FlstTimeslices := TTimesliceList.Create;
  FintfLogger := nil;
end;

destructor TCirclePathCalculator.Destroy;
begin
  FlstTimeslices := nil;
  // FlstCircles := nil;
  inherited Destroy;
end;

procedure TCirclePathCalculator.AddCircleWithPosition(const ACircle: ICircle;
  const Position: TPointF);
var
  intfTimeSlice: ITimeslice;
  intfPathParts: IPathPartList;
  intfPathPart: IPathPart;
  intfVector: IBasicVector;
begin
  if (FlstTimeslices = nil) then
    FlstTimeslices := TTimesliceList.Create;

  if (FlstTimeslices.Count = 0) then
  begin
    intfPathParts := TPathPartList.Create;
    intfTimeSlice := TTimeslice.Create(intfPathParts);
    intfTimeSlice.StartTime := 0;
    FlstTimeslices.Add(intfTimeSlice);
  end
  else
    intfPathParts := FlstTimeslices[0].PathParts;

  intfVector := TBasicVector.Create(Position, 0, 0, 0);
  intfPathPart := TPathPart.Create(ACircle, intfVector);
  intfPathParts.Add(intfPathPart);

end;

procedure TCirclePathCalculator.Clear;
begin
  FlstTimeslices.Clear;
end;


procedure TCirclePathCalculator.GainThePlot;
var
  dLastEndTime, dMaxSliceTime: double;
  AHitDetail: TEdgeHitDetail;
  APathPartList: IPathPartList;
  APathPart, APathPart2: IPathPart;
  intfTimeslice: ITimeslice;
  intfCircleCollisionResult: ICircleCollisionResult;
  BounceResult: TBounceResult;
begin

  // Record starting position
  APathPartList := FlstTimeslices[0].PathParts;

  FlstTimeslices.Clear;
  dLastEndTime := 0;

  repeat
    AHitDetail.EdgeHit := ehNone;
    dMaxSliceTime := GetMaxTimeFromPathPartList(APathPartList);
    intfTimeslice := TTimeslice.Create(APathPartList);
    intfTimeslice.StartTime := dLastEndTime;
    intfTimeslice.EndTime := dLastEndTime + dMaxSliceTime;


    AHitDetail := GetNextCollisionFromTime(intfTimeslice.PathParts, 0);
    if (AHitDetail.EdgeHit <> ehNone) then
    begin
      intfTimeslice.EndTime := AHitDetail.HitTime + intfTimeslice.StartTime;
      dLastEndTime := intfTimeslice.EndTime;
      FlstTimeslices.add(intfTimeslice);
      LogMessage(intfTimeslice.ToString);

      //Copy the path parts from the previous timeslice to a new timeslice (with positions and velocities adjusted)
      APathPartList := GetPathPartsStateAtTime(intfTimeslice.PathParts,
        AHitDetail.HitTime);

      // adjust the angle of the item(s) affected
      if (AHitDetail.EdgeHit <> ehCircle) then
      begin
        APathPart := TCircleUtils.GetPathPartForCircleID(APathPartList,
          AHitDetail.iCircleId);
        case AHitDetail.EdgeHit of
          ehLeft, ehRight: APathPart.Vector.ReverseX;
          ehTop, ehBottom: APathPart.Vector.ReverseY;
        end;
      end
      else
      begin
        // handle circle hit
        if not supports(AHitDetail.intfDetails, ICircleCollisionResult,
          intfCircleCollisionResult) then
          raise Exception.Create('Failed to obtain ICircleCollisionResult');

        APathPart := TCircleUtils.GetPathPartForCircleID(APathPartList,
          intfCircleCollisionResult.CircleId1);
        APathPart2 := TCircleUtils.GetPathPartForCircleID(APathPartList,
          intfCircleCollisionResult.CircleId2);

        BounceResult := TCollisionDetection.CalculateBounceAfterHittingCircle(
          APathPart, intfCircleCollisionResult.Circle1XAtHit,
          intfCircleCollisionResult.Circle1YAtHit, APathPart2,
          AHitDetail.HitTime);


        APathPart.Vector.InitialVelocity :=
          Sqrt(Sqr(BounceResult.Vector1.Data[0]) +
          sqr(BounceResult.Vector1.Data[1]));
        APathPart.Vector.Angle :=
          ArcTan2(BounceResult.Vector1.Data[1], BounceResult.Vector1.Data[0]);
        APathPart.Vector.EndTime :=
          TBasicMotion.GetTimeToStop(APathPart.Vector.InitialVelocity);


        APathPart2.Vector.InitialVelocity :=
          Sqrt(Sqr(BounceResult.Vector2.Data[0]) +
          sqr(BounceResult.Vector2.Data[1]));
        APathPart2.Vector.Angle :=
          ArcTan2(BounceResult.Vector2.Data[1], BounceResult.Vector2.Data[0]);
        APathPart2.Vector.EndTime :=
          TBasicMotion.GetTimeToStop(APathPart2.Vector.InitialVelocity);
      end;

    end;

  until AHitDetail.EdgeHit = ehNone;

  FlstTimeslices.add(intfTimeslice);
  LogMessage(intfTimeslice.ToString);
end;


end.
