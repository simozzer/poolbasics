unit unBallsInMotion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unHelperInterfaces, uncollisiontypes, fgl, Graphics;

type

  { TCirclePathCalculator }

  TCirclePathCalculator = class(TInterfacedObject, IPathPlotter, IBasicLoggerClient)
  private
    FlstCircles: ICirclesList;
    FintfLogger: IBasicLogger;

    FlstTimeslices: ITimesliceList;
    function GetPathPartsFromCircles(const lstCircles: ICirclesList): IPathPartList;

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
    function GetThePlotAtTime(const dTime: double): ICirclesList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCircle(const ACircle: ICircle);
    procedure Clear;
    procedure GainThePlot;
    property Timeslices: ITimesliceList read GetTimeslices;
  end;

implementation

uses
  unCollisionDetection, ComObj, unOtherCircles, unCirclePhysics, Types,
  unPathPartImplementation, unTimesliceImpl;

function GetPathPartForCircleID(const intfList: IPathPartList; const iCircleID:  Integer): IPathPart;
var
  i : Integer;
  intfIdentity : IIdentity;
begin
  RESULT := nil;
  i := 0;
  while (i < intfList.Count) do
  begin
    if supports(intfList[i].Circle,IIdentity, intfIdentity) and (intfIdentity.Id = iCircleID) then
    begin
      RESULT := intfList[i];
      exit;
    end;
    inc(i);
  end;

end;

{ TCirclePathCalculator }

function TCirclePathCalculator.GetPathPartsFromCircles(
  const lstCircles: ICirclesList): IPathPartList;
var
  i: integer;
  intfObjectWithVector: IObjectWithVector;
  intfPathPart: IPathPart;
begin
  Result := TPathPartList.Create;
  for i := 0 to pred(lstCircles.Count) do
  begin
    if supports(lstCircles[i], IObjectWithVector, intfObjectWithVector) then
    begin
      intfPathPart := TPathPart.Create(lstCircles[i], intfObjectWithVector.Vector);
      Result.add(intfPathPart);
    end
    else
      raise Exception.Create('Could not obtain IObjectWithVector');
  end;

end;

function TCirclePathCalculator.GetMaxTimeFromPathPartList(
  const lstPathParts: IPathPartList): double;
var
  dTime: double;
  i: integer;
  intfObjectWithVector: IObjectWithVector;
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

    ATargetVector := TBasicVector.Create(APointAtTime,
      dVelocityAtTime, dAngle, 0);

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
  intfObjectWithVector: IObjectWithVector;
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
  intfObjectWithVector: IObjectWithVector;
  AEdgeHit: TEdgeHit;
  intfEdgeHitPathPart: IPathPart;
  intfCircleCollisionResult: ICircleCollisionResult;
  intfStoreCircleCollisionResult: ICircleCollisionResult;
  iPathPartIndex: integer;
  intfIdentity : IIdentity;
begin
  Result.EdgeHit := ehNone;
  Result.HitTime := 0;
  Result.iPathPartId := -1;
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
    TCollisionDetection.DetectEdgeHits(intfPathPart.Vector,
      intfPathPart.Circle.Radius, dEdgeHitTime, AEdgeHit);
    if (AEdgeHit <> ehNone) and ((dEarliestHitTime < 0) or
      (dEdgeHitTime < dEarliestHitTime)) then
    begin
      intfEdgeHitPathPart := intfPathPart;
      dEarliestHitTime := dEdgeHitTime;
      if supports(intfPathPart.Circle, IIdentity, intfIdentity) then
        iPathPartIndex := intfIdentity.Id;
    end;

    // Test if circle hits a stationary circle
      {*
      for j := 0 to pred(intfStationaryPathParts.Count) do
      begin
        intfCircleCollisionResult :=
          TCollisionDetection.DetectStationaryCircleHit(intfCircle,
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
      *}
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
    Result.iPathPartId := iPathPartIndex;
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

function TCirclePathCalculator.GetThePlotAtTime(const dTime: double): ICirclesList;
var
  i: integer;
  intfTimeslice: ITimeslice;
begin
  i := 0;
  Result := nil;
  //TODO

  if (Result = nil) then Result := FlstCircles;
end;

constructor TCirclePathCalculator.Create;
begin
  FlstCircles := TCirclesList.Create;
  FlstTimeslices := TTimesliceList.Create;
  FintfLogger := nil;
end;

destructor TCirclePathCalculator.Destroy;
begin
  FlstTimeslices := nil;
  FlstCircles := nil;
  inherited Destroy;
end;

procedure TCirclePathCalculator.AddCircle(const ACircle: ICircle);
begin
  FlstCircles.add(ACircle);
end;

procedure TCirclePathCalculator.Clear;
begin
  FlstCircles.Clear;
end;


procedure TCirclePathCalculator.GainThePlot;
var
  dLastDuration, dLastEndTime, dMaxSliceTime: double;
  AHitDetail: TEdgeHitDetail;
  APathPartList: IPathPartList;
  APathPart: IPathPart;
  intfTimeslice: ITimeslice;
  intfPathPart: IPathPart;
  intfCircleCollisionResult: ICircleCollisionResult;
  intfCopy: IUnknown;
begin
  FlstTimeslices.Clear;

  // Record starting position
  APathPartList := GetPathPartsFromCircles(FlstCircles);

  dLastEndTime := 0;
  dLastDuration:= 0;

  repeat
    AHitDetail.EdgeHit := ehNone;
    dMaxSliceTime := GetMaxTimeFromPathPartList(APathPartList);
    intfTimeslice := TTimeslice.Create(APathPartList);
    intfTimeslice.StartTime := dLastEndTime;
    intfTimeslice.EndTime:= dLastEndTime + dMaxSliceTime;


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

      LogMessage('PRE: ' + intfTimeslice.ToString());

      // adjust the angle of the item(s) affected
      APathPart := GetPathPartForCircleID(APathPartList, AHitDetail.iPathPartId);
      LogMessage(Format('Start: %f',[APathPart.Vector.Angle]));
      case AHitDetail.EdgeHit of
        ehLeft, ehRight: APathPart.Vector.ReverseX;
        ehTop, ehBottom: APathPart.Vector.ReverseY;
      end;
      LogMessage(Format('End: %f',[APathPart.Vector.Angle]));

            LogMessage('POST: ' + intfTimeslice.ToString());
            dLastDuration:= AHitDetail.HitTime;
    end;

  until AHitDetail.EdgeHit = ehNone;

  FlstTimeslices.add(intfTimeslice);
  LogMessage(intfTimeslice.ToString);
end;


end.
