unit unTrajectoryPaths;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unCirclePhysics, unHelperInterfaces;

type

  ITrajectoryPaths = Interface['{98B877F0-13F2-4B85-AFFE-4E395428FF99}']
    function GetCount: Cardinal;
    function GetItems: TList;
    function getItem(const iIndex : Cardinal): TBasicVector;
    property Items: TList read GetItems;
    property Item[const iIndex : Cardinal]: TBasicVector read GetItem;
    property Count : Cardinal read GetCount;

    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetVectorForTime(const dTime: double): TBasicVector;
    procedure CalculateTrajectories;
  end;

  { TTrajectoryPath }

  TTrajectoryPath = class(TInterfacedObject, ITrajectoryPaths, IBasicLoggerClient)
  private
    FintfLogger: IBasicLogger;
    FTrajectories: TList;
  private
    procedure SetLogger(const intfLogger: IBasicLogger);
    procedure LogMessage(const sMessage: String);
    function GetCount: Cardinal;
    function GetItems: TList;
    function GetItem(const iIndex: cardinal): TBasicVector;

    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetVectorForTime(const dTime: double): TBasicVector;
    procedure CalculateTrajectories;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  TEdgeHit = (ehNone, ehLeft, ehTop, ehRight, ehBottom);


implementation

uses
  uncirclephysicsconstants;

{ TTrajectoryPath }

function TTrajectoryPath.GetVectorForTime(const dTime: double): TBasicVector;
var
  AVector: TBasicVector;
  i: integer;
begin
  i := 0;
  Result := nil;
  while (Result = nil) and (i < FTrajectories.Count) do
  begin
    AVector := TBasicVector(FTrajectories[i]);
    if (dTime >= AVector.StartTime) and (dTime <= AVector.EndTime) then
      Result := AVector
    else
      Inc(i);
  end;
  if (Result = nil) and (FTrajectories.Count > 0) then
    AVector := TBasicVector(FTrajectories[FTrajectories.Count - 1]);

end;

procedure TTrajectoryPath.CalculateTrajectories;
var
  dTimeToStop, dXAtStop, dYatStop, dDeplacement, dEarliestHit, dHitTime: double;
  dXAtCollide, dyAtCollide, dVelAtCollide, dCollideTime, dPreCollisionAngle: double;
  APathPart, aNextPathPart: TBasicVector;
  EdgeHit: TEdgeHit;
begin

  APathPart := TBasicVector(FTrajectories[0]);
  repeat
    EdgeHit := ehNone;


    dTimeToStop := TBasicMotion.GetTimeToStop(APathPart.InitialVelocity);
    dEarliestHit := dTimeToStop;

    dXAtStop := APathPart.GetXAtTime(dTimeToStop);
    dYatStop := APathPart.GetYAtTime(dTimeToStop);

    if (dXAtStop <= BALL_RADIUS) then
    begin
      dDeplacement := APathPart.OriginX - BALL_RADIUS;
      dHitTime := APathPart.GetTimeToXDeplacement(dDeplacement);
      if (dHitTime < dEarliestHit) and (dHitTime > 0) then
      begin
        EdgeHit := ehLeft;
        dEarliestHit := dHitTime;
      end;
    end;

    if (dYAtStop <= BALL_RADIUS) then
    begin
      dDeplacement := APathPart.OriginY - BALL_RADIUS;
      dHitTime := APathPart.GetTimeToYDeplacement(dDeplacement);
      if (dHitTime < dEarliestHit) and (dHitTime > 0) then
      begin
        EdgeHit := ehTop;
        dEarliestHit := dHitTime;
      end;
    end;

    if (dXAtStop >= (BOARD_WIDTH - BALL_RADIUS)) then
    begin
      dDeplacement := BOARD_WIDTH - BALL_RADIUS - APathPart.OriginX;
      dHitTime := APathPart.GetTimeToXDeplacement(dDeplacement);
      if (dHitTime < dEarliestHit) and (dHitTime > 0) then
      begin
        EdgeHit := ehRight;
        dEarliestHit := dHitTime;
      end;
    end;


    if (dYAtStop >= (BOARD_HEIGHT - BALL_RADIUS)) then
    begin
      dDeplacement := BOARD_HEIGHT - BALL_RADIUS - APathPart.OriginY;
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
      dVelAtCollide := TBasicMotion.GetVelocityAtTime(APathPart.InitialVelocity,
        dEarliestHit);
      dCollideTime := APathPart.StartTime + dEarliestHit;
      dPreCollisionAngle := APathPart.Angle;

      APathPart.EndTime := dCollideTime;

      aNextPathPart := TBasicVector.Create(dXAtCollide, dyAtCollide,
        dVelAtCollide, dPreCollisionAngle, dCollideTime);

      FTrajectories.Add(aNextPathPart);

      APathPart := aNextPathPart;
      LogMessage(APathPart.ToString());
    end;


    case EdgeHit of
      ehNone: LogMessage('No Edge Hit');
      ehLeft:
      begin
        LogMessage('Hit left edge');
        APathPart.ReverseX;
      end;

      ehTop:
      begin
        LogMessage('Hit top edge');
        aNextPathPart.ReverseY();
      end;

      ehRight:
      begin
        LogMessage('Hit right edge');
        aNextPathPart.ReverseX();
      end;

      ehBottom:
      begin
        LogMessage('Hit bottom edge');
        aNextPathPart.ReverseY();
      end;

    end;
    if EdgeHit <> ehNone then
    begin
      LogMessage('End Time: ' + FloatToStr(APathPart.StartTime) +
        ', Angle: ' + FloatToStr(TBasicMotion.RadToDeg(APathPart.Angle)));

    end;

  until EdgeHit = ehNone;
end;

procedure TTrajectoryPath.SetLogger(const intfLogger: IBasicLogger);
begin
  if supports(intfLogger, IBasicLogger) then
    FintfLogger := intfLogger
  else
    FintfLogger := nil;
end;

procedure TTrajectoryPath.LogMessage(const sMessage: String);
begin
  if supports(FintfLogger, IBasicLogger) then
    FintfLogger.LogMessage(sMessage);
end;

function TTrajectoryPath.GetCount: Cardinal;
begin
  RESULT := FTrajectories.Count;
end;

function TTrajectoryPath.GetItems: TList;
begin
  RESULT := FTrajectories;
end;

function TTrajectoryPath.GetItem(const iIndex: Cardinal): TBasicVector;
begin
  RESULT := TBasicVector(FTrajectories[iIndex]);
end;

function TTrajectoryPath.GetXAtTime(const dTime: double): double;
var
  AVector: TBasicVector;
  dTimeInVector: double;
begin
  Result := 0;
  AVector := GetVectorForTime(dTime);
  if (AVector <> nil) then
  begin
    dTimeInVector := dTime - AVector.StartTime;
    Result := AVector.GetXAtTime(dTimeInVector);
  end;
end;

function TTrajectoryPath.GetYAtTime(const dTime: double): double;
var
  AVector: TBasicVector;
  dTimeInVector: double;
begin
  Result := 0;
  AVector := GetVectorForTime(dTime);
  if (AVector <> nil) then
  begin
    dTimeInVector := dTime - AVector.StartTime;
    Result := AVector.GetYAtTime(dTimeInVector);
  end;
end;

constructor TTrajectoryPath.Create;
begin
  FTrajectories := TList.Create;
  FintfLogger := nil;
end;

destructor TTrajectoryPath.Destroy;
begin
  FTrajectories.Free;
  inherited Destroy;
end;

end.
