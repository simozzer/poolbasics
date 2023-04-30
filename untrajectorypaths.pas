unit unTrajectoryPaths;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unCirclePhysics, unHelperInterfaces, unOtherCircles, Matrix;

type
  TEdgeHit = (ehNone, ehLeft, ehTop, ehRight, ehBottom, ehCircle);


  TEdgeHitDetail = record
    EdgeHit: TEdgeHit;
    HitTime: double;
  end;

  TBounceResult = record
    Vector1: Tvector2_double;
    Vector2: Tvector2_double;
  end;


  ITrajectoryPaths = interface
    ['{98B877F0-13F2-4B85-AFFE-4E395428FF99}']
    function GetCount: cardinal;
    function GetItems: TList;
    function getItem(const iIndex: cardinal): TBasicVector;
    function GetCircles: TCirclesList;
    procedure SetCircles(const lstCircles: TCirclesList);
    property Items: TList read GetItems;
    property Item[const iIndex: cardinal]: TBasicVector read GetItem;
    property Count: cardinal read GetCount;
    property OtherCircles: TCirclesList read GetCircles write SetCircles;

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
    FlstCircles: TCirclesList;
  private
    procedure SetLogger(const intfLogger: IBasicLogger);
    procedure LogMessage(const sMessage: string);
    function GetCount: cardinal;
    function GetItems: TList;
    function GetItem(const iIndex: cardinal): TBasicVector;
    function GetCircles: TCirclesList;
    procedure SetCircles(const lstCircles: TCirclesList);

    procedure ProcessEdgeHits(const AVector: TBasicVector; const dRadius: double;
      var dEarliestHitTime: double; var EdgeHit: TEdgeHit);

    function getTimeToHitStationaryCircle(const AVector: TBasicVector;
      const ACircle: TCircle; var dXCircleHit: double; var dYCircleHit: double): double;

    function CalculateBounceAfterHittingCircle(const AVector: TBasicVector;
      const dX, dY: double; const ACircle: TCircle; const dHitTime : DOuble): TBounceResult;

    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetVectorForTime(const dTime: double): TBasicVector;
    procedure CalculateTrajectories;
  public
    constructor Create;
    destructor Destroy; override;
  end;




implementation

uses
  uncirclephysicsconstants, Math;

{ TTrajectoryPath }

function TTrajectoryPath.GetVectorForTime(const dTime: double): TBasicVector;
var
  AVector: TBasicVector;
  i: integer;
  dt: double;
begin
  i := 0;
  dT := dTime;
  if (dT < 0) then dT := 0;
  Result := nil;
  while (Result = nil) and (i < FTrajectories.Count) do
  begin
    AVector := TBasicVector(FTrajectories[i]);
    if (dT >= AVector.StartTime) and (dT <= AVector.EndTime) then
      Result := AVector
    else
      Inc(i);
  end;
  {
  if (Result = nil) and (FTrajectories.Count > 0) then
  begin
    RESULT := TBasicVector(FTrajectories[FTrajectories.Count - 1]);
  end;
  }

end;

procedure TTrajectoryPath.CalculateTrajectories;
var
  dTimeToStop, dEarliestHit, dHitTime: double;
  dXAtCollide, dyAtCollide, dVelAtCollide, dCollideTime, dPreCollisionAngle: double;
  APathVector, aNextPathVector: TBasicVector;
  EdgeHit: TEdgeHit;
  ACircle: TCircle;
  i: integer;

  dXCircleHit, dYCircleHit: double;

  BounceResult : TBounceResult;
begin

  APathVector := TBasicVector(FTrajectories[0]);
  repeat
    EdgeHit := ehNone;

    dTimeToStop := APathVector.GetTimeToStop;
    dEarliestHit := dTimeToStop;


    if (FlstCircles <> nil) then
    begin
      for i := 0 to pred(FlstCircles.Count) do
      begin
        ACircle := FlstCircles[i];
        if ACircle.Stationary then
        begin

          dXCircleHit := 0;
          dYCircleHit := 0;
          dHitTime := getTimeToHitStationaryCircle(APathVector, ACircle,
            dXCircleHit, dYCircleHit);

          if (dHitTime > 0) and (dHitTime < dEarliestHit) then
          begin
            dEarliestHit := dHitTime;
            EdgeHit := ehCircle;

            BounceResult := CalculateBounceAfterHittingCircle(APathVector, dXCircleHit, dYCircleHit,
              ACircle, dHitTime);

            ACircle.Stationary := False; // for now just mark the other circle as moving
          end;
        end;

      end;
    end;



    ProcessEdgeHits(APathVector, PUCK_RADIUS, dEarliestHit, EdgeHit);

    if (EdgeHit <> ehNone) then
    begin
      dXAtCollide := APathVector.GetXAtTime(dEarliestHit);
      dYAtCollide := APathVector.GetYAtTime(dEarliestHit);
      dVelAtCollide := TBasicMotion.GetVelocityAtTime(APathVector.InitialVelocity,
        dEarliestHit);
      dCollideTime := APathVector.StartTime + dEarliestHit;
      dPreCollisionAngle := APathVector.Angle;

      APathVector.EndTime := dCollideTime;

      aNextPathVector := TBasicVector.Create(dXAtCollide, dyAtCollide,
        dVelAtCollide, dPreCollisionAngle, dCollideTime);

      FTrajectories.Add(aNextPathVector);

      APathVector := aNextPathVector;
      LogMessage(APathVector.ToString());
    end;


    case EdgeHit of
      ehNone: LogMessage('No Edge Hit');
      ehLeft:
      begin
        //  LogMessage('Hit left edge');
        APathVector.ReverseX;
      end;

      ehTop:
      begin
        //    LogMessage('Hit top edge');
        aNextPathVector.ReverseY();
      end;

      ehRight:
      begin
        // LogMessage('Hit right edge');
        aNextPathVector.ReverseX();
      end;

      ehBottom:
      begin
        //  LogMessage('Hit bottom edge');
        aNextPathVector.ReverseY();
      end;

      ehCircle:
      begin
        // TODO: Refine
        LogMessage('Hit Circle');
        aNextPathVector.InitialVelocity :=
          Sqrt(Sqr(BounceResult.Vector1.Data[0]) +
          sqr(BounceResult.Vector1.Data[1]));
        aNextPathVector.Angle :=
          ArcTan2(BounceResult.Vector1.Data[1],
          BounceResult.Vector1.Data[0]);
        aNextPathVector.EndTime :=
          aNextPathVector.StartTime + TBasicMotion.GetTimeToStop(
          aNextPathVector.InitialVelocity);
      end;

    end;

    if EdgeHit <> ehNone then
    begin
      LogMessage('End Time: ' + FloatToStr(APathVector.StartTime) +
        ', Angle: ' + FloatToStr(TBasicMotion.RadToDeg(APathVector.Angle)));

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

procedure TTrajectoryPath.LogMessage(const sMessage: string);
begin
  if supports(FintfLogger, IBasicLogger) then
    FintfLogger.LogMessage(sMessage);
end;

function TTrajectoryPath.GetCount: cardinal;
begin
  Result := FTrajectories.Count;
end;

function TTrajectoryPath.GetItems: TList;
begin
  Result := FTrajectories;
end;

function TTrajectoryPath.GetItem(const iIndex: cardinal): TBasicVector;
begin
  Result := TBasicVector(FTrajectories[iIndex]);
end;

function TTrajectoryPath.GetCircles: TCirclesList;
begin
  Result := FlstCircles;
end;

procedure TTrajectoryPath.SetCircles(const lstCircles: TCirclesList);
begin
  FlstCircles := lstCircles;
end;

procedure TTrajectoryPath.ProcessEdgeHits(const AVector: TBasicVector;
  const dRadius: double; var dEarliestHitTime: double; var EdgeHit: TEdgeHit);
var
  dMaxDisplacmentXAtStop, dMaxDisplacmentYAtStop, dDeplacement, dHitTime: double;
begin
  dMaxDisplacmentXAtStop := AVector.GetDisplacementXAtStop;
  dMaxDisplacmentYAtStop := AVector.GetDisplacementYAtStop;

  if (dMaxDisplacmentXAtStop <= dRadius) then
  begin
    dDeplacement := AVector.OriginX - dRadius;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) and (dHitTime > 0) then
    begin
      EdgeHit := ehLeft;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentYAtStop <= dRadius) then
  begin
    dDeplacement := AVector.OriginY - dRadius;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) and (dHitTime > 0) then
    begin
      EdgeHit := ehTop;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentXAtStop >= (BOARD_WIDTH - dRadius)) then
  begin
    dDeplacement := BOARD_WIDTH - dRadius - AVector.OriginX;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) and (dHitTime > 0) then
    begin
      EdgeHit := ehRight;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentYAtStop >= (BOARD_HEIGHT - dRadius)) then
  begin
    dDeplacement := BOARD_HEIGHT - dRadius - AVector.OriginY;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) then
    begin
      EdgeHit := ehBottom;
      dEarliestHitTime := dHitTime;
    end;
  end;
end;

function TTrajectoryPath.getTimeToHitStationaryCircle(const AVector: TBasicVector;
  const ACircle: TCircle; var dXCircleHit: double; var dYCircleHit: double): double;
var
  dDistanceBetween2Centers, dSumRadii, dDistanceBewteen2Circles,
  dDotProduct_D, dyDistanceToColissionSquared_F, dXDiffereneAtCollision_T,
  dHitTime, dXDistanceToCollision_distance, dActualDistanceToCollision: double;
  AThisVector, AVectorBetween2Centers: T2DVector;
  NormalizedVector_N: T2DVector;
begin
  Result := -1;

  // check if we travel far enough to hit circle
  dDistanceBetween2Centers := ACircle.Distance(AVector.OriginX, AVector.OriginY);
  dSumRadii := PUCK_RADIUS + ACircle.Radius;
  dDistanceBewteen2Circles := dDistanceBetween2Centers - dSumRadii;
  if (dDistanceBewteen2Circles < TBasicMotion.GetDistanceToStop(
    AVector.InitialVelocity)) then
  begin
    AThisVector := T2DVector.CreateWithAngle(dDistanceBewteen2Circles, AVector.Angle);
    try
      // Get the normalized vector for this ball
      NormalizedVector_N := AThisVector.GetNormalised;
      try
        // Get the vector between the 2 ball centers
        AVectorBetween2Centers :=
          T2DVector.Create(ACircle.CenterX - AVector.OriginX,
          ACircle.CenterY - AVector.OriginY);
        try

          //dDotProduct := AVectorBetween2Centers.Magnitude * cos(-AVectorBetween2Centers.Angle);
          dDotProduct_D := AVectorBetween2Centers.GetDotProduct(NormalizedVector_N);

          // check we're moving towards the target
          if (dDotProduct_D > 0) then
          begin
            // Check that we get close enough for collision
            // double F = (lengthC * lengthC) - (D * D);
            dyDistanceToColissionSquared_F :=
              Sqr(AVectorBetween2Centers.Magnitude) - sqr(dDotProduct_D);
            if (dyDistanceToColissionSquared_F < SQr(dSumRadii)) then
            begin
              // find the distance
              //double T = sumRadiiSquared - F;
              dXDiffereneAtCollision_T :=
                Sqr(dSumRadii) - dyDistanceToColissionSquared_F;
              if (dXDiffereneAtCollision_T >= 0) then
              begin
                //doubleble distance = D - sqrt(T);
                dXDistanceToCollision_distance :=
                  dDotProduct_D - Sqrt(dXDiffereneAtCollision_T);
                // check distance to travel is enough for possible collision
                if TBasicMotion.GetDistanceToStop(AVector.InitialVelocity) >=
                  dXDistanceToCollision_distance then
                begin
                  // Set the length so that the circles will just touch.
                  dXCircleHit :=
                    NormalizedVector_N.Vector.Data[0] *
                    dXDistanceToCollision_distance;
                  dYCircleHit :=
                    NormalizedVector_N.Vector.Data[1] *
                    dXDistanceToCollision_distance;

                  // Calculate the time at which the collision occurred
                  dActualDistanceToCollision :=
                    Sqrt(Sqr(dXCircleHit) + Sqr(dYCircleHit));
                  dHitTime :=
                    TBasicMotion.GetTimeToDistance(
                    AVector.InitialVelocity, dActualDistanceToCollision);

                  Result := dHitTime;
                end;
              end;
            end;
          end;

        finally
          AVectorBetween2Centers.Free;
        end;

      finally
        NormalizedVector_N.Free;
      end;

    finally
      AThisVector.Free;
    end;
  end;
end;

function TTrajectoryPath.CalculateBounceAfterHittingCircle(const AVector: TBasicVector;
  const dX, dY: double; const ACircle: TCircle; const dHitTime : Double): TBounceResult;
var
  aNormalisedVectorBetweenCentersAtCollision, vecOtherCircleBeforeCollision,
  vecThisMovement, aCalcVec: Tvector2_double;
  dThisLengthOfMoveVectorAlongCollisionNormal_a1,
  dOtherLengthOfMoveVectorAlongCollisionNormal_a2, dOptimisedP: double;

begin
  // Get normalised data between 2 centers
  aNormalisedVectorBetweenCentersAtCollision.init(
    ACircle.CenterX - dX, ACircle.CenterY - dY);
  aNormalisedVectorBetweenCentersAtCollision.init(
    aNormalisedVectorBetweenCentersAtCollision.Data[0] /
    aNormalisedVectorBetweenCentersAtCollision.length,
    aNormalisedVectorBetweenCentersAtCollision.Data[1] /
    aNormalisedVectorBetweenCentersAtCollision.length);

  vecOtherCircleBeforeCollision.init_zero;

  // find length of move against normalised vector

  vecThisMovement :=
    AVector.GetVelocityVectorAtTime(dHitTime);

  // get dot product of movement vector and normalised collision vector
  dThisLengthOfMoveVectorAlongCollisionNormal_a1 :=
    vecThisMovement.Data[0] * aNormalisedVectorBetweenCentersAtCollision.Data[0] +
    vecThisMovement.Data[1] * aNormalisedVectorBetweenCentersAtCollision.Data[1];

  dOtherLengthOfMoveVectorAlongCollisionNormal_a2 :=
    0.0; // TODO

  //  (2.0 * (a1 - a2)) / (circle1.mass + circle2.mass);
  dOptimisedP :=
    (2.0 * (dThisLengthOfMoveVectorAlongCollisionNormal_a1 -
    dOtherLengthOfMoveVectorAlongCollisionNormal_a2)) / (1 + 1);

  // Calculate v1', the new movement vector of circle1
  //                              Vector v1' = v1 - optimizedP * circle2.mass * n;
  aCalcVec.init(dOptimisedP * aNormalisedVectorBetweenCentersAtCollision.Data[0],
    dOptimisedP * aNormalisedVectorBetweenCentersAtCollision.Data[1]);

  RESULT.Vector1 := AVector.GetVelocityVectorAtTime(dHitTime) - aCalcVec;
    Format('This vel after collision = XVEL: %f, YVEL: %f',
    [RESULT.Vector1.Data[0], RESULT.Vector1.Data[1]]);
  // Calculate v2', the new movement vector of circle2
  // v2' = v2 + optimizedP * m1 * n
  RESULT.Vector2 :=
    vecOtherCircleBeforeCollision - aCalcVec;
  LogMessage(
    Format('Other vel after collision = XVEL: %f, YVEL: %f',
    [RESULT.Vector2.Data[0], RESULT.Vector2.Data[1]]));

  // todo.. move check into later stage (May have hit another circle first)
  ACircle.Stationary := False;
end;


function TTrajectoryPath.GetXAtTime(const dTime: double): double;
var
  AVector: TBasicVector;
  dTimeInVector: double;
  dT: double;
begin
  Result := 0;
  dt := dTime;
  if (dT < 0) then dt := 0;
  AVector := GetVectorForTime(dt);
  if (AVector <> nil) then
  begin
    dTimeInVector := dT - AVector.StartTime;
    Result := AVector.GetXAtTime(dTimeInVector);
  end
  else
  begin
    LogMessage(Format('GetXatTime vector not found %f', [dTime]));
  end;
end;

function TTrajectoryPath.GetYAtTime(const dTime: double): double;
var
  AVector: TBasicVector;
  dTimeInVector: double;
  dT: double;
begin
  Result := 0;
  dt := dTime;
  if (dT < 0) then dt := 0;
  AVector := GetVectorForTime(dTime);
  if (AVector <> nil) then
  begin
    dTimeInVector := dT - AVector.StartTime;
    Result := AVector.GetYAtTime(dTimeInVector);
  end
  else
  begin
    LogMessage(Format('GetYatTime vector not found %f', [dTime]));
  end;
end;

constructor TTrajectoryPath.Create;
begin
  FTrajectories := TList.Create;
  FlstCircles := nil;
  FintfLogger := nil;
end;

destructor TTrajectoryPath.Destroy;
begin
  FTrajectories.Free;
  inherited Destroy;
end;

end.
