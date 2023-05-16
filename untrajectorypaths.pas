unit unTrajectoryPaths;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unCirclePhysics, unHelperInterfaces, unOtherCircles,
  Matrix, uncollisiontypes, unCollisionDetection;

type


  { TTrajectoryPath }

  TTrajectoryPath = class(TInterfacedObject, ITrajectoryPaths, IBasicLoggerClient)
  private
    FintfLogger: IBasicLogger;
    FTrajectories: TInterfaceList;
    FlstCircles: ICirclesList;

    procedure SetLogger(const intfLogger: IBasicLogger);
    procedure LogMessage(const sMessage: string);
  public
    function GetCount: cardinal;
    function GetItems: TInterfaceList;
    function GetItem(const iIndex: cardinal): IBasicVector;
    function GetCircles: ICirclesList;
    procedure SetCircles(const lstCircles: ICirclesList);

    function getTimeToHitStationaryCircle(const AVector: IBasicVector;
      const ATargetCircle: ICircle; var dXCircleHit: double; var dYCircleHit: double): double;

    function CalculateBounceAfterHittingCircle(const AVector: IBasicVector;
      const dX, dY: double; const ATargetCircle: ICircle;
      const dHitTime: double): TBounceResult;

    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetVectorForTime(const dTime: double): IBasicVector;
    procedure CalculateTrajectories;
    constructor Create;
    destructor Destroy; override;
    property Count: cardinal read GetCount;
    property Items: TInterfaceList read GetItems;
  end;




implementation

uses
  uncirclephysicsconstants, Math, Types;

{ TTrajectoryPath }

function TTrajectoryPath.GetVectorForTime(const dTime: double): IBasicVector;
var
  AVector: IBasicVector;
  i: integer;
  dt: double;
begin
  i := 0;
  dT := dTime;
  if (dT < 0) then dT := 0;
  Result := nil;
  while (Result = nil) and (i < FTrajectories.Count) do
  begin
    AVector := IBasicVector(FTrajectories[i]);
    if (dT >= AVector.StartTime) and (dT <= AVector.EndTime) then
      Result := AVector
    else
      Inc(i);
  end;

end;

procedure TTrajectoryPath.CalculateTrajectories;
var
  dTimeToStop, dEarliestHit, dHitTime: double;
  dVelAtCollide, dCollideTime, dPreCollisionAngle: double;
  APathVector, aNextPathVector: IBasicVector;
  EdgeHit: TEdgeHit;
  ACircle: ICircle;
  i: integer;

  ptCollision: TPointF;
  dXCircleHit, dYCircleHit: double;

  BounceResult: TBounceResult;
begin

  ptCollision.X := 0;
  ptCollision.Y := 0;
  APathVector := IBasicVector(FTrajectories[0]);
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

            BounceResult := CalculateBounceAfterHittingCircle(APathVector,
              dXCircleHit + APathVector.Origin.X, dYCircleHit +
              APathVector.Origin.Y, ACircle, dHitTime);

            ACircle.Stationary := False; // for now just mark the other circle as moving
          end;
        end;

      end;
    end;



    TCollisionDetection.DetectEdgeHits(APathVector, PUCK_RADIUS, dEarliestHit, EdgeHit);

    if (EdgeHit <> ehNone) then
    begin
      ptCollision.X := APathVector.GetXAtTime(dEarliestHit);
      ptCollision.Y := APathVector.GetYAtTime(dEarliestHit);
      dVelAtCollide := TBasicMotion.GetVelocityAtTime(APathVector.InitialVelocity,
        dEarliestHit);
      dCollideTime := APathVector.StartTime + dEarliestHit;
      dPreCollisionAngle := APathVector.Angle;

      APathVector.EndTime := dCollideTime;

      aNextPathVector := TBasicVector.Create(ptCollision,
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
          ArcTan2(BounceResult.Vector1.Data[1], BounceResult.Vector1.Data[0]);
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

function TTrajectoryPath.GetItems: TInterfaceList;
begin
  Result := FTrajectories;
end;

function TTrajectoryPath.GetItem(const iIndex: cardinal): IBasicVector;
begin
  Result := IBasicVector(FTrajectories[iIndex]);
end;

function TTrajectoryPath.GetCircles: ICirclesList;
begin
  Result := FlstCircles;
end;

procedure TTrajectoryPath.SetCircles(const lstCircles: ICirclesList);
begin
  FlstCircles := lstCircles;
end;

function TTrajectoryPath.getTimeToHitStationaryCircle(const AVector: IBasicVector;
  const ATargetCircle: ICircle; var dXCircleHit: double; var dYCircleHit: double): double;
var
  dDistanceBetween2Centers, dSumRadii, dDistanceBewteen2Circles,
  dDotProduct_D, dyDistanceToColissionSquared_F, dXDiffereneAtCollision_T,
  dHitTime, dXDistanceToCollision_distance, dActualDistanceToCollision: double;
  AThisVector, AVectorBetween2Centers: I2DVector;
  NormalizedVector_N: I2DVector;

  intfTargetCircleVector : IBasicVector;
  intfVectorAccess: IObjectWithVector;
begin
  Result := -1;

  if not supports(ATargetCircle,IObjectWithVector,intfVectorAccess) then
    raise Exception.Create('Could not obtain IObjectWithVector')
  else
    intfTargetCircleVector := intfVectorAccess.GetBasicVector;

  // check if we travel far enough to hit circle
  dDistanceBetween2Centers := intfTargetCircleVector.Origin.Distance(AVector.Origin);
  dSumRadii := PUCK_RADIUS + ATargetCircle.Radius;
  dDistanceBewteen2Circles := dDistanceBetween2Centers - dSumRadii;
  if (dDistanceBewteen2Circles < TBasicMotion.GetDistanceToStop(
    AVector.InitialVelocity)) then
  begin
    AThisVector := T2DVector.CreateWithAngle(dDistanceBewteen2Circles, AVector.Angle);

    // Get the normalized vector for this ball
    NormalizedVector_N := AThisVector.GetNormalised;

    // Get the vector between the 2 ball centers
    AVectorBetween2Centers :=
      T2DVector.Create(intfTargetCircleVector.Origin.X - AVector.Origin.X,
      intfTargetCircleVector.Origin.Y - AVector.Origin.Y);


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
  end;
end;

function TTrajectoryPath.CalculateBounceAfterHittingCircle(
  const AVector: IBasicVector; const dX, dY: double; const ATargetCircle: ICircle;
  const dHitTime: double): TBounceResult;
var
  deltaX, deltaY, dAngle, dSin, dCos, dPuckAngle, dAngleDifference: double;
  vx1, vy1, vx2, vy2: double;
  vecVel: Tvector2_double;

  vx1Final, vx2Final, vy1Final, vy2Final: double;
  vx1Rotated, vx2Rotated, vy1Rotated, vy2Rotated: double;
  circleVecVel: Tvector2_double;

  intfVectorAccess : IObjectWithVector;
  intfTargetCircleVector : IBasicVector;
begin
  if not supports(ATargetCircle,IObjectWithVector,intfVectorAccess) then
    raise Exception.Create('Could not obtain IObjectWithVector')
  else
    intfTargetCircleVector := intfVectorAccess.GetBasicVector;

  // calculate distance between 2 circles
  deltaX := Dx - intfTargetCircleVector.Origin.X;
  deltaY := Dy - intfTargetCircleVector.Origin.Y;

  // Calculate collision angle
  dAngle := arctan2(deltaY, deltaX);

  // calculate the angle of the puck

  dPuckAngle := AVector.Angle;
  dAngleDifference := dPuckAngle - dAngle;
  LogMessage(Format('Difference in collision angle %f', [dAngleDifference]));

  dSin := Sin(dAngle);
  dCos := Cos(dAngle);

  vecVel := AVector.GetVelocityVectorAtTime(dHitTime);
  circleVecVel.init_zero; // TODO change for moving circle

  // Rotate the velocities so that we can calculate the new velocities
  vx1 := vecVel.Data[0] * dCos + vecVel.Data[1] * dSin;
  vy1 := vecVel.Data[1] * dCos - vecVel.Data[0] * dSin;
  vx2 := circleVecVel.Data[0] * dCos + circleVecVel.Data[1] * dSin;
  vy2 := circleVecVel.Data[1] * dCos - circleVecVel.Data[0] * dSin;

  // Calculate the new velocities after the collision
  vx1Final := ((PUCK_RADIUS - ATargetCircle.Radius) * vx1 + (2 * ATargetCircle.Radius) * vx2) /
    (PUCK_RADIUS + ATargetCircle.Radius);
  vx2Final := ((ATargetCircle.Radius - PUCK_RADIUS) * vx2 + (2 * PUCK_RADIUS) * vx1) /
    (PUCK_RADIUS + ATargetCircle.Radius);
  vy1Final := vy1 - DECELERATION;
  vy2Final := vy2;// - DECELERATION;

  // Rotate the velocities back again
  vx1Rotated := vx1Final * dCos - vy1Final * dSin;
  vy1Rotated := vy1Final * dCos + vx1Final * dSin;
  vx2Rotated := vx2Final * dCos - vy2Final * dSin;
  vy2Rotated := vy2Final * dCos + vx2Final * dSin;

  Result.Vector1.init(vx1Rotated, vy1Rotated);
  Format('This vel after collision = XVEL: %f, YVEL: %f',
    [Result.Vector1.Data[0], Result.Vector1.Data[1]]);
  // Calculate v2', the new movement vector of circle2
  // v2' = v2 + optimizedP * m1 * n
  Result.Vector2.init(vx2Rotated, vy2Rotated);
  LogMessage(
    Format('Other vel after collision = XVEL: %f, YVEL: %f',
    [Result.Vector2.Data[0], Result.Vector2.Data[1]]));

  // todo.. move check into later stage (May have hit another circle first)
  ATargetCircle.Stationary := False;
end;


function TTrajectoryPath.GetXAtTime(const dTime: double): double;
var
  AVector: IBasicVector;
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
  AVector: IBasicVector;
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
  FTrajectories := TInterfaceList.Create;
  FlstCircles := nil;
  FintfLogger := nil;
end;

destructor TTrajectoryPath.Destroy;
begin
  FTrajectories.Free;
  inherited Destroy;
end;

end.
