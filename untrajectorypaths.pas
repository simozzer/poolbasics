unit unTrajectoryPaths;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unCirclePhysics, unHelperInterfaces, unOtherCircles;

type
  TEdgeHit = (ehNone, ehLeft, ehTop, ehRight, ehBottom, ehCircle);


  TEdgeHitDetail = record
    EdgeHit: TEdgeHit;
    HitTime: double;
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

    procedure ProcessEdgeHits(const AVector: TBasicVector;
      var dEarliestHitTime: double; var EdgeHit: TEdgeHit);
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
  uncirclephysicsconstants, Matrix, Math;

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
  begin
    AVector := TBasicVector(FTrajectories[FTrajectories.Count - 1]);
  end;

end;

procedure TTrajectoryPath.CalculateTrajectories;
var
  dTimeToStop,
  dEarliestHit, dHitTime: double;
  dXAtCollide, dyAtCollide, dVelAtCollide, dCollideTime, dPreCollisionAngle: double;
  APathVector, aNextPathVector: TBasicVector;
  EdgeHit: TEdgeHit;
  ACircle: TCircle;
  i: integer;

  dDistance_dist, dSumRadii, dDotProduct_D, dXDistanceToCollision_distance,
  dXDiffereneAtCollision_T, dyDistanceToColissionSquared_F: double;
  AThisVector, NormalizedVector_N, AMinusVector_C: T2DVector;

  dXCircleHit, dyCirlceHIt, dActualDistanceToCollision: double;
  vecOtherCircleBeforeCollision: Tvector2_double;
  aNormalisedVectorBetweenCentersAtCollision, aMainVecAfterCircleCollision,
  aOtherVecAfterCircleCollision, aCalcVec: Tvector2_double;

  dThisLengthOfMoveVectorAlongCollisionNormal_a1,
  dOtherLengthOfMoveVectorAlongCollisionNormal_a2: double;
  dOptimisedP: double;

  vecThisMovement: Tvector2_double;
begin

  APathVector := TBasicVector(FTrajectories[0]);
  repeat
    EdgeHit := ehNone;

    dTimeToStop := APathVector.GetTimeToStop;
    dEarliestHit := dTimeToStop;

    aMainVecAfterCircleCollision.init_zero;
    aOtherVecAfterCircleCollision.init_zero;


    if (FlstCircles <> nil) then
    begin
      for i := 0 to pred(FlstCircles.Count) do
      begin
        ACircle := FlstCircles[i];
        if ACircle.Stationary then
        begin

          // check if we travel far enough to hit circle
          dDistance_dist := ACircle.Distance(APathVector.OriginX, APathVector.OriginY);
          dSumRadii := BALL_RADIUS + ACircle.Radius;
          dDistance_dist := dDistance_dist - dSumRadii;
          if (dDistance_dist < TBasicMotion.GetDistanceToStop(
            APathVector.InitialVelocity)) then
          begin
            AThisVector := T2DVector.CreateWithAngle(dDistance_dist, APathVector.Angle);
            try
              // Get the normalized vector for this ball
              NormalizedVector_N := AThisVector.GetNormalised;
              try
                // Get the vector between the 2 ball centers
                AMinusVector_C :=
                  T2DVector.Create(ACircle.CenterX - APathVector.OriginX,
                  ACircle.CenterY - APathVector.OriginY);
                try

                  //dDotProduct := AMinusVector_C.Magnitude * cos(-AMinusVector_C.Angle);
                  dDotProduct_D := AMinusVector_C.GetDotProduct(NormalizedVector_N);
                  // alternative dotProduct = ax × bx + ay × by

                  //LogMessage(format('Dot product: %f', [dDotProduct_D]));

                  // check we're moving towards the target
                  if (dDotProduct_D > 0) then
                  begin
                    // Check that we get close enough for collision
                    // double F = (lengthC * lengthC) - (D * D);
                    dyDistanceToColissionSquared_F :=
                      Sqr(AMinusVector_C.Magnitude) - sqr(dDotProduct_D);
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
                        if TBasicMotion.GetDistanceToStop(APathVector.InitialVelocity) >=
                          dXDistanceToCollision_distance then
                        begin
                          // Set the length so that the circles will just touch.
                          dXCircleHit :=
                            NormalizedVector_N.Vector.Data[0] *
                            dXDistanceToCollision_distance;
                          dyCirlceHIt :=
                            NormalizedVector_N.Vector.Data[1] *
                            dXDistanceToCollision_distance;
                          dActualDistanceToCollision :=
                            Sqrt(Sqr(dXCircleHit) + Sqr(dyCirlceHIt));
                          dHitTime :=
                            TBasicMotion.GetTimeToDistance(
                            APathVector.InitialVelocity, dActualDistanceToCollision);
                          if (dHitTime < dEarliestHit) then
                          begin
                            dEarliestHit := dHitTime;
                            EdgeHit := ehCircle;
                            // TODO... add circle collision data


                            // Get normalised data between 2 centers
                            aNormalisedVectorBetweenCentersAtCollision.init(
                              ACircle.CenterX - dXCircleHit, ACircle.CenterY -
                              dyCirlceHIt);
                            aNormalisedVectorBetweenCentersAtCollision.init(
                              aNormalisedVectorBetweenCentersAtCollision.Data[0] /
                              aNormalisedVectorBetweenCentersAtCollision.length,
                              aNormalisedVectorBetweenCentersAtCollision.Data[1] /
                              aNormalisedVectorBetweenCentersAtCollision.length);

                            vecOtherCircleBeforeCollision.init_zero;

                            // find length of move against normalised vector

                            vecThisMovement :=
                              APathVector.GetVelocityVectorAtTime(dHitTime);

                            // get dot product of movement vector and normalised collision vector
                            dThisLengthOfMoveVectorAlongCollisionNormal_a1 :=
                              vecThisMovement.Data[0] *
                              aNormalisedVectorBetweenCentersAtCollision.Data[0] +
                              vecThisMovement.Data[1] *
                              aNormalisedVectorBetweenCentersAtCollision.Data[1];

                            dOtherLengthOfMoveVectorAlongCollisionNormal_a2 :=
                              0.0; // TODO

                            //                              (2.0 * (a1 - a2)) / (circle1.mass + circle2.mass);
                            dOptimisedP :=
                              (2.0 *
                              (dThisLengthOfMoveVectorAlongCollisionNormal_a1 -
                              dOtherLengthOfMoveVectorAlongCollisionNormal_a2)) /
                              (1 + 1);

                            // Calculate v1', the new movement vector of circle1
                            //                              Vector v1' = v1 - optimizedP * circle2.mass * n;
                            aCalcVec.init(dOptimisedP *
                              aNormalisedVectorBetweenCentersAtCollision.Data[0],
                              dOptimisedP *
                              aNormalisedVectorBetweenCentersAtCollision.Data[1]);
                            aMainVecAfterCircleCollision :=
                              (APathVector.GetVelocityVectorAtTime(dHitTime) - aCalcVec);
                            LogMessage(
                              Format('This vel after collision = XVEL: %f, YVEL: %f',
                              [aMainVecAfterCircleCollision.Data[0],
                              aMainVecAfterCircleCollision.Data[1]]));
                            // Calculate v2', the new movement vector of circle2
                            // v2' = v2 + optimizedP * m1 * n
                            aOtherVecAfterCircleCollision :=
                              vecOtherCircleBeforeCollision - aCalcVec;
                            LogMessage(
                              Format('Other vel after collision = XVEL: %f, YVEL: %f',
                              [aOtherVecAfterCircleCollision.Data[0],
                              aOtherVecAfterCircleCollision.Data[1]]));

                            // todo.. move check into later stage (May have hit another circle first)
                            ACircle.Stationary:=false;

                          end;

                        end;
                      end;
                    end;
                  end;
                finally
                  AMinusVector_C.Free;
                end;
              finally
                NormalizedVector_N.Free;
              end;
            finally
              AThisVector.Free;
            end;
          end;
        end;

      end;
    end;



    ProcessEdgeHits(APathVector, dEarliestHit, EdgeHit);

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
        LogMessage('Hit left edge');
        APathVector.ReverseX;
      end;

      ehTop:
      begin
        LogMessage('Hit top edge');
        aNextPathVector.ReverseY();
      end;

      ehRight:
      begin
        LogMessage('Hit right edge');
        aNextPathVector.ReverseX();
      end;

      ehBottom:
      begin
        LogMessage('Hit bottom edge');
        aNextPathVector.ReverseY();
      end;

      ehCircle:
      begin
        // TODO: Refine
        LogMessage('Hit Circle');
        aNextPathVector.InitialVelocity :=
          Sqrt(Sqr(aMainVecAfterCircleCollision.Data[0]) +
          sqr(aMainVecAfterCircleCollision.Data[1]));
        aNextPathVector.Angle :=
          ArcTan2(aMainVecAfterCircleCollision.Data[1],
          aMainVecAfterCircleCollision.Data[0]);
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
  var dEarliestHitTime: double; var EdgeHit: TEdgeHit);
var
  dMaxDisplacmentXAtStop, dMaxDisplacmentYAtStop, dDeplacement, dHitTime: double;
begin
  dMaxDisplacmentXAtStop := AVector.GetDisplacementXAtStop;
  dMaxDisplacmentYAtStop := AVector.GetDisplacementYAtStop;

  if (dMaxDisplacmentXAtStop <= BALL_RADIUS) then
  begin
    dDeplacement := AVector.OriginX - BALL_RADIUS;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) and (dHitTime > 0) then
    begin
      EdgeHit := ehLeft;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentYAtStop <= BALL_RADIUS) then
  begin
    dDeplacement := AVector.OriginY - BALL_RADIUS;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) and (dHitTime > 0) then
    begin
      EdgeHit := ehTop;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentXAtStop >= (BOARD_WIDTH - BALL_RADIUS)) then
  begin
    dDeplacement := BOARD_WIDTH - BALL_RADIUS - AVector.OriginX;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) and (dHitTime > 0) then
    begin
      EdgeHit := ehRight;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dMaxDisplacmentYAtStop >= (BOARD_HEIGHT - BALL_RADIUS)) then
  begin
    dDeplacement := BOARD_HEIGHT - BALL_RADIUS - AVector.OriginY;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if (dHitTime < dEarliestHitTime) then
    begin
      EdgeHit := ehBottom;
      dEarliestHitTime := dHitTime;
    end;
  end;
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
  FlstCircles := nil;
  FintfLogger := nil;
end;

destructor TTrajectoryPath.Destroy;
begin
  FTrajectories.Free;
  inherited Destroy;
end;

end.
