unit unCollisionDetection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uncollisiontypes, unHelperInterfaces;

type
  { TCollisionDetection }

  { TCircleCollisionResult }

  TCircleCollisionResult = class(TInterfacedObject, ICircleCollisionResult)
  private
    FCircleId1, FCircleId2: integer;
    FdCircle1XAtHit: double;
    FdCircle1YAtHit: double;
    FdCircle2XAtHit: double;
    FdCircle2YAtHit: double;
    FdHitTime: double;
    function GetCircleId1: integer;
    function GetCircleId2: integer;
    function GetHitTime: double;
    function GetCircle1XAtHit: double;
    function GetCircle1YAtHit: double;
    function GetCircle2XAtHit: double;
    function GetCircle2YAtHit: double;
  public
    constructor Create(const iCircleID1, iCircleID2: integer;
      const dHitTime: double; const dX1AtHit, dY1AtHit, dX2AtHit, dY2AtHit: double);

  end;

  TCollisionDetection = class
  public
    class procedure DetectEdgeHits(const APathPart: IPathPart;
      var dEarliestHitTime: double; var EdgeHit: TEdgeHit);

    class function DetectStationaryCircleHit(const APathPart1: IPathPart;
      const APathPart2: IPathPart): ICircleCollisionResult;

    class function CalculateBounceAfterHittingCircle(const APathPart: IPathPart;
      const dX, dY: double; const ATargetPathPart: IPathPart;
      const dHitTime: double): TBounceResult;
  end;

{ TCircleCollisionResult }



implementation

uses
  uncirclephysicsconstants, unCirclePhysics, Matrix, unCircleUtils;

{ TCircleCollisionResult }

function TCircleCollisionResult.GetCircleId1: integer;
begin
  Result := FCircleId1;
end;

function TCircleCollisionResult.GetCircleId2: integer;
begin
  Result := FCircleId2;
end;

function TCircleCollisionResult.GetHitTime: double;
begin
  Result := FdHitTime;
end;

function TCircleCollisionResult.GetCircle1XAtHit: double;
begin
  Result := FdCircle1XAtHit;
end;

function TCircleCollisionResult.GetCircle1YAtHit: double;
begin
  Result := FdCircle1YAtHit;
end;

function TCircleCollisionResult.GetCircle2XAtHit: double;
begin
  Result := FdCircle2XAtHit;
end;

function TCircleCollisionResult.GetCircle2YAtHit: double;
begin
  Result := FdCircle2YAtHit;
end;

constructor TCircleCollisionResult.Create(const iCircleId1, iCircleId2: integer;
  const dHitTime: double; const dX1AtHit, dY1AtHit, dX2AtHit, dY2AtHit: double);
begin
  FCircleId1 := iCircleId1;
  FCircleId2 := iCircleId2;
  FdHitTime := dHitTime;
  FdCircle1XAtHit := dX1AtHit;
  FdCircle1YAtHit := dY1AtHit;
  FdCircle2XAtHit := dX2AtHit;
  FdCircle2YAtHit := dY2AtHit;
end;


{ TCollisionDetection }

class procedure TCollisionDetection.DetectEdgeHits(const APathPart: IPathPart;
  var dEarliestHitTime: double; var EdgeHit: TEdgeHit);
var
  dXAtStop, dYAtStop, dDeplacement, dHitTime: double;
  AVector: IBasicVector;
  dRadius: double;
begin
  AVector := APathPart.Vector;
  dRadius := APathPart.Circle.Radius;
  dXAtStop := AVector.GetXAtStop;
  dYAtStop := AVector.GetYAtStop;



  if (dXAtStop <= dRadius) then
  begin
    dDeplacement := AVector.Origin.X - dRadius;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehLeft;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dYAtStop <= dRadius) then
  begin
    dDeplacement := AVector.Origin.Y - dRadius;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehTop;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dXAtStop >= (BOARD_WIDTH - dRadius)) then
  begin
    dDeplacement := BOARD_WIDTH - dRadius - AVector.Origin.X;
    dHitTime := AVector.GetTimeToXDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehRight;
      dEarliestHitTime := dHitTime;
    end;
  end;

  if (dYAtStop >= (BOARD_HEIGHT - dRadius)) then
  begin
    dDeplacement := BOARD_HEIGHT - dRadius - AVector.Origin.Y;
    dHitTime := AVector.GetTimeToYDeplacement(dDeplacement);
    if ((dEarliestHitTime < 0) or (dHitTime < dEarliestHitTime)) and (dHitTime > 0) then
    begin
      EdgeHit := ehBottom;
      dEarliestHitTime := dHitTime;
    end;
  end;
end;

class function TCollisionDetection.DetectStationaryCircleHit(
  const APathPart1: IPathPart; const APathPart2: IPathPart): ICircleCollisionResult;
var
  dDistanceBetween2Centers, dSumRadii, dDistanceBewteen2Circles,
  dDotProduct_D, dyDistanceToColissionSquared_F, dXDiffereneAtCollision_T,
  dHitTime, dXDistanceToCollision_distance, dActualDistanceToCollision: double;
  ACircle1Vector, ACircle2Vector: IBasicVector;
  AThisVector, AVectorBetween2Centers: I2DVector;
  NormalizedVector_N: I2DVector;
  dXCircleHit, dYCircleHit: double;
  ACircle1, aCircle2: ICircle;
  iCircleId1, iCircleId2: integer;
begin
  Result := nil;

  ACircle1 := APathPart1.Circle;
  aCircle2 := APathPart2.Circle;
  ACircle1Vector := APathPart1.Vector;
  ACircle2Vector := APathPart2.Vector;

  iCircleId1 := TCircleUtils.GetCircleId(ACircle1);
  iCircleId2 := TCircleUtils.GetCircleId(ACircle2);


  // check if we travel far enough to hit circle
  dDistanceBetween2Centers := ACircle2Vector.Origin.Distance(ACircle1Vector.Origin);
  dSumRadii := ACircle1.Radius + ACircle2.Radius;
  dDistanceBewteen2Circles := dDistanceBetween2Centers - dSumRadii;
  if (dDistanceBewteen2Circles < TBasicMotion.GetDistanceToStop(
    ACircle1Vector.InitialVelocity)) then
  begin
    AThisVector := T2DVector.CreateWithAngle(dDistanceBewteen2Circles,
      ACircle1Vector.Angle);

    // Get the normalized vector for this ball
    NormalizedVector_N := AThisVector.GetNormalised;

    // Get the vector between the 2 ball centers
    AVectorBetween2Centers :=
      T2DVector.Create(ACircle2Vector.Origin.X - ACircle1Vector.Origin.X,
      ACircle2Vector.Origin.Y - ACircle1Vector.Origin.Y);

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
          if TBasicMotion.GetDistanceToStop(ACircle1Vector.InitialVelocity) >=
            dXDistanceToCollision_distance then
          begin
            // Set the length so that the circles will just touch.
            dXCircleHit :=
              (NormalizedVector_N.Vector.Data[0] *
              dXDistanceToCollision_distance);
            dYCircleHit :=
              (NormalizedVector_N.Vector.Data[1] *
              dXDistanceToCollision_distance);

            // Calculate the time at which the collision occurred
            dActualDistanceToCollision :=
              Sqrt(Sqr(dXCircleHit) + Sqr(dYCircleHit));
            dHitTime :=
              TBasicMotion.GetTimeToDistance(
              ACircle1Vector.InitialVelocity, dActualDistanceToCollision);

            Result := TCircleCollisionResult.Create(iCircleId1, iCircleId2,
              dHitTime, dXCircleHit + ACircle1Vector.Origin.X,
              dYCircleHit + ACircle1Vector.Origin.Y, ACircle2Vector.Origin.X,
              ACircle2Vector.Origin.Y);
          end;
        end;
      end;
    end;
  end;
end;

class function TCollisionDetection.CalculateBounceAfterHittingCircle(
  const APathPart: IPathPart; const dX, dY: double; const ATargetPathPart: IPathPart;
  const dHitTime: double): TBounceResult;
var
  AVector, intfTargetCircleVector: IBasicVector;

  n, v1, v2, finalv1, finalv2: Tvector2_double;
  a1, a2, optimizedP: double;
  dFactor: double;
begin

  AVector := APathPart.Vector;
  intfTargetCircleVector := ATargetPathPart.Vector;

  v1 := AVector.GetVelocityVectorAtTime(dHitTime);
  v2 := intfTargetCircleVector.GetVelocityVectorAtTime(dHitTime);

  // First, find the normalized vector n from the center of
  // circle1 to the center of circle2
  n.init(DX - intfTargetCircleVector.Origin.X, DY - intfTargetCircleVector.Origin.Y);
  n.init(
    n.Data[0] / n.length,
    n.Data[1] / n.length);
  // Find the length of the component of each of the movement
  // vectors along n.
  // a1 = v1 . n
  // a2 = v2 . n
  a1 := (v1.Data[0] * n.Data[0]) + (v1.Data[1] * n.Data[1]); //v1.dot(n)
  a2 := (v2.Data[0] * n.Data[0]) + (v2.Data[1] * n.Data[1]); // v2.dot(n);

  // Using the optimized version,
  // optimizedP =  2(a1 - a2)
  //              -----------
  //                m1 + m2
  optimizedP := (2.0 * (a1 - a2)) / (APathPart.Circle.mass +
    ATargetPathPart.Circle.mass);

  // Calculate v1', the new movement vector of circle1
  // v1' = v1 - optimizedP * m2 * n
  dFactor := optimizedP * ATargetPathPart.Circle.Mass;
  finalv1.init(v1.Data[0] - (n.Data[0] * dFactor),
    v1.Data[1] - (n.Data[1] * dFactor));

  // Calculate v1', the new movement vector of circle1
  // v2' = v2 + optimizedP * m1 * n
  dFactor := optimizedP * APathPart.Circle.Mass;
  finalv2.init(v2.Data[0] + (n.Data[0] * dFactor),
    v2.Data[1] + (n.Data[1] * dFactor));

  Result.Vector1 := finalv1;
  Result.Vector2 := finalv2;
end;


end.
